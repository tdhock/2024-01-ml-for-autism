library(data.table)
NSCH_categories_TDH <- setkey(fread(
  "NSCH_categories_NA_counts_TDH.csv"
)[, column_name := tolower(column_name)], category, column_name)
input.meta.dt <- NSCH_categories_TDH[category!=""]
feature.names.list <- with(input.meta.dt, split(column_name, category))
feature.names.list[["all"]] <- input.meta.dt$column_name
two.years <- fread("download-nsch-convert-do-2019-2020.csv", stringsAsFactors=TRUE)
task.list <- list()
for(feature.names in names(feature.names.list)){
  feature.names.vec <- feature.names.list[[feature.names]]
  task.dt <- two.years[, c("survey_year", "Autism", feature.names.vec), with=FALSE]
  task_id <- sprintf("%s.%d", feature.names, length(feature.names.vec))
  task.list[[task_id]] <- mlr3::TaskClassif$new(
    task_id, task.dt, target="Autism"
  )$set_col_roles(
    "survey_year",c("group","stratum")
  )$set_col_roles(
    "Autism",c("target","stratum")
  )
}

subtrain.valid.cv <- mlr3::ResamplingCV$new()
subtrain.valid.cv$param_set$values$folds <- 5
same_other_cv <- mlr3resampling::ResamplingSameOtherCV$new()
same_other_cv$param_set$values$folds <- 10
knn.learner <- mlr3learners::LearnerClassifKKNN$new()
knn.learner$predict_type <- "prob"
knn.learner$param_set$values$k <- paradox::to_tune(1, 20)
knn.tuned = mlr3tuning::auto_tuner(
  tuner = mlr3tuning::TunerGridSearch$new(),
  learner = knn.learner,
  resampling = subtrain.valid.cv,
  measure = mlr3::msr("classif.auc"))
xgboost.learner <- mlr3learners::LearnerClassifXgboost$new()
xgboost.learner$predict_type <- "prob"
xgboost.learner$param_set$values$eta <- paradox::to_tune(0.001, 1, log=TRUE)
xgboost.learner$param_set$values$nrounds <- paradox::to_tune(1, 100)
grid.search.5 <- mlr3tuning::TunerGridSearch$new()
grid.search.5$param_set$values$resolution <- 5
xgboost.tuned = mlr3tuning::auto_tuner(
  tuner = grid.search.5,
  learner = xgboost.learner,
  resampling = subtrain.valid.cv,
  measure = mlr3::msr("classif.auc"))
ranger.learner <- mlr3learners::LearnerClassifRanger$new()
ranger.learner$predict_type <- "prob"
ranger.tuned = mlr3tuning::auto_tuner(
  tuner = grid.search.5,
  learner = mlr3tuningspaces::lts(ranger.learner),
  resampling = subtrain.valid.cv,
  measure = mlr3::msr("classif.auc"))
(reg.learner.list <- list(
  ranger.tuned, xgboost.tuned, knn.tuned,
  mlr3learners::LearnerClassifCVGlmnet$new(),
  mlr3::LearnerClassifRpart$new(),
  mlr3::LearnerClassifFeatureless$new()))
(reg.bench.grid <- mlr3::benchmark_grid(
  task.list,
  reg.learner.list,
  same_other_cv))

reg.dir <- "download-nsch-mlr3batchmark-registry"
unlink(reg.dir, recursive=TRUE)
reg = batchtools::makeExperimentRegistry(
  file.dir = reg.dir,
  seed = 1,
  packages = "mlr3verse"
)
mlr3batchmark::batchmark(
  reg.bench.grid, store_models = TRUE, reg=reg)
(job.table <- batchtools::getJobTable(reg=reg))
chunks <- data.frame(job.table, chunk=1)
batchtools::submitJobs(chunks, resources=list(
  walltime = 24*60*60,#seconds
  memory = 64000,#megabytes per cpu
  ncpus=1,  #>1 for multicore/parallel jobs.
  ntasks=1, #>1 for MPI jobs.
  chunks.as.arrayjobs=TRUE), reg=reg)


reg.dir <- "data-batchmark-registry"
reg=batchtools::loadRegistry(reg.dir)
print(batchtools::getStatus(reg=reg))
jobs.after <- batchtools::getJobTable(reg=reg)
table(jobs.after$error)
ids <- jobs.after[!is.na(done) & is.na(error), job.id]
ignore.learner <- function(L){
  L$learner_state$model <- NULL
  L
}
if(FALSE){#https://github.com/mlr-org/mlr3batchmark/pull/29
  remotes::install_github("tdhock/mlr3batchmark@reduceResultsList.fun")
}
bmr = mlr3batchmark::reduceResultsBatchmark(ids, reg = reg, store_backends = FALSE, reduceResultsList.fun=ignore.learner)
out.RData <- paste0(reg.dir, ".RData")
save(bmr, file=out.RData)
