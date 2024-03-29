library(data.table)
category366 <- fread("download-nsch-convert-do-2019-2020-366cols-categories.csv")
options(width=50)
table(category366$category)
input.meta.dt <- category366[category!=""]
feature.names.list <- with(input.meta.dt, split(column_name, category))
feature.names.list[["all"]] <- input.meta.dt$column_name
two.years <- fread("download-nsch-convert-do-2019-2020-366cols.csv", stringsAsFactors=TRUE)
names(bench.dt)
names(two.years)
task.list <- list()
for(feature.names in names(feature.names.list)){
  feature.names.vec <- feature.names.list[[feature.names]]
  task.col.names <- c("survey_year", "Autism", feature.names.vec)
  task.dt <- two.years[, task.col.names, with=FALSE]
  setnames(task.dt, gsub("'", "", gsub("[$]", "USD", gsub("[],:;+?()/<> =[-]", "_", task.col.names, perl=TRUE))))
  flist <- list()
  flist[[feature.names]] <- feature.names.vec
  flist[[paste0("not_",feature.names)]] <- setdiff(
    feature.names.list$all, feature.names.vec)
  for(model.name in names(flist)){
    fname.vec <- flist[[model.name]]
    if(length(fname.vec)>1){
      task_id <- sprintf("%s.%d", model.name, length(fname.vec))
      one.task <- mlr3::TaskClassif$new(
        task_id, task.dt, target="Autism")
      one.task$col_roles$stratum <- c("survey_year","Autism")
      one.task$col_roles$group <- "survey_year"
      task.list[[task_id]] <- one.task
    }
  }
}
names(task.list)
bench.dt <- data.table(task.dt)
names(bench.dt)[2] <- "y"
fwrite(bench.dt, "~/genomic-ml/projects/cv-same-other-paper/data_Classif/NSCH_autism.csv")

subtrain.valid.cv <- mlr3resampling::ResamplingIgnoreGroupCV$new()
subtrain.valid.cv$param_set$values$folds <- 5
##subtrain.valid.cv$instantiate(task.list[[1]])
same_other_cv <- mlr3resampling::ResamplingSameOtherCV$new()
same_other_cv$param_set$values$folds <- 10
knn.learner <- mlr3learners::LearnerClassifKKNN$new()
knn.learner$param_set$values$k <- paradox::to_tune(1, 20)
knn.learner$predict_type <- "prob"
knn.tuned = mlr3tuning::auto_tuner(
  tuner = mlr3tuning::TunerGridSearch$new(),
  learner = knn.learner,
  resampling = subtrain.valid.cv,
  measure = mlr3::msr("classif.auc"))
knn.tuned$id <- "classif.nearest_neighbors"
xgboost.learner <- mlr3learners::LearnerClassifXgboost$new()
xgboost.learner$param_set$values$eta <- paradox::to_tune(0.001, 1, log=TRUE)
xgboost.learner$param_set$values$nrounds <- paradox::to_tune(1, 100)
grid.search.5 <- mlr3tuning::TunerGridSearch$new()
grid.search.5$param_set$values$resolution <- 5
xgboost.learner$predict_type <- "prob"
xgboost.tuned = mlr3tuning::auto_tuner(
  tuner = grid.search.5,
  learner = xgboost.learner,
  resampling = subtrain.valid.cv,
  measure = mlr3::msr("classif.auc"))
xgboost.tuned$id <- "classif.xgboost"
if(FALSE){
  ## Error : <TaskClassif:age_sex.6> has the following unsupported feature types: factor
  ## https://mlr3book.mlr-org.com/chapters/chapter9/preprocessing.html#factor-encoding
  xgboost.tuned$train(task.list[[1]])
  xgboost.pipeline$train(task.list[[1]])
}
if(FALSE){#ranger is computationally intensive
  ranger.learner <- mlr3learners::LearnerClassifRanger$new()
  ranger.learner$predict_type <- "prob"
  ranger.tuned = mlr3tuning::auto_tuner(
    tuner = grid.search.5,
    learner = mlr3tuningspaces::lts(ranger.learner),
    resampling = subtrain.valid.cv,
    measure = mlr3::msr("classif.auc"))
  ranger.tuned$id <- "classif.ranger"
}
glmnet.learner <- mlr3learners::LearnerClassifCVGlmnet$new()
rpart.learner <- mlr3::LearnerClassifRpart$new()
fless.learner <- mlr3::LearnerClassifFeatureless$new()
(learner.list <- list(
  xgboost.tuned, knn.tuned,
  glmnet.learner, rpart.learner, fless.learner))
for(learner.i in seq_along(learner.list)){
  learner.list[[learner.i]]$predict_type <- "prob"
}
(reg.bench.grid <- mlr3::benchmark_grid(
  task.list,
  learner.list,
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
  walltime = 6*60*60,#seconds
  memory = 8000,#megabytes per cpu
  ncpus=1,  #>1 for multicore/parallel jobs.
  ntasks=1, #>1 for MPI jobs.
  chunks.as.arrayjobs=TRUE), reg=reg)

reg.obj <- readRDS(file.path(reg.dir, "registry.rds"))
sacct.arg <- reg.obj$status[1, paste0("-j", sub("_.*", "", batch.id))]
sacct.dt <- slurm::sacct(sacct.arg)
reg=batchtools::loadRegistry(reg.dir)
print(batchtools::getStatus(reg=reg))
jobs.after <- batchtools::getJobTable(
  reg=reg
)[, `:=`(
  learner_id = sapply(algo.pars, function(dt)dt[["learner_id"]]),
  task_id = sapply(prob.pars, function(dt)dt[["task_id"]])
)][]
usage.long <- jobs.after[sacct.dt, .(hours, megabytes, learner_id, task_id), on=.(job.id=task)][is.finite(megabytes)][order(megabytes)]
sum(usage.long$hours)/4
usage.wide <- dcast(usage.long, learner_id + task_id ~ ., list(min, median, max, length), value.var=c("hours", "megabytes"))
options(width=150)
usage.wide[order(megabytes_max), .(learner_id, task_id, megabytes_min, megabytes_median, megabytes_max, megabytes_length)]
usage.wide[order(hours_max), .(learner_id, task_id, hours_min, hours_median, hours_max, hours_length)]
table(jobs.after$error)
jobs.after[!is.na(error), .(error, task_id, learner_id)]
jobs.after[!is.na(done), .(time.running, task_id, learner_id)]
ids <- jobs.after[!is.na(done) & is.na(error), job.id]
only.keep.glmnet.coef <- function(L){
  m <- L$learner_state$model
  L$learner_state$model <- if(inherits(m, "cv.glmnet")){
    print(coef(m))
  }
  L
}
if(FALSE){#https://github.com/mlr-org/mlr3batchmark/pull/29
  remotes::install_github("tdhock/mlr3batchmark@reduceResultsList.fun")
}
bmr = mlr3batchmark::reduceResultsBatchmark(ids, reg = reg, store_backends = FALSE, reduceResultsList.fun=only.keep.glmnet.coef)
out.RData <- paste0(reg.dir, ".RData")
save(bmr, file=out.RData)
