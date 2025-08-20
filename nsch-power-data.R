library(data.table)
two.years <- fread("download-nsch-convert-do-2019-2020-366cols.csv", stringsAsFactors=TRUE)
task.list <- list()
task.dt <- two.years[survey_year==2019]
setnames(task.dt, gsub("'", "", gsub("[$]", "USD", gsub("[],:;+?()/<> =[-]", "_", names(task.dt), perl=TRUE))))
one.task <- mlr3::TaskClassif$new(
  "2019", task.dt, target="Autism")
one.task$col_roles$stratum <- "Autism"
task.list[["2019"]] <- one.task
sizes_cv <- mlr3resampling::ResamplingSameOtherSizesCV$new()
sizes_cv$param_set$values$sizes <- 6
glmnet.learner <- mlr3learners::LearnerClassifCVGlmnet$new()
glmnet.learner$param_set$values$nfolds <- 3
fless.learner <- mlr3::LearnerClassifFeatureless$new()
(learner.list <- list(
  glmnet.learner, fless.learner))
for(learner.i in seq_along(learner.list)){
  learner.list[[learner.i]]$predict_type <- "prob"
}
(bench.grid <- mlr3::benchmark_grid(
  task.list,
  learner.list,
  sizes_cv))
future::plan("multisession")
bench.result <- mlr3::benchmark(bench.grid, store_models=TRUE)
save(bench.result, file="nsch-power-data.RData")
 
