library(ggplot2)
library(data.table)
(objs <- load("download-nsch-mlr3batchmark-registry.RData"))
msr.list <- mlr3::msrs(c("classif.auc", "classif.acc"))
score.dt <- mlr3resampling::score(bmr, msr.list)
score.dt[, table(task_id, train.groups)]

score.some <- score.dt[task_id=="home.13" & train.groups=="same"]
ggplot()+
  geom_point(aes(
    classif.acc, learner_id),
    data=score.some)+
  facet_grid(. ~ survey_year, labeller=label_both, scales="free")
