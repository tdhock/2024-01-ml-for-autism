library(ggplot2)
library(data.table)
(objs <- load("download-nsch-mlr3batchmark-registry-errors.RData"))
msr.list <- mlr3::msrs(c("classif.auc", "classif.acc"))
score.dt <- mlr3resampling::score(bmr, msr.list)
table(score.dt$task_id)

score.all.same <- score.dt["all.31"==task_id & train.groups=="same"]
gg <- ggplot()+
  ggtitle("Train using 31 features, predict on same year")+
  theme(plot.margin=grid::unit(c(1,1,1,1), "lines"))+
  geom_point(aes(
    classif.acc*100, algorithm),
    shape=1,
    data=score.all.same)+
  scale_x_continuous(
    "Percent correctly predicted labels in test set")+
  facet_grid(. ~ survey_year, labeller=label_both, scales="free")
png("download-nsch-mlr3batchmark-registry-errors.png", width=8, height=2, units="in", res=100)
print(gg)
dev.off()
system("cd /projects/genomic-ml && unpublish_data projects/2024-01-ml-for-autism && publish_data projects/2024-01-ml-for-autism")

score.all.all <- score.dt[grepl("all",task_id) & train.groups=="all"]
ggplot()+
  geom_point(aes(
    classif.acc, algorithm),
    data=score.all.all)+
  facet_grid(. ~ survey_year, labeller=label_both, scales="free")
