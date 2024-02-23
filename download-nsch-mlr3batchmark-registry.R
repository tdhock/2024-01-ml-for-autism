library(ggplot2)
library(data.table)
(objs <- load("download-nsch-mlr3batchmark-registry.RData"))
msr.list <- mlr3::msrs(c("classif.auc", "classif.acc"))
score.dt <- mlr3resampling::score(bmr, msr.list)
score.dt[, percent.accuracy := 100*classif.acc]
score.dt[, table(task_id, algorithm)]

score.some <- score.dt[task_id=="all.364" & train.groups=="same" & survey_year==2020]
(stats.some <- dcast(
  score.some,
  algorithm ~ .,
  list(mean, sd, length),
  value.var=c("percent.accuracy", "classif.auc")
)[order(percent.accuracy_mean)])
score.some[, Algorithm := factor(algorithm, stats.some$algorithm)]
gg <- ggplot()+
  ggtitle("Survey year 2020, all 364 features, all 10 cross-validation folds")+
  theme(plot.margin=grid::unit(c(0,1,0,0), "lines"))+
  geom_point(aes(
    percent.accuracy, Algorithm),
    shape=1,
    data=score.some)+
  scale_x_continuous(
    "Percent correctly predicted labels in test set, one dot per train/test split",
    breaks=seq(96, 98, by=0.1))
print(gg)
png("download-nsch-mlr3batchmark-registry-one-set-all-features.png", width=8, height=2, units="in", res=100)
print(gg)
dev.off()

stats.some[, Algorithm := factor(algorithm, stats.some$algorithm)]
gg <- ggplot()+
  ggtitle("Survey year 2020, all 364 features, summary of 10 cross-validation folds")+
  geom_segment(aes(
    percent.accuracy_mean+percent.accuracy_sd, Algorithm,
    xend=percent.accuracy_mean-percent.accuracy_sd, yend=Algorithm),
    data=stats.some)+
  geom_point(aes(
    percent.accuracy_mean, Algorithm),
    shape=1,
    data=stats.some)+
  geom_text(aes(
    percent.accuracy_mean, Algorithm,
    label=sprintf("%.2f±%.2f", percent.accuracy_mean, percent.accuracy_sd)),
    vjust=1.5,
    hjust=0,
    data=stats.some)+
  scale_x_continuous(
    "Percent correctly predicted labels in test set, mean±SD over 10 train/test splits",
    breaks=seq(96, 98, by=0.1))
png("download-nsch-mlr3batchmark-registry-one-set-all-features-stats.png", width=8, height=2, units="in", res=100)
print(gg)
dev.off()

roc.dt.list <- list()
for(row.i in 1:nrow(score.some)){
  score.row <- score.some[row.i]
  pred.obj <- score.row$prediction[[1]]
  roc.df <- WeightedROC::WeightedROC(
    pred.obj$data$prob[,"Yes"],
    ifelse(pred.obj$data$truth=="Yes", 1, 0))
  roc.dt.list[[row.i]] <- data.table(
    score.row[, .(Algorithm, test.fold)],
    roc.df)
}
(roc.dt <- rbindlist(roc.dt.list))
roc.breaks <- seq(0, 1, by=0.2)
gg <- ggplot()+
  ggtitle("Survey year 2020, all 364 features,\none ROC curve per cross-validation fold")+
  geom_path(aes(
    FPR, TPR, color=Algorithm, group=test.fold),
    data=roc.dt)+
  coord_equal()+
  scale_x_continuous(
    "False Positive Rate",
    breaks=roc.breaks)+
  scale_y_continuous(
    "True Positive Rate",
    breaks=roc.breaks)
png("download-nsch-mlr3batchmark-registry-one-set-all-features-roc.png", width=6, height=6, units="in", res=100)
print(gg)
dev.off()
