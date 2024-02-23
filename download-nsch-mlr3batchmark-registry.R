library(ggplot2)
library(glmnet)
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
roc.points.list <- list()
for(row.i in 1:nrow(score.some)){
  score.row <- score.some[row.i]
  pred.obj <- score.row$prediction[[1]]
  prob.vec <- pred.obj$data$prob[,"Yes"]
  roc.dt <- data.table(WeightedROC::WeightedROC(
    prob.vec,
    ifelse(pred.obj$data$truth=="Yes", 1, 0)
  ))[, prev.thresh := c(-Inf, threshold[-.N])][]
  pred.point <- roc.dt[prev.thresh < 0.5 & 0.5 < threshold]
  if(nrow(pred.point)!=1){
    stop("more than one pred point")
  }
  meta.dt <- score.row[, .(Algorithm, test.fold)]
  roc.points.list[[row.i]] <- data.table(meta.dt, pred.point)
  roc.dt.list[[row.i]] <- data.table(meta.dt, roc.dt)
}
(roc.dt <- rbindlist(roc.dt.list))
(roc.points <- rbindlist(roc.points.list))
roc.breaks <- seq(0, 1, by=0.2)
gg <- ggplot()+
  ggtitle("Survey year 2020, all 364 features,\nOne ROC curve per cross-validation fold")+
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
png("download-nsch-mlr3batchmark-registry-one-set-all-features-roc.png", width=7, height=6, units="in", res=100)
print(gg)
dev.off()
gg.point <- gg+
  geom_point(aes(
    FPR, TPR, color=Algorithm),
    shape=21,
    fill="white",
    data=roc.points)
png("download-nsch-mlr3batchmark-registry-one-set-all-features-roc-point.png", width=7, height=6, units="in", res=100)
print(gg.point)
dev.off()
gg.zoom <- gg.point+
  ggtitle("Survey year 2020, all 364 features, zoom to show FPR/TPR of predictions")+
  theme(panel.spacing=grid::unit(2, "lines"))+
  facet_wrap("test.fold", ncol=5, labeller=label_both)+
  scale_x_continuous(
    "False Positive Rate",
    breaks=seq(0,0.01,by=0.005))+
  scale_y_continuous(
    "True Positive Rate",
    breaks=seq(0,0.5,by=0.1))+
  coord_cartesian(xlim=c(0,0.01),ylim=c(0,0.5))
png("download-nsch-mlr3batchmark-registry-one-set-all-features-roc-zoom.png", width=10, height=6, units="in", res=100)
print(gg.zoom)
dev.off()

gg <- ggplot()+
  ggtitle("Survey year 2020, all 364 features, AUC over 10 cross-validation folds")+
  geom_segment(aes(
    classif.auc_mean+classif.auc_sd, Algorithm,
    xend=classif.auc_mean-classif.auc_sd, yend=Algorithm),
    data=stats.some)+
  geom_point(aes(
    classif.auc_mean, Algorithm),
    shape=1,
    data=stats.some)+
  geom_text(aes(
    classif.auc_mean, Algorithm,
    hjust=ifelse(Algorithm=="featureless", 0, 1),
    label=sprintf("%.4f±%.4f", classif.auc_mean, classif.auc_sd)),
    vjust=1.5,
    data=stats.some)+
  scale_x_continuous(
    "Area Under the test set ROC Curve, mean±SD over 10 train/test splits")
png("download-nsch-mlr3batchmark-registry-one-set-all-features-auc.png", width=8, height=2, units="in", res=100)
print(gg)
dev.off()

## feature importance
score.glmnet <- score.dt[algorithm%in%c("cv_glmnet","featureless") & survey_year==2020 & train.groups=="same"]
(stats.glmnet <- dcast(
  score.glmnet,
  algorithm + task_id ~ .,
  list(mean, sd, length),
  value.var=c("percent.accuracy", "classif.auc")
)[order(percent.accuracy_mean)])
levs <- stats.glmnet[algorithm=="cv_glmnet", task_id]
score.glmnet[, Features := factor(task_id, levs)]
stats.glmnet[, Features := factor(task_id, levs)]
ggplot()+
  geom_point(aes(
    percent.accuracy, Features, color=algorithm),
    shape=1,
    data=score.glmnet)
gg <- ggplot()+
  ggtitle("Survey year 2020, train on feature subsets,\nsummary of 10 cross-validation folds")+
  geom_segment(aes(
    percent.accuracy_mean+percent.accuracy_sd, Features,
    color=algorithm,
    xend=percent.accuracy_mean-percent.accuracy_sd, yend=Features),
    data=stats.glmnet)+
  geom_point(aes(
    percent.accuracy_mean, Features,
    color=algorithm),
    shape=1,
    data=stats.glmnet)+
  geom_text(aes(
    percent.accuracy_mean, Features,
    color=algorithm,
    label=sprintf("%.2f±%.2f", percent.accuracy_mean, percent.accuracy_sd)),
    vjust=1.5,
    hjust=0,
    data=stats.glmnet[algorithm=="cv_glmnet"])+
  scale_x_continuous(
    "Percent correctly predicted labels in test set, mean±SD over 10 train/test splits",
    breaks=seq(96, 98, by=0.1))
png("download-nsch-mlr3batchmark-registry-one-set-compare-features.png", width=8, height=4, units="in", res=100)
print(gg)
dev.off()

## TODO after new reduce 
only.glmnet <- score.glmnet[task_id=="all.364" & algorithm=="cv_glmnet"]
weight.dt.list <- list()
for(score.i in 1:nrow(only.glmnet)){
  score.row <- only.glmnet[score.i]
  fit <- score.row$learner[[1]]$model
  weight.mat <- coef(fit)[-1,]
  weight.dt.list[[score.i]] <- score.row[, .(
    test.fold, seed, train_min_size,
    weight=as.numeric(weight.mat),
    variable=factor(names(weight.mat), levs))]
}
(weight.dt <- rbindlist(weight.dt.list))

## train on one year, predict on another.
score.all <- score.dt[task_id=="all.364" & algorithm%in%c("featureless","cv_glmnet")]
ggplot()+
  geom_point(aes(
    percent.accuracy, train.groups,
    color=algorithm),
    shape=1,
    data=score.all)+
  facet_grid(. ~ test.group, labeller=label_both, scales="free")
(stats.all <- dcast(
  score.all,
  algorithm + train.groups + test.group ~ .,
  list(mean, sd, length),
  value.var=c("percent.accuracy", "classif.auc")
)[order(percent.accuracy_mean)])
gg <- ggplot()+
  ggtitle("Fix test group, train on same/other/all, summary of 10 cross-validation folds")+
  facet_grid(. ~ test.group, labeller=label_both, scales="free")+
  geom_segment(aes(
    percent.accuracy_mean+percent.accuracy_sd, train.groups,
    color=algorithm,
    xend=percent.accuracy_mean-percent.accuracy_sd, yend=train.groups),
    data=stats.all)+
  geom_point(aes(
    percent.accuracy_mean, train.groups,
    color=algorithm),
    shape=1,
    data=stats.all)+
  geom_text(aes(
    percent.accuracy_mean, train.groups,
    color=algorithm,
    hjust=ifelse(algorithm=="featureless", 0, 0.5),
    label=sprintf("%.2f±%.2f", percent.accuracy_mean, percent.accuracy_sd)),
    vjust=1.5,
    data=stats.all)+
  scale_x_continuous(
    "Percent correctly predicted labels in test set, mean±SD over 10 train/test splits",
    breaks=seq(96, 98, by=0.2))
png("download-nsch-mlr3batchmark-registry-predict-new-year.png", width=8, height=2, units="in", res=100)
print(gg)
dev.off()

