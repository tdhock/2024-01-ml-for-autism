library(data.table)
library(glmnet)
load("nsch-power-data.RData")
score_dt <- mlr3resampling::score(bench.result, mlr3::msrs("classif.auc"))[, "Features selected" := NA_integer_]
for(learner.i in 1:nrow(score_dt)){
  weight_mat <- coef(score_dt$learner[[learner.i]]$model)[-1,]
  n_sel <-  if(is.null(weight_mat)) 0 else {
    sum(weight_mat != 0)
  }
  set(score_dt, i=learner.i, j="Features selected", value=n_sel)
}
score_stats <- dcast(
  score_dt[, "Test AUC" := classif.auc],
  algorithm + n.train.groups ~ .,
  list(mean, sd, length, min, max, median),
  value.var=c("Test AUC", "Features selected"))
score_tall <- melt(
  score_stats,
  measure.vars=measure(metric, value.name, sep="_"))
library(ggplot2)
gg <- ggplot()+
  geom_ribbon(aes(
    n.train.groups, ymax=max, ymin=min, fill=algorithm),
    data=score_tall,
    alpha=0.5)+
  geom_line(aes(
    n.train.groups, median, color=algorithm),
    size=1,
    data=score_tall)+
  facet_grid(metric ~ ., scales="free")+
  scale_x_log10("Number of surveys in train set")+
  theme_bw()+
  theme(legend.position=c(0.8, 0.2))+
  scale_y_continuous("")+
  geom_blank(aes(
    x, y),
    data=data.frame(x=300, y=1, metric="Test AUC"))+
  ggtitle("NCSH 2019 data, 3% Autism prevalence\nPredicting Autism diagnosis using linear model\n365 features input to learning algorithm")
png("nsch-power.png", width=5, height=4, units="in", res=200)
print(gg)
dev.off()
