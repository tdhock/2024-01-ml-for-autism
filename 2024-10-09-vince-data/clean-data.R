library(data.table)
pre <- "https://github.com/vas235/ASG3-machine-learning-prep/raw/d80c308ac403a0ca272634dcbd452adb647e3656/"
for(local.file in c("clean-data.rds","variable-config.json")){
  if(!file.exists(local.file)){
    u <- paste0(pre, local.file)
    download.file(u, local.file)
  }
}
clean_data <- readRDS("clean-data.rds")
with(clean_data, table(year, k2q35a, useNA="always"))
with(clean_data, table(year, useNA="always"))
not.X <- c("year", "k2q35a")
names(clean_data)
X.name.vec <- setdiff(names(clean_data),not.X)
fwrite(dcast(clean_data, year ~ k5q21, length), sep="\t")

## viz responses not used in 2016.
dcast(clean_data, family ~ year, length)
family.props <- melt(
  dcast(clean_data, family ~ year, length),
  measure.vars=measure(year=as.integer, pattern="([0-9]+)"),
  value.name="count"
)[
, prop := count/sum(count), by=year
][]
library(ggplot2)
ggplot()+
  geom_tile(aes(
    year, family, fill=log10(prop)),
    data=family.props)+
  scale_fill_gradient(low="white", high="red")+
  geom_text(aes(
    year, family, label=sprintf("%.1f", prop*100)),
    data=family.props)

## viz substitution.  
family.long <- melt(
  clean_data[
  , familyOld := ifelse(
    family %in% c("Grandparent household", "Single father"),
    "Other relation",
    as.character(family))
  ],
  measure.vars=c("family", "familyOld"),
  id.vars="year")
family.wide <- dcast(
  family.long, variable + year + value ~ ., length
)[
, prop := ./sum(.), by=.(variable, year)
][]
ggplot()+
  theme_bw()+
  theme(panel.grid.minor=element_blank())+
  facet_grid(. ~ variable, labeller=label_both)+
  coord_equal()+
  geom_tile(aes(
    year, value, fill=log10(prop)),
    color="grey",
    data=family.wide)+
  scale_fill_gradient(low="white", high="red")+
  geom_text(aes(
    year+0.4, value, label=sprintf("%.1f", prop*100)),
    hjust=1,
    data=family.wide)+
  scale_x_continuous(breaks=unique(family.wide$year))


interest.props <- melt(
  dcast(clean_data[
  , interest_curiosity := k6q71_r
  ], interest_curiosity ~ year, length),
  measure.vars=measure(year=as.integer, pattern="(^[0-9]+)"),
  value.name="count"
)[
, prop := count/sum(count), by=year
][]
library(ggplot2)
ggplot()+
  geom_tile(aes(
    year, interest_curiosity, fill=log10(prop)),
    data=interest.props)+
  scale_fill_gradient(low="white", high="red")+
  geom_text(aes(
    year, interest_curiosity, label=sprintf("%.1f", prop*100)),
    data=interest.props)
## 2016-2017
##     Question: How true are each of the following statements about this child?...This child shows interest and curiosity in learning new things
##         1 = Definitely true
##         2 = Somewhat true
##         3 = Not true 
##     2018-2022
##     Question : How often:...Does this child show interest and curiosity in learning new things?
##         1 = Always
##         2 = Usually
##         3 = Sometimes
##         4 = Never

## viz substitution interest.
interest.long <- melt(
  clean_data[
  , interest_curiosity_old := ifelse(
    interest_curiosity=="Usually",
    "Sometimes",
    as.character(interest_curiosity))
  ],
  measure.vars=c("interest_curiosity", "interest_curiosity_old"),
  id.vars="year")
interest.wide <- dcast(
  interest.long, variable + year + value ~ ., length
)[
, prop := ./sum(.), by=.(variable, year)
][]
ggplot()+
  theme_bw()+
  theme(panel.grid.minor=element_blank())+
  facet_grid(. ~ variable, labeller=label_both)+
  coord_equal()+
  geom_tile(aes(
    year, value, fill=log10(prop)),
    color="grey",
    data=interest.wide)+
  scale_fill_gradient(low="white", high="red")+
  geom_text(aes(
    year+0.4, value, label=sprintf("%.1f", prop*100)),
    hjust=1,
    data=interest.wide)+
  scale_x_continuous(breaks=unique(interest.wide$year))



count.dt.list <- list()
categorical.dt.list <- list()
for(X.name in X.name.vec){
  X.col <- clean_data[[X.name]]
  col.type <- "numeric"
  if(is.factor(X.col)){
    col.type <- "factor"
    X.dt <- clean_data[
    , c(X.name, "year"), with=FALSE
    ][
    , {
      u <- unique(year)
      .(count=.N, n.years=length(u), years=paste(u, collapse=","))
    }, by=c(value=X.name)
    ][, `:=`(
      min.years = min(n.years),
      max.years = max(n.years)
    )][]
    categorical.dt.list[[X.name]] <- data.table(variable=X.name, X.dt)
  }
  dc.dt <- dcast(data.table(isNA=is.na(X.col),year=clean_data$year), . ~ year, mean,value.var="isNA")[,-1]
  count.dt.list[[X.name]] <- data.table(variable=X.name, n.unique=length(unique(X.col)), col.type, "%NA"=dc.dt*100)
}

## > table(clean_data[["k6q71_r.1"]]) ### .1???
##     Never Sometimes    Always   Usually 
##      2042     29830    180976     64060 
## > table(clean_data[["k6q71_r"]])
##     Never Sometimes    Always   Usually 
##      2042     29830    180976     64060 

dcast(clean_data[year>2016], year ~ fpl_i1)

regex.list <- list(
  var=list(
    "label var ",
    variable=".*?",
    ' +"',
    desc=".*?",
    '"'),
  define=list(
    "label define ",
    variable=".*?",
    "_lab +",
    value=".*?",
    ' +"',
    desc=".*?",
    '"'))
year.do.some <- Sys.glob("../2024-08-06-vince-data/clean-data/download-nsch-data/*.do")
if(FALSE){
  unlink("clean-data/download-nsch-data/*.csv")
}
do.dt.list <- list()
for(year.do in year.do.some){
  print(year.do)
  year <- as.integer(gsub("_.*?$|^.*?_", "", year.do))
  for(data.type in names(regex.list)){
    type.csv <- paste0(year.do, ".", data.type,".csv")
    if(file.exists(type.csv)){
      type.dt <- fread(type.csv)
    }else{
      type.dt <- nc::capture_all_str(year.do, regex.list[[data.type]])
      fwrite(type.dt, type.csv)
    }
    do.dt.list[[data.type]][[year.do]] <- data.table(year, type.dt)
  }
}
do.meta <- list()
anomaly.list <- list()
by.var.list <- list(
  define=c("variable","value"),
  var="variable")
var.config.list <- RJSONIO::fromJSON("variable-config.json")
names(var.config.list$transformations)
transform.names <- names(var.config.list$transformations$transform)
col.count.list <- list()
for(data.type in names(do.dt.list)){
  one.do <- rbindlist(do.dt.list[[data.type]])
  by.vec <- by.var.list[[data.type]]
  col.counts <- data.table(one.do)[
  , n.values := .N, by=by.vec
  ][
  , .(
    years=paste(year,collapse=","),
    n.years=.N,
    n.values=n.values[1]
  ), keyby=c(by.vec,"desc")
  ]
  col.count.list[[data.type]] <- col.counts
  anomaly.list[[data.type]] <- col.counts[
    n.years<n.values
  ][
    names(clean_data), nomatch=0L
  ][
  , trans := variable %in% transform.names
  ]
  do.meta[[data.type]] <- one.do
  out.csv <- sprintf("clean-data-all-%s.csv", data.type)
  setkeyv(one.do, by.vec)
  fwrite(one.do, out.csv)
}
anomaly.list$define[, .(
  years=paste(year,collapse=",")
), by=.(variable,value)]

var.desc <- col.count.list$var[names(clean_data), .(
  desc,
  n.desc=.N,
  years,
  n.years
), on="variable", by=.EACHI][order(variable)]
fwrite(var.desc, "clean-data-var-all-desc.csv")
not.one <- var.desc[n.desc != 1]
fwrite(not.one, "clean-data-var-not-one-desc.csv")
(most.freq <- var.desc[, .SD[which.max(n.years)], by=variable])

(count.dt <- rbindlist(count.dt.list)[
  order(n.unique)
][
, diff2017.2016 := `%NA.2017`-`%NA.2016`
][])
(count.join <- most.freq[,.(variable,desc,n.desc)][
  count.dt,
  on="variable", mult="first"])
fwrite(count.join,"clean-data-unique-type-missing.csv")

(categorical.dt <- rbindlist(categorical.dt.list)[order(min.years, variable, years, value), .(min.years, max.years, n.years, years, variable, count, value)])
categorical.dt[min.years<max.years]
(categorical.desc <- most.freq[, .(variable,desc)][categorical.dt,on="variable"][min.years<max.years])
categorical.desc[is.na(desc)]
fwrite(categorical.desc, "clean-data-values-only-in-some-years.csv")

