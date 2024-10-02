library(data.table)
clean_data <- readRDS("clean-data/clean-data.rds")
with(clean_data, table(year, k2q35a, useNA="always"))
not.X <- c("year", "k2q35a")
names(clean_data)
X.name.vec <- setdiff(names(clean_data),not.X)

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
  count.dt.list[[X.name]] <- data.table(X.name, n.unique=length(unique(X.col)), col.type)
}
(count.dt <- rbindlist(count.dt.list)[order(n.unique)])
(categorical.dt <- rbindlist(categorical.dt.list)[order(min.years, variable, years, value), .(min.years, max.years, n.years, years, variable, count, value)])
categorical.dt[min.years<max.years]

## > table(clean_data[["k6q71_r.1"]]) ### .1???
##     Never Sometimes    Always   Usually 
##      2042     29830    180976     64060 
## > table(clean_data[["k6q71_r"]])
##     Never Sometimes    Always   Usually 
##      2042     29830    180976     64060 


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
year.do.some <- Sys.glob("clean-data/download-nsch-data/*.do")
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
var.config.list <- RJSONIO::fromJSON("clean-data/variable-config.json")
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
}
anomaly.list$define

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
(categorical.desc <- most.freq[, .(variable,desc)][categorical.dt,on="variable"][min.years<max.years])
categorical.desc[is.na(desc)]
fwrite(categorical.desc, "clean-data-values-only-in-some-years.csv")

names(var.config.list$transformations$merge_columns)
names(var.config.list$transformations$rename_columns)
names(clean_data)

year.dt[, Autism := k2q35a]
prop.NA <- colMeans(is.na(year.dt))
half.names <- setdiff(names(prop.NA)[prop.NA<0.1], c("Autism","k2q35a","survey_year","year"))
some.col.names <- c("survey_year", "Autism", input.meta.dt$column_name)
some.col.names <- c("survey_year", "Autism", half.names)
year.some.cols <- year.dt[, some.col.names, with=FALSE]
keep <- apply(!is.na(year.some.cols), 1, all)
year.keep <- year.some.cols[keep]
year.X.dt <- year.keep[, half.names, with=FALSE]
X.mat.list <- list()
for(col.i in seq_along(year.X.dt)){
  col.vec <- year.X.dt[[col.i]]
  col.tab <- table(col.vec)
  if(length(col.tab)>1){
    col.name <- names(year.X.dt)[[col.i]]
    new.name <- var.dt[col.name, desc]
    if(is.na(new.name)){
      cat(sprintf("consider excluding column %s\n", col.name))
    }
    if(is.factor(col.vec)){
      col.levs <- levels(col.vec)
      out.levs <- col.levs[-length(col.levs)]
      for(lev in out.levs){
        X.mat.list[[sprintf("%s=%s", new.name, lev)]] <- ifelse(col.vec==lev, 1, 0)
      }
    }else if(is.numeric(col.vec)){
      X.mat.list[[new.name]] <- col.vec
    }
  }
}
X.mat <- do.call(cbind, X.mat.list)
y.vec <- year.keep$Autism
