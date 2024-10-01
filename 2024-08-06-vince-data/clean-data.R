library(data.table)
clean_data <- readRDS("clean-data/clean-data.rds")
with(clean_data, table(year, k2q35a, useNA="always"))
names(clean_data)

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
for(data.type in names(do.dt.list)){
  one.do <- rbindlist(do.dt.list[[data.type]])
  do.meta[[data.type]] <- one.do
}

do.meta$define[, `:=`(
  n.values=.N,
  n.unique=length(unique(desc))
), keyby=.(variable,value)
][n.unique>1]

max.years <- length(unique(do.meta$define$year))
define_anomalies <- do.meta$define[, .(
  years=paste(year, collapse=","),
  n.years=.N,
  n.values=n.values[1]
), keyby=.(variable, value, desc)][n.years<n.values]
fwrite(define_anomalies, "define_anomalies.csv")

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
