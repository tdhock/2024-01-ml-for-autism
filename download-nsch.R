data.dir <- "download-nsch-data"
for(year in 2016:2022){
  year.html <- file.path(data.dir, paste0(year, ".html"))
  u <- paste0("https://www.census.gov/programs-surveys/nsch/data/datasets/nsch", year.html)
  if(!file.exists(year.html))download.file(u, year.html)
  url.dt <- nc::capture_all_str(
    year.html,
    'href="',
    url=".*?topical.*?zip",
    '"')
  for(url.i in 1:nrow(url.dt)){
    zip.url <- paste0("http:", url.dt[url.i, url])
    year.zip <- file.path(data.dir, basename(zip.url))
    if(!file.exists(year.zip))download.file(zip.url, year.zip)
    unzip(year.zip, exdir=data.dir)
  }
}

data.list <- list()
for(sas7bdat in Sys.glob(file.path(data.dir, "*.sas7bdat"))){
  data.list[[sas7bdat]] <- haven::read_sas(sas7bdat)
}

one.dta <- haven::read_dta("download-nsch-data/nsch_2018_topical.dta")
one.dta$k2q33a
?haven::labelled

library(data.table)
name.dt.list <- list()
dim.dt.list <- list()
for(sas7bdat in names(data.list)){
  year.dt <- data.list[[sas7bdat]]
  name.dt.list[[sas7bdat]] <- data.table(sas7bdat, column=names(year.dt))
  dim.dt.list[[sas7bdat]] <- data.table(sas7bdat, nrow=nrow(year.dt), ncol=ncol(year.dt))
}
name.dt <- rbindlist(name.dt.list)
dim.dt <- rbindlist(dim.dt.list)
name.counts <- name.dt[, .(years=.N), by=column][order(-years, column)]
data.table::fwrite(name.counts, "download-nsch-column-counts.csv")
data.table::fwrite(dim.dt, "download-nsch-nrow-ncol.csv")
common.cols <- name.counts[years==max(years), column]
str(data.list[[1]][, common.cols], list.len=length(common.cols))

year.do.vec <- Sys.glob(file.path(data.dir, "*.do"))
year.do <- year.do.vec[1]
year.dta <- sub("do$", "dta", year.do)
year.tib <- haven::read_dta(year.dta)
table(year.tib[["screentime"]], useNA="always")

define.csv <- paste0(year.do, ".define.csv")
if(file.exists(define.csv)){
  define.dt <- fread(define.csv)
}else{
  define.dt <- nc::capture_all_str(
    year.do,
    "label define ",
    variable=".*?",
    "_lab +",
    value=".*?",
    ' +"',
    desc=".*?",
    '"')
  fwrite(define.dt, define.csv)
}
## label var screentime  "How Much Time Spent with TV, Cellphone, Computer"
## cap label values screentime screentime_lab
## label define screentime_lab  1  "Less than 1 hour"
## label define screentime_lab  2  "1 hour", add
define.dt[variable=="screentime"]

fread("NSCH_columns.csv", header=TRUE)

define.not.missing <- define.dt[
  !grepl("[.]", value)
][
, value.int := as.integer(value)
]
year.dt <- data.table(year.tib)
## check parsing.
options(width=150)
define.not.missing[, {
  print(variable)
  num.vec <- year.tib[[variable]]
  fac.vec <- factor(num.vec, value.int, desc)
  val.tab <- table(fac.vec, num.vec, useNA="always")
  na.i <- which(is.na(rownames(val.tab)))
  na.counts <- val.tab[na.i,]
  if(sum(na.counts!=0)>1){
    ## More than one original numeric value would be converted to NA,
    ## so just print out what would happen, and keep the original
    ## numeric column.
    print(val.tab)
  }else{
    set(year.dt, j=variable, value=fac.vec)
  }
  NULL
}, by=variable]
## TODO convert to censored? for now omit.
## a1_age 75 or older
## a1_liveusa 1970 or earlier
## a2_liveusa 1970 or earlier
## a2_age 75 or older
## birthwt_oz_s 155 or more
## birthwt_oz_s 72 or less
## breastfedend_mo_s 30 or more
## famcount 8 or more
## frstformula_mo_s 12 or more
## frstsolids_mo_s 15 or more
## hhcount 10 or more
## k11q43r 15 or more
## k2q35a_1_years 15, 16 or 17
## momage 18 or younger
## momage 45 or older
## sesplanyr 16 or 17
## sesplanmo 7 or 8
## fpl_i1 50 or less
## fpl_i1 400 or more
## ...
## fpl_i6 50 or less
## fpl_i6 400 or more
year.dt[["momage"]]

define.dt[variable=="fipsst"]


## label var a2_if  "Imputation Flag for A2 Variables"
## cap label values a2_if a2_if_lab
## label define a2_if_lab  1  "Imputed"
## label define a2_if_lab  0  "Not imputed", add
## label define a2_if_lab  .m "No valid response", add

## label var a1_active  "Adult 1 - Active Duty"
## cap label values a1_active a1_active_lab
## label define a1_active_lab  1  "Never served in the military"
## label define a1_active_lab  2  "Only on active duty for training in the Reserves or National Guard", add