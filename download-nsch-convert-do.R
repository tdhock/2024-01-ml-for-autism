library(data.table)
NSCH_categories_TDH <- setkey(fread(
  "NSCH_categories_NA_counts_TDH.csv"
)[, column_name := tolower(column_name)], category, column_name)
input.meta.dt <- NSCH_categories_TDH[category!=""]
feature.names.list <- with(input.meta.dt, split(column_name, category))
feature.names.list[["all"]] <- input.meta.dt$column_name
data.dir <- "download-nsch-data"
(year.do.vec <- Sys.glob(file.path(data.dir, "*.do")))
year.do.some <- grep("2019|2020", year.do.vec, value=TRUE)
out.dt.list <- list()
for(year.do in year.do.some){
  year.dta <- sub("do$", "dta", year.do)
  year.tib <- haven::read_dta(year.dta)
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
  define.not.missing <- define.dt[
    !grepl("[.]", value)
  ][
  , value.int := as.integer(value)
  ][]
  year.dt <- data.table(
    year.tib
  )[, survey_year := as.integer(sub("_.*", "", sub(".*?_", "", year.do)))]
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
  year.dt[, Autism := k2q35a]
  year.some.cols <- year.dt[, c("survey_year", "Autism", input.meta.dt$column_name), with=FALSE]
  keep <- apply(!is.na(year.some.cols), 1, all)
  out.dt.list[[year.dta]] <- year.some.cols[keep]
}
out.dt <- rbindlist(out.dt.list)
out.dt[, table(survey_year, Autism)]
fwrite(out.dt, "download-nsch-convert-do-2019-2020.csv")
