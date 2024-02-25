library(data.table)
library(glmnet)
NSCH_categories_TDH <- setkey(fread(
  "NSCH_categories_NA_counts_TDH.csv"
)[, column_name := tolower(column_name)], column_name)
input.meta.dt <- NSCH_categories_TDH[category!=""]
feature.names.list <- with(input.meta.dt, split(column_name, category))
feature.names.list[["all"]] <- input.meta.dt$column_name
data.dir <- "download-nsch-data"
(year.do.vec <- Sys.glob(file.path(data.dir, "*.do")))
year.do.some <- grep("2019|2020", year.do.vec, value=TRUE)
out.dt.list <- list()
compare.dt.list <- list()
for(year.do in year.do.some){
  year.dta <- sub("do$", "dta", year.do)
  year.tib <- haven::read_dta(year.dta)
  var.csv <- paste0(year.do, ".var.csv")
  if(file.exists(var.csv)){
    var.dt <- fread(var.csv)
  }else{
    var.dt <- nc::capture_all_str(
      year.do,
      "label var ",
      variable=".*?",
      ' +"',
      desc=".*?",
      '"')
    fwrite(var.dt, var.csv)
  }
  setkey(var.dt, variable)
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
  ##options(width=150)
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
  if(FALSE){
    y.tab <- table(y.vec)
    class.weight.list <- list(
      balanced=1/y.tab[paste(y.vec)],
      identity=rep(1, length(y.vec)))
    fit.list <- list()
    for(class.weight.name in names(class.weight.list)){
      class.weight.vec <- class.weight.list[[class.weight.name]]
      fit.list[[class.weight.name]] <- cv.glmnet(
        X.mat, y.vec, class.weight.vec, family="binomial")
    }
    for(class.weight.name in names(fit.list)){
      fit <- fit.list[[class.weight.name]]
      weight.mat <- coef(fit)
      print(weight.mat[as.logical(weight.mat!=0),])
    }
  }
  year.out <- year.keep[, data.table(
    survey_year, Autism, X.mat)]
  out.dt.list[[year.dta]] <- year.out
  get_meta <- function(data.type, dt){
    na.dt <- is.na(dt)
    is.autism <- if(data.type=="raw"){
      dt[["k2q35a"]]==1
    }else{
      dt[["Autism"]]=="Yes"
    }
    data.table(
      data.type,
      nrow=nrow(dt),
      ncol=ncol(dt),
      stop("TODO how many columns before one hot")
      "%Autism"=100*mean(is.autism, na.rm=TRUE),
      "%rowsNA"=100*mean(apply(na.dt, 1, any)),
      "%colsNA"=100*mean(apply(na.dt, 2, any)))
  }
  print(compare.dt.list[[year.dta]] <- data.table(
    year.out[1, .(year=survey_year)], rbind(
      get_meta("raw", year.tib),
      get_meta("processed", year.out))))
}
(compare.dt <- rbindlist(compare.dt.list))

fwrite(compare.dt, "download-nsch-convert-do-compare.csv")
library(xtable)
xt <- xtable(compare.dt, digits=4)
print(xt, type="latex", file="download-nsch-convert-do-compare.tex", include.rownames=FALSE, floating=FALSE)
common.names <- Reduce(intersect, sapply(out.dt.list, names))
out.dt <- rbindlist(lapply(out.dt.list, function(DT)DT[, common.names,with=FALSE]))
out.dt[, table(survey_year, Autism)]
sum(is.na(out.dt))
out.dt[, table(survey_year)]
##fwrite(data.table(column_name=names(out.dt), category=""), "download-nsch-convert-do-2019-2020-366cols-categories.csv")
fwrite(out.dt, "download-nsch-convert-do-2019-2020-366cols.csv")
