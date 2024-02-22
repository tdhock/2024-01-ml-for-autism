data.dir <- "download-nsch-data"
data.list <- list()
for(sas7bdat in Sys.glob(file.path(data.dir, "*.sas7bdat"))){
  data.list[[sas7bdat]] <- haven::read_sas(sas7bdat)
}
name.dt.list <- list()
dim.dt.list <- list()
for(sas7bdat in names(data.list)){
  year.dt <- data.list[[sas7bdat]]
  prop.NA <- colMeans(is.na(year.dt))
  name.dt.list[[sas7bdat]] <- data.table(
    sas7bdat,
    column=names(year.dt),
    prop.NA)
  dim.dt.list[[sas7bdat]] <- data.table(
    sas7bdat,
    nrow=nrow(year.dt),
    ncol=ncol(year.dt),
    ncol_no_NA=sum(prop.NA==0))
}
(name.dt <- rbindlist(name.dt.list))
(dim.dt <- rbindlist(dim.dt.list))
(name.counts <- name.dt[, .(
  years=.N,
  min_prop_NA=min(prop.NA),
  max_prop_NA=max(prop.NA)
), by=column][order(-years, max_prop_NA, column)])
data.table::fwrite(name.counts, "download-nsch-column-counts.csv")
data.table::fwrite(dim.dt, "download-nsch-nrow-ncol.csv")
common.cols <- name.counts[years==max(years), column]
str(data.list[[1]][, common.cols], list.len=length(common.cols))
