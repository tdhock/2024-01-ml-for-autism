data.dir <- "download-ncsh-data"
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

one.dta <- haven::read_dta("download-ncsh-data/nsch_2018_topical.dta")
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
data.table::fwrite(name.counts, "download-ncsh-column-counts.csv")
data.table::fwrite(dim.dt, "download-ncsh-nrow-ncol.csv")
common.cols <- name.counts[years==max(years), column]
str(data.list[[1]][, common.cols], list.len=length(common.cols))
