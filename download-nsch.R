library(data.table)
data.dir <- "download-nsch-data"
dir.create(data.dir, showWarnings = FALSE)
nsch.prefix <- "https://www.census.gov/programs-surveys/nsch/data/datasets."
for(year in 2016:2022){
  year.html <- paste0(year, ".html")
  ##https://www.census.gov/programs-surveys/nsch/data/datasets.2020.html
  u <- paste0(nsch.prefix, year.html)
  dest.html <- file.path(data.dir, year.html)
  if(!file.exists(dest.html))download.file(u, dest.html)
  url.dt <- nc::capture_all_str(
    dest.html,
    'href="',
    url=".*?topical.*?zip",
    '"')
  if(nrow(url.dt)==0)stop("no urls on ", dest.html)
  for(url.i in 1:nrow(url.dt)){
    zip.url <- paste0("http:", url.dt[url.i, url])
    year.zip <- file.path(data.dir, basename(zip.url))
    if(!file.exists(year.zip))download.file(zip.url, year.zip)
    unzip(year.zip, exdir=data.dir)
  }
}

one.dta <- haven::read_dta("download-nsch-data/nsch_2018_topical.dta")
one.dta$k2q33a
?haven::labelled


define.dt[variable=="fipsst"]

NSCH_categories <- fread(
  "NSCH_categories.csv", header=TRUE
)[
, variable := tolower(column_name)
]
NSCH_categories[category=="Output"]
table(year.dt$k2q35a,useNA="always")#114 NA, use this one.
table(year.dt$k2q35b,useNA="always")#29695 NA, ignore.
NSCH_categories[!category%in%c("","yellow","orange","Output","Comorbidity")]
some.categories <- c("Services","Residence","birth")
some.columns <- NSCH_categories[category%in%some.categories]
for(variable in some.columns$variable){
  print(variable)
  print(table(year.dt[[variable]], useNA="always"))
}
(prop.na.vec <- sort(colMeans(is.na(year.dt))))
NSCH_categories[names(prop.na.vec), prop.na := prop.na.vec, on="variable"]
fwrite(NSCH_categories[order(prop.na, category, variable)], "NSCH_categories_NA_counts.csv")
NSCH_categories_TDH <- setkey(fread("NSCH_categories_NA_counts_TDH.csv"), category, column_name)
NSCH_categories_TDH[category!=""]

## label var a2_if  "Imputation Flag for A2 Variables"
## cap label values a2_if a2_if_lab
## label define a2_if_lab  1  "Imputed"
## label define a2_if_lab  0  "Not imputed", add
## label define a2_if_lab  .m "No valid response", add

## label var a1_active  "Adult 1 - Active Duty"
## cap label values a1_active a1_active_lab
## label define a1_active_lab  1  "Never served in the military"
## label define a1_active_lab  2  "Only on active duty for training in the Reserves or National Guard", add
