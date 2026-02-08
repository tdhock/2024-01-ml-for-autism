fwrite_list <- function(data_list, year_dir, verbose=FALSE){
  size_dt_list <- list()
  for(data_type in names(data_list)){
    type_dt <- data_list[[data_type]]
    out.csv <- file.path(
      year_dir,
      sprintf("%s.csv", data_type))
    if(verbose)cat(sprintf(
      "writing %s %d×%d\n",
      out.csv, nrow(type_dt), ncol(type_dt)))
    data.table::fwrite(type_dt, out.csv)
    size_dt_list[[data_type]] <- data.table(
      out.csv,
      data_type,
      rows=nrow(type_dt),
      cols=ncol(type_dt))
  }
  rbindlist(size_dt_list)
}

parse_dta <- function(f)list(surveys=haven::read_stata(f))

Stata2csv <- function(Stata_dir, csv_dir, year, verbose=FALSE){
  file_list <- list(
    "nsch_%d_topical.do"=parse_do,
    "nsch_%de_topical.dta"=parse_dta)
  year_dir <- file.path(csv_dir, year)
  if(verbose)cat(sprintf("converting %s to %s\n", Stata_dir, year_dir))
  dir.create(year_dir, showWarnings=FALSE, recursive=TRUE)
  size_dt_list <- list()
  for(fmt in names(file_list)){
    read_fun <- file_list[[fmt]]
    data_file <- file.path(
      Stata_dir,
      sprintf(fmt, year))
    if(verbose)cat(sprintf("reading %s\n", data_file))
    data_list <- read_fun(data_file)
    fmt_sizes <- fwrite_list(data_list, year_dir, verbose)
    size_dt_list[[fmt]] <- data.table(
      data_file, fmt_sizes)
  }
  rbindlist(size_dt_list)
}

get_year <- function(year_url, data.dir=tempdir()){
  year.html <- basename(year_url)
  data.dir.year.html <- file.path(data.dir, year.html)
  if(!file.exists(data.dir.year.html)){
    download.file(year_url, data.dir.year.html)
  }
  url.dt <- nc::capture_all_str(
    data.dir.year.html,
    url=list(
      "//.*?",
      base="[^/]*topical_Stata[.]zip"))
  if(nrow(url.dt)==0)
    stop("no topical_Stata.zip urls on ", year_url)
  http_url <- paste0("http:", url.dt$url)
  data.dir.year.zip <- file.path(data.dir, url.dt$base)
  if(!file.exists(data.dir.year.zip)){
    download.file(http_url, data.dir.year.zip)
  }
  unzip(data.dir.year.zip, exdir=data.dir)
}

do_patterns <- list(
  var=list(),
  define=list("_lab +", value=".*?"))
parse_do <- function(year.do){
  do_list <- list()
  for(data_type in names(do_patterns)){
    do_list[[data_type]] <- nc::capture_all_str(
      year.do,
      "label ",
      data_type,
      " ",
      variable=".*?",
      do_patterns[[data_type]],
      ' +"',
      desc=".*?",
      '"')
  }
  do_list
}

get_years_csv <- function(data_dir, verbose=FALSE){
  original_Stata <- file.path(data_dir, "00_original_Stata")
  dir.create(original_Stata, showWarnings = FALSE, recursive = TRUE)
  csv_dir <- file.path(data_dir, "01_original_csv")
  index_dt <- nsch::get_nsch_index(file.path(original_Stata, "datasets.html"))
  size_dt_list <- list()
  for(year_i in 1:nrow(index_dt)){
    index_row <- index_dt[year_i]
    get_year(index_row$url, original_Stata)
    year_dt <- Stata2csv(
      original_Stata, csv_dir,
      index_row$year, verbose=TRUE)
    size_dt_list[[year_i]] <- data.table(
      index_row, year_dt)
  }
  size_dt <- rbindlist(size_dt_list)
  data.table::fwrite(size_dt, "01_original_sizes.csv")
  size_dt
}

library(data.table)
remotes::install_github("NAU-ASD3/nsch@download-funs")
(size_dt <- get_years_csv("NSCH_data", verbose=TRUE))
dcast(size_dt, year ~ data_type, value.var=c("rows", "cols"))
