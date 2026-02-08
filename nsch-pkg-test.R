fwrite_list <- function(data_list, year_dir, verbose=FALSE){
  for(data_type in names(data_list)){
    type_dt <- data_list[[data_type]]
    out.csv <- file.path(
      year_dir,
      sprintf("%s.csv", data_type))
    if(verbose)cat(sprintf(
      "writing %s %d×%d\n",
      out.csv, nrow(type_dt), ncol(type_dt)))
    data.table::fwrite(type_dt, out.csv)
  }
}

parse_dta <- function(f)list(surveys=haven::read_stata(f))

Stata2csv <- function(Stata_dir, csv_dir, year, verbose=FALSE){
  file_list <- list(
    "nsch_%d_topical.do"=parse_do,
    "nsch_%de_topical.dta"=parse_dta)
  year_dir <- file.path(csv_dir, year)
  if(verbose)cat(sprintf("converting %s to %s\n", Stata_dir, year_dir))
  dir.create(year_dir, showWarnings=FALSE, recursive=TRUE)
  for(fmt in names(file_list)){
    read_fun <- file_list[[fmt]]
    data_file <- file.path(
      Stata_dir,
      sprintf(fmt, year))
    if(verbose)cat(sprintf("reading %s\n", data_file))
    data_list <- read_fun(data_file)
    fwrite_list(data_list, year_dir, verbose)
  }
}

get_year <- function(year_url, data.dir=tempdir()){
  year.html <- basename(year_url)
  data.dir.year.html <- file.path(data.dir, year.html)
  if(!file.exists(data.dir.year.html)){
    download.file(year_url, data.dir.year.html)
  }
  url.dt <- nc::capture_all_str(
    data.dir.year.html,
    url="//.*?topical_Stata[.]zip")
  if(nrow(url.dt)==0)
    stop("no topical_Stata.zip urls on ", year_url)
  http_url <- paste0("http:", url.dt$url)
  year.zip <- basename(http_url)
  data.dir.year.zip <- file.path(data.dir, year.zip)
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


remotes::install_github("NAU-ASD3/nsch@download-funs")
data_dir <- "nsch-pkg-test"
original_Stata <- file.path(data_dir, "00_original_Stata")
dir.create(original_Stata, showWarnings = FALSE, recursive = TRUE)
csv_dir <- file.path(data_dir, "01_original_csv")
index_dt <- nsch::get_nsch_index(file.path(original_Stata, "datasets.html"))
for(year_i in 1:nrow(index_dt)){
  index_row <- index_dt[year_i]
  get_year(index_row$url, original_Stata)
  Stata2csv(
    original_Stata, csv_dir,
    index_row$year, verbose=TRUE)
}

