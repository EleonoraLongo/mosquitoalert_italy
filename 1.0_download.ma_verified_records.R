#' Download Mosquito Alert public records data from GitHub repository
#' 
#' @param destination.folder Character string. The choosen destination folder path.
#' @param zenodo.file Character, allrecordsxxxx.json, where xxxx is the year. The .json file to be exctracted.
#' @param month Character, 1, 2, 3, ..., 12. Month to filter the .json file.
#' @param nation Character, A-3 ISO country code. Nation to filter the .json file.
#' @param report.type Character, adult, bite, site. Type of records to filter the .json file.
#' @returns An object inside the environment with the records from the .json file of interest.
#' @examples 
#' \dontrun{
#' download.ma_verified_records(
#'             destination.folder = getwd(),
#'             years = 2023
#' )
#'
#' download.ma_verified_records(
#'             years = 2023:2024,
#'             month = 2,
#'             nation = "ITA",
#'             report.type = "adult")
#' }

download.ma_verified_records <- function(
    destination.folder = NULL, 
    years = NULL, 
    month = NULL,
    nation = NULL,
    report.type = NULL
    ) {
  
  ####LOADING PACKAGES USED####
  required_packages <- c("httr", "utils", "jsonlite", "dplyr", "tidyr", "purrr")
  
  #install if not present
  new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
  if(length(new_packages)) install.packages(new_packages)
  
  #loading packages
  lapply(required_packages, library, character.only = TRUE)
  
  ####FOLDER WHERE TO DOWNLOAD IF NEEDED####
  if(!is.null(destination.folder)) {
    if(!dir.exists(destination.folder)) {
      dir.create(destination.folder, recursive = TRUE)
    }
  }
  
  ####DOWNLOAD (temporary memory)####
  #complete path for file download (all reports)
  zip.url <- "https://github.com/MosquitoAlert/Data/archive/refs/heads/master.zip"
  res <- httr::GET(zip.url)
  httr::stop_for_status(res)
  raw_zip <- httr::content(res, "raw")
  
  #temporary file 
  temp_zip <- tempfile(fileext = ".zip")
  writeBin(raw_zip, temp_zip)
  
  #list of contents
  zip_list <- utils::unzip(temp_zip, list = TRUE)$Name
  
  #all_records content access
  inner_zip_path <- grep("^Data-master/all_reports\\.zip$", zip_list, value = TRUE)
  if (length(inner_zip_path) != 1) {
    stop("File all_reports.zip not found in master.zip")
  }
  
  #all_records temporary file
  temp_inner_dir <- tempfile()
  dir.create(temp_inner_dir)
  utils::unzip(temp_zip, files = inner_zip_path, exdir = temp_inner_dir)
  temp_inner_zip <- file.path(temp_inner_dir, inner_zip_path)
  
  #list of all_records content
  inner_list <- utils::unzip(temp_inner_zip, list = TRUE)$Name
  
  ####YEAR and FILTERS####
  yrs_path <- paste(years, collapse = "|")
  file_path   <- paste0(".*/all_reports(", yrs_path, ")\\.json$")
  to_extract <- grep(file_path, inner_list, value = TRUE)
  if (length(to_extract) == 0) {
    stop("No all_reportsYYYY.json found in all_reports.zip for requested years")
  }
  
  #extract, save (optional) and parse
  df_list <- purrr::map(to_extract, function(fname) {
    utils::unzip(
      zipfile = temp_inner_zip,
      files   = fname,
      exdir   = temp_inner_dir,
      junkpaths = TRUE
    )
    local_name <- basename(fname)  
    fpath      <- file.path(temp_inner_dir, local_name)
    
    #save local copy if needed
    if (!is.null(destination.folder)) {
      file.copy(fpath,
                file.path(destination.folder, local_name),
                overwrite = TRUE)
    }
    
    #parse
    jsonlite::fromJSON(fpath, flatten = TRUE) %>%
      as_tibble()
  })
  
  #bind and apply filters
  records <- dplyr::bind_rows(df_list)
  if (!is.null(month)) records <- filter(records, creation_month %in% month)
  if (!is.null(nation)) records <- filter(records, country %in% nation)
  if (!is.null(report.type)) records <- filter(records, type %in% report.type)
  records <- tidyr::drop_na(records, lat, lon)
  
  return(records)
}
