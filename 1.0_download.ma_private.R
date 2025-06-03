#' Download and Filter Mosquito Alert Private Data
#'
#' This function downloads private data from Google Drive using a shared file link,
#' extracts a file from a password-protected ZIP archive, identifies and standardizes latitude and longitude columns,
#' optionally filters by bounding box (`bbox`), transforms coordinates, intersects with a provided grid,
#' and optionally saves the processed data as an RDS file.
#'
#' @param grid An optional `sf` object representing the grid for spatial intersection (e.g., national or local grid).
#' @param file_zip_link Character. Google Drive shared link for the ZIP file (e.g., "https://drive.google.com/file/d/FILE_ID/view").
#' @param file_inside_zip_name Optional. Exact name of the file to extract from the ZIP archive (e.g., "user_locations_small_cell.Rds").
#'                             If NULL, the first file found in the ZIP will be used.
#' @param password Character. Password required to extract the ZIP archive.
#' @param bbox Optional. A matrix (2x2) specifying the bounding box with row names `"masked_lon"` and `"masked_lat"` 
#'             and column names `"min"` and `"max"`. Used to spatially filter the data.
#' @param save_result Logical. If TRUE, saves the filtered and intersected data to an RDS file.
#' @param save_path Optional. Full file path for saving the RDS file (e.g., "output_directory/filtered_data.Rds"). Required if `save_result = TRUE`.
#'
#' @return An `sf` object containing the filtered and intersected data, with standardized longitude and latitude columns.
#'
#' @examples
#' \dontrun{
#' grid <- st_read("path/to/grid.gpkg")
#' link <- "https://drive.google.com/xxxxxxxx"
#' bbox <- matrix(c(6.5, 18.55, 35.49, 47.1),
#'                ncol = 2, nrow = 2, byrow = TRUE,
#'                dimnames = list(c("masked_lon", "masked_lat"), c("min", "max")))
#' password <- "your_password"
#'
#' result <- download.ma_private(
#'   grid = grid,
#'   file_zip_link = link,
#'   file_inside_zip_name = "user_locations_small_cell.Rds",
#'   password = password,
#'   bbox = bbox,
#'   save_result = TRUE,
#'   save_path = "output_directory/user_locations_filtered.Rds"
#' )
#' }
#'
#' @export

download.ma_private <- function(
    grid = NULL,  
    file_zip_link = NULL,   
    file_inside_zip_name = NULL,
    password = NULL,
    bbox = NULL,  
    save_result = FALSE, 
    save_path = NULL
) {
  
  ####LIBRARIES####
  required_packages <- c("googledrive", "dplyr", "sf","stringr")
  new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(required_packages, library, character.only = TRUE)
  
  ####AUTHENTICATE####
  googledrive::drive_auth()
  
  if (is.null(file_zip_link)) stop("You must specify 'file_zip_link'.")
  
  ####EXTRACT FILE ID FROM LINK####
  file_id <- stringr::str_match(file_zip_link, "/d/([a-zA-Z0-9_-]+)")[,2]
  if (is.na(file_id)) stop("Could not extract file ID from the provided link.")
  
  ####DOWNLOAD ZIP FILE####
  zip_file <- tempfile(fileext = ".zip")
  googledrive::drive_download(
    as_id(file_id),
    path = zip_file,
    overwrite = TRUE
  )
  
  ####UNZIP FILE####
  unzip_dir <- tempfile()
  dir.create(unzip_dir)
  cmd <- sprintf('"C:/Program Files/7-Zip/7z.exe" x -p"%s" "%s" -o"%s"', password, zip_file, unzip_dir)
  status <- system(cmd)
  if (status != 0) stop("Extraction failed. Check password and system 7z installation.")
  
  ####FIND FILE INSIDE ZIP####
  files_in_zip <- list.files(unzip_dir, recursive = TRUE, full.names = TRUE)
  if (length(files_in_zip) == 0) stop("No files found in ZIP.")
  
  if (!is.null(file_inside_zip_name)) {
    file_to_extract <- files_in_zip[basename(files_in_zip) == file_inside_zip_name]
    if (length(file_to_extract) == 0) stop("Specified file not found in ZIP.")
    file_to_extract <- file_to_extract[1]
  } else {
    file_to_extract <- files_in_zip[1]  # Default to the first file
  }
  
  ####READ FILE BASED ON EXTENSION####
  ext <- tools::file_ext(file_to_extract)
  message("Reading file: ", basename(file_to_extract))
  
  db <- switch(
    tolower(ext),
    "rds" = readRDS(file_to_extract),
    "csv" = read.csv(file_to_extract),
    "gpkg" = sf::st_read(file_to_extract),
    stop("Unsupported file type: ", ext)
  )
  
  ####DETERMINE LONGITUDE/LATITUDE COLUMNS####
  if (all(c("masked_lon", "masked_lat") %in% colnames(db))) {
    db <- db %>% mutate(lon = masked_lon, lat = masked_lat)
  } else if (all(c("current_location_lon", "current_location_lat", "selected_location_lon", "selected_location_lat") %in% colnames(db))) {
    db <- db %>% mutate(
      lon = ifelse(is.na(current_location_lon), selected_location_lon, current_location_lon),
      lat = ifelse(is.na(current_location_lat), selected_location_lat, current_location_lat)
    ) %>%
      filter(!is.na(lat) & !is.na(lon))
  } else if (all(c("lon", "lat") %in% colnames(db))) {
    message("Using existing 'lat' and 'lon' columns.")
  } else {
    stop("No valid columns for longitude and latitude were found.")
  }
  
  ####FILTER BY BBOX IF PROVIDED####
  if (!is.null(bbox)) {
    db <- db %>%
      filter(lon > bbox["masked_lon", "min"], lon < bbox["masked_lon", "max"],
             lat > bbox["masked_lat", "min"], lat < bbox["masked_lat", "max"])
  }
  
  ####CONVERT TO SF AND TRANSFORM CRS####
  db <- sf::st_as_sf(db, coords = c("lon", "lat"), crs = 4326)
  
  ####INTERSECT WITH GRID IF PROVIDED####
  if (!is.null(grid)) {
    db <- sf::st_crop(db, st_bbox(grid))
    grid <- grid %>% dplyr::select(-matches("^TigacellID$"))  
    db <- sf::st_join(db, grid, left = FALSE)
    if ("TigacellID.x" %in% colnames(db)) {
      db <- db %>%
        rename(TigacellID = TigacellID.x) %>%
        dplyr::select(-TigacellID.y)
    }
  }
  
  ####SAVE RESULT IF REQUESTED####
  if (save_result) {
    if (is.null(save_path)) stop("Please specify 'save_path' if 'save_result = TRUE'.")
    saveRDS(db, save_path)
    message("Saved filtered data to: ", save_path)
  }
  
  ####RETURN RESULT####
  return(db)
}
