#' Process User Locations for Mosquito Alert Verified Records
#'
#' This function processes user locations for verified records, filtering by user participation IDs, 
#' calculating time-related variables (week, day of year, biweek), and optionally saving outputs.
#'
#' @param user_locations An `sf` object or `data.frame` containing user locations. Must include `user_coverage_uuid` and `fix_date`.
#' @param user_participation A `data.frame` containing user participation data. Must include `user_coverage_uuid`.
#' @param save_path Optional. Character string specifying directory to save separate yearly files. If NULL, files are not saved.
#'
#' @return Creates separate dataframes for each year in the global environment and optionally saves them as RDS files.
#'
#' @examples
#' \dontrun{
#' user.locations.ma_verified_records(
#'   user_locations = user_locations_df,
#'   user_participation = participation_df,
#'   save_path = "path/to/save"
#' )
#' }
#'
#' @export

user.locations.ma_verified_records <- function(
    user_locations = NULL, 
    user_participation = NULL,
    save_path = NULL
) {
  
  ####LIBRARIES####
  required_packages <- c("lubridate", "dplyr", "sf")
  new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(required_packages, library, character.only = TRUE)
  
  #####estrai user ids da user_participation#####
  ID_filtered <- unique(user_participation$user_coverage_uuid)
  
  #####filtro per gli utenti selezionati#####
  user_locations <- user_locations %>%
    dplyr::filter(user_coverage_uuid %in% ID_filtered) %>%
    dplyr::mutate(
      year = lubridate::year(fix_date),
      week = lubridate::week(fix_date),
      yday = lubridate::yday(fix_date),
      biweek = ceiling(pmin(yday, 364) / 14),
      participation_days = round(participation_days)
    )
  
  #####rinomina TigacellID se necessario#####
  if ("TigacellID.x" %in% names(user_locations)) {
    user_locations <- user_locations %>%
      dplyr::rename(TigacellID = TigacellID.x)
  }
  
  #####divide per anno#####
  user_locations_split <- split(user_locations, user_locations$year)
  
  #####assegna ogni anno come oggetto separato nell’environment globale#####
  for (yr in names(user_locations_split)) {
    varname <- paste0("user_locations_", yr)
    assign(varname, user_locations_split[[yr]], envir = .GlobalEnv)
    cat(paste0("Created object in environment: ", varname, "\n"))
    
    if (!is.null(save_path)) {
      dir.create(save_path, showWarnings = FALSE, recursive = TRUE)
      file_path <- file.path(save_path, paste0(varname, ".Rds"))
      saveRDS(user_locations_split[[yr]], file_path)
      cat(paste0("Saved: ", file_path, "\n"))
    }
  }
  
  cat("Done! All user_locations objects are now in the global environment.\n")
}