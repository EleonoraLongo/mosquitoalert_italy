#' Calculate Sampling Effort (SE) for Mosquito Alert Verified Records
#'
#' This function processes user locations and participation data, integrates them with sampling effort records, 
#' and computes biweekly sampling effort values for each Tigacell and year. The final data includes SE values 
#' integrated with the records dataset.
#'
#' @param user_participation A `data.frame` containing user participation data. Must include `user_coverage_uuid` and `first_fix_time`.
#' @param user_locations_list A named list of `sf` or `data.frame` objects for each year, containing user location data. 
#'                            Each element should contain `user_coverage_uuid`, `fix_date`, `TigacellID`, and `participation_days`.
#' @param sampling_effort A `data.frame` with sampling effort data, including `TigacellID`, `date`, `SE`, and optionally `year`.
#' @param records A `data.frame` of verified records to merge with SE data. Must include `TigacellID`, `biweek`, and `creation_date`.
#'
#' @return A `data.frame` of records with integrated biweekly sampling effort (SE) data.
#'
#' @examples
#' \dontrun{
#' final_data <- SE.ma_verified_records(
#'   user_participation = participation_df,
#'   user_locations_list = list(user_locations_2020, user_locations_2021),
#'   sampling_effort = sampling_effort_df,
#'   records = records_df
#' )
#' }
#'
#' @export

SE.ma_verified_records <- function(
    user_participation = NULL, 
    user_locations_list = NULL, 
    sampling_effort = NULL, 
    records = NULL
) {
  
  ####LIBRARIES####
  required_packages <- c("lubridate", "dplyr", "data.table", "sf")
  new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(required_packages, library, character.only = TRUE)
  
  #####participation#####
  user_participation <- user_participation %>%
    dplyr::mutate(year = lubridate::year(first_fix_time))
  
  ID_filtered <- unique(user_participation$user_coverage_uuid)
  
  #####locations#####
  annual_results <- list()
  
  for (year_name in names(user_locations_list)) {
    user_locations <- user_locations_list[[year_name]]
    
    user_locations <- user_locations %>%
      dplyr::filter(user_coverage_uuid %in% ID_filtered) %>%
      dplyr::mutate(
        year = lubridate::year(fix_date),
        week = lubridate::week(fix_date),
        yday = lubridate::yday(fix_date),
        biweek = ceiling(pmin(yday, 364) / 14),
        participation_days = round(participation_days)
      )
    
    #####sampling effort#####
    sampling_effort <- sampling_effort %>%
      dplyr::mutate(
        date = as.Date(date),
        year = lubridate::year(date)
      )
    
    sampling_effort_year <- sampling_effort %>%
      dplyr::filter(year %in% unique(user_locations$year)) %>%
      dplyr::select(TigacellID, date, SE)
    
    #####merge#####
    merged_data <- merge(user_locations, sampling_effort_year,
                         by.x = c("fix_date", "TigacellID"),
                         by.y = c("date", "TigacellID"),
                         all.x = TRUE)
    
    merged_data <- merged_data %>%
      dplyr::select(biweek, year, participation_days, SE, TigacellID, user_coverage_uuid) %>%
      distinct() %>%
      sf::st_drop_geometry() %>%
      data.table::as.data.table()
    
    annual_results[[year_name]] <- merged_data
  }
  
  #####combine#####
  combined_data <- data.table::rbindlist(annual_results, use.names = TRUE, fill = TRUE)
  
  #####biweek se#####
  biweek_SE <- combined_data[, .(SE = sum(SE, na.rm = TRUE)), by = .(TigacellID, biweek, year)]
  
  #####final db#####
  records <- records %>%
    dplyr::mutate(year = lubridate::year(creation_date))
  
  final_data <- merge(records, biweek_SE,
                      by = c("TigacellID", "biweek", "year"),
                      all.x = TRUE)
  
  #####add mean SE to NA#####
  if ("SE.y" %in% colnames(final_data)) {
    final_data <- final_data[, setdiff(names(final_data), "SE.y")]
  }
  
  if ("SE.x" %in% colnames(final_data)) {
    colnames(final_data)[colnames(final_data) == "SE.x"] <- "SE"
  }
  
  mean_SE <- mean(biweek_SE$SE, na.rm = TRUE)
  final_data$SE <- ifelse(is.na(final_data$SE), mean_SE, final_data$SE)
  
  return(final_data)
}
