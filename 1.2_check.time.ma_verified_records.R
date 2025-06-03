#' Check Time Agreement for Mosquito Alert Verified Records
#'
#' This function assesses the agreement between the creation time of verified records 
#' and the capture time extracted from associated photo EXIF metadata.
#'
#' @param records A `data.frame` containing verified Mosquito Alert records. 
#'                Must include a `creation_time` column (POSIXct format) and a `movelab_annotation.photo_html` column for the photo URLs.
#' @param n_photos Integer (optional). Number of records to sample per iteration. If NULL, uses all records.
#' @param time_unit Character. One of `"minutes"`, `"days"`, or `"weeks"`. Defines the temporal unit for agreement threshold.
#' @param time_value Numeric. The value of the threshold in the specified `time_unit` (e.g., 60 for 60 minutes).
#' @param folder Character. Path to the local folder where photos will be downloaded.
#' @param n_iter Integer. Number of iterations for sampling and agreement calculation.
#'
#' @return A list with two elements:
#'   \item{results}{A `data.frame` summarizing each iteration's sample size and agreement percentage.}
#'   \item{summary}{A `data.frame` containing the weighted mean agreement and its 95% confidence interval.}
#'
#' @examples
#' \dontrun{
#' results <- check.time.ma_verified_records(
#'   records = records_df,
#'   n_photos = 500,
#'   time_unit = "minutes",
#'   time_value = 60,
#'   folder = "path/to/download/photos",
#'   n_iter = 10
#' )
#'
#' print(results$results)
#' print(results$summary)
#' }
#'
#' @export

check.time.ma_verified_records <- function(
    records = NULL,
    n_photos = NULL,
    time_unit = c("minutes", "days", "weeks"),
    time_value = NULL,        
    folder = NULL,
    n_iter = NULL
) {
  
  ####LOADING PACKAGES####
  required_packages <- c("dplyr", "stringr", "exifr")
  new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) {
    install.packages(new_packages)
  }
  lapply(required_packages, library, character.only = TRUE)
  
  
  time_unit <- match.arg(time_unit)
  
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
  
  base_url <- "https://webserver.mosquitoalert.com"
  results <- vector("list", n_iter)
  
  for (iter in seq_len(n_iter)) {
    n_photos_use <- min(n_photos, nrow(records))
    
    db_sample <- records %>%
      dplyr::sample_n(n_photos_use, replace = FALSE) %>%
      dplyr::mutate(
        photo_url_relative = stringr::str_extract(movelab_annotation.photo_html, '/media/tigapics/[^"]+'),
        full_photo_url = dplyr::if_else(is.na(photo_url_relative), NA_character_, paste0(base_url, photo_url_relative)),
        local_path = dplyr::if_else(is.na(photo_url_relative), NA_character_, file.path(folder, basename(photo_url_relative)))
      )
    
    #Download photos
    for (i in seq_len(nrow(db_sample))) {
      if (!is.na(db_sample$full_photo_url[i]) && !file.exists(db_sample$local_path[i])) {
        tryCatch(download.file(db_sample$full_photo_url[i], db_sample$local_path[i], mode = "wb"),
                 error = function(e) message("Errore download: ", db_sample$full_photo_url[i]))
      }
    }
    
    agreement_vec <- logical(nrow(db_sample))
    
    for (i in seq_len(nrow(db_sample))) {
      if (is.na(db_sample$local_path[i]) || !file.exists(db_sample$local_path[i])) {
        agreement_vec[i] <- NA
        next
      }
      
      exif_data <- tryCatch(exifr::read_exif(db_sample$local_path[i]), error = function(e) NULL)
      
      if (is.null(exif_data) || nrow(exif_data) == 0) {
        agreement_vec[i] <- NA
        next
      }
      
      #Check if columns exists and take correct values
      exif_value <- NA
      if ("DateTimeOriginal" %in% colnames(exif_data) && !is.na(exif_data$DateTimeOriginal[1])) {
        exif_value <- exif_data$DateTimeOriginal[1]
      } else if ("CreateDate" %in% colnames(exif_data) && !is.na(exif_data$CreateDate[1])) {
        exif_value <- exif_data$CreateDate[1]
      }
      
      if (is.na(exif_value) || exif_value == "") {
        agreement_vec[i] <- NA
        next
      }
      
      exif_time <- as.POSIXct(exif_value, format = "%Y:%m:%d %H:%M:%S", tz = "UTC")
      creation_t <- db_sample$creation_time[i]
      
      if (is.na(creation_t) || is.na(exif_time)) {
        agreement_vec[i] <- NA
        next
      }
      
      diff_min <- abs(as.numeric(difftime(exif_time, creation_t, units = "mins")))
      threshold_min <- switch(time_unit,
                              "minutes" = time_value,
                              "days" = time_value * 24 * 60,
                              "weeks" = time_value * 7 * 24 * 60)
      
      agreement_vec[i] <- (!is.na(diff_min) && diff_min <= threshold_min)
    }
    
    n_agree <- sum(agreement_vec, na.rm = TRUE)
    n_total <- sum(!is.na(agreement_vec))
    percent_agreement <- ifelse(n_total == 0, NA_real_, (n_agree / n_total) * 100)
    
    results[[iter]] <- data.frame(iteration = iter, sample_size = n_total, agreement_percentage = percent_agreement)
    cat(sprintf("Iteration %d - Sample Size: %d - Agreement: %.2f%%\n", iter, n_total, percent_agreement))
  }
  
  #Bind results
  results_df <- do.call(rbind, results)
  
  #Calculate weighted average and confidence interval
  weighted_mean <- sum(results_df$sample_size * results_df$agreement_percentage, na.rm = TRUE) / 
    sum(results_df$sample_size, na.rm = TRUE)
  w <- results_df$sample_size / sum(results_df$sample_size, na.rm = TRUE)
  mean_diff_sq <- (results_df$agreement_percentage - weighted_mean) ^ 2
  weighted_variance <- sum(w * mean_diff_sq, na.rm = TRUE)
  weighted_sd <- sqrt(weighted_variance)
  
  valid_iter <- sum(!is.na(results_df$agreement_percentage))
  ci_lower <- weighted_mean - 1.96 * (weighted_sd / sqrt(valid_iter))
  ci_upper <- weighted_mean + 1.96 * (weighted_sd / sqrt(valid_iter))
  
  cat(sprintf("\nWeighted Mean Agreement: %.2f%%\n", weighted_mean))
  cat(sprintf("95%% CI: [%.2f%%, %.2f%%]\n", ci_lower, ci_upper))
  
  return(list(
    results = results_df,
    summary = data.frame(
      weighted_mean = weighted_mean,
      ci_lower = ci_lower,
      ci_upper = ci_upper
    )
  ))
}

