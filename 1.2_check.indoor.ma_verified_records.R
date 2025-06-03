#' Check Indoor/Outdoor Classification for Mosquito Alert Verified Records
#'
#' This function assesses the agreement between reported locations (from verified Mosquito Alert records) 
#' and estimated indoor/outdoor classifications derived from spatial raster data or photo EXIF metadata.
#'
#' @param db_spatial A `data.frame` or `sf` object containing the spatial records to evaluate.
#'                   Must include `lon`, `lat`, and a `answer_13_1` column for reference classification.
#' @param buil_raster A `SpatRaster` object representing the binary or probabilistic indoor/outdoor classification of the area.
#' @param method Character. One of `"dilated"`, `"buffer"`, or `"photo_exif"`. Defines the method used for classification:
#'               - `"dilated"`: Uses the dilated raster directly for extraction.
#'               - `"buffer"`: Extracts mean raster values within a buffer around each point.
#'               - `"photo_exif"`: Uses GPS data from photo EXIF metadata (requires `photo_folder`).
#' @param n_points Integer (optional). Number of records to sample per iteration. Default `NULL` implies all records.
#' @param n_iter Integer (optional). Number of iterations to run. Default `NULL` implies a single iteration.
#' @param buffer_dist Numeric (optional). Buffer distance (in raster CRS units) for the `"buffer"` method.
#' @param threshold Numeric (optional). Threshold to determine "inside" or "outside" for `"buffer"` method.
#' @param photo_folder Character (optional). Path to folder where photo files are stored. Required for `"photo_exif"` method.
#'
#' @return A `data.frame` summarizing each iteration's sample size and agreement percentage.
#'
#' @examples
#' \dontrun{
#' # Example usage with 'dilated' method
#' results <- check.indoor.ma_verified_records(
#'   db_spatial = records_df,
#'   buil_raster = raster_layer,
#'   method = "dilated",
#'   n_points = 500,
#'   n_iter = 10
#' )
#'
#' # Example usage with 'buffer' method
#' results <- check.indoor.ma_verified_records(
#'   db_spatial = records_df,
#'   buil_raster = raster_layer,
#'   method = "buffer",
#'   n_points = 500,
#'   n_iter = 10,
#'   buffer_dist = 20,
#'   threshold = 0.5
#' )
#'
#' # Example usage with 'photo_exif' method
#' results <- check.indoor.ma_verified_records(
#'   db_spatial = records_df,
#'   buil_raster = raster_layer,
#'   method = "photo_exif",
#'   n_points = 500,
#'   n_iter = 10,
#'   photo_folder = "path/to/photos"
#' )
#' }
#'
#' @export

check.indoor.ma_verified_records <- function(
    db_spatial,
    buil_raster,
    method = c("dilated", "buffer", "photo_exif"),
    n_points = NULL,
    n_iter = NULL,
    buffer_dist = NULL,
    threshold = NULL,
    photo_folder = NULL
) {
  
  
  ####LOADING PACKAGES USED####
  required_packages <- c("dplyr", "sf", "terra", "exifr", "stringr")
  new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) {
    install.packages(new_packages)
  }
  lapply(required_packages, library, character.only = TRUE)
  
  
  method <- match.arg(method)
  
  #Check if sf or else convert
  if (!inherits(db_spatial, "sf")) {
    db_spatial <- sf::st_as_sf(db_spatial, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  }
  
  if (!"answer_13_1" %in% names(db_spatial)) {
    stop("The data must include a 'answer_13_1' column for comparison.")
  }
  
  results <- vector("list", n_iter)
  
  for (i in seq_len(n_iter)) {
    replace_sampling <- nrow(db_spatial) < n_points
    
    if (method == "dilated") {
      db_sample <- db_spatial %>% dplyr::sample_n(n_points, replace = replace_sampling)
      points_vect <- terra::vect(sf::st_transform(db_sample, crs(buil_raster)))
      vals <- terra::extract(buil_raster, points_vect)[, 2]
      
      db_sample$estimated_category <- ifelse(vals == 1, "Inside a building", "Outdoors")
      db_sample$agreement <- (db_sample$estimated_category == db_sample$answer_13_1)
      n_agree <- sum(db_sample$agreement, na.rm = TRUE)
      n_total <- sum(!is.na(db_sample$agreement))
      percent_agreement <- ifelse(n_total == 0, NA_real_, (n_agree / n_total) * 100)
      res <- data.frame(iteration = i, sample_size = n_total, agreement_percentage = percent_agreement)
      
    } else if (method == "buffer") {
      db_sample <- db_spatial %>% dplyr::sample_n(n_points, replace = replace_sampling)
      db_sample <- sf::st_transform(db_sample, crs = buil_raster)
      coverage_vals <- numeric(nrow(db_sample))
      
      for (j in seq_len(nrow(db_sample))) {
        buffer_geom <- sf::st_buffer(db_sample[j, ], dist = buffer_dist)
        buffer_vect <- terra::vect(buffer_geom)
        cov <- terra::extract(buil_raster, buffer_vect, fun = mean, na.rm = TRUE)[, 2]
        coverage_vals[j] <- ifelse(is.na(cov), NA, cov)
      }
      
      db_sample$coverage <- coverage_vals
      db_sample$estimated_category <- ifelse(db_sample$coverage >= threshold, "Inside a building", "Outdoors")
      db_sample$agreement <- (db_sample$estimated_category == db_sample$answer_13_1)
      n_agree <- sum(db_sample$agreement, na.rm = TRUE)
      n_total <- sum(!is.na(db_sample$agreement))
      percent_agreement <- ifelse(n_total == 0, NA_real_, (n_agree / n_total) * 100)
      res <- data.frame(iteration = i, sample_size = n_total, agreement_percentage = percent_agreement)
      
    } else if (method == "photo_exif") {
      if (is.null(photo_folder)) stop("Please provide 'photo_folder' for photo_exif method.")
      res <- check_inside_building2(db_spatial, buil_raster, folder = photo_folder, n_photos = n_points)
      res$iteration <- i
    }
    
    results[[i]] <- res
    cat(paste("Iteration:", i, "- Sample Size:", res$sample_size, "- Agreement:", res$agreement_percentage, "%\n"))
  }
  
  results_df <- do.call(rbind, results)
  
  #Calculate weighted mean and confidence interval
  weighted_mean <- sum(results_df$sample_size * results_df$agreement_percentage, na.rm = TRUE) /
    sum(results_df$sample_size, na.rm = TRUE)
  w <- results_df$sample_size / sum(results_df$sample_size, na.rm = TRUE)
  mean_diff_sq <- (results_df$agreement_percentage - weighted_mean)^2
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