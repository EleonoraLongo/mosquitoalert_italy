#' Check Spatial Correlation for Mosquito Alert Verified Records
#'
#' This function assesses the spatial correlation between sampling effort (SE) and record counts
#' for each Tigacell, using a negative binomial model with random intercept and Moran's I test.
#'
#' @param records A `data.frame` of verified records. Must contain `TigacellID` and `SE` columns.
#' @param grid An `sf` object representing the Tigacell grid. Must contain a `TigacellID` column.
#' @param k_neighbors Integer. Number of nearest neighbors to use in the spatial weights matrix (default = 4).
#' @param chains Integer. Number of MCMC chains to run for the Bayesian model (default = 4).
#' @param cores Integer. Number of cores for parallel computation (default = 4).
#' @param seed Integer. Random seed for reproducibility (default = 42).
#'
#' @return A list containing:
#'   \item{model}{The fitted `brms` model object.}
#'   \item{moran_test}{The Moran's I test result for residuals.}
#'   \item{traceplot}{Trace plot of MCMC chains.}
#'   \item{area_plot}{Posterior intervals for fixed effects.}
#'   \item{effect_plot}{Predicted vs observed plot for SE effect.}
#'   \item{moran_plot}{Moran scatterplot of residuals and lag residuals.}
#'   \item{cell_data}{The final data frame used for modeling.}
#'
#' @examples
#' \dontrun{
#' result <- check.spatial.cor.ma_verified_records(
#'   records = records_df,
#'   grid = grid_sf,
#'   k_neighbors = 4,
#'   chains = 4,
#'   cores = 4,
#'   seed = 123
#' )
#' print(result$moran_test)
#' plot(result$traceplot)
#' plot(result$effect_plot)
#' }
#'
#' @export

check.spatial.cor.ma_verified_records <- function(
    records = NULL, 
    grid = NULL, 
    k_neighbors = NULL, 
    chains = NULL, 
    cores = NULL, 
    seed = NULL
    ) {
  
  ####LOADING PACKAGES####
  required_packages <- c("dplyr", "sf", "spdep", "brms", "bayesplot", "ggplot2", "tibble")
  new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) install.packages(new_packages)
  lapply(required_packages, library, character.only = TRUE)
  
  #Standardize sampling effort (SE)
  records <- records %>%
    dplyr::mutate(standard_SE = (SE - mean(SE, na.rm = TRUE)) / sd(SE, na.rm = TRUE))
  
  #Count records and mean SE by Tigacell
  cell_counts <- records %>%
    dplyr::group_by(TigacellID) %>%
    dplyr::summarise(
      n_records = n(),
      standard_SE_mean = mean(standard_SE, na.rm = TRUE),
      .groups = "drop"
    )
  
  #Bind with grid and NA removal
  cell_data <- grid %>%
    dplyr::left_join(cell_counts, by = "TigacellID") %>%
    dplyr::filter(!is.na(n_records), !is.na(standard_SE_mean))
  
  #Centroids and coordinates of spatial weights
  centroids <- sf::st_centroid(cell_data)
  coords <- sf::st_coordinates(centroids)
  knn <- spdep::knearneigh(coords, k = k_neighbors)
  nb <- spdep::knn2nb(knn)
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  
  #brms model
  model_tiga <- brms::brm(
    formula = n_records ~ standard_SE_mean + (1 | TigacellID),
    data = cell_data,
    family = negbinomial(),
    chains = chains, cores = cores, seed = seed
  )
  
  #Residuals and Moran I test
  res_pearson <- residuals(model_tiga, type = "pearson")[, "Estimate"]
  moran_res <- spdep::moran.test(res_pearson, lw, zero.policy = TRUE)
  
  #Posterior draws e plots
  posterior <- brms::as_draws_df(model_tiga)
  
  traceplot <- bayesplot::mcmc_trace(posterior, pars = c("b_Intercept", "b_standard_SE_mean"))
  area_plot <- bayesplot::mcmc_areas(posterior, pars = c("b_Intercept", "b_standard_SE_mean"), prob = 0.95)
  
  df_pred <- brms::posterior_epred(model_tiga) %>%
    apply(2, median) %>%
    tibble::enframe(name = "obs", value = "yhat") %>%
    dplyr::mutate(standard_SE_mean = cell_data$standard_SE_mean,
                  observed = cell_data$n_records)
  
  effect_plot <- ggplot2::ggplot(df_pred, ggplot2::aes(x = standard_SE_mean, y = observed)) +
    ggplot2::geom_point(alpha = 0.3) +
    ggplot2::geom_line(ggplot2::aes(y = yhat), color = "red", linewidth = 0.1) +
    ggplot2::labs(x = "Mean standard_SE", y = "N° of records") +
    ggplot2::theme_bw()
  
  #Residual lags
  lag_resid <- spdep::lag.listw(lw, res_pearson, zero.policy = TRUE)
  db_moran <- dplyr::tibble(resid = res_pearson, lag_resid = lag_resid)
  
  moran_plot <- ggplot2::ggplot(db_moran, ggplot2::aes(x = resid, y = lag_resid)) +
    ggplot2::geom_point(alpha = 0.4) +
    ggplot2::geom_smooth(method = "lm", color = "red", se = FALSE) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::labs(x = "Pearson residuals", y = "Spatial lag of residuals") +
    ggplot2::theme_bw()
  
  #print Moran I test
  print(moran_res)
  
  #Results
  return(list(
    model = model_tiga,
    moran_test = moran_res,
    traceplot = traceplot,
    area_plot = area_plot,
    effect_plot = effect_plot,
    moran_plot = moran_plot,
    cell_data = cell_data
  ))
}