#' Create spatial grid from sf points and clip to shape boundary
#'
#' This function generates a regular grid from a set of spatial points (in `sf` format),
#' clips it to a given geographic boundary (shapefile), and assigns a unique TigacellID
#' based on centroid coordinates. Optionally, it allows sampling and saving the resulting grid.
#'
#' @param input_points An `sf` object with POINT geometry. These are the spatial points used to generate the grid.
#' @param shape_file_path Character. Path to the shapefile (.shp or .gpkg) used to crop the grid.
#' @param epsg_code Integer. EPSG code to assign/project the CRS of spatial data.
#' @param sample_by Optional character. Column name used for group-wise sampling before grid generation.
#' @param sample_n Optional integer. Number of points to sample per group and/or globally.
#' @param save_path Optional character. File path where the resulting grid will be saved (e.g., `path/grid.gpkg`).
#'
#' @return A `sf` object containing the grid polygons clipped to the shape, each with a `TigacellID` column.
#'
#' @examples
#' \dontrun{
#' grid <- create_grid(
#'   input_points = points,
#'   shape_file_path = shape,
#'   epsg_code = 4326,
#'   sample_by = "TigacellID",
#'   sample_n = 10000,
#'   save_path = "output/grid_italy.gpkg"
#' )
#' }
#'
#' @export


create_grid <- function(
    input_points = NULL,          
    shape = NULL,       
    epsg_code = NULL,      
    sample_by = NULL,      
    sample_n = NULL,       
    save_path = NULL       
) {
  
  ####LOADING PACKAGES USED####
  required_packages <- c("sf", "dplyr", "sp")
  
  #install if not present
  new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
  if(length(new_packages)) install.packages(new_packages)
  
  #loading packages
  lapply(required_packages, library, character.only = TRUE)
  
  ####INPUT POINTS####
  if (is.character(input_points) && file.exists(input_points)) {
    ext <- tools::file_ext(input_points)
    if (ext == "rds") {
      input_points <- readRDS(input_points)
    } else if (ext == "shp") {
      input_points <- sf::st_read(input_points, quiet = TRUE)
    } else if (ext == "csv") {
      input_points <- read.csv(input_points)
    } else {
      stop("File format not supported")
    }
  }
  
  input_points <- sf::st_transform(input_points, crs = epsg_code)
  
  ####GEOGRAPHIC BOUNDARIES####
  shape <- sf::st_read(shape_file_path, quiet = TRUE)
  
  shape <- sf::st_transform(shape, crs = sf::st_crs(input_points))
  
  #sf conversion
  if (!inherits(input_points, "sf")) {
    stop("input_points must be an sf object with POINT geometry")
  }
  
  #CSR transformation
  input_points <- sf::st_transform(input_points, crs = epsg_code)
  
  #sample
  if (!is.null(sample_by)) {
    input_points <- input_points %>%
      dplyr::group_by(.data[[sample_by]]) %>%
      dplyr::slice_sample(n = sample_n) %>%
      dplyr::ungroup() %>%
      dplyr::slice_sample(n = sample_n, replace = FALSE)
  }
  
  ####COORDINATES EXTRACTION####
  coords_df <- input_points %>%
    dplyr::mutate(
      masked_lon = sf::st_coordinates(.)[,1],
      masked_lat = sf::st_coordinates(.)[,2]
    ) %>%
    dplyr::select(masked_lon, masked_lat)
  
  ####GRID CREATION####
  #spatial conversion
  coords_matrix <- sf::st_coordinates(input_points)
  coords_df <- as.data.frame(coords_matrix) 
  spatial_pts <- sp::SpatialPoints(coords_df, proj4string = sp::CRS(sf::st_crs(input_points)$proj4string))
  
  grid_topology <- sp::points2grid(spatial_pts)
  
  grid_polygons <- sp::as.SpatialPolygons.GridTopology(grid_topology)
  grid_sf <- sf::st_as_sf(grid_polygons)
  
  #give crs if not present
  if (is.na(sf::st_crs(grid_sf))) {
    sf::st_crs(grid_sf) <- sf::st_crs(shape)
  }
  
  grid_sf <- sf::st_transform(grid_sf, sf::st_crs(shape))
  
  #crop with geographic boundaries
  grid_cropped <- sf::st_crop(grid_sf, sf::st_bbox(shape))
  grid_joined <- sf::st_join(grid_cropped, shape, left = FALSE)
  grid_unique <- grid_joined %>% dplyr::distinct(geometry, .keep_all = TRUE)
  
  #grid centroids
  grid_with_centroids <- grid_unique %>%
    dplyr::mutate(centroid = sf::st_centroid(geometry)) %>%
    dplyr::mutate(
      cent_text = sf::st_as_text(centroid),
      TigacellID = gsub(" ", "_", gsub(".*\\((.*)\\)", "\\1", cent_text))
    )
  
  #saving (optional)
  if (!is.null(save_path)) {
    sf::st_write(grid_with_centroids, save_path, delete_layer = TRUE)
  }
  
  return(grid_with_centroids)
}

