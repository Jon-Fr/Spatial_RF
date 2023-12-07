################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sp")
p_load("sf")
p_load("terra")
################################################################################
## End (preparation)
################################################################################


################################################################################
## Functions
################################################################################

#####
## Function to create a prediction distance df and a point distance df 
## (distance between a data point and the nearest point) 
##
info_dist = function(path_predArea, dataPoints_df, c_r_s, resolution,
                             xy){
  
  ## Calculate the prediction distance
  # Load the prediction area file
  pred_area = terra::vect(path_predArea)
  
  # Create raster grid from polygon
  raster_pl = terra::rast(pred_area, resolution = resolution, crs = c_r_s)
    
  # Set crs
  #terra::crs(raster_pl) = c_r_s
  
  # Set NA to 1
  raster_pl = terra::subst(raster_pl, NA, 1)
  
  # Convert the data points df to a spatial vector
  spv_dp = terra::vect(dataPoints_df, geom=xy, crs = c_r_s)
  
  # Calculate the prediction distance
  dist_dp_pl = terra::distance(raster_pl, spv_dp, unit="m")
  
  # Set raster cell values to NA that where NA in the original raster
  dist_dp_pl = terra::mask(dist_dp_pl, raster_pl)
  
  # Raster to df
  dist_dp_pl_df = terra::as.data.frame(dist_dp_pl)
  
  ## Calculate the point distance
  # Create sf object
  sf_df = sf::st_as_sf(dataPoints_df, coords = xy, crs = c_r_s)
  
  # Calculate distance between all points
  dist_dp_mat = st_distance(sf_df)
  
  # Calculate nearest distance
  n_dist = apply(dist_dp_mat, 1, function(x) {
    return(sort(x, partial = 2)[2])})
  
  # Create df 
  n_dist_df = as.data.frame(n_dist)
  
  ## Return list
  results = list(predDist_df = dist_dp_pl_df, n_pointDist_df = n_dist_df)
  
  # Remove some not longer necessary data
  remove("dist_dp_pl_df", "dist_dp_pl", "raster_pl")
  
  return(results)
}
##
## End (Function the a prediction distance df and a point distance df)
####

####
## Function to compute concave hull add a buffer and export it)
##

c_hull_buff = function(data, buffer_dist, c_r_s, coords_vec, out_path){
  # Create sf object from the data
  sf_df = sf::st_as_sf(data, coords = coords_vec, crs = c_r_s)
  # Compute concave hull
  c_hull = concaveman::concaveman(sf_df , concavity = 2, length_threshold = 0)
  # Add buffer
  c_hull_buffered = sf::st_buffer(c_hull, dist = buffer_dist)
  # Export result
  sf::st_write(c_hull_buffered, paste(out_path, ".gpkg", sep = ""))
}
##
## End (function to compute concave hull add a buffer and export it)
####

####
## Box-Cox functions
##
inv_boxcox <- function(x, lambda) {
  if (abs(lambda) < 1e-15) {
    res <- exp(x)
  } else {
    res <- (x*lambda + 1)^(1/lambda)
  }
  res
}

boxcox <- function(x, lambda) {
  if (abs(lambda) < 1e-15) {
    res <- log(x)
  } else {
    res <- (x^lambda - 1) / lambda
  }
  res
}
##
## End (Box-Cox functions)
####
################################################################################
## End (functions)
################################################################################


################################################################################
## Test area
################################################################################

################################################################################
## End (test area)
################################################################################