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
## Function the get some statistical parameters for the prediction distance 
##

info_predDist = function(path_predArea, dataPoints_df, c_r_s, resolution,
                             xy){
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
  
  # Calculate the distance
  dist_dp_pl = terra::distance(raster_pl, spv_dp, unit="m")
  
  # Set raster cell values to NA that where NA in the original raster
  dist_dp_pl = terra::mask(dist_dp_pl, raster_pl)
  
  # Raster to df
  dist_dp_pl_df = terra::as.data.frame(dist_dp_pl)
  
  # Get the max, mean and median prediction distance as well as the standard 
  # deviation and the median absolute deviation
  mean_pd = mean(x = dist_dp_pl_df[,1], na.rm = TRUE)
  sd_pd = sd(x = dist_dp_pl_df[,1], na.rm = TRUE)
  median_pd = median(x = dist_dp_pl_df[,1], na.rm = TRUE)
  mad_pd = mad(x = dist_dp_pl_df[,1], na.rm = TRUE)
  max_pd = max(x = dist_dp_pl_df[,1], na.rm =TRUE)
  # Return list
  results = list(max_predDist = max_pd, mean_predDist = mean_pd, 
                 med_predDist = median_pd,
                 sd_predDist = sd_pd, mad_predDist = mad_pd, 
                 predDist_df = dist_dp_pl_df)
  
  # Remove some not longer necessary data
  remove("dist_dp_pl_df", "dist_dp_pl", "raster_pl")
  
  return(results)
}
##
## End (function the get some statistical parameters for the pd)
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
################################################################################
## End (functions)
################################################################################
