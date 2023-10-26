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
## Get the mean and median prediction distance 
##

mean_med_predDist = function(path_predArea, dataPoints_df, c_r_s){
  # Load the prediction area file
  pred_area = terra::vect(path_predArea)
  
  # Create raster with same extent as the prediction area
  raster_pl = terra::rast(pred_area, resolution = 20,
                          xmin = xmin(pred_area), xmax = xmax(pred_area), 
                          ymin = ymin(pred_area), ymax = ymax(pred_area))
  # Set crs
  terra::crs(raster_pl) = c_r_s
  
  # Set NA to 1
  raster_pl = terra::subst(raster_pl, NA, 1)
  
  # Convert the data points df to a spatial vector
  spv_dp = terra::vect(dataPoints_df, geom=c("x", "y"), crs = c_r_s)
  
  # Calculate the distance
  dist_dp_pl = terra::distance(raster_pl, spv_dp, unit="m")
  
  # Set raster cell values to NA that where NA in the original raster
  dist_dp_pl = terra::mask(dist_dp_pl, raster_pl)
  
  # Raster to df
  dist_dp_pl_df = terra::as.data.frame(dist_dp_pl)
  
  # Get the mean prediction distance 
  mean_pd = mean(x = dist_dp_pl_df[,1], na.rm=TRUE)
  median_pd =  median(x = dist_dp_pl_df[,1], na.rm=TRUE)
  
  # Remove some not longer necessary data
  remove("dist_dp_pl_df", "dist_dp_pl", "raster_pl")
  
  # Return results
  results = list(mean_predDist = mean_pd, med_predDist = median_pd)
  return(results)
}
##
## End (Get the mean and median prediction distance)
####
################################################################################
## End (functions)
################################################################################
