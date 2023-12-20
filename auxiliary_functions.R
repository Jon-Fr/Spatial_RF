################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sp")
p_load("sf")
p_load("terra")
p_load("sperrorest")
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

####
## Partition leave one out function with buffer that retrains only cases where 
## the distance between the test point and the training points is smaller than 
## the buffer + the tolerance
##
partition_tt_dist <- function(data,
                              coords = c("x", "y"),
                              buffer = 0,
                              tolerance = 100,
                              c_r_s = NULL, 
                              ndisc = nrow(data),
                              return_train = TRUE,
                              repetition = 1) {
  resample <- list()
  index <- seq_len(nrow(data)) # nocov
  
  res <- list()
  for (i in index) {
    if (!is.null(buffer)) {
      di <- sqrt((data[, coords[1]] - data[i, coords[1]])^2 + # nolint
                   (data[, coords[2]] - data[i, coords[2]])^2) # nolint
    }
    train_sel <- numeric()
    
    # leave-one-out with buffer:
    test_sel <- i
    if (return_train) {
      if (is.null(buffer)) {
        train_sel <- seq_len(nrow(data))[-i] # nocov
      } else {
        train_sel <- which(di > buffer)
      }
    }
    if (return_train & (length(train_sel) == 0)) {
      warning(paste0(
        "empty training set in 'partition_disc': 'buffer'", # nocov  #nolint
        " too large?"
      )) # nocov
    }
    res[[as.character(i)]] <- list(train = train_sel, test = test_sel)
  }
  resample[[as.character(1)]] <- res
  
  ## Calculate distance to nearest neighbor
  # Create new resample object for the cases that fulfill the condition 
  resample_new = list()
  for (i in 1:nrow(data)){
    # Create sf object
    sf_df = sf::st_as_sf(data, coords = coords)
    # Calculate distance between test point and training points
    dist_tt = st_distance(sf_df[resample$"1"[[i]]$test,], 
                          sf_df[resample$"1"[[i]]$train,])
    # Get minimum distance between test and train
    min_dist = min(dist_tt)
    # check if the minimum distance is smaller than the buffer + tolerance
    # if it is add this case to the new resample object
    if (min_dist < buffer + tolerance){
      resample_new = append(resample_new, resample$"1"[i])
    }
  }
  resample_new = list(resample_new)
  
  repres <- as.represampling(resample_new) # nolint
  
  return(repres)
}
##
## End (partition leave one out function ...)
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