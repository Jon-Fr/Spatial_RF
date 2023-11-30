################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sp")
p_load("sf")
p_load("terra")
p_load("sperrorest")
p_load("concaveman")

# Fewer decimal places, apply penalty on exponential notation 
options("scipen"= 999, "digits"=4)

# Load data
load("Data/run_nitrate_de.rda")

d = d
################################################################################
## End (preparation)
################################################################################


################################################################################
## Data points and subsets
################################################################################
####
## Export all data points
##

# Convert the data df to a spatial vector
spv_dp = terra::vect(d, geom=c("X", "Y"))

# Set crs
terra::crs(spv_dp) <- "EPSG:25832"

# Write file
terra::writeVector(spv_dp, "Data/data_points.gpkg")
##
## End (export all data points)
####


####
## Subset data points (based on Hydogeologische Großräume)
##

# Subset based on Hydogeologische Großräume
WuS_SuB = d[d$GR_NAME == "West- und süddeutsches Schichtstufen- und Bruchschollenland",]
NuM_L = d[d$GR_NAME == "Nord- und mitteldeutsches Lockergesteinsgebiet",] 

# Add id column
WuS_SuB$id = 1:nrow(WuS_SuB)
NuM_L$id = 1:nrow(NuM_L)

# Save the subsets with the formula
save(WuS_SuB, fo, file = "Data/WuS_SuB.rda")
save(NuM_L, fo, file = "Data/NuM_L.rda")

# Create local sub subsets
WuS_SuB_sub = sperrorest::partition_kmeans(data = WuS_SuB, 
                               coords = c("X", "Y"), 
                               nfold = 10,
                               repetition = 1, 
                               seed1 = 123, 
                               return_factor = FALSE,
                               balancing_steps = 1, 
                               order_clusters = TRUE)

NuM_L_sub = sperrorest::partition_kmeans(data = NuM_L, 
                             coords = c("X", "Y"), 
                             nfold = 10,
                             repetition = 1, 
                             seed1 = 123, 
                             return_factor = FALSE,
                             balancing_steps = 1, 
                             order_clusters = TRUE)

# Save the local sub subsets
save_local_subsets = function(data, resampling_object, name_fp){
  for (i in 1:length(resampling_object$"1")){
    rows = resampling_object[["1"]][[i]]$test
    sub_subset = data[rows,]
    name = paste(name_fp, i,".rda", sep = "")
    save(sub_subset, fo, file = name)
  }
}

save_local_subsets(data = WuS_SuB, resampling_object = WuS_SuB_sub, 
                   name_fp = "Data/WuS_SuB_sub_")

save_local_subsets(data = NuM_L, resampling_object = NuM_L_sub, 
                   name_fp = "Data/NuM_L_sub_")

##
## End (subset data points (based on Hydogeologische Großräume))
####


####
## Subset data points (with external vector file)
##

# Convert the data df to a spatial vector
spv_dp = terra::vect(d, geom=c("X", "Y"))

# Set crs
terra::crs(spv_dp) <- "EPSG:25832"

# Load test area file
test_area = terra::vect("Data/test_area_small.gpkg")

# Clip training data points
subset_dp = terra::crop(spv_dp, test_area)

# Convert the spatial vector to a df
subset_dp = as.data.frame(subset_dp, geom = "XY")

# Save the subset with the formula
save(subset_dp, fo, file = "Data/data_points_subset2.rda")
##
## End (subset data points (with external vector file))
#### 
################################################################################
## End (Data points and subsets)
################################################################################


################################################################################
## End (data handling (data points and subsets))
################################################################################


################################################################################
## Prediction area
################################################################################

# Load functions
source("auxiliary_functions.R", encoding = "UTF-8")

####
## "Create" prediction areas (concave hull + 1 km buffer) and 
## calculate statistical parameters for the prediction distance 

# Create data vector
dv = c("NuM_L_sub_4")

# Create vector for the results
info_pd_vec = c()

# Loop
for (i in dv){
  # Load data
  load(paste("Data/", i, ".rda", sep = ""))
  d = sub_subset
  
  # "Create" prediction areas
  c_hull_buff(data = d, buffer_dist = 1000, c_r_s = "EPSG:25832", 
              coords_vec = c("X", "Y"), 
              out_path = paste("Data/", i, "_prediction_area", sep = ""))
  
  # Get information about the prediction distance 
  info_pd = info_predDist(path_predArea = paste("Data/", i, "_prediction_area.gpkg", sep = ""), 
                          dataPoints_df = d,
                          c_r_s = "EPSG:25832",
                          resolution = 100,
                          xy = c("X", "Y"))
  # Append results
  info_pd_vec = append(info_pd_vec, info_pd)
}
##
## End ("create" prediction areas (concave hull + 1 km buffer) and 
## calculate statistical parameters for the prediction distance )
####
################################################################################
## End (prediction area)
################################################################################