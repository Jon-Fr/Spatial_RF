################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sp")
p_load("sf")
p_load("terra")

# Fewer decimal places
options(digits=4)

# Load data
load("run_nitrate_de.rda")
################################################################################
## End (preparation)
################################################################################


################################################################################
## Data handling
################################################################################
###
## Export training data points
##

# Convert the data df to a spatial vector
spv_dp = terra::vect(d, geom=c("X", "Y"))

# Set crs
terra::crs(spv_dp) <- "EPSG:25832"

# Write file
terra::writeVector(spv_dp, "data_points.gpkg")
##
## End (export trainings data points)
####


####
## Subset training data for testing
##

# Load test area file
test_area = terra::vect("test_area_small.gpkg")

# Clip training data points
subset_dp = terra::crop(spv_dp, test_area)

# Convert the spatial vector to a df
subset_dp = as.data.frame(subset_dp, geom = "XY")

# Save the subset with the formula
save(subset_dp, fo, file = "data_points_subset2.rda")
##
## End (subset training data for testing)
#### 
################################################################################
## End (data handling)
################################################################################
