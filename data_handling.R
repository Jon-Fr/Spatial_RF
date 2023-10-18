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
test_area = terra::vect("test_area.gpkg")

# Clip training data points
subset_dp = terra::crop(spv_dp, test_area)

# Convert the spatial vector to a df
subset_dp = as.data.frame(subset_dp, geom = "XY")

# save the subset
save(subset_dp, file = "data_points_subset")
##
## End (subset training data for testing)
#### 
################################################################################
## End (data handling)
################################################################################
