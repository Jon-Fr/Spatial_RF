################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sp")
p_load("sf")
p_load("terra")
p_load("sperrorest")
p_load("purrr")
p_load("parallel")
p_load("doParallel")
p_load("foreach")

p_load("ranger")
library(landmap)

# Additional functions that are not included in packages
source("auxiliary_functions.R", encoding = "UTF-8")
source("spdiagnostics-functions.R", encoding = "UTF-8") # Brenning 2022

# Fewer decimal places
options(digits=4)

# Load data and formula (for now use a subset)
load("data_points_subset2.rda")
d = subset_dp

####
## Data and formula preparation
##

# Add layer id column
d$id = paste("layer.",1:nrow(d), sep = "")

# Create a spatial points df 
sp_df = sp::SpatialPointsDataFrame(d[,c("x","y")], d)

# Load prediction area (for now use the test area as prediction area)
pred_area = terra::vect("test_area_small.gpkg")

# Create raster with same extent as the prediction area
raster_pl = terra::rast(pred_area, resolution = 100,
                        xmin = xmin(pred_area), xmax = xmax(pred_area), 
                        ymin = ymin(pred_area), ymax = ymax(pred_area))

# Set crs
terra::crs(raster_pl) = "EPSG:25832"

# Set NA to 1
raster_pl = terra::subst(raster_pl, NA, 1)

# Raster to df
pl_df = terra::as.data.frame(raster_pl, xy = TRUE)

# Df to spatial pixels df
spx_df = SpatialPixelsDataFrame(points=pl_df[c("x","y")],data=pl_df)

# Remove the raster and the df
remove(raster_pl, pl_df)

# Derive buffer distance layers
dist_layers = landmap::buffer.dist(sp_df["bcNitrate"], spx_df[1], 
                                   classes=as.factor(1:nrow(sp_df)))

# Get the layer names for the formula
layer_names = paste(names(dist_layers), collapse="+")

# Set first part of the model formula (simplified for testing)
fo_firstPart = "bcNitrate ~ crestime + cgwn + cgeschw + log10carea + 
                  elevation + cAckerland + log10_gwn + agrum_log10_restime + 
                  Ackerland + lbm_class_Gruenland + lbm_class_Unbewachsen + 
                  lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung +"

# Complete the model formula
fo = as.formula(paste(fo_firstPart, layer_names))

# Overlay points and layers
ov_dp_dl = over(sp_df["bcNitrate"], dist_layers)

# Create df with the data from d and the buffer distance data
final_d = cbind(d, ov_dp_dl)
##
## End (data and formula preparation)
####
################################################################################
## End (preparation)
################################################################################


################################################################################
## Random Forest with distance matrix columns as explanatory variables (RF-DMC) 
## prediction 
################################################################################
################################################################################
## End (RF-DMC) prediction)
################################################################################


################################################################################
## RF-DMC spatial leave one out cross validation 
################################################################################
#####
## Get the mean and median prediction distance 
## (for now use the test area as prediction area)
##

m_m_pd = mean_med_predDist(path_predArea = "test_area_small.gpkg", 
                           dataPoints_df = d, c_r_s = "EPSG:25832")
##
## End (get the mean and median prediction distance)
####


####
## Cross validation
## 

# Create model function 
RF_DMC_fun = function(formula, data, fo_firstPart){
  # Set first part of the model formula (simplified for testing)
  fo_firstPart = fo_firstPart
  # Set the second part of the model formula
  layers_for_formular = paste(data$id, collapse="+")
  # Complete the model formula
  c_formula = as.formula(paste(fo_firstPart, layers_for_formular))
  # Create model
  RF_dcm_model = ranger::ranger(formula = c_formula,
                                data = data)
  return(RF_dcm_model)
}

# Create prediction function
RF_DMC_pred_fun = function(object, newdata){
  RF_dcm_prediction = predict(object = object, 
                              data = newdata)
  return(RF_dcm_prediction$predictions)
}

# Perform the spatial cross-validation
# Future for parallelization
future::plan(future.callr::callr, workers = 10)
sp_cv_RF_DMC = sperrorest::sperrorest(formula = fo, data = final_d, 
                                  coords = c("x","y"), 
                                  model_fun = RF_DMC_fun,
                                  model_args = list(fo_firstPart=fo_firstPart),
                                  pred_fun = RF_DMC_pred_fun,
                                  smp_fun = partition_loo, 
                                  smp_args = list(buffer = m_m_pd$med_predDist))

# Get test RMSE
test_RMSE = sp_cv_RF_DMC$error_rep$test_rmse
test_RMSE
##
## End (cross validation)
#### 
################################################################################
## End (RF-DMC spatial leave one out cross validation) 
################################################################################


################################################################################
## Test area
################################################################################

####
## Explore the relationship between buffer distance and RMSE
##

# Start time measurement
start_time = Sys.time()

# Setup backend to use many processors
totalCores = parallel::detectCores()

# Leave two cores to reduce computer load
cluster = parallel::makeCluster(totalCores[1]-2) 
doParallel::registerDoParallel(cluster)

# explore
test = data.frame(seq(0, 20000, 1000))

test2 = foreach (i = iter(test, by="row"), .combine=c, 
                 .packages = c("sperrorest", "ranger")) %dopar%{
  sp_cv_RF_DMC = sperrorest::sperrorest(formula = fo, data = final_d, 
                                        coords = c("x","y"), 
                                        model_fun = RF_DMC_fun, 
                                        model_args = list(fo_firstPart=
                                                            fo_firstPart),
                                        pred_fun = RF_DMC_pred_fun,
                                        smp_fun = partition_loo,
                                        smp_args = list(buffer=i),
                                        mode_rep = "loop", 
                                        mode_fold = "loop")
  test_RMSE = sp_cv_RF_DMC$error_rep$test_rmse
  }

plot(test2~test[ ,1])

# Stop cluster
stopCluster(cluster)

# End time measurement
end_time = Sys.time()
print("bygone time")
print(end_time - start_time)
##
## End (explore the relationship between buffer distance and RMSE)
####
################################################################################
## End (test area)
################################################################################
