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
p_load("future")

p_load("meteo")

# Additional functions that are not included in packages
source("auxiliary_functions.R", encoding = "UTF-8")
source("spdiagnostics-functions.R", encoding = "UTF-8") # Brenning 2022

# Fewer decimal places
options(digits=4)

# Load data and formula (for now use a subset)
load("data_points_subset.rda")
d = subset_dp

# Simplified formula for testing
fo = as.formula(bcNitrate ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                  cAckerland + log10_gwn + agrum_log10_restime + Ackerland + 
                  lbm_class_Gruenland + lbm_class_Unbewachsen + 
                  lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + x + y)

####
## Data and model argument preparation
##

# Observation column
obs_col = "bcNitrate"

# CRS
c_r_s = "EPSG:25832"

# Add id column
d$id = 1:nrow(d)

# Names of the station ID (staid), longitude (x), latitude (y) 
# and time columns in data
d.staid.x.y.z = c("id","x","y",NA)
##
## End (data and model argument preparation)
####
################################################################################
## End (preparation)
################################################################################


################################################################################
## Random Forest spatial interpolation (RFSI) prediction 
################################################################################
################################################################################
## End (RFSI prediction) 
################################################################################


################################################################################
## RFSI spatial leave one out cross validation 
################################################################################
#####
## Get the mean and median prediction distance 
## (for now use the test area as prediction area)
##

m_m_pd = mean_med_predDist(path_predArea = "test_area.gpkg", dataPoints_df = d,
                           c_r_s = "EPSG:25832")
##
## End (get the mean and median prediction distance)
####


####
## Cross validation
## 

# Create model function 
RFSI_fun = function(formula, data, data.staid.x.y.z, c_r_s){
  # Create model
  RFSI_model = meteo::rfsi(formula = formula,
                      data = data,
                      s.crs = c_r_s,
                      data.staid.x.y.z = data.staid.x.y.z,
                      n.obs = 10, # number of nearest observations
                      progress = FALSE,
                      # ranger parameters
                      num.trees = 500)
  # Return model and training data
  return_list = list(model = RFSI_model, train_data = data)
  return(return_list)
}


# Create prediction function
RFSI_pred_fun = function(object, newdata, data.staid.x.y.z, obs_col, c_r_s){
  # Get the training data
  train_data = object$train_data
  # Prediction
  RFSI_prediction = meteo::pred.rfsi(model = object$model,
                                     data = train_data,
                                     s.crs = c_r_s,
                                     data.staid.x.y.z = data.staid.x.y.z,
                                     obs.col = obs_col,
                                     newdata = newdata,
                                     newdata.staid.x.y.z = data.staid.x.y.z,
                                     newdata.s.crs = c_r_s,
                                     progress = FALSE,
                                     soil3d=FALSE)
  # Necessary step to return numeric prediction values instead of a df
  RFSI_prediction = data.matrix(RFSI_prediction, rownames.force = NA)
  return(RFSI_prediction[,4])
}

# Perform the spatial cross-validation
# Future for parallelization
future::plan(future.callr::callr, workers = 10)
sp_cv_RFSI = sperrorest::sperrorest(formula = fo, data = d, coords = c("x","y"), 
                                    model_fun = RFSI_fun,
                                    model_args = list(c_r_s = c_r_s,
                                      data.staid.x.y.z=d.staid.x.y.z),
                                    pred_fun = RFSI_pred_fun,
                                    pred_args = list(obs_col=obs_col, 
                                                     c_r_s = c_r_s,
                                                     data.staid.x.y.z=
                                                       d.staid.x.y.z),
                                    smp_fun = partition_loo, 
                                    smp_args = list(buffer=m_m_pd$med_predDist))

# Get test RMSE
test_RMSE = sp_cv_RFSI$error_rep$test_rmse
test_RMSE
##
## End (cross validation)
#### 
################################################################################
## End (RFSI spatial leave one out cross validation)
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

test2 = foreach::foreach (i = iter(test, by="row"), .combine=c, 
                 .packages = c("sperrorest", "meteo")) %dopar%{

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