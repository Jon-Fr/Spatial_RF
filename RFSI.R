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

# Fewer decimal places, apply penalty on exponential notation 
options("scipen"= 999, "digits"=4)

# Load data (for now use a subset)
load("Data/NuM_L_sub_4.rda")
d = sub_subset

# Get information about the prediction distance 
info_pd = info_predDist(path_predArea = "Data/NuM_L_sub_4_prediction_area.gpkg", 
                        dataPoints_df = d,
                        c_r_s = "EPSG:25832",
                        resolution = 100,
                        xy = c("X", "Y"))

pd_df = info_pd$predDist_df
hist(pd_df$lyr.1)
third_quartile = quantile(x = pd_df$lyr.1, probs = c(0.75))
tq_pd = third_quartile
max_pd = info_pd$max_predDist
mean_pd = info_pd$mean_predDist
sd_pd = info_pd$sd_predDist
med_pd = info_pd$med_predDist
mad_pd = info_pd$mad_predDist

# Adjusted formula
fo = as.formula(bcNitrate ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                  cAckerland + log10_gwn + agrum_log10_restime + Ackerland + 
                  lbm_class_Gruenland + lbm_class_Unbewachsen + 
                  lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + X + Y)

####
## Data and model argument preparation
##

# Observation column
obs_col = "bcNitrate"

# CRS
c_r_s = "EPSG:25832"

# Add id column
d$id = 1:nrow(d)

# Names of the station ID (staid), longitude (X), latitude (Y) 
# and time columns in data
d.staid.x.y.z = c("id","X","Y",NA)
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

# Start time measurement
start_time = Sys.time()
print(start_time)

# Perform the spatial cross-validation
# Future for parallelization
future::plan(future.callr::callr, workers = 10)
sp_cv_RFSI = sperrorest::sperrorest(formula = fo, data = d, coords = c("X","Y"), 
                                    model_fun = RFSI_fun,
                                    model_args = list(c_r_s = c_r_s,
                                      data.staid.x.y.z=d.staid.x.y.z),
                                    pred_fun = RFSI_pred_fun,
                                    pred_args = list(obs_col=obs_col, 
                                                     c_r_s = c_r_s,
                                                     data.staid.x.y.z=
                                                       d.staid.x.y.z),
                                    smp_fun = partition_loo, 
                                    smp_args = list(buffer=med_pd))

# Get test RMSE
test_RMSE = sp_cv_RFSI$error_rep$test_rmse
test_RMSE

# End time measurement
end_time = Sys.time()
bygone_time = end_time - start_time
##
## End (cross validation)
#### 
################################################################################
## End (RFSI spatial leave one out cross validation)
################################################################################


################################################################################
## Test area
################################################################################
################################################################################
## End (test area)
################################################################################