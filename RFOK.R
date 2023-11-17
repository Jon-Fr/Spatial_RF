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

p_load("ranger")
p_load("gstat")       
p_load("automap")

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
## Model argument preparation
##

# Observation column
obs_col = "bcNitrate"
##
## End Model argument preparation)
####
################################################################################
## End (preparation)
################################################################################


################################################################################
## Random Forest with Ordinary Kriging of the residuals (RFOK) prediction 
################################################################################
################################################################################
## End (RFOK prediction) 
################################################################################


################################################################################
## RFOK spatial leave one out cross validation 
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
RF_fun = function(formula, data){
  RF_model = ranger::ranger(formula = formula, 
                            data = data)
  # Return RF model and training data
  return_list = list(model = RF_model, train_data = data)
  return(return_list)
}

# Create prediction function
RFOK_pred_fun = function(object, newdata, obs_col){
  # Make the RF prediction
  RF_prediction = predict(object = object$model,
                          data = newdata)
  # Add the RF prediction to the df
  newdata$RF_predictions = RF_prediction$predictions
  # Get training data
  train_data = object$train_data
  # Get the RF prediction and residuals
  RF_prediction = predict(object = object$model,
                          data = train_data)
  # Add the RF prediction and residuals to the df
  train_data$RF_predictions = RF_prediction$predictions
  train_data$RF_residuals = train_data[, obs_col] - RF_prediction$predictions
  # Create spatial points dfs
  sp_df_train = sp::SpatialPointsDataFrame(train_data[,c("x","y")], train_data)
  # Fit spherical variogram model
  resid_vm = automap::autofitVariogram(formula = RF_residuals ~ 1, 
                                       input_data = sp_df_train, 
                                       model = c("Sph"))
  # Get the model
  resid_vmm = resid_vm["var_model"]$var_model
  # Create a spatial points df 
  newdata_sp_df = sp::SpatialPointsDataFrame(newdata[,c("x","y")], newdata)
  # Ordinary Kriging residual interpolation
  ok_pred = gstat::krige(formula = RF_residuals ~ 1, sp_df_train, 
                         model = resid_vmm, 
                         newdata = newdata_sp_df, 
                         debug.level = 0)
  # Sum up the residual interpolation and the RF prediction
  newdata_sp_df$final_predcition = newdata_sp_df$RF_predictions + 
    ok_pred$var1.pred  
  return(newdata_sp_df$final_predcition)
}

# Perform the spatial cross-validation
# Future for parallelization
future::plan(future.callr::callr, workers = 10)
sp_cv_RFOK = sperrorest::sperrorest(formula = fo, data = d, coords = c("x","y"), 
                                    model_fun = RF_fun, 
                                    pred_fun = RFOK_pred_fun,
                                    pred_args = list(obs_col=obs_col),
                                    smp_fun = partition_loo, 
                                    smp_args = list(buffer=m_m_pd$med_predDist))

# Get test RMSE
test_RMSE = sp_cv_RFOK$error_rep$test_rmse
test_RMSE
##
## End (cross validation)
#### 
################################################################################
## End (RFOK spatial leave one out cross validation) 
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

test2 = foreach::foreach(i = iter(test, by="row"), .combine=c, 
                 .packages = c("sperrorest", "ranger", "sp", "automap", 
                               "gstat")) %dopar%{
  sp_cv_RFRK = sperrorest::sperrorest(formula = fo, data = d, 
                                      coords = c("x","y"), 
                                      model_fun = RF_fun, 
                                      pred_fun = RFRK_pred_fun,
                                      pred_args = list(obs_col=obs_col),
                                      smp_fun = partition_loo,
                                      smp_args = list(buffer = i),
                                      mode_rep = "loop", 
                                      mode_fold = "loop")
  
  test_RMSE = sp_cv_RFRK$error_rep$test_rmse
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

