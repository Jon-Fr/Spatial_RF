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
## Random Forest with Ordinary Kriging of the out of bag residuals (RF-oob-OK) 
## prediction 
################################################################################
################################################################################
## End (RF-oob-OK prediction) 
################################################################################


################################################################################
## RF-oob-OK spatial leave one out cross validation 
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
RF_fun = function(formula, data, obs_col){
  # Create RF model
  RF_model = ranger::ranger(formula = formula, 
                            data = data)
  # Calculate the oob residuals and add them to the df
  data$oob_resi = data[, obs_col] - RF_model$predictions
  # Return RF model and training data
  return_list = list(model = RF_model, train_data = data)
  return(return_list)
}

# Create prediction function
RF_oob_OK_pred_fun = function(object, newdata){
  # Make the RF prediction
  RF_prediction = predict(object = object$model,
                          data = newdata)
  # Add the RF prediction to the df
  newdata$RF_predictions = RF_prediction$predictions
  # Get training data
  train_data = object$train_data
  # Create spatial points dfs
  sp_df_train = sp::SpatialPointsDataFrame(train_data[,c("x","y")], train_data)
  # Fit spherical variogram model
  resid_vm = automap::autofitVariogram(formula = oob_resi ~ 1, 
                                       input_data = sp_df_train, 
                                       model = c("Sph"))
  # Get the model
  resid_vmm = resid_vm["var_model"]$var_model
  # Create a spatial points df 
  newdata_sp_df = sp::SpatialPointsDataFrame(newdata[,c("x","y")], newdata)
  # Ordinary Kriging oob residual interpolation
  ok_pred = gstat::krige(formula = oob_resi ~ 1, sp_df_train, 
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
sp_cv_RFOK = sperrorest::sperrorest(formula = fo, data = d[1:100, ], 
                                    coords = c("x","y"), 
                                    model_fun = RF_fun,
                                    model_args = list(obs_col=obs_col),
                                    pred_fun = RF_oob_OK_pred_fun,
                                    smp_fun = partition_loo, 
                                    smp_args = list(buffer=m_m_pd$med_predDist))

# Get test RMSE
test_RMSE = sp_cv_RFOK$error_rep$test_rmse
test_RMSE
##
## End (cross validation)
#### 
################################################################################
## End (RF-oob-OK spatial leave one out cross validation) 
################################################################################