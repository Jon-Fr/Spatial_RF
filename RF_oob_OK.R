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
## Get information about the prediction distance 
## 
info_pd = info_predDist(path_predArea = "Data/NuM_L_sub_2_prediction_area.gpkg", 
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
##
## End (get 


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
  sp_df_train = sp::SpatialPointsDataFrame(train_data[,c("X","Y")], train_data)
  # Fit variogram model
  resid_vm = automap::autofitVariogram(formula = oob_resi ~ 1,
                                       input_data = sp_df_train,
                                       model = c("Sph"))
                                       #model = c("Mat", "Exp", "Sph"),
                                       #kappa = c(0.1,0.2,0.3),
                                       #fix.values = c(0, NA, NA))
  # Get the model
  resid_vmm = resid_vm["var_model"]$var_model
  # Create a spatial points df 
  newdata_sp_df = sp::SpatialPointsDataFrame(newdata[,c("X","Y")], newdata)
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

# Start time measurement
start_time = Sys.time()
print(start_time)

# Perform the spatial cross-validation
# Future for parallelization
future::plan(future.callr::callr, workers = 10)
sp_cv_RFOK = sperrorest::sperrorest(formula = fo, data = d, 
                                    coords = c("X","Y"), 
                                    model_fun = RF_fun,
                                    model_args = list(obs_col=obs_col),
                                    pred_fun = RF_oob_OK_pred_fun,
                                    smp_fun = partition_loo, 
                                    smp_args = list(buffer=med_pd))

# Get test RMSE
test_RMSE = sp_cv_RFOK$error_rep$test_rmse
test_RMSE

# End time measurement
end_time = Sys.time()
bygone_time = end_time - start_time
##
## End (cross validation)
#### 
################################################################################
## End (RF-oob-OK spatial leave one out cross validation) 
################################################################################