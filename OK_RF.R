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
p_load("nlme")
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

ok_fo = as.formula(bcNitrate ~ 1)
##
## End Model argument preparation)
####
################################################################################
## End (preparation)
################################################################################


################################################################################
## Combined weighted Ordinary Kriging and Random Forest prediction (OK-RF) 
## prediction 
################################################################################
################################################################################
## End (OK-RF prediction) 
################################################################################


################################################################################
## OK-RF spatial leave one out cross validation 
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
RF_vm_fun = function(formula, data, ok_fo){
  # Create RF model
  RF_model = ranger::ranger(formula = formula, 
                            data = data)
  # Create a spatial points df 
  sp_df = sp::SpatialPointsDataFrame(coords = data[,c("x","y")], data = data)
  # variogram model fitting
  vmf = automap::autofitVariogram(formula = ok_fo, input_data = sp_df,
                                        model = c("Sph"), cressie = TRUE)
  var_model = vmf["var_model"]$var_model
  # Return RF model, variogram model and training data
  return_list = list(RF_model = RF_model, 
                     v_model = var_model, 
                     train_data = data)
  return(return_list)
}


# Create prediction function
OK_RF_pred_fun = function(object, newdata, ok_fo){
  # Get training data
  train_data = object$train_data
  # Create spatial points dfs
  train_sp_df = sp::SpatialPointsDataFrame(train_data[,c("x","y")], train_data)
  newdata_sp_df = sp::SpatialPointsDataFrame(newdata[,c("x","y")], newdata)
  # Get the range of the variogram model
  range = object$v_model[2,"range"]
  # Kriging interpolation/prediction
  ok_pred = krige(formula = ok_fo, train_sp_df, 
                  model = object$v_model, newdata = newdata_sp_df,
                  debug.level = 0)
  ok_inter = ok_pred$var1.pred
  # RF prediction
  RF_prediction = predict(object = object$RF_model,
                          data = newdata)
  RF_prediction_values = RF_prediction$predictions
  # Compute distance between newdata and the nearest training data point 
  dist = rep(NA, nrow(newdata))
  for (i in 1:nrow(newdata)){
    dist[i] = min(sqrt((train_data[, "x"] - newdata[i, "x"])^2 
                       + (train_data[, "y"] - newdata[i, "y"])^2))
  }
  # Compute weights
  weights = dist/range
  # Weight predictions
  final_prediction = (1-weights) * ok_inter + weights * 
    RF_prediction_values
  return(final_prediction)
}

# Perform the spatial cross-validation
# Future for parallelization
future::plan(future.callr::callr, workers = 10)
sp_cv_OK_RF = sperrorest::sperrorest(formula = fo, data = d, 
                                     coords = c("x","y"), 
                                     model_fun = RF_vm_fun,
                                     model_args = list(ok_fo = ok_fo),
                                     pred_args = list(ok_fo = ok_fo),
                                     pred_fun = OK_RF_pred_fun,
                                     smp_fun = partition_loo, 
                                     smp_args = list(buffer=
                                                       m_m_pd$med_predDist))

# Get test RMSE
test_RMSE = sp_cv_OK_RF$error_rep$test_rmse
test_RMSE
##
## End (cross validation)
#### 
################################################################################
## End (OK-RF spatial leave one out cross validation) 
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
                 .packages = c("sperrorest", "ranger", "sp", "automap", 
                               "gstat")) %dopar%{
  sp_cv_OK_RF = sperrorest::sperrorest(formula = fo, data = d, 
                                       coords = c("x","y"), 
                                       model_fun = RF_vm_fun,
                                       model_args = list(ok_fo = ok_fo),
                                       pred_args = list(ok_fo = ok_fo),
                                       pred_fun = OK_RF_pred_fun,
                                       smp_fun = partition_loo, 
                                       smp_args = list(buffer=i),
                                       mode_rep = "loop", 
                                       mode_fold = "loop")
  
  test_RMSE = sp_cv_OK_RF$error_rep$test_rmse
  }

plot(test2~test[ ,1])

# Stop cluster
parallel::stopCluster(cluster)

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
