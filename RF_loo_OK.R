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
                  lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + 
                  ok_inter_pred + ok_inter_var + x + y)

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
## Random Forest with explanatory variables from leave one out Ordinary Kriging  
## (RF-llo-OK) prediction 
################################################################################
################################################################################
## End (RF-llo-OK prediction) 
################################################################################


################################################################################
## RF-llo-OK spatial leave one out cross validation 
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
## llo-OK multicore function
##
llo_OK_fun = function(data, buffer_dist, ok_fo){
  # Leave one out resampling
  resamp = sperrorest::partition_loo(data = data, ndisc = nrow(data), 
                                     replace = FALSE, coords = c("x","y"), 
                                     buffer = buffer_dist, repetition = 1)
  # Create a spatial points df 
  sp_df = sp::SpatialPointsDataFrame(coords = data[,c("x","y")], data = data)
  # Foreach for parallel processing
  ok_i_p = foreach::foreach(i = 1:nrow(sp_df),
                            .packages = c("gstat", "automap", "sp")) %dopar%{
                      # Get training and test ids
                      id_train = resamp[["1"]][[i]]$train
                      id_test = resamp[["1"]][[i]]$test
                      
                      # Get a training and test spatial points df 
                      train_sp_df = sp_df[id_train, ]
                      test_sp_df = sp_df[id_test, ]
                      
                      # Fit spherical variogram model
                      resid_vm = automap::autofitVariogram(formula = ok_fo, 
                                                  input_data = train_sp_df, 
                                                  model = c("Sph"),
                                                  cressie = TRUE)
                      # Get the model
                      resid_vmm = resid_vm["var_model"]$var_model
                      
                      # Interpolation
                      ok_pred = gstat::krige(ok_fo, train_sp_df, 
                                             model = resid_vmm, 
                                             newdata = test_sp_df,
                                             debug.level = 0)
                      pred = c(ok_pred$var1.pred, ok_pred$var1.var)
                    }
  
}

# Setup backend to use many processors
totalCores = parallel::detectCores()

# Leave two cores to reduce computer load
cluster = parallel::makeCluster(totalCores[1]-5) 
doParallel::registerDoParallel(cluster)

# Exectute llo-OK function
llo_ok_res = llo_OK_fun(data = d, 
                        buffer_dist = m_m_pd$med_predDist, 
                        ok_fo = ok_fo) 

# Stop cluster
stopCluster(cluster)

# Add the results to the df 
llo_ok_res_vec = unlist(llo_ok_res)
ok_inter_pred = llo_ok_res_vec[seq(1, length(llo_ok_res_vec), 2)]
ok_inter_var = llo_ok_res_vec[seq(2, length(llo_ok_res_vec), 2)]
d$ok_inter_pred = ok_inter_pred
d$ok_inter_var = ok_inter_var
##
## End (llo-OK multicore function)
####


####
## Cross validation
## 

# Create model function 
RF_fun = function(formula, data){
  RF_model = ranger::ranger(formula = formula, 
                            data = data)
  return(RF_model)
}

# Create prediction function
RF_pred_fun = function(object, newdata){
  RF_prediction = predict(object = object,
                          data = newdata)
  return(RF_prediction$predictions)
}

# Perform the spatial cross-validation
# Future for parallelization
future::plan(future.callr::callr, workers = 10)
sp_cv_RF = sperrorest::sperrorest(formula = fo, data = d, coords = c("x","y"), 
                                  model_fun = RF_fun, 
                                  pred_fun = RF_pred_fun,
                                  smp_fun = partition_loo, 
                                  smp_args = list(buffer = m_m_pd$med_predDist))

# Get test RMSE
test_RMSE = sp_cv_RF$error_rep$test_rmse
test_RMSE
##
## End (cross validation)
#### 
################################################################################
## End (RF-llo-OK spatial leave one out cross validation) 
################################################################################


################################################################################
## Spatial prediction error profile (SPEP) and 
# spatial variable importance profile (SVIP) (Brenning 2022) 
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
## llo-OK function
##
llo_OK_fun = function(data, buffer_dist, ok_fo){
  # Leave one out resampling
  resamp = sperrorest::partition_loo(data = data, ndisc = nrow(data), 
                                     replace = FALSE, coords = c("x","y"), 
                                     buffer = buffer_dist, repetition = 1)
  # Create a spatial points df 
  sp_df = sp::SpatialPointsDataFrame(coords = data[,c("x","y")], data = data)
  # Create vectors for storing the results
  res_pred = c()
  res_var = c()
  # For loop
  for (i in 1:nrow(sp_df)){
    # Get training and test ids
    id_train = resamp[["1"]][[i]]$train
    id_test = resamp[["1"]][[i]]$test
    
    # Get a training and test spatial points df 
    train_sp_df = sp_df[id_train, ]
    test_sp_df = sp_df[id_test, ]
    
    # Fit spherical variogram model
    resid_vm = automap::autofitVariogram(formula = ok_fo, 
                                         input_data = train_sp_df, 
                                         model = c("Sph"),
                                         cressie = TRUE)
    # Get the model
    resid_vmm = resid_vm["var_model"]$var_model
    
    # Interpolation
    ok_pred = gstat::krige(ok_fo, train_sp_df, model = resid_vmm, 
                           newdata = test_sp_df, debug.level = 0)
    pred = c(ok_pred$var1.pred, ok_pred$var1.var)
    # Add results to result vectors
    res_pred = append(res_pred, ok_pred$var1.pred)
    res_var = append(res_var, ok_pred$var1.var)
  }
  # Add results to df 
  data$ok_inter_pred = res_pred
  data$ok_inter_var = res_var
  return(data)
}

# Test llo-OK function
#llo_ok_res = llo_OK_fun(data = d, 
#                        buffer_dist = 1000, 
#                        ok_fo = ok_fo) 
##
## End (llo-OK function)
####


####
## Cross validation
## 

# Create model function 
RF_fun = function(formula, data, buffer_dist, ok_fo){
  # loo OK
  data = llo_OK_fun(data = data, buffer_dist = buffer_dist, ok_fo = ok_fo)
  # Create RF model
  RF_model = ranger::ranger(formula = formula, 
                            data = data)
  # Return training data and RF_model
  r_l = list("model" = RF_model, "train_data" = data)
  return(r_l)
}

# Create prediction function
RF_pred_fun = function(object, newdata, ok_fo){
  # Perform Ordinary kriging interpolation for the newdata point
  # Create spdfs
  train_data = object$train_data
  train_sp_df = sp::SpatialPointsDataFrame(coords = train_data[,c("x","y")], 
                                     data = train_data)
  sp_df = sp::SpatialPointsDataFrame(coords = newdata[,c("x","y")], 
                                     data = newdata)
  # Fit spherical variogram model
  resid_vm = automap::autofitVariogram(formula = ok_fo, 
                                       input_data = train_sp_df, 
                                       model = c("Sph"),
                                       cressie = TRUE)
  # Get the model
  resid_vmm = resid_vm["var_model"]$var_model
  # Interpolation
  ok_pred = gstat::krige(ok_fo, train_sp_df, 
                         model = resid_vmm, newdata = sp_df, debug.level = 0)
  # Add the prediction and the variance to newdata
  newdata$ok_inter_pred = ok_pred$var1.pred
  newdata$ok_inter_var = ok_pred$var1.var  
  #  Perform RF prediction
  RF_prediction = predict(object = object$model,
                          data = newdata)
  return(RF_prediction$predictions)
}
################################################################################
## End (SPEP and SVIP)
################################################################################


################################################################################
## Test area
################################################################################
################################################################################
## End (test area)
################################################################################



