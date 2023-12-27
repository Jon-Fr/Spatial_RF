################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sp")
p_load("sf")
p_load("sperrorest")
p_load("ranger")
p_load("gstat")       
p_load("nlme")
p_load("automap")

# Additional functions that are not included in packages
source("auxiliary_functions.R", encoding = "UTF-8")

# Fewer decimal places, apply penalty on exponential notation 
options("scipen"= 999, "digits"=4)

# Load data and formula
data_set = "WuS_SuB"
load("Data/WuS_SuB.rda")
d = WuS_SuB
fo_RF = fo_RF_WuS_SuB

# Get information about the prediction distance 
pd_df = info_d_WuS_SuB$predDist_df
mean_pd = mean(pd_df$lyr.1)
med_pd = median(pd_df$lyr.1)

# Set buffer 
buffer = 0

# Set tolerance (all = partition_loo with buffer)
tolerance = "all"

# Set number of permutations 
n_perm = 10

# Calculate importance for these variables
imp_vars_RF = all.vars(fo_RF)[-1]

# Set partition function and sample arguments 
if (tolerance == "all"){
  partition_fun = partition_loo
  smp_args = list(buffer = buffer)
} else{
  partition_fun = partition_tt_dist
  smp_args = list(buffer = buffer, tolerance = tolerance)
}

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

####
## Cross validation
## 

# Create model function 
RF_vm_fun = function(formula, data, ok_fo){
  # Create RF model
  RF_model = ranger::ranger(formula = formula, 
                            data = data,
                            oob.error = FALSE,
                            seed = 7)
  # Create a spatial points df 
  sp_df = sp::SpatialPointsDataFrame(coords = data[,c("X","Y")], data = data)
  # variogram model fitting
  vmf = automap::autofitVariogram(formula = ok_fo, input_data = sp_df,
                                  model = c("Mat", "Exp"),
                                  kappa = c(0.1,0.2),
                                  fix.values = c(0, NA, NA))
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
  train_sp_df = sp::SpatialPointsDataFrame(train_data[,c("X","Y")], train_data)
  newdata_sp_df = sp::SpatialPointsDataFrame(newdata[,c("X","Y")], newdata)
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
    dist[i] = min(sqrt((train_data[, "X"] - newdata[i, "X"])^2 
                       + (train_data[, "Y"] - newdata[i, "Y"])^2))
  }
  # Compute weights
  weights = dist/range
  # Weight predictions
  final_prediction = (1-weights) * ok_inter + weights * 
    RF_prediction_values
  return(final_prediction)
}

# Start time measurement
start_time = Sys.time()
print(start_time)

# Perform the spatial cross-validation
sp_cv_OK_RF = sperrorest::sperrorest(formula = fo_RF, data = d, 
                                     coords = c("X","Y"), 
                                     model_fun = RF_vm_fun,
                                     model_args = list(ok_fo = ok_fo),
                                     pred_args = list(ok_fo = ok_fo),
                                     pred_fun = OK_RF_pred_fun,
                                     smp_fun = partition_fun, 
                                     smp_args = smp_args,
                                     importance = TRUE, 
                                     imp_permutations = n_perm,
                                     imp_variables = imp_vars_RF,
                                     imp_sample_from = "all",
                                     distance = TRUE)

# Get test RMSE
test_RMSE = sp_cv_OK_RF$error_rep$test_rmse
test_RMSE

# End time measurement
end_time = Sys.time()
bygone_time = end_time - start_time
print(bygone_time)

# Set file name 
file_name = paste("Results/", data_set,"_sp_cv_OK_RF_", as.character(round(buffer)),
                  "_+", as.character(tolerance), "_", as.character(n_perm),
                  ".rda", sep = "")
# Save result 
save(sp_cv_OK_RF, bygone_time, file = file_name)
##
## End (cross validation)
#### 
################################################################################
## End (OK-RF spatial leave one out cross validation) 
################################################################################


################################################################################
## Test area
################################################################################
################################################################################
## End (test area)
################################################################################
