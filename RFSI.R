################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sperrorest")
p_load("meteo")

# Additional functions that are not included in packages
source("auxiliary_functions.R", encoding = "UTF-8")

# Fewer decimal places, apply penalty on exponential notation 
options("scipen"= 999, "digits"=4)

# Load data and formula
data_set = "NuM_L"
load("Data/NuM_L.rda")
d = NuM_L
fo_RF = fo_RF_NuM_L_bc

# Set buffer 
buffer = 0

# Set tolerance (all = partition_loo without buffer)
tolerance = 50

# Set number of permutations 
n_perm = 0

# Use another error fun (should only be true for the calculation of the 
# retransformation RMSE)
re_bc = TRUE
if (re_bc){
  error_fun = err_re_bc
} else{
  error_fun = err_default
}

# Calculate importance for these variables
imp_vars_RF = all.vars(fo_RF)[-1]
imp_vars_RF = NULL

# Set partition function and sample arguments 
if (tolerance == "all"){
  partition_fun = partition_loo
  smp_args = list(buffer = buffer)
} else{
  partition_fun = partition_tt_dist
  smp_args = list(buffer = buffer, tolerance = tolerance)
}

####
## Data and model argument preparation
##
# Observation column
obs_col = "bcNitrate"

# CRS
c_r_s = "EPSG:25832"

# Names of the station ID (staid), longitude (X), latitude (Y) 
# and time columns in data
d.staid.x.y.z = c("ID_NuM_L","X","Y",NA)
##
## End (data and model argument preparation)
####
################################################################################
## End (preparation)
################################################################################


################################################################################
## RFSI spatial leave one out cross validation 
################################################################################
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
                      n.obs = 5, # number of nearest observations
                      progress = FALSE,
                      # ranger parameters
                      seed = 7,
                      oob.error = FALSE,
                      num.trees = 500,
                      sample.fraction = 1,
                      min.node.size = 5,
                      min.bucket = 1,
                      max.depth = 0,
                      splitrule = "variance")
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
sp_cv_RFSI = sperrorest::sperrorest(formula = fo_RF, data = d, 
                                    coords = c("X","Y"), 
                                    model_fun = RFSI_fun,
                                    model_args = list(c_r_s = c_r_s,
                                      data.staid.x.y.z=d.staid.x.y.z),
                                    pred_fun = RFSI_pred_fun,
                                    pred_args = list(obs_col=obs_col, 
                                                     c_r_s = c_r_s,
                                                     data.staid.x.y.z=
                                                       d.staid.x.y.z),
                                    smp_fun = partition_fun, 
                                    smp_args = smp_args,
                                    imp_permutations = n_perm,
                                    imp_variables = imp_vars_RF,
                                    imp_sample_from = "all",
                                    distance = TRUE,
                                    err_fun = error_fun)

# Get test RMSE
test_RMSE = sp_cv_RFSI$error_rep$test_rmse
test_RMSE

# End time measurement
end_time = Sys.time()
bygone_time = end_time - start_time
print(bygone_time)

# Set file name 
file_name = paste("Results/",data_set,"_sp_cv_RFSI_",as.character(round(buffer)),
                  "_+", as.character(tolerance), "_", as.character(n_perm),
                  ".rda", sep = "")
# Save result 
save(sp_cv_RFSI, bygone_time, file = file_name)
##
## End (cross validation)
#### 
################################################################################
## End (RFSI spatial leave one out cross validation)
################################################################################
