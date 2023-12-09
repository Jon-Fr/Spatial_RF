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
source("spdiagnostics-functions.R", encoding = "UTF-8") # Brenning 2022

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

####
## Data and model argument preparation
##
## Get mtry value
# Get number of explanatory variables
# formula to character
fo_chr = as.character(fo_RF)
# count number of +, add 1
count = 1
for (i in 1:nchar(fo_chr[[3]])){
  chr = substring(fo_chr[[3]],i,i)
  if (chr == "+"){
   count = count + 1 
  }
}
mtry_n = round(sqrt(count))

# Observation column
obs_col = "bcNitrate"

# CRS
c_r_s = "EPSG:25832"

# Names of the station ID (staid), longitude (X), latitude (Y) 
# and time columns in data
d.staid.x.y.z = c("ID_WuS_SuB","X","Y",NA)
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
####
## Cross validation
## 

# Create model function 
RFSI_fun = function(formula, data, data.staid.x.y.z, c_r_s, mtry_n){
  # Create model
  RFSI_model = meteo::rfsi(formula = formula,
                      data = data,
                      s.crs = c_r_s,
                      data.staid.x.y.z = data.staid.x.y.z,
                      n.obs = 50, # number of nearest observations
                      progress = FALSE,
                      # ranger parameters
                      seed = 7,
                      oob.error = FALSE,
                      num.trees = 500,
                      sample.fraction = 1,
                      mtry = mtry_n,
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

start_time = Sys.time()
test = RFSI_fun(formula = fo_RF, data = d[2:2360,], data.staid.x.y.z = d.staid.x.y.z, c_r_s = c_r_s, mtry_n = mtry_n)
test1 = RFSI_pred_fun(object = test, newdata = d[1,], data.staid.x.y.z = d.staid.x.y.z, obs_col = obs_col, c_r_s = c_r_s)
print(Sys.time() - start_time)

# Start time measurement
start_time = Sys.time()
print(start_time)

# Perform the spatial cross-validation
# Future for parallelization
future::plan(future.callr::callr, workers = 10)
sp_cv_RFSI = sperrorest::sperrorest(formula = fo_RF, data = d, 
                                    coords = c("X","Y"), 
                                    model_fun = RFSI_fun,
                                    model_args = list(c_r_s = c_r_s,
                                      data.staid.x.y.z=d.staid.x.y.z),
                                    pred_fun = RFSI_pred_fun,
                                    pred_args = list(obs_col=obs_col, 
                                                     c_r_s = c_r_s,
                                                     data.staid.x.y.z=
                                                       d.staid.x.y.z,
                                                     mtry_n = mtry_n),
                                    smp_fun = partition_loo, 
                                    smp_args = list(buffer=med_pd))

# Get test RMSE
test_RMSE = sp_cv_RFSI$error_rep$test_rmse
test_RMSE

# End time measurement
end_time = Sys.time()
bygone_time = end_time - start_time
print(bygone_time)

# Set file name 
file_name = paste("Results/",data_set,"_sp_cv_RFSI_",as.character(round(buffer)),
                  ".rda", sep = "")
# Save result 
save(sp_cv_RFSI, bygone_time, file = file_name)
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