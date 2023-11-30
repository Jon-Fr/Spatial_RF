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

p_load("RandomForestsGLS")

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
# Coordinate columns
coord_columns = c("X", "Y")

# Observation column
obs_col = "bcNitrate"

# Covariate columns
covari_columns = c("crestime", "cgwn", "cgeschw", "log10carea", "elevation",
                   "cAckerland", "log10_gwn", "agrum_log10_restime", 
                   "Ackerland", "lbm_class_Gruenland", "lbm_class_Unbewachsen", 
                   "lbm_class_FeuchtgebieteWasser", "lbm_class_Siedlung",
                   "aea20_13", "aea20_6", "X", "Y")

# Set random seed
set.seed(1)
##
## End (data and model argument preparation)
####
################################################################################
## End (preparation)
################################################################################


################################################################################
## Generalized Least Square based Random Forest  (RF-GLS) prediction 
################################################################################
################################################################################
## End (RF-GLS prediction) 
################################################################################


################################################################################
## RF-GLS spatial leave one out cross validation 
################################################################################

####
## Cross validation
## 

# Create model function 
RF_GLS_fun = function(formula, data, coord_columns, obs_col, covari_columns){
  # Prepare the model arguments
  num_of_rows = nrow(data)
  coordinates_matrix = as.matrix(data[, coord_columns])
  observations = data[, obs_col]
  covariates_matrix = data.matrix(data[, covari_columns])
  num_of_cols = ncol(covariates_matrix)
  # Create model
  RF_GLS_M = RandomForestsGLS::RFGLS_estimate_spatial(
    coords = coordinates_matrix,
    X = covariates_matrix,
    y = observations,
    param_estimate = TRUE,
    cov.model = "matern",
    ntree = 50,
    mtry = round(num_of_cols/2),
    h = 10)
  return(RF_GLS_M)
}

# Create prediction function
RF_GSL_pred_fun = function(object, newdata, covari_columns, coord_columns){
  # Prepare the data
  num_of_rows = nrow(newdata)
  coordinates_matrix = as.matrix(newdata[, coord_columns])
  covariates_matrix = data.matrix(newdata[, covari_columns])
  # Prediction
  RF_GLS_prediction = RandomForestsGLS::RFGLS_predict_spatial(
                                                  RFGLS_out = object,
                                                  Xtest = covariates_matrix,
                                                  coords.0 = coordinates_matrix)
  # Calculate the mean of all tree predictions
  prediction = RF_GLS_prediction$prediction
  return(prediction)
}

# Start time measurement
start_time = Sys.time()
print(start_time)

# Perform the spatial cross-validation
# Future for parallelization
#future::plan(future.callr::callr, workers = 10)
sp_cv_RF_GLS = sperrorest::sperrorest(formula = fo, data = d, 
                            coords = c("X","Y"), 
                            model_fun = RF_GLS_fun,
                            model_args = list(coord_columns = coord_columns,
                                              obs_col = obs_col, 
                                              covari_columns = covari_columns),
                            pred_fun = RF_GSL_pred_fun,
                            pred_args = list(covari_columns = covari_columns, 
                                             coord_columns = coord_columns),
                            smp_fun = partition_loo, 
                            smp_args = list(buffer = 0))

# Get test RMSE
test_RMSE = sp_cv_RF_GLS$error_rep$test_rmse
test_RMSE

# End time measurement
end_time = Sys.time()
bygone_time = end_time - start_time
##
## End (cross validation)
#### 
################################################################################
## End (RF-GLS spatial leave one out cross validation) 
################################################################################


################################################################################
## Test area
################################################################################
resamp = partition_loo(data = d, ndisc = nrow(d), replace = FALSE, 
                       coords = c("X","Y"), buffer = med_pd, 
                       repetition = 1)

id_train = resamp[["1"]][[150]]$train
id_test = resamp[["1"]][[150]]$test

# Start time measurement
start_time = Sys.time()

train_df = d[id_train, ]
test_df = d[id_test, ]

a = RF_GLS_fun(formula = fo, data = train_df, coord_columns = coord_columns, obs_col = obs_col, 
               covari_columns = covari_columns)

b = RF_GSL_pred_fun(object = a, newdata = test_df, covari_columns = covari_columns, coord_columns = coord_columns)

# End time measurement
end_time = Sys.time()
print("bygone time")
print(end_time - start_time)
################################################################################
## End (test area)
################################################################################
