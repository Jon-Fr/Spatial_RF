################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sperrorest")
p_load("RandomForestsGLS")

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

# Set tolerance (all = partition_loo without buffer)
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
## Data and model argument preparation
##
# Coordinate columns
coord_columns = c("X", "Y")

# Observation column
obs_col = "bcNitrate"

# Covariate columns
covari_columns = c("crestime", "cgwn", "cgeschw", "log10carea", "elevation",
                   "nfk", "humus", "cAckerland", "log10_gwn", 
                   "agrum_log10_restime", "agrum_log10_gwn", "agrum_log10_geschw",
                   "Ackerland", "lbm_class_Gruenland", "lbm_class_Unbewachsen", 
                   "lbm_class_FeuchtgebieteWasser", "lbm_class_Siedlung", "X", 
                   "Y", "tc45", "tc315", "aea20_1", "aea20_2", "aea20_12", 
                   "aea20_13")

# Set random seed
set.seed(7)
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
    cov.model = "exponential",
    ntree = 50,
    mtry = round(num_of_cols/3),
    h = 1)
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

# Test
start_time = Sys.time()
print(start_time)

i = 1230

test = RF_GLS_fun(formula = fo_RF, data = d[-c(i),], 
                  coord_columns = coord_columns, covari_columns = covari_columns,
                  obs_col = obs_col)

end_time = Sys.time()
bygone_time = end_time - start_time
print(bygone_time)

test1 = RF_GSL_pred_fun(object = test, newdata = d[i,], 
                        coord_columns = coord_columns, 
                        covari_columns = covari_columns)

end_time = Sys.time()
bygone_time = end_time - start_time
print(bygone_time)

# Start time measurement
start_time = Sys.time()
print(start_time)

# Perform the spatial cross-validation
sp_cv_RF_GLS = sperrorest::sperrorest(formula = fo_RF, data = d, 
                            coords = c("X","Y"), 
                            model_fun = RF_GLS_fun,
                            model_args = list(coord_columns = coord_columns,
                                              obs_col = obs_col, 
                                              covari_columns = covari_columns),
                            pred_fun = RF_GSL_pred_fun,
                            pred_args = list(covari_columns = covari_columns, 
                                             coord_columns = coord_columns),
                            smp_fun = partition_fun, 
                            smp_args = smp_args,
                            importance = TRUE, 
                            imp_permutations = n_perm,
                            imp_variables = imp_vars_RF,
                            imp_sample_from = "all",
                            distance = TRUE)

# Get test RMSE
test_RMSE = sp_cv_RF_GLS$error_rep$test_rmse
test_RMSE

# End time measurement
end_time = Sys.time()
bygone_time = end_time - start_time
print(bygone_time)

# Set file name 
file_name = paste("Results/",data_set,"_sp_cv_RF_GLS_",as.character(round(buffer)),
                  "_+", as.character(tolerance), "_", as.character(n_perm),
                  ".rda", sep = "")
# Save result 
save(sp_cv_RF_GLS, bygone_time, file = file_name)
##
## End (cross validation)
#### 
################################################################################
## End (RF-GLS spatial leave one out cross validation) 
################################################################################
