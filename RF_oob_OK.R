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
data_set = "NuM_L"
load("Data/NuM_L.rda")
d = NuM_L
fo_RF = fo_RF_NuM_L

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
## Model argument preparation
##

# Observation column
obs_col = "subMittelwert"
##
## End Model argument preparation)
####
################################################################################
## End (preparation)
################################################################################


################################################################################
## RF-oob-OK spatial leave one out cross validation 
################################################################################

####
## Cross validation
## 

# Create model function 
RF_fun = function(formula, data, obs_col){
  # Create RF model
  RF_model = ranger::ranger(formula = formula, 
                            data = data,
                            seed = 7)
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
                                       model = c("Mat", "Exp"),
                                       kappa = c(seq(0.1, 0.4, 0.1)),
                                       fix.values = c(0, NA, NA))
  # Get the model
  resid_vmm = resid_vm["var_model"]$var_model
  # Create a spatial points df 
  newdata_sp_df = sp::SpatialPointsDataFrame(newdata[,c("X","Y")], newdata)
  # Ordinary Kriging oob residual interpolation
  ok_pred = gstat::krige(formula = oob_resi ~ 1, sp_df_train, 
                         model = resid_vmm, 
                         newdata = newdata_sp_df, 
                         debug.level = 0,
                         nmax = 200)
  # Sum up the residual interpolation and the RF prediction
  newdata_sp_df$final_prediction = newdata_sp_df$RF_predictions + 
    ok_pred$var1.pred  
  return(newdata_sp_df$final_prediction)
}

# Start time measurement
start_time = Sys.time()
print(start_time)

# Perform the spatial cross-validation
sp_cv_RF_oob_OK = sperrorest::sperrorest(formula = fo_RF, data = d, 
                                    coords = c("X","Y"), 
                                    model_fun = RF_fun,
                                    model_args = list(obs_col=obs_col),
                                    pred_fun = RF_oob_OK_pred_fun,
                                    smp_fun = partition_fun, 
                                    smp_args = smp_args,
                                    imp_permutations = n_perm,
                                    imp_variables = imp_vars_RF,
                                    imp_sample_from = "all",
                                    distance = TRUE)

# Get test RMSE
test_RMSE = sp_cv_RF_oob_OK$error_rep$test_rmse
test_RMSE

# End time measurement
end_time = Sys.time()
bygone_time = end_time - start_time
print(bygone_time)

# Set file name 
file_name = paste("Results/",data_set,"_sp_cv_RF_oob_OK_",as.character(round(buffer)),
                  "_+", as.character(tolerance), "_", as.character(n_perm),
                  ".rda", sep = "")
# Save result 
save(sp_cv_RF_oob_OK, bygone_time, file = file_name)
##
## End (cross validation)
#### 
################################################################################
## End (RF-oob-OK spatial leave one out cross validation) 
################################################################################
