################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sperrorest")
p_load("ranger")

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
################################################################################
## End (preparation)
################################################################################


################################################################################
## RF spatial leave one out cross validation 
################################################################################

####
## Cross validation
## 

# Create model function 
RF_fun = function(formula, data){
  RF_model = ranger::ranger(formula = formula, 
                            data = data,
                            oob.error = FALSE,
                            seed = 7)
  return(RF_model)
}

# Create prediction function
RF_pred_fun = function(object, newdata){
  RF_prediction = predict(object = object,
                          data = newdata)
  return(RF_prediction$predictions)
}

# Start time measurement
start_time = Sys.time()
print(start_time)

# Perform the spatial cross-validation
sp_cv_RF = sperrorest::sperrorest(formula = fo_RF, data = d, 
                                  coords = c("X","Y"), 
                                  model_fun = RF_fun, 
                                  pred_fun = RF_pred_fun,
                                  smp_fun = partition_fun, 
                                  smp_args = smp_args,
                                  imp_permutations = n_perm,
                                  imp_variables = imp_vars_RF,
                                  imp_sample_from = "all",
                                  distance = TRUE,
                                  err_fun = error_fun)

# End time measurement
end_time = Sys.time()
bygone_time = end_time - start_time
print(bygone_time)

# Set file name 
file_name = paste("Results/", data_set,"_sp_cv_RF_", as.character(round(buffer)),
                  "_+", as.character(tolerance), "_", as.character(n_perm),
                  ".rda", sep = "")
# Save result 
save(sp_cv_RF, bygone_time, file = file_name)
##
## End (cross validation)
#### 
################################################################################
## End (Random Forest (RF) spatial leave one out cross validation) 
################################################################################
