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
fo_lm = fo_lm_NuM_L_bc

# Set buffer 
buffer = 40000

# Set tolerance (all = partition_loo without buffer)
tolerance = 100

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

# Formula for base RF-model
fo = as.formula(bcNitrate ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                  nfk + humus + cAckerland + log10_gwn + agrum_log10_restime + 
                  agrum_log10_gwn + agrum_log10_geschw + Ackerland + 
                  lbm_class_Gruenland + lbm_class_Unbewachsen + 
                  lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + 
                  aea20_2 + aea20_8 + aea20_12)

# Calculate importance for these variables
imp_vars_lm = all.vars(fo_lm)[-1]
imp_vars_lm = NULL
imp_vars_bRF = all.vars(fo)[-1]
imp_vars_lm = NULL

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
## Get baseline values  
################################################################################

####
## Standard deviation of the variable of interest, intercept model RMSE
##
s_dev = sd(d$subMittelwert )
s_dev

# Formula
fo_ic = subMittelwert ~ 1

# Model and prediction function 
ic_fun = function(formula, data){
  lm_m = lm(formula, data)
  return(lm_m)
}
ic_pred_fun = function(object, newdata){
  predi = predict(object = object, newdata = newdata)
  return(predi)
}

# Spatial cross validation
sp_cv_ic = sperrorest::sperrorest(formula = fo_ic, data = d, coords = c("X","Y"), 
                                   model_fun = ic_fun, 
                                   pred_fun = ic_pred_fun,
                                   smp_fun = partition_fun, 
                                   smp_args = smp_args,
                                   distance = TRUE)

# Get test RMSE
test_RMSE = summary(sp_cv_ic$error_fold)
test_RMSE
##
## 
#### End (standard deviation of the variable of interest, intercept RMSE)


####
## RMSE of linear model 
## 
# Create model function 
lm_fun = function(formula, data){
  lm_m = lm(formula, data)
  return(lm_m)
}

# Create prediction function
lm_pred_fun = function(object, newdata){
  predi = predict(object = object, newdata = newdata)
  return(predi)
}

# Start time measurement
start_time = Sys.time()
print(start_time)

# Perform the spatial cross-validation
sp_cv_MLR = sperrorest::sperrorest(formula = fo, data = d, coords = c("X","Y"), 
                                  model_fun = lm_fun, 
                                  pred_fun = lm_pred_fun,
                                  smp_fun = partition_fun, 
                                  smp_args = smp_args,
                                  imp_permutations = n_perm,
                                  imp_variables = imp_vars_lm,
                                  imp_sample_from = "all",
                                  distance = TRUE,
                                  err_fun = error_fun)

# End time measurement
end_time = Sys.time()
bygone_time = end_time - start_time
print(bygone_time)

# Set file name 
file_name = paste("Results/", data_set, "_sp_cv_MLR_", as.character(round(buffer)),
                  "_+", as.character(tolerance), "_", as.character(n_perm), 
                  ".rda", sep = "")
# Save result 
save(sp_cv_MLR, bygone_time, file = file_name)
##
## End (RMSE of linear model)
####


####
## RMSE of a random forest model without coordinates as explanatory variables 
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
# Future for parallelization
#future::plan(future.callr::callr, workers = 10)
sp_cv_bRF = sperrorest::sperrorest(formula = fo, data = d, 
                                  coords = c("X","Y"), 
                                  model_fun = RF_fun, 
                                  pred_fun = RF_pred_fun,
                                  smp_fun = partition_fun, 
                                  smp_args = smp_args,
                                  imp_permutations = n_perm,
                                  imp_variables = imp_vars_bRF,
                                  imp_sample_from = "all",
                                  distance = TRUE,
                                  err_fun = error_fun)

# End time measurement
end_time = Sys.time()
bygone_time = end_time - start_time
print(bygone_time)

# Set file name 
file_name = paste("Results/", data_set, "_sp_cv_bRF_", as.character(round(buffer)),
                  "_+", as.character(tolerance), "_", as.character(n_perm), 
                  ".rda", sep = "")
# Save result 
save(sp_cv_bRF, bygone_time, file = file_name)
##
## End (RMSE of a random forest model without coordinates as ...) 
#### 
################################################################################
## (End Get baseline values)
################################################################################
