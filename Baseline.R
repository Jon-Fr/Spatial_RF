################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sperrorest")
p_load("future")

# Additional functions that are not included in packages
source("spdiagnostics-functions.R", encoding = "UTF-8") # Brenning 2022

# Fewer decimal places, apply penalty on exponential notation 
options("scipen"= 999, "digits"=4)

# Load data and formula 
data_set = "WuS_SuB"
load("Data/WuS_SuB.rda")
d = WuS_SuB
fo_lm = fo_lm_WuS_SuB

# Get information about the prediction distance 
pd_df = info_d_WuS_SuB$predDist_df
mean_pd = mean(pd_df$lyr.1)
med_pd = median(pd_df$lyr.1)

# Set buffer 
buffer = med_pd

# Formula for base RF-model
fo = as.formula(bcNitrate ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                  nfk + humus + cAckerland + log10_gwn + agrum_log10_restime + 
                  agrum_log10_gwn + agrum_log10_geschw + Ackerland + 
                  lbm_class_Gruenland + lbm_class_Unbewachsen + 
                  lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + 
                  aea20_1 + aea20_2 + aea20_12 + aea20_13)

# Calculate importance for these variables
imp_vars_lm = all.vars(fo_lm)[-1]
imp_vars_RF = all.vars(fo)[-1]
################################################################################
## End (preparation)
################################################################################


################################################################################
## Get baseline values  
################################################################################

####
## Standard deviation of the variable of interest
##
s_dev = sd(d$bcNitrate)
s_dev
##
## 
#### End (standard deviation of the variable of interest)


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
# Future for parallelization
#future::plan(future.callr::callr, workers = 10)
sp_cv_MLR = sperrorest::sperrorest(formula = fo_lm, data = d, coords = c("X","Y"), 
                                  model_fun = lm_fun, 
                                  pred_fun = lm_pred_fun,
                                  smp_fun = partition_loo, 
                                  smp_args = list(buffer = buffer),
                                  importance = TRUE, 
                                  imp_permutations = 1,
                                  imp_variables = imp_vars_lm,
                                  imp_sample_from = "all")

# Get test RMSE
test_RMSE = sp_cv_MLR$error_rep$test_rmse
test_RMSE

# End time measurement
end_time = Sys.time()
bygone_time = end_time - start_time
print(bygone_time)

# Set file name 
file_name = paste("Results/",data_set,"_sp_cv_MLR_",as.character(round(buffer)),
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
sp_cv_RF = sperrorest::sperrorest(formula = fo, data = d, 
                                  coords = c("X","Y"), 
                                  model_fun = RF_fun, 
                                  pred_fun = RF_pred_fun,
                                  smp_fun = partition_loo, 
                                  smp_args = list(buffer = buffer),
                                  importance = TRUE, 
                                  imp_permutations = 20,
                                  imp_variables = imp_vars_RF,
                                  imp_sample_from = "all")

# Get test RMSE
test_RMSE = sp_cv_RF$error_rep$test_rmse
test_RMSE

# End time measurement
end_time = Sys.time()
bygone_time = end_time - start_time
print(bygone_time)

# Set file name 
file_name = paste("Results/",data_set,"_sp_cv_bRF_",as.character(round(buffer)),
                  ".rda", sep = "")
# Save result 
save(sp_cv_RF, bygone_time, file = file_name)
##
## End (RMSE of a random forest model without coordinates as ...) 
#### 
################################################################################
## (End Get baseline values)
################################################################################