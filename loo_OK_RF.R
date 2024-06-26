################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sp")
p_load("sf")
p_load("sperrorest")
p_load("parallel")
p_load("doParallel")
p_load("foreach")
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

# Set buffer 
buffer = 3600

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
# Adjusted RF formula
fo = as.formula(bcNitrate ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                  nfk + humus + cAckerland + log10_gwn + agrum_log10_restime + 
                  agrum_log10_gwn + agrum_log10_geschw + Ackerland + 
                  lbm_class_Gruenland + lbm_class_Unbewachsen + 
                  lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + X + Y + 
                  tc45 + tc315 + ok_inter_pred + ok_inter_var + 
                  aea20_1 + aea20_2 + aea20_12 + aea20_13)

# OK formula
ok_fo = as.formula(bcNitrate ~ 1)

# loo-OK buffer distance 
loo_buffer = 0
##
## End Model argument preparation)
####

# Calculate importance for these variables. The standard formula and not the 
# adjusted formula is used because the importance of the OK variables is  
# evaluated together with the X and Y coordinate 
imp_vars_RF = all.vars(fo_RF)[-1]
imp_vars_RF = NULL
################################################################################
## End (preparation)
################################################################################


################################################################################
## RF-loo-OK spatial leave one out cross validation 
################################################################################

####
## loo-OK multicore function
##
loo_OK_fun = function(data, buffer_dist, ok_fo, nno){
  # Leave one out resampling
  resamp = sperrorest::partition_loo(data = data, ndisc = nrow(data), 
                                     replace = FALSE, coords = c("X","Y"), 
                                     buffer = buffer_dist, repetition = 1)
  # Create a spatial points df 
  sp_df = sp::SpatialPointsDataFrame(coords = data[,c("X","Y")], data = data)
  # Foreach for parallel processing
  ok_i_p = foreach::foreach(i = 1:nrow(sp_df),
                            .packages = c("gstat", "automap", "sp")) %dopar%{
                      # Get training and test ids
                      id_train = resamp[["1"]][[i]]$train
                      id_test = resamp[["1"]][[i]]$test
                      
                      # Get a training and test spatial points df 
                      train_sp_df = sp_df[id_train, ]
                      test_sp_df = sp_df[id_test, ]
                      
                      # Fit variogram model
                      resid_vm = automap::autofitVariogram(formula = ok_fo, 
                                                  input_data = train_sp_df, 
                                                  model = c("Mat", "Exp"),
                                                  kappa = c(seq(0.1, 0.4, 0.1)),
                                                  fix.values = c(0, NA, NA))
                      # Get the model
                      resid_vmm = resid_vm["var_model"]$var_model
                      
                      # Interpolation
                      ok_pred = gstat::krige(ok_fo, train_sp_df, 
                                             model = resid_vmm, 
                                             newdata = test_sp_df,
                                             debug.level = 0,
                                             nmax = nno)
                      pred = c(ok_pred$var1.pred, ok_pred$var1.var)
                    }
}

# Start time measurement
start_time_loo_OK = Sys.time()
print(start_time_loo_OK )

# Setup backend to use many processors
totalCores = parallel::detectCores()
# Single core for comparison 
sc = 1

# Leave some cores to reduce computer load
cluster = parallel::makeCluster(sc) 
doParallel::registerDoParallel(cluster)

# Execute loo-OK function
loo_ok_res = loo_OK_fun(data = d, 
                        buffer_dist = loo_buffer, 
                        ok_fo = ok_fo,
                        nno = 200) 

# Stop cluster
stopCluster(cluster)

# Add the results to the df 
loo_ok_res_vec = unlist(loo_ok_res)
ok_inter_pred = loo_ok_res_vec[seq(1, length(loo_ok_res_vec), 2)]
ok_inter_var = loo_ok_res_vec[seq(2, length(loo_ok_res_vec), 2)]
d$ok_inter_pred = ok_inter_pred
d$ok_inter_var = ok_inter_var

# End time measurement
end_time_loo_OK = Sys.time()
bygone_time_loo_OK  = end_time_loo_OK - start_time_loo_OK
print(bygone_time_loo_OK)
##
## End (loo-OK multicore function)
####


####
## Cross validation
## 

## For the variable importance assessment it is necessary to perform the loo-OK
## for the newdata location in the prediction function.

# Create model function 
RF_looOK_fun = function(formula, data, ok_fo){
  ## RF model
  RF_model = ranger::ranger(formula = formula, 
                            data = data,
                            oob.error = FALSE,
                            seed = 7)
  ## Variogram model
  # Create spatial points dataframe 
  sp_df = sp::SpatialPointsDataFrame(coords = data[,c("X","Y")], data = data)
  # Fit variogram model
  resid_vm = automap::autofitVariogram(formula = ok_fo, 
                                       input_data = sp_df, 
                                       model = c("Mat", "Exp"),
                                       kappa = c(seq(0.1, 0.4, 0.1)),
                                       fix.values = c(0, NA, NA))
  # Create return list
  return_list = list(RF_m = RF_model, resid_vmm = resid_vm["var_model"]$var_model, 
                     train_data = sp_df)
  return(return_list)
}

# Create prediction function
RF_looOK_pred_fun = function(object, newdata, ok_fo, nno){
  ## loo-OK
  # Newdata spatial points dataframe 
  new_sp_df = sp::SpatialPointsDataFrame(coords = newdata[,c("X","Y")], 
                                         data = newdata)
  # OK prediction 
  ok_pred = gstat::krige(formula = ok_fo, 
                         locations = object$train_data, 
                         model = object$resid_vmm, 
                         newdata = new_sp_df,
                         debug.level = 0,
                         nmax = nno)
  # Replace OK values of newdata
  newdata$ok_inter_pred = ok_pred$var1.pred 
  newdata$ok_inter_var = ok_pred$var1.var
  ## RF prediction
  RF_prediction = predict(object = object$RF_m,
                          data = newdata)
  return(RF_prediction$predictions)
}

# Start time measurement
start_time = Sys.time()
print(start_time)

# Perform the spatial cross-validation
sp_cv_loo_OK_RF = sperrorest::sperrorest(formula = fo, data = d, 
                                         coords = c("X","Y"), 
                                         model_fun = RF_looOK_fun,
                                         model_args = list(ok_fo = ok_fo),
                                         pred_fun = RF_looOK_pred_fun,
                                         pred_args = list(ok_fo = ok_fo, nno = 200),
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
file_name = paste("Results/",data_set,"_sp_cv_loo_OK_RF_",as.character(round(buffer)),
                  "_+", as.character(tolerance), "_", as.character(n_perm),
                  ".rda", sep = "")

# Save result 
save(sp_cv_loo_OK_RF, bygone_time, bygone_time_loo_OK, file = file_name)
##
## End (cross validation)
#### 
################################################################################
## End (RF-loo-OK spatial leave one out cross validation) 
################################################################################
