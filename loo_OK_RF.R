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

# Load data
data_set = "NuM_L"
load("Data/NuM_L.rda")
d = NuM_L

# Get information about the prediction distance 
pd_df = info_d_NuM_L$predDist_df
mean_pd = mean(pd_df$lyr.1)
med_pd = median(pd_df$lyr.1)

# Set buffer 
buffer = 0

# Set tolerance (all = partition_loo with buffer)
tolerance = 50

# Set number of permutations 
n_perm = 10

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
# RF formula
fo = as.formula(bcNitrate ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                  nfk + humus + cAckerland + log10_gwn + agrum_log10_restime + 
                  agrum_log10_gwn + agrum_log10_geschw + Ackerland + 
                  lbm_class_Gruenland + lbm_class_Unbewachsen + 
                  lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + X + Y + 
                  tc45 + tc315 + ok_inter_pred + ok_inter_var + 
                  aea20_2 + aea20_8 + aea20_12)

# OK formula
ok_fo = as.formula(bcNitrate ~ 1)
##
## End Model argument preparation)
####

# Calculate importance for these variables
imp_vars_llo_OK_RF = all.vars(fo)[-1]
################################################################################
## End (preparation)
################################################################################


################################################################################
## Random Forest with explanatory variables from leave one out Ordinary Kriging  
## (RF-llo-OK) prediction 
################################################################################
################################################################################
## End (RF-llo-OK prediction) 
################################################################################


################################################################################
## RF-llo-OK spatial leave one out cross validation 
################################################################################

####
## llo-OK multicore function
##
llo_OK_fun = function(data, buffer_dist, ok_fo){
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
                                                  kappa = c(0.1,0.2),
                                                  fix.values = c(0, NA, NA))
                      # Get the model
                      resid_vmm = resid_vm["var_model"]$var_model
                      
                      # Interpolation
                      ok_pred = gstat::krige(ok_fo, train_sp_df, 
                                             model = resid_vmm, 
                                             newdata = test_sp_df,
                                             debug.level = 0)
                      pred = c(ok_pred$var1.pred, ok_pred$var1.var)
                    }
}

# Start time measurement
start_time_llo_OK = Sys.time()
print(start_time_llo_OK )

# Setup backend to use many processors
totalCores = parallel::detectCores()
# Single core for comparison 
sc = 1

# Leave some cores to reduce computer load
cluster = parallel::makeCluster(sc) 
doParallel::registerDoParallel(cluster)

# Execute llo-OK function
llo_ok_res = llo_OK_fun(data = d, 
                        buffer_dist = buffer, 
                        ok_fo = ok_fo) 

# Stop cluster
stopCluster(cluster)

# Add the results to the df 
llo_ok_res_vec = unlist(llo_ok_res)
ok_inter_pred = llo_ok_res_vec[seq(1, length(llo_ok_res_vec), 2)]
ok_inter_var = llo_ok_res_vec[seq(2, length(llo_ok_res_vec), 2)]
d$ok_inter_pred = ok_inter_pred
d$ok_inter_var = ok_inter_var

# End time measurement
end_time_llo_OK = Sys.time()
bygone_time_llo_OK  = end_time_llo_OK - start_time_llo_OK
print(bygone_time_llo_OK)
##
## End (llo-OK multicore function)
####


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
sp_cv_llo_OK_RF = sperrorest::sperrorest(formula = fo, data = d, 
                                         coords = c("X","Y"), 
                                  model_fun = RF_fun, 
                                  pred_fun = RF_pred_fun,
                                  smp_fun = partition_fun, 
                                  smp_args = smp_args,
                                  imp_permutations = n_perm,
                                  imp_variables = imp_vars_llo_OK_RF,
                                  imp_sample_from = "all",
                                  distance = TRUE)

# Get test RMSE
test_RMSE = sp_cv_llo_OK_RF$error_rep$test_rmse
test_RMSE

# End time measurement
end_time = Sys.time()
bygone_time = end_time - start_time
print(bygone_time)

# Set file name 
file_name = paste("Results/",data_set,"_sp_cv_llo_OK_RF_",as.character(round(buffer)),
                  "_+", as.character(tolerance), "_", as.character(n_perm),
                  ".rda", sep = "")
# Save result 
save(sp_cv_llo_OK_RF, bygone_time, bygone_time_llo_OK, file = file_name)
##
## End (cross validation)
#### 
################################################################################
## End (RF-llo-OK spatial leave one out cross validation) 
################################################################################
