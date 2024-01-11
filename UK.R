################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sp")
p_load("sf")
p_load("sperrorest")
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
fo_lm = fo_lm_WuS_SuB

# Get information about the prediction distance 
pd_df = info_d_WuS_SuB$predDist_df
max_pd = max(pd_df$lyr.1)
mean_pd = mean(pd_df$lyr.1)
med_pd = median(pd_df$lyr.1)

# Set buffer 
buffer = 0

# Set tolerance (all = partition_loo with buffer)
tolerance = "all"

# Set number of permutations 
n_perm = 10

# Calculate importance for these variables
imp_vars_lm = all.vars(fo_lm)[-1]

# Create a spatial points df 
sp_df = sp::SpatialPointsDataFrame(d[,c("X","Y")], d)

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
## Universal Kriging (UK) prediction/interpolation 
################################################################################
################################################################################
## End (UK prediction/interpolation)
################################################################################


################################################################################
## UK preliminary analyses for NuM_L dataset
################################################################################
pA = FALSE
if (pA){
  ##### 
  ## Directional semivariograms and multiple linear regression
  ## 
  
  ## Directional semivariograms
  dir_svgm_fd = gstat::variogram(fo_lm, data=sp_df, cutoff = max_pd, width = 1000, 
                                 alpha = c(0, 45, 90, 135)) 
  dir_svgm_sd = gstat::variogram(fo_lm, data=sp_df, cutoff = med_pd, width = 300,
                                 alpha = c(0, 45, 90, 135)) 
  #0:north, 45:north-east, 90:east, 135:south-east 
  plot(dir_svgm_fd, cex.lab = 1.25, xlab = "distance (m)")
  plot(dir_svgm_sd, cex.lab = 1.25, xlab = "distance (m)")
  
  
  ## Conduct multiple linear regression (MLR)
  # Create MLR model
  MLR_model = lm(fo_lm, data=d)
  summary(MLR_model)
  # Some diagnostic plots
  plot(residuals(MLR_model) ~ predict(MLR_model), cex.lab = 1.25,
       xlab = "predicted values", ylab = "residuals")
  abline(h=0,lty="dashed")
  # Add residuals to sp_df
  sp_df$mlr_resi = MLR_model$residuals
  ##
  ## End (directional semivariograms and multiple linear regression)
  ####
  
  
  ####
  ## Variogram model fitting
  ##
  
  ## Empirical semivariogram (EmSv) of the residuals 
  ## of the multiple linear regression model
  # Full distance
  emp_svario_resi_fd = gstat::variogram(mlr_resi~1, data=sp_df, 
                                        cutoff = max_pd, width = 1000)  
  plot(emp_svario_resi_fd$dist, emp_svario_resi_fd$gamma)
  
  # Short distance (median predction distance)
  emp_svario_resi_sd = gstat::variogram(mlr_resi~1, data=sp_df, cutoff = med_pd, 
                                        width = 100) 
  plot(emp_svario_resi_sd$dist, emp_svario_resi_sd$gamma)
  
  
  ## Fit variogram model to OLS residuals (semi-automatic)
  # Fit model by eye 
  resid_vm = gstat::vgm(model = "Exc", range = 1000, psill = 8)
  print(plot(emp_svario_resi_fd, pl = FALSE, model = resid_vm))
  print(plot(emp_svario_resi_sd, pl = FALSE, model = resid_vm))
  
  # Adjust the variogram model with gstat automatic fit 
  resid_vmf = fit.variogram(object =  emp_svario_resi_sd, resid_vm)
  print(plot(emp_svario_resi_fd, pl = FALSE, model = resid_vmf))
  print(plot(emp_svario_resi_sd, pl = FALSE, model = resid_vmf))
  
  ## Fit variogram model to OLS residuals (automatic)
  resid_vmf_au = automap::autofitVariogram(formula = fo_lm, 
                                           input_data = sp_df, 
                                           model = c("Mat", "Exp"),
                                           kappa = c(0.1,0.2,0.3),
                                           fix.values = c(0, NA, NA),
                                           verbose = TRUE)
  print(plot(emp_svario_resi_fd, pl = FALSE, 
             model = resid_vmf_au["var_model"]$var_model))
  print(plot(emp_svario_resi_sd, pl = FALSE, 
             model = resid_vmf_au["var_model"]$var_model))
  
  ## Empirical semivariogram (EmSv) of the residuals 
  ## of a generalized least squares regression model
  
  # Prepare the correlogram for the GLS model 
  # Exc is not available therefore Exp is used
  # Get the range 
  resid_vm_exp = gstat::vgm(model = "Exp", range = 1000, psill = 7.5)
  print(plot(emp_svario_resi_fd, pl = FALSE, model = resid_vm_exp ))
  print(plot(emp_svario_resi_sd, pl = FALSE, model = resid_vm_exp ))
  range = resid_vm_exp[1,"range"]
  # gstat automatic fit does not work
  #resid_vmf_exp = fit.variogram(object =  emp_svario_resi_sd, resid_vm_exp)
  #print(plot(emp_svario_resi_fd, pl = FALSE, model = resid_vmf_exp))
  #print(plot(emp_svario_resi_sd, pl = FALSE, model = resid_vmf_exp))
  #range = resid_vmf_exp[1,"range"]
  
  # Create correlogram for the GLS model
  autocor = nlme::corExp(c(range), nugget=FALSE, fixed = TRUE)
  autocor = nlme::Initialize(autocor, data = sp_df)
  autocor
  
  # Perform gls regression
  gls_model = nlme::gls(fo_lm, correlation = autocor, data = sp_df)
  summary(gls_model)
  # Add residuals to sp_df
  sp_df$gls_resi = resid(gls_model)
  
  # EmSv (full distance)
  emp_svario_gls_resi_fd = variogram(gls_resi ~ 1, data=sp_df, 
                                     cutoff = max_pd, width = 1000)
  plot(emp_svario_gls_resi_fd$dist, emp_svario_gls_resi_fd$gamma)
  
  # EmSv (short distance)                             
  emp_svario_gls_resi_sd = variogram(gls_resi ~ 1, data=sp_df, cutoff = med_pd, 
                                     width = 100)
  plot(emp_svario_gls_resi_sd$dist, emp_svario_gls_resi_sd$gamma)
  
  
  ## Fit variogram model to GLS residuals (semi-automatic)
  # Fit model by eye 
  gls_resid_vm = gstat::vgm(model = "Exc", range = 1000, psill = 8)
  print(plot(emp_svario_gls_resi_fd, pl = FALSE, model = gls_resid_vm))
  print(plot(emp_svario_gls_resi_sd, pl = FALSE, model = gls_resid_vm))
  
  # Adjust the variogram model with gstat automatic fit 
  gls_resid_vmf = fit.variogram(object =  emp_svario_gls_resi_sd, gls_resid_vm)
  print(plot(emp_svario_gls_resi_fd, pl = FALSE, model = gls_resid_vmf))
  print(plot(emp_svario_gls_resi_sd, pl = FALSE, model = gls_resid_vmf))
  
  ## Fit variogram model to GLS residuals (automatic)
  ## (results are okay)
  gls_resid_vmf_au = automap::autofitVariogram(formula = fo_lm, 
                                           input_data = sp_df, 
                                           model = c("Mat", "Exp"),
                                           fix.values = c(0, NA, NA),
                                           kappa = c(0.1, 0.2),
                                           GLS.model = resid_vmf_au["var_model"]$var_model,
                                           verbose = TRUE)
  
  print(plot(emp_svario_gls_resi_fd, pl = FALSE, 
             model = gls_resid_vmf_au["var_model"]$var_model))
  print(plot(emp_svario_gls_resi_sd, pl = FALSE, 
             model = gls_resid_vmf_au["var_model"]$var_model))
  ##
  ## Variogram model fitting
  ####
}
################################################################################
## End (UK preliminary analyses for NuM_L dataset)
################################################################################

################################################################################
## UK preliminary analyses for Wus_SuB dataset
################################################################################
pA = TRUE
if (pA){
  ##### 
  ## Directional semivariograms and multiple linear regression
  ## 
  
  ## Directional semivariograms
  dir_svgm_fd = gstat::variogram(fo_lm, data=sp_df, cutoff = max_pd, width = 1000, 
                                 alpha = c(0, 45, 90, 135)) 
  dir_svgm_sd = gstat::variogram(fo_lm, data=sp_df, cutoff = med_pd, width = 1000,
                                 alpha = c(0, 45, 90, 135)) 
  #0:north, 45:north-east, 90:east, 135:south-east 
  plot(dir_svgm_fd, cex.lab = 1.25, xlab = "distance (m)")
  plot(dir_svgm_sd, cex.lab = 1.25, xlab = "distance (m)")
  
  
  ## Conduct multiple linear regression (MLR)
  # Create MLR model
  MLR_model = lm(fo_lm, data=d)
  summary(MLR_model)
  # Some diagnostic plots
  plot(residuals(MLR_model) ~ predict(MLR_model), cex.lab = 1.25,
       xlab = "predicted values", ylab = "residuals")
  abline(h=0,lty="dashed")
  # Add residuals to sp_df
  sp_df$mlr_resi = MLR_model$residuals
  ##
  ## End (directional semivariograms and multiple linear regression)
  ####
  
  
  ####
  ## Variogram model fitting
  ##
  
  ## Empirical semivariogram (EmSv) of the residuals 
  ## of the multiple linear regression model
  # Full distance
  emp_svario_resi_fd = gstat::variogram(mlr_resi~1, data=sp_df, 
                                        cutoff = max_pd, width = 1000)  
  plot(emp_svario_resi_fd$dist, emp_svario_resi_fd$gamma)
  
  # Short distance (median predction distance)
  emp_svario_resi_sd = gstat::variogram(mlr_resi~1, data=sp_df, cutoff = med_pd, 
                                        width = 200) 
  plot(emp_svario_resi_sd$dist, emp_svario_resi_sd$gamma)
  
  
  ## Fit variogram model to OLS residuals (semi-automatic)
  # Fit model by eye 
  resid_vm = gstat::vgm(model = "Exc", range = 1100, psill = 5.5)
  print(plot(emp_svario_resi_fd, pl = FALSE, model = resid_vm))
  print(plot(emp_svario_resi_sd, pl = FALSE, model = resid_vm))
  
  # Adjust the variogram model with gstat automatic fit 
  resid_vmf = fit.variogram(object =  emp_svario_resi_sd, resid_vm)
  print(plot(emp_svario_resi_fd, pl = FALSE, model = resid_vmf))
  print(plot(emp_svario_resi_sd, pl = FALSE, model = resid_vmf))
  
  ## Fit variogram model to OLS residuals (automatic)
  resid_vmf_au = automap::autofitVariogram(formula = fo_lm, 
                                           input_data = sp_df, 
                                           model = c("Mat", "Exp"),
                                           kappa = c(0.1,0.2,0.3),
                                           fix.values = c(0, NA, NA),
                                           verbose = TRUE)
  print(plot(emp_svario_resi_fd, pl = FALSE, 
             model = resid_vmf_au["var_model"]$var_model))
  print(plot(emp_svario_resi_sd, pl = FALSE, 
             model = resid_vmf_au["var_model"]$var_model))
  
  ## Empirical semivariogram (EmSv) of the residuals 
  ## of a generalized least squares regression model
  
  # Prepare the correlogram for the GLS model 
  # Exc is not available therefore Exp is used
  # Get the range 
  resid_vm_exp = gstat::vgm(model = "Exp", range = 1000, psill = 5.5)
  print(plot(emp_svario_resi_fd, pl = FALSE, model = resid_vm_exp ))
  print(plot(emp_svario_resi_sd, pl = FALSE, model = resid_vm_exp ))
  range = resid_vm_exp[1,"range"]
  # gstat automatic fit does not work
  #resid_vmf_exp = fit.variogram(object =  emp_svario_resi_sd, resid_vm_exp)
  #print(plot(emp_svario_resi_fd, pl = FALSE, model = resid_vmf_exp))
  #print(plot(emp_svario_resi_sd, pl = FALSE, model = resid_vmf_exp))
  #range = resid_vmf_exp[1,"range"]
  
  # Create correlogram for the GLS model
  autocor = nlme::corExp(c(range), nugget=FALSE, fixed = TRUE)
  autocor = nlme::Initialize(autocor, data = sp_df)
  autocor
  
  # Perform gls regression
  gls_model = nlme::gls(fo_lm, correlation = autocor, data = sp_df)
  summary(gls_model)
  # Add residuals to sp_df
  sp_df$gls_resi = resid(gls_model)
  
  # EmSv (full distance)
  emp_svario_gls_resi_fd = variogram(gls_resi ~ 1, data=sp_df, 
                                     cutoff = max_pd, width = 1000)
  plot(emp_svario_gls_resi_fd$dist, emp_svario_gls_resi_fd$gamma)
  
  # EmSv (short distance)                             
  emp_svario_gls_resi_sd = variogram(gls_resi ~ 1, data=sp_df, cutoff = med_pd, 
                                     width = 200)
  plot(emp_svario_gls_resi_sd$dist, emp_svario_gls_resi_sd$gamma)
  
  
  ## Fit variogram model to GLS residuals (semi-automatic)
  # Fit model by eye 
  gls_resid_vm = gstat::vgm(model = "Exc", range = 1000, psill = 5)
  print(plot(emp_svario_gls_resi_fd, pl = FALSE, model = gls_resid_vm))
  print(plot(emp_svario_gls_resi_sd, pl = FALSE, model = gls_resid_vm))
  
  # Adjust the variogram model with gstat automatic fit 
  gls_resid_vmf = fit.variogram(object =  emp_svario_gls_resi_sd, gls_resid_vm)
  print(plot(emp_svario_gls_resi_fd, pl = FALSE, model = gls_resid_vmf))
  print(plot(emp_svario_gls_resi_sd, pl = FALSE, model = gls_resid_vmf))
  
  ## Fit variogram model to GLS residuals (automatic)
  ## (results are okay)
  gls_resid_vmf_au = automap::autofitVariogram(formula = fo_lm, 
                                               input_data = sp_df, 
                                               model = c("Mat", "Exp"),
                                               fix.values = c(0, NA, NA),
                                               kappa = c(0.1, 0.2),
                                               GLS.model = resid_vmf_au["var_model"]$var_model,
                                               verbose = TRUE)
  
  print(plot(emp_svario_gls_resi_fd, pl = FALSE, 
             model = gls_resid_vmf_au["var_model"]$var_model))
  print(plot(emp_svario_gls_resi_sd, pl = FALSE, 
             model = gls_resid_vmf_au["var_model"]$var_model))
  ##
  ## Variogram model fitting
  ####
}
################################################################################
## End (UK preliminary analyses for Wus_SuB dataset)
################################################################################


################################################################################
## UK spatial leave one out cross validation 
################################################################################

####
## Cross validation
## 

# Create model function 
vmf_fun = function(formula, data){
  # Create a spatial points df 
  sp_df = sp::SpatialPointsDataFrame(coords = data[,c("X","Y")], data = data)
  # OLS residual variogram model fitting
  resid_vmf = automap::autofitVariogram(formula = formula, input_data = sp_df,
                                        model = c("Mat", "Exp"),
                                        kappa = c(0.1, 0.2),
                                        fix.values = c(0, NA, NA))
  # GLS residual variogram model fitting
  #resid_vmf_gls = automap::autofitVariogram(formula = formula,
  #                                          input_data = sp_df, 
  #                                          model = c("Mat", "Exp"),
  #                                          kappa = c(0.1, 0.2),
  #                                          fix.values = c(0, NA, NA),
  #                                          GLS.model = resid_vmf["var_model"]$var_model)
  # Return variogram model and training data
  return_list = list(model = resid_vmf["var_model"]$var_model, 
                     train_data = sp_df)
  return(return_list)
}

# Create prediction function
UK_pred_fun = function(object, newdata, formula){
  # Get the training data
  sp_df_train = object$train_data
  # Create spatial points df
  sp_df_newdata = sp::SpatialPointsDataFrame(newdata[,c("X","Y")], newdata)
  # Prediction
  uk_pred = gstat::krige(formula = formula, 
                         locations = sp_df_train,
                         model = object$model,
                         newdata = sp_df_newdata,
                         debug.level = 0)
  return(uk_pred$var1.pred)
}

# Start time measurement
start_time = Sys.time()
print(start_time)

# Perform the spatial cross-validation
sp_cv_UK = sperrorest::sperrorest(formula = fo_lm, data = d, 
                                  coords = c("X","Y"), 
                                  model_fun = vmf_fun, 
                                  pred_fun = UK_pred_fun,
                                  pred_args = list(formula = fo_lm),
                                  smp_fun = partition_fun, 
                                  smp_args = smp_args,
                                  imp_permutations = n_perm,
                                  imp_variables = imp_vars_lm,
                                  imp_sample_from = "all",
                                  distance = TRUE)

# Get test RMSE
test_RMSE = sp_cv_UK$error_rep$test_rmse
test_RMSE

# End time measurement
end_time = Sys.time()
bygone_time = end_time - start_time
print(bygone_time)

# Set file name 
file_name = paste("Results/", data_set,"_sp_cv_UK_", as.character(round(buffer)),
                  "_+", as.character(tolerance), "_", as.character(n_perm),
                  ".rda", sep = "")
# Save result 
save(sp_cv_UK, bygone_time, file = file_name)
##
## End (cross validation)
####
################################################################################
## End UK spatial leave one out cross validation)
################################################################################
