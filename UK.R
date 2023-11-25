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

p_load("dplyr")

p_load("gstat")       
p_load("nlme")
p_load("automap")

# Additional functions that are not included in packages
source("auxiliary_functions.R", encoding = "UTF-8")
source("spdiagnostics-functions.R", encoding = "UTF-8") # Brenning 2022

# Fewer decimal places, apply penalty on exponential notation 
options("scipen"= 999, "digits"=4)

# Load data (for now use a subset)
load("Data/")
d = 

# Adjusted formula
fo = as.formula(bcNitrate ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                  cAckerland + log10_gwn + agrum_log10_restime + Ackerland + 
                  lbm_class_Gruenland + lbm_class_Unbewachsen + 
                  lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + 
                  aea20_13 + aea20_6 + X + Y)
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

#### 
## Preparation 
##

# Create a spatial points df 
sp_df = sp::SpatialPointsDataFrame(d[,c("X","Y")], d)


# Get information about the prediction distance 
info_pd = info_predDist(path_predArea = "Data/NuM_L_sub_2_prediction_area.gpkg", 
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
##
## End (preparation )
####

##### 
## Directional semivariograms and multiple linear regression
## 

## Directional semivariograms
dir_svgm_fd = gstat::variogram(fo, data=sp_df, cutoff = max_pd, 
                               alpha = c(0, 45, 90, 135)) 
dir_svgm_sd = gstat::variogram(fo, data=sp_df, cutoff = mean_pd, 
                               alpha = c(0, 45, 90, 135)) 
#0:north, 45:north-east, 90:east, 135:south-east 
plot(dir_svgm_fd, cex.lab = 1.25, xlab = "distance (m)")
plot(dir_svgm_sd, cex.lab = 1.25, xlab = "distance (m)")


## Conduct multiple linear regression (MLR)
# Create MLR model
MLR_model = lm(fo, data=d)
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
emp_svario_resi_fd = gstat::variogram(mlr_resi~1, data=sp_df, cressie = TRUE, 
                                      cutoff = max_pd, width = 1000)  
plot(emp_svario_resi_fd$dist, emp_svario_resi_fd$gamma)

# Short distance (mean predction distance)
emp_svario_resi_sd = gstat::variogram(mlr_resi~1, data=sp_df, cutoff = mean_pd, 
                                      cressie = TRUE, width = 100) 
plot(emp_svario_resi_sd$dist, emp_svario_resi_sd$gamma)


## Fit variogram model to OLS residuals (semi-automatic)
# Fit model by eye 
resid_vm = gstat::vgm(model = "Exc", range = 2750, psill = 16)
print(plot(emp_svario_resi_fd, pl = FALSE, model = resid_vm))
print(plot(emp_svario_resi_sd, pl = FALSE, model = resid_vm))

# Adjust the variogram model with gstat automatic fit 
# (does not work well in this case)
#resid_vmf = fit.variogram(object =  emp_svario_resi_sd, resid_vm)
#print(plot(emp_svario_resi_fd, pl = FALSE, model = resid_vmf))
#print(plot(emp_svario_resi_sd, pl = FALSE, model = resid_vmf))

## Fit variogram model to OLS residuals (automatic)
## (results are okay)
resid_vmf_au = automap::autofitVariogram(formula = fo, 
                                         input_data = sp_df, 
                                         model = c("Mat", "Exp"),
                                         fix.values = c(0, NA, NA),
                                         cressie = TRUE,
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
resid_vm_exp = gstat::vgm(model = "Exp", range = 2000, psill = 14)
print(plot(emp_svario_resi_fd, pl = FALSE, model = resid_vm_exp ))
print(plot(emp_svario_resi_sd, pl = FALSE, model = resid_vm_exp ))
range = resid_vm_exp[1,"range"]
#resid_vmf_exp = fit.variogram(object =  emp_svario_resi_sd, resid_vm_exp)
#print(plot(emp_svario_resi_fd, pl = FALSE, model = resid_vmf_exp))
#print(plot(emp_svario_resi_sd, pl = FALSE, model = resid_vmf_exp))
#range = resid_vmf_exp[1,"range"]

# Create correlogram for the GLS model
autocor = nlme::corExp(c(range), nugget=FALSE, fixed = TRUE)
autocor = nlme::Initialize(autocor, data = sp_df)
autocor

# Perform gls regression
gls_model = nlme::gls(fo, correlation = autocor, data = sp_df)
summary(gls_model)
# Add residuals to sp_df
sp_df$gls_resi = resid(gls_model)

# EmSv (full distance)
emp_svario_gls_resi_fd = variogram(gls_resi ~ 1, data=sp_df, 
                                   cressie = TRUE, cutoff = max_pd, 
                                   width = 1000)
plot(emp_svario_gls_resi_fd$dist, emp_svario_gls_resi_fd$gamma)

# EmSv (short distance)                             
emp_svario_gls_resi_sd = variogram(gls_resi ~ 1, data=sp_df, 
                                   cressie = TRUE, cutoff = mean_pd, 
                                   width = 300)
plot(emp_svario_gls_resi_sd$dist, emp_svario_gls_resi_sd$gamma)


## Fit variogram model to GLS residuals (semi-automatic)
# Fit model by eye 
gls_resid_vm = gstat::vgm(model = "Exc", range = 2500, psill = 15.5)
print(plot(emp_svario_gls_resi_fd, pl = FALSE, model = gls_resid_vm))
print(plot(emp_svario_gls_resi_sd, pl = FALSE, model = gls_resid_vm))

# Adjust the variogram model with gstat automatic fit 
# (does not work well in this case)
#gls_resid_vmf = fit.variogram(object =  emp_svario_gls_resi_sd, gls_resid_vm)
#print(plot(emp_svario_gls_resi_fd, pl = FALSE, model = gls_resid_vmf))
#print(plot(emp_svario_gls_resi_sd, pl = FALSE, model = gls_resid_vmf))

## Fit variogram model to GLS residuals (automatic)
## (results are okay)
gls_resid_vmf_au = automap::autofitVariogram(formula = fo, 
                                         input_data = sp_df, 
                                         model = c("Mat", "Exp"),
                                         fix.values = c(0, NA, NA),
                                         GLS.model = resid_vmf_au["var_model"]$var_model,
                                         cressie = TRUE,
                                         verbose = TRUE)

print(plot(emp_svario_gls_resi_fd, pl = FALSE, 
           model = gls_resid_vmf_au["var_model"]$var_model))
print(plot(emp_svario_gls_resi_sd, pl = FALSE, 
           model = gls_resid_vmf_au["var_model"]$var_model))
##
## Variogram model fitting
####

################################################################################
## End (UK preliminary analyses for NuM_L dataset)
################################################################################


################################################################################
## UK spatial leave one out cross validation 
################################################################################
#####
## Get information about the prediction distance 
## 
info_pd = info_predDist(path_predArea = "Data/NuM_L_Gebiet.gpkg", 
                        dataPoints_df = d,
                        c_r_s = "EPSG:25832",
                        resolution = 100,
                        xy = c("X", "Y"))

pd_df = info_pd$predDist_df
hist(pd_df$lyr.1)
third_quartile = quantile(x = pd_df$lyr.1, probs = c(0.75))
tq_pd = third_quartile
max_pd = 135010
mean_pd = 28075
sd_pd = 30275
med_pd = 14711
mad_pd = 18508
##
## End (get information about the prediction distance )
####


####
## Cross validation
## 

# Create model function 
vmf_fun = function(formula, data){
  # Create a spatial points df 
  sp_df = sp::SpatialPointsDataFrame(coords = data[,c("X","Y")], data = data)
  # OLS residual variogram model fitting
  resid_vmf = automap::autofitVariogram(formula = formula, input_data = sp_df,
                                        model = c("Mat", "Exp"), cressie = TRUE)
  # GLS residual variogram model fitting
  resid_vmf_gls = automap::autofitVariogram(formula = formula,
                                            input_data = sp_df, 
                                            model = c("Mat", "Exp"),
                                            GLS.model = resid_vmf["var_model"]
                                            $var_model, cressie = TRUE)
  # Return variogram model and training data
  return_list = list(model = resid_vmf_gls["var_model"]$var_model, 
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

# Perform the spatial cross-validation
# Future for parallelization
#future::plan(future.callr::callr, workers = 10)
sp_cv_UK = sperrorest::sperrorest(formula = fo, data = d, coords = c("X","Y"), 
                                  model_fun = vmf_fun, 
                                  pred_fun = UK_pred_fun,
                                  pred_args = list(formula = fo),
                                  smp_fun = partition_loo, 
                                  smp_args = list(buffer = info_pd$med_predDist))

# Get test RMSE
test_RMSE = sp_cv_UK$error_rep$test_rmse
test_RMSE
##
## End (cross validation)
####
################################################################################
## End UK spatial leave one out cross validation)
################################################################################


################################################################################
## Spatial prediction error profile (SPEP) and 
# spatial variable importance profile (SVIP) (Brenning 2022) 
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
                                        model = c("Sph"), cressie = TRUE)
  # GLS residual variogram model fitting
  resid_vmf_gls = automap::autofitVariogram(formula = formula,
                                            input_data = sp_df, 
                                            model = c("Sph"),
                                            GLS.model = resid_vmf["var_model"]
                                            $var_model, cressie = TRUE)
  # return variogram model an training data
  return_list = list(model = resid_vmf_gls["var_model"]$var_model, 
                     train_data = sp_df)
  return(return_list)
}


# Create prediction function
UK_pred_fun = function(object, newdata, formula){
  # Get training data
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

# Arguments for partition_discs2
smp_args = list(maxdist = 20000, 
                 repetition = 16000, 
                 seed1 = 123)

# Perform spatial leave one out cross validation with random buffer distance 
# between 0 and maxdist
# Future for parallelization
#future::plan(future.callr::callr, workers = 10)
SPEP_UK = sperrorest(formula = fo, data = d, coords = c("X","Y"), 
                     model_fun = vmf_fun, 
                     pred_fun = UK_pred_fun,
                     pred_args = list(formula = fo),
                     smp_fun = partition_discs2,
                     smp_args = smp_args,
                     importance = TRUE,
                     imp_permutations = 1,
                     imp_sample_from = "all",
                     distance = TRUE, 
                     verbose = 1, progress = FALSE,
                     # do not run this in parallel mode:
                     mode_rep = "loop", mode_fold = "loop",
                     err_fun = err_meuse)
##
## End (cross validation)
#### 


####
## Plot SPEP 
##

# Store result in list
res = list(SPEP_UK)

# Extract RMSE function
extract_errors <- function(res, type = "test") {
  res$error_fold %>% map(errdists, type = type) %>% dplyr::bind_rows()
}

# Spatial leave-one-out RMSE
ed <- lapply(res, extract_errors)

# Compute smoothed prediction error profiles
MAXDIST = 20000 
brk <- seq(300, 0.95*MAXDIST, length = 30)
rmse <- lapply(ed, function(x) smth(rmses0(x, breaks = brk)))

# Plot smoothed SPEP
nm <- "1"
plot(rmse[[1]]$dist, rmse[[1]]$smth_rmse, type = "l", xlab = "distance", 
     ylab = paste(nm, "RMSE"), main = nm); abline(h = 0)
##
## End (plot (standard) SPEP) 
####


####
## Plot SVIP
##

# Extract importance function
extract_importance <- function(res) {
  # In this context, 'bias' is simply the difference between observed
  # and predicted - there is no averaging since the test set contains
  # only one observation in LOO CV.
  imp <- res$importance[ sapply(res$importance, function(x) class(x) != "resampling") ]
  imp <- imp %>% flatten() %>% map("bias") %>% as.data.frame() %>% t()
  rownames(imp) <- NULL
  vnms <- rownames(res$importance[[1]][[1]])
  colnames(imp) <- vnms
  imp <- as.data.frame(imp)
  imp$dist <- (res$error_fold %>% map(biasdists) %>% dplyr::bind_rows())$dist
  imp$obs <- res$error_fold %>% map(function(x) x[[1]]$test$obs) %>% unlist() %>% unname()
  imp$pred <- res$error_fold %>% map(function(x) x[[1]]$test$pred) %>% unlist() %>% unname()
  imp$bias <- res$error_fold %>% map(function(x) x[[1]]$test$bias) %>% unlist() %>% unname()
  # Reconstruct the value predicted with permuted feature values:
  for (vnm in vnms) {
    imp[,vnm] <- imp[,vnm] + imp$bias
  }
  imp
}

extract_dmer <- function(imp, breaks, nms = NULL, normalize = FALSE) {
  if (is.null(nms)) {
    nms <- colnames(imp)
    nms <- nms[ !(nms %in% c("dist", "obs", "pred", "bias")) ]
  }
  dmer <- nms %>% 
    map(function(x) rmses0(imp[,c(x,"obs","dist")], which = x, breaks = breaks))
  names(dmer) <- nms
  dmer <- dmer %>% map(2) %>% dplyr::bind_cols() %>% as.data.frame()
  if (normalize)
    dmer <- dmer / rowSums(dmer)
  dmer$dist <- (breaks[-1] + breaks[-length(breaks)]) / 2
  dmer <- smth(dmer)
  dmer
}

# For each model, extract permuted predictions and distances:
imp <- lapply(res, extract_importance)
# These are NOT yet the importances.

# For each model, compute binned permuted predictions:
dmer <- lapply(imp, extract_dmer, breaks = brk)

# Compute permutation importances by subtracting the corresponding
# unpermuted RMSE:
vnms <- all.vars(fo)[-1]
for (m in names(dmer)) { # for each model
  dmer[[m]] <- dmer[[m]][,c(vnms, "dist")] # drop 'dist'
  for (vnm in vnms) { # for each predictor
    dmer[[m]][,vnm] <- dmer[[m]][,vnm] - rmse[[m]]$rmse
  }
  # Smoothed profile:
  dmer[[m]] <- smth(dmer[[m]])
}

nm = "1"
# Plot smoothed SVIP
plot(dmer[[1]]$dist, dmer[[1]]$cgeschw, type = "l", xlab = "distance", 
     ylab = paste(nm, "RMSE"), main = nm); abline(h = 0)
##
## End (plot SVIP)
####
################################################################################
## End (SPEP and SVIP)
################################################################################


################################################################################
## Test area
################################################################################

####
## Explore the relationship between buffer distance and RMSE
##

# Start time measurement
start_time = Sys.time()
print(start_time)

# Setup backend to use many processors
totalCores = parallel::detectCores()

# Leave two cores to reduce computer load
cluster = parallel::makeCluster(totalCores[1]-2) 
doParallel::registerDoParallel(cluster)

# explore
test = data.frame(seq(0, 20000, 1000))

test2 = foreach::foreach(i = iter(test, by="row"), .combine=c, 
                 .packages = c("sperrorest", "sp", "automap", "gstat")) %dopar%{
  sp_cv_UK = sperrorest::sperrorest(formula = fo, data = d, coords = c("X","Y"), 
                                    model_fun = vmf_fun, 
                                    pred_fun = UK_pred_fun,
                                    pred_args = list(formula = fo),
                                    smp_fun = partition_loo, 
                                    smp_args = list(buffer = i),
                                    mode_rep = "loop", 
                                    mode_fold = "loop")
    
  test_RMSE = sp_cv_UK$error_rep$test_rmse
  }

plot(test2~test[ ,1])

# Stop cluster
parallel::stopCluster(cluster)

# End time measurement
end_time = Sys.time()
print("bygone time")
print(end_time - start_time)
##
## End (explore the relationship between buffer distance and RMSE)
####
################################################################################
## End (test area)
################################################################################

