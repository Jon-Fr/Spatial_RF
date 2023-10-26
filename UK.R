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

p_load("dplyr")

p_load("gstat")       
p_load("nlme")
p_load("automap")

# Additional functions that are not included in packages
source("auxiliary_functions.R", encoding = "UTF-8")
source("spdiagnostics-functions.R", encoding = "UTF-8") # Brenning 2022

# Fewer decimal places
options(digits=4)

# Load data and formula (for now use a subset)
load("data_points_subset.rda")
d = subset_dp

# Simplified formula for testing
fo = as.formula(bcNitrate ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                  cAckerland + log10_gwn + agrum_log10_restime + Ackerland + 
                  lbm_class_Gruenland + lbm_class_Unbewachsen + 
                  lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung)
################################################################################
## End (preparation)
################################################################################


################################################################################
## Universal Kriging (UK) prediction/interpolation 
################################################################################

#### 
## Preparation 
##

# Create a spatial points df 
sp_df = sp::SpatialPointsDataFrame(d[,c("x","y")], d)
##
## End (preparation )
####


##### 
## Preliminary analyses
## 

## Conduct multiple linear regression (MLR)
# Create MLR model
MLR_model = lm(fo, data=d)
summary(MLR_model)
# Some diagnostic plots
plot(residuals(MLR_model) ~ predict(MLR_model), cex.lab = 1.25,
     xlab = "predicted values", ylab = "residuals")
abline(h=0,lty="dashed")

# Directional semivariograms
dir_svgm = gstat::variogram(fo, data=sp_df, cutoff = 100000, 
                            alpha = c(0, 45, 90, 135)) 
#0:north, 45:north-east, 90:east, 135:south-east 
plot(dir_svgm, cex.lab = 1.25, xlab = "distance (m)")
##
## End (preliminary analyses)
####


####
## Create variogram model for the UK interpolation (the automatized way)
##

# OLS residual variogram model fitting
resid_vmf_au = automap::autofitVariogram(formula = fo, input_data = sp_df, 
                                         model = c("Sph"), cressie = TRUE)

# GLS residual variogram model fitting
resid_vmf_gls_au = automap::autofitVariogram(formula = fo,input_data = sp_df, 
                                             model = c("Sph"), 
                                             GLS.model = resid_vmf_au
                                             ["var_model"]$var_model, 
                                             cressie = TRUE)

# Obtain range and nugget-to-sill ratio
resid_vmf_gls_au = resid_vmf_gls_au["var_model"]$var_model
range = resid_vmf_gls_au[2,"range"]
range
n_t_s = resid_vmf_gls_au[1,"psill"] / sum(resid_vmf_gls_au[,"psill"])
n_t_s

# Plot result 
# Prepare the correlogram for the 
# generalized least square regression (GLS) model
autocor = nlme::corSpher(c(range, n_t_s), nugget=TRUE, fixed=TRUE)
autocor = nlme::Initialize(autocor, data = sp_df)
autocor

# Perform GLS regression
gls_model = nlme::gls(fo, correlation = autocor, 
                      data = sp_df)

# Residual Semivariogram of GLS regression residuals
sp_df$gls_model_resid = resid(gls_model)
resid_v_gls = gstat::variogram(gls_model_resid ~ 1, data = sp_df, cutoff=range, 
                               cressie=TRUE)

print(plot(resid_v_gls, pl = TRUE, model = resid_vmf_gls_au))
##
## End (create variogram model for the UK interpolation (the automatized way))
####
################################################################################
## End (Universal Kriging (UK) prediction/interpolation)
################################################################################


################################################################################
## Universal Kriging (UK) spatial leave one out cross validation 
################################################################################

#####
## Get the mean and median prediction distance 
## (for now use the test area as prediction area)
##

m_m_pd = mean_med_predDist(path_predArea = "test_area.gpkg", dataPoints_df = d,
                           c_r_s = "EPSG:25832")
##
## End (Get the mean and median prediction distance)
####


####
## Cross validation
## 

# Create model function 
vmf_fun = function(formula, data){
  # Create a spatial points df 
  sp_df = sp::SpatialPointsDataFrame(coords = data[,c("x","y")], data = data)
  # OLS residual variogram model fitting
  resid_vmf = automap::autofitVariogram(formula = formula, input_data = sp_df,
                                        model = c("Sph"), cressie = TRUE)
  # GLS residual variogram model fitting
  resid_vmf_gls = automap::autofitVariogram(formula = formula,
                                            input_data = sp_df, 
                                            model = c("Sph"),
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
  sp_df_newdata = sp::SpatialPointsDataFrame(newdata[,c("x","y")], newdata)
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
#future::plan(future.callr::callr, workers = 5)
sp_cv_UK = sperrorest::sperrorest(formula = fo, data = d, coords = c("x","y"), 
                                  model_fun = vmf_fun, 
                                  pred_fun = UK_pred_fun,
                                  pred_args = list(formula = fo),
                                  smp_fun = partition_loo, 
                                  smp_args = list(buffer = m_m_pd$med_predDist))

# Get test RMSE
test_RMSE = sp_cv_UK$error_rep$test_rmse
test_RMSE
##
## End (cross validation)
####
################################################################################
## End (Universal Kriging (UK) spatial leave one out cross validation)
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
  sp_df = sp::SpatialPointsDataFrame(coords = data[,c("x","y")], data = data)
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
  sp_df_newdata = sp::SpatialPointsDataFrame(newdata[,c("x","y")], newdata)
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
                 repetition = 200, 
                 seed1 = 123)

# Perform spatial leave one out cross validation with random buffer distance 
# between 0 and maxdist
# Future for parallelization
#future::plan(future.callr::callr, workers = 5)
SPEP_UK = sperrorest(formula = fo, data = d, coords = c("x","y"), 
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
## Plot SPEP (based on false positives and false negatives) 
##

obspreddist <- function(x, type = "test", decision_threshold) {
  if (is.atomic(x)) {
    return(list(obs = logical(), pred = logical(), dist = numeric()))
  } else if (is.null(names(x[[type]]))) {
    return(list(obs = logical(), pred = logical(), dist = numeric()))
  } else {
    return(list(obs = x[[type]]$obs > decision_threshold, 
                pred = x[[type]]$pred > decision_threshold, 
                numobs = x[[type]]$obs,
                numpred = x[[type]]$pred,
                numerr = x[[type]]$bias,
                dist = x$distance))
  }
}

obspreddists <- function(x, ...) x %>% map(obspreddist, ...) %>% dplyr::bind_rows() %>% as.data.frame()

extract_obspreds <- function(res, type = "test", decision_threshold = threshold) {
  RES <- res$error_fold %>% map(obspreddists, type = type, decision_threshold = decision_threshold) %>% dplyr::bind_rows()
  RES$fp <- RES$pred & !RES$obs
  RES$fn <- RES$obs & !RES$pred
  RES$acc <- RES$obs == RES$pred
  RES$err <- RES$pred - RES$obs
  RES
}

# Spatial leave-one-out RMSE:
ed <- lapply(res, extract_obspreds)

# Compute smoothed prediction error profiles:
brk <- seq(300, 0.95*MAXDIST, length = 30)
errs <- lapply(ed, function(x) smth(means0(x, breaks = brk, 
                                           which = c("acc", "err", "fp", "fn"))))

nm <- "1"
# hist(ed[[nm]]$numerr, br = 200, main = nm)
# qqnorm((ed[[nm]]$numerr - mean(ed[[nm]]$numerr)) / sd(ed[[nm]]$numerr)); abline(c(0,1))
plot(errs[[nm]]$dist, errs[[nm]]$smth_err, type = "l", xlab = "distance", 
     ylab = paste(nm, "error"), main = nm); abline(h = 0)
##
## End (plot SPEP (based on false positives and false negatives))
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

# Setup backend to use many processors
totalCores = parallel::detectCores()

# Leave two cores to reduce computer load
cluster = parallel::makeCluster(totalCores[1]-2) 
doParallel::registerDoParallel(cluster)

# explore
test = data.frame(seq(0, 20000, 1000))

test2 = foreach (i = iter(test, by="row"), .combine=c, 
                 .packages = c("sperrorest", "sp", "automap", "gstat")) %dopar%{
  sp_cv_UK = sperrorest::sperrorest(formula = fo, data = d, coords = c("x","y"), 
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
stopCluster(cluster)

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

