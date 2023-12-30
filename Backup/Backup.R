################################################################################
## Spatial prediction error profile (SPEP) and 
# spatial variable importance profile (SVIP) (Brenning 2022) (loo_OK_RF)
################################################################################
#####
## Get the mean and median prediction distance 
## (for now use the test area as prediction area)
##

m_m_pd = mean_med_predDist(path_predArea = "test_area.gpkg", dataPoints_df = d,
                           c_r_s = "EPSG:25832")
##
## End (get the mean and median prediction distance)
####

####
## llo-OK function
##
llo_OK_fun = function(data, buffer_dist, ok_fo){
  # Leave one out resampling
  resamp = sperrorest::partition_loo(data = data, ndisc = nrow(data), 
                                     replace = FALSE, coords = c("x","y"), 
                                     buffer = buffer_dist, repetition = 1)
  # Create a spatial points df 
  sp_df = sp::SpatialPointsDataFrame(coords = data[,c("x","y")], data = data)
  # Create vectors for storing the results
  res_pred = c()
  res_var = c()
  # For loop
  for (i in 1:nrow(sp_df)){
    # Get training and test ids
    id_train = resamp[["1"]][[i]]$train
    id_test = resamp[["1"]][[i]]$test
    
    # Get a training and test spatial points df 
    train_sp_df = sp_df[id_train, ]
    test_sp_df = sp_df[id_test, ]
    
    # Fit spherical variogram model
    resid_vm = automap::autofitVariogram(formula = ok_fo, 
                                         input_data = train_sp_df, 
                                         model = c("Sph"),
                                         cressie = TRUE)
    # Get the model
    resid_vmm = resid_vm["var_model"]$var_model
    
    # Interpolation
    ok_pred = gstat::krige(ok_fo, train_sp_df, model = resid_vmm, 
                           newdata = test_sp_df, debug.level = 0)
    pred = c(ok_pred$var1.pred, ok_pred$var1.var)
    # Add results to result vectors
    res_pred = append(res_pred, ok_pred$var1.pred)
    res_var = append(res_var, ok_pred$var1.var)
  }
  # Add results to df 
  data$ok_inter_pred = res_pred
  data$ok_inter_var = res_var
  return(data)
}

# Test llo-OK function
#llo_ok_res = llo_OK_fun(data = d, 
#                        buffer_dist = 1000, 
#                        ok_fo = ok_fo) 
##
## End (llo-OK function)
####


####
## Cross validation
## 

# Create model function 
RF_fun = function(formula, data, buffer_dist, ok_fo){
  # loo OK
  data = llo_OK_fun(data = data, buffer_dist = buffer_dist, ok_fo = ok_fo)
  # Create RF model
  RF_model = ranger::ranger(formula = formula, 
                            data = data)
  # Return training data and RF_model
  r_l = list("model" = RF_model, "train_data" = data)
  return(r_l)
}

# Create prediction function
RF_pred_fun = function(object, newdata, ok_fo){
  # Perform Ordinary kriging interpolation for the newdata point
  # Create spdfs
  train_data = object$train_data
  train_sp_df = sp::SpatialPointsDataFrame(coords = train_data[,c("x","y")], 
                                           data = train_data)
  sp_df = sp::SpatialPointsDataFrame(coords = newdata[,c("x","y")], 
                                     data = newdata)
  # Fit spherical variogram model
  resid_vm = automap::autofitVariogram(formula = ok_fo, 
                                       input_data = train_sp_df, 
                                       model = c("Sph"),
                                       cressie = TRUE)
  # Get the model
  resid_vmm = resid_vm["var_model"]$var_model
  # Interpolation
  ok_pred = gstat::krige(ok_fo, train_sp_df, 
                         model = resid_vmm, newdata = sp_df, debug.level = 0)
  # Add the prediction and the variance to newdata
  newdata$ok_inter_pred = ok_pred$var1.pred
  newdata$ok_inter_var = ok_pred$var1.var  
  #  Perform RF prediction
  RF_prediction = predict(object = object$model,
                          data = newdata)
  return(RF_prediction$predictions)
}
################################################################################
## End (SPEP and SVIP) (loo_OK_RF)
################################################################################


################################################################################
## Spatial prediction error profile (SPEP) and 
# spatial variable importance profile (SVIP) (Brenning 2022) (UK)
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
                                        model = c("Sph"))
  # GLS residual variogram model fitting
  resid_vmf_gls = automap::autofitVariogram(formula = formula,
                                            input_data = sp_df, 
                                            model = c("Sph"),
                                            GLS.model = resid_vmf["var_model"]
                                            $var_model)
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
## End (SPEP and SVIP) (UK)
################################################################################