################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sperrorest")
p_load("ranger")
p_load("meteo")
p_load("sp")
p_load("sf")
p_load("gstat")       
p_load("nlme")
p_load("automap")
p_load("parallel")
p_load("doParallel")
p_load("foreach")
p_load("spmoran")

# Fewer decimal places, apply penalty on exponential notation 
options("scipen"= 999, "digits"=4)

# Load data and formulas
data_set = "WuS_SuB"
load("Data/WuS_SuB.rda")
d = WuS_SuB
fo_lm = fo_lm_WuS_SuB
fo_RF = fo_RF_WuS_SuB

# Formula for base RF-model
bRF_fo = as.formula(bcNitrate ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                  nfk + humus + cAckerland + log10_gwn + agrum_log10_restime + 
                  agrum_log10_gwn + agrum_log10_geschw + Ackerland + 
                  lbm_class_Gruenland + lbm_class_Unbewachsen + 
                  lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + 
                  aea20_1 + aea20_2 + aea20_12 + aea20_13 + X + Y)

# Formula for loo-OK-RF
loo_OK_RF_fo = as.formula(bcNitrate ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                  nfk + humus + cAckerland + log10_gwn + agrum_log10_restime + 
                  agrum_log10_gwn + agrum_log10_geschw + Ackerland + 
                  lbm_class_Gruenland + lbm_class_Unbewachsen + 
                  lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + X + Y + 
                  tc45 + tc315 + ok_inter_pred + ok_inter_var + 
                  aea20_1 + aea20_2 + aea20_12 + aea20_13)

# OK formula
ok_fo = as.formula(bcNitrate ~ 1)

# Observation column for RFSI and RF-oob-OK
obs_col = "bcNitrate"

# Weighting distance for OK-RF (based on the investigation of the spatial autocorrelation)
wd = 13504 # WuS_SuB

####
## Sample a training and test/prediction subset 
##

# Set the size of the subsets
n_train = 1000
n_pred = 1000
n_total = n_train + n_pred

# Set seed
set.seed(7)

# Draw a sample of size n-total from the data set
sample_ids = sample(2360, size = n_total)

d_sample = d[sample_ids, ]

# Get the train and prediction subsets
tt_ids = sperrorest::partition_cv(data = d_sample, coords = c("X", "Y"), 
                                  nfold = 2, repetition = 1, seed1 = 7)

d_train = d_sample[tt_ids[[1]][[1]]$train, ]
d_pred = d_sample[tt_ids[[1]][[1]]$test, ]
##
## End (sample a training and test/prediction subset)
####

####
## Data and model argument preparation RFSI
##
# CRS
c_r_s = "EPSG:25832"

# Names of the station ID (staid), longitude (X), latitude (Y) 
# and time columns in data
d.staid.x.y.z = c("ID_WuS_SuB","X","Y",NA)
##
## End (data and model argument preparation RFSI)
####

# Decide which models to run
MLR = TRUE
bRF = TRUE
RF = TRUE
RFSI = TRUE
RF_MEv = TRUE
loo_OK_RF = TRUE
RF_oob_OK = TRUE
OK_RF = TRUE
UK = TRUE
################################################################################
## End (preparation)
################################################################################


################################################################################
## MLR
################################################################################
if (MLR){
  # Start time measurement (train)
  start_time_train = Sys.time()
  
  # Create model
  lm_m = lm(fo_lm, d_train)
  
  # End time measurement (train)
  end_time_train = Sys.time()
  bygone_time_train = end_time_train - start_time_train
  
  # Start time measurement (predict)
  start_time_predict = Sys.time()
  
  # Prediction
  MLR_predi = predict(object = lm_m, newdata = d_pred)
  
  # End time measurement (predict)
  end_time_predict = Sys.time()
  bygone_time_predict = end_time_predict - start_time_predict
  
  # Set file name 
  file_name = paste("Results/", data_set, "_CT_MLR_", as.character(n_total),
                    ".rda", sep = "")
  # Save result 
  save(bygone_time_train, bygone_time_predict, file = file_name)
}
################################################################################
## End (MLR)
################################################################################


################################################################################
## bRF
################################################################################
if (bRF){
  # Start time measurement (train)
  start_time_train = Sys.time()
  
  # Train model
  bRF_model = ranger::ranger(formula = bRF_fo, 
                             data = d_train,
                             oob.error = FALSE,
                             seed = 7)
  
  # End time measurement (train)
  end_time_train = Sys.time()
  bygone_time_train = end_time_train - start_time_train
  
  # Start time measurement (predict)
  start_time_predict = Sys.time()
  
  # Prediction
  bRF_predi = predict(object = bRF_model, data = d_pred)
  
  # End time measurement (predict)
  end_time_predict = Sys.time()
  bygone_time_predict = end_time_predict - start_time_predict
  
  # Set file name 
  file_name = paste("Results/", data_set, "_CT_bRF_", as.character(n_total),
                    ".rda", sep = "")
  
  # Save result 
  save(bygone_time_train, bygone_time_predict, file = file_name)
}
################################################################################
## End (bRF)
################################################################################


################################################################################
## RF
################################################################################
if (RF){
  # Start time measurement (train)
  start_time_train = Sys.time()
  
  # Train model
  RF_model = ranger::ranger(formula = fo_RF, 
                             data = d_train,
                             oob.error = FALSE,
                             seed = 7)
  
  # End time measurement (train)
  end_time_train = Sys.time()
  bygone_time_train = end_time_train - start_time_train
  
  # Start time measurement (predict)
  start_time_predict = Sys.time()
  
  # Prediction
  RF_predi = predict(object = RF_model, data = d_pred)
  
  # End time measurement (predict)
  end_time_predict = Sys.time()
  bygone_time_predict = end_time_predict - start_time_predict
  
  # Set file name 
  file_name = paste("Results/", data_set, "_CT_RF_", as.character(n_total),
                    ".rda", sep = "")
  
  # Save result 
  save(bygone_time_train, bygone_time_predict, file = file_name)
}
################################################################################
## End (RF)
################################################################################


################################################################################
## RFSI
################################################################################
if (RFSI){
  # Start time measurement (train)
  start_time_train = Sys.time()
  
  # Create model
  RFSI_model = meteo::rfsi(formula = fo_RF,
                           data = d_train,
                           s.crs = c_r_s,
                           data.staid.x.y.z = d.staid.x.y.z,
                           n.obs = 5, # number of nearest observations
                           progress = FALSE,
                           # ranger parameters
                           seed = 7,
                           oob.error = FALSE,
                           num.trees = 500,
                           sample.fraction = 1,
                           min.node.size = 5,
                           min.bucket = 1,
                           max.depth = 0,
                           splitrule = "variance")
  
  # End time measurement (train)
  end_time_train = Sys.time()
  bygone_time_train = end_time_train - start_time_train
  
  # Start time measurement (predict)
  start_time_predict = Sys.time()
  
  # Prediction
  RFSI_prediction = meteo::pred.rfsi(model = RFSI_model,
                                     data = d_train,
                                     s.crs = c_r_s,
                                     data.staid.x.y.z = d.staid.x.y.z,
                                     obs.col = obs_col,
                                     newdata = d_pred,
                                     newdata.staid.x.y.z = d.staid.x.y.z,
                                     newdata.s.crs = c_r_s,
                                     progress = FALSE,
                                     soil3d=FALSE)
  
  # End time measurement (predict)
  end_time_predict = Sys.time()
  bygone_time_predict = end_time_predict - start_time_predict
  
  # Set file name 
  file_name = paste("Results/", data_set, "_CT_RFSI_", as.character(n_total),
                    ".rda", sep = "")
  # Save result 
  save(bygone_time_train, bygone_time_predict, file = file_name)
}
################################################################################
## End (RFSI)
################################################################################

################################################################################
## RF-MEv
################################################################################
if (RF_MEv){
  # Start time measurement (preprocessing)
  start_time_pp = Sys.time()
  
  # Create matrix of spatial point coordinates 
  coord_m = cbind(d_sample$X, d_sample$Y)
  # Calculate Moran eigenvectors and eigenvalues (MEvEv)
  MEvEv = spmoran::meigen(coords = coord_m, threshold = 0)
  # Store eigenvectors in a df and combine it with the data df
  Evec_df = as.data.frame(MEvEv$sf)
  c_data = cbind(d_sample, Evec_df)
  
  # Get first part of the formula 
  voi = all.vars(fo_RF)[1]
  covair = all.vars(fo_RF)[-1]
  covair_part = paste(covair, collapse = "+")
  fo_fp = paste(voi, covair_part, sep = "~")
  # Create second part of the model formula and complete the formula
  fo_sp = paste(colnames(Evec_df), collapse="+")
  fo_c = as.formula(paste(fo_fp, fo_sp, sep="+"))
  
  # Recreate the subsets, so that the subsets contain the eigenvector values 
  d_train = c_data[tt_ids[[1]][[1]]$train, ]
  d_pred = c_data[tt_ids[[1]][[1]]$test, ]
  
  # End time measurement (preprocessing)
  end_time_pp = Sys.time()
  bygone_time_pp = end_time_pp - start_time_pp
  
  # Start time measurement (train)
  start_time_train = Sys.time()
  
  # Train model
  RF_MEv_model = ranger::ranger(formula = fo_c, data = d_train, 
                                 oob.error = FALSE, seed = 7)
  
  # End time measurement (train)
  end_time_train = Sys.time()
  bygone_time_train = end_time_train - start_time_train
  
  # Start time measurement (predict)
  start_time_predict = Sys.time()
  
  # Prediction
  RF_MEv_predi = predict(object = RF_MEv_model, data = d_pred)
  
  # End time measurement (predict)
  end_time_predict = Sys.time()
  bygone_time_predict = end_time_predict - start_time_predict
  
  # Set file name 
  file_name = paste("Results/", data_set, "_CT_RF_MEv_", as.character(n_total),
                    ".rda", sep = "")
  # Save result 
  save(bygone_time_pp, bygone_time_train, bygone_time_predict, 
       file = file_name)
}
################################################################################
## End (RF-MEv)
################################################################################


################################################################################
## loo-OK-RF
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
                                                                   kappa = c(0.1,0.2),
                                                                   fix.values = c(0, NA, NA))
                              # Get the model
                              resid_vmm = resid_vm["var_model"]$var_model
                              
                              # Interpolation
                              if (nno != 0){
                              ok_pred = gstat::krige(ok_fo, train_sp_df, 
                                                     model = resid_vmm, 
                                                     newdata = test_sp_df,
                                                     debug.level = 0,
                                                     nmax = nno)
                              } else{
                                ok_pred = gstat::krige(ok_fo, train_sp_df, 
                                                       model = resid_vmm, 
                                                       newdata = test_sp_df,
                                                       debug.level = 0)
                              }
                              pred = c(ok_pred$var1.pred, ok_pred$var1.var)
                            }
}

if (loo_OK_RF){
  # Start time measurement (preprocessing)
  start_time_pp = Sys.time()
  
  # Setup backend to use many processors
  totalCores = parallel::detectCores()
  
  # Leave some cores to reduce computer load
  cluster = parallel::makeCluster(totalCores-2) 
  doParallel::registerDoParallel(cluster)
  
  # Execute loo-OK function
  loo_ok_res = loo_OK_fun(data = d_train, 
                          buffer_dist = 0, 
                          ok_fo = ok_fo,
                          nno = 0)
  
  # Stop cluster
  stopCluster(cluster)
  
  # Add the llo-OK results to the train df 
  loo_ok_res_vec = unlist(loo_ok_res)
  ok_inter_pred = loo_ok_res_vec[seq(1, length(loo_ok_res_vec), 2)]
  ok_inter_var = loo_ok_res_vec[seq(2, length(loo_ok_res_vec), 2)]
  d_train$ok_inter_pred = ok_inter_pred
  d_train$ok_inter_var = ok_inter_var
  
  ## Normal OK for the prediction data
  # Create spatial points dfs
  sp_df_train = sp::SpatialPointsDataFrame(d_train[,c("X","Y")], d_train)
  sp_df_pred = sp::SpatialPointsDataFrame(d_pred[,c("X","Y")], d_pred)
  
  # Fit variogram model
  resid_vm = automap::autofitVariogram(formula = ok_fo,
                                       input_data = sp_df_train,
                                       model = c("Mat", "Exp"),
                                       kappa = c(0.1,0.2),
                                       fix.values = c(0, NA, NA))
  # Get the variogram model
  resid_vmm = resid_vm["var_model"]$var_model
  
  # Ordinary Kriging interpolation
  ok_pred = gstat::krige(formula = ok_fo, sp_df_train, 
                         model = resid_vmm, 
                         newdata = sp_df_pred, 
                         debug.level = 0)
  
  # Add the OK results to the predcition df 
  d_train$ok_inter_pred = ok_pred$var1.pred 
  d_train$ok_inter_var = ok_pred$var1.var
  
  # End time measurement (preprocessing)
  end_time_pp = Sys.time()
  bygone_time_pp = end_time_pp - start_time_pp
  
  # Start time measurement (train)
  start_time_train = Sys.time()
  
  # Train model
  loo_OK_RF_model = ranger::ranger(formula = loo_OK_RF_fo, data = d_train, 
                                oob.error = FALSE, seed = 7)
  
  # End time measurement (train)
  end_time_train = Sys.time()
  bygone_time_train = end_time_train - start_time_train
  
  # Start time measurement (predict)
  start_time_predict = Sys.time()
  
  # Prediction
  loo_OK_RF_predi = predict(object = loo_OK_RF_model, data = d_pred)
  
  # End time measurement (predict)
  end_time_predict = Sys.time()
  bygone_time_predict = end_time_predict - start_time_predict
  
  # Set file name 
  file_name = paste("Results/", data_set, "_CT_loo_OK_RF_", as.character(n_total),
                    ".rda", sep = "")
  # Save result 
  save(bygone_time_pp, bygone_time_train, bygone_time_predict, file = file_name)
}
################################################################################
## End (loo-OK-RF)
################################################################################


################################################################################
## RF-oob-OK
################################################################################
if (RF_oob_OK){
  # Start time measurement (train)
  start_time_train = Sys.time()
  
  # Train model
  RF_model = ranger::ranger(formula = fo_RF, 
                             data = d_train,
                             oob.error = TRUE,
                             seed = 7)
  
  # End time measurement (train)
  end_time_train = Sys.time()
  bygone_time_train = end_time_train - start_time_train
  
  # Start time measurement (predict)
  start_time_predict = Sys.time()
  
  # RF Prediction
  RF_predi = predict(object = RF_model, data = d_pred)
  
  # Calculate the oob residuals and add them to the df
  d_train$oob_resi = d_train[, obs_col] - RF_model$predictions
  
  ## OK oob residuals interpolation
  # Create spatial points train df
  sp_df_train = sp::SpatialPointsDataFrame(d_train[,c("X","Y")], d_train)
  sp_df_pred = sp::SpatialPointsDataFrame(d_pred[,c("X","Y")], d_pred)
  
  # Fit variogram model
  resid_vm = automap::autofitVariogram(formula = oob_resi ~ 1,
                                       input_data = sp_df_train,
                                       model = c("Mat", "Exp"),
                                       kappa = c(0.1,0.2),
                                       fix.values = c(0, NA, NA))
  # Get the model
  resid_vmm = resid_vm["var_model"]$var_model
  
  # Ordinary Kriging oob residual interpolation
  ok_pred = gstat::krige(formula = oob_resi ~ 1, sp_df_train, 
                         model = resid_vmm, 
                         newdata = sp_df_pred, 
                         debug.level = 0)
  
  # Sum up the residual interpolation and the RF prediction
  final_prediction = RF_predi$predictions + ok_pred$var1.pred  
  
  # End time measurement (predict)
  end_time_predict = Sys.time()
  bygone_time_predict = end_time_predict - start_time_predict
  
  # Set file name 
  file_name = paste("Results/", data_set, "_CT_RF_oob_OK_", as.character(n_total),
                    ".rda", sep = "")
  # Save result 
  save(bygone_time_train, bygone_time_predict, file = file_name)
}
################################################################################
## End (RF-oob-OK)
################################################################################


################################################################################
## OK-RF
################################################################################
if (OK_RF){
  # Start time measurement (train)
  start_time_train = Sys.time()
  
  # Train RF model
  RF_model = ranger::ranger(formula = fo_RF, 
                            data = d_train,
                            oob.error = FALSE,
                            seed = 7)
  
  # Compute distance between the prediction data and the nearest training data point 
  dist = rep(NA, nrow(d_pred))
  for (i in 1:nrow(d_pred)){
    dist[i] = min(sqrt((d_train[, "X"] - d_pred[i, "X"])^2 
                       + (d_train[, "Y"] - d_pred[i, "Y"])^2))
  }
  
  # Create spatial points dfs
  sp_df_train = sp::SpatialPointsDataFrame(d_train[,c("X","Y")], d_train)
  sp_df_pred = sp::SpatialPointsDataFrame(d_pred[,c("X","Y")], d_pred)
  
  # Fit variogram model
  resid_vm = automap::autofitVariogram(formula = ok_fo,
                                       input_data = sp_df_train,
                                       model = c("Mat", "Exp"),
                                       kappa = c(0.1,0.2),
                                       fix.values = c(0, NA, NA))
  # Get the variogram model
  resid_vmm = resid_vm["var_model"]$var_model
  
  # End time measurement (train)
  end_time_train = Sys.time()
  bygone_time_train = end_time_train - start_time_train
  
  # Start time measurement (predict)
  start_time_predict = Sys.time()
  
  # Ordinary Kriging interpolation
  ok_pred = gstat::krige(formula = ok_fo, sp_df_train, 
                         model = resid_vmm, 
                         newdata = sp_df_pred, 
                         debug.level = 0)
  
  # RF Prediction
  RF_predi = predict(object = RF_model, data = d_pred)
  
  # Compute weights
  weights = dist/wd
  
  # Replace invalid weights
  weights[weights > 1] <- 1
  
  # Calculate OK-weights 
  temp_weights = weights-1
  OK_weights = temp_weights*-1
  
  # Weight predictions
  final_prediction = OK_weights * ok_pred$var1.pred + weights * RF_predi$predictions
  
  # End time measurement (predict)
  end_time_predict = Sys.time()
  bygone_time_predict = end_time_predict - start_time_predict
  
  # Set file name 
  file_name = paste("Results/", data_set, "_CT_OK_RF_", as.character(n_total),
                    ".rda", sep = "")
  
  # Save result 
  save(bygone_time_train, bygone_time_predict, file = file_name)
}
################################################################################
## End (OK-RF)
################################################################################


################################################################################
## UK
################################################################################
if (UK){
  # Start time measurement (predict)
  start_time_predict = Sys.time()
  
  # Create spatial points dfs
  sp_df_train = sp::SpatialPointsDataFrame(d_train[,c("X","Y")], d_train)
  sp_df_pred = sp::SpatialPointsDataFrame(d_pred[,c("X","Y")], d_pred)
  
  # OLS residual variogram model fitting
  resid_vmf = automap::autofitVariogram(formula = fo_lm, 
                                        input_data = sp_df_train,
                                        model = c("Mat", "Exp"),
                                        kappa = c(0.1, 0.2),
                                        fix.values = c(0, NA, NA))
  # Get the variogram model 
  resid_vmm = resid_vmf["var_model"]$var_model
  
  # UK prediction
  uk_pred = gstat::krige(formula = fo_lm, 
                         locations = sp_df_train,
                         model = resid_vmm,
                         newdata = sp_df_pred,
                         debug.level = 0)
  
  # End time measurement (predict)
  end_time_predict = Sys.time()
  bygone_time_predict = end_time_predict - start_time_predict
  
  # Set file name 
  file_name = paste("Results/", data_set, "_CT_UK_", as.character(n_total),
                    ".rda", sep = "")
  # Save result 
  save( bygone_time_predict, file = file_name)
}
################################################################################
## End (UK)
################################################################################
