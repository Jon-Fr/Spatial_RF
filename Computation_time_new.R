################################################################################
## Preparation (strictly necessary)
################################################################################
# Decide which models to run
MLR = TRUE
bRF = TRUE
RF_K = TRUE
RF_GLS  = TRUE
RFSI = TRUE
RF_MEv = TRUE
loo_OK_RF = TRUE
RF_oob_OK = TRUE
OK_RF = TRUE
UK = TRUE


# Set the size of the training and test data set
n_train = 2000
n_pred = n_train * 25

# Load necessary packages
library("pacman")
p_load("sperrorest")
p_load("ranger")
p_load("meteo")
p_load("sp")
p_load("sf")
p_load("gstat")       
p_load("automap")
p_load("parallel")
p_load("doParallel")
p_load("foreach")
p_load("spmoran")
p_load("RandomForestsGLS")

# Fewer decimal places, apply penalty on exponential notation 
options("scipen"= 999, "digits"=4)

# Load data and formulas
data_set = "WuS_SuB"
load("Data/WuS_SuB.rda")
pred_d = readRDS("Data/WuS_SuB_pred.rds")
pred_d$ID_WuS_SuB = 1:nrow(pred_d)
d = WuS_SuB
fo_lm = fo_lm_WuS_SuB
fo_RF = fo_RF_WuS_SuB

# Formula for base RF-model
bRF_fo = as.formula(subMittelwert  ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                      nfk + humus + cAckerland + log10_gwn + agrum_log10_restime + 
                      agrum_log10_gwn + agrum_log10_geschw + Ackerland + 
                      lbm_class_Gruenland + lbm_class_Unbewachsen + 
                      lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + 
                      aea20_1 + aea20_2 + aea20_12 + aea20_13 + X + Y)

# Formula for loo-OK-RF
loo_OK_RF_fo = as.formula(subMittelwert ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                            nfk + humus + cAckerland + log10_gwn + agrum_log10_restime + 
                            agrum_log10_gwn + agrum_log10_geschw + Ackerland + 
                            lbm_class_Gruenland + lbm_class_Unbewachsen + 
                            lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + X + Y + 
                            tc45 + tc315 + ok_inter_pred + ok_inter_var + 
                            aea20_1 + aea20_2 + aea20_12 + aea20_13)

# OK formula
ok_fo = as.formula(subMittelwert  ~ 1)

# Explanatory variables and number of explanatory variables for RF-GLS
ex_v_columns = c("crestime", "cgwn", "cgeschw", "log10carea", "elevation",
                 "nfk", "humus", "cAckerland", "log10_gwn", 
                 "agrum_log10_restime", "agrum_log10_gwn", "agrum_log10_geschw",
                 "Ackerland", "lbm_class_Gruenland", "lbm_class_Unbewachsen", 
                 "lbm_class_FeuchtgebieteWasser", "lbm_class_Siedlung", "X", 
                 "Y", "tc45", "tc315", "aea20_1", "aea20_2", "aea20_12", 
                 "aea20_13")
num_of_ex_v = length(ex_v_columns )

# Observation column for RFSI, RF-oob-OK and RF-GLS
obs_col = "subMittelwert"

# CRS for RFSI
c_r_s = "EPSG:25832"

# Names of the station ID (staid), longitude (X), latitude (Y)
# and time columns in data for RFSI
d.staid.x.y.z = c("ID_WuS_SuB","X","Y",NA)

# Weighting distance for OK-RF (based on the investigation of the spatial autocorrelation)
wd = 26976 # WuS_SuB

####
## Sample a training and test/prediction subset 
##

# Set seed
set.seed(77)

## Draw a sample of size n-training from the WuS_SuB data set
# Get row ids
train_sample_ids = sample(2360, size = n_train)
# Get data
d_train  = d[train_sample_ids, ]

## Draw a sample of size n-prediction from the WuS_SuB prediction data set
# Get row ids
pred_sample_ids = sample(50680, size = n_pred)
# Get data
d_pred  = pred_d[pred_sample_ids, ]

##
## End (sample a training and test/prediction subset)
####

# Create spatial points dfs for UK and OK-RF
sp_df_train = sp::SpatialPointsDataFrame(d_train[,c("X","Y")], d_train)
sp_df_pred = sp::SpatialPointsDataFrame(d_pred[,c("X","Y")], d_pred)

# Coordinates and explanatory variable matrices as well as observation values for RF-GLS
c_matrix_train = as.matrix(d_train[, c("X", "Y")])
c_matrix_pred = as.matrix(d_pred[, c("X", "Y")])
ex_v_matrix_train = data.matrix(d_train[, ex_v_columns])
ex_v_matrix_pred = data.matrix(d_pred[, ex_v_columns])
observations = d_train[, obs_col]

################################################################################
## End (preparation)
################################################################################

################################################################################
## RF-GLS
################################################################################
if (RF_GLS){
  # Set number of repetitions for train
  if (n_train == 500){
    nrep = 20
  } else if (n_train == 1000){
    nrep = 2
  } else {
    nrep = 1
  }
  # Set seed
  set.seed(77)
  
  # Start time measurement (train)
  start_time_train = Sys.time()
  
  for (i in 1:nrep){
    # Train model
    RF_GLS_M = RandomForestsGLS::RFGLS_estimate_spatial(
      coords = c_matrix_train,
      X = ex_v_matrix_train,
      y = observations,
      param_estimate = TRUE,
      cov.model = "exponential",
      mtry = floor(sqrt(num_of_ex_v)),
      h = 20)
    
    # End time measurement (train)
    end_time_train = Sys.time()
    bygone_time_train = end_time_train - start_time_train
    
    # Start time measurement (predict)
    start_time_predict = Sys.time()
  }
  
  for (i in 1:1000){
    # Prediction
    RF_GLS_prediction = RandomForestsGLS::RFGLS_predict_spatial(
      RFGLS_out = RF_GLS_M,
      Xtest = ex_v_matrix_pred,
      coords.0 = c_matrix_pred)
  }
  
  # End time measurement (predict)
  end_time_predict = Sys.time()
  bygone_time_predict = end_time_predict - start_time_predict
  
  # Set file name 
  file_name = paste("Results/", data_set, "_CT_RF_GLS_", as.character(n_train),
                    ".rda", sep = "")
  # Save result 
  save(bygone_time_train, bygone_time_predict, file = file_name)
}
################################################################################
## End (RF-GLS)
################################################################################


################################################################################
## MLR
################################################################################
if (MLR){
  # Start time measurement (train)
  start_time_train = Sys.time()
  
  for (i in 1:1000){
    # Create model
    lm_m = lm(fo_lm, d_train)
  }
  
  # End time measurement (train)
  end_time_train = Sys.time()
  bygone_time_train = end_time_train - start_time_train
  
  # Start time measurement (predict)
  start_time_predict = Sys.time()
  
  for (i in 1:1000){
    # Prediction
    MLR_predi = predict(object = lm_m, newdata = d_pred)
  }
  
  # End time measurement (predict)
  end_time_predict = Sys.time()
  bygone_time_predict = end_time_predict - start_time_predict
  
  # Set file name 
  file_name = paste("Results/", data_set, "_CT_MLR_", as.character(n_train),
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
  
  for (i in 1:1000){
    # Train model
    bRF_model = ranger::ranger(formula = bRF_fo, 
                               data = d_train,
                               oob.error = FALSE,
                               seed = 7)
  }
  
  # End time measurement (train)
  end_time_train = Sys.time()
  bygone_time_train = end_time_train - start_time_train
  
  # Start time measurement (predict)
  start_time_predict = Sys.time()
  
  for (i in 1:1000){
    # Prediction
    bRF_predi = predict(object = bRF_model, data = d_pred)
  }
  
  # End time measurement (predict)
  end_time_predict = Sys.time()
  bygone_time_predict = end_time_predict - start_time_predict
  
  # Set file name 
  file_name = paste("Results/", data_set, "_CT_bRF_", as.character(n_train),
                    ".rda", sep = "")
  
  # Save result 
  save(bygone_time_train, bygone_time_predict, file = file_name)
}
################################################################################
## End (bRF)
################################################################################


################################################################################
## RF-K
################################################################################
if (RF_K){
  # Start time measurement (train)
  start_time_train = Sys.time()
  
  for (i in 1:1000){
    # Train model
    RF_model = ranger::ranger(formula = fo_RF, 
                              data = d_train,
                              oob.error = FALSE,
                              seed = 7)
  }
  
  # End time measurement (train)
  end_time_train = Sys.time()
  bygone_time_train = end_time_train - start_time_train
  
  # Start time measurement (predict)
  start_time_predict = Sys.time()
  
  for (i in 1:1000){
    # Prediction
    RF_predi = predict(object = RF_model, data = d_pred)
  }
  
  # End time measurement (predict)
  end_time_predict = Sys.time()
  bygone_time_predict = end_time_predict - start_time_predict
  
  # Set file name 
  file_name = paste("Results/", data_set, "_CT_RF_", as.character(n_train),
                    ".rda", sep = "")
  
  # Save result 
  save(bygone_time_train, bygone_time_predict, file = file_name)
}
################################################################################
## End (RF-K)
################################################################################


################################################################################
## RFSI
################################################################################
if (RFSI){
  # Start time measurement (train)
  start_time_train = Sys.time()
  
  for (i in 1:1000){
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
  }
  
  # End time measurement (train)
  end_time_train = Sys.time()
  bygone_time_train = end_time_train - start_time_train
  
  # Start time measurement (predict)
  start_time_predict = Sys.time()
  
  for (i in 1:1000){
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
  }
  
  # End time measurement (predict)
  end_time_predict = Sys.time()
  bygone_time_predict = end_time_predict - start_time_predict
  
  # Set file name 
  file_name = paste("Results/", data_set, "_CT_RFSI_", as.character(n_train),
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
  # Set number of repetitions for preprocessing
  if (n_train == 500){
    nrep = 1000
  } else if (n_train == 1000){
    nrep = 500
  } else {
    nrep = 100
  }
  
  # Start time measurement (preprocessing)
  start_time_pp = Sys.time()
  
  for (i in 1:nrep){
    ## Eigenvectors for the training data
    # Create matrix of spatial point coordinates 
    coord_m = cbind(d_train$X, d_train$Y)
    # Calculate Moran eigenvectors and eigenvalues (MEvEv)
    MEvEv = spmoran::meigen(coords = coord_m, threshold = 0.25)
    # Store eigenvectors in a df and combine it with the training data df
    Evec_df = as.data.frame(MEvEv$sf)
    c_train = cbind(d_train, Evec_df)
    
    ## Eigenvectors for the prediction data
    # Create matrix of spatial point coordinates 
    coord_m = cbind(d_pred$X, d_pred$Y)
    # Calculate Moran eigenvectors and eigenvalues
    MEvEv_n = spmoran::meigen0(meig = MEvEv, coords0 = coord_m)
    # Store eigenvectors in a df and combine it with the pred data df
    Evec_ndf = as.data.frame(MEvEv_n$sf)
    c_pred = cbind(d_pred, Evec_ndf)
    
    # Get first part of the formula 
    voi = all.vars(fo_RF)[1]
    covair = all.vars(fo_RF)[-1]
    covair_part = paste(covair, collapse = "+")
    fo_fp = paste(voi, covair_part, sep = "~")
    # Create second part of the model formula and complete the formula
    fo_sp = paste(colnames(Evec_df), collapse="+")
    fo_c = as.formula(paste(fo_fp, fo_sp, sep="+"))
  }
  
  # End time measurement (preprocessing)
  end_time_pp = Sys.time()
  bygone_time_pp = end_time_pp - start_time_pp
  
  # Start time measurement (train)
  start_time_train = Sys.time()
  
  for (i in 1:1000){
    # Train model
    RF_MEv_model = ranger::ranger(formula = fo_c, data = c_train, 
                                  oob.error = FALSE, seed = 7)
  }
  
  # End time measurement (train)
  end_time_train = Sys.time()
  bygone_time_train = end_time_train - start_time_train
  
  # Start time measurement (predict)
  start_time_predict = Sys.time()
  
  for (i in 1:1000){
    # Prediction
    RF_MEv_predi = predict(object = RF_MEv_model, data = c_pred)
  }
  
  # End time measurement (predict)
  end_time_predict = Sys.time()
  bygone_time_predict = end_time_predict - start_time_predict
  
  # Set file name 
  file_name = paste("Results/", data_set, "_CT_RF_MEv_", as.character(n_train),
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
                                                                   kappa = c(seq(0.1, 0.4, 0.1)),
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

# Create a copy of the train and pred df
d_train_loo = d_train
d_pred_loo = d_pred

if (loo_OK_RF){
  # Set number of repetitions for preprocessing
  if (n_train == 500){
    nrep = 100
  } else if (n_train == 1000){
    nrep = 20
  } else {
    nrep = 10
  }
  
  # Start time measurement (preprocessing)
  start_time_pp = Sys.time()
  
  for (i in 1:nrep){
    # Setup backend to use many processors
    totalCores = parallel::detectCores()
    
    # Leave some cores to reduce computer load
    cluster = parallel::makeCluster(totalCores) 
    doParallel::registerDoParallel(cluster)
    
    # Execute loo-OK function
    loo_ok_res = loo_OK_fun(data = d_train_loo, 
                            buffer_dist = 0, 
                            ok_fo = ok_fo,
                            nno = 0)
    
    # Stop cluster
    stopCluster(cluster)
    
    # Add the loo-OK results to the train df 
    loo_ok_res_vec = unlist(loo_ok_res)
    ok_inter_pred = loo_ok_res_vec[seq(1, length(loo_ok_res_vec), 2)]
    ok_inter_var = loo_ok_res_vec[seq(2, length(loo_ok_res_vec), 2)]
    d_train_loo$ok_inter_pred = ok_inter_pred
    d_train_loo$ok_inter_var = ok_inter_var
    
    ## Normal OK for the prediction data
    # Create spatial points dfs
    sp_df_train_loo = sp::SpatialPointsDataFrame(d_train_loo[,c("X","Y")], d_train_loo)
    sp_df_pred_loo = sp::SpatialPointsDataFrame(d_pred_loo[,c("X","Y")], d_pred_loo)
    
    # Fit variogram model
    resid_vm = automap::autofitVariogram(formula = ok_fo,
                                         input_data = sp_df_train_loo,
                                         model = c("Mat", "Exp"),
                                         kappa = c(seq(0.1, 0.4, 0.1)),
                                         fix.values = c(0, NA, NA))
    # Get the variogram model
    resid_vmm = resid_vm["var_model"]$var_model
    
    # Ordinary Kriging interpolation
    ok_pred = gstat::krige(formula = ok_fo, sp_df_train_loo, 
                           model = resid_vmm, 
                           newdata = sp_df_pred_loo, 
                           debug.level = 0)
    
    # Add the OK results to the predcition df 
    d_pred_loo$ok_inter_pred = ok_pred$var1.pred 
    d_pred_loo$ok_inter_var = ok_pred$var1.var
  }
  
  # End time measurement (preprocessing)
  end_time_pp = Sys.time()
  bygone_time_pp = end_time_pp - start_time_pp
  
  # Start time measurement (train)
  start_time_train = Sys.time()
  
  for (i in 1:1000){
    # Train model
    loo_OK_RF_model = ranger::ranger(formula = loo_OK_RF_fo, data = d_train_loo, 
                                     oob.error = FALSE, seed = 7)
  }
  
  # End time measurement (train)
  end_time_train = Sys.time()
  bygone_time_train = end_time_train - start_time_train
  
  # Start time measurement (predict)
  start_time_predict = Sys.time()
  
  for (i in 1:1000){
    # Prediction
    loo_OK_RF_predi = predict(object = loo_OK_RF_model, data = d_pred_loo)
  }
  
  # End time measurement (predict)
  end_time_predict = Sys.time()
  bygone_time_predict = end_time_predict - start_time_predict
  
  # Set file name 
  file_name = paste("Results/", data_set, "_CT_loo_OK_RF_", as.character(n_train),
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
  
  # Set number of repetitions for prediction
  if (n_train == 500){
    nrep = 100
  } else if (n_train == 1000){
    nrep = 50
  } else {
    nrep = 10
  }
  
  # Create a copy of the train df
  d_train_oob = d_train
  
  # Start time measurement (train)
  start_time_train = Sys.time()
  
  for (i in 1:1000){
    # Train model
    RF_model = ranger::ranger(formula = fo_RF, 
                              data = d_train_oob,
                              oob.error = TRUE,
                              seed = 7)
  }
  
  # End time measurement (train)
  end_time_train = Sys.time()
  bygone_time_train = end_time_train - start_time_train
  
  # Start time measurement (predict)
  start_time_predict = Sys.time()
  
  for (i in 1:nrep){
    # RF Prediction
    RF_predi = predict(object = RF_model, data = d_pred)
    
    # Calculate the oob residuals and add them to the df
    d_train_oob$oob_resi = d_train_oob[, obs_col] - RF_model$predictions
    
    ## OK oob residuals interpolation
    # Create spatial points train df
    sp_df_train = sp::SpatialPointsDataFrame(d_train_oob[,c("X","Y")], d_train_oob)
    sp_df_pred = sp::SpatialPointsDataFrame(d_pred[,c("X","Y")], d_pred)
    
    # Fit variogram model
    resid_vm = automap::autofitVariogram(formula = oob_resi ~ 1,
                                         input_data = sp_df_train,
                                         model = c("Mat", "Exp"),
                                         kappa = c(seq(0.1, 0.4, 0.1)),
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
  }
  
  # End time measurement (predict)
  end_time_predict = Sys.time()
  bygone_time_predict = end_time_predict - start_time_predict
  
  # Set file name 
  file_name = paste("Results/", data_set, "_CT_RF_oob_OK_", as.character(n_train),
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
  
  # Set number of repetitions for prediction
  if (n_train == 500){
    nrep = 100
  } else if (n_train == 1000){
    nrep = 50
  } else {
    nrep = 10
  }
  
  # Start time measurement (train)
  start_time_train = Sys.time()
  
  for (i in 1:1000){
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
    
    # Fit variogram model
    resid_vm = automap::autofitVariogram(formula = ok_fo,
                                         input_data = sp_df_train,
                                         model = c("Mat", "Exp"),
                                         kappa = c(seq(0.1, 0.4, 0.1)),
                                         fix.values = c(0, NA, NA))
    # Get the variogram model
    resid_vmm = resid_vm["var_model"]$var_model
  }
  
  # End time measurement (train)
  end_time_train = Sys.time()
  bygone_time_train = end_time_train - start_time_train
  
  # Start time measurement (predict)
  start_time_predict = Sys.time()
  
  for (i in 1:nrep){
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
  }
  
  # End time measurement (predict)
  end_time_predict = Sys.time()
  bygone_time_predict = end_time_predict - start_time_predict
  
  # Set file name 
  file_name = paste("Results/", data_set, "_CT_OK_RF_", as.character(n_train),
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
  # Set number of repetitions for prediction
  if (n_train == 500){
    nrep = 100
  } else if (n_train == 1000){
    nrep = 50
  } else {
    nrep = 10
  }
  
  # Start time measurement (train)
  start_time_train = Sys.time()
  
  for (i in 1:1000){
    # OLS residual variogram model fitting
    resid_vmf = automap::autofitVariogram(formula = fo_lm, 
                                          input_data = sp_df_train,
                                          model = c("Mat", "Exp"),
                                          kappa = c(seq(0.1, 0.4, 0.1)),
                                          fix.values = c(0, NA, NA))
    # Get the variogram model 
    resid_vmm = resid_vmf["var_model"]$var_model
  }
  
  # End time measurement (train)
  end_time_train = Sys.time()
  bygone_time_train = end_time_train - start_time_train
  
  
  # Start time measurement (predict)
  start_time_predict = Sys.time()
  
  for (i in 1:nrep){
    # UK prediction
    uk_pred = gstat::krige(formula = fo_lm, 
                           locations = sp_df_train,
                           model = resid_vmm,
                           newdata = sp_df_pred,
                           debug.level = 0)
  }
  
  # End time measurement (predict)
  end_time_predict = Sys.time()
  bygone_time_predict = end_time_predict - start_time_predict
  
  # Set file name 
  file_name = paste("Results/", data_set, "_CT_UK_", as.character(n_train),
                    ".rda", sep = "")
  # Save result 
  save(bygone_time_train, bygone_time_predict, file = file_name)
}
################################################################################
## End (UK)
################################################################################


################################################################################
## Post processing of the results
################################################################################

# Create vectors for the results
loo_OK_RF_pre_pro = c()
RF_MEv_pre_pro = c()
RF_pre_pro = c(0, 0, 0)
bRF_pre_pro = c(0, 0, 0)
RF_GLS_pre_pro = c(0, 0, 0)
RF_oob_OK_pre_pro = c(0, 0, 0)
MLR_pre_pro = c(0, 0, 0)
OK_RF_pre_pro = c(0, 0, 0)
RFSI_pre_pro = c(0, 0, 0)
UK_pre_pro = c(0, 0, 0)


RF_train = c()
bRF_train = c()
loo_OK_RF_train = c()
RF_MEv_train = c()
RF_GLS_train = c()
RF_oob_OK_train = c()
MLR_train = c()
OK_RF_train = c()
RFSI_train = c()
UK_train = c()

RF_predi = c()
bRF_predi = c()
loo_OK_RF_predi = c()
RF_MEv_predi = c()
RF_GLS_predi = c()
RF_oob_OK_predi = c()
MLR_predi = c()
OK_RF_predi = c() 
RFSI_predi = c()
UK_predi = c()

# Vector of the first part of the file names
f_names_vec1 = c("WuS_SuB_CT_bRF", "WuS_SuB_CT_UK", "WuS_SuB_CT_MLR", 
                 "WuS_SuB_CT_RF", "WuS_SuB_CT_RF_GLS", "WuS_SuB_CT_OK_RF", 
                 "WuS_SuB_CT_RF_oob_OK", "WuS_SuB_CT_RFSI", "WuS_SuB_CT_RF_MEv",
                 "WuS_SuB_CT_loo_OK_RF")

# Vector of the second part of the file names
f_names_vec2 = c("_500.rda", "_1000.rda", "_2000.rda") 

# for loops
for (i1 in f_names_vec1){
  for (i2 in f_names_vec2){
    # Complete file name
    f_name = paste("Results/CT/", i1, i2, sep = "")
    # Load file
    load(f_name)
    # bRF
    if (i1 == "WuS_SuB_CT_bRF"){
      bRF_train = append(bRF_train/1000, bygone_time_train)
      bRF_predi = append(bRF_predi/1000, bygone_time_predict)
      # RF
    } else if (i1 == "WuS_SuB_CT_RF"){
      RF_train = append(RF_train/1000, bygone_time_train)
      RF_predi = append(RF_predi/1000, bygone_time_predict)
      # MLR
    } else if (i1 == "WuS_SuB_CT_MLR"){
      MLR_train = append(MLR_train/1000, bygone_time_train)
      MLR_predi = append(MLR_predi/1000, bygone_time_predict)
      # RF_oob_OK
    } else if (i1 == "WuS_SuB_CT_RF_oob_OK"){
      if (i2 == "_500.rda"){
        divi = 100
      } else if (i2 == "_1000.rda"){
        divi = 50
      } else {
        divi = 10
      }
      RF_oob_OK_train = append(RF_oob_OK_train/1000, bygone_time_train)
      RF_oob_OK_predi = append(RF_oob_OK_predi/divi, bygone_time_predict)
      # OK_RF
    } else if (i1 == "WuS_SuB_CT_OK_RF"){
      if (i2 == "_500.rda"){
        divi = 100
      } else if (i2 == "_1000.rda"){
        divi = 50
      } else {
        divi = 10
      }
      OK_RF_train = append(OK_RF_train/1000, bygone_time_train)
      OK_RF_predi = append(OK_RF_predi/divi, bygone_time_predict)
      # RF_GLS
    } else if (i1 == "WuS_SuB_CT_RF_GLS"){
      if (i2 == "_500.rda"){
        divi = 20
      } else if (i2 == "_1000.rda"){
        divi = 2
      } else {
        divi = 1
      }
      RF_GLS_train = append(RF_GLS_train/divi, bygone_time_train)
      RF_GLS_predi = append(RF_GLS_predi/1000, bygone_time_predict)
      # RFSI
    } else if (i1 == "WuS_SuB_CT_RFSI"){
      RFSI_train = append(RFSI_train/1000, bygone_time_train)
      RFSI_predi = append(RFSI_predi/1000, bygone_time_predict)
      # UK
    } else if (i1 == "WuS_SuB_CT_UK"){
      if (i2 == "_500.rda"){
        divi = 100
      } else if (i2 == "_1000.rda"){
        divi = 50
      } else {
        divi = 10
      }
      UK_train = append(UK_train/1000, bygone_time_train)
      UK_predi = append(UK_predi/divi, bygone_time_predict)
      # RF_MEv
    } else if (i1 == "WuS_SuB_CT_RF_MEv"){
      if (i2 == "_500.rda"){
        divi = 1000
      } else if (i2 == "_1000.rda"){
        divi = 500
      } else {
        divi = 100
      }
      RF_MEv_train = append(RF_MEv_train/1000, bygone_time_train)
      RF_MEv_predi = append(RF_MEv_predi/1000, bygone_time_predict)
      RF_MEv_pre_pro = append(RF_MEv_pre_pro/divi, bygone_time_pp)
      # loo_OK_RF
    } else if (i1 == "WuS_SuB_CT_loo_OK_RF"){
      if (i2 == "_500.rda"){
        divi = 100
      } else if (i2 == "_1000.rda"){
        divi = 20
      } else {
        divi = 10
      }
      loo_OK_RF_train = append(loo_OK_RF_train/1000, bygone_time_train)
      loo_OK_RF_predi = append(loo_OK_RF_predi/1000, bygone_time_predict)
      loo_OK_RF_pre_pro = append(loo_OK_RF_pre_pro/divi, bygone_time_pp)
    }
  }
}

# Create data frames 
CT_df_pre_pro = data.frame(loo_OK_RF_pre_pro,
                           RF_MEv_pre_pro,
                           RF_pre_pro,
                           bRF_pre_pro,
                           RF_GLS_pre_pro,
                           RF_oob_OK_pre_pro,
                           MLR_pre_pro,
                           OK_RF_pre_pro,
                           RFSI_pre_pro,
                           UK_pre_pro)

CT_df_train = data.frame(loo_OK_RF_train,
                         RF_MEv_train,
                         RF_train,
                         bRF_train,
                         RF_GLS_train,
                         RF_oob_OK_train,
                         MLR_train,
                         OK_RF_train,
                         RFSI_train,
                         UK_train)

CT_df_predi = data.frame(loo_OK_RF_predi,
                         RF_MEv_predi,
                         RF_predi,
                         bRF_predi,
                         RF_GLS_predi,
                         RF_oob_OK_predi,
                         MLR_predi,
                         OK_RF_predi,
                         RFSI_predi,
                         UK_predi)

CT_df_comp = CT_df_pre_pro + CT_df_train + CT_df_predi
################################################################################
## End (post processing of the results)
################################################################################
