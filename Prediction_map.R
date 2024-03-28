################################################################################
## Preparation
################################################################################
# Load necessary packages
library("pacman")
p_load("sp")
p_load("sf")
p_load("ranger")
p_load("gstat")       
p_load("automap")
p_load("terra")

# For run as background job
Base_m = TRUE
NuM_L_pm = TRUE
WuS_SuB_pm = TRUE

# Observation column
obs_col = "subMittelwert"

# Load training data 
load("Data/NuM_L.rda")
load("Data/WuS_SuB.rda")

fo_RF_NuM_L_base = as.formula(subMittelwert ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                                nfk + humus + cAckerland + log10_gwn + agrum_log10_restime + 
                                agrum_log10_gwn + agrum_log10_geschw + Ackerland + lbm_class_Gruenland + 
                                lbm_class_Unbewachsen + lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + 
                                aea20_2 + aea20_8 + aea20_12)
fo_RF_WuS_SuB_base = as.formula(subMittelwert ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                                  nfk + humus + cAckerland + log10_gwn + agrum_log10_restime + 
                                  agrum_log10_gwn + agrum_log10_geschw + Ackerland + lbm_class_Gruenland + 
                                  lbm_class_Unbewachsen + lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + 
                                  aea20_1 + aea20_2 + aea20_12 + aea20_13)

####
## Manage prediction data 
##

# Load prediction data
pred_d = readRDS("Data/pred_loc.rds")

# Compute tilted coordinates (formula: Møller et al. 2020) at 45 and 315 degree 
# 315 is choose instead of 135 because 135 will produce negative coordinates
pred_d$tc45 = sqrt(pred_d$Y^2 + pred_d$X^2) * cos(45 - atan(pred_d$Y / pred_d$X))
pred_d$tc315 = sqrt(pred_d$Y^2 + pred_d$X^2) * cos(315 - atan(pred_d$Y / pred_d$X))

# Subset based on Hydogeologische Großräume
NuM_L_pred = pred_d[pred_d$GR_NAME == "Nord- und mitteldeutsches Lockergesteinsgebiet",] 
WuS_SuB_pred = pred_d[pred_d$GR_NAME == "West- und süddeutsches Schichtstufen- und Bruchschollenland",]

# Sf df to pure df
NuM_L_pred = as.data.frame(NuM_L_pred)
WuS_SuB_pred = as.data.frame(WuS_SuB_pred)

# Get the number of rows to calculate how many points were removed
NuM_L_pred_n0 = nrow(NuM_L_pred)
WuS_SuB_pred_n0 = nrow(WuS_SuB_pred)

# Get unique values and frequency 
aea_NuM_L_pred = table(NuM_L_pred$aea20)
print(aea_NuM_L_pred)
aea_WuS_SuB_pred = table(WuS_SuB_pred$aea20)
print(aea_WuS_SuB_pred)
# Merge some AEA classes drop some others (see data handling)
NuM_L_pred = NuM_L_pred[NuM_L_pred$aea20 != 5, ] 
NuM_L_pred = NuM_L_pred[NuM_L_pred$aea20 != 6, ] 
NuM_L_pred = NuM_L_pred[NuM_L_pred$aea20 != 7, ] 
NuM_L_pred = NuM_L_pred[NuM_L_pred$aea20 != 14, ] 
NuM_L_pred$aea20[NuM_L_pred$aea20 == 1] <- 2
NuM_L_pred$aea20[NuM_L_pred$aea20 == 4] <- 3
NuM_L_pred$aea20[NuM_L_pred$aea20 == 9] <- 8
NuM_L_pred$aea20[NuM_L_pred$aea20 == 13] <- 12
WuS_SuB_pred = WuS_SuB_pred[WuS_SuB_pred$aea20 != 6, ]
WuS_SuB_pred = WuS_SuB_pred[WuS_SuB_pred$aea20 != 9, ]
WuS_SuB_pred = WuS_SuB_pred[WuS_SuB_pred$aea20 != 15, ]
WuS_SuB_pred = WuS_SuB_pred[WuS_SuB_pred$aea20 != 19, ]
WuS_SuB_pred = WuS_SuB_pred[WuS_SuB_pred$aea20 != 20, ]
WuS_SuB_pred$aea20[WuS_SuB_pred$aea20 == 3] <- 2
WuS_SuB_pred$aea20[WuS_SuB_pred$aea20 == 7] <- 2
WuS_SuB_pred$aea20[WuS_SuB_pred$aea20 == 8] <- 2
WuS_SuB_pred$aea20[WuS_SuB_pred$aea20 == 14] <- 13
# Check results
aea_NuM_L_pred_new = table(NuM_L_pred$aea20)
print(aea_NuM_L_pred_new)
aea_WuS_SuB_pred_new = table(WuS_SuB_pred$aea20)
print(aea_WuS_SuB_pred_new)
# Define indicator variables 
NuM_L_pred$aea20_2 = (NuM_L_pred$aea20 == "2") * 1
NuM_L_pred$aea20_8 = (NuM_L_pred$aea20 == "8") * 1
NuM_L_pred$aea20_12 = (NuM_L_pred$aea20 == "12") * 1
WuS_SuB_pred$aea20_1 = (WuS_SuB_pred$aea20 == "1") * 1
WuS_SuB_pred$aea20_2 = (WuS_SuB_pred$aea20 == "2") * 1
WuS_SuB_pred$aea20_12 = (WuS_SuB_pred$aea20 == "12") * 1
WuS_SuB_pred$aea20_13 = (WuS_SuB_pred$aea20 == "13") * 1
WuS_SuB_pred$aea20_18 = (WuS_SuB_pred$aea20 == "18") * 1

NuM_L_pred$lbm_class_Gruenland  = (NuM_L_pred$lbm_class == "Grünland") * 1
NuM_L_pred$lbm_class_Unbewachsen  = (NuM_L_pred$lbm_class == "Unbewachsen") * 1
NuM_L_pred$lbm_class_FeuchtgebieteWasser  = (NuM_L_pred$lbm_class == "Feuchtgebiete" | NuM_L_pred$lbm_class == "Wasser") * 1
NuM_L_pred$lbm_class_Siedlung  = (NuM_L_pred$lbm_class == "Siedlung") * 1
WuS_SuB_pred$lbm_class_Gruenland  = (WuS_SuB_pred$lbm_class == "Grünland") * 1
WuS_SuB_pred$lbm_class_Unbewachsen  = (WuS_SuB_pred$lbm_class == "Unbewachsen") * 1
WuS_SuB_pred$lbm_class_FeuchtgebieteWasser  = (WuS_SuB_pred$lbm_class == "Feuchtgebiete" | WuS_SuB_pred$lbm_class == "Wasser") * 1
WuS_SuB_pred$lbm_class_Siedlung  = (WuS_SuB_pred$lbm_class == "Siedlung") * 1

# Keep only the necessary columns 
NuM_L_pred = NuM_L_pred[, c("crestime", "cgwn", "cgeschw", 
                            "log10carea", "elevation", "nfk", "humus", 
                            "cAckerland", "log10_gwn", "agrum_log10_restime", 
                            "agrum_log10_gwn", "agrum_log10_geschw", "Ackerland", 
                            "lbm_class_Gruenland", "lbm_class_Unbewachsen", 
                            "lbm_class_FeuchtgebieteWasser", "lbm_class_Siedlung", 
                            "X", "Y", "tc45", "tc315", "aea20_2", "aea20_8", "aea20_12")]
WuS_SuB_pred = WuS_SuB_pred[, c("crestime", "cgwn", "cgeschw", "log10carea", 
                              "elevation", "nfk", "humus", "cAckerland", "log10_gwn", 
                              "agrum_log10_restime", "agrum_log10_gwn", 
                              "agrum_log10_geschw", "Ackerland", "lbm_class_Gruenland", 
                              "lbm_class_Unbewachsen", "lbm_class_FeuchtgebieteWasser", 
                              "lbm_class_Siedlung", "X", "Y", "tc45", "tc315", "aea20_1", 
                             "aea20_2", "aea20_12", "aea20_13")]

# Get the number of rows to calculate how many points were removed
NuM_L_pred_n1 = nrow(NuM_L_pred)
WuS_SuB_pred_n1 = nrow(WuS_SuB_pred)
  
# Remove rows with missing data
NuM_L_pred = NuM_L_pred[complete.cases(NuM_L_pred),]
WuS_SuB_pred = WuS_SuB_pred[complete.cases(WuS_SuB_pred),]

# Save prepared prediction dfs result
#saveRDS(NuM_L_pred, 'NuM_L_pred.rds')
#saveRDS(WuS_SuB_pred, 'WuS_SuB_pred.rds')

# Get the number of rows to calculate how many points were removed
NuM_L_pred_n2 = nrow(NuM_L_pred)
WuS_SuB_pred_n2 = nrow(WuS_SuB_pred)

NuM_L_aea_removed = NuM_L_pred_n0 - NuM_L_pred_n1
NuM_L_aea_removed_p = (NuM_L_aea_removed/NuM_L_pred_n0) * 100
NuM_L_NaN_removed = NuM_L_pred_n0 - NuM_L_pred_n2 - NuM_L_aea_removed
NuM_L_NaN_removed_p = (NuM_L_NaN_removed/NuM_L_pred_n0) * 100

WuS_SuB_aea_removed = WuS_SuB_pred_n0 - WuS_SuB_pred_n1
WuS_SuB_aea_removed_p = (WuS_SuB_aea_removed/WuS_SuB_pred_n0) * 100
WuS_SuB_NaN_removed = WuS_SuB_pred_n0 - WuS_SuB_pred_n2 - WuS_SuB_aea_removed
WuS_SuB_NaN_removed_p = (WuS_SuB_NaN_removed/WuS_SuB_pred_n0) * 100

##
## End (manage predcition data) 
####
################################################################################
## End (preparation)
################################################################################

################################################################################
## Prediction 
################################################################################
## Base RF
# Create model function 
RF_base_fun = function(formula, data){
  RF_model = ranger::ranger(formula = formula, 
                            data = data,
                            oob.error = FALSE,
                            seed = 7)
  return(RF_model)
}

# Create prediction function
RF_pred_base_fun = function(object, newdata){
  RF_prediction = predict(object = object,
                          data = newdata)
  # Add the RF predictions to the df
  newdata$RF_predictions = RF_prediction$predictions
  return(newdata)
}

## RF-oob-OK
# Create model function 
RF_fun = function(formula, data, obs_col){
  # Create RF model
  RF_model = ranger::ranger(formula = formula, 
                            data = data,
                            seed = 7)
  # Calculate the oob residuals and add them to the df
  data$oob_resi = data[, obs_col] - RF_model$predictions
  # Return RF model and training data
  return_list = list(model = RF_model, train_data = data)
  return(return_list)
}

# Create prediction function
RF_oob_OK_pred_fun = function(object, newdata){
  # Make the RF prediction
  RF_prediction = predict(object = object$model,
                          data = newdata)
  # Add the RF predictions to the df
  newdata$RF_prediction = RF_prediction$predictions
  # Get training data
  train_data = object$train_data
  # Create spatial points dfs
  sp_df_train = sp::SpatialPointsDataFrame(train_data[,c("X","Y")], train_data)
  # Fit variogram model
  resid_vm = automap::autofitVariogram(formula = oob_resi ~ 1,
                                       input_data = sp_df_train,
                                       model = c("Mat", "Exp"),
                                       kappa = c(seq(0.1, 0.4, 0.1)),
                                       fix.values = c(0, NA, NA))
  # Get the model
  resid_vmm = resid_vm["var_model"]$var_model
  # Create a spatial points df 
  newdata_sp_df = sp::SpatialPointsDataFrame(newdata[,c("X","Y")], newdata)
  # Ordinary Kriging oob residual interpolation
  ok_pred = gstat::krige(formula = oob_resi ~ 1, sp_df_train, 
                         model = resid_vmm, 
                         newdata = newdata_sp_df, 
                         debug.level = 0,
                         nmax = 200)
  # Sum up the residual interpolation and the RF prediction
  newdata_sp_df$final_prediction = newdata_sp_df$RF_predictions + 
    ok_pred$var1.pred  
  return(newdata_sp_df)
}

if (NuM_L_pm & Base_m){
  ## NuM_L
  # Model
  NuM_L_RF_model = RF_base_fun(data = NuM_L, formula = fo_RF_NuM_L_base)
  # Prediction
  NuM_L_pred_sp_df = RF_pred_base_fun(object = NuM_L_RF_model, 
                                        newdata = NuM_L_pred)
  # Save result
  saveRDS(NuM_L_pred_sp_df, file = "Results/NuM_L_base_preds_for_pred_map.rds")
}

if (WuS_SuB_pm & Base_m){
  ## WuS_SuB
  # Model
  WuS_SuB_RF_model = RF_base_fun(data = WuS_SuB, formula = fo_RF_WuS_SuB_base)
  # Prediction
  WuS_SuB_pred_sp_df = RF_pred_base_fun(object = WuS_SuB_RF_model, 
                                       newdata = WuS_SuB_pred)
  # Save result
  saveRDS(WuS_SuB_pred_sp_df, file = "Results/WuS_SuB_base_preds_for_pred_map.rds")
}

if (NuM_L_pm){
  ## NuM_L
  # Model
  NuM_L_RF_model = RF_fun(data = NuM_L, formula = fo_RF_NuM_L, obs_col = obs_col)
  # Prediction
  NuM_L_pred_sp_df = RF_oob_OK_pred_fun(object = NuM_L_RF_model, 
                                        newdata = NuM_L_pred)
  # Save result
  saveRDS(NuM_L_pred_sp_df, file = "Results/NuM_L_preds_for_pred_map.rds")
}

if (WuS_SuB_pm){
  ## WuS_SuB
  # Model
  WuS_SuB_RF_model = RF_fun(data = WuS_SuB, formula = fo_RF_WuS_SuB, obs_col = obs_col)
  # Prediction
  WuS_SuB_pred_sp_df = RF_oob_OK_pred_fun(object = WuS_SuB_RF_model, 
                                        newdata = WuS_SuB_pred)
  # Save result
  saveRDS(WuS_SuB_pred_sp_df, file = "Results/WuS_SuB_preds_for_pred_map.rds")
}
################################################################################
## End (prediction) 
################################################################################

################################################################################
## Post processing
################################################################################
# Load data
NuM_L_pred_sp_df = readRDS("Results/orgNitrate/NuM_L_preds_for_pred_map.rds")
WuS_SuB_pred_sp_df = readRDS("Results/orgNitrate/WuS_SuB_preds_for_pred_map.rds")

## NuM_L
# Remove unnecessary columns
NuM_L_pred_sp_df = NuM_L_pred_sp_df[, c("final_prediction")]

# Sp df to spatial pixels df
NuM_L_spx_df = SpatialPixelsDataFrame(points = NuM_L_pred_sp_df, data = NuM_L_pred_sp_df@data)

# Spatial pixels df to raster
NuM_L_predciton_raster = terra::rast(NuM_L_spx_df)

# Check result
NuM_L_predciton_raster["final_prediction"]
summary(NuM_L_predciton_raster["final_prediction"], size = nrow(NuM_L_pred_sp_df))
plot(NuM_L_predciton_raster["final_prediction"])

# Get the percentage of raster cells with values of and above 50 mg/l
NuM_L_pred_sp_df$a50 = NuM_L_pred_sp_df$final_prediction
NuM_L_pred_sp_df$a50[NuM_L_pred_sp_df$a50 < 50] <- 0
NuM_L_pred_sp_df$a50[NuM_L_pred_sp_df$a50 >= 50] <- 1
NuM_L_pred_a50_table = table(NuM_L_pred_sp_df$a50)
NuM_L_pred_a50_table[2]/(NuM_L_pred_a50_table[2]+NuM_L_pred_a50_table[1])

# Save the prediction raster 
terra::writeRaster(NuM_L_predciton_raster, filename = "Results/NuM_L_predciton_raster.tif")

## WuS_SuB
# Remove unnecessary columns
WuS_SuB_pred_sp_df = WuS_SuB_pred_sp_df[, c("final_prediction")]

# Sp df to spatial pixels df
WuS_SuB_spx_df = SpatialPixelsDataFrame(points = WuS_SuB_pred_sp_df, data = WuS_SuB_pred_sp_df@data)

# Spatial pixels df to raster
WuS_SuB_predciton_raster = terra::rast(WuS_SuB_spx_df)

# Check result
WuS_SuB_predciton_raster["final_prediction"]
summary(WuS_SuB_predciton_raster["final_prediction"], size = nrow(WuS_SuB_pred_sp_df))
plot(WuS_SuB_predciton_raster["final_prediction"])

# Get the percentage of raster cells with values of and above 50 mg/l
WuS_SuB_pred_sp_df$a50 = WuS_SuB_pred_sp_df$final_prediction
WuS_SuB_pred_sp_df$a50[WuS_SuB_pred_sp_df$a50 < 50] <- 0
WuS_SuB_pred_sp_df$a50[WuS_SuB_pred_sp_df$a50 >= 50] <- 1
WuS_SuB_pred_a50_table = table(WuS_SuB_pred_sp_df$a50)
WuS_SuB_pred_a50_table[2]/(WuS_SuB_pred_a50_table[2]+WuS_SuB_pred_a50_table[1])

# Save the prediction raster 
terra::writeRaster(WuS_SuB_predciton_raster, filename = "Results/WuS_SuB_predciton_raster.tif")

## Calculate 95 quantile of all predicted values
temp = append(NuM_L_pred_sp_df$final_prediction, WuS_SuB_pred_sp_df$final_prediction)
all_p95 = quantile(x = temp, probs = c(0.95))

################################################################################
## End (post processing)
################################################################################

################################################################################
## Post processing (base)
################################################################################
# Load data
NuM_L_pred_df = readRDS("Results/orgNitrate/NuM_L_base_preds_for_pred_map.rds")
WuS_SuB_pred_df = readRDS("Results/orgNitrate/WuS_SuB_base_preds_for_pred_map.rds")

# Df to sp_df
NuM_L_pred_sp_df = sp::SpatialPointsDataFrame(NuM_L_pred_df[,c("X","Y")], NuM_L_pred_df)
WuS_SuB_pred_sp_df = sp::SpatialPointsDataFrame(WuS_SuB_pred_df[,c("X","Y")], WuS_SuB_pred_df)

## NuM_L
# Remove unnecessary columns
NuM_L_pred_sp_df = NuM_L_pred_sp_df[, c("RF_predictions")]

# Sp df to spatial pixels df
NuM_L_spx_df = SpatialPixelsDataFrame(points = NuM_L_pred_sp_df, data = NuM_L_pred_sp_df@data)

# Spatial pixels df to raster
NuM_L_predciton_raster = terra::rast(NuM_L_spx_df)

# Check result
NuM_L_predciton_raster["RF_predictions"]
summary(NuM_L_predciton_raster["RF_predictions"], size = nrow(NuM_L_pred_sp_df))
plot(NuM_L_predciton_raster["RF_predictions"])

# Get the percentage of raster cells with values of and above 50 mg/l
NuM_L_pred_sp_df$a50 = NuM_L_pred_sp_df$RF_predictions
NuM_L_pred_sp_df$a50[NuM_L_pred_sp_df$a50 < 50] <- 0
NuM_L_pred_sp_df$a50[NuM_L_pred_sp_df$a50 >= 50] <- 1
NuM_L_pred_a50_table = table(NuM_L_pred_sp_df$a50)
NuM_L_pred_a50_table[2]/(NuM_L_pred_a50_table[2]+NuM_L_pred_a50_table[1])

# Save the prediction raster 
terra::writeRaster(NuM_L_predciton_raster, filename = "Results/NuM_L_base_predciton_raster.tif")

## WuS_SuB
# Remove unnecessary columns
WuS_SuB_pred_sp_df = WuS_SuB_pred_sp_df[, c("RF_predictions")]

# Sp df to spatial pixels df
WuS_SuB_spx_df = SpatialPixelsDataFrame(points = WuS_SuB_pred_sp_df, data = WuS_SuB_pred_sp_df@data)

# Spatial pixels df to raster
WuS_SuB_predciton_raster = terra::rast(WuS_SuB_spx_df)

# Check result
WuS_SuB_predciton_raster["RF_predictions"]
summary(WuS_SuB_predciton_raster["RF_predictions"], size = nrow(WuS_SuB_pred_sp_df))
plot(WuS_SuB_predciton_raster["RF_predictions"])

# Get the percentage of raster cells with values of and above 50 mg/l
WuS_SuB_pred_sp_df$a50 = WuS_SuB_pred_sp_df$RF_predictions
WuS_SuB_pred_sp_df$a50[WuS_SuB_pred_sp_df$a50 < 50] <- 0
WuS_SuB_pred_sp_df$a50[WuS_SuB_pred_sp_df$a50 >= 50] <- 1
WuS_SuB_pred_a50_table = table(WuS_SuB_pred_sp_df$a50)
WuS_SuB_pred_a50_table[2]/(WuS_SuB_pred_a50_table[2]+WuS_SuB_pred_a50_table[1])

# Save the prediction raster 
terra::writeRaster(WuS_SuB_predciton_raster, filename = "Results/WuS_SuB_base_predciton_raster.tif")

## Calculate 95 quantile of all predicted values
temp = append(NuM_L_pred_sp_df$RF_predictions, WuS_SuB_pred_sp_df$RF_predictions)
all_p95 = quantile(x = temp, probs = c(0.95))

################################################################################
## End (post processing (base))
################################################################################