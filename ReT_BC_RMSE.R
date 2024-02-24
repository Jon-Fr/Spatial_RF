################################################################################
## Calculate RMSE/RSE of retransformed predictions
################################################################################
# Additional functions that are not included in packages
source("auxiliary_functions.R", encoding = "UTF-8")

# Load NuM_L and WuS_SuB to get the observation values
load("Data/NuM_L.rda")
load("Data/WuS_SuB.rda")

# Vector of the second part of the file names
f_names_vec1 = c("NuM_L/", "WuS_SuB/")

# Vector of the second part of the file names
f_names_vec2 = c("NuM_L_sp_cv_bRF_", "NuM_L_sp_cv_MLR_", "NuM_L_sp_cv_RF_",
                 "NuM_L_sp_cv_RF_MEv_", "SuB_NuM_L_sp_cv_RFSI_", 
                 "NuM_L_sp_cv_loo_OK_RF_", "SuB_NuM_L_sp_cv_OK_RF_",
                 "NuM_L_sp_cv_RF_oob_OK_", "SuB_NuM_L_sp_cv_UK_",
                 "WuS_SuB_sp_cv_bRF_", "WuS_SuB_sp_cv_MLR_", 
                 "WuS_SuB_sp_cv_RF_",
                 "WuS_SuB_sp_cv_RF_MEv_", "SuB_WuS_SuB_sp_cv_RFSI_", 
                 "WuS_SuB_sp_cv_loo_OK_RF_", "SuB_WuS_SuB_sp_cv_OK_RF_",
                 "WuS_SuB_sp_cv_RF_oob_OK_", "SuB_WuS_SuB_sp_cv_UK_")

# Vector of the third part of the file names
f_names_vec3 = c("0_+50_0.rda", "100_+50_0.rda", "400_+100_0.rda",
                 "900_+100_0.rda", "1600_+100_0.rda", "3600_+100_0.rda", 
                 "6400_+100_0.rda", "10000_+100_0.rda", "16900_+100_0.rda",
                 "25600_+100_0.rda", "40000_+100_0.rda") 

# Create vectors to store the mean RMSE values per distance
NuM_L_bRF_mRv = c()
NuM_L_MLR_mRv = c() 
NuM_L_RF_mRv = c()  
NuM_L_RFSI_mRv = c()
NuM_L_RF_MEv_mRv = c()  
NuM_L_OK_RF_mRv = c() 
NuM_L_loo_OK_RF_mRv = c()   
NuM_L_RF_oob_OK_mRv = c()
NuM_L_UK_mRv = c()
WuS_SuB_bRF_mRv = c()
WuS_SuB_MLR_mRv = c() 
WuS_SuB_RF_mRv = c()  
WuS_SuB_RFSI_mRv = c()
WuS_SuB_RF_MEv_mRv = c()  
WuS_SuB_OK_RF_mRv = c()  
WuS_SuB_loo_OK_RF_mRv = c()  
WuS_SuB_RF_oob_OK_mRv = c()
WuS_SuB_UK_mRv = c() 

# Vectors for the mean distance to the nearest neighbor 
WuS_SuB_mean_nn_dist = c()
NuM_L_mean_nn_dist = c()
  
# For loops
for (i1 in f_names_vec1){
  for (i2 in f_names_vec2){
    for (i3 in f_names_vec3){
    # Complete file name
    f_name = paste("Results/bcNitrateRe/",i1, i2, i3, sep = "")
    # Load file
    load(f_name)
    ## Get mean RMSE value and mean nearest neighbor distance
    ## NuM_L_sp_cv_bRF
    if (i2 == "NuM_L_sp_cv_bRF_"){
      # RMSE
      RMSE_values = c()
      for (i4 in 1:length(sp_cv_bRF$represampling[[1]])){
        # Get prediction value and id/row number
        pred_value_bc = sp_cv_bRF$error_fold[[1]][[i4]]$test$pred
        pred_id = sp_cv_bRF$represampling[[1]][[i4]]$test
        # Get observation value
        obs_value = NuM_L[i4, "subMittelwert"]
        # Retransform obs value 
        pred_value = inv_boxcox(pred_value_bc, 0.0202)
        RMSE = sqrt((obs_value-pred_value)^2)
        # Add result to vector
        RMSE_values = append(RMSE_values, RMSE)
      }
      # Add mean of vector to result vector
      NuM_L_bRF_mRv = append(NuM_L_bRF_mRv, mean(RMSE_values)) 
      # nn distance
      nn_distances = c()
      for (i5 in 1:length(sp_cv_bRF$error_fold[[1]])){
        nn_dist = sp_cv_bRF$error_fold[[1]][[i5]]$distance
        nn_distances = append(nn_distances, nn_dist)
      }
      NuM_L_mean_nn_dist = append(NuM_L_mean_nn_dist, mean(nn_distances))
    ## NuM_L_sp_cv_MLR
    } else if (i2 == "NuM_L_sp_cv_MLR_"){
        RMSE_values = c()
        for (i4 in 1:length(sp_cv_MLR$represampling[[1]])){
          pred_value_bc = sp_cv_MLR$error_fold[[1]][[i4]]$test$pred
          pred_id = sp_cv_MLR$represampling[[1]][[i4]]$test
          obs_value = NuM_L[i4, "subMittelwert"]
          pred_value = inv_boxcox(pred_value_bc, 0.0202)
          RMSE = sqrt((obs_value-pred_value)^2)
          RMSE_values = append(RMSE_values, RMSE)
        }
        NuM_L_MLR_mRv = append(NuM_L_MLR_mRv, mean(RMSE_values))
    ## NuM_L_sp_cv_RFSI   
    } else if (i2 == "NuM_L_sp_cv_RFSI_"){
        RMSE_values = c()
        for (i4 in 1:length(sp_cv_RFSI$represampling[[1]])){
          pred_value_bc = sp_cv_RFSI$error_fold[[1]][[i4]]$test$pred
          pred_id = sp_cv_RFSI$represampling[[1]][[i4]]$test
          obs_value = NuM_L[i4, "subMittelwert"]
          pred_value = inv_boxcox(pred_value_bc, 0.0202)
          RMSE = sqrt((obs_value-pred_value)^2)
          RMSE_values = append(RMSE_values, RMSE)
        }
        NuM_L_RFSI_mRv = append(NuM_L_RFSI_mRv, mean(RMSE_values))
    ## NuM_L_sp_cv_RF_MEv 
    } else if (i2 == "NuM_L_sp_cv_RF_MEv_"){
        RMSE_values = c()
        for (i4 in 1:length(sp_cv_RF_MEv$represampling[[1]])){
          pred_value_bc = sp_cv_RF_MEv$error_fold[[1]][[i4]]$test$pred
          pred_id = sp_cv_RF_MEv$represampling[[1]][[i4]]$test
          obs_value = NuM_L[i4, "subMittelwert"]
          pred_value = inv_boxcox(pred_value_bc, 0.0202)
          RMSE = sqrt((obs_value-pred_value)^2)
          RMSE_values = append(RMSE_values, RMSE)
        }
        NuM_L_RF_MEv_mRv = append(NuM_L_RF_MEv_mRv, mean(RMSE_values))
    ## NuM_L_sp_cv_RF   
    } else if (i2 == "NuM_L_sp_cv_RF_"){
        RMSE_values = c()
        for (i4 in 1:length(sp_cv_RF$represampling[[1]])){
          pred_value_bc = sp_cv_RF$error_fold[[1]][[i4]]$test$pred
          pred_id = sp_cv_RF$represampling[[1]][[i4]]$test
          obs_value = NuM_L[i4, "subMittelwert"]
          pred_value = inv_boxcox(pred_value_bc, 0.0202)
          RMSE = sqrt((obs_value-pred_value)^2)
          RMSE_values = append(RMSE_values, RMSE)
        }
        NuM_L_RF_mRv = append(NuM_L_RF_mRv, mean(RMSE_values))
    ## NuM_L_sp_cv_OK_RF 
    } else if (i2 == "NuM_L_sp_cv_OK_RF_"){
        RMSE_values = c()
        for (i4 in 1:length(sp_cv_OK_RF$represampling[[1]])){
          pred_value_bc = sp_cv_OK_RF$error_fold[[1]][[i4]]$test$pred
          pred_id = sp_cv_OK_RF$represampling[[1]][[i4]]$test
          obs_value = NuM_L[i4, "subMittelwert"]
          pred_value = inv_boxcox(pred_value_bc, 0.0202)
          RMSE = sqrt((obs_value-pred_value)^2)
          RMSE_values = append(RMSE_values, RMSE)
        }
        NuM_L_OK_RF_mRv = append(NuM_L_OK_RF_mRv, mean(RMSE_values))
    ## NuM_L_sp_cv_loo_OK_RF   
    } else if (i2 == "NuM_L_sp_cv_loo_OK_RF_"){
        RMSE_values = c()
        for (i4 in 1:length(sp_cv_loo_OK_RF$represampling[[1]])){
          pred_value_bc = sp_cv_loo_OK_RF$error_fold[[1]][[i4]]$test$pred
          pred_id = sp_cv_loo_OK_RF$represampling[[1]][[i4]]$test
          obs_value = NuM_L[i4, "subMittelwert"]
          pred_value = inv_boxcox(pred_value_bc, 0.0202)
          RMSE = sqrt((obs_value-pred_value)^2)
          RMSE_values = append(RMSE_values, RMSE)
        }
        NuM_L_loo_OK_RF_mRv = append(NuM_L_loo_OK_RF_mRv, mean(RMSE_values))
    ## NuM_L_sp_cv_RF_oob_OK   
    } else if (i2 == "NuM_L_sp_cv_RF_oob_OK_"){
        RMSE_values = c()
        for (i4 in 1:length(sp_cv_RF_oob_OK$represampling[[1]])){
          pred_value_bc = sp_cv_RF_oob_OK$error_fold[[1]][[i4]]$test$pred
          pred_id = sp_cv_RF_oob_OK$represampling[[1]][[i4]]$test
          obs_value = NuM_L[i4, "subMittelwert"]
          pred_value = inv_boxcox(pred_value_bc, 0.0202)
          RMSE = sqrt((obs_value-pred_value)^2)
          RMSE_values = append(RMSE_values, RMSE)
        }
        NuM_L_RF_oob_OK_mRv = append(NuM_L_RF_oob_OK_mRv, mean(RMSE_values))
    ## NuM_L_sp_cv_UK     
    } else if (i2 == "NuM_L_sp_cv_UK_"){
        RMSE_values = c()
        for (i4 in 1:length(sp_cv_UK$represampling[[1]])){
          pred_value_bc = sp_cv_UK$error_fold[[1]][[i4]]$test$pred
          pred_id = sp_cv_UK$represampling[[1]][[i4]]$test
          obs_value = NuM_L[i4, "subMittelwert"]
          pred_value = inv_boxcox(pred_value_bc, 0.0202)
          RMSE = sqrt((obs_value-pred_value)^2)
          RMSE_values = append(RMSE_values, RMSE)
        }
        NuM_L_UK_mRv = append(NuM_L_UK_mRv, mean(RMSE_values))
    ## WuS_SuB_sp_cv_bRF
    } else if (i2 == "WuS_SuB_sp_cv_bRF_"){
        RMSE_values = c()
        for (i4 in 1:length(sp_cv_bRF$represampling[[1]])){
          pred_value_bc = sp_cv_bRF$error_fold[[1]][[i4]]$test$pred
          pred_id = sp_cv_bRF$represampling[[1]][[i4]]$test
          obs_value = WuS_SuB[i4, "subMittelwert"]
          pred_value = inv_boxcox(pred_value_bc, 0.4242)
          RMSE = sqrt((obs_value-pred_value)^2)
          RMSE_values = append(RMSE_values, RMSE)
        }
        WuS_SuB_bRF_mRv = append(WuS_SuB_bRF_mRv, mean(RMSE_values)) 
        nn_distances = c()
        for (i5 in 1:length(sp_cv_bRF$error_fold[[1]])){
          nn_dist = sp_cv_bRF$error_fold[[1]][[i5]]$distance
          nn_distances = append(nn_distances, nn_dist)
        }
        WuS_SuB_mean_nn_dist = append(WuS_SuB_mean_nn_dist, mean(nn_distances))
    ## WuS_SuB_sp_cv_MLR    
    } else if (i2 == "WuS_SuB_sp_cv_MLR_"){
        RMSE_values = c()
        for (i4 in 1:length(sp_cv_MLR$represampling[[1]])){
          pred_value_bc = sp_cv_MLR$error_fold[[1]][[i4]]$test$pred
          pred_id = sp_cv_MLR$represampling[[1]][[i4]]$test
          obs_value = WuS_SuB[i4, "subMittelwert"]
          pred_value = inv_boxcox(pred_value_bc, 0.4242)
          RMSE = sqrt((obs_value-pred_value)^2)
          RMSE_values = append(RMSE_values, RMSE)
        }
        WuS_SuB_MLR_mRv = append(WuS_SuB_MLR_mRv, mean(RMSE_values))
    ## WuS_SuB_sp_cv_RFSI   
    } else if (i2 == "WuS_SuB_sp_cv_RFSI_"){
          RMSE_values = c()
          for (i4 in 1:length(sp_cv_RFSI$represampling[[1]])){
            pred_value_bc = sp_cv_RFSI$error_fold[[1]][[i4]]$test$pred
            pred_id = sp_cv_RFSI$represampling[[1]][[i4]]$test
            obs_value = WuS_SuB[i4, "subMittelwert"]
            pred_value = inv_boxcox(pred_value_bc, 0.4242)
            RMSE = sqrt((obs_value-pred_value)^2)
            RMSE_values = append(RMSE_values, RMSE)
          }
          WuS_SuB_RFSI_mRv = append(WuS_SuB_RFSI_mRv, mean(RMSE_values))
    ## WuS_SuB_sp_cv_RF_MEv 
    } else if (i2 == "WuS_SuB_sp_cv_RF_MEv_"){
      RMSE_values = c()
        for (i4 in 1:length(sp_cv_RF_MEv$represampling[[1]])){
          pred_value_bc = sp_cv_RF_MEv$error_fold[[1]][[i4]]$test$pred
          pred_id = sp_cv_RF_MEv$represampling[[1]][[i4]]$test
          obs_value = WuS_SuB[i4, "subMittelwert"]
          pred_value = inv_boxcox(pred_value_bc, 0.4242)
          RMSE = sqrt((obs_value-pred_value)^2)
          RMSE_values = append(RMSE_values, RMSE)
        }
        WuS_SuB_RF_MEv_mRv = append(WuS_SuB_RF_MEv_mRv, mean(RMSE_values))
    ## WuS_SuB_sp_cv_RF   
    } else if (i2 == "WuS_SuB_sp_cv_RF_"){
        RMSE_values = c()
        for (i4 in 1:length(sp_cv_RF$represampling[[1]])){
          pred_value_bc = sp_cv_RF$error_fold[[1]][[i4]]$test$pred
          pred_id = sp_cv_RF$represampling[[1]][[i4]]$test
          obs_value = WuS_SuB[i4, "subMittelwert"]
          pred_value = inv_boxcox(pred_value_bc, 0.4242)
          RMSE = sqrt((obs_value-pred_value)^2)
          RMSE_values = append(RMSE_values, RMSE)
        }
        WuS_SuB_RF_mRv = append(WuS_SuB_RF_mRv, mean(RMSE_values))
    ## WuS_SuB_sp_cv_OK_RF 
    } else if (i2 == "WuS_SuB_sp_cv_OK_RF_"){
        RMSE_values = c()
        for (i4 in 1:length(sp_cv_OK_RF$represampling[[1]])){
          pred_value_bc = sp_cv_OK_RF$error_fold[[1]][[i4]]$test$pred
          pred_id = sp_cv_OK_RF$represampling[[1]][[i4]]$test
          obs_value = WuS_SuB[i4, "subMittelwert"]
          pred_value = inv_boxcox(pred_value_bc, 0.4242)
          RMSE = sqrt((obs_value-pred_value)^2)
          RMSE_values = append(RMSE_values, RMSE)
      }
      WuS_SuB_OK_RF_mRv = append(WuS_SuB_OK_RF_mRv, mean(RMSE_values))
    ## WuS_SuB_sp_cv_loo_OK_RF   
    } else if (i2 == "WuS_SuB_sp_cv_loo_OK_RF_"){
        RMSE_values = c()
        for (i4 in 1:length(sp_cv_loo_OK_RF$represampling[[1]])){
          pred_value_bc = sp_cv_loo_OK_RF$error_fold[[1]][[i4]]$test$pred
          pred_id = sp_cv_loo_OK_RF$represampling[[1]][[i4]]$test
          obs_value = WuS_SuB[i4, "subMittelwert"]
          pred_value = inv_boxcox(pred_value_bc, 0.4242)
          RMSE = sqrt((obs_value-pred_value)^2)
          RMSE_values = append(RMSE_values, RMSE)
        }
        WuS_SuB_loo_OK_RF_mRv = append(WuS_SuB_loo_OK_RF_mRv, mean(RMSE_values))
    ## WuS_SuB_sp_cv_RF_oob_OK   
    } else if (i2 == "WuS_SuB_sp_cv_RF_oob_OK_"){
        RMSE_values = c()
        for (i4 in 1:length(sp_cv_RF_oob_OK$represampling[[1]])){
          pred_value_bc = sp_cv_RF_oob_OK$error_fold[[1]][[i4]]$test$pred
          pred_id = sp_cv_RF_oob_OK$represampling[[1]][[i4]]$test
          obs_value = WuS_SuB[i4, "subMittelwert"]
          pred_value = inv_boxcox(pred_value_bc, 0.4242)
          RMSE = sqrt((obs_value-pred_value)^2)
          RMSE_values = append(RMSE_values, RMSE)
        }
        WuS_SuB_RF_oob_OK_mRv = append(WuS_SuB_RF_oob_OK_mRv, mean(RMSE_values))
    ## WuS_SuB_sp_cv_UK     
    } else if (i2 == "WuS_SuB_sp_cv_UK_"){
        RMSE_values = c()
        for (i4 in 1:length(sp_cv_UK$represampling[[1]])){
          pred_value_bc = sp_cv_UK$error_fold[[1]][[i4]]$test$pred
          pred_id = sp_cv_UK$represampling[[1]][[i4]]$test
          obs_value = WuS_SuB[i4, "subMittelwert"]
          pred_value = inv_boxcox(pred_value_bc, 0.4242)
          RMSE = sqrt((obs_value-pred_value)^2)
          RMSE_values = append(RMSE_values, RMSE)
        }
        WuS_SuB_UK_mRv = append(WuS_SuB_UK_mRv, mean(RMSE_values))
    }
    }
  }
}
################################################################################
## End (Calculate RMSE of retransformed predictions)
################################################################################


################################################################################
## Plot results
################################################################################
####
## NuM_L
##


##
## End (NuM_L)
####


####
## WuS_SuB
##


##
## End (WuS_SuB)
####
################################################################################
## End (plot results)
################################################################################


################################################################################
## Test area
################################################################################
load("Data/NuM_L.rda")
load("Results/bcNitrateRe/NuM_L/NuM_L_sp_cv_bRF_0_+50_0.rda")

pred_value_bc_1 = sp_cv_bRF$error_fold[[1]][[1]]$test$pred
pred_id_1 = sp_cv_bRF$represampling[[1]][[1]]$test

l_re = length(sp_cv_bRF$represampling[[1]])
################################################################################
## End (test area)
################################################################################

