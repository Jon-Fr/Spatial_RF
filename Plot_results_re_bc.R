################################################################################
## Calculate mean RMSE and bias of retransformed predictions
################################################################################
# Fewer decimal places, apply penalty on exponential notation 
options("scipen"= 999, "digits"=4)

# Additional functions that are not included in packages
source("auxiliary_functions.R", encoding = "UTF-8")

# Load NuM_L and WuS_SuB to get the observation values
load("Data/NuM_L.rda")
load("Data/WuS_SuB.rda")

# Vector of the first part of the file names
f_names_vec1 = c("NuM_L/", "WuS_SuB/")

# Vector of the second part of the file names
f_names_vec2 = c("NuM_L_sp_cv_bRF_", "NuM_L_sp_cv_MLR_", "NuM_L_sp_cv_RF_",
                 "NuM_L_sp_cv_RF_MEv_", "NuM_L_sp_cv_RFSI_", 
                 "NuM_L_sp_cv_loo_OK_RF_", "NuM_L_sp_cv_OK_RF_",
                 "NuM_L_sp_cv_RF_oob_OK_", "NuM_L_sp_cv_UK_",
                 "WuS_SuB_sp_cv_bRF_", "WuS_SuB_sp_cv_MLR_", 
                 "WuS_SuB_sp_cv_RF_",
                 "WuS_SuB_sp_cv_RF_MEv_", "WuS_SuB_sp_cv_RFSI_", 
                 "WuS_SuB_sp_cv_loo_OK_RF_", "WuS_SuB_sp_cv_OK_RF_",
                 "WuS_SuB_sp_cv_RF_oob_OK_", "WuS_SuB_sp_cv_UK_")

# Vector of the third part of the file names
f_names_vec3 = c("0_+50_0.rda", "100_+50_0.rda", "400_+100_0.rda",
                 "900_+100_0.rda", "1600_+100_0.rda", "3600_+100_0.rda", 
                 "6400_+100_0.rda", "10000_+100_0.rda", "16900_+100_0.rda",
                 "25600_+100_0.rda", "40000_+100_0.rda") 

# Create vectors to store the mean RMSE and bias values per distance
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
NuM_L_bRF_b = c()
NuM_L_MLR_b = c() 
NuM_L_RF_b = c()  
NuM_L_RFSI_b = c()
NuM_L_RF_MEv_b = c()  
NuM_L_OK_RF_b = c() 
NuM_L_loo_OK_RF_b = c()   
NuM_L_RF_oob_OK_b = c()
NuM_L_UK_b = c()
WuS_SuB_bRF_b = c()
WuS_SuB_MLR_b = c() 
WuS_SuB_RF_b = c()  
WuS_SuB_RFSI_b = c()
WuS_SuB_RF_MEv_b = c()  
WuS_SuB_OK_RF_b = c()  
WuS_SuB_loo_OK_RF_b = c()  
WuS_SuB_RF_oob_OK_b = c()
WuS_SuB_UK_b = c() 

# Observation values
NuM_L_obs = list()
WuS_SuB_obs = list()

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
    area_c1 = substr(i1, 1, 3)
    area_c2 = substr(i2, 1, 3)
    if (area_c1 == area_c2){
      load(f_name)
      ## Get mean RMSE and bias value and mean nearest neighbor distance
      ## NuM_L_sp_cv_bRF
      if (i2 == "NuM_L_sp_cv_bRF_"){
        # RMSE and bias
        RMSE_values = c()
        diff_values = c()
        RMSE_c_values = c()
        obser_values = c()
        for (i4 in 1:length(sp_cv_bRF$represampling[[1]])){
          # Get prediction value and id/row number
          pred_value_bc = sp_cv_bRF$error_fold[[1]][[i4]]$test$pred
          pred_id = sp_cv_bRF$represampling[[1]][[i4]]$test
          # Get observation values
          obs_value = NuM_L[pred_id, "subMittelwert"]
          obser_values = append(obser_values, obs_value)
          # Retransform pred value 
          pred_value = inv_boxcox(pred_value_bc, 0.0202)
          # Calculate RMSE and difference
          RMSE = sqrt((obs_value-pred_value)^2)
          differ = obs_value-pred_value
          # Add results to vectors
          RMSE_values = append(RMSE_values, RMSE)
          diff_values = append(diff_values, differ)
        }
        # Add observation values 
        NuM_L_obs[[length(NuM_L_obs)+1]] = obser_values
        # Add mean of RMSE vector to result vector
        NuM_L_bRF_mRv = append(NuM_L_bRF_mRv, mean(RMSE_values)) 
        # Get bias (mean of difference) and add it to the vector
        NuM_L_bRF_b = append(NuM_L_bRF_b, mean(diff_values)) 
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
          diff_values = c()
          for (i4 in 1:length(sp_cv_MLR$represampling[[1]])){
            pred_value_bc = sp_cv_MLR$error_fold[[1]][[i4]]$test$pred
            pred_id = sp_cv_MLR$represampling[[1]][[i4]]$test
            obs_value = NuM_L[pred_id, "subMittelwert"]
            pred_value = inv_boxcox(pred_value_bc, 0.0202)
            RMSE = sqrt((obs_value-pred_value)^2)
            RMSE_values = append(RMSE_values, RMSE)
            differ = obs_value-pred_value
            diff_values = append(diff_values, differ)
          }
          NuM_L_MLR_mRv = append(NuM_L_MLR_mRv, mean(RMSE_values))
          NuM_L_MLR_b = append(NuM_L_MLR_b, mean(diff_values)) 
      ## NuM_L_sp_cv_RFSI   
      } else if (i2 == "NuM_L_sp_cv_RFSI_"){
          RMSE_values = c()
          diff_values = c()
          for (i4 in 1:length(sp_cv_RFSI$represampling[[1]])){
            pred_value_bc = sp_cv_RFSI$error_fold[[1]][[i4]]$test$pred
            pred_id = sp_cv_RFSI$represampling[[1]][[i4]]$test
            obs_value = NuM_L[pred_id, "subMittelwert"]
            pred_value = inv_boxcox(pred_value_bc, 0.0202)
            RMSE = sqrt((obs_value-pred_value)^2)
            RMSE_values = append(RMSE_values, RMSE)
            differ = obs_value-pred_value
            diff_values = append(diff_values, differ)
          }
          NuM_L_RFSI_mRv = append(NuM_L_RFSI_mRv, mean(RMSE_values))
          NuM_L_RFSI_b = append(NuM_L_RFSI_b, mean(diff_values)) 
      ## NuM_L_sp_cv_RF_MEv 
      } else if (i2 == "NuM_L_sp_cv_RF_MEv_"){
          RMSE_values = c()
          diff_values = c()
          for (i4 in 1:length(sp_cv_RF_MEv$represampling[[1]])){
            pred_value_bc = sp_cv_RF_MEv$error_fold[[1]][[i4]]$test$pred
            pred_id = sp_cv_RF_MEv$represampling[[1]][[i4]]$test
            obs_value = NuM_L[pred_id, "subMittelwert"]
            pred_value = inv_boxcox(pred_value_bc, 0.0202)
            RMSE = sqrt((obs_value-pred_value)^2)
            RMSE_values = append(RMSE_values, RMSE)
            differ = obs_value-pred_value
            diff_values = append(diff_values, differ)
          }
          NuM_L_RF_MEv_mRv = append(NuM_L_RF_MEv_mRv, mean(RMSE_values))
          NuM_L_RF_MEv_b = append(NuM_L_RF_MEv_b, mean(diff_values)) 
      ## NuM_L_sp_cv_RF   
      } else if (i2 == "NuM_L_sp_cv_RF_"){
          RMSE_values = c()
          diff_values = c()
          for (i4 in 1:length(sp_cv_RF$represampling[[1]])){
            pred_value_bc = sp_cv_RF$error_fold[[1]][[i4]]$test$pred
            pred_id = sp_cv_RF$represampling[[1]][[i4]]$test
            obs_value = NuM_L[pred_id, "subMittelwert"]
            pred_value = inv_boxcox(pred_value_bc, 0.0202)
            RMSE = sqrt((obs_value-pred_value)^2)
            RMSE_values = append(RMSE_values, RMSE)
            differ = obs_value-pred_value
            diff_values = append(diff_values, differ)
          }
          NuM_L_RF_mRv = append(NuM_L_RF_mRv, mean(RMSE_values))
          NuM_L_RF_b = append(NuM_L_RF_b, mean(diff_values)) 
      ## NuM_L_sp_cv_OK_RF 
      } else if (i2 == "NuM_L_sp_cv_OK_RF_"){
          RMSE_values = c()
          diff_values = c()
          for (i4 in 1:length(sp_cv_OK_RF$represampling[[1]])){
            pred_value_bc = sp_cv_OK_RF$error_fold[[1]][[i4]]$test$pred
            pred_id = sp_cv_OK_RF$represampling[[1]][[i4]]$test
            obs_value = NuM_L[pred_id, "subMittelwert"]
            pred_value = inv_boxcox(pred_value_bc, 0.0202)
            RMSE = sqrt((obs_value-pred_value)^2)
            RMSE_values = append(RMSE_values, RMSE)
            differ = obs_value-pred_value
            diff_values = append(diff_values, differ)
          }
          NuM_L_OK_RF_mRv = append(NuM_L_OK_RF_mRv, mean(RMSE_values))
          NuM_L_OK_RF_b = append(NuM_L_OK_RF_b, mean(diff_values)) 
      ## NuM_L_sp_cv_loo_OK_RF   
      } else if (i2 == "NuM_L_sp_cv_loo_OK_RF_"){
          RMSE_values = c()
          diff_values = c()
          for (i4 in 1:length(sp_cv_loo_OK_RF$represampling[[1]])){
            pred_value_bc = sp_cv_loo_OK_RF$error_fold[[1]][[i4]]$test$pred
            pred_id = sp_cv_loo_OK_RF$represampling[[1]][[i4]]$test
            obs_value = NuM_L[pred_id, "subMittelwert"]
            pred_value = inv_boxcox(pred_value_bc, 0.0202)
            RMSE = sqrt((obs_value-pred_value)^2)
            RMSE_values = append(RMSE_values, RMSE)
            differ = obs_value-pred_value
            diff_values = append(diff_values, differ)
          }
          NuM_L_loo_OK_RF_mRv = append(NuM_L_loo_OK_RF_mRv, mean(RMSE_values))
          NuM_L_loo_OK_RF_b = append(NuM_L_loo_OK_RF_b, mean(diff_values)) 
      ## NuM_L_sp_cv_RF_oob_OK   
      } else if (i2 == "NuM_L_sp_cv_RF_oob_OK_"){
          RMSE_values = c()
          diff_values = c()
          for (i4 in 1:length(sp_cv_RF_oob_OK$represampling[[1]])){
            pred_value_bc = sp_cv_RF_oob_OK$error_fold[[1]][[i4]]$test$pred
            pred_id = sp_cv_RF_oob_OK$represampling[[1]][[i4]]$test
            obs_value = NuM_L[pred_id, "subMittelwert"]
            pred_value = inv_boxcox(pred_value_bc, 0.0202)
            RMSE = sqrt((obs_value-pred_value)^2)
            RMSE_values = append(RMSE_values, RMSE)
            differ = obs_value-pred_value
            diff_values = append(diff_values, differ)
          }
          NuM_L_RF_oob_OK_mRv = append(NuM_L_RF_oob_OK_mRv, mean(RMSE_values))
          NuM_L_RF_oob_OK_b = append(NuM_L_RF_oob_OK_b, mean(diff_values)) 
      ## NuM_L_sp_cv_UK     
      } else if (i2 == "NuM_L_sp_cv_UK_"){
          RMSE_values = c()
          diff_values = c()
          for (i4 in 1:length(sp_cv_UK$represampling[[1]])){
            pred_value_bc = sp_cv_UK$error_fold[[1]][[i4]]$test$pred
            pred_id = sp_cv_UK$represampling[[1]][[i4]]$test
            obs_value = NuM_L[pred_id, "subMittelwert"]
            pred_value = inv_boxcox(pred_value_bc, 0.0202)
            RMSE = sqrt((obs_value-pred_value)^2)
            RMSE_values = append(RMSE_values, RMSE)
            differ = obs_value-pred_value
            diff_values = append(diff_values, differ)
          }
          NuM_L_UK_mRv = append(NuM_L_UK_mRv, mean(RMSE_values))
          NuM_L_UK_b = append(NuM_L_UK_b, mean(diff_values)) 
      ## WuS_SuB_sp_cv_bRF
      } else if (i2 == "WuS_SuB_sp_cv_bRF_"){
          RMSE_values = c()
          diff_values = c()
          obser_values = c()
          for (i4 in 1:length(sp_cv_bRF$represampling[[1]])){
            pred_value_bc = sp_cv_bRF$error_fold[[1]][[i4]]$test$pred
            pred_id = sp_cv_bRF$represampling[[1]][[i4]]$test
            obs_value = WuS_SuB[pred_id, "subMittelwert"]
            obser_values = append(obser_values, obs_value)
            pred_value = inv_boxcox(pred_value_bc, 0.4242)
            RMSE = sqrt((obs_value-pred_value)^2)
            RMSE_values = append(RMSE_values, RMSE)
            differ = obs_value-pred_value
            diff_values = append(diff_values, differ)
          }
          WuS_SuB_obs[[length(WuS_SuB_obs)+1]] = obser_values
          WuS_SuB_bRF_mRv = append(WuS_SuB_bRF_mRv, mean(RMSE_values))
          WuS_SuB_bRF_b = append(WuS_SuB_bRF_b, mean(diff_values)) 
          nn_distances = c()
          diff_values = c()
          for (i5 in 1:length(sp_cv_bRF$error_fold[[1]])){
            nn_dist = sp_cv_bRF$error_fold[[1]][[i5]]$distance
            nn_distances = append(nn_distances, nn_dist)
          }
          WuS_SuB_mean_nn_dist = append(WuS_SuB_mean_nn_dist, mean(nn_distances))
      ## WuS_SuB_sp_cv_MLR    
      } else if (i2 == "WuS_SuB_sp_cv_MLR_"){
          RMSE_values = c()
          diff_values = c()
          for (i4 in 1:length(sp_cv_MLR$represampling[[1]])){
            pred_value_bc = sp_cv_MLR$error_fold[[1]][[i4]]$test$pred
            pred_id = sp_cv_MLR$represampling[[1]][[i4]]$test
            obs_value = WuS_SuB[pred_id, "subMittelwert"]
            pred_value = inv_boxcox(pred_value_bc, 0.4242)
            RMSE = sqrt((obs_value-pred_value)^2)
            RMSE_values = append(RMSE_values, RMSE)
            differ = obs_value-pred_value
            diff_values = append(diff_values, differ)
          }
          WuS_SuB_MLR_mRv = append(WuS_SuB_MLR_mRv, mean(RMSE_values))
          WuS_SuB_MLR_b = append(WuS_SuB_MLR_b, mean(diff_values)) 
      ## WuS_SuB_sp_cv_RFSI   
      } else if (i2 == "WuS_SuB_sp_cv_RFSI_"){
            RMSE_values = c()
            diff_values = c()
            for (i4 in 1:length(sp_cv_RFSI$represampling[[1]])){
              pred_value_bc = sp_cv_RFSI$error_fold[[1]][[i4]]$test$pred
              pred_id = sp_cv_RFSI$represampling[[1]][[i4]]$test
              obs_value = WuS_SuB[pred_id, "subMittelwert"]
              pred_value = inv_boxcox(pred_value_bc, 0.4242)
              RMSE = sqrt((obs_value-pred_value)^2)
              RMSE_values = append(RMSE_values, RMSE)
              differ = obs_value-pred_value
              diff_values = append(diff_values, differ)
            }
            WuS_SuB_RFSI_mRv = append(WuS_SuB_RFSI_mRv, mean(RMSE_values))
            WuS_SuB_RFSI_b = append(WuS_SuB_RFSI_b, mean(diff_values)) 
      ## WuS_SuB_sp_cv_RF_MEv 
      } else if (i2 == "WuS_SuB_sp_cv_RF_MEv_"){
        RMSE_values = c()
        diff_values = c()
          for (i4 in 1:length(sp_cv_RF_MEv$represampling[[1]])){
            pred_value_bc = sp_cv_RF_MEv$error_fold[[1]][[i4]]$test$pred
            pred_id = sp_cv_RF_MEv$represampling[[1]][[i4]]$test
            obs_value = WuS_SuB[pred_id, "subMittelwert"]
            pred_value = inv_boxcox(pred_value_bc, 0.4242)
            RMSE = sqrt((obs_value-pred_value)^2)
            RMSE_values = append(RMSE_values, RMSE)
            differ = obs_value-pred_value
            diff_values = append(diff_values, differ)
          }
          WuS_SuB_RF_MEv_mRv = append(WuS_SuB_RF_MEv_mRv, mean(RMSE_values))
          WuS_SuB_RF_MEv_b = append(WuS_SuB_RF_MEv_b, mean(diff_values)) 
      ## WuS_SuB_sp_cv_RF   
      } else if (i2 == "WuS_SuB_sp_cv_RF_"){
          RMSE_values = c()
          diff_values = c()
          for (i4 in 1:length(sp_cv_RF$represampling[[1]])){
            pred_value_bc = sp_cv_RF$error_fold[[1]][[i4]]$test$pred
            pred_id = sp_cv_RF$represampling[[1]][[i4]]$test
            obs_value = WuS_SuB[pred_id, "subMittelwert"]
            pred_value = inv_boxcox(pred_value_bc, 0.4242)
            RMSE = sqrt((obs_value-pred_value)^2)
            RMSE_values = append(RMSE_values, RMSE)
            differ = obs_value-pred_value
            diff_values = append(diff_values, differ)
          }
          WuS_SuB_RF_mRv = append(WuS_SuB_RF_mRv, mean(RMSE_values))
          WuS_SuB_RF_b = append(WuS_SuB_RF_b, mean(diff_values)) 
      ## WuS_SuB_sp_cv_OK_RF 
      } else if (i2 == "WuS_SuB_sp_cv_OK_RF_"){
          RMSE_values = c()
          diff_values = c()
          for (i4 in 1:length(sp_cv_OK_RF$represampling[[1]])){
            pred_value_bc = sp_cv_OK_RF$error_fold[[1]][[i4]]$test$pred
            pred_id = sp_cv_OK_RF$represampling[[1]][[i4]]$test
            obs_value = WuS_SuB[pred_id, "subMittelwert"]
            pred_value = inv_boxcox(pred_value_bc, 0.4242)
            RMSE = sqrt((obs_value-pred_value)^2)
            RMSE_values = append(RMSE_values, RMSE)
            differ = obs_value-pred_value
            diff_values = append(diff_values, differ)
        }
        WuS_SuB_OK_RF_mRv = append(WuS_SuB_OK_RF_mRv, mean(RMSE_values))
        WuS_SuB_OK_RF_b = append(WuS_SuB_OK_RF_b, mean(diff_values)) 
      ## WuS_SuB_sp_cv_loo_OK_RF   
      } else if (i2 == "WuS_SuB_sp_cv_loo_OK_RF_"){
          RMSE_values = c()
          diff_values = c()
          for (i4 in 1:length(sp_cv_loo_OK_RF$represampling[[1]])){
            pred_value_bc = sp_cv_loo_OK_RF$error_fold[[1]][[i4]]$test$pred
            pred_id = sp_cv_loo_OK_RF$represampling[[1]][[i4]]$test
            obs_value = WuS_SuB[pred_id, "subMittelwert"]
            pred_value = inv_boxcox(pred_value_bc, 0.4242)
            RMSE = sqrt((obs_value-pred_value)^2)
            RMSE_values = append(RMSE_values, RMSE)
            differ = obs_value-pred_value
            diff_values = append(diff_values, differ)
          }
          WuS_SuB_loo_OK_RF_mRv = append(WuS_SuB_loo_OK_RF_mRv, mean(RMSE_values))
          WuS_SuB_loo_OK_RF_b = append(WuS_SuB_loo_OK_RF_b, mean(diff_values)) 
      ## WuS_SuB_sp_cv_RF_oob_OK   
      } else if (i2 == "WuS_SuB_sp_cv_RF_oob_OK_"){
          RMSE_values = c()
          diff_values = c()
          for (i4 in 1:length(sp_cv_RF_oob_OK$represampling[[1]])){
            pred_value_bc = sp_cv_RF_oob_OK$error_fold[[1]][[i4]]$test$pred
            pred_id = sp_cv_RF_oob_OK$represampling[[1]][[i4]]$test
            obs_value = WuS_SuB[pred_id, "subMittelwert"]
            pred_value = inv_boxcox(pred_value_bc, 0.4242)
            RMSE = sqrt((obs_value-pred_value)^2)
            RMSE_values = append(RMSE_values, RMSE)
            differ = obs_value-pred_value
            diff_values = append(diff_values, differ)
          }
          WuS_SuB_RF_oob_OK_mRv = append(WuS_SuB_RF_oob_OK_mRv, mean(RMSE_values))
          WuS_SuB_RF_oob_OK_b = append(WuS_SuB_RF_oob_OK_b, mean(diff_values)) 
      ## WuS_SuB_sp_cv_UK     
      } else if (i2 == "WuS_SuB_sp_cv_UK_"){
          RMSE_values = c()
          diff_values = c()
          for (i4 in 1:length(sp_cv_UK$represampling[[1]])){
            pred_value_bc = sp_cv_UK$error_fold[[1]][[i4]]$test$pred
            pred_id = sp_cv_UK$represampling[[1]][[i4]]$test
            obs_value = WuS_SuB[pred_id, "subMittelwert"]
            pred_value = inv_boxcox(pred_value_bc, 0.4242)
            RMSE = sqrt((obs_value-pred_value)^2)
            RMSE_values = append(RMSE_values, RMSE)
            differ = obs_value-pred_value
            diff_values = append(diff_values, differ)
          }
          WuS_SuB_UK_mRv = append(WuS_SuB_UK_mRv, mean(RMSE_values))
          WuS_SuB_UK_b = append(WuS_SuB_UK_b, mean(diff_values)) 
      }
    }
    }
  }
}

# Scale the nn dist vecs 
NuM_L_mean_sqrt_nn_dist = sqrt(NuM_L_mean_nn_dist)
WuS_SuB_mean_sqrt_nn_dist = sqrt(WuS_SuB_mean_nn_dist)

# Test constant prediction of the median
NuM_L_medis = c()
NuM_L_c_mRMSE = c()
for (i in 1:length(NuM_L_obs)){
  medi = median(NuM_L_obs[[i]])
  NuM_L_medis = append(NuM_L_medis, medi)
  RMSE_c_values = c()
  for (i2 in 1:length(NuM_L_obs[[i]])){
    RMSE = sqrt((NuM_L_obs[[i]][[i2]] - medi)^2)
    RMSE_c_values = append(RMSE_c_values, RMSE)
  }
  NuM_L_c_mRMSE = append(NuM_L_c_mRMSE, mean(RMSE_c_values))
}

WuS_SuB_medis = c()
WuS_SuB_c_mRMSE = c()
for (i in 1:length(WuS_SuB_obs)){
  medi = median(WuS_SuB_obs[[i]])
  WuS_SuB_medis = append(WuS_SuB_medis, medi)
  RMSE_c_values = c()
  for (i2 in 1:length(WuS_SuB_obs[[i]])){
    RMSE = sqrt((WuS_SuB_obs[[i]][[i2]] - medi)^2)
    RMSE_c_values = append(RMSE_c_values, RMSE)
  }
  WuS_SuB_c_mRMSE = append(WuS_SuB_c_mRMSE, mean(RMSE_c_values))
}
################################################################################
## End (Calculate mean RMSE and bias of retransformed predictions)
################################################################################


################################################################################
## Plot SPEPs
################################################################################
# Load original scale and bc scale dfs for comparison 
load("Results/bcNitrateRe/mRMSE_dfs_org.rda")
load("Results/bcNitrateRe/mBias_dfs_org.rda")
load("Results/bcNitrateRe/mBias_dfs_bc.rda")W
# Multipilcation with -1 makes the interpreation easier 
WuS_SuB_mBias_df = WuS_SuB_mBias_df * -1
WuS_SuB_mBias_df_bc = WuS_SuB_mBias_df_bc * -1
NuM_L_mBias_df = NuM_L_mBias_df * -1
NuM_L_mBias_df_bc = NuM_L_mBias_df_bc * -1

# Create dfs
NuM_L_mRMSE_df_re = data.frame(NuM_L_UK_mRv, NuM_L_RF_oob_OK_mRv, 
                            NuM_L_loo_OK_RF_mRv, 
                            NuM_L_RF_MEv_mRv, NuM_L_RFSI_mRv, NuM_L_RF_mRv, 
                            NuM_L_bRF_mRv,
                            NuM_L_MLR_mRv, NuM_L_OK_RF_mRv)

WuS_SuB_mRMSE_df_re = data.frame(WuS_SuB_UK_mRv, WuS_SuB_RF_oob_OK_mRv, 
                              WuS_SuB_loo_OK_RF_mRv, 
                              WuS_SuB_RF_MEv_mRv, WuS_SuB_RFSI_mRv, 
                              WuS_SuB_RF_mRv, WuS_SuB_bRF_mRv,
                              WuS_SuB_MLR_mRv, WuS_SuB_OK_RF_mRv)

NuM_L_mBias_df_re = data.frame(NuM_L_UK_b, NuM_L_RF_oob_OK_b, 
                               NuM_L_loo_OK_RF_b, 
                               NuM_L_RF_MEv_b, NuM_L_RFSI_b, NuM_L_RF_b, 
                               NuM_L_bRF_b,
                               NuM_L_MLR_b, NuM_L_OK_RF_b)
NuM_L_mBias_df_re = NuM_L_mBias_df_re * -1

WuS_SuB_mBias_df_re = data.frame(WuS_SuB_UK_b, WuS_SuB_RF_oob_OK_b, 
                                 WuS_SuB_loo_OK_RF_b, 
                                 WuS_SuB_RF_MEv_b, WuS_SuB_RFSI_b, 
                                 WuS_SuB_RF_b, WuS_SuB_bRF_b,
                                 WuS_SuB_MLR_b, WuS_SuB_OK_RF_b)
WuS_SuB_mBias_df_re = WuS_SuB_mBias_df_re * -1

# Calculate difference df
NuM_L_mRMSE_df_diff = NuM_L_mRMSE_df_re - NuM_L_mRMSE_df
WuS_SuB_mRMSE_df_diff = WuS_SuB_mRMSE_df_re - WuS_SuB_mRMSE_df

# Percentage change df
NuM_L_mRMSE_df_pc = (NuM_L_mRMSE_df_diff / NuM_L_mRMSE_df) * 100
WuS_SuB_mRMSE_df_pc = (WuS_SuB_mRMSE_df_diff / WuS_SuB_mRMSE_df) * 100


# Create color vector 
col_vec = c("#666666", "#9900FF", "#0000FF", "#FF0000", "#FF9933", "#33CC33",
            "#CCCCCC", "#000000", "#66FFFF")

## Set plot layout
layout_mat <- matrix(c(1,2,1,3), nrow = 2, ncol = 2,
                     byrow = TRUE)
layout_mat

my_lay = layout(mat = layout_mat, 
                heights = c(2.5, 2.5),
                widths = c(0.5, 4.5), respect =FALSE)
layout.show(my_lay)

## Percentage change plot ##

# First plot
par(mar = c(2, 0, 0, 0)) # bottom, left, top, right margins
plot(NULL, ylab = "", bty = "n", 
     xlim = c(0, 0.1), ylim = c(0, 0.1), xaxt = "n", yaxt = "n")
mtext(expression("Prozentual Veränderung des mittleren RMSE"),
      side = 4, line = -4, col = 1, cex = 1.1)

## NuM_L
# Margins
par(mar = c(3, 0, 1, 1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1,                 
     xlab = "", 
     ylab = "",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(-45, 45),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 2.236068, 10.95445, 21.21320, 30.82207, 40.62019, 60.41523, 
               80.31189, 100.24969, 130.19217, 160.15617, 200.12496), 
     labels = c(0, 5, 120, 450, 950, 1650, 3650, 6450, 10050, 16950, 25650, 
                40050))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot values
for (i in 1:ncol(NuM_L_mRMSE_df_pc)){
  # Points
  points(x = NuM_L_mean_sqrt_nn_dist,
         y = NuM_L_mRMSE_df_pc[,i],
         pch = 21,
         bg = col_vec[i])
  # Line
  lines(x = NuM_L_mean_sqrt_nn_dist,
        y = NuM_L_mRMSE_df_pc[,i],
        col = col_vec[i],
        lty = 2, lwd = 2)
}

legend(x = "bottomright" , legend = c("a)"), bty = "n", cex = 1.25)

## WuS_SuB
# Margins
par(mar = c(4, 0, 0, 1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1,                 
     xlab = "Mittlere Vorhersagedistanz [m]", 
     #ylab = "Mittlerer RMSE",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(-10, 10),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 5, 11.18034, 21.21320, 30.82207, 40.62019, 60.41523, 
               80.31189, 100.24969, 130.19217, 160.15617, 200.12496), 
     labels = c(0, 25, 125, 450, 950, 1650, 3650, 6450, 10050, 16950, 25650, 
                40050))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot values
for (i in 1:ncol(WuS_SuB_mRMSE_df_pc)){
  # Points
  points(x = WuS_SuB_mean_sqrt_nn_dist,
         y = WuS_SuB_mRMSE_df_pc[,i],
         pch = 21,
         bg = col_vec[i])
  # Line
  lines(x = WuS_SuB_mean_sqrt_nn_dist,
        y = WuS_SuB_mRMSE_df_pc[,i],
        col = col_vec[i],
        lty = 2, lwd = 2)
}

legend(x = "bottomright" , legend = c("b)"), bty = "n", cex = 1.25)

# Legend
legend(x = "topright", cex = 0.8, ncol = 2,
       legend = c("MLR", "UK", "RF", "RF-K", "RF-MEv", "RF-oob-OK", "OK-RF", 
                  "RF-loo-OK", "RFSI"),
       fill = c("#000000", "#666666", "#CCCCCC", "#33CC33", "#FF0000", "#9900FF",
                "#66FFFF", "#0000FF", "#FF9933"))

## Percentage change plot ##


## SPEP ##

# First plot
par(mar = c(2, 0, 0, 0)) # bottom, left, top, right margins
plot(NULL, ylab = "", bty = "n", 
     xlim = c(0, 0.1), ylim = c(0, 0.1), xaxt = "n", yaxt = "n")
mtext(expression("Mittlerer RMSE [mg/L]"),
      side = 4, line = -4, col = 1, cex = 1.1)

## NuM_L
# Margins
par(mar = c(3, 0, 1, 1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1,                 
     xlab = "", 
     ylab = "",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(0, 40),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 2.236068, 10.95445, 21.21320, 30.82207, 40.62019, 60.41523, 
               80.31189, 100.24969, 130.19217, 160.15617, 200.12496), 
     labels = c(0, 5, 120, 450, 950, 1650, 3650, 6450, 10050, 16950, 25650, 
                40050))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot mRMSE values
for (i in 1:ncol(NuM_L_mRMSE_df_re)){
  # Points
  points(x = NuM_L_mean_sqrt_nn_dist,
         y = NuM_L_mRMSE_df_re[,i],
         pch = 21,
         bg = col_vec[i])
  # Line
  lines(x = NuM_L_mean_sqrt_nn_dist,
        y = NuM_L_mRMSE_df_re[,i],
        col = col_vec[i],
        lty = 2, lwd = 2)
}

legend(x = "bottomleft" , legend = c("a)"), bty = "n", cex = 1.25)

## WuS_SuB
# Margins
par(mar = c(4, 0, 0, 1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1,                 
     xlab = "Mittlere Vorhersagedistanz [m]", 
     #ylab = "Mittlerer RMSE",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(0, 13),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 5, 11.18034, 21.21320, 30.82207, 40.62019, 60.41523, 80.31189,
               100.19980, 130.15376, 160.12495, 200.09998), 
     labels = c(0, 25, 125, 450, 950, 1650, 3650, 6450, 10040, 16940, 25640, 
                40040))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot mRMSE values
for (i in 1:ncol(WuS_SuB_mRMSE_df_re)){
  # Points
  points(x = WuS_SuB_mean_sqrt_nn_dist,
         y = WuS_SuB_mRMSE_df_re[,i],
         pch = 21,
         bg = col_vec[i])
  # Line
  lines(x = WuS_SuB_mean_sqrt_nn_dist,
        y = WuS_SuB_mRMSE_df_re[,i],
        col = col_vec[i],
        lty = 2, lwd = 2)
}

# Legend
legend(x = "bottomright", cex = 0.8, ncol = 2,
       legend = c("MLR", "UK", "RF", "RF-K", "RF-MEv", "RF-oob-OK", "OK-RF", 
                  "RF-loo-OK", "RFSI"),
       fill = c("#000000", "#666666", "#CCCCCC", "#33CC33", "#FF0000", "#9900FF",
                "#66FFFF", "#0000FF", "#FF9933"))

legend(x = "bottomleft" , legend = c("b)"), bty = "n", cex = 1.25)

## SPEP ##


## Bias Plot ##
## Set plot layout
layout_mat <- matrix(c(1,2,3,4), nrow = 4, ncol = 1,
                     byrow = TRUE)
layout_mat

my_lay = layout(mat = layout_mat, 
                heights = c(2.5, 2.5, 2.5, 0.35),
                widths = c(0.5, 4.5), respect =FALSE)
layout.show(my_lay)


# First Plot
# Margins
par(mar = c(2, 4, 0, 1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1,                 
     xlab = "", 
     ylab = "Verzerrung [mg/L]",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(-25, 20),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 2.236068, 10.95445, 21.21320, 30.82207, 40.62019, 60.41523, 
               80.31189, 100.24969, 130.19217, 160.15617, 200.12496), 
     labels = c("","","","","","","","","","","",""))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot values
for (i in 1:ncol(NuM_L_mBias_df)){
  # Points
  points(x = NuM_L_mean_sqrt_nn_dist,
         y = NuM_L_mBias_df[,i],
         pch = 21,
         bg = col_vec[i])
  # Line
  lines(x = NuM_L_mean_sqrt_nn_dist,
        y = NuM_L_mBias_df[,i],
        col = col_vec[i],
        lty = 2, lwd = 2)
}


legend(x = "bottomright" , legend = c("a)"), bty = "n", cex = 1.25)

# Second plot 
# Margins
par(mar = c(2, 4, 0, 1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1,                 
     xlab = "", 
     ylab = "Verzerrung [Box-Cox]",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(-0.3, 0.6),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 2.236068, 10.95445, 21.21320, 30.82207, 40.62019, 60.41523, 
               80.31189, 100.24969, 130.19217, 160.15617, 200.12496), 
     labels = c("","","","","","","","","","","",""))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot values
for (i in 1:ncol(NuM_L_mBias_df_bc)){
  # Points
  points(x = NuM_L_mean_sqrt_nn_dist,
         y = NuM_L_mBias_df_bc[,i],
         pch = 21,
         bg = col_vec[i])
  # Line
  lines(x = NuM_L_mean_sqrt_nn_dist,
        y = NuM_L_mBias_df_bc[,i],
        col = col_vec[i],
        lty = 2, lwd = 2)
}

legend(x = "bottomright" , legend = c("b)"), bty = "n", cex = 1.25)

# Third Plot
# Margins
par(mar = c(2, 4, 0, 1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1,                 
     xlab = "", 
     ylab = "Verzerrung [mg/L]",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(-25, 20),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 2.236068, 10.95445, 21.21320, 30.82207, 40.62019, 60.41523, 
               80.31189, 100.24969, 130.19217, 160.15617, 200.12496), 
     labels = c(0, 5, 120, 450, 950, 1650, 3650, 6450, 10050, 16950, 25650, 
                40050))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot values
for (i in 1:ncol(NuM_L_mBias_df_re)){
  # Points
  points(x = NuM_L_mean_sqrt_nn_dist,
         y = NuM_L_mBias_df_re[,i],
         pch = 21,
         bg = col_vec[i])
  # Line
  lines(x = NuM_L_mean_sqrt_nn_dist,
        y = NuM_L_mBias_df_re[,i],
        col = col_vec[i],
        lty = 2, lwd = 2)
}

# Legend
legend(x = "top", cex = 1, ncol = 2,
       legend = c("MLR", "UK", "RF", "RF-K", "RF-MEv", "RF-oob-OK", "OK-RF", 
                  "RF-loo-OK", "RFSI"),
       fill = c("#000000", "#666666", "#CCCCCC", "#33CC33", "#FF0000", "#9900FF",
                "#66FFFF", "#0000FF", "#FF9933"))

legend(x = "topright" , legend = c("c)"), bty = "n", cex = 1.25)

# Last plot
par(mar = c(0, 4, 0, 1)) # bottom, left, top, right margins
plot(NULL, ylab = "", bty = "n", 
     xlim = c(0, 0.1), ylim = c(0, 0.1), xaxt = "n", yaxt = "n")
mtext(expression("Mittlere Vorhersagedistanz [m]"),
      side = 1, line = -1, col = 1, cex = 1)


# First Plot
# Margins
par(mar = c(2, 4, 0, 1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1,                 
     xlab = "", 
     ylab = "Verzerrung [mg/L]",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(-5, 4),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 2.236068, 10.95445, 21.21320, 30.82207, 40.62019, 60.41523, 
               80.31189, 100.24969, 130.19217, 160.15617, 200.12496), 
     labels = c("","","","","","","","","","","",""))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot values
for (i in 1:ncol(WuS_SuB_mBias_df)){
  # Points
  points(x = WuS_SuB_mean_sqrt_nn_dist,
         y = WuS_SuB_mBias_df[,i],
         pch = 21,
         bg = col_vec[i])
  # Line
  lines(x = WuS_SuB_mean_sqrt_nn_dist,
        y = WuS_SuB_mBias_df[,i],
        col = col_vec[i],
        lty = 2, lwd = 2)
}



legend(x = "bottomright" , legend = c("a)"), bty = "n", cex = 1.25)

# Second Plot
# Margins
par(mar = c(2, 4, 0, 1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1,                 
     xlab = "", 
     ylab = "Verzerrung [Box-Cox]",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(-0.4, 0.3),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 2.236068, 10.95445, 21.21320, 30.82207, 40.62019, 60.41523, 
               80.31189, 100.24969, 130.19217, 160.15617, 200.12496), 
     labels = c("","","","","","","","","","","",""))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot values
for (i in 1:ncol(WuS_SuB_mBias_df_bc)){
  # Points
  points(x = WuS_SuB_mean_sqrt_nn_dist,
         y = WuS_SuB_mBias_df_bc[,i],
         pch = 21,
         bg = col_vec[i])
  # Line
  lines(x = WuS_SuB_mean_sqrt_nn_dist,
        y = WuS_SuB_mBias_df_bc[,i],
        col = col_vec[i],
        lty = 2, lwd = 2)
}

legend(x = "bottomright" , legend = c("b)"), bty = "n", cex = 1.25)

# Third Plot
# Margins
par(mar = c(2, 4, 0, 1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1,                 
     xlab = "", 
     ylab = "Verzerrung [mg/L]",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(-5, 4),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 2.236068, 10.95445, 21.21320, 30.82207, 40.62019, 60.41523, 
               80.31189, 100.24969, 130.19217, 160.15617, 200.12496), 
     labels = c(0, 5, 120, 450, 950, 1650, 3650, 6450, 10050, 16950, 25650, 
                40050))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot values
for (i in 1:ncol(WuS_SuB_mBias_df_re)){
  # Points
  points(x = WuS_SuB_mean_sqrt_nn_dist,
         y = WuS_SuB_mBias_df_re[,i],
         pch = 21,
         bg = col_vec[i])
  # Line
  lines(x = WuS_SuB_mean_sqrt_nn_dist,
        y = WuS_SuB_mBias_df_re[,i],
        col = col_vec[i],
        lty = 2, lwd = 2)
}


# Legend
legend(x = "topright", cex = 1, ncol = 2,
       legend = c("MLR", "UK", "RF", "RF-K", "RF-MEv", "RF-oob-OK", "OK-RF", 
                  "RF-loo-OK", "RFSI"),
       fill = c("#000000", "#666666", "#CCCCCC", "#33CC33", "#FF0000", "#9900FF",
                "#66FFFF", "#0000FF", "#FF9933"))

legend(x = "bottomright" , legend = c("c)"), bty = "n", cex = 1.25)

# Last plot
par(mar = c(0, 4, 0, 1)) # bottom, left, top, right margins
plot(NULL, ylab = "", bty = "n", 
     xlim = c(0, 0.1), ylim = c(0, 0.1), xaxt = "n", yaxt = "n")
mtext(expression("Mittlere Vorhersagedistanz [m]"),
      side = 1, line = -1, col = 1, cex = 1)

################################################################################
## End (plot SPEPs)
################################################################################


################################################################################
## Test area
################################################################################
max_bc = max(NuM_L$bcNitrate)
min_bc = min(NuM_L$bcNitrate)
range_bc = max_bc - min_bc
sd_bc = sd(NuM_L$bcNitrate)
range_to_sd_bc = (range_bc/sd_bc)
mean_bc = mean(NuM_L$bcNitrate)

max_sm = max(NuM_L$subMittelwert)
min_sm = min(NuM_L$subMittelwert)
range_sm = max_sm - min_sm
sd_sm = sd(NuM_L$subMittelwert)
range_to_sd_sm = (range_bc/sd_bc)
mean_sm = mean(NuM_L$subMittelwert)
################################################################################
## End (test area)
################################################################################

