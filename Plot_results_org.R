################################################################################
## Non spatial loo CV
################################################################################
# Create vectors for the RMSE values
NuM_L_bRF = c()
NuM_L_MLR = c() 
NuM_L_RF = c()  
NuM_L_RFSI = c()
NuM_L_RF_MEv = c()  
NuM_L_OK_RF = c() 
NuM_L_loo_OK_RF = c()   
NuM_L_RF_oob_OK = c()
NuM_L_UK = c()
WuS_SuB_bRF = c()
WuS_SuB_MLR = c() 
WuS_SuB_RF = c()  
WuS_SuB_RFSI = c()
WuS_SuB_RF_MEv = c()  
WuS_SuB_OK_RF = c()  
WuS_SuB_loo_OK_RF = c()  
WuS_SuB_RF_oob_OK = c()
WuS_SuB_UK = c() 

# Vectors for the mean distance to the nearest neighbor 
WuS_SuB_mean_nn_dist = c()
NuM_L_mean_nn_dist = c()

# Vector of the first part of the file names
f_names_vec1 = c("NuM_L/non_spatial_loo_cv/", "WuS_SuB/non_spatial_loo_cv/")

# Vector of the second part of the file names
f_names_vec2 = c("NuM_L_sp_cv_bRF_", 
                 "NuM_L_sp_cv_MLR_", 
                 "NuM_L_sp_cv_RF_",
                 "NuM_L_sp_cv_RF_MEv_", 
                 "NuM_L_sp_cv_RFSI_", 
                 "NuM_L_sp_cv_loo_OK_RF_", 
                 "NuM_L_sp_cv_OK_RF_",
                 "NuM_L_sp_cv_RF_oob_OK_", 
                 "WuS_SuB_sp_cv_bRF_", 
                 "WuS_SuB_sp_cv_MLR_", 
                 "WuS_SuB_sp_cv_RF_",
                 "WuS_SuB_sp_cv_RF_MEv_", 
                 "WuS_SuB_sp_cv_RFSI_", 
                 "WuS_SuB_sp_cv_loo_OK_RF_", 
                 "WuS_SuB_sp_cv_OK_RF_",
                 "WuS_SuB_sp_cv_RF_oob_OK_") # "WuS_SuB_sp_cv_UK_", "NuM_L_sp_cv_UK_"


# Load results and store the RMSE values in the vectors
for (i1 in f_names_vec1){
  for (i2 in f_names_vec2){
      # Complete file name
      f_name = paste("Results/orgNitrate/",i1, i2, "0_+all_10.rda", sep = "")
      # Load file
      area_c1 = substr(i1, 1, 3)
      area_c2 = substr(i2, 1, 3)
      if (area_c1 == area_c2){
        load(f_name)
        ## Get mean RMSE value and mean nearest neighbor distance
        ## NuM_L_sp_cv_bRF
        if (i2 == "NuM_L_sp_cv_bRF_"){
          # RMSE
          err = summary(sp_cv_bRF$error_fold)
          mRMSE = err["test.rmse", "mean"]
          NuM_L_bRF = append(NuM_L_bRF, mRMSE)
          # nn distance
          nn_distances = c()
          for (i4 in 1:length(sp_cv_bRF$error_fold[[1]])){
            nn_dist = sp_cv_bRF$error_fold[[1]][[i4]]$distance
            nn_distances = append(nn_distances, nn_dist)
          }
          NuM_L_mean_nn_dist = append(NuM_L_mean_nn_dist, mean(nn_distances))
          ## NuM_L_sp_cv_MLR
        } else if (i2 == "NuM_L_sp_cv_MLR_"){
          err = summary(sp_cv_MLR$error_fold)
          mRMSE = err["test.rmse", "mean"]
          NuM_L_MLR = append(NuM_L_MLR, mRMSE)
          ## NuM_L_sp_cv_RF
        } else if (i2 == "NuM_L_sp_cv_RF_"){
          err = summary(sp_cv_RF$error_fold)
          mRMSE = err["test.rmse", "mean"]
          NuM_L_RF = append(NuM_L_RF, mRMSE)
          ## NuM_L_sp_cv_RFSI
        } else if (i2 == "NuM_L_sp_cv_RFSI_"){
          err = summary(sp_cv_RFSI$error_fold)
          mRMSE = err["test.rmse", "mean"]
          NuM_L_RFSI = append(NuM_L_RFSI, mRMSE)
          ## NuM_L_sp_cv_RF_MEv
        } else if (i2 == "NuM_L_sp_cv_RF_MEv_"){
          err = summary(sp_cv_RF_MEv$error_fold)
          mRMSE = err["test.rmse", "mean"]
          NuM_L_RF_MEv = append(NuM_L_RF_MEv, mRMSE)
          ## NuM_L_sp_cv_OK_RF
        } else if (i2 == "NuM_L_sp_cv_OK_RF_"){
          err = summary(sp_cv_OK_RF$error_fold)
          mRMSE = err["test.rmse", "mean"]
          NuM_L_OK_RF = append(NuM_L_OK_RF, mRMSE)
          ## NuM_L_sp_cv_loo_OK_RF
        } else if (i2 == "NuM_L_sp_cv_loo_OK_RF_"){
          err = summary(sp_cv_loo_OK_RF$error_fold)
          mRMSE = err["test.rmse", "mean"]
          NuM_L_loo_OK_RF = append(NuM_L_loo_OK_RF, mRMSE)
          ## NuM_L_sp_cv_RF_oob_OK
        } else if (i2 == "NuM_L_sp_cv_RF_oob_OK_"){
          err = summary(sp_cv_RF_oob_OK$error_fold)
          mRMSE = err["test.rmse", "mean"]
          NuM_L_RF_oob_OK = append(NuM_L_RF_oob_OK, mRMSE)
          ## NuM_L_sp_cv_UK
        } else if (i2 == "NuM_L_sp_cv_UK_"){
          err = summary(sp_cv_UK$error_fold)
          mRMSE = err["test.rmse", "mean"]
          NuM_L_UK = append(NuM_L_UK, mRMSE)
          ## WuS_SuB_sp_cv_bRF
        } else if (i2 == "WuS_SuB_sp_cv_bRF_"){
          # RMSE
          err = summary(sp_cv_bRF$error_fold)
          mRMSE = err["test.rmse", "mean"]
          WuS_SuB_bRF = append(WuS_SuB_bRF, mRMSE)
          # nn distance
          nn_distances = c()
          for (i4 in 1:length(sp_cv_bRF$error_fold[[1]])){
            nn_dist = sp_cv_bRF$error_fold[[1]][[i4]]$distance
            nn_distances = append(nn_distances, nn_dist)
          }
          WuS_SuB_mean_nn_dist = append(WuS_SuB_mean_nn_dist, mean(nn_distances))
          ## WuS_SuB_sp_cv_MLR
        } else if (i2 == "WuS_SuB_sp_cv_MLR_"){
          err = summary(sp_cv_MLR$error_fold)
          mRMSE = err["test.rmse", "mean"]
          WuS_SuB_MLR = append(WuS_SuB_MLR, mRMSE)
          ## WuS_SuB_sp_cv_RF
        } else if (i2 == "WuS_SuB_sp_cv_RF_"){
          err = summary(sp_cv_RF$error_fold)
          mRMSE = err["test.rmse", "mean"]
          WuS_SuB_RF = append(WuS_SuB_RF, mRMSE)
          ## WuS_SuB_sp_cv_RFSI
        } else if (i2 == "WuS_SuB_sp_cv_RFSI_"){
          err = summary(sp_cv_RFSI$error_fold)
          mRMSE = err["test.rmse", "mean"]
          WuS_SuB_RFSI = append(WuS_SuB_RFSI, mRMSE)
          ## WuS_SuB_sp_cv_RF_MEv
        } else if (i2 == "WuS_SuB_sp_cv_RF_MEv_"){
          err = summary(sp_cv_RF_MEv$error_fold)
          mRMSE = err["test.rmse", "mean"]
          WuS_SuB_RF_MEv = append(WuS_SuB_RF_MEv, mRMSE)
          ## WuS_SuB_sp_cv_OK_RF
        } else if (i2 == "WuS_SuB_sp_cv_OK_RF_"){
          err = summary(sp_cv_OK_RF$error_fold)
          mRMSE = err["test.rmse", "mean"]
          WuS_SuB_OK_RF = append(WuS_SuB_OK_RF, mRMSE)
          ## WuS_SuB_sp_cv_loo_OK_RF
        } else if (i2 == "WuS_SuB_sp_cv_loo_OK_RF_"){
          err = summary(sp_cv_loo_OK_RF$error_fold)
          mRMSE = err["test.rmse", "mean"]
          WuS_SuB_loo_OK_RF = append(WuS_SuB_loo_OK_RF, mRMSE)
          ## WuS_SuB_sp_cv_RF_oob_OK
        } else if (i2 == "WuS_SuB_sp_cv_RF_oob_OK_"){
          err = summary(sp_cv_RF_oob_OK$error_fold)
          mRMSE = err["test.rmse", "mean"]
          WuS_SuB_RF_oob_OK = append(WuS_SuB_RF_oob_OK, mRMSE)
          ## WuS_SuB_sp_cv_UK
        } else if (i2 == "WuS_SuB_sp_cv_UK_"){
          err = summary(sp_cv_UK$error_fold)
          mRMSE = err["test.rmse", "mean"]
          WuS_SuB_UK = append(WuS_SuB_UK, mRMSE)
        }
        
    }
  }
}

# Create dfs
NuM_L_mRMSE_df = data.frame(NuM_L_RF_oob_OK, NuM_L_loo_OK_RF, 
                            NuM_L_RF_MEv, NuM_L_RFSI, NuM_L_RF, NuM_L_bRF,
                            NuM_L_MLR, NuM_L_OK_RF)

WuS_SuB_mRMSE_df = data.frame(WuS_SuB_RF_oob_OK, WuS_SuB_loo_OK_RF, 
                              WuS_SuB_RF_MEv, WuS_SuB_RFSI, WuS_SuB_RF, WuS_SuB_bRF,
                              WuS_SuB_MLR, WuS_SuB_OK_RF)
################################################################################
## End (non spatial loo CV)
################################################################################


################################################################################
## Spatial prediction error profile (SPEP)
################################################################################

####
## Save mean RMSE and bias values in vectors
##

# Create vectors
NuM_L_bRF = c()
NuM_L_MLR = c() 
NuM_L_RF = c()  
NuM_L_RFSI = c()
NuM_L_RF_MEv = c()  
NuM_L_OK_RF = c() 
NuM_L_loo_OK_RF = c()   
NuM_L_RF_oob_OK = c()
NuM_L_UK = c()
WuS_SuB_bRF = c()
WuS_SuB_MLR = c() 
WuS_SuB_RF = c()  
WuS_SuB_RFSI = c()
WuS_SuB_RF_MEv = c()  
WuS_SuB_OK_RF = c()  
WuS_SuB_loo_OK_RF = c()  
WuS_SuB_RF_oob_OK = c()
WuS_SuB_UK = c()
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

# Vectors for the mean distance to the nearest neighbor 
WuS_SuB_mean_nn_dist = c()
NuM_L_mean_nn_dist = c()

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
f_names_vec3 = c("0_+50_10.rda", "100_+50_10.rda", "400_+100_10.rda",
                 "900_+100_10.rda", "1600_+100_10.rda", "3600_+100_10.rda", 
                 "6400_+100_10.rda", "10000_+100_10.rda", "16900_+100_10.rda",
                 "25600_+100_10.rda", "40000_+100_10.rda") 

# Load results and store the RMSE values in the vectors
for (i1 in f_names_vec1){
  for (i2 in f_names_vec2){
    for (i3 in f_names_vec3){
      # Complete file name
      f_name = paste("Results/orgNitrate/",i1, i2, i3, sep = "")
      # Load file
      area_c1 = substr(i1, 1, 3)
      area_c2 = substr(i2, 1, 3)
      if (area_c1 == area_c2){
        load(f_name)
        ## Get mean RMSE and bias value and mean nearest neighbor distance
        ## NuM_L_sp_cv_bRF
        if (i2 == "NuM_L_sp_cv_bRF_"){
          # RMSE
          err = summary(sp_cv_bRF$error_fold)
          mRMSE = err["test.rmse", "mean"]
          NuM_L_bRF = append(NuM_L_bRF, mRMSE)
          # Bias
          mBias = err["test.bias", "mean"]
          NuM_L_bRF_b = append(NuM_L_bRF_b, mBias)
          # nn distance
          nn_distances = c()
          for (i4 in 1:length(sp_cv_bRF$error_fold[[1]])){
            nn_dist = sp_cv_bRF$error_fold[[1]][[i4]]$distance
            nn_distances = append(nn_distances, nn_dist)
          }
          NuM_L_mean_nn_dist = append(NuM_L_mean_nn_dist, mean(nn_distances))
        ## NuM_L_sp_cv_MLR
        } else if (i2 == "NuM_L_sp_cv_MLR_"){
          err = summary(sp_cv_MLR$error_fold)
          mRMSE = err["test.rmse", "mean"]
          NuM_L_MLR = append(NuM_L_MLR, mRMSE)
          mBias = err["test.bias", "mean"]
          NuM_L_MLR_b = append(NuM_L_MLR_b, mBias)
        ## NuM_L_sp_cv_RF
        } else if (i2 == "NuM_L_sp_cv_RF_"){
          err = summary(sp_cv_RF$error_fold)
          mRMSE = err["test.rmse", "mean"]
          NuM_L_RF = append(NuM_L_RF, mRMSE)
          mBias = err["test.bias", "mean"]
          NuM_L_RF_b = append(NuM_L_RF_b, mBias)
        ## NuM_L_sp_cv_RFSI
        } else if (i2 == "NuM_L_sp_cv_RFSI_"){
          err = summary(sp_cv_RFSI$error_fold)
          mRMSE = err["test.rmse", "mean"]
          NuM_L_RFSI = append(NuM_L_RFSI, mRMSE)
          mBias = err["test.bias", "mean"]
          NuM_L_RFSI_b = append(NuM_L_RFSI_b, mBias)
        ## NuM_L_sp_cv_RF_MEv
        } else if (i2 == "NuM_L_sp_cv_RF_MEv_"){
          err = summary(sp_cv_RF_MEv$error_fold)
          mRMSE = err["test.rmse", "mean"]
          NuM_L_RF_MEv = append(NuM_L_RF_MEv, mRMSE)
          mBias = err["test.bias", "mean"]
          NuM_L_RF_MEv_b = append(NuM_L_RF_MEv_b, mBias)
        ## NuM_L_sp_cv_OK_RF
        } else if (i2 == "NuM_L_sp_cv_OK_RF_"){
          err = summary(sp_cv_OK_RF$error_fold)
          mRMSE = err["test.rmse", "mean"]
          NuM_L_OK_RF = append(NuM_L_OK_RF, mRMSE)
          mBias = err["test.bias", "mean"]
          NuM_L_OK_RF_b = append(NuM_L_OK_RF_b, mBias)
        ## NuM_L_sp_cv_loo_OK_RF
        } else if (i2 == "NuM_L_sp_cv_loo_OK_RF_"){
          err = summary(sp_cv_loo_OK_RF$error_fold)
          mRMSE = err["test.rmse", "mean"]
          NuM_L_loo_OK_RF = append(NuM_L_loo_OK_RF, mRMSE)
          mBias = err["test.bias", "mean"]
          NuM_L_loo_OK_RF_b = append(NuM_L_loo_OK_RF_b, mBias)
        ## NuM_L_sp_cv_RF_oob_OK
        } else if (i2 == "NuM_L_sp_cv_RF_oob_OK_"){
          err = summary(sp_cv_RF_oob_OK$error_fold)
          mRMSE = err["test.rmse", "mean"]
          NuM_L_RF_oob_OK = append(NuM_L_RF_oob_OK, mRMSE)
          mBias = err["test.bias", "mean"]
          NuM_L_RF_oob_OK_b = append(NuM_L_RF_oob_OK_b, mBias)
        ## NuM_L_sp_cv_UK
        } else if (i2 == "NuM_L_sp_cv_UK_"){
          err = summary(sp_cv_UK$error_fold)
          mRMSE = err["test.rmse", "mean"]
          NuM_L_UK = append(NuM_L_UK, mRMSE)
          mBias = err["test.bias", "mean"]
          NuM_L_UK_b = append(NuM_L_UK_b, mBias)
         ## WuS_SuB_sp_cv_bRF
        } else if (i2 == "WuS_SuB_sp_cv_bRF_"){
          # RMSE
          err = summary(sp_cv_bRF$error_fold)
          mRMSE = err["test.rmse", "mean"]
          WuS_SuB_bRF = append(WuS_SuB_bRF, mRMSE)
          # Bias
          mBias = err["test.bias", "mean"]
          WuS_SuB_bRF_b = append(WuS_SuB_bRF_b, mBias)
          # nn distance
          nn_distances = c()
          for (i4 in 1:length(sp_cv_bRF$error_fold[[1]])){
            nn_dist = sp_cv_bRF$error_fold[[1]][[i4]]$distance
            nn_distances = append(nn_distances, nn_dist)
          }
          WuS_SuB_mean_nn_dist = append(WuS_SuB_mean_nn_dist, mean(nn_distances))
          ## WuS_SuB_sp_cv_MLR
        } else if (i2 == "WuS_SuB_sp_cv_MLR_"){
          err = summary(sp_cv_MLR$error_fold)
          mRMSE = err["test.rmse", "mean"]
          WuS_SuB_MLR = append(WuS_SuB_MLR, mRMSE)
          mBias = err["test.bias", "mean"]
          WuS_SuB_MLR_b = append(WuS_SuB_MLR_b, mBias)
          ## WuS_SuB_sp_cv_RF
        } else if (i2 == "WuS_SuB_sp_cv_RF_"){
          err = summary(sp_cv_RF$error_fold)
          mRMSE = err["test.rmse", "mean"]
          WuS_SuB_RF = append(WuS_SuB_RF, mRMSE)
          mBias = err["test.bias", "mean"]
          WuS_SuB_RF_b = append(WuS_SuB_RF_b, mBias)
          ## WuS_SuB_sp_cv_RFSI
        } else if (i2 == "WuS_SuB_sp_cv_RFSI_"){
          err = summary(sp_cv_RFSI$error_fold)
          mRMSE = err["test.rmse", "mean"]
          WuS_SuB_RFSI = append(WuS_SuB_RFSI, mRMSE)
          mBias = err["test.bias", "mean"]
          WuS_SuB_RFSI_b = append(WuS_SuB_RFSI_b, mBias)
          ## WuS_SuB_sp_cv_RF_MEv
        } else if (i2 == "WuS_SuB_sp_cv_RF_MEv_"){
          err = summary(sp_cv_RF_MEv$error_fold)
          mRMSE = err["test.rmse", "mean"]
          WuS_SuB_RF_MEv = append(WuS_SuB_RF_MEv, mRMSE)
          mBias = err["test.bias", "mean"]
          WuS_SuB_RF_MEv_b = append(WuS_SuB_RF_MEv_b, mBias)
          ## WuS_SuB_sp_cv_OK_RF
        } else if (i2 == "WuS_SuB_sp_cv_OK_RF_"){
          err = summary(sp_cv_OK_RF$error_fold)
          mRMSE = err["test.rmse", "mean"]
          WuS_SuB_OK_RF = append(WuS_SuB_OK_RF, mRMSE)
          mBias = err["test.bias", "mean"]
          WuS_SuB_OK_RF_b = append(WuS_SuB_OK_RF_b, mBias)
          ## WuS_SuB_sp_cv_loo_OK_RF
        } else if (i2 == "WuS_SuB_sp_cv_loo_OK_RF_"){
          err = summary(sp_cv_loo_OK_RF$error_fold)
          mRMSE = err["test.rmse", "mean"]
          WuS_SuB_loo_OK_RF = append(WuS_SuB_loo_OK_RF, mRMSE)
          mBias = err["test.bias", "mean"]
          WuS_SuB_loo_OK_RF_b = append(WuS_SuB_loo_OK_RF_b, mBias)
          ## WuS_SuB_sp_cv_RF_oob_OK
        } else if (i2 == "WuS_SuB_sp_cv_RF_oob_OK_"){
          err = summary(sp_cv_RF_oob_OK$error_fold)
          mRMSE = err["test.rmse", "mean"]
          WuS_SuB_RF_oob_OK = append(WuS_SuB_RF_oob_OK, mRMSE)
          mBias = err["test.bias", "mean"]
          WuS_SuB_RF_oob_OK_b = append(WuS_SuB_RF_oob_OK_b, mBias)
          ## WuS_SuB_sp_cv_UK
        } else if (i2 == "WuS_SuB_sp_cv_UK_"){
          err = summary(sp_cv_UK$error_fold)
          mRMSE = err["test.rmse", "mean"]
          WuS_SuB_UK = append(WuS_SuB_UK, mRMSE)
          mBias = err["test.bias", "mean"]
          WuS_SuB_UK_b = append(WuS_SuB_UK_b, mBias)
        }
        
      }
    }
  }
}
        
# Scale the nn dist vecs 
NuM_L_mean_sqrt_nn_dist = sqrt(NuM_L_mean_nn_dist)
WuS_SuB_mean_sqrt_nn_dist = sqrt(WuS_SuB_mean_nn_dist)        
##
## End (save mean RMSE and bias values in vectors)
####


####
## Plot SPEP
##

# Create dfs
NuM_L_mRMSE_df = data.frame(NuM_L_UK, NuM_L_RF_oob_OK, NuM_L_loo_OK_RF, 
                            NuM_L_RF_MEv, NuM_L_RFSI, NuM_L_RF, NuM_L_bRF,
                            NuM_L_MLR, NuM_L_OK_RF)

WuS_SuB_mRMSE_df = data.frame(WuS_SuB_UK, WuS_SuB_RF_oob_OK, WuS_SuB_loo_OK_RF, 
                            WuS_SuB_RF_MEv, WuS_SuB_RFSI, WuS_SuB_RF, WuS_SuB_bRF,
                            WuS_SuB_MLR, WuS_SuB_OK_RF)

# Positive bias = prediction value to low; negative bias prediction value to high
# a multiplication with -1 can make the interpretation easier 
NuM_L_mBias_df = data.frame(NuM_L_UK_b, NuM_L_RF_oob_OK_b, NuM_L_loo_OK_RF_b, 
                            NuM_L_RF_MEv_b, NuM_L_RFSI_b, NuM_L_RF_b, NuM_L_bRF_b,
                            NuM_L_MLR_b, NuM_L_OK_RF_b)

WuS_SuB_mBias_df = data.frame(WuS_SuB_UK_b, WuS_SuB_RF_oob_OK_b, WuS_SuB_loo_OK_RF_b, 
                              WuS_SuB_RF_MEv_b, WuS_SuB_RFSI_b, WuS_SuB_RF_b, WuS_SuB_bRF_b,
                              WuS_SuB_MLR_b, WuS_SuB_OK_RF_b)

# Save dfs
#save(NuM_L_mRMSE_df, WuS_SuB_mRMSE_df, file = "mRMSE_dfs_org.rda")
#save(NuM_L_mBias_df, WuS_SuB_mBias_df, file = "mBias_dfs_org.rda")

(NuM_L_mRMSE_df$NuM_L_RF_oob_OK - NuM_L_mRMSE_df$NuM_L_bRF) / NuM_L_mRMSE_df$NuM_L_bRF *100
(WuS_SuB_mRMSE_df$WuS_SuB_RF_oob_OK - WuS_SuB_mRMSE_df$WuS_SuB_bRF) / WuS_SuB_mRMSE_df$WuS_SuB_bRF *100

NmBdf = NuM_L_mBias_df*-1
NmBdf

aNmBdf = abs(NmBdf) 
(aNmBdf - aNmBdf$NuM_L_bRF_b) / aNmBdf$NuM_L_bRF_b * 100

WmBdf = WuS_SuB_mBias_df*-1
WmBdf

aWmBdf = abs(WmBdf) 
(aWmBdf - aWmBdf$WuS_SuB_bRF_b) / aWmBdf$WuS_SuB_bRF_b * 100
  
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
for (i in 1:ncol(NuM_L_mRMSE_df)){
  # Points
  points(x = NuM_L_mean_sqrt_nn_dist,
         y = NuM_L_mRMSE_df[,i],
         pch = 21,
         bg = col_vec[i])
  # Line
  lines(x = NuM_L_mean_sqrt_nn_dist,
        y = NuM_L_mRMSE_df[,i],
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
axis(1, at = c(0, 5, 11.18034, 21.21320, 30.82207, 40.62019, 60.41523, 
               80.31189, 100.24969, 130.19217, 160.15617, 200.12496), 
     labels = c(0, 25, 125, 450, 950, 1650, 3650, 6450, 10050, 16950, 25650, 
                40050))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot mRMSE values
for (i in 1:ncol(WuS_SuB_mRMSE_df)){
  # Points
  points(x = WuS_SuB_mean_sqrt_nn_dist,
         y = WuS_SuB_mRMSE_df[,i],
         pch = 21,
         bg = col_vec[i])
  # Line
  lines(x = WuS_SuB_mean_sqrt_nn_dist,
        y = WuS_SuB_mRMSE_df[,i],
        col = col_vec[i],
        lty = 2, lwd = 2)
}

# Legend
legend(x = "bottomright", cex = 1, ncol = 2,
       legend = c("MLR", "UK", "RF", "RF-K", "RF-MEv", "RF-oob-OK", "OK-RF", 
                  "RF-loo-OK", "RFSI"),
       fill = c("#000000", "#666666", "#CCCCCC", "#33CC33", "#FF0000", "#9900FF",
                "#66FFFF", "#0000FF", "#FF9933"))

legend(x = "bottomleft" , legend = c("b)"), bty = "n", cex = 1.25)

## Create difference to lowest RMSE dfs
# Create vector for the values
lR_vec_NuM_L = c()
lR_vec_WuS_SuB = c()
# For loop
for(i in 1:nrow(WuS_SuB_mRMSE_df)){
  lR_vec_WuS_SuB = append(lR_vec_WuS_SuB, min(WuS_SuB_mRMSE_df[i,]))
}
for(i in 1:nrow(NuM_L_mRMSE_df)){
  lR_vec_NuM_L = append(lR_vec_NuM_L, min(NuM_L_mRMSE_df[i,]))
}
# Create dfs
Diff_to_lR_NuM_L = NuM_L_mRMSE_df - lR_vec_NuM_L
Diff_to_lR_WuS_SuB = WuS_SuB_mRMSE_df - lR_vec_WuS_SuB

## Plot results
# First plot
par(mar = c(2, 0, 0, 0)) # bottom, left, top, right margins
plot(NULL, ylab = "", bty = "n", 
     xlim = c(0, 0.1), ylim = c(0, 0.1), xaxt = "n", yaxt = "n")
mtext(expression("Differenz zum niedrigsten mittleren RMSE [mg/L]"),
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
     ylim = c(0, 25),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 2.236068, 10.95445, 21.21320, 30.82207, 40.62019, 60.41523, 
               80.31189, 100.24969, 130.19217, 160.15617, 200.12496), 
     labels = c(0, 5, 120, 450, 950, 1650, 3650, 6450, 10050, 16950, 25650, 
                40050))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot mRMSE values
for (i in 1:ncol(Diff_to_lR_NuM_L)){
  # Points
  points(x = NuM_L_mean_sqrt_nn_dist,
         y = Diff_to_lR_NuM_L[,i],
         pch = 21,
         bg = col_vec[i])
  # Line
  lines(x = NuM_L_mean_sqrt_nn_dist,
        y = Diff_to_lR_NuM_L[,i],
        col = col_vec[i],
        lty = 2, lwd = 2)
}

legend(x = "topright" , legend = c("a)"), bty = "n", cex = 1.25)

## WuS_SuB
# Margins
par(mar = c(4, 0, 0, 1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1,                 
     xlab = "Mittlere Vorhersagedistanz [m]", 
     #ylab = "",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(0, 6),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 5, 11.18034, 21.21320, 30.82207, 40.62019, 60.41523, 
               80.31189, 100.24969, 130.19217, 160.15617, 200.12496), 
     labels = c(0, 25, 125, 450, 950, 1650, 3650, 6450, 10050, 16950, 25650, 
                40050))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot mRMSE values
for (i in 1:ncol(Diff_to_lR_WuS_SuB)){
  # Points
  points(x = WuS_SuB_mean_sqrt_nn_dist,
         y = Diff_to_lR_WuS_SuB[,i],
         pch = 21,
         bg = col_vec[i])
  # Line
  lines(x = WuS_SuB_mean_sqrt_nn_dist,
        y = Diff_to_lR_WuS_SuB[,i],
        col = col_vec[i],
        lty = 2, lwd = 2)
}

# Legend
legend(x = "top", cex = 1, ncol = 2,
       legend = c("MLR", "UK", "RF", "RF-K", "RF-MEv", "RF-oob-OK", "OK-RF", 
                  "RF-loo-OK", "RFSI"),
       fill = c("#000000", "#666666", "#CCCCCC", "#33CC33", "#FF0000", "#9900FF",
                "#66FFFF", "#0000FF", "#FF9933"))

legend(x = "topright" , legend = c("b)"), bty = "n", cex = 1.25)

##
## End (plot SPEP)
####
################################################################################
## End (SPEP)
################################################################################


################################################################################
## Spatial variable importance profiles (SVIPs) 
################################################################################

####
## Get information about the relative importance of the variable/
## get the four most important variables per distance 
##

# Create vectors for the results 
NuM_L_bRF_v = c()
NuM_L_MLR_v = c() 
NuM_L_RF_v = c()  
NuM_L_RFSI_v = c()
NuM_L_RF_MEv_v = c()  
NuM_L_OK_RF_v = c() 
NuM_L_loo_OK_RF_v = c()   
NuM_L_RF_oob_OK_v = c()
NuM_L_UK_v = c()
WuS_SuB_bRF_v = c()
WuS_SuB_MLR_v = c() 
WuS_SuB_RF_v = c()  
WuS_SuB_RFSI_v = c()
WuS_SuB_RF_MEv_v = c()  
WuS_SuB_OK_RF_v = c()  
WuS_SuB_loo_OK_RF_v = c()  
WuS_SuB_RF_oob_OK_v = c()
WuS_SuB_UK_v = c()

NuM_L_over_v = c()
WuS_SuB_over_v = c()

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
f_names_vec3 = c("0_+50_10.rda", "100_+50_10.rda", "400_+100_10.rda",
                 "900_+100_10.rda", "1600_+100_10.rda", "3600_+100_10.rda", 
                 "6400_+100_10.rda", "10000_+100_10.rda", "16900_+100_10.rda",
                 "25600_+100_10.rda", "40000_+100_10.rda") 

# Load results and store the RMSE values in the vectors
for (i1 in f_names_vec1){
  for (i2 in f_names_vec2){
    for (i3 in f_names_vec3){
      # Complete file name
      f_name = paste("Results/orgNitrate/",i1, i2, i3, sep = "")
      # Load file
      area_c1 = substr(i1, 1, 3)
      area_c2 = substr(i2, 1, 3)
      if (area_c1 == area_c2){
        load(f_name)
        ## NuM_L_sp_cv_bRF
        if (i2 == "NuM_L_sp_cv_bRF_"){
          # Get the four most important variables per distance 
          imp <- summary(sp_cv_bRF$importance)
          sorted_df = imp[order(imp$mean.rmse),]
          sorted_rN = rownames(sorted_df)
          one_four = sorted_rN[1:4]
          NuM_L_bRF_v = append(NuM_L_bRF_v, one_four)
          NuM_L_over_v = append(NuM_L_over_v, one_four)
          # Get the names of all variables
          Var_names = sorted_rN
          ## NuM_L_sp_cv_MLR
        } else if (i2 == "NuM_L_sp_cv_MLR_"){
          imp <- summary(sp_cv_MLR$importance)
          sorted_df = imp[order(imp$mean.rmse),]
          sorted_rN = rownames(sorted_df)
          one_four = sorted_rN[1:4]
          NuM_L_MLR_v = append(NuM_L_MLR_v, one_four)
          NuM_L_over_v = append(NuM_L_over_v, one_four)
          ## NuM_L_sp_cv_RF
        } else if (i2 == "NuM_L_sp_cv_RF_"){
          imp <- summary(sp_cv_RF$importance)
          sorted_df = imp[order(imp$mean.rmse),]
          sorted_rN = rownames(sorted_df)
          one_four = sorted_rN[1:4]
          NuM_L_RF_v = append(NuM_L_RF_v, one_four)
          NuM_L_over_v = append(NuM_L_over_v, one_four)
          ## NuM_L_sp_cv_RFSI
        } else if (i2 == "NuM_L_sp_cv_RFSI_"){
          imp <- summary(sp_cv_RFSI$importance)
          sorted_df = imp[order(imp$mean.rmse),]
          sorted_rN = rownames(sorted_df)
          one_four = sorted_rN[1:4]
          NuM_L_RFSI_v = append(NuM_L_RFSI_v, one_four)
          NuM_L_over_v = append(NuM_L_over_v, one_four)
          ## NuM_L_sp_cv_RF_MEv
        } else if (i2 == "NuM_L_sp_cv_RF_MEv_"){
          imp <- summary(sp_cv_RF_MEv$importance)
          sorted_df = imp[order(imp$mean.rmse),]
          sorted_rN = rownames(sorted_df)
          one_four = sorted_rN[1:4]
          NuM_L_RF_MEv_v = append(NuM_L_RF_MEv_v, one_four)
          NuM_L_over_v = append(NuM_L_over_v, one_four)
          ## NuM_L_sp_cv_OK_RF
        } else if (i2 == "NuM_L_sp_cv_OK_RF_"){
          imp <- summary(sp_cv_OK_RF$importance)
          sorted_df = imp[order(imp$mean.rmse),]
          sorted_rN = rownames(sorted_df)
          one_four = sorted_rN[1:4]
          NuM_L_OK_RF_v = append(NuM_L_OK_RF_v, one_four)
          NuM_L_over_v = append(NuM_L_over_v, one_four)
          ## NuM_L_sp_cv_loo_OK_RF
        } else if (i2 == "NuM_L_sp_cv_loo_OK_RF_"){
          imp <- summary(sp_cv_loo_OK_RF$importance)
          sorted_df = imp[order(imp$mean.rmse),]
          sorted_rN = rownames(sorted_df)
          one_four = sorted_rN[1:4]
          NuM_L_loo_OK_RF_v = append(NuM_L_loo_OK_RF_v, one_four)
          NuM_L_over_v = append(NuM_L_over_v, one_four)
          ## NuM_L_sp_cv_RF_oob_OK
        } else if (i2 == "NuM_L_sp_cv_RF_oob_OK_"){
          imp <- summary(sp_cv_RF_oob_OK$importance)
          sorted_df = imp[order(imp$mean.rmse),]
          sorted_rN = rownames(sorted_df)
          one_four = sorted_rN[1:4]
          NuM_L_RF_oob_OK_v = append(NuM_L_RF_oob_OK_v, one_four)
          NuM_L_over_v = append(NuM_L_over_v, one_four)
          ## NuM_L_sp_cv_UK
        } else if (i2 == "NuM_L_sp_cv_UK_"){
          imp <- summary(sp_cv_UK$importance)
          sorted_df = imp[order(imp$mean.rmse),]
          sorted_rN = rownames(sorted_df)
          one_four = sorted_rN[1:4]
          NuM_L_UK_v = append(NuM_L_UK_v, one_four)
          NuM_L_over_v = append(NuM_L_over_v, one_four)
          ## WuS_SuB_sp_cv_bRF
        } else if (i2 == "WuS_SuB_sp_cv_bRF_"){
          imp <- summary(sp_cv_bRF$importance)
          sorted_df = imp[order(imp$mean.rmse),]
          sorted_rN = rownames(sorted_df)
          one_four = sorted_rN[1:4]
          WuS_SuB_bRF_v = append(WuS_SuB_bRF_v, one_four)
          WuS_SuB_over_v = append(WuS_SuB_over_v, one_four)
          ## WuS_SuB_sp_cv_MLR
        } else if (i2 == "WuS_SuB_sp_cv_MLR_"){
          imp <- summary(sp_cv_MLR$importance)
          sorted_df = imp[order(imp$mean.rmse),]
          sorted_rN = rownames(sorted_df)
          one_four = sorted_rN[1:4]
          WuS_SuB_MLR_v = append(WuS_SuB_MLR_v, one_four)
          WuS_SuB_over_v = append(WuS_SuB_over_v, one_four)
          ## WuS_SuB_sp_cv_RF
        } else if (i2 == "WuS_SuB_sp_cv_RF_"){
          imp <- summary(sp_cv_RF$importance)
          sorted_df = imp[order(imp$mean.rmse),]
          sorted_rN = rownames(sorted_df)
          one_four = sorted_rN[1:4]
          WuS_SuB_RF_v = append(WuS_SuB_RF_v, one_four)
          WuS_SuB_over_v = append(WuS_SuB_over_v, one_four)
          ## WuS_SuB_sp_cv_RFSI
        } else if (i2 == "WuS_SuB_sp_cv_RFSI_"){
          imp <- summary(sp_cv_RFSI$importance)
          sorted_df = imp[order(imp$mean.rmse),]
          sorted_rN = rownames(sorted_df)
          one_four = sorted_rN[1:4]
          WuS_SuB_RFSI_v = append(WuS_SuB_RFSI_v, one_four)
          WuS_SuB_over_v = append(WuS_SuB_over_v, one_four)
          ## WuS_SuB_sp_cv_RF_MEv
        } else if (i2 == "WuS_SuB_sp_cv_RF_MEv_"){
          imp <- summary(sp_cv_RF_MEv$importance)
          sorted_df = imp[order(imp$mean.rmse),]
          sorted_rN = rownames(sorted_df)
          one_four = sorted_rN[1:4]
          WuS_SuB_RF_MEv_v = append(WuS_SuB_RF_MEv_v, one_four)
          WuS_SuB_over_v = append(WuS_SuB_over_v, one_four)
          ## WuS_SuB_sp_cv_OK_RF
        } else if (i2 == "WuS_SuB_sp_cv_OK_RF_"){
          imp <- summary(sp_cv_OK_RF$importance)
          sorted_df = imp[order(imp$mean.rmse),]
          sorted_rN = rownames(sorted_df)
          one_four = sorted_rN[1:4]
          WuS_SuB_OK_RF_v = append(WuS_SuB_OK_RF_v, one_four)
          WuS_SuB_over_v = append(WuS_SuB_over_v, one_four)
          ## WuS_SuB_sp_cv_loo_OK_RF
        } else if (i2 == "WuS_SuB_sp_cv_loo_OK_RF_"){
          imp <- summary(sp_cv_loo_OK_RF$importance)
          sorted_df = imp[order(imp$mean.rmse),]
          sorted_rN = rownames(sorted_df)
          one_four = sorted_rN[1:4]
          WuS_SuB_loo_OK_RF_v = append(WuS_SuB_loo_OK_RF_v, one_four)
          WuS_SuB_over_v = append(WuS_SuB_over_v, one_four)
          ## WuS_SuB_sp_cv_RF_oob_OK
        } else if (i2 == "WuS_SuB_sp_cv_RF_oob_OK_"){
          imp <- summary(sp_cv_RF_oob_OK$importance)
          sorted_df = imp[order(imp$mean.rmse),]
          sorted_rN = rownames(sorted_df)
          one_four = sorted_rN[1:4]
          WuS_SuB_RF_oob_OK_v = append(WuS_SuB_RF_oob_OK_v, one_four)
          WuS_SuB_over_v = append(WuS_SuB_over_v, one_four)
          ## WuS_SuB_sp_cv_UK
        } else if (i2 == "WuS_SuB_sp_cv_UK_"){
          imp <- summary(sp_cv_UK$importance)
          sorted_df = imp[order(imp$mean.rmse),]
          sorted_rN = rownames(sorted_df)
          one_four = sorted_rN[1:4]
          WuS_SuB_UK_v = append(WuS_SuB_UK_v, one_four)
          WuS_SuB_over_v = append(WuS_SuB_over_v, one_four)
        }
        
      }
    }
  }
}


# Get unique values and frequency 
NuM_L_bRF_table = sort(table(NuM_L_bRF_v))
NuM_L_MLR_table = sort(table(NuM_L_MLR_v))
NuM_L_RF_table = sort(table(NuM_L_RF_v))  
NuM_L_RFSI_table = sort(table(NuM_L_RFSI_v))
NuM_L_RF_MEv_table = sort(table(NuM_L_RF_MEv_v))  
NuM_L_OK_RF_table = sort(table(NuM_L_OK_RF_v)) 
NuM_L_loo_OK_RF_table = sort(table(NuM_L_loo_OK_RF_v))   
NuM_L_RF_oob_OK_table = sort(table(NuM_L_RF_oob_OK_v))
NuM_L_UK_table = sort(table(NuM_L_UK_v))
WuS_SuB_bRF_table = sort(table(WuS_SuB_bRF_v))
WuS_SuB_MLR_table = sort(table(WuS_SuB_MLR_v)) 
WuS_SuB_RF_table = sort(table(WuS_SuB_RF_v))  
WuS_SuB_RFSI_table = sort(table(WuS_SuB_RFSI_v))
WuS_SuB_RF_MEv_table = sort(table(WuS_SuB_RF_MEv_v))  
WuS_SuB_OK_RF_table = sort(table(WuS_SuB_OK_RF_v))  
WuS_SuB_loo_OK_RF_table = sort(table(WuS_SuB_loo_OK_RF_v))  
WuS_SuB_RF_oob_OK_table = sort(table(WuS_SuB_RF_oob_OK_v))
WuS_SuB_UK_table = sort(table(WuS_SuB_UK_v))

NuM_L_over_table = sort(table(NuM_L_over_v))
WuS_SuB_over_table = sort(table(WuS_SuB_over_v))

##
## End (get information about the relative importance of the variables) 
####


####
## Store the importance/RMSE difference values in vectors
##

# cAckerland
NuM_L_cA_MLR_vec = c()
NuM_L_cA_bRF_vec = c()
NuM_L_cA_RF_vec = c() 
NuM_L_cA_RFSI_vec = c()
NuM_L_cA_RF_MEv_vec = c()
NuM_L_cA_OK_RF_vec = c()
NuM_L_cA_loo_OK_RF_vec = c()
NuM_L_cA_RF_oob_OK_vec = c()
NuM_L_cA_UK_vec = c()
WuS_SuB_cA_MLR_vec = c()
WuS_SuB_cA_bRF_vec = c()
WuS_SuB_cA_RF_vec = c() 
WuS_SuB_cA_RFSI_vec = c()
WuS_SuB_cA_RF_MEv_vec = c()
WuS_SuB_cA_OK_RF_vec = c()
WuS_SuB_cA_loo_OK_RF_vec = c()
WuS_SuB_cA_RF_oob_OK_vec = c()
WuS_SuB_cA_UK_vec = c()

# X
NuM_L_X_MLR_vec = c()
NuM_L_X_RF_vec = c() 
NuM_L_X_RFSI_vec = c()
NuM_L_X_RF_MEv_vec = c()
NuM_L_X_OK_RF_vec = c()
NuM_L_X_loo_OK_RF_vec = c()
NuM_L_X_RF_oob_OK_vec = c()
NuM_L_X_UK_vec = c()
WuS_SuB_X_MLR_vec = c()
WuS_SuB_X_bRF_vec = c()
WuS_SuB_X_RF_vec = c() 
WuS_SuB_X_RFSI_vec = c()
WuS_SuB_X_RF_MEv_vec = c()
WuS_SuB_X_OK_RF_vec = c()
WuS_SuB_X_loo_OK_RF_vec = c()
WuS_SuB_X_RF_oob_OK_vec = c()
WuS_SuB_X_UK_vec = c()

# Y
NuM_L_Y_MLR_vec = c()
NuM_L_Y_RF_vec = c() 
NuM_L_Y_RFSI_vec = c()
NuM_L_Y_RF_MEv_vec = c()
NuM_L_Y_OK_RF_vec = c()
NuM_L_Y_loo_OK_RF_vec = c()
NuM_L_Y_RF_oob_OK_vec = c()
NuM_L_Y_UK_vec = c()
WuS_SuB_Y_MLR_vec = c()
WuS_SuB_Y_bRF_vec = c()
WuS_SuB_Y_RF_vec = c() 
WuS_SuB_Y_RFSI_vec = c()
WuS_SuB_Y_RF_MEv_vec = c()
WuS_SuB_Y_OK_RF_vec = c()
WuS_SuB_Y_loo_OK_RF_vec = c()
WuS_SuB_Y_RF_oob_OK_vec = c()
WuS_SuB_Y_UK_vec = c()

# tc45
NuM_L_tc45_RF_vec = c() 
NuM_L_tc45_RFSI_vec = c()
NuM_L_tc45_RF_MEv_vec = c()
NuM_L_tc45_OK_RF_vec = c()
NuM_L_tc45_loo_OK_RF_vec = c()
NuM_L_tc45_RF_oob_OK_vec = c()

# tc315
WuS_SuB_tc315_RF_vec = c() 
WuS_SuB_tc315_RFSI_vec = c()
WuS_SuB_tc315_RF_MEv_vec = c()
WuS_SuB_tc315_OK_RF_vec = c()
WuS_SuB_tc315_loo_OK_RF_vec = c()
WuS_SuB_tc315_RF_oob_OK_vec = c()

# Vectors for the mean distance to the nearest neighbor 
WuS_SuB_mean_nn_dist = c()
NuM_L_mean_nn_dist = c()

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
f_names_vec3 = c("0_+50_10.rda", "100_+50_10.rda", "400_+100_10.rda",
                 "900_+100_10.rda", "1600_+100_10.rda", "3600_+100_10.rda", 
                 "6400_+100_10.rda", "10000_+100_10.rda", "16900_+100_10.rda",
                 "25600_+100_10.rda", "40000_+100_10.rda") 

# Load results and store the RMSE values in the vectors
for (i1 in f_names_vec1){
  for (i2 in f_names_vec2){
    for (i3 in f_names_vec3){
      # Complete file name
      f_name = paste("Results/orgNitrate/",i1, i2, i3, sep = "")
      # Load file
      area_c1 = substr(i1, 1, 3)
      area_c2 = substr(i2, 1, 3)
      if (area_c1 == area_c2){
        load(f_name)
        ## Get mean importance and mean nearest neighbor distance
        ## NuM_L_sp_cv_bRF
        if (i2 == "NuM_L_sp_cv_bRF_"){
          # Importance
          imp_bRF = summary(sp_cv_bRF$importance)
          # Convert decrease to increase
          imp_bRF$mean.rmse = imp_bRF$mean.rmse * -1
          # Imp values of the variables  
          NuM_L_cA_bRF_vec = append(NuM_L_cA_bRF_vec, imp_bRF["cAckerland", "mean.rmse"])
          # nn distance
          nn_distances = c()
          for (i4 in 1:length(sp_cv_bRF$error_fold[[1]])){
            nn_dist = sp_cv_bRF$error_fold[[1]][[i4]]$distance
            nn_distances = append(nn_distances, nn_dist)
          }
          NuM_L_mean_nn_dist = append(NuM_L_mean_nn_dist, mean(nn_distances))
          ## NuM_L_sp_cv_MLR
        } else if (i2 == "NuM_L_sp_cv_MLR_"){
          imp_MLR = summary(sp_cv_MLR$importance)
          imp_MLR$mean.rmse = imp_MLR$mean.rmse * -1
          NuM_L_cA_MLR_vec = append(NuM_L_cA_MLR_vec, imp_MLR["cAckerland", "mean.rmse"])
          NuM_L_X_MLR_vec = append(NuM_L_X_MLR_vec, imp_MLR["X", "mean.rmse"])
          NuM_L_Y_MLR_vec = append(NuM_L_Y_MLR_vec, imp_MLR["Y", "mean.rmse"])
          ## NuM_L_sp_cv_RF
        } else if (i2 == "NuM_L_sp_cv_RF_"){
          imp_RF = summary(sp_cv_RF$importance)
          imp_RF$mean.rmse = imp_RF$mean.rmse * -1
          NuM_L_cA_RF_vec = append(NuM_L_cA_RF_vec, imp_RF["cAckerland", "mean.rmse"])
          NuM_L_X_RF_vec = append(NuM_L_X_RF_vec, imp_RF["X", "mean.rmse"])
          NuM_L_Y_RF_vec = append(NuM_L_Y_RF_vec, imp_RF["Y", "mean.rmse"])
          NuM_L_tc45_RF_vec = append(NuM_L_tc45_RF_vec, imp_RF["tc45", "mean.rmse"])
          ## NuM_L_sp_cv_RFSI
        } else if (i2 == "NuM_L_sp_cv_RFSI_"){
          imp_RFSI = summary(sp_cv_RFSI$importance)
          imp_RFSI$mean.rmse = imp_RFSI$mean.rmse * -1
          NuM_L_cA_RFSI_vec = append(NuM_L_cA_RFSI_vec, imp_RFSI["cAckerland", "mean.rmse"])
          NuM_L_X_RFSI_vec = append(NuM_L_X_RFSI_vec, imp_RFSI["X", "mean.rmse"])
          NuM_L_Y_RFSI_vec = append(NuM_L_Y_RFSI_vec, imp_RFSI["Y", "mean.rmse"])
          NuM_L_tc45_RFSI_vec = append(NuM_L_tc45_RFSI_vec, imp_RFSI["tc45", "mean.rmse"])
          ## NuM_L_sp_cv_RF_MEv
        } else if (i2 == "NuM_L_sp_cv_RF_MEv_"){
          imp_RF_MEv = summary(sp_cv_RF_MEv$importance)
          imp_RF_MEv$mean.rmse = imp_RF_MEv$mean.rmse * -1
          NuM_L_cA_RF_MEv_vec = append(NuM_L_cA_RF_MEv_vec, imp_RF_MEv["cAckerland", "mean.rmse"])
          NuM_L_X_RF_MEv_vec = append(NuM_L_X_RF_MEv_vec, imp_RF_MEv["X", "mean.rmse"])
          NuM_L_Y_RF_MEv_vec = append(NuM_L_Y_RF_MEv_vec, imp_RF_MEv["Y", "mean.rmse"])
          NuM_L_tc45_RF_MEv_vec = append(NuM_L_tc45_RF_MEv_vec, imp_RF_MEv["tc45", "mean.rmse"])
          ## NuM_L_sp_cv_OK_RF
        } else if (i2 == "NuM_L_sp_cv_OK_RF_"){
          imp_OK_RF = summary(sp_cv_OK_RF$importance)
          imp_OK_RF$mean.rmse = imp_OK_RF$mean.rmse * -1
          NuM_L_cA_OK_RF_vec = append(NuM_L_cA_OK_RF_vec, imp_OK_RF["cAckerland", "mean.rmse"])
          NuM_L_X_OK_RF_vec = append(NuM_L_X_OK_RF_vec, imp_OK_RF["X", "mean.rmse"])
          NuM_L_Y_OK_RF_vec = append(NuM_L_Y_OK_RF_vec, imp_OK_RF["Y", "mean.rmse"])
          NuM_L_tc45_OK_RF_vec = append(NuM_L_tc45_OK_RF_vec, imp_OK_RF["tc45", "mean.rmse"])
          ## NuM_L_sp_cv_loo_OK_RF
        } else if (i2 == "NuM_L_sp_cv_loo_OK_RF_"){
          imp_loo_OK_RF = summary(sp_cv_loo_OK_RF$importance)
          imp_loo_OK_RF$mean.rmse = imp_loo_OK_RF$mean.rmse * -1
          NuM_L_cA_loo_OK_RF_vec = append(NuM_L_cA_loo_OK_RF_vec, imp_loo_OK_RF["cAckerland", "mean.rmse"])
          NuM_L_X_loo_OK_RF_vec = append(NuM_L_X_loo_OK_RF_vec, imp_loo_OK_RF["X", "mean.rmse"])
          NuM_L_Y_loo_OK_RF_vec = append(NuM_L_Y_loo_OK_RF_vec, imp_loo_OK_RF["Y", "mean.rmse"])
          NuM_L_tc45_loo_OK_RF_vec = append(NuM_L_tc45_loo_OK_RF_vec, imp_loo_OK_RF["tc45", "mean.rmse"])
          ## NuM_L_sp_cv_RF_oob_OK
        } else if (i2 == "NuM_L_sp_cv_RF_oob_OK_"){
          imp_RF_oob_OK = summary(sp_cv_RF_oob_OK$importance)
          imp_RF_oob_OK$mean.rmse = imp_RF_oob_OK$mean.rmse * -1
          NuM_L_cA_RF_oob_OK_vec = append(NuM_L_cA_RF_oob_OK_vec, imp_RF_oob_OK["cAckerland", "mean.rmse"])
          NuM_L_X_RF_oob_OK_vec = append(NuM_L_X_RF_oob_OK_vec, imp_RF_oob_OK["X", "mean.rmse"])
          NuM_L_Y_RF_oob_OK_vec = append(NuM_L_Y_RF_oob_OK_vec, imp_RF_oob_OK["Y", "mean.rmse"])
          NuM_L_tc45_RF_oob_OK_vec = append(NuM_L_tc45_RF_oob_OK_vec, imp_RF_oob_OK["tc45", "mean.rmse"])
          ## NuM_L_sp_cv_UK
        } else if (i2 == "NuM_L_sp_cv_UK_"){
          imp_UK = summary(sp_cv_UK$importance)
          imp_UK$mean.rmse = imp_UK$mean.rmse * -1
          NuM_L_cA_UK_vec = append(NuM_L_cA_UK_vec, imp_UK["cAckerland", "mean.rmse"])
          NuM_L_X_UK_vec = append(NuM_L_X_UK_vec, imp_UK["X", "mean.rmse"])
          NuM_L_Y_UK_vec = append(NuM_L_Y_UK_vec, imp_UK["Y", "mean.rmse"])
          ## WuS_SuB_sp_cv_bRF
        } else if (i2 == "WuS_SuB_sp_cv_bRF_"){
          imp_bRF = summary(sp_cv_bRF$importance)
          imp_bRF$mean.rmse = imp_bRF$mean.rmse * -1
          WuS_SuB_cA_bRF_vec = append(WuS_SuB_cA_bRF_vec, imp_bRF["cAckerland", "mean.rmse"])
          nn_distances = c()
          for (i4 in 1:length(sp_cv_bRF$error_fold[[1]])){
            nn_dist = sp_cv_bRF$error_fold[[1]][[i4]]$distance
            nn_distances = append(nn_distances, nn_dist)
          }
          WuS_SuB_mean_nn_dist = append(WuS_SuB_mean_nn_dist, mean(nn_distances))
          ## WuS_SuB_sp_cv_MLR
        } else if (i2 == "WuS_SuB_sp_cv_MLR_"){
          imp_MLR = summary(sp_cv_MLR$importance)
          imp_MLR$mean.rmse = imp_MLR$mean.rmse * -1
          WuS_SuB_cA_MLR_vec = append(WuS_SuB_cA_MLR_vec, imp_MLR["cAckerland", "mean.rmse"])
          WuS_SuB_X_MLR_vec = append(WuS_SuB_X_MLR_vec, imp_MLR["X", "mean.rmse"])
          WuS_SuB_Y_MLR_vec = append(WuS_SuB_Y_MLR_vec, imp_MLR["Y", "mean.rmse"])
          ## WuS_SuB_sp_cv_RF
        } else if (i2 == "WuS_SuB_sp_cv_RF_"){
          imp_RF = summary(sp_cv_RF$importance)
          imp_RF$mean.rmse = imp_RF$mean.rmse * -1
          WuS_SuB_cA_RF_vec = append(WuS_SuB_cA_RF_vec, imp_RF["cAckerland", "mean.rmse"])
          WuS_SuB_X_RF_vec = append(WuS_SuB_X_RF_vec, imp_RF["X", "mean.rmse"])
          WuS_SuB_Y_RF_vec = append(WuS_SuB_Y_RF_vec, imp_RF["Y", "mean.rmse"])
          WuS_SuB_tc315_RF_vec = append(WuS_SuB_tc315_RF_vec, imp_RF["tc315", "mean.rmse"])
          ## WuS_SuB_sp_cv_RFSI
        } else if (i2 == "WuS_SuB_sp_cv_RFSI_"){
          imp_RFSI = summary(sp_cv_RFSI$importance)
          imp_RFSI$mean.rmse = imp_RFSI$mean.rmse * -1
          WuS_SuB_cA_RFSI_vec = append(WuS_SuB_cA_RFSI_vec, imp_RFSI["cAckerland", "mean.rmse"])
          WuS_SuB_X_RFSI_vec = append(WuS_SuB_X_RFSI_vec, imp_RFSI["X", "mean.rmse"])
          WuS_SuB_Y_RFSI_vec = append(WuS_SuB_Y_RFSI_vec, imp_RFSI["Y", "mean.rmse"])
          WuS_SuB_tc315_RFSI_vec = append(WuS_SuB_tc315_RFSI_vec, imp_RFSI["tc315", "mean.rmse"])
          ## WuS_SuB_sp_cv_RF_MEv
        } else if (i2 == "WuS_SuB_sp_cv_RF_MEv_"){
          imp_RF_MEv = summary(sp_cv_RF_MEv$importance)
          imp_RF_MEv$mean.rmse = imp_RF_MEv$mean.rmse * -1
          WuS_SuB_cA_RF_MEv_vec = append(WuS_SuB_cA_RF_MEv_vec, imp_RF_MEv["cAckerland", "mean.rmse"])
          WuS_SuB_X_RF_MEv_vec = append(WuS_SuB_X_RF_MEv_vec, imp_RF_MEv["X", "mean.rmse"])
          WuS_SuB_Y_RF_MEv_vec = append(WuS_SuB_Y_RF_MEv_vec, imp_RF_MEv["Y", "mean.rmse"])
          WuS_SuB_tc315_RF_MEv_vec = append(WuS_SuB_tc315_RF_MEv_vec, imp_RF_MEv["tc315", "mean.rmse"])
          ## WuS_SuB_sp_cv_OK_RF
        } else if (i2 == "WuS_SuB_sp_cv_OK_RF_"){
          imp_OK_RF = summary(sp_cv_OK_RF$importance)
          imp_OK_RF$mean.rmse = imp_OK_RF$mean.rmse * -1
          WuS_SuB_cA_OK_RF_vec = append(WuS_SuB_cA_OK_RF_vec, imp_OK_RF["cAckerland", "mean.rmse"])
          WuS_SuB_X_OK_RF_vec = append(WuS_SuB_X_OK_RF_vec, imp_OK_RF["X", "mean.rmse"])
          WuS_SuB_Y_OK_RF_vec = append(WuS_SuB_Y_OK_RF_vec, imp_OK_RF["Y", "mean.rmse"])
          WuS_SuB_tc315_OK_RF_vec = append(WuS_SuB_tc315_OK_RF_vec, imp_OK_RF["tc315", "mean.rmse"])
          ## WuS_SuB_sp_cv_loo_OK_RF
        } else if (i2 == "WuS_SuB_sp_cv_loo_OK_RF_"){
          imp_loo_OK_RF = summary(sp_cv_loo_OK_RF$importance)
          imp_loo_OK_RF$mean.rmse = imp_loo_OK_RF$mean.rmse * -1
          WuS_SuB_cA_loo_OK_RF_vec = append(WuS_SuB_cA_loo_OK_RF_vec, imp_loo_OK_RF["cAckerland", "mean.rmse"])
          WuS_SuB_X_loo_OK_RF_vec = append(WuS_SuB_X_loo_OK_RF_vec, imp_loo_OK_RF["X", "mean.rmse"])
          WuS_SuB_Y_loo_OK_RF_vec = append(WuS_SuB_Y_loo_OK_RF_vec, imp_loo_OK_RF["Y", "mean.rmse"])
          WuS_SuB_tc315_loo_OK_RF_vec = append(WuS_SuB_tc315_loo_OK_RF_vec, imp_loo_OK_RF["tc315", "mean.rmse"])
          ## WuS_SuB_sp_cv_RF_oob_OK
        } else if (i2 == "WuS_SuB_sp_cv_RF_oob_OK_"){
          imp_RF_oob_OK = summary(sp_cv_RF_oob_OK$importance)
          imp_RF_oob_OK$mean.rmse = imp_RF_oob_OK$mean.rmse * -1
          WuS_SuB_cA_RF_oob_OK_vec = append(WuS_SuB_cA_RF_oob_OK_vec, imp_RF_oob_OK["cAckerland", "mean.rmse"])
          WuS_SuB_X_RF_oob_OK_vec = append(WuS_SuB_X_RF_oob_OK_vec, imp_RF_oob_OK["X", "mean.rmse"])
          WuS_SuB_Y_RF_oob_OK_vec = append(WuS_SuB_Y_RF_oob_OK_vec, imp_RF_oob_OK["Y", "mean.rmse"])
          WuS_SuB_tc315_RF_oob_OK_vec = append(WuS_SuB_tc315_RF_oob_OK_vec, imp_RF_oob_OK["tc315", "mean.rmse"])
          ## WuS_SuB_sp_cv_UK
        } else if (i2 == "WuS_SuB_sp_cv_UK_"){
          imp_UK = summary(sp_cv_UK$importance)
          imp_UK$mean.rmse = imp_UK$mean.rmse * -1
          WuS_SuB_cA_UK_vec = append(WuS_SuB_cA_UK_vec, imp_UK["cAckerland", "mean.rmse"])
          WuS_SuB_X_UK_vec = append(WuS_SuB_X_UK_vec, imp_UK["X", "mean.rmse"])
          WuS_SuB_Y_UK_vec = append(WuS_SuB_Y_UK_vec, imp_UK["Y", "mean.rmse"])
        }
        
      }
    }
  }
}

# Scale the nn dist vecs 
NuM_L_mean_sqrt_nn_dist = sqrt(NuM_L_mean_nn_dist)
WuS_SuB_mean_sqrt_nn_dist = sqrt(WuS_SuB_mean_nn_dist)        
##
## End (store the importance/RMSE difference values in vectors)
####


####
## Plot SVIPs
##

# Create importance data frames 
NuM_L_cA_df = data.frame(NuM_L_cA_MLR_vec, NuM_L_cA_bRF_vec, NuM_L_cA_RFSI_vec,
                         NuM_L_cA_RF_MEv_vec, NuM_L_cA_OK_RF_vec, 
                         NuM_L_cA_loo_OK_RF_vec, NuM_L_cA_RF_oob_OK_vec, 
                         NuM_L_cA_UK_vec, NuM_L_cA_RF_vec)

WuS_SuB_cA_df = data.frame(WuS_SuB_cA_MLR_vec, WuS_SuB_cA_bRF_vec, 
                           WuS_SuB_cA_RFSI_vec, WuS_SuB_cA_RF_MEv_vec, 
                           WuS_SuB_cA_OK_RF_vec, WuS_SuB_cA_loo_OK_RF_vec, 
                           WuS_SuB_cA_RF_oob_OK_vec, WuS_SuB_cA_UK_vec,
                           WuS_SuB_cA_RF_vec)

NuM_L_X_df = data.frame(NuM_L_X_MLR_vec, NuM_L_X_RFSI_vec,
                         NuM_L_X_RF_MEv_vec, NuM_L_X_OK_RF_vec, 
                         NuM_L_X_loo_OK_RF_vec, NuM_L_X_RF_oob_OK_vec, 
                         NuM_L_X_UK_vec, NuM_L_X_RF_vec)

WuS_SuB_X_df = data.frame(WuS_SuB_X_MLR_vec, 
                           WuS_SuB_X_RFSI_vec, WuS_SuB_X_RF_MEv_vec, 
                           WuS_SuB_X_OK_RF_vec, WuS_SuB_X_loo_OK_RF_vec, 
                           WuS_SuB_X_RF_oob_OK_vec, WuS_SuB_X_UK_vec,
                           WuS_SuB_X_RF_vec)

NuM_L_Y_df = data.frame(NuM_L_Y_MLR_vec, NuM_L_Y_RFSI_vec,
                        NuM_L_Y_RF_MEv_vec, NuM_L_Y_OK_RF_vec, 
                        NuM_L_Y_loo_OK_RF_vec, NuM_L_Y_RF_oob_OK_vec, 
                        NuM_L_Y_UK_vec, NuM_L_Y_RF_vec)

WuS_SuB_Y_df = data.frame(WuS_SuB_Y_MLR_vec,
                          WuS_SuB_Y_RFSI_vec, WuS_SuB_Y_RF_MEv_vec, 
                          WuS_SuB_Y_OK_RF_vec, WuS_SuB_Y_loo_OK_RF_vec, 
                          WuS_SuB_Y_RF_oob_OK_vec, WuS_SuB_Y_UK_vec,
                          WuS_SuB_Y_RF_vec)

NuM_L_tc45_df = data.frame(NuM_L_tc45_RFSI_vec,
                        NuM_L_tc45_RF_MEv_vec, NuM_L_tc45_OK_RF_vec, 
                        NuM_L_tc45_loo_OK_RF_vec, NuM_L_tc45_RF_oob_OK_vec, 
                        NuM_L_tc45_RF_vec)

WuS_SuB_tc315_df = data.frame(WuS_SuB_tc315_RFSI_vec,
                           WuS_SuB_tc315_RF_MEv_vec, WuS_SuB_tc315_OK_RF_vec, 
                           WuS_SuB_tc315_loo_OK_RF_vec, 
                           WuS_SuB_tc315_RF_oob_OK_vec, 
                           WuS_SuB_tc315_RF_vec)

(NuM_L_cA_df - NuM_L_cA_df$NuM_L_cA_bRF_vec) /NuM_L_cA_df$NuM_L_cA_bRF_vec *100
(WuS_SuB_cA_df - WuS_SuB_cA_df$WuS_SuB_cA_bRF_vec) /WuS_SuB_cA_df$WuS_SuB_cA_bRF_vec *100

# Colors vector
col_vec = c("#000000", "#CCCCCC", "#FF9933", "#FF0000", "#66FFFF", "#0000FF", 
            "#9900FF", "#666666", "#33CC33")

col_vec_xy = c("#000000", "#FF9933", "#FF0000", "#66FFFF", "#0000FF", 
            "#9900FF", "#666666", "#33CC33")

col_vec_tc = c("#FF9933", "#FF0000", "#66FFFF", "#0000FF", "#9900FF", "#33CC33")


## Plot loops
# Set plot layout
layout_mat <- matrix(c(1,2,1,3,1,4,1,5,1,6), nrow = 5, ncol = 2,
                     byrow = TRUE)
layout_mat

my_lay = layout(mat = layout_mat, 
                heights = c(2.5, 2.5, 2.5, 2.5, 0.35),
                widths = c(0.35, 4.5), respect =FALSE)
layout.show(my_lay)


## NuM_L
# First plot
par(mar = c(2, 0, 0, 0)) # bottom, left, top, right margins
plot(NULL, ylab = "", bty = "n", 
     xlim = c(0, 0.1), ylim = c(0, 0.1), xaxt = "n", yaxt = "n")
mtext(expression("Mittlere Zunahme des RMSE"),
      side = 4, line = -4, col = 1, cex = 1)

# cAckerland
# Margins
par(mar = c(2, 0, 0, 1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1,                 
     xlab = "", 
     ylab = "",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(-4, 26),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 5, 11.18034, 21.21320, 30.82207, 40.62019, 60.41523, 80.31189,
               100.19980, 130.15376, 160.12495, 200.09998), 
     labels = c("", "", "", "", "", "", "", "", "", "", "", ""))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot values
for (i in 1:ncol(NuM_L_cA_df)){
  # Points
  points(x = NuM_L_mean_sqrt_nn_dist,
         y = NuM_L_cA_df[,i],
         pch = 21,
         bg = col_vec[i])
  # Line
  lines(x = NuM_L_mean_sqrt_nn_dist,
        y = NuM_L_cA_df[,i],
        col = col_vec[i],
        lty = 2, lwd = 2)
}

legend(x = "top" , legend = c("Anteil Ackerland im EZG"), cex = 1.25)

# X
# Margins
par(mar = c(2, 0, 0, 1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1,                 
     xlab = "", 
     ylab = "",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(-4, 26),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 5, 11.18034, 21.21320, 30.82207, 40.62019, 60.41523, 80.31189,
               100.19980, 130.15376, 160.12495, 200.09998), 
     labels = c("", "", "", "", "", "", "", "", "", "", "", ""))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot values
for (i in 1:ncol(NuM_L_X_df)){
  # Points
  points(x = NuM_L_mean_sqrt_nn_dist,
         y = NuM_L_X_df[,i],
         pch = 21,
         bg = col_vec_xy[i])
  # Line
  lines(x = NuM_L_mean_sqrt_nn_dist,
        y = NuM_L_X_df[,i],
        col = col_vec_xy[i],
        lty = 2, lwd = 2)
}

legend(x = "top" , legend = c("X-Koordinate"), cex = 1.25)

# Y
# Margins
par(mar = c(2, 0, 0, 1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1,                 
     xlab = "", 
     ylab = "",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(-4, 26),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 5, 11.18034, 21.21320, 30.82207, 40.62019, 60.41523, 80.31189,
               100.19980, 130.15376, 160.12495, 200.09998), 
    labels = c("", "", "", "", "", "", "", "", "", "", "", ""))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot values
for (i in 1:ncol(NuM_L_Y_df)){
  # Points
  points(x = NuM_L_mean_sqrt_nn_dist,
         y = NuM_L_Y_df[,i],
         pch = 21,
         bg = col_vec_xy[i])
  # Line
  lines(x = NuM_L_mean_sqrt_nn_dist,
        y = NuM_L_Y_df[,i],
        col = col_vec_xy[i],
        lty = 2, lwd = 2)
}

legend(x = "top" , legend = c("Y-Koordinate"), cex = 1.25)

# tc45
# Margins
par(mar = c(2, 0, 0, 1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1, 
     main = "",
     xlab = "Mittlere Vorhersagedistanz [m]", 
     ylab = "",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(-4, 26),
     #yaxt = "n",
     type = "n")
axis(1, at = c(0, 2.236068, 10.95445, 21.21320, 30.82207, 40.62019, 60.41523, 
               80.31189, 100.24969, 130.19217, 160.15617, 200.12496), 
     labels = c(0, 5, 120, 450, 950, 1650, 3650, 6450, 10050, 16950, 25650, 
                40050))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot values
for (i in 1:ncol(NuM_L_tc45_df)){
  # Points
  points(x = NuM_L_mean_sqrt_nn_dist,
         y = NuM_L_tc45_df[,i],
         pch = 21,
         bg = col_vec_tc[i])
  # Line
  lines(x = NuM_L_mean_sqrt_nn_dist,
        y = NuM_L_tc45_df[,i],
        col = col_vec_tc[i],
        lty = 2, lwd = 2)
}
legend(x = "top" , legend = c("Schräge Koordinate 45°"), cex = 1.25)

# Legend
legend(x = "topright", cex = 1, ncol = 2,
       legend = c("MLR", "UK", "RF", "RF-K", "RF-MEv", "RF-oob-OK", "OK-RF", 
                  "RF-loo-OK", "RFSI"),
       fill = c("#000000", "#666666", "#CCCCCC", "#33CC33", "#FF0000", "#9900FF",
                "#66FFFF", "#0000FF", "#FF9933"))

# Last plot
par(mar = c(0, 0, 0, 0)) # bottom, left, top, right margins
plot(NULL, ylab = "", bty = "n", 
     xlim = c(0, 0.1), ylim = c(0, 0.1), xaxt = "n", yaxt = "n")
mtext(expression("Mittlere Vorhersagedistanz [m]"),
      side = 1, line = -1, col = 1, cex = 1)


## WuS_SuB
# First plot
par(mar = c(2, 0, 0, 0)) # bottom, left, top, right margins
plot(NULL, ylab = "", bty = "n", 
     xlim = c(0, 0.1), ylim = c(0, 0.1), xaxt = "n", yaxt = "n")
mtext(expression("Mittlere Zunahme des RMSE"),
      side = 4, line = -4, col = 1, cex = 1)

# cAckerland
# Margins
par(mar = c(2, 0, 0, 1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1,                 
     xlab = "", 
     ylab = "",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(0, 9),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 5, 11.18034, 21.21320, 30.82207, 40.62019, 60.41523, 80.31189,
               100.19980, 130.15376, 160.12495, 200.09998), 
     labels = c("", "", "", "", "", "", "", "", "", "", "", ""))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot values
for (i in 1:ncol(WuS_SuB_cA_df)){
  # Points
  points(x = WuS_SuB_mean_sqrt_nn_dist,
         y = WuS_SuB_cA_df[,i],
         pch = 21,
         bg = col_vec[i])
  # Line
  lines(x = WuS_SuB_mean_sqrt_nn_dist,
        y = WuS_SuB_cA_df[,i],
        col = col_vec[i],
        lty = 2, lwd = 2)
}

legend(x = "top" , legend = c("Anteil Ackerland im EZG"), cex = 1.25)

# X
# Margins
par(mar = c(2, 0, 0, 1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1,                 
     xlab = "", 
     ylab = "",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(0, 9),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 5, 11.18034, 21.21320, 30.82207, 40.62019, 60.41523, 80.31189,
               100.19980, 130.15376, 160.12495, 200.09998), 
     labels = c("", "", "", "", "", "", "", "", "", "", "", ""))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot values
for (i in 1:ncol(WuS_SuB_X_df)){
  # Points
  points(x = WuS_SuB_mean_sqrt_nn_dist,
         y = WuS_SuB_X_df[,i],
         pch = 21,
         bg = col_vec_xy[i])
  # Line
  lines(x = WuS_SuB_mean_sqrt_nn_dist,
        y = WuS_SuB_X_df[,i],
        col = col_vec_xy[i],
        lty = 2, lwd = 2)
}

legend(x = "top" , legend = c("X Koordinate"), cex = 1.25)

# Y
# Margins
par(mar = c(2, 0, 0, 1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1,                 
     xlab = "", 
     ylab = "",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(0, 9),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 5, 11.18034, 21.21320, 30.82207, 40.62019, 60.41523, 80.31189,
               100.19980, 130.15376, 160.12495, 200.09998), 
     labels = c("", "", "", "", "", "", "", "", "", "", "", ""))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot values
for (i in 1:ncol(WuS_SuB_Y_df)){
  # Points
  points(x = WuS_SuB_mean_sqrt_nn_dist,
         y = WuS_SuB_Y_df[,i],
         pch = 21,
         bg = col_vec_xy[i])
  # Line
  lines(x = WuS_SuB_mean_sqrt_nn_dist,
        y = WuS_SuB_Y_df[,i],
        col = col_vec_xy[i],
        lty = 2, lwd = 2)
}

legend(x = "top" , legend = c("Y Koordinate"), cex = 1.25)

# tc315
# Margins
par(mar = c(2, 0, 0, 1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1, 
     main = "",
     xlab = "Mittlere Vorhersagedistanz [m]", 
     ylab = "",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(0, 9),
     #yaxt = "n",
     type = "n")
axis(1, at = c(0, 5, 11.18034, 21.21320, 30.82207, 40.62019, 60.41523, 
               80.31189, 100.24969, 130.19217, 160.15617, 200.12496), 
     labels = c(0, 25, 125, 450, 950, 1650, 3650, 6450, 10050, 16950, 25650, 
                40050))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot values
for (i in 1:ncol(WuS_SuB_tc315_df)){
  # Points
  points(x = WuS_SuB_mean_sqrt_nn_dist,
         y = WuS_SuB_tc315_df[,i],
         pch = 21,
         bg = col_vec_tc[i])
  # Line
  lines(x = WuS_SuB_mean_sqrt_nn_dist,
        y = WuS_SuB_tc315_df[,i],
        col = col_vec_tc[i],
        lty = 2, lwd = 2)
}
legend(x = "top" , legend = c("Schräge Koordinate 315°"), cex = 1.25)

# Legend
legend(x = "topright", cex = 1, ncol = 2,
       legend = c("MLR", "UK", "RF", "RF-K", "RF-MEv", "RF-oob-OK", "OK-RF", 
                  "RF-loo-OK", "RFSI"),
       fill = c("#000000", "#666666", "#CCCCCC", "#33CC33", "#FF0000", "#9900FF",
                "#66FFFF", "#0000FF", "#FF9933"))

# Last plot
par(mar = c(0, 0, 0, 0)) # bottom, left, top, right margins
plot(NULL, ylab = "", bty = "n", 
     xlim = c(0, 0.1), ylim = c(0, 0.1), xaxt = "n", yaxt = "n")
mtext(expression("Mittlere Vorhersagedistanz [m]"),
      side = 1, line = -1, col = 1, cex = 1)

##
## End (plot SVIPs)
####
################################################################################
## End (SVIPs) 
################################################################################


################################################################################
## Test area
################################################################################
load("Results/bcNitrateRe/mRMSE_dfs_org.rda")
diff_bRF_NuM_L = NuM_L_mRMSE_df - NuM_L_mRMSE_df$NuM_L_bRF
diff_bRF_WuS_SuB = WuS_SuB_mRMSE_df - WuS_SuB_mRMSE_df$WuS_SuB_bRF
diff_MLR_NuM_L = NuM_L_mRMSE_df - NuM_L_mRMSE_df$NuM_L_MLR
diff_MLR_WuS_SuB = WuS_SuB_mRMSE_df - WuS_SuB_mRMSE_df$WuS_SuB_MLR
diff_UK_NuM_L =  NuM_L_mRMSE_df - NuM_L_mRMSE_df$NuM_L_UK
diff_UK_WuS_SuB = WuS_SuB_mRMSE_df - WuS_SuB_mRMSE_df$WuS_SuB_UK
diff_RF_oob_OK_WuS_SuB = WuS_SuB_mRMSE_df - WuS_SuB_mRMSE_df$WuS_SuB_RF_oob_OK
diff_RF_oob_OK_NuM_L = NuM_L_mRMSE_df - NuM_L_mRMSE_df$NuM_L_RF_oob_OK

load("Results/orgNitrate/NuM_L/NuM_L_sp_cv_bRF_100_+50_10.rda")

error_vec = c()

for (i in 1:length(sp_cv_bRF$represampling[[1]])){
  error_vec = append(error_vec, sp_cv_bRF$error_fold[[1]][[i]]$test$rmse)
  median_vec = append(median_vec, sp_cv_bRF$error_fold[[1]][[i]]$test$median)
}

err <- summary(sp_cv_bRF$error_fold)
rmse = err["test.rmse", "mean"]

error_vec
median_vec

mean(error_vec)


load("Data/NuM_L.rda")
load("Results/bcNitrateRe/mRMSE_dfs_org.rda")
files = c("NuM_L_sp_cv_loo_OK_RF_40000_+100_10.rda", 
          "NuM_L_sp_cv_loo_OK_RF_25600_+100_10.rda", 
          "NuM_L_sp_cv_loo_OK_RF_16900_+100_10.rda",
          "NuM_L_sp_cv_loo_OK_RF_10000_+100_10.rda",
          "NuM_L_sp_cv_loo_OK_RF_6400_+100_10.rda",
          "NuM_L_sp_cv_loo_OK_RF_3600_+100_10.rda")

RMSE_vec = c()

for (i1 in files){
  load(paste("Results/", i1, sep = ""))
  RMSE_values = c()
  for (i2 in 1:length(sp_cv_loo_OK_RF$represampling[[1]])){
    # Get prediction value and id/row number
    pred_value = sp_cv_loo_OK_RF$error_fold[[1]][[i2]]$test$pred
    pred_id = sp_cv_loo_OK_RF$represampling[[1]][[i2]]$test
    # Get observation value
    obs_value = NuM_L[pred_id, "subMittelwert"]
    # RMSE
    RMSE = sqrt((obs_value-pred_value)^2)
    # Add result to vector
    RMSE_values = append(RMSE_values, RMSE)
  }
  mRMSE = mean(RMSE_values)
  RMSE_vec = append(RMSE_vec, mRMSE)  
}

NuM_L_mRMSE_df
RMSE_vec
################################################################################
## End (test area)
################################################################################


################################################################################
## Variable importance plots (VIPs)
################################################################################
# Vector of the first part of the file names
f_names_vec1 = c("WuS_SuB_sp_cv_bRF_", "WuS_SuB_sp_cv_MLR_", "WuS_SuB_sp_cv_RF_",
                 "WuS_SuB_sp_cv_RF_MEv_", "WuS_SuB_sp_cv_RFSI_", 
                 "WuS_SuB_sp_cv_loo_OK_RF_", "WuS_SuB_sp_cv_OK_RF_",
                 "WuS_SuB_sp_cv_RF_oob_OK_", "WuS_SuB_sp_cv_UK_")

# Second part of the file name
f_names_p2 = "0_+all_10.rda" 

# Load files
for (i in f_names_vec1){
  load(paste("Results/WuS_SuB/", i, f_names_p2, sep = ""))
}

# Get importance
imp_MLR = summary(sp_cv_MLR$importance)
imp_UK = summary(sp_cv_UK$importance)
imp_bRF = summary(sp_cv_bRF$importance)
imp_RF = summary(sp_cv_RF$importance)
imp_RFSI = summary(sp_cv_RFSI$importance)
imp_RF_MEv = summary(sp_cv_RF_MEv$importance)
imp_OK_RF = summary(sp_cv_OK_RF$importance)
imp_loo_OK_RF = summary(sp_cv_llo_OK_RF$importance)
imp_RF_oob_OK = summary(sp_cv_RF_oob_OK$importance)

# Convert decrease to increase
imp_MLR$mean.rmse = imp_MLR$mean.rmse * -1
imp_UK$mean.rmse = imp_UK$mean.rmse * -1
imp_bRF$mean.rmse = imp_bRF$mean.rmse * -1
imp_RF$mean.rmse = imp_RF$mean.rmse * -1
imp_RFSI$mean.rmse = imp_RFSI$mean.rmse * -1
imp_RF_MEv$mean.rmse = imp_RF_MEv$mean.rmse * -1
imp_OK_RF$mean.rmse = imp_OK_RF$mean.rmse * -1
imp_loo_OK_RF$mean.rmse = imp_loo_OK_RF$mean.rmse * -1
imp_RF_oob_OK$mean.rmse = imp_RF_oob_OK$mean.rmse * -1

# Barplots
par(mfrow=c(3,3))

imp_MLR = imp_MLR[order(imp_MLR$mean.rmse, decreasing = FALSE),]
par(mar = c(4,9,1,0.7)) # bottom, left, top, right margins
barplot(imp_MLR$mean.rmse[19:23], names.arg = rownames(imp_MLR)[19:23], 
        horiz = TRUE, las = 1, xlim = c(0, 0.7), main = "MLR")

imp_UK = imp_UK[order(imp_UK$mean.rmse, decreasing = FALSE),]
barplot(imp_UK$mean.rmse[19:23], names.arg = rownames(imp_UK)[19:23], 
        horiz = TRUE, las = 1, xlim = c(0, 0.7), main = "UK")

imp_bRF = imp_bRF[order(imp_bRF$mean.rmse, decreasing = FALSE),]
barplot(imp_bRF$mean.rmse[17:21], names.arg = rownames(imp_bRF)[17:21], 
        horiz = TRUE, las = 1, xlim = c(0, 0.7), main = "RF")

imp_RF = imp_RF[order(imp_RF$mean.rmse, decreasing = FALSE),]
barplot(imp_RF$mean.rmse[21:25], names.arg = rownames(imp_RF)[21:25], 
        horiz = TRUE, las = 1, xlim = c(0, 0.7), main = "RF-K")

imp_RFSI = imp_RFSI[order(imp_RFSI$mean.rmse, decreasing = FALSE),]
barplot(imp_RFSI$mean.rmse[21:25], names.arg = rownames(imp_RFSI)[21:25], 
        horiz = TRUE, las = 1, xlim = c(0, 0.7), main = "RFSI")

imp_RF_MEv = imp_RF_MEv[order(imp_RF_MEv$mean.rmse, decreasing = FALSE),]
barplot(imp_RF_MEv$mean.rmse[21:25], names.arg = rownames(imp_RF_MEv)[21:25], 
        horiz = TRUE, las = 1, xlim = c(0, 0.7), main = "RF-MEv")

imp_OK_RF = imp_OK_RF[order(imp_OK_RF$mean.rmse, decreasing = FALSE),]
par(mar = c(4.1,9,0.7,0.7)) # bottom, left, top, right margins
barplot(imp_OK_RF$mean.rmse[21:25], names.arg = rownames(imp_OK_RF)[21:25], 
        horiz = TRUE, las = 1, xlim = c(0, 0.7), main = "OK-RF", xlab = "Mittlere Zunahme des RMSE")

imp_loo_OK_RF = imp_loo_OK_RF[order(imp_loo_OK_RF$mean.rmse, decreasing = FALSE),]
barplot(imp_loo_OK_RF$mean.rmse[21:25], names.arg = rownames(imp_loo_OK_RF)[21:25], 
        horiz = TRUE, las = 1, xlim = c(0, 0.7), main = "loo-OK-RF", xlab = "Mittlere Zunahme des RMSE")

imp_RF_oob_OK = imp_RF_oob_OK[order(imp_RF_oob_OK$mean.rmse, decreasing = FALSE),]
barplot(imp_RF_oob_OK$mean.rmse[21:25], names.arg = rownames(imp_RF_oob_OK)[21:25], 
        horiz = TRUE, las = 1, xlim = c(0, 0.7), main = "RF-oob-OK", xlab = "Mittlere Zunahme des RMSE")
################################################################################
## End (VIPs)
################################################################################