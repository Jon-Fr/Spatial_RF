################################################################################
## Spatial prediction error profile (SPEP)
################################################################################

####
## Save mean RMSE values in vectors
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
}

# Scale the nn dist vecs 
NuM_L_mean_sqrt_nn_dist = sqrt(NuM_L_mean_nn_dist)
WuS_SuB_mean_sqrt_nn_dist = sqrt(WuS_SuB_mean_nn_dist)        
##
## End (save mean RMSE values in vectors)
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

# Create color vector 
col_vec = c("#666666", "#9900FF", "#0000FF", "#FF0000", "#FF9933", "#33CC33",
            "#CCCCCC", "#000000", "#66FFFF")


## NuM_L
# Margins
#par(mar = c(4.1,3.9,0.1,0.1)) # bottom, left, top, right
# Create a blank plotting space
plot(x = 1,                 
     xlab = "Vorhersagedistanz [m]", 
     ylab = "Mittlerer RMSE",
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

# Legend
legend(x = "bottomright", cex = 1, ncol = 2,
       legend = c("MLR", "UK", "RF", "RF-K", "RF-MEv", "RF-oob-OK", "OK-RF", 
                  "RF-loo-OK", "RFSI"),
       fill = c("#000000", "#666666", "#CCCCCC", "#33CC33", "#FF0000", "#9900FF",
                "#66FFFF", "#0000FF", "#FF9933"))


## WuS_SuB
# Margins
#par(mar = c(4.1,3.9,0.1,0.1)) # bottom, left, top, right
# Create a blank plotting space
plot(x = 1,                 
     xlab = "Vorhersagedistanz [m]", 
     ylab = "Mittlerer RMSE",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(0, 12),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 5, 11.18034, 21.21320, 30.82207, 40.62019, 60.41523, 80.31189,
               100.19980, 130.15376, 160.12495, 200.09998), 
     labels = c(0, 25, 125, 450, 950, 1650, 3650, 6450, 10040, 16940, 25640, 
                40040))
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

##
## End (plot SPEP)
####
################################################################################
## End (SPEP)
################################################################################