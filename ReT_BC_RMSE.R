################################################################################
## Calculate mean RMSE of retransformed predictions
################################################################################
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
    area_c1 = substr(i1, 1, 3)
    area_c2 = substr(i2, 1, 3)
    if (area_c1 == area_c2){
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
          obs_value = NuM_L[pred_id, "subMittelwert"]
          # Retransform pred value 
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
            obs_value = NuM_L[pred_id, "subMittelwert"]
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
            obs_value = NuM_L[pred_id, "subMittelwert"]
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
            obs_value = NuM_L[pred_id, "subMittelwert"]
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
            obs_value = NuM_L[pred_id, "subMittelwert"]
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
            obs_value = NuM_L[pred_id, "subMittelwert"]
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
            obs_value = NuM_L[pred_id, "subMittelwert"]
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
            obs_value = NuM_L[pred_id, "subMittelwert"]
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
            obs_value = NuM_L[pred_id, "subMittelwert"]
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
            obs_value = WuS_SuB[pred_id, "subMittelwert"]
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
            obs_value = WuS_SuB[pred_id, "subMittelwert"]
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
              obs_value = WuS_SuB[pred_id, "subMittelwert"]
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
            obs_value = WuS_SuB[pred_id, "subMittelwert"]
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
            obs_value = WuS_SuB[pred_id, "subMittelwert"]
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
            obs_value = WuS_SuB[pred_id, "subMittelwert"]
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
            obs_value = WuS_SuB[pred_id, "subMittelwert"]
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
            obs_value = WuS_SuB[pred_id, "subMittelwert"]
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
            obs_value = WuS_SuB[pred_id, "subMittelwert"]
            pred_value = inv_boxcox(pred_value_bc, 0.4242)
            RMSE = sqrt((obs_value-pred_value)^2)
            RMSE_values = append(RMSE_values, RMSE)
          }
          WuS_SuB_UK_mRv = append(WuS_SuB_UK_mRv, mean(RMSE_values))
      }
    }
    }
  }
}

# Scale the nn dist vecs 
NuM_L_mean_sqrt_nn_dist = sqrt(NuM_L_mean_nn_dist)
WuS_SuB_mean_sqrt_nn_dist = sqrt(WuS_SuB_mean_nn_dist)
################################################################################
## End (Calculate RMSE of retransformed predictions)
################################################################################


################################################################################
## Plot SPEPs
################################################################################
# Load original scale dfs for comparison 
load("Results/bcNitrateRe/mRMSE_dfs_org.rda")

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

# Plot mRMSE values
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

legend(x = "bottomleft" , legend = c("a)"), bty = "n", cex = 1.25)

## WuS_SuB
# Margins
par(mar = c(4, 0, 0, 1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1,                 
     xlab = "Vorhersagedistanz [m]", 
     #ylab = "Mittlerer RMSE",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(-10, 10),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 5, 11.18034, 21.21320, 30.82207, 40.62019, 60.41523, 80.31189,
               100.19980, 130.15376, 160.12495, 200.09998), 
     labels = c(0, 25, 125, 450, 950, 1650, 3650, 6450, 10040, 16940, 25640, 
                40040))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot mRMSE values
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

legend(x = "bottomleft" , legend = c("b)"), bty = "n", cex = 1.25)

# Legend
legend(x = "topright", cex = 0.8, ncol = 2,
       legend = c("MLR", "UK", "RF", "RF-K", "RF-MEv", "RF-oob-OK", "OK-RF", 
                  "RF-loo-OK", "RFSI"),
       fill = c("#000000", "#666666", "#CCCCCC", "#33CC33", "#FF0000", "#9900FF",
                "#66FFFF", "#0000FF", "#FF9933"))

# First plot
par(mar = c(2, 0, 0, 0)) # bottom, left, top, right margins
plot(NULL, ylab = "", bty = "n", 
     xlim = c(0, 0.1), ylim = c(0, 0.1), xaxt = "n", yaxt = "n")
mtext(expression("Mittlerer RMSE"),
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
     xlab = "Vorhersagedistanz [m]", 
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

# First plot
par(mar = c(2, 0, 0, 0)) # bottom, left, top, right margins
plot(NULL, ylab = "", bty = "n", 
     xlim = c(0, 0.1), ylim = c(0, 0.1), xaxt = "n", yaxt = "n")
mtext(expression("Mittlerer RMSE"),
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
     xlab = "Vorhersagedistanz [m]", 
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

