################################################################################
## Spatial prediction error profile (SPEP)
################################################################################

####
## Save RMSE values in vectors
##

# Create vectors
MLR_RMSE_vec = c()
bRF_RMSE_vec = c()
RF_RMSE_vec = c() 
RFSI_RMSE_vec = c()
RF_MEv_RMSE_vec = c()
OK_RF_RMSE_vec = c()
loo_OK_RF_RMSE_vec = c()
RF_oob_OK_RMSE_vec = c()
UK_RMSE_vec = c()

mean_nn_dist_vec = c()

# Vector of the first part of the file names
f_names_vec1 = c("NuM_L_sp_cv_bRF_", "NuM_L_sp_cv_MLR_", "NuM_L_sp_cv_RF_",
                 "NuM_L_sp_cv_RF_MEv_", "NuM_L_sp_cv_RFSI_", 
                 "NuM_L_sp_cv_loo_OK_RF_", "NuM_L_sp_cv_OK_RF_",
                 "NuM_L_sp_cv_RF_oob_OK_", "NuM_L_sp_cv_UK_")

# Vector of the second part of the file names
f_names_vec2 = c("0_+50_10.rda", "100_+50_10.rda", "400_+100_10.rda",
                 "900_+100_10.rda", "1600_+100_10.rda", "3600_+100_10.rda", 
                 "6400_+100_10.rda", "10000_+100_10.rda", "16900_+100_10.rda",
                 "25600_+100_10.rda", "40000_+100_10.rda")

# Load results and store the RMSE values in the vectors
for (i1 in f_names_vec1){
  for (i2 in f_names_vec2){
    # Complete file name
    f_name = paste("Results/NuM_L/",i1, i2, sep = "")
    # Load file
    load(f_name)
    ## Get RMSE values and mean nearest neighbor distance
    if (i1 == "NuM_L_sp_cv_bRF_"){
      # RMSE
      RMSE = sp_cv_bRF$error_rep$test_rmse
      bRF_RMSE_vec = append(bRF_RMSE_vec, RMSE)
      # nn distance
      nn_distances = c()
      for (i3 in 1:length(sp_cv_bRF$error_fold[[1]])){
        nn_dist = sp_cv_bRF$error_fold[[1]][[i3]]$distance
        nn_distances = append(nn_distances, nn_dist)
      }
      mean_nn_dist_vec = append(mean_nn_dist_vec, mean(nn_distances))
      
    } else if (i1 == "NuM_L_sp_cv_MLR_"){
      RMSE = sp_cv_MLR$error_rep$test_rmse
      MLR_RMSE_vec = append(MLR_RMSE_vec, RMSE)
      
    } else if (i1 == "NuM_L_sp_cv_RF_"){
      RMSE = sp_cv_RF$error_rep$test_rmse
      RF_RMSE_vec = append(RF_RMSE_vec, RMSE)
      
    } else if (i1 == "NuM_L_sp_cv_RF_MEv_"){
      RMSE = sp_cv_RF_MEv$error_rep$test_rmse
      RF_MEv_RMSE_vec = append(RF_MEv_RMSE_vec, RMSE)
      
    } else if (i1 == "NuM_L_sp_cv_RFSI_"){
      RMSE = sp_cv_RFSI$error_rep$test_rmse
      RFSI_RMSE_vec = append(RFSI_RMSE_vec, RMSE)
      
    } else if (i1 == "NuM_L_sp_cv_OK_RF_"){
      RMSE = sp_cv_OK_RF$error_rep$test_rmse
      OK_RF_RMSE_vec = append(OK_RF_RMSE_vec, RMSE)
      
    } else if (i1 == "NuM_L_sp_cv_loo_OK_RF_"){
      RMSE = sp_cv_llo_OK_RF$error_rep$test_rmse
      loo_OK_RF_RMSE_vec = append(loo_OK_RF_RMSE_vec, RMSE)
      
    } else if (i1 == "NuM_L_sp_cv_RF_oob_OK_"){
      RMSE = sp_cv_RF_oob_OK$error_rep$test_rmse
      RF_oob_OK_RMSE_vec = append(RF_oob_OK_RMSE_vec, RMSE)
      
    } else{
      RMSE = sp_cv_UK$error_rep$test_rmse
      UK_RMSE_vec = append(UK_RMSE_vec, RMSE)
    }
  }
}

# Scale the nn dist vec 
mean_nn_sqrt_dist_vec = sqrt(mean_nn_dist_vec)
##
## End (save RMSE values in vectors)
####


####
## Plot SPEP
##

# Create df
RMSE_df = data.frame(UK_RMSE_vec, RF_oob_OK_RMSE_vec, loo_OK_RF_RMSE_vec, 
                     RF_MEv_RMSE_vec, RFSI_RMSE_vec, RF_RMSE_vec, bRF_RMSE_vec,
                     MLR_RMSE_vec, OK_RF_RMSE_vec)

# Colors vector
col_vec = c("#00FF00", "#FF33FF", "#FFFF00", "#FF0000", "#FF9933", "#0000FF", 
            "#009900", "#000000", "#66FFFF")

# Margins
par(mar = c(4.5,4.5,0.5,0.5)) # bottom, left, top, right

# Create a blank plotting space
plot(x = 1,                 
     xlab = "Vorhersagedistanz [m]", 
     ylab = "RMSE",
     xlim = c(0, 210),
     xaxt = "n",
     ylim = c(1.15, 2.85),
     yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(5, 10, 20, 30, 40, 60, 80, 100, 130, 160, 200), 
     labels = c(25, 100, 400, 900, 1600, 3600, 6400, 10000, 16900, 25600, 40000),
     cex.axis = 0.75)
axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8))

# Plot RMSE values
for (i in 1:nrow(RMSE_df)){
  # Points
  points(x = mean_nn_sqrt_dist_vec,
         y = RMSE_df[,i],
         pch = 21,
         bg = col_vec[1])
  # Line
  lines(x = mean_nn_sqrt_dist_vec,
        y = RMSE_df[,i],
        col = col_vec[1],
        lty = 2)
}

# Legend
legend(x = "bottomright",
       legend = c("MLR", "UK", "RF", "RF-K", "RF-MEv", "RF-oob-OK", "OK-RF", 
                  "RF-loo-OK", "RFSI"),
       fill = c("#000000", "#00FF00", "#009900", "#0000FF", "#FF0000", "#FF33FF",
                "#66FFFF", "#FFFF00", "#FF9933"))

##
## End (plot SPEP)
####
################################################################################
## End (SPEP)
################################################################################
