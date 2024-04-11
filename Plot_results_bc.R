################################################################################
## Histograms
################################################################################
# Load data
load("Data/NuM_L.rda")
load("Data/WuS_SuB.rda")

## NuM_L
# Get some values for sm and bc
NuM_L_range_bc = max(NuM_L$bcNitrate) - min(NuM_L$bcNitrate)  
NuM_L_min_bc = min(NuM_L$bcNitrate)
NuM_L_max_sm = max(NuM_L$subMittelwert)

NuM_L_hist_sm = hist(NuM_L$subMittelwert, breaks = (55))
NuM_L_hist_bc = hist(NuM_L$bcNitrate, breaks = (55))

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
mtext(expression("Anzahl an Datenpunkten"),
      side = 4, line = -4, col = 1, cex = 1.1)

# Second plot
par(mar = c(4, 0, 1, 0)) # bottom, left, top, right margins
plot(NuM_L_hist_sm, xlab = "Nitratkonzentration [mg/L]", ylab = "", 
     main = "", xaxt = "n", yaxt = "n", xlim = c(0, 600), ylim = c(0, 2000))
axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550),
     labels = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550), cex.axis = 1)
axis(2, at = c(0, 500, 1000, 1500, 2000), 
     labels = c(0, 500, 1000, 1500, 2000), cex.axis = 1)


# Third plot
par(mar = c(4, 0, 1, 0)) # bottom, left, top, right margins
plot(NuM_L_hist_bc, xlab = "Nitratkonzentration [Box-Cox transformiert]", ylab = "", 
     main = "",  xaxt = "n", yaxt = "n",  xlim = c(-4, 7), ylim = c(0, 600))

axis(1, at = c(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7),
     labels = c(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7), cex.axis = 1)
axis(2, at = c(0, 200, 400, 600), 
     labels = c(0, 200, 400, 600), cex.axis = 1)


## WuS_SuB
# Get some values for sm and bc
WuS_SuB_range_bc = max(WuS_SuB$bcNitrate) - min(WuS_SuB$bcNitrate)  
WuS_SuB_min_bc = min(WuS_SuB$bcNitrate)
WuS_SuB_max_sm = max(WuS_SuB$subMittelwert)

WuS_SuB_hist_sm = hist(WuS_SuB$subMittelwert, breaks = (27))
WuS_SuB_hist_bc = hist(WuS_SuB$bcNitrate, breaks = (27))

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
mtext(expression("Anzahl an Datenpunkten"),
      side = 4, line = -4, col = 1, cex = 1.1)

# Second plot
par(mar = c(4, 0, 1, 0)) # bottom, left, top, right margins
plot(WuS_SuB_hist_sm, xlab = "Nitratkonzentration [mg/L]", ylab = "", 
     main = "", xaxt = "n", yaxt = "n", xlim = c(0, 300), ylim = c(0, 750))
axis(1, at = c(0, 50, 100, 150, 200, 250, 300),
     labels = c(0, 50, 100, 150, 200, 250, 300), cex.axis = 1)
axis(2, at = c(0, 250, 500, 750), 
     labels = c(0, 200, 400, 600), cex.axis = 1)

# Third plot
par(mar = c(4, 0, 1, 0)) # bottom, left, top, right margins
plot(WuS_SuB_hist_bc, xlab = "Nitratkonzentration [Box-Cox transformiert]", ylab = "", 
     main = "",  xaxt = "n", yaxt = "n",  xlim = c(-5, 25), ylim = c(0, 300))

axis(1, at = c(-5, 0, 5, 10, 15, 20, 25),
     labels = c(-5, 0, 5, 10, 15, 20, 25), cex.axis = 1)
axis(2, at = c(0, 100, 200, 300), 
     labels = c(0, 100, 200, 300), cex.axis = 1)

## Alternative plot 
# Set plot layout
layout_mat <- matrix(c(1,2,1,3,1,4,1,5), nrow = 4, ncol = 2,
                     byrow = TRUE)
layout_mat

my_lay = layout(mat = layout_mat, 
                heights = c(2.5, 2.5, 2.5, 2.5),
                widths = c(0.5, 4.5), respect =FALSE)
layout.show(my_lay)

# First plot
par(mar = c(2, 0, 0, 0)) # bottom, left, top, right margins
plot(NULL, ylab = "", bty = "n", 
     xlim = c(0, 0.1), ylim = c(0, 0.1), xaxt = "n", yaxt = "n")
mtext(expression("Anzahl an Datenpunkten"),
      side = 4, line = -4, col = 1, cex = 1.1)

# Second plot
par(mar = c(4, 0, 1, 0)) # bottom, left, top, right margins
plot(NuM_L_hist_sm, xlab = "Nitratkonzentration [mg/L]", ylab = "", 
     main = "", xaxt = "n", yaxt = "n", xlim = c(0, 600), ylim = c(0, 2000))
axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550),
     labels = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550), cex.axis = 1)
axis(2, at = c(0, 500, 1000, 1500, 2000), 
     labels = c(0, 500, 1000, 1500, 2000), cex.axis = 1)

legend(x = "topright" , legend = c("a)"), bty = "n", cex = 1.25)

# Third plot
par(mar = c(4, 0, 1, 0)) # bottom, left, top, right margins
plot(NuM_L_hist_bc, xlab = "Nitratkonzentration [Box-Cox transformiert]", ylab = "", 
     main = "",  xaxt = "n", yaxt = "n",  xlim = c(-4, 7), ylim = c(0, 600))

axis(1, at = c(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7),
     labels = c(-4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7), cex.axis = 1)
axis(2, at = c(0, 200, 400, 600), 
     labels = c(0, 200, 400, 600), cex.axis = 1)

legend(x = "topright" , legend = c("b)"), bty = "n", cex = 1.25)

# Fourth plot
par(mar = c(4, 0, 1, 0)) # bottom, left, top, right margins
plot(WuS_SuB_hist_sm, xlab = "Nitratkonzentration [mg/L]", ylab = "", 
     main = "", xaxt = "n", yaxt = "n", xlim = c(0, 300), ylim = c(0, 750))
axis(1, at = c(0, 50, 100, 150, 200, 250, 300),
     labels = c(0, 50, 100, 150, 200, 250, 300), cex.axis = 1)
axis(2, at = c(0, 250, 500, 750), 
     labels = c(0, 200, 400, 600), cex.axis = 1)

legend(x = "topright" , legend = c("c)"), bty = "n", cex = 1.25)

# Fifth plot
par(mar = c(4, 0, 1, 0)) # bottom, left, top, right margins
plot(WuS_SuB_hist_bc, xlab = "Nitratkonzentration [Box-Cox transformiert]", ylab = "", 
     main = "",  xaxt = "n", yaxt = "n",  xlim = c(-5, 25), ylim = c(0, 300))

axis(1, at = c(-5, 0, 5, 10, 15, 20, 25),
     labels = c(-5, 0, 5, 10, 15, 20, 25), cex.axis = 1)
axis(2, at = c(0, 100, 200, 300), 
     labels = c(0, 100, 200, 300), cex.axis = 1)

legend(x = "topright" , legend = c("d)"), bty = "n", cex = 1.25)
################################################################################
## End (Histograms)
################################################################################


################################################################################
## Spatial prediction error profile (SPEP)
################################################################################
# Load data
load("Data/NuM_L.rda")
load("Data/WuS_SuB.rda")

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
f_names_vec3 = c("0_+50_", "100_+50_", "400_+100_",
                 "900_+100_", "1600_+100_", "3600_+100_", 
                 "6400_+100_", "10000_+100_", "16900_+100_",
                 "25600_+100_", "40000_+100_") 

# Load results and store the RMSE values in the vectors
for (i1 in f_names_vec1){
  for (i2 in f_names_vec2){
    for (i3 in f_names_vec3){
      # Fourth part
      if (i2 == "WuS_SuB_sp_cv_RF_MEv_"|
          i2 == "WuS_SuB_sp_cv_OK_RF_"|
          i2 == "WuS_SuB_sp_cv_loo_OK_RF_"| 
          i2 == "NuM_L_sp_cv_RF_MEv_"|
          i2 == "NuM_L_sp_cv_loo_OK_RF_"|
          i2 == "NuM_L_sp_cv_OK_RF_"){
        p4 = "0.rda"
      } else {
        p4 = "10.rda"
      }
      # Complete file name
      f_name = paste("Results/bcNitrate/",i1, i2, i3, p4, sep = "")
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
          RMSE_values = c()
          diff_values = c()
          for (i4 in 1:length(sp_cv_loo_OK_RF$represampling[[1]])){
            pred_value = sp_cv_loo_OK_RF$error_fold[[1]][[i4]]$test$pred
            pred_id = sp_cv_loo_OK_RF$represampling[[1]][[i4]]$test
            obs_value = NuM_L[pred_id, "bcNitrate"]
            RMSE = sqrt((obs_value-pred_value)^2)
            differ = obs_value-pred_value
            RMSE_values = append(RMSE_values, RMSE)
            diff_values = append(diff_values, differ)
          }
          NuM_L_loo_OK_RF = append(NuM_L_loo_OK_RF, mean(RMSE_values))
          NuM_L_loo_OK_RF_b = append(NuM_L_loo_OK_RF_b, mean(diff_values))
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
          RMSE_values = c()
          diff_values = c()
          for (i4 in 1:length(sp_cv_loo_OK_RF$represampling[[1]])){
            pred_value = sp_cv_loo_OK_RF$error_fold[[1]][[i4]]$test$pred
            pred_id = sp_cv_loo_OK_RF$represampling[[1]][[i4]]$test
            obs_value = WuS_SuB[pred_id, "bcNitrate"]
            RMSE = sqrt((obs_value-pred_value)^2)
            differ = obs_value-pred_value
            RMSE_values = append(RMSE_values, RMSE)
            diff_values = append(diff_values, differ)
          }
          WuS_SuB_loo_OK_RF = append(WuS_SuB_loo_OK_RF, mean(RMSE_values))
          WuS_SuB_loo_OK_RF_b = append(WuS_SuB_loo_OK_RF_b, mean(diff_values))
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
## End (save mean RMSE values in vectors)
####


####
## Plot SPEP
##

# Create dfs
NuM_L_mRMSE_df_bc = data.frame(NuM_L_UK, NuM_L_RF_oob_OK, NuM_L_loo_OK_RF, 
                            NuM_L_RF_MEv, NuM_L_RFSI, NuM_L_RF, NuM_L_bRF,
                            NuM_L_MLR, NuM_L_OK_RF)

WuS_SuB_mRMSE_df_bc = data.frame(WuS_SuB_UK, WuS_SuB_RF_oob_OK, WuS_SuB_loo_OK_RF, 
                              WuS_SuB_RF_MEv, WuS_SuB_RFSI, WuS_SuB_RF, WuS_SuB_bRF,
                              WuS_SuB_MLR, WuS_SuB_OK_RF)

NuM_L_mBias_df_bc = data.frame(NuM_L_UK_b, NuM_L_RF_oob_OK_b, NuM_L_loo_OK_RF_b, 
                            NuM_L_RF_MEv_b, NuM_L_RFSI_b, NuM_L_RF_b, NuM_L_bRF_b,
                            NuM_L_MLR_b, NuM_L_OK_RF_b)

WuS_SuB_mBias_df_bc = data.frame(WuS_SuB_UK_b, WuS_SuB_RF_oob_OK_b, WuS_SuB_loo_OK_RF_b, 
                              WuS_SuB_RF_MEv_b, WuS_SuB_RFSI_b, WuS_SuB_RF_b, WuS_SuB_bRF_b,
                              WuS_SuB_MLR_b, WuS_SuB_OK_RF_b)

# Save dfs
#save(NuM_L_mBias_df_bc, WuS_SuB_mBias_df_bc, file = "mBias_dfs_bc.rda")

(NuM_L_mRMSE_df_bc$NuM_L_OK_RF - NuM_L_mRMSE_df_bc$NuM_L_RF) / NuM_L_mRMSE_df_bc$NuM_L_RF *100
(WuS_SuB_mRMSE_df_bc$WuS_SuB_OK_RF - WuS_SuB_mRMSE_df_bc$WuS_SuB_RF) / WuS_SuB_mRMSE_df_bc$WuS_SuB_RF *100

(NuM_L_mRMSE_df_bc$NuM_L_RF - NuM_L_mRMSE_df_bc$NuM_L_bRF) / NuM_L_mRMSE_df_bc$NuM_L_bRF *100
(WuS_SuB_mRMSE_df_bc$WuS_SuB_RF - WuS_SuB_mRMSE_df_bc$WuS_SuB_bRF) / WuS_SuB_mRMSE_df_bc$WuS_SuB_bRF *100


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
mtext(expression("Mittlerer RMSE [Box-Cox]"),
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
     ylim = c(0, 2.75),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 2.236068, 10.95445, 21.21320, 30.82207, 40.62019, 60.41523, 
               80.31189, 100.24969, 130.19217, 160.15617, 200.12496), 
     labels = c(0, 5, 120, 450, 950, 1650, 3650, 6450, 10050, 16950, 25650, 
                40050))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot mRMSE values
for (i in 1:ncol(NuM_L_mRMSE_df_bc)){
  # Points
  points(x = NuM_L_mean_sqrt_nn_dist,
         y = NuM_L_mRMSE_df_bc[,i],
         pch = 21,
         bg = col_vec[i])
  # Line
  lines(x = NuM_L_mean_sqrt_nn_dist,
        y = NuM_L_mRMSE_df_bc[,i],
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
     ylim = c(0, 2),
     #yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 5, 11.18034, 21.21320, 30.82207, 40.62019, 60.41523, 
               80.31189, 100.24969, 130.19217, 160.15617, 200.12496), 
     labels = c(0, 25, 125, 450, 950, 1650, 3650, 6450, 10050, 16950, 25650, 
                40050))
#axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# Plot mRMSE values
for (i in 1:ncol(WuS_SuB_mRMSE_df_bc)){
  # Points
  points(x = WuS_SuB_mean_sqrt_nn_dist,
         y = WuS_SuB_mRMSE_df_bc[,i],
         pch = 21,
         bg = col_vec[i])
  # Line
  lines(x = WuS_SuB_mean_sqrt_nn_dist,
        y = WuS_SuB_mRMSE_df_bc[,i],
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
for(i in 1:nrow(WuS_SuB_mRMSE_df_bc)){
  lR_vec_WuS_SuB = append(lR_vec_WuS_SuB, min(WuS_SuB_mRMSE_df_bc[i,]))
}
for(i in 1:nrow(NuM_L_mRMSE_df_bc)){
  lR_vec_NuM_L = append(lR_vec_NuM_L, min(NuM_L_mRMSE_df_bc[i,]))
}
# Create dfs
Diff_to_lR_NuM_L = NuM_L_mRMSE_df_bc - lR_vec_NuM_L
Diff_to_lR_WuS_SuB = WuS_SuB_mRMSE_df_bc - lR_vec_WuS_SuB

# First plot
par(mar = c(2, 0, 0, 0)) # bottom, left, top, right margins
plot(NULL, ylab = "", bty = "n", 
     xlim = c(0, 0.1), ylim = c(0, 0.1), xaxt = "n", yaxt = "n")
mtext(expression("Differenz zum niedrigsten mittleren RMSE [Box-Cox]"),
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
     ylim = c(0, 1.8),
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
     xlab = "Vorhersagedistanz [m]", 
     #ylab = "Mittlerer RMSE",
     xlim = c(7, 194),
     xaxt = "n",
     ylim = c(0, 1),
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