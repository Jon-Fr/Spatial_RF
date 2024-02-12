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
f_names_vec1 = c("WuS_SuB_sp_cv_bRF_", "WuS_SuB_sp_cv_MLR_", "WuS_SuB_sp_cv_RF_",
                "WuS_SuB_sp_cv_RF_MEv_", "WuS_SuB_sp_cv_RFSI_", 
                "WuS_SuB_sp_cv_loo_OK_RF_", "WuS_SuB_sp_cv_OK_RF_",
                "WuS_SuB_sp_cv_RF_oob_OK_", "WuS_SuB_sp_cv_UK_")

# Vector of the second part of the file names
f_names_vec2 = c("0_+50_10.rda", "100_+50_10.rda", "400_+100_10.rda",
                 "900_+100_10.rda", "1600_+100_10.rda", "3600_+100_10.rda", 
                 "6400_+100_10.rda", "10000_+100_10.rda", "16900_+100_10.rda",
                 "25600_+100_10.rda", "40000_+100_10.rda")

# Load results and store the RMSE values in the vectors
for (i1 in f_names_vec1){
  for (i2 in f_names_vec2){
    # Complete file name
    f_name = paste("Results/Wus_SuB/",i1, i2, sep = "")
    # Load file
    load(f_name)
    ## Get RMSE values and mean nearest neighbor distance
    if (i1 == "WuS_SuB_sp_cv_bRF_"){
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
      
    } else if (i1 == "WuS_SuB_sp_cv_MLR_"){
      RMSE = sp_cv_MLR$error_rep$test_rmse
      MLR_RMSE_vec = append(MLR_RMSE_vec, RMSE)
      
    } else if (i1 == "WuS_SuB_sp_cv_RF_"){
      RMSE = sp_cv_RF$error_rep$test_rmse
      RF_RMSE_vec = append(RF_RMSE_vec, RMSE)
      
    } else if (i1 == "WuS_SuB_sp_cv_RF_MEv_"){
      RMSE = sp_cv_RF_MEv$error_rep$test_rmse
      RF_MEv_RMSE_vec = append(RF_MEv_RMSE_vec, RMSE)
      
    } else if (i1 == "WuS_SuB_sp_cv_RFSI_"){
      RMSE = sp_cv_RFSI$error_rep$test_rmse
      RFSI_RMSE_vec = append(RFSI_RMSE_vec, RMSE)
      
    } else if (i1 == "WuS_SuB_sp_cv_OK_RF_"){
      RMSE = sp_cv_OK_RF$error_rep$test_rmse
      OK_RF_RMSE_vec = append(OK_RF_RMSE_vec, RMSE)
      
    } else if (i1 == "WuS_SuB_sp_cv_loo_OK_RF_"){
      RMSE = sp_cv_llo_OK_RF$error_rep$test_rmse
      loo_OK_RF_RMSE_vec = append(loo_OK_RF_RMSE_vec, RMSE)
      
    } else if (i1 == "WuS_SuB_sp_cv_RF_oob_OK_"){
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
par(mfrow = c(1,1))
# Margins
par(mar = c(4.1,3.9,0.1,0.1)) # bottom, left, top, right
# Create a blank plotting space
plot(x = 1,                 
     xlab = "Vorhersagedistanz [m]", 
     ylab = "RMSE",
     xlim = c(7, 200),
     xaxt = "n",
     ylim = c(1.2, 2.6),
     yaxt = "n",
     main = "",
     type = "n")
axis(1, at = c(0, 5, 11.18, 21.21, 30.82, 40, 60, 80, 100, 130, 160, 200), 
     labels = c(0, 25, 125, 450, 950, 1600, 3600, 6400, 10000, 16900, 25600, 40000),
     cex.axis = 0.75)
axis(2, at = c(1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6))

# MLR RMSE values
points(x = mean_nn_sqrt_dist_vec,
       y = MLR_RMSE_vec,
       pch = 16,
       col = ("#000000"))

lines(x = mean_nn_sqrt_dist_vec,
      y = MLR_RMSE_vec,
      col = ("#000000"),
      lty = 2, lwd = 2)

# UK_RMSE values
points(x = mean_nn_sqrt_dist_vec,
       y = UK_RMSE_vec,
       pch = 21,
       bg = ("#666666"))

lines(x = mean_nn_sqrt_dist_vec,
      y = UK_RMSE_vec,
      col = ("#666666"),
      lty = 2, lwd = 2)

# bRF RMSE values
points(x = mean_nn_sqrt_dist_vec,
       y = bRF_RMSE_vec,
       pch = 21,
       bg = ("#CCCCCC"))

lines(x = mean_nn_sqrt_dist_vec,
      y = bRF_RMSE_vec,
      col = ("#CCCCCC"),
      lty = 2, lwd = 2)

# RF RMSE values
points(x = mean_nn_sqrt_dist_vec,
       y = RF_RMSE_vec,
       pch = 21,
       bg = ("#33CC33"))

lines(x = mean_nn_sqrt_dist_vec,
      y = RF_RMSE_vec,
      col = ("#33CC33"),
      lty = 2, lwd = 2)

# RFMEv RMSE values
points(x = mean_nn_sqrt_dist_vec,
       y = RF_MEv_RMSE_vec,
       pch = 21,
       bg = ("#FF0000"))

lines(x = mean_nn_sqrt_dist_vec,
      y = RF_MEv_RMSE_vec,
      col = ("#FF0000"),
      lty = 2, lwd = 2)

# RF_oob_OK RMSE values
points(x = mean_nn_sqrt_dist_vec,
       y = RF_oob_OK_RMSE_vec,
       pch = 21,
       bg = ("#9900FF"))

lines(x = mean_nn_sqrt_dist_vec,
      y = RF_oob_OK_RMSE_vec,
      col = ("#9900FF"),
      lty = 2, lwd = 2)

# OK_RF RMSE values
points(x = mean_nn_sqrt_dist_vec,
       y = OK_RF_RMSE_vec,
       pch = 21,
       bg = ("#66FFFF"))

lines(x = mean_nn_sqrt_dist_vec,
      y = OK_RF_RMSE_vec,
      col = ("#66FFFF"),
      lty = 2, lwd = 2)

# loo_OK_RF RMSE values
points(x = mean_nn_sqrt_dist_vec,
       y = loo_OK_RF_RMSE_vec,
       pch = 21,
       bg = ("#0000FF"))

lines(x = mean_nn_sqrt_dist_vec,
      y = loo_OK_RF_RMSE_vec,
      col = ("#0000FF"),
      lty = 2, lwd = 2)

# RFSI RMSE values
points(x = mean_nn_sqrt_dist_vec,
       y = RFSI_RMSE_vec,
       pch = 21,
       bg = ("#FF9933"))

lines(x = mean_nn_sqrt_dist_vec,
      y = RFSI_RMSE_vec,
      col = ("#FF9933"),
      lty = 2, lwd = 2)

# Legend
legend(x = "bottomright", cex = 0.9,
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



################################################################################
## Spatial variable importance profiles (SVIPs) 
################################################################################

####
## Store the importance/RMSE difference values in vectors
##

# cAckerland
cA_MLR_vec = c()
cA_bRF_vec = c()
cA_RF_vec = c() 
cA_RFSI_vec = c()
cA_RF_MEv_vec = c()
cA_OK_RF_vec = c()
cA_loo_OK_RF_vec = c()
cA_RF_oob_OK_vec = c()
cA_UK_vec = c()

# X
X_MLR_vec = c()
X_bRF_vec = c()
X_RF_vec = c() 
X_RFSI_vec = c()
X_RF_MEv_vec = c()
X_OK_RF_vec = c()
X_loo_OK_RF_vec = c()
X_RF_oob_OK_vec = c()
X_UK_vec = c()

# Y
Y_MLR_vec = c()
Y_bRF_vec = c()
Y_RF_vec = c() 
Y_RFSI_vec = c()
Y_RF_MEv_vec = c()
Y_OK_RF_vec = c()
Y_loo_OK_RF_vec = c()
Y_RF_oob_OK_vec = c()
Y_UK_vec = c()

# humus
hu_MLR_vec = c()
hu_bRF_vec = c()
hu_RF_vec = c() 
hu_RFSI_vec = c()
hu_RF_MEv_vec = c()
hu_OK_RF_vec = c()
hu_loo_OK_RF_vec = c()
hu_RF_oob_OK_vec = c()
hu_UK_vec = c()

# tc315
tc315_RF_vec = c() 
tc315_RFSI_vec = c()
tc315_RF_MEv_vec = c()
tc315_OK_RF_vec = c()
tc315_loo_OK_RF_vec = c()
tc315_RF_oob_OK_vec = c()

# Mean nearest neighbor distance vector
mean_nn_dist_vec = c()

# Vector of the first part of the file names
f_names_vec1 = c("WuS_SuB_sp_cv_bRF_", "WuS_SuB_sp_cv_MLR_", "WuS_SuB_sp_cv_RF_",
                 "WuS_SuB_sp_cv_RF_MEv_", "WuS_SuB_sp_cv_RFSI_", 
                 "WuS_SuB_sp_cv_loo_OK_RF_", "WuS_SuB_sp_cv_OK_RF_",
                 "WuS_SuB_sp_cv_RF_oob_OK_", "WuS_SuB_sp_cv_UK_")

# Vector of the second part of the file names
f_names_vec2 = c("0_+50_10.rda", "100_+50_10.rda", "400_+100_10.rda",
                 "900_+100_10.rda", "1600_+100_10.rda", "3600_+100_10.rda", 
                 "6400_+100_10.rda", "10000_+100_10.rda", "16900_+100_10.rda",
                 "25600_+100_10.rda", "40000_+100_10.rda")

# Load results and store the importance values in the vectors
for (i1 in f_names_vec1){
  for (i2 in f_names_vec2){
    # Complete file name
    f_name = paste("Results/Wus_SuB/",i1, i2, sep = "")
    # Load file
    load(f_name)
    ## Get importance values and mean nearest neighbor distance
    if (i1 == "WuS_SuB_sp_cv_bRF_"){
      # Importance
      imp_bRF = summary(sp_cv_bRF$importance)
      # Convert decrease to increase
      imp_bRF$mean.rmse = imp_bRF$mean.rmse * -1
      # Imp values of the variables  
      cA_bRF_vec = append(cA_bRF_vec, imp_bRF["cAckerland", "mean.rmse"])
      X_bRF_vec = append(X_bRF_vec, imp_bRF["X", "mean.rmse"])
      Y_bRF_vec = append(Y_bRF_vec, imp_bRF["Y", "mean.rmse"])
      hu_bRF_vec = append(hu_bRF_vec, imp_bRF["humus", "mean.rmse"])
      
      # nn distance
      nn_distances = c()
      for (i3 in 1:length(sp_cv_bRF$error_fold[[1]])){
        nn_dist = sp_cv_bRF$error_fold[[1]][[i3]]$distance
        nn_distances = append(nn_distances, nn_dist)
      }
      mean_nn_dist_vec = append(mean_nn_dist_vec, mean(nn_distances))
      
    } else if (i1 == "WuS_SuB_sp_cv_MLR_"){
      imp_MLR = summary(sp_cv_MLR$importance)
      imp_MLR$mean.rmse = imp_MLR$mean.rmse * -1
      cA_MLR_vec = append(cA_MLR_vec, imp_MLR["cAckerland", "mean.rmse"])
      X_MLR_vec = append(X_MLR_vec, imp_MLR["X", "mean.rmse"])
      Y_MLR_vec = append(Y_MLR_vec, imp_MLR["Y", "mean.rmse"])
      hu_MLR_vec = append(hu_MLR_vec, imp_MLR["humus", "mean.rmse"])

    } else if (i1 == "WuS_SuB_sp_cv_RF_"){
      imp_RF = summary(sp_cv_RF$importance)
      imp_RF$mean.rmse = imp_RF$mean.rmse * -1
      cA_RF_vec = append(cA_RF_vec,imp_RF["cAckerland", "mean.rmse"])
      X_RF_vec = append(X_RF_vec, imp_RF["X", "mean.rmse"])
      Y_RF_vec = append(Y_RF_vec, imp_RF["Y", "mean.rmse"])
      hu_RF_vec = append(hu_RF_vec, imp_RF["humus", "mean.rmse"])
      tc315_RF_vec = append(tc315_RF_vec, imp_RF["humus", "mean.rmse"])

    } else if (i1 == "WuS_SuB_sp_cv_RF_MEv_"){
      imp_RF_MEv = summary(sp_cv_RF_MEv$importance)
      imp_RF_MEv$mean.rmse = imp_RF_MEv$mean.rmse * -1
      cA_RF_MEv_vec = append(cA_RF_MEv_vec,imp_RF_MEv["cAckerland", "mean.rmse"])
      X_RF_MEv_vec = append(X_RF_MEv_vec, imp_RF_MEv["X", "mean.rmse"])
      Y_RF_MEv_vec = append(Y_RF_MEv_vec, imp_RF_MEv["Y", "mean.rmse"])
      hu_RF_MEv_vec = append(hu_RF_MEv_vec, imp_RF_MEv["humus", "mean.rmse"])
      tc315_RF_MEv_vec = append(tc315_RF_MEv_vec, imp_RF_MEv["humus", "mean.rmse"])
      
    } else if (i1 == "WuS_SuB_sp_cv_RFSI_"){
      imp_RFSI = summary(sp_cv_RFSI$importance)
      imp_RFSI$mean.rmse = imp_RFSI$mean.rmse * -1
      cA_RFSI_vec = append(cA_RFSI_vec,imp_RFSI["cAckerland", "mean.rmse"])
      X_RFSI_vec = append(X_RFSI_vec, imp_RFSI["X", "mean.rmse"])
      Y_RFSI_vec = append(Y_RFSI_vec, imp_RFSI["Y", "mean.rmse"])
      hu_RFSI_vec = append(hu_RFSI_vec, imp_RFSI["humus", "mean.rmse"])
      tc315_RFSI_vec = append(tc315_RFSI_vec, imp_RFSI["humus", "mean.rmse"])
      
    } else if (i1 == "WuS_SuB_sp_cv_OK_RF_"){
      imp_OK_RF = summary(sp_cv_OK_RF$importance)
      imp_OK_RF$mean.rmse = imp_OK_RF$mean.rmse * -1
      cA_OK_RF_vec = append(cA_OK_RF_vec, imp_OK_RF["cAckerland", "mean.rmse"])
      X_OK_RF_vec = append(X_OK_RF_vec, imp_OK_RF["X", "mean.rmse"])
      Y_OK_RF_vec = append(Y_OK_RF_vec, imp_OK_RF["Y", "mean.rmse"])
      hu_OK_RF_vec = append(hu_OK_RF_vec, imp_OK_RF["humus", "mean.rmse"])
      tc315_OK_RF_vec = append(tc315_OK_RF_vec, imp_OK_RF["humus", "mean.rmse"])
      
    } else if (i1 == "WuS_SuB_sp_cv_loo_OK_RF_"){
      imp_loo_OK_RF = summary(sp_cv_llo_OK_RF$importance)
      imp_loo_OK_RF$mean.rmse = imp_loo_OK_RF$mean.rmse * -1
      cA_loo_OK_RF_vec = append(cA_loo_OK_RF_vec,imp_loo_OK_RF["cAckerland", "mean.rmse"])
      X_loo_OK_RF_vec = append(X_loo_OK_RF_vec, imp_loo_OK_RF["X", "mean.rmse"])
      Y_loo_OK_RF_vec = append(Y_loo_OK_RF_vec, imp_loo_OK_RF["Y", "mean.rmse"])
      hu_loo_OK_RF_vec = append(hu_loo_OK_RF_vec, imp_loo_OK_RF["humus", "mean.rmse"])
      tc315_loo_OK_RF_vec = append(tc315_loo_OK_RF_vec, imp_loo_OK_RF["humus", "mean.rmse"])
      
    } else if (i1 == "WuS_SuB_sp_cv_RF_oob_OK_"){
      imp_RF_oob_OK = summary(sp_cv_RF_oob_OK$importance)
      imp_RF_oob_OK$mean.rmse = imp_RF_oob_OK$mean.rmse * -1
      cA_RF_oob_OK_vec = append(cA_RF_oob_OK_vec,imp_RF_oob_OK["cAckerland", "mean.rmse"])
      X_RF_oob_OK_vec = append(X_RF_oob_OK_vec, imp_RF_oob_OK["X", "mean.rmse"])
      Y_RF_oob_OK_vec = append(Y_RF_oob_OK_vec, imp_RF_oob_OK["Y", "mean.rmse"])
      hu_RF_oob_OK_vec = append(hu_RF_oob_OK_vec, imp_RF_oob_OK["humus", "mean.rmse"])
      tc315_RF_oob_OK_vec = append(tc315_RF_oob_OK_vec, imp_RF_oob_OK["humus", "mean.rmse"])
      
    } else{
      imp_UK = summary(sp_cv_UK$importance)
      imp_UK$mean.rmse = imp_UK$mean.rmse * -1
      cA_UK_vec = append(cA_UK_vec,imp_UK["cAckerland", "mean.rmse"])
      X_UK_vec = append(X_UK_vec, imp_UK["X", "mean.rmse"])
      Y_UK_vec = append(Y_UK_vec, imp_UK["Y", "mean.rmse"])
      hu_UK_vec = append(hu_UK_vec, imp_UK["humus", "mean.rmse"])
    }
  }
}

# Scale the nn dist vec 
mean_nn_sqrt_dist_vec = sqrt(mean_nn_dist_vec)
  
##
## End (store the importance/RMSE difference values in vectors)
####

####
## Plot SVIPs
##

# Create importance data frames 
cAckerland_df = data.frame(cA_UK_vec, cA_RF_oob_OK_vec, cA_loo_OK_RF_vec, 
                        cA_RF_MEv_vec, cA_RFSI_vec, cA_RF_vec, cA_bRF_vec, 
                        cA_MLR_vec, cA_OK_RF_vec)

humus_df = data.frame(hu_UK_vec, hu_RF_oob_OK_vec, hu_loo_OK_RF_vec, 
                        hu_RF_MEv_vec, hu_RFSI_vec, hu_RF_vec, hu_bRF_vec, 
                        hu_MLR_vec, hu_OK_RF_vec)

X_df = data.frame(X_UK_vec, X_RF_oob_OK_vec, X_loo_OK_RF_vec, 
               X_RF_MEv_vec, X_RFSI_vec, X_RF_vec, X_bRF_vec, 
               X_MLR_vec, X_OK_RF_vec)

Y_df = data.frame(Y_UK_vec, Y_RF_oob_OK_vec, Y_loo_OK_RF_vec, 
               Y_RF_MEv_vec, Y_RFSI_vec, Y_RF_vec, Y_bRF_vec, 
               Y_MLR_vec, Y_OK_RF_vec)

tc315_df = data.frame( tc315_RF_oob_OK_vec, tc315_loo_OK_RF_vec, 
               tc315_RF_MEv_vec, tc315_RFSI_vec, tc315_RF_vec, tc315_OK_RF_vec)

# Colors vector
col_vec = c("#666666", "#9900FF", "#0000FF", "#FF0000", "#FF9933", "#33CC33", 
            "#CCCCCC", "#000000", "#66FFFF")

col_vec_tc315 = c("#9900FF", "#0000FF", "#FF0000", "#FF9933", "#33CC33", 
                  "#66FFFF")


## Plot loops

# cAckerland and humus 
# Plot settings 
par(mfrow=c(2,1))
par(mar = c(0.4,3.9,0.1,0.1)) # bottom, left, top, right margins

# Create a blank plotting space
plot(x = 1,                 
     xlab = "", 
     ylab = "Mittlere Zunahme des RMSE",
     xlim = c(7, 200),
     xaxt = "n",
     ylim = c(0, 1.1),
     #yaxt = "n",
     main = "",
     type = "n")
#axis(1, at = c(0, 5, 11.18, 21.21, 30.82, 40, 60, 80, 100, 130, 160, 200), 
#     labels = c(0, 25, 125, 450, 950, 1600, 3600, 6400, 10000, 16900, 25600, 40000),
#     cex.axis = 0.75)
legend(x = "topright" , legend = c("a)"), bty = "n")
legend(x = "bottomright", cex = 0.75, ncol = 3,
       legend = c("MLR", "UK", "RF", "RF-K", "RF-MEv", "RF-oob-OK", "OK-RF", 
                  "RF-loo-OK", "RFSI"),
       fill = c("#000000", "#666666", "#CCCCCC", "#0000FF", "#FF0000", "#9900FF",
                "#66FFFF", "#0000FF", "#FF9933"))
for (i2 in 1:ncol(cAckerland_df)){
  # Imp values
  points(x = mean_nn_sqrt_dist_vec,
         y = cAckerland_df[,i2],
         pch = 21,
         bg = (col_vec[i2]))
  lines(x = mean_nn_sqrt_dist_vec,
        y = cAckerland_df[,i2],
        col = (col_vec[i2]),
        lty = 2)
}  
par(mar = c(4.1,3.9,0.1,0.1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1,                 
     xlab = "Vorhersagedistanz [m]", 
     ylab = "Mittlere Zunahme des RMSE",
     xlim = c(7, 200),
     xaxt = "n",
     ylim = c(0, 0.2),
     #yaxt = "n",
     main = "",
     type = "n")
legend(x = "topright" , legend = c("b)"), bty = "n")
axis(1, at = c(0, 5, 11.18, 21.21, 30.82, 40, 60, 80, 100, 130, 160, 200), 
     labels = c(0, 25, 125, 450, 950, 1600, 3600, 6400, 10000, 16900, 25600, 40000),
     cex.axis = 0.75)
for (i2 in 1:ncol(humus_df)){
  # Imp values
  points(x = mean_nn_sqrt_dist_vec,
         y = humus_df[,i2],
         pch = 21,
         bg = (col_vec[i2]))
  lines(x = mean_nn_sqrt_dist_vec,
        y = humus_df[,i2],
        col = (col_vec[i2]),
        lty = 2)
}      


# Coordinates
# Plot settings 
par(mfrow=c(3,1))
par(mar = c(0.4,3.9,0.1,0.1)) # bottom, left, top, right margins

# Create a blank plotting space
plot(x = 1,                 
     xlab = "", 
     ylab = "Mittlere Zunahme des RMSE",
     xlim = c(7, 200),
     xaxt = "n",
     ylim = c(0, 1.4),
     #yaxt = "n",
     main = "",
     type = "n")
legend(x = "topright" , legend = c("a)"), bty = "n")
legend(x = "top", cex = 0.88, ncol = 2,
       legend = c("MLR", "UK", "RF", "RF-K", "RF-MEv", "RF-oob-OK", "OK-RF", 
                  "RF-loo-OK", "RFSI"),
       fill = c("#000000", "#666666", "#CCCCCC", "#0000FF", "#FF0000", "#9900FF",
                "#66FFFF", "#0000FF", "#FF9933"))
#axis(1, at = c(0, 5, 11.18, 21.21, 30.82, 40, 60, 80, 100, 130, 160, 200), 
#     labels = c(0, 25, 125, 450, 950, 1600, 3600, 6400, 10000, 16900, 25600, 40000),
#     cex.axis = 0.75)
for (i2 in 1:ncol(X_df)){
  # Imp values
  points(x = mean_nn_sqrt_dist_vec,
         y = X_df[,i2],
         pch = 21,
         bg = (col_vec[i2]))
  lines(x = mean_nn_sqrt_dist_vec,
        y = X_df[,i2],
        col = (col_vec[i2]),
        lty = 2)
}      
# Create a blank plotting space
plot(x = 1,                 
     xlab = "", 
     ylab = "Mittlere Zunahme des RMSE",
     xlim = c(7, 200),
     xaxt = "n",
     ylim = c(0, 1.1),
     #yaxt = "n",
     main = "",
     type = "n")
legend(x = "topright" , legend = c("b)"), bty = "n")
#axis(1, at = c(0, 5, 11.18, 21.21, 30.82, 40, 60, 80, 100, 130, 160, 200), 
#     labels = c(0, 25, 125, 450, 950, 1600, 3600, 6400, 10000, 16900, 25600, 40000),
#     cex.axis = 0.75)
for (i2 in 1:ncol(Y_df)){
  # Imp values
  points(x = mean_nn_sqrt_dist_vec,
         y = Y_df[,i2],
         pch = 21,
         bg = (col_vec[i2]))
  lines(x = mean_nn_sqrt_dist_vec,
        y = Y_df[,i2],
        col = (col_vec[i2]),
        lty = 2)
}      
par(mar = c(4.1,3.9,0.1,0.1)) # bottom, left, top, right margins
# Create a blank plotting space
plot(x = 1,                 
     xlab = "Vorhersagedistanz [m]", 
     ylab = "Mittlere Zunahme des RMSE",
     xlim = c(7, 200),
     xaxt = "n",
     ylim = c(0, 0.12),
     #yaxt = "n",
     main = "",
     type = "n")
legend(x = "topright" , legend = c("c)"), bty = "n")
axis(1, at = c(0, 5, 11.18, 21.21, 30.82, 40, 60, 80, 100, 130, 160, 200), 
     labels = c(0, 25, 125, 450, 950, 1600, 3600, 6400, 10000, 16900, 25600, 40000),
     cex.axis = 0.75)
for (i2 in 1:ncol(tc315_df)){
  # Imp values
  points(x = mean_nn_sqrt_dist_vec,
         y = tc315_df[,i2],
         pch = 21,
         bg = (col_vec_tc315[i2]))
  lines(x = mean_nn_sqrt_dist_vec,
        y = tc315_df[,i2],
        col = (col_vec_tc315[i2]),
        lty = 2)
}            
##
## End (plot SVIPs)
####
################################################################################
## End (SVIPs) 
################################################################################