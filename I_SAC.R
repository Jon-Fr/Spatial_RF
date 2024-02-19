################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sp")
p_load("sf")
p_load("gstat")  
p_load("automap")

# Fewer decimal places, apply penalty on exponential notation 
options("scipen"= 999, "digits"=4)

# Load data set formulas 
load("Data/WuS_SuB.rda")
load("Data/NuM_L.rda")
fo_lm_NuM_L = as.formula(subMittelwert ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                           nfk + humus + cAckerland + log10_gwn + agrum_log10_restime + 
                           agrum_log10_gwn + agrum_log10_geschw + Ackerland + lbm_class_Gruenland + 
                           lbm_class_Unbewachsen + lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + 
                           aea20_2 + aea20_8 + aea20_12 + X + Y)
fo_lm_WuS_SuB = as.formula(subMittelwert ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                             nfk + humus + cAckerland + log10_gwn + agrum_log10_restime + 
                             agrum_log10_gwn + agrum_log10_geschw + Ackerland + lbm_class_Gruenland + 
                             lbm_class_Unbewachsen + lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + 
                             aea20_1 + aea20_2 + aea20_12 + aea20_13 + X + Y)
#N = NuM_L
#W = WuS_SuB

# Get information about the prediction distance 
pd_df_N = info_d_NuM_L$predDist_df
third_quartile_N = quantile(x = pd_df_N$lyr.1, probs = c(0.75))
tq_pd_N = third_quartile_N
max_pd_N = max(pd_df_N$lyr.1)
mean_pd_N = mean(pd_df_N$lyr.1)
med_pd_N = median(pd_df_N$lyr.1)
hist_N = hist(pd_df_N$lyr.1, breaks = (135))
color_N = ifelse(hist_N$breaks <=med_pd_N, "ivory", ifelse(hist_N$breaks <=third_quartile_N & hist_N$breaks > med_pd_N, "gray39", "#333333"))
par(mar = c(3.1,4.4,1,0.5)) # bottom, left, top, right margins
plot(hist_N, xlab = "", ylab = expression("Anzahl [100 m"^2 *" Rasterzellen]"), 
     main = "", col = color_N, xaxt = "n", yaxt = "n", xlim = c(0, 175000))
axis(1, at = c(0, 25000, 50000, 75000, 100000, 125000, 150000, 175000))
axis(2, at = c(0, 500000, 1000000, 1500000, 2000000, 2500000))
legend(x = "topright" , legend = c("a)"), bty = "n")

pd_df_W = info_d_WuS_SuB$predDist_df
third_quartile_W = quantile(x = pd_df_W$lyr.1, probs = c(0.75))
tq_pd_W = third_quartile_W
max_pd_W = max(pd_df_W$lyr.1)
mean_pd_W = mean(pd_df_W$lyr.1)
med_pd_W = median(pd_df_W$lyr.1)
hist_W = hist(pd_df_W$lyr.1, breaks = (173))
color_W = ifelse(hist_W$breaks <=med_pd_W, "ivory", ifelse(hist_W$breaks <=third_quartile_W & hist_W$breaks > med_pd_W, "gray39", "#333333"))
par(mar = c(4.1,4.4,0,0.5)) # bottom, left, top, right margins
plot(hist_W, xlab = "Entfernung [m]", ylab = expression("Anzahl [100 m"^2 *" Rasterzellen]"), 
     main = "", col = color_W, xaxt = "n", yaxt = "n",  xlim = c(0, 175000), ylim = c(0, 1250000))
axis(1, at = c(0, 25000, 50000, 75000, 100000, 125000, 150000, 175000))
axis(2, at = c(0, 250000, 500000, 750000, 1000000, 1250000))
legend(x = "topright" , legend = c("b)"), bty = "n")

par(mfrow = c(2,1))

# Create spatial points dfs 
sp_df_W = sp::SpatialPointsDataFrame(WuS_SuB[,c("X","Y")], WuS_SuB) 
sp_df_N = sp::SpatialPointsDataFrame(NuM_L[,c("X","Y")], NuM_L)  
################################################################################
## End (preparation)
################################################################################


################################################################################
## Investigation of the spatial autocorrelation
################################################################################
####
## Empirical semivariogram (EmSv) of the variable of interest
##
emp_svario_N = gstat::variogram(subMittelwert ~1, data=sp_df_N, cutoff = 150000, 
                              width = 100) 
plot(emp_svario_N$dist, emp_svario_N$gamma)

emp_svario_W = gstat::variogram(subMittelwert ~1, data=sp_df_W, cutoff = 175000, 
                                width = 100) 
plot(emp_svario_W$dist, emp_svario_W$gamma)

# Fit variogram model
vmf_N = automap::autofitVariogram(formula = subMittelwert ~ 1, 
                                         input_data = sp_df_N, 
                                          model = c("Mat", "Exp"),
                                         kappa = c(seq(0.1, 0.4, 0.1)),
                                         fix.values = c(0, NA, NA),
                                         verbose = TRUE)


vmf_W = automap::autofitVariogram(formula = subMittelwert ~ 1, 
                                        input_data = sp_df_W, 
                                        model = c("Mat", "Exp"),
                                        kappa = c(seq(0.1, 0.4, 0.1)),
                                        fix.values = c(0, NA, NA),
                                        verbose = TRUE)


# Get points to plot the variogram model 
vm_points_N = gstat::variogramLine(vmf_N["var_model"]$var_model, maxdist = 50000)
vm_points_W = gstat::variogramLine(vmf_W["var_model"]$var_model, maxdist = 50000)

# Plots
par(mar = c(4.1,4,0.1,0.5)) # bottom, left, top, right margins

plot(emp_svario_N$dist, emp_svario_N$gamma, xlab = "", 
     ylab = "Semivarianz", xlim = c(1200,49000), ylim = c(0, 3500))
lines(x = vm_points_N$dist,
      y = vm_points_N$gamma,
      col = ("#0000FF"),
      lty = 1)
legend(x = "bottomright" , legend = c("a)"), bty = "n")

plot(emp_svario_W$dist, emp_svario_W$gamma, xlab = "Entfernung [m]", 
     ylab = "Semivarianz", xlim = c(1200,49000), ylim = c(0, 400))
lines(x = vm_points_W$dist,
      y = vm_points_W$gamma,
      col = ("#0000FF"),
      lty = 1)
legend(x = "bottomright" , legend = c("b)"), bty = "n")
##
## End (EmSv of the variable of interest)
####


####
## EmSv of the residuals of a multiple linear regression model
##

# Create MLR model
MLR_model_N = lm(fo_lm_NuM_L, data=NuM_L)
MLR_model_W = lm(fo_lm_WuS_SuB, data=WuS_SuB)
# Add residuals to sp_df
sp_df_N$mlr_resi = MLR_model_N$residuals
sp_df_W$mlr_resi = MLR_model_W$residuals

emp_svario_resi_N = gstat::variogram(mlr_resi~1, data=sp_df_N, cutoff = 50000, 
                                   width = 100) 
emp_svario_resi_W = gstat::variogram(mlr_resi~1, data=sp_df_W, cutoff = 50000, 
                                     width = 100) 

# Fit variogram model
resid_vmf_N = automap::autofitVariogram(formula = mlr_resi~1, 
                                         input_data = sp_df_N, 
                                         model = c("Mat", "Exp"),
                                         kappa = c(seq(0.1, 0.4, 0.1)),
                                         fix.values = c(0, NA, NA),
                                         verbose = TRUE)

resid_vmf_W = automap::autofitVariogram(formula = mlr_resi~1, 
                                        input_data = sp_df_W, 
                                        model = c("Mat", "Exp"),
                                        kappa = c(seq(0.1, 0.4, 0.1)),
                                        fix.values = c(0, NA, NA),
                                        verbose = TRUE)

# Get points to plot the variogram model 
vm_points_N = gstat::variogramLine(resid_vmf_N["var_model"]$var_model, maxdist = 50000)
vm_points_W = gstat::variogramLine(resid_vmf_W["var_model"]$var_model, maxdist = 50000)

# Plots
par(mar = c(4.0,4,0.5,0.5)) # bottom, left, top, right margins

plot(emp_svario_resi_N$dist, emp_svario_resi_N$gamma, xlab = "", 
     ylab = "Semivarianz", xlim = c(1200,49000), ylim = c(0, 3500))
lines(x = vm_points_N$dist,
      y = vm_points_N$gamma,
      col = ("#0000FF"),
      lty = 1)
legend(x = "bottomright" , legend = c("a)"), bty = "n")

plot(emp_svario_resi_W$dist, emp_svario_resi_W$gamma, xlab = "Entfernung [m]", 
     ylab = "Semivarianz", xlim = c(1200,49000), ylim = c(0, 300))
lines(x = vm_points_W$dist,
      y = vm_points_W$gamma,
      col = ("#0000FF"),
      lty = 1)
legend(x = "bottomright" , legend = c("b)"), bty = "n")
##
## End (EmSv of the residuals of a multiple linear regression model)
####
################################################################################
## End (investigation of the spatial autocorrelatio)
################################################################################