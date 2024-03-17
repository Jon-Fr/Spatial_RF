################################################################################
## Preparation 
################################################################################
# Load necessary packages
library("pacman")
p_load("sp")
p_load("sf")
p_load("gstat")  
p_load("automap")
p_load("dplyr")

# Fewer decimal places, apply penalty on exponential notation 
options("scipen"= 999, "digits"=4)

# Load data sets and formulas 
load("Data/WuS_SuB.rda")
load("Data/NuM_L.rda")

#N = NuM_L
#W = WuS_SuB

## Get information about the prediction distance and plot histograms
pd_df_N = info_d_NuM_L$predDist_df
# Part of the Area below range value of variogram model
br_vec_N = pd_df_N[pd_df_N$lyr.1 <3928, ]
pbr_N = length(br_vec_N)/nrow(pd_df_N)

first_quartile_N = quantile(x = pd_df_N$lyr.1, probs = c(0.25))
third_quartile_N = quantile(x = pd_df_N$lyr.1, probs = c(0.75))
tq_pd_N = third_quartile_N
max_pd_N = max(pd_df_N$lyr.1)
mean_pd_N = mean(pd_df_N$lyr.1)
med_pd_N = median(pd_df_N$lyr.1)
hist_N = hist(pd_df_N$lyr.1, breaks = (135))
color_N = ifelse(hist_N$breaks <=first_quartile_N, "white", ifelse(hist_N$breaks <=med_pd_N & hist_N$breaks > first_quartile_N, "snow3", ifelse(hist_N$breaks <=third_quartile_N & hist_N$breaks > med_pd_N, "gray57", "#333333")))

pd_df_W = info_d_WuS_SuB$predDist_df
# Part of the Area below range value of variogram model
br_vec_W = pd_df_W[pd_df_W$lyr.1 <26976, ]
pbr_W = length(br_vec_W)/nrow(pd_df_W)

first_quartile_W = quantile(x = pd_df_W$lyr.1, probs = c(0.25))
third_quartile_W = quantile(x = pd_df_W$lyr.1, probs = c(0.75))
tq_pd_W = third_quartile_W
max_pd_W = max(pd_df_W$lyr.1)
med_pd_W = mean(pd_df_W$lyr.1)
med_pd_W = median(pd_df_W$lyr.1)
hist_W = hist(pd_df_W$lyr.1, breaks = (173))
color_W = ifelse(hist_W$breaks <=first_quartile_W, "white", ifelse(hist_W$breaks <=med_pd_W & hist_W$breaks > first_quartile_W, "snow3", ifelse(hist_W$breaks <=third_quartile_W & hist_W$breaks > med_pd_W, "gray57", "#333333")))

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
mtext(expression("Anzahl an 100 m"^2 *" Rasterzellen in tausend"),
      side = 4, line = -4, col = 1, cex = 1.1)

# Second plot
par(mar = c(2, 0, 2, 0)) # bottom, left, top, right margins
plot(hist_N, xlab = "", ylab = "", 
     main = "", col = color_N, xaxt = "n", yaxt = "n", xlim = c(0, 175000))
axis(1, at = c(0, 25000, 50000, 75000, 100000, 125000, 150000, 175000),
     labels = c("", "", "", "", "", "", "", ""), cex.axis = 1.2)#, lwd.ticks = 0)
axis(2, at = c(0, 500000, 1000000, 1500000, 2000000, 2500000), 
     labels = c(0, 500, 100, 150, 200, 250), cex.axis = 1.2)
legend(x = "topright" , legend = c("a)"), bty = "n", cex = 1.25)
legend(x = "top", cex = 1.25,
       legend = c("1. Quartil", "2. Quartil", "3. Quartil", "4. Quartil"),
       fill = c("white", "snow3", "gray57", "#333333"), bty = "n")

# Third plot
par(mar = c(4, 0, 0, 0)) # bottom, left, top, right margins
plot(hist_W, xlab = "", ylab = "", 
     main = "", col = color_W, xaxt = "n", yaxt = "n",  xlim = c(0, 175000), ylim = c(0, 1250000))
axis(1, at = c(0, 25000, 50000, 75000, 100000, 125000, 150000, 175000),
     labels = c(0, 25, 50, 75, 100, 125, 150, 175), cex.axis = 1.2)
axis(2, at = c(0, 250000, 500000, 750000, 1000000, 1250000), 
     labels = c(0, 25, 50, 75, 100, 125), cex.axis = 1.2)
title(xlab = "Vorhersagedistanz [km]", mgp = c(2.5, 2.5, 2.5), cex.lab = 1.25) 
legend(x = "topright" , legend = c("b)"), bty = "n", cex = 1.25)

# Create spatial points dfs 
sp_df_W = sp::SpatialPointsDataFrame(WuS_SuB[,c("X","Y")], WuS_SuB) 
sp_df_N = sp::SpatialPointsDataFrame(NuM_L[,c("X","Y")], NuM_L)  
################################################################################
## End (preparation)
################################################################################


################################################################################
## Investigation of the spatial autocorrelation
################################################################################
par(mfrow = c(1, 1))
####
## Empirical semivariogram (EmSv) of the variable of interest
##
emp_svario_N = gstat::variogram(subMittelwert ~1, data=sp_df_N, cutoff = 10000, 
                              width = 100) 
plot(emp_svario_N$dist, emp_svario_N$gamma, ylim = c(0,5000))

emp_svario_N = gstat::variogram(bcNitrate ~1, data=sp_df_N, cutoff = 10000, 
                                width = 10) 
plot(emp_svario_N$dist, emp_svario_N$gamma)

emp_svario_W = gstat::variogram(subMittelwert ~1, data=sp_df_W, cutoff = 10000, 
                                width = 100) 
plot(emp_svario_W$dist, emp_svario_W$gamma, ylim = c(0,500))

emp_svario_W = gstat::variogram(bcNitrate ~1, data=sp_df_W, cutoff = 10000, 
                                width = 10) 
plot(emp_svario_W$dist, emp_svario_W$gamma, ylim = c(0,20))

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
MLR_model_N_bc = lm(fo_lm_NuM_L_bc, data=NuM_L)
MLR_model_W_bc = lm(fo_lm_WuS_SuB_bc, data=WuS_SuB)
# Add residuals to sp_df
sp_df_N$mlr_resi = MLR_model_N$residuals
sp_df_W$mlr_resi = MLR_model_W$residuals
sp_df_N$mlr_resi_bc = MLR_model_N_bc$residuals
sp_df_W$mlr_resi_bc = MLR_model_W_bc$residuals

emp_svario_resi_N = gstat::variogram(mlr_resi~1, data=sp_df_N, cutoff = 50000, 
                                   width = 100) 
plot(emp_svario_resi_N$dist, emp_svario_resi_N$gamma, ylim = c(0, 5000))

emp_svario_resi_N = gstat::variogram(mlr_resi_bc~1, data=sp_df_N, cutoff = 50000, 
                                     width = 100)
plot(emp_svario_resi_N$dist, emp_svario_resi_N$gamma)

emp_svario_resi_W = gstat::variogram(mlr_resi~1, data=sp_df_W, cutoff = 50000, 
                                     width = 100) 
plot(emp_svario_resi_W$dist, emp_svario_resi_W$gamma, ylim = c(0, 400))

emp_svario_resi_W = gstat::variogram(mlr_resi_bc~1, data=sp_df_W, cutoff = 50000, 
                                     width = 10)
plot(emp_svario_resi_W$dist, emp_svario_resi_W$gamma, ylim = c(0, 10))


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
vm_points_N = gstat::variogramLine(resid_vmf_N["var_model"]$var_model, maxdist = 51000)
vm_points_W = gstat::variogramLine(resid_vmf_W["var_model"]$var_model, maxdist = 51000)

vm_points_N = dplyr::add_row( vm_points_N, dist = 0, gamma = 0, .before = 1)

vm_points_W = dplyr::add_row(vm_points_W, dist = 0, gamma = 0, .before = 1)

## Set plot layout
layout_mat <- matrix(c(1,2,4,1,3,5), nrow = 2, ncol = 3,
                     byrow = TRUE)
layout_mat

my_lay = layout(mat = layout_mat, 
                heights = c(2.5, 2.5),
                widths = c(0.5, 4.5, 4.5), respect =FALSE)
layout.show(my_lay)

# First plot
par(mar = c(2, 0, 0, 0)) # bottom, left, top, right margins
plot(NULL, ylab = "", bty = "n", 
     xlim = c(0, 0.1), ylim = c(0, 0.1), xaxt = "n", yaxt = "n")
mtext("Semivarianz", side = 4, line = -3, col = 1, cex = 1.1)

# Second plot
par(mar = c(2, 1, 2, 0.2)) # bottom, left, top, right margins
plot(emp_svario_resi_N$dist, emp_svario_resi_N$gamma, xlab = "", 
     ylab = "", xlim = c(1500,48500), ylim = c(0, 3500), xaxt = "n",
     yaxt = "n")
axis(1, at = c(0, 10000, 20000, 30000, 40000, 50000),
     labels = c("", "", "", "", "", ""), cex.axis = 1.2)
axis(2, at = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500), 
     labels = c(0, "", 1000, "", 2000, "", 3000, ""), cex.axis = 1.2)
lines(x = vm_points_N$dist,
      y = vm_points_N$gamma,
      col = ("#0000FF"),
      lty = 1)
legend(x = "bottomright" , legend = c("a)"), bty = "n", cex = 1.25)

# Third Plot
par(mar = c(4, 1, 0, 0.2)) # bottom, left, top, right margins
plot(emp_svario_resi_W$dist, emp_svario_resi_W$gamma, xlab = "",
     ylab = "", xlim = c(1500,48500), ylim = c(0, 300), xaxt = "n", yaxt = "n")
axis(1, at = c(0, 10000, 20000, 30000, 40000, 50000), 
     labels = c(0, 10, 20, 30, 40, 50), cex.axis = 1.2)
axis(2, at = c(0, 50, 100, 150, 200, 250, 300, 350), 
     labels = c(0, "", 100, "", 200, "", 300, ""), cex.axis = 1.2)
lines(x = vm_points_W$dist,
      y = vm_points_W$gamma,
      col = ("#0000FF"),
      lty = 1)
legend(x = "bottomright" , legend = c("b)"), bty = "n", cex = 1.25)
title(xlab = "Entfernung [km]", mgp = c(2.5, 2.5, 2.5), cex.lab = 1.25) 

# Get points to plot 

emp_svario_resi_N_n = gstat::variogram(mlr_resi~1, data=sp_df_N, cutoff = 50000, 
                                     width = 100) 

emp_svario_resi_W_n = gstat::variogram(mlr_resi~1, data=sp_df_W, cutoff = 50000, 
                                     width = 100) 

vm_points_N = gstat::variogramLine(resid_vmf_N["var_model"]$var_model, maxdist = 11000)
vm_points_W = gstat::variogramLine(resid_vmf_W["var_model"]$var_model, maxdist = 11000)
vm_points_N = dplyr::add_row( vm_points_N, dist = 0, gamma = 0, .before = 1)
vm_points_W = dplyr::add_row(vm_points_W, dist = 0, gamma = 0, .before = 1)


# Fourth plot
par(mar = c(2, 1, 2, 0.2)) # bottom, left, top, right margins
plot(emp_svario_resi_N_n$dist, emp_svario_resi_N_n$gamma, xlab = "", 
     ylab = "", xlim = c(300,9750), ylim = c(0, 3500), xaxt = "n",
     yaxt = "n")
axis(1, at = c(0, 2000, 4000, 6000, 8000, 10000),
     labels = c("", "", "", "", "", ""), cex.axis = 1.2)
axis(2, at = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500), 
     labels = c("", "", "", "", "", "", "", ""), cex.axis = 1.2)
lines(x = vm_points_N$dist,
      y = vm_points_N$gamma,
      col = ("#0000FF"),
      lty = 1)
legend(x = "bottomright" , legend = c("b)"), bty = "n", cex = 1.25)

# Fifth Plot
par(mar = c(4, 1, 0, 0.2)) # bottom, left, top, right margins
plot(emp_svario_resi_W_n$dist, emp_svario_resi_W_n$gamma, xlab = "",
     ylab = "", xlim = c(300,9750), ylim = c(0, 300), xaxt = "n", yaxt = "n")
axis(1, at = c(0, 2000, 4000, 6000, 8000, 10000), 
     labels = c(0, 2, 4, 6, 8, 10), cex.axis = 1.2)
axis(2, at = c(0, 50, 100, 150, 200, 250, 300, 350), 
     labels = c("", "", "", "", "", "", "", ""), cex.axis = 1.2)
lines(x = vm_points_W$dist,
      y = vm_points_W$gamma,
      col = ("#0000FF"),
      lty = 1)
legend(x = "bottomright" , legend = c("d)"), bty = "n", cex = 1.25)
title(xlab = "Entfernung [km]", mgp = c(2.5, 2.5, 2.5), cex.lab = 1.25) 
##
## End (EmSv of the residuals of a multiple linear regression model)
####
################################################################################
## End (investigation of the spatial autocorrelatio)
################################################################################