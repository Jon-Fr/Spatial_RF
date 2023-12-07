################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sp")
p_load("sf")
p_load("gstat")       

# Fewer decimal places, apply penalty on exponential notation 
options("scipen"= 999, "digits"=4)

# Load data and formula 
load("Data/NuM_L.rda")
d = NuM_L
  
# Get information about the prediction distance 
pd_df = info_d_NuM_L$predDist_df
hist(pd_df$lyr.1)
third_quartile = quantile(x = pd_df$lyr.1, probs = c(0.75))
tq_pd = third_quartile
max_pd = max(pd_df$lyr.1)
mean_pd = mean(pd_df$lyr.1)
med_pd = median(pd_df$lyr.1)


# Create a spatial points df 
sp_df = sp::SpatialPointsDataFrame(d[,c("X","Y")], d)  
################################################################################
## End (preparation)
################################################################################


################################################################################
## Investigation of the spatial autocorrelation)
################################################################################

####
## Empirical semivariogram (EmSv) of the variable of interest
##
# Full distance (maximal prediction distance)
emp_svario_fd = gstat::variogram(bcNitrate~1, data=sp_df, cutoff = max_pd, 
                                 width = 1000) 
plot(emp_svario_fd$dist, emp_svario_fd$gamma)

# Short distance (median prediction distance)
emp_svario_sd = gstat::variogram(bcNitrate~1, data=sp_df, cutoff = med_pd, 
                                 width = 100) 
plot(emp_svario_sd$dist, emp_svario_sd$gamma)
##
## End (EmSv of the variable of interest)
####


####
## EmSv of the residuals of a multiple linear regression model
##

# Create MLR model
MLR_model = lm(fo_lm, data=d)
# Add residuals to sp_df
sp_df$mlr_resi = MLR_model$residuals

# Full distance
emp_svario_resi_fd = gstat::variogram(bcNitrate~1, data=sp_df, cutoff = max_pd, 
                                      width = 1000)  
plot(emp_svario_resi_fd$dist, emp_svario_resi_fd$gamma)

# Short distance (median predction distance)
emp_svario_resi_sd = gstat::variogram(bcNitrate~1, data=sp_df, cutoff = med_pd, 
                                      width = 100) 
plot(emp_svario_resi_sd$dist, emp_svario_resi_sd$gamma)
##
## End (EmSv of the residuals of a multiple linear regression model)
####
################################################################################
## End (investigation of the spatial autocorrelatio)
################################################################################