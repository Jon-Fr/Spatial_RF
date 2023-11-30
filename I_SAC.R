################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sp")
p_load("sf")
p_load("terra")
p_load("sperrorest")
p_load("purrr")
p_load("parallel")
p_load("doParallel")
p_load("foreach")
p_load("future")

p_load("gstat")       

# Additional functions that are not included in packages
source("auxiliary_functions.R", encoding = "UTF-8")
source("spdiagnostics-functions.R", encoding = "UTF-8") # Brenning 2022

# Fewer decimal places, apply penalty on exponential notation 
options("scipen"= 999, "digits"=4)

# Load data and formula (for now use a subset)
load("Data/.rda")
d = 
  
# Get information about the prediction distance 
info_pd = info_predDist(path_predArea = "Data/.gpkg", 
                          dataPoints_df = d,
                          c_r_s = "EPSG:25832",
                          resolution = 100,
                          xy = c("X", "Y"))

pd_df = info_pd$predDist_df
hist(pd_df$lyr.1)
third_quartile = quantile(x = pd_df$lyr.1, probs = c(0.75))
tq_pd = third_quartile
max_pd = info_pd$max_predDist
mean_pd = info_pd$mean_predDist
sd_pd = info_pd$sd_predDist
med_pd = info_pd$med_predDist
mad_pd = info_pd$mad_predDist

# Create a spatial points df 
sp_df = sp::SpatialPointsDataFrame(d[,c("X","Y")], d)  

# Adjusted formula
fo = as.formula(bcNitrate ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                  cAckerland + log10_gwn + agrum_log10_restime + Ackerland + 
                  lbm_class_Gruenland + lbm_class_Unbewachsen + 
                  lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + 
                  aea20_13 + aea20_6 + X + Y)
################################################################################
## End (preparation)
################################################################################


################################################################################
## Investigation of the spatial autocorrelation
################################################################################

####
## Empirical semivariogram (EmSv) of the variable of interest
##
# Full distance (maximal prediction distance)
emp_svario_fd = gstat::variogram(bcNitrate~1, data=sp_df, cressie = TRUE, 
                                 cutoff = max_pd, width = 1000) 
plot(emp_svario_fd$dist, emp_svario_fd$gamma)

# Short distance (mean prediction distance)
emp_svario_sd = gstat::variogram(bcNitrate~1, data=sp_df, cutoff = mean_pd, 
                                 cressie = TRUE, width = 300) 
plot(emp_svario_sd$dist, emp_svario_sd$gamma)
##
## End (EmSv of the variable of interest)
####


####
## EmSv of the residuals of a multiple linear regression model
##

# Create MLR model
MLR_model = lm(fo, data=d)
# Add residuals to sp_df
sp_df$mlr_resi = MLR_model$residuals

# Full distance
emp_svario_resi_fd = gstat::variogram(bcNitrate~1, data=sp_df, cressie = TRUE, 
                                      cutoff = max_pd, width = 1000)  
plot(emp_svario_resi_fd$dist, emp_svario_resi_fd$gamma)

# Short distance (mean predction distance)
emp_svario_resi_sd = gstat::variogram(bcNitrate~1, data=sp_df, cutoff = mean_pd, 
                                      cressie = TRUE, width = 300) 
plot(emp_svario_resi_sd$dist, emp_svario_resi_sd$gamma)
##
## End (EmSv of the residuals of a multiple linear regression model)
####
################################################################################
## Investigation of the spatial autocorrelation
################################################################################