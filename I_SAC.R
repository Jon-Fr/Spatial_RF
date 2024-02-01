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

# Load data and formula 
load("Data/WuS_SuB.rda")
d = WuS_SuB
fo_lm = fo_lm_WuS_SuB

load("Data/NuM_L.rda")
d = NuM_L
fo_lm = fo_lm_NuM_L

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
emp_svario = gstat::variogram(bcNitrate~1, data=sp_df, cutoff = 150000, 
                                 width = 100) 
plot(emp_svario$dist, emp_svario$gamma)

# Fit variogram model
resid_vmf_au = automap::autofitVariogram(formula = bcNitrate ~ 1, 
                                         input_data = sp_df, 
                                         model = c("Mat", "Exp"),
                                         kappa = c(0.1,0.2,0.3),
                                         fix.values = c(0, NA, NA),
                                         verbose = TRUE)

print(plot(emp_svario, pl = FALSE, 
           model = resid_vmf_au["var_model"]$var_model))
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

emp_svario_resi = gstat::variogram(mlr_resi~1, data=sp_df, cutoff = 40000, 
                                      width = 100) 
plot(emp_svario_resi$dist, emp_svario_resi$gamma)

# Fit variogram model
resid_vmf_au = automap::autofitVariogram(formula = mlr_resi~1, 
                                         input_data = sp_df, 
                                         model = c("Mat", "Exp"),
                                         kappa = c(0.1,0.2,0.3),
                                         fix.values = c(0, NA, NA),
                                         verbose = TRUE)

print(plot(emp_svario_resi, pl = FALSE, 
           model = resid_vmf_au["var_model"]$var_model))
##
## End (EmSv of the residuals of a multiple linear regression model)
####
################################################################################
## End (investigation of the spatial autocorrelatio)
################################################################################