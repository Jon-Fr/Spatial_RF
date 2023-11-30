################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sperrorest")

# Additional functions that are not included in packages
source("auxiliary_functions.R", encoding = "UTF-8")

# Fewer decimal places, apply penalty on exponential notation 
options("scipen"= 999, "digits"=4)

# Load data (for now use a subset)
load("Data/NuM_L_sub_4.rda")
d = sub_subset

# Get information about the prediction distance 
info_pd = info_predDist(path_predArea = "Data/NuM_L_sub_4_prediction_area.gpkg", 
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

# Set formula 
fo = as.formula(bcNitrate ~ 1)
################################################################################
## End (preparation)
################################################################################


################################################################################
## Get baseline values  
################################################################################

####
## Standard deviation of the variable of interest
##
s_dev = sd(d$bcNitrate)
##
## 
#### End (standard deviation of the variable of interest)


####
## RMSE of linear model with only the intercept
## (in this case the intercept has the same value as the mean so it would also
## be possible to use the mean instead of an lm)

# Create model function 
lm_inter_fun = function(formula, data){
  lm_m = lm(formula, data)
  return(lm_m)
}

# Create prediction function
lm_inter_pred_fun = function(object, newdata){
  predi = predict(object = object, newdata = newdata)
  return(predi)
}

# Perform the spatial cross-validation
# Future for parallelization
#future::plan(future.callr::callr, workers = 10)
sp_cv_UK = sperrorest::sperrorest(formula = fo, data = d, coords = c("X","Y"), 
                                  model_fun = lm_inter_fun, 
                                  pred_fun = lm_inter_pred_fun,
                                  smp_fun = partition_loo, 
                                  smp_args = list(buffer = med_pd))

# Get test RMSE
test_RMSE = sp_cv_UK$error_rep$test_rmse
test_RMSE
##
## End (RMSE of linear model with only the intercept)
####

################################################################################
## (End Get baseline values)
################################################################################