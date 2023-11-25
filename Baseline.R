################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sperrorest")

# Fewer decimal places, apply penalty on exponential notation 
options("scipen"= 999, "digits"=4)

# Load data (for now use a subset)
# Load data (for now use a subset)
load("Data/")
d = 

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
                                  smp_args = list(buffer = 0))

# Get test RMSE
test_RMSE = sp_cv_UK$error_rep$test_rmse
test_RMSE
##
## End (RMSE of linear model with only the intercept)
####

################################################################################
## (End Get baseline values)
################################################################################