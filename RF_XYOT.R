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

p_load("ranger")
p_load("dplyr")

# Additional functions that are not included in packages
source("spdiagnostics-functions.R", encoding = "UTF-8") # Brenning 2022

# Fewer decimal places, apply penalty on exponential notation 
options("scipen"= 999, "digits"=4)

# Load data and formula
data_set = "WuS_SuB"
load("Data/WuS_SuB.rda")
d = WuS_SuB
fo_RF = fo_RF_WuS_SuB

# Get information about the prediction distance 
pd_df = info_d_WuS_SuB$predDist_df
mean_pd = mean(pd_df$lyr.1)
med_pd = median(pd_df$lyr.1)

# Set buffer 
buffer = med_pd
################################################################################
## End (preparation)
################################################################################


################################################################################
## Random Forest (RF) prediction 
################################################################################
################################################################################
## End (RF prediction) 
################################################################################


################################################################################
## RF spatial leave one out cross validation 
################################################################################

####
## Cross validation
## 

# Create model function 
RF_fun = function(formula, data){
  RF_model = ranger::ranger(formula = formula, 
                            data = data,
                            oob.error = FALSE,
                            seed = 7)
  return(RF_model)
}

# Create prediction function
RF_pred_fun = function(object, newdata){
  RF_prediction = predict(object = object,
                          data = newdata)
  return(RF_prediction$predictions)
}

# Start time measurement
start_time = Sys.time()
print(start_time)

# Perform the spatial cross-validation
# Future for parallelization
#future::plan(future.callr::callr, workers = 10)
sp_cv_RF = sperrorest::sperrorest(formula = fo_RF, data = d, 
                                  coords = c("X","Y"), 
                                  model_fun = RF_fun, 
                                  pred_fun = RF_pred_fun,
                                  smp_fun = partition_loo, 
                                  smp_args = list(buffer = buffer))

# Get test RMSE
test_RMSE = sp_cv_RF$error_rep$test_rmse
test_RMSE

# End time measurement
end_time = Sys.time()
bygone_time = end_time - start_time
print(bygone_time)

# Set file name 
file_name = paste("Results/",data_set,"_sp_cv_RF_",as.character(round(buffer)),
                  ".rda", sep = "")
# Save result 
save(sp_cv_RF, bygone_time, file = file_name)
##
## End (cross validation)
#### 
################################################################################
## End (Random Forest (RF) spatial leave one out cross validation) 
################################################################################


################################################################################
## Test area
################################################################################

####
## Explore the relationship between buffer distance and RMSE
##

# Start time measurement
start_time = Sys.time()

# Setup backend to use many processors
totalCores = parallel::detectCores()

# Leave two cores to reduce computer load
cluster = parallel::makeCluster(totalCores[1]-2) 
doParallel::registerDoParallel(cluster)

# explore
test = data.frame(seq(0, 20000, 1000))

test2 = foreach::foreach(i = iter(test, by="row"), .combine=c, 
                 .packages = c("sperrorest", "ranger")) %dopar%{
  sp_cv_RF = sperrorest::sperrorest(formula = fo, data = d, coords = c("X","Y"), 
                                    model_fun = RF_fun, 
                                    pred_fun = RF_pred_fun,
                                    smp_fun = partition_loo,
                                    smp_args = list(buffer = i),
                                    mode_rep = "sequential", 
                                    mode_fold = "sequential")
  
  test_RMSE = sp_cv_RF$error_rep$test_rmse
  }

plot(test2~test[ ,1])

# Stop cluster
parallel::stopCluster(cluster)

# End time measurement
end_time = Sys.time()
bygone_time = end_time - start_time
##
## End (explore the relationship between buffer distance and RMSE)
####
################################################################################
## End (test area)
################################################################################
