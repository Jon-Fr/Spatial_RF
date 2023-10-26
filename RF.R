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

p_load("ranger")

# Additional functions that are not included in packages
source("auxiliary_functions.R", encoding = "UTF-8")
source("spdiagnostics-functions.R", encoding = "UTF-8") # Brenning 2022

# Fewer decimal places
options(digits=4)

# Load data and formula (for now use a subset)
load("data_points_subset.rda")
d = subset_dp

# Simplified formula for testing
fo = as.formula(bcNitrate ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                  cAckerland + log10_gwn + agrum_log10_restime + Ackerland + 
                  lbm_class_Gruenland + lbm_class_Unbewachsen + 
                  lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung)
################################################################################
## End (preparation)
################################################################################


################################################################################
## Random Forest (RF) prediction 
################################################################################
################################################################################
## End (Random Forest (RF) prediction) 
################################################################################


################################################################################
## Random Forest (RF) spatial leave one out cross validation 
################################################################################
#####
## Get the mean and median prediction distance 
## (for now use the test area as prediction area)
##

m_m_pd = mean_med_predDist(path_predArea = "test_area.gpkg", dataPoints_df = d,
                           c_r_s = "EPSG:25832")
##
## End (get the mean and median prediction distance)
####


####
## Cross validation
## 

# Create model function 
RF_fun = function(formula, data){
  RF_model = ranger::ranger(formula = formula, 
                            data = data)
  return(RF_model)
}

# Create prediction function
RF_pred_fun = function(object, newdata){
  RF_prediction = predict(object = object,
                          data = newdata)
  return(RF_prediction$predictions)
}

# Perform the spatial cross-validation
# Future for parallelization
future::plan(future.callr::callr, workers = 10)
sp_cv_RF = sperrorest::sperrorest(formula = fo, data = d, coords = c("x","y"), 
                                  model_fun = RF_fun, 
                                  pred_fun = RF_pred_fun,
                                  smp_fun = partition_loo, 
                                  smp_args = list(buffer = m_m_pd$med_predDist))

# Get test RMSE
test_RMSE = sp_cv_RF$error_rep$test_rmse
test_RMSE
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

test2 = foreach (i = iter(test, by="row"), .combine=c, 
                 .packages = c("sperrorest", "ranger")) %dopar%{
  sp_cv_RF = sperrorest::sperrorest(formula = fo, data = d, coords = c("x","y"), 
                                    model_fun = RF_fun, 
                                    pred_fun = RF_pred_fun,
                                    smp_fun = partition_loo,
                                    smp_args = list(buffer = i),
                                    mode_rep = "loop", 
                                    mode_fold = "loop")
  
  test_RMSE = sp_cv_RF$error_rep$test_rmse
  }

plot(test2~test[ ,1])

# Stop cluster
stopCluster(cluster)

# End time measurement
end_time = Sys.time()
print("bygone time")
print(end_time - start_time)
##
## End (explore the relationship between buffer distance and RMSE)
####
################################################################################
## End (test area)
################################################################################
