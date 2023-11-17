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
p_load("spmoran")

# Additional functions that are not included in packages
source("auxiliary_functions.R", encoding = "UTF-8")
source("spdiagnostics-functions.R", encoding = "UTF-8") # Brenning 2022

# Fewer decimal places
options(digits=4)

# Load data and formula (for now use a subset)
load("data_points_subset.rda")
d = subset_dp

# Dummy formula 
fo = as.formula(x ~ y)

# Set first part of the model formula (simplified for testing)
fo_firstPart = "bcNitrate ~ crestime + cgwn + cgeschw + log10carea + 
                  elevation + cAckerland + log10_gwn + agrum_log10_restime + 
                  Ackerland + lbm_class_Gruenland + lbm_class_Unbewachsen + 
                  lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + x + y +"
################################################################################
## End (preparation)
################################################################################


################################################################################
## Random Forest with  Moran eigenvectors explanatory variables (RF-MEv) 
## prediction 
################################################################################
################################################################################
## End (RF-MEv prediction) 
################################################################################


################################################################################
## RF-MEv spatial leave one out cross validation 
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
RF_MEv_fun = function(formula, data, fo_fp){
  # Create matrix of spatial point coordinates 
  coord_m = cbind(data$x, data$y)
  # Calculate Moran eigenvectors and eigenvalues (MEvEv)
  MEvEv = spmoran::meigen(coords = coord_m, threshold = 0.25)
  # Store eigenvectors in a df and combine it with the data df
  Evec_df = as.data.frame(MEvEv$sf)
  c_data = cbind(data, Evec_df)
  # Create second part of the model formula and complete the formula
  fo_sp = paste(colnames(Evec_df), collapse="+")
  fo_c = as.formula(paste(fo_fp, fo_sp))
  # Create RF model
  RF_model = ranger::ranger(formula = fo_c, 
                            data = c_data)
  # Return the model as well as the MEvEv. The MEvEv are required to estimates 
  # the Moran eigenvectors at unobserved sitess
  r_l = list("model" = RF_model, "MEvEv" = MEvEv)
  return(r_l)
}

# Create prediction function
RF_MEv_pred_fun = function(object, newdata){
  # Create matrix of spatial point coordinates 
  coord_m = cbind(newdata$x, newdata$y)
  # Calculate Moran eigenvectors and eigenvalues
  MEvEv = spmoran::meigen0(meig = object$MEvEv, coords0 = coord_m)
  # Store eigenvectors in a df and combine it with the newdata df
  Evec_df = as.data.frame(MEvEv$sf)
  c_newdata = cbind(newdata, Evec_df)
  # RF prediction
  RF_prediction = predict(object = object$model,
                          data = c_newdata)
  return(RF_prediction$predictions)
}

# Perform the spatial cross-validation
# Future for parallelization
#future::plan(future.callr::callr, workers = 10)
sp_cv_RF_MEv = sperrorest::sperrorest(formula = fo, data = d[1:300,], 
                                      coords = c("x","y"), 
                                      model_fun = RF_MEv_fun,
                                      model_args = list(fo_fp = fo_firstPart),
                                      pred_fun = RF_MEv_pred_fun,
                                      smp_fun = partition_loo, 
                                      smp_args = list(buffer=m_m_pd$med_predDist))

# Get test RMSE
test_RMSE = sp_cv_RF_MEv$error_rep$test_rmse
test_RMSE

##
## End (cross validation)
#### 
################################################################################
## End (RF-MEv spatial leave one out cross validation) 
################################################################################

# Start time measurement
start_time = Sys.time()
print(start_time)

test = RF_MEv_fun(formula = fo, data = d[3:758, ], fo_fp = fo_firstPart)
test1 = RF_MEv_pred_fun(object = test, newdata = d[1,])

d[1, "bcNitrate"]

# End time measurement
end_time = Sys.time()
print("bygone time")
print(end_time - start_time)