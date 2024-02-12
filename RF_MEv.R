################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sperrorest")
p_load("ranger")
p_load("spmoran")

# Additional functions that are not included in packages
source("auxiliary_functions.R", encoding = "UTF-8")
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
buffer = 0

# Set tolerance (all = partition_loo without buffer)
tolerance = "all"

# Set number of permutations 
n_perm = 10

# Calculate importance for these variables
imp_vars_RF = all.vars(fo_RF)[-1]

# Set partition function and sample arguments 
if (tolerance == "all"){
  partition_fun = partition_loo
  smp_args = list(buffer = buffer)
} else{
  partition_fun = partition_tt_dist
  smp_args = list(buffer = buffer, tolerance = tolerance)
}
################################################################################
## End (preparation)
################################################################################


################################################################################
## RF-MEv spatial leave one out cross validation 
################################################################################

####
## Cross validation
## 

# Create model function 
RF_MEv_fun = function(formula, data){
  # Create matrix of spatial point coordinates 
  coord_m = cbind(data$X, data$Y)
  # Calculate Moran eigenvectors and eigenvalues (MEvEv)
  MEvEv = spmoran::meigen(coords = coord_m, threshold = 0)
  # Store eigenvectors in a df and combine it with the data df
  Evec_df = as.data.frame(MEvEv$sf)
  c_data = cbind(data, Evec_df)
  # Get first part of the formula 
  voi = all.vars(fo_RF)[1]
  covair = all.vars(fo_RF)[-1]
  covair_part = paste(covair, collapse = "+")
  fo_fp = paste(voi, covair_part, sep = "~")
  # Create second part of the model formula and complete the formula
  fo_sp = paste(colnames(Evec_df), collapse="+")
  fo_c = as.formula(paste(fo_fp, fo_sp, sep="+"))
  # Create RF model
  RF_model = ranger::ranger(formula = fo_c, 
                            data = c_data,
                            oob.error = FALSE,
                            seed = 7)
  # Return the model, the MEvEv and the eigenvector df. 
  # The MEvEv are required to estimates the Moran eigenvectors at unobserved 
  # sites.  
  r_l = list("model" = RF_model, "MEvEv" = MEvEv)
  return(r_l)
}

# Create prediction function
RF_MEv_pred_fun = function(object, newdata){
  # Create matrix of spatial point coordinates 
  coord_m = cbind(newdata$X, newdata$Y)
  # Calculate Moran eigenvectors and eigenvalues
  MEvEv_n = spmoran::meigen0(meig = object$MEvEv, coords0 = coord_m)
  # Store eigenvectors in a df and combine it with the newdata df
  Evec_ndf = as.data.frame(MEvEv_n$sf)
  c_newdata = cbind(newdata, Evec_ndf)
  # RF prediction
  RF_prediction = predict(object = object$model,
                          data = c_newdata)
  return(RF_prediction$predictions)
}

# Start time measurement
start_time = Sys.time()
print(start_time)

# Perform the spatial cross-validation
sp_cv_RF_MEv = sperrorest::sperrorest(formula = fo_RF, data = d, 
                                      coords = c("X","Y"), 
                                      model_fun = RF_MEv_fun,
                                      pred_fun = RF_MEv_pred_fun,
                                      smp_fun = partition_fun, 
                                      smp_args = smp_args,
                                      imp_permutations = n_perm,
                                      imp_variables = imp_vars_RF,
                                      imp_sample_from = "all",
                                      distance = TRUE)

# Get test RMSE
test_RMSE = sp_cv_RF_MEv$error_rep$test_rmse
test_RMSE

# End time measurement
end_time = Sys.time()
bygone_time = end_time - start_time
print(bygone_time)

# Set file name 
file_name = paste("Results/", data_set,"_sp_cv_RF_MEv_", as.character(round(buffer)),
                  "_+", as.character(tolerance), "_", as.character(n_perm),
                  ".rda", sep = "")
# Save result 
save(sp_cv_RF_MEv, bygone_time, file = file_name)
##
## End (cross validation)
#### 
################################################################################
## End (RF-MEv spatial leave one out cross validation) 
################################################################################
