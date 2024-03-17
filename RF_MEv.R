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

# Fewer decimal places, apply penalty on exponential notation 
options("scipen"= 999, "digits"=4)

# Load data and formula
data_set = "WuS_SuB"
load("Data/WuS_SuB.rda")
d = WuS_SuB
fo_RF = fo_RF_WuS_SuB

# Set buffer 
buffer = 40000

# Set tolerance (all = partition_loo without buffer)
tolerance = 100

# Set number of permutations 
n_perm = 0

# Use another error fun (should only be true for the calculation of the 
# retransformation RMSE)
re_bc = TRUE
if (re_bc){
  error_fun = err_re_bc
} else{
  error_fun = err_default
}

# Calculate importance for these variables
imp_vars_RF = all.vars(fo_RF)[-1]
imp_vars_RF = NULL

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
  MEvEv = spmoran::meigen(coords = coord_m, threshold = 0.25)
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

#test = RF_MEv_fun(formula = fo_RF_WuS_SuB, data = d[c(1:2359),])
#test1 = partition_fun(data = d, coords = c("X","Y"), buffer = buffer, tolerance = tolerance)
#test2 = RF_MEv_fun(formula = fo_RF_NuM_L, data = d[test1[[1]]$"86"$train,])


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
                                      distance = TRUE,
                                      err_fun = error_fun)

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
