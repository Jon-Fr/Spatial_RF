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

# Load data
data_set = "NuM_L"
load("Data/NuM_L.rda")
d = NuM_L

# Get information about the prediction distance 
pd_df = info_d_NuM_L$predDist_df
mean_pd = mean(pd_df$lyr.1)
med_pd = median(pd_df$lyr.1)

# Set buffer 
buffer = 0

# Set tolerance (all = partition_loo with buffer)
tolerance = "all"

# Set number of permutations 
n_perm = 10

# Set formula D1, D2, D3 are dummy variables
fo = as.formula(bcNitrate ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                       nfk + humus + cAckerland + log10_gwn + agrum_log10_restime + 
                       agrum_log10_gwn + agrum_log10_geschw + Ackerland + 
                       lbm_class_Gruenland + lbm_class_Unbewachsen + 
                       lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + X + Y + 
                       tc45 + tc315 + D1 + D2 + D3 +
                       aea20_2 + aea20_8 + aea20_12)

# Since the eigenvectors are calculated in the model function or in the 
# prediction function (for newdata), but the permutation of the values for 
# calculating the VI takes place outside of these functions, dummy 
# variables are required to estimate the VI for the eigenvectors.
d$D1 = 1:nrow(d)
d$D2 = 1:nrow(d)
d$D3 = 1:nrow(d)

d$checkD = 1:nrow(d)

# Calculate importance for these variables
imp_vars_RF_MEv = all.vars(fo)[-1]

# Set partition function and sample arguments 
if (tolerance == "all"){
  partition_fun = partition_loo
  smp_args = list(buffer = buffer)
} else{
  partition_fun = partition_tt_dist
  smp_args = list(buffer = buffer, tolerance = tolerance)
}

# Set first part of the model formula 
fo_firstPart = "bcNitrate ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                  nfk + humus + cAckerland + log10_gwn + agrum_log10_restime + 
                  agrum_log10_gwn + agrum_log10_geschw + Ackerland + 
                  lbm_class_Gruenland + lbm_class_Unbewachsen + 
                  lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + X + Y + 
                  tc45 +  tc315 + aea20_2 + aea20_8 + aea20_12 +"
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

####
## Cross validation
## 

# Create model function 
RF_MEv_fun = function(formula, data, fo_fp){
  # Create matrix of spatial point coordinates 
  coord_m = cbind(data$X, data$Y)
  # Calculate Moran eigenvectors and eigenvalues (MEvEv)
  MEvEv = spmoran::meigen(coords = coord_m, threshold = 0)
  # Store eigenvectors in a df and combine it with the data df
  Evec_df = as.data.frame(MEvEv$sf)
  c_data = cbind(data, Evec_df)
  # Create second part of the model formula and complete the formula
  fo_sp = paste(colnames(Evec_df), collapse="+")
  fo_c = as.formula(paste(fo_fp, fo_sp))
  # Create RF model
  RF_model = ranger::ranger(formula = fo_c, 
                            data = c_data,
                            oob.error = FALSE,
                            seed = 7)
  # Return the model, the MEvEv and the eigenvector df. 
  # The MEvEv are required to estimates the Moran eigenvectors at unobserved 
  # sites. The eigenvector df is required for the PVI assessment. 
  r_l = list("model" = RF_model, "MEvEv" = MEvEv, "Evec_df" = Evec_df)
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
  # Check if one of the dummy variables was permuted if it was permute the value
  # of the corresponding eigenvector
  return(sample(object$Evec_df$V1, 1))
  if (!identical(newdata$checkD, newdata$D1)){
    c_newdata$V1 = sample(object$Evec_df$V1, 1)
  }
  if (!identical(newdata$checkD, newdata$D2)){
    c_newdata$V2 = sample(object$Evec_df$V2, 1)
  }
  if (!identical(newdata$checkD, newdata$D3)){
    c_newdata$V3 = sample(object$Evec_df$V3, 1)
  }
  # RF prediction
  RF_prediction = predict(object = object$model,
                          data = c_newdata)
  return(RF_prediction$predictions)
}

# Start time measurement
start_time = Sys.time()
print(start_time)

# Perform the spatial cross-validation
sp_cv_RF_MEv = sperrorest::sperrorest(formula = fo, data = d, 
                                      coords = c("X","Y"), 
                                      model_fun = RF_MEv_fun,
                                      model_args = list(fo_fp = fo_firstPart),
                                      pred_fun = RF_MEv_pred_fun,
                                      smp_fun = partition_fun, 
                                      smp_args = smp_args,
                                      imp_permutations = n_perm,
                                      imp_variables = imp_vars_RF_MEv,
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
