################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sp")
p_load("sf")
p_load("terra")
p_load("sperrorest")
p_load("ranger")
library(landmap)

# Additional functions that are not included in packages
source("auxiliary_functions.R", encoding = "UTF-8")

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
buffer = 1

# Set tolerance (all = partition_loo with buffer)
tolerance = 1

# Set number of permutations 
n_perm = 1

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

####
## Data and formula preparation
##

# Start time measurement
start_time_dmc = Sys.time()
print(start_time_dmc)

idCol = "ID_WuS_SuB"

# Create a spatial points df 
sp_df = sp::SpatialPointsDataFrame(d[,c("X","Y")], d)

# Load prediction area (for now use the test area as prediction area)
pred_area = terra::vect("Data/WuS_SuB_Gebiet.gpkg")

# Create raster with same extent as the prediction area
raster_pl = terra::rast(pred_area, resolution = 10000,
                        xmin = xmin(pred_area), xmax = xmax(pred_area), 
                        ymin = ymin(pred_area), ymax = ymax(pred_area))

# Set crs
terra::crs(raster_pl) = "EPSG:25832"

# Set NA to 1
raster_pl = terra::subst(raster_pl, NA, 1)

# Raster to df
pl_df = terra::as.data.frame(raster_pl, xy = TRUE)

# Df to spatial pixels df
spx_df = SpatialPixelsDataFrame(points=pl_df[c("x","y")],data=pl_df)

# Remove the raster and the df
remove(raster_pl, pl_df)

# Derive buffer distance layers
dist_layers = landmap::buffer.dist(sp_df["bcNitrate"], spx_df[1], 
                                   classes=as.factor(1:nrow(sp_df)))

# Get the layer names for the formula
layer_names = paste(names(dist_layers), collapse="+")

# Set first part of the model formula 
fo_firstPart = "bcNitrate ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                  nfk + humus + cAckerland + log10_gwn + agrum_log10_restime + 
                  agrum_log10_gwn + agrum_log10_geschw + Ackerland + 
                  lbm_class_Gruenland + lbm_class_Unbewachsen + 
                  lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + X + Y + 
                  tc45 + tc315 + aea20_1 + aea20_2 + aea20_12 + aea20_13 +"

# Complete the model formula
fo_c = paste(fo_firstPart, layer_names)
fo = as.formula(fo_c)

# Overlay points and layers
ov_dp_dl = over(sp_df["bcNitrate"], dist_layers)

# Create df with the data from d and the buffer distance data
final_d = cbind(d, ov_dp_dl)

# End time measurement
end_time_dmc = Sys.time()
bygone_time_dmc = end_time_dmc - start_time_dmc
print(bygone_time_dmc)
##
## End (data and formula preparation)
####
################################################################################
## End (preparation)
################################################################################


################################################################################
## Random Forest for spatial data (RFsp) (with distance matrix columns as 
## explanatory variables) prediction 
################################################################################
################################################################################
## End (RFsp) prediction)
################################################################################


################################################################################
## RFsp spatial leave one out cross validation 
################################################################################

####
## Cross validation
## 

# Create model function 
RFsp_fun = function(formula, data, fo_firstPart, idCol){
  # Set first part of the model formula (simplified for testing)
  fo_firstPart = fo_firstPart
  # Set the second part of the model formula
  layers_for_formular = paste(data[,idCol], collapse="+")
  # Complete the model formula
  c_formula = as.formula(paste(fo_firstPart, layers_for_formular))
  # Create model
  RF_dcm_model = ranger::ranger(formula = c_formula,
                                data = data,
                                oob.error = FALSE,
                                seed = 7)
  return(RF_dcm_model)
}

# Create prediction function
RFsp_pred_fun = function(object, newdata){
  RF_dcm_prediction = predict(object = object, 
                              data = newdata)
  return(RF_dcm_prediction$predictions)
}

i = 500
test = RFsp_fun(formula = fo_RF, data = d[-c(i),], fo_firstPart = 
                  fo_firstPart, idCol = idCol)
test1 = RFsp_pred_fun(object = test, newdata = d[i, ])

# Start time measurement
start_time = Sys.time()
print(start_time)

# Perform the spatial cross-validation
sp_cv_RFsp = sperrorest::sperrorest(formula = fo_RF, data = final_d, 
                                  coords = c("X","Y"), 
                                  model_fun = RFsp_fun,
                                  model_args = list(fo_firstPart=fo_firstPart,
                                                    idCol = idCol),
                                  pred_fun = RFsp_pred_fun,
                                  smp_fun = partition_fun, 
                                  smp_args = smp_args,
                                  importance = TRUE, 
                                  imp_permutations = n_perm,
                                  imp_variables = imp_vars_RF,
                                  imp_sample_from = "all",
                                  distance = TRUE)

# Get test RMSE
test_RMSE = sp_cv_RFsp$error_rep$test_rmse
test_RMSE

# End time measurement
end_time = Sys.time()
bygone_time = end_time - start_time
print(bygone_time)

# Set file name 
file_name = paste("Results/",data_set,"_sp_cv_RFsp_",as.character(round(buffer)),
                  "_+", as.character(tolerance), "_", as.character(n_perm),
                  ".rda", sep = "")
# Save result 
save(sp_cv_RFsp, bygone_time, bygone_time_dmc, file = file_name)
##
## End (cross validation)
#### 
################################################################################
## End (RFsp spatial leave one out cross validation) 
################################################################################
