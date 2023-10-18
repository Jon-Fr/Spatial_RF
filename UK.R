################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sp")
p_load("sf")
p_load("terra")
p_load("gstat")
p_load("sperrorest")
p_load("nlme")
p_load("automap")

# Fewer decimal places
options(digits=4)

# Load data and formula
load("data_points_subset.rda")
d = subset_dp

# Simplified formula for testing
fo = as.formula(bcNitrate ~ crestime + cgwn + cgeschw + log10carea + elevation + 
  cAckerland + log10_gwn + agrum_log10_restime + Ackerland + 
  lbm_class_Gruenland + lbm_class_Unbewachsen + lbm_class_FeuchtgebieteWasser + 
  lbm_class_Siedlung)
################################################################################
## End (preparation)
################################################################################


################################################################################
## Universal Kriging (UK) prediction/interpolation 
################################################################################

#### 
## Preparation 
##

# Create a spatial points df 
sp_df = sp::SpatialPointsDataFrame(d[,c("x","y")], d)
##
## End (preparation )
####


##### 
## Preliminary analyses
## 

## Conduct multiple linear regression (MLR)
# Create MLR model
MLR_model = lm(fo, data=d)
summary(MLR_model)
# Some diagnostic plots
plot(residuals(MLR_model) ~ predict(MLR_model), cex.lab = 1.25,
     xlab = "predicted values", ylab = "residuals")
abline(h=0,lty="dashed")

# Directional semivariograms
dir_svgm = gstat::variogram(fo, data=sp_df, cutoff = 10000, 
                            alpha = c(0, 45, 90, 135)) 
#0:north, 45:north-east, 90:east, 135:south-east 
plot(dir_svgm, cex.lab = 1.25, xlab = "distance (m)")
##
## End (preliminary analyses)
####


####
## Create variogram model for the UK interpolation (the automatized way)
##

# OLS residual variogram model fitting
resid_vmf_au = automap::autofitVariogram(formula = fo, input_data = sp_df, 
                                         model = c("Sph"), cressie = TRUE)

# GLS residual variogram model fitting
resid_vmf_gls_au = automap::autofitVariogram(formula = fo,input_data = sp_df, 
                                             model = c("Sph"), 
                                             GLS.model = resid_vmf_au
                                             ["var_model"]$var_model, 
                                             cressie = TRUE)

# Obtain range and nugget-to-sill ratio
resid_vmf_gls_au = resid_vmf_gls_au["var_model"]$var_model
range = resid_vmf_gls_au[2,"range"]
range
n_t_s = resid_vmf_gls_au[1,"psill"] / sum(resid_vmf_gls_au[,"psill"])
n_t_s

# Plot result 
# Prepare the correlogram for the 
# generalized least square regression (GLS) model
autocor = nlme::corSpher(c(range, n_t_s), nugget=TRUE, fixed=TRUE)
autocor = nlme::Initialize(autocor, data = sp_df)
autocor

# Perform GLS regression
gls_model = nlme::gls(fo, correlation = autocor, 
                      data = sp_df)

# Residual Semivariogram of GLS regression residuals
sp_df$gls_model_resid = resid(gls_model)
resid_v_gls = gstat::variogram(gls_model_resid ~ 1, data = sp_df, cutoff=range, 
                               cressie=TRUE)

print(plot(resid_v_gls, pl = TRUE, model = resid_vmf_gls_au))
##
## End (create variogram model for the UK interpolation (the automatized way))
####
################################################################################
## End (Universal Kriging (UK) prediction/interpolation)
################################################################################


################################################################################
## Universal Kriging (UK) spatial leave one out cross validation 
################################################################################

#####
## Get the mean and median prediction distance 
## (for now use the test area as prediction area)
##

# Load the test area file
test_area = terra::vect("test_area.gpkg")

# Create raster with same extent as the test area
raster_pl = terra::rast(test_area, resolution = 20,
                          xmin = xmin(test_area), xmax = xmax(test_area), 
                          ymin = ymin(test_area), ymax = ymax(test_area))
# Set NA to 1
raster_pl = terra::subst(raster_pl, NA, 1)

# Convert the data df to a spatial vector
spv_dp = terra::vect(d, geom=c("x", "y"), crs = "EPSG:25832")

# Set crs
terra::crs(raster_pl) = "EPSG:25832"
terra::crs(spv_dp) = "EPSG:25832"

# Calculate the distance
dist_dp_pl = terra::distance(raster_pl, spv_dp, unit="m")

# Set raster cell values to NA that where NA in the original raster
dist_dp_pl = terra::mask(dist_dp_pl, raster_pl)

# Raster to df
dist_dp_pl_df = terra::as.data.frame(dist_dp_pl)

# Get the mean prediction distance 
mean_pd = mean(x = dist_dp_pl_df[,1], na.rm=TRUE)
median_pd =  median(x = dist_dp_pl_df[,1], na.rm=TRUE)

# Remove some not longer necessary data
remove("dist_dp_pl_df", "dist_dp_pl", "raster_pl")
##
## End (Get the mean and median prediction distance)
####


####
## Cross validation
## 

# Create model function 
vmf_fun = function(formula, data){
  # Create a spatial points df 
  sp_df = sp::SpatialPointsDataFrame(coords = data[,c("x","y")], data = data)
  # OLS residual variogram model fitting
  resid_vmf = automap::autofitVariogram(formula = formula, input_data = sp_df,
                                        model = c("Sph"), cressie = TRUE)
  # GLS residual variogram model fitting
  resid_vmf_gls = automap::autofitVariogram(formula = formula,
                                            input_data = sp_df, 
                                            model = c("Sph"),
                                            GLS.model = resid_vmf["var_model"]
                                            $var_model, cressie = TRUE)
  # return variogram model an training data
  return_list = list(model = resid_vmf_gls["var_model"]$var_model, 
                     train_data = sp_df)
  return(return_list)
}

# Create prediction function
UK_pred_fun = function(object, newdata, formula){
  # Create spatial points dfs
  sp_df_train = object$train_data
  sp_df_newdata = SpatialPointsDataFrame(newdata[,c("x","y")], newdata)
  # Prediction
  uk_pred = gstat::krige(formula = formula, locations = sp_df_train, 
                         model = object$model, newdata = sp_df_newdata)
  return(uk_pred$var1.pred)
}

# Perform the spatial cross-validation
# Future for parallelization
#future::plan(future.callr::callr, workers = 5)
sp_cv_UK = sperrorest::sperrorest(formula = fo, data = d, coords = c("x","y"), 
                                  model_fun = vmf_fun, 
                                  pred_fun = UK_pred_fun,
                                  pred_args = list(formula = fo),
                                  smp_fun = partition_loo, 
                                  smp_args = list(buffer = median_pd))

# Get test RMSE
test_RMSE = sp_cv_UK$error_rep$test_rmse
test_RMSE
##
## End (Cross validation)
####
################################################################################
## End (Universal Kriging (UK) spatial leave one out cross validation)
################################################################################




