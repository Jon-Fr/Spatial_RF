source("auxiliary_functions.R", encoding = "UTF-8")

# Number of potions with prediction distance between ... and ...

load("Data/WuS_SuB.rda")
load("Data/NuM_L.rda")

n_p_vec_w_ = c()
n_p_vec_n_ = c()
buffer_vec = c(0, 100, 400, 900, 1600, 3600, 6400, 10000, 16900, 25600, 40000)
tolerance_vec = c(50, 50, 100, 100, 100, 100, 100, 100, 100, 100, 100)

for (i in 1:length(buffer_vec)){
  print(buffer_vec[i])
  # WuS_SuB
  n_p_w_ = partition_tt_dist(data = WuS_SuB, coords = c("X", "Y"), 
                             buffer = buffer_vec[i], 
                             tolerance = tolerance_vec[i])
  n_p_vec_w_ = append(n_p_vec_w_, n_p_w_)
  # NuM_L
  print(buffer_vec[i])
  n_p_n_ = partition_tt_dist(data = NuM_L, coords = c("X", "Y"), 
                             buffer = buffer_vec[i], 
                             tolerance = tolerance_vec[i])
  n_p_vec_n_ = append(n_p_vec_n_, n_p_n_)
}

# Calculate mean prediction distance
test1 = sp_cv_MLR$error_fold[[1]][[1]]$distance

nn_distances = c()
for (i in 1:length(sp_cv_MLR$error_fold[[1]])){
  nn_dist = sp_cv_MLR$error_fold[[1]][[i]]$distance
  nn_distances = append(nn_distances, nn_dist)
}
median(nn_distances)


# Load result
file_name = "WuS_SuB_sp_cv_UK_100_+50_10.rda"
load(paste("Results/bcNitrate/WuS_SuB/",file_name, sep = ""))

bygone_time
RMSE = sp_cv_UK$error_rep$test_rmse
RMSE



# Determine most imporatne variables
imp <- summary(sp_cv_UK$importance)

sorted_df <- imp[order(imp$mean.rmse),]

sorted_rN = rownames(sorted_df)


# Create a barplot - looks better with greater importance at the top:
imp <- imp[order(imp$mean.rmse, decreasing = TRUE),]
par(mar = c(5,7,1,1)) # bottom, left, top, right margins
barplot(imp$mean.rmse, names.arg = rownames(imp), horiz = TRUE, las = 1, 
        xlab = "Mean ... in RMSE")



################################################################################
# Test importance
################################################################################
library("pacman")
p_load("sperrorest")

voi = c(1:1000)
imp_var1 = sqrt(voi) 
random_vec = runif(1000, min = -5, max = 5)
imp_var2 = voi^2 + random_vec
random_var1 = runif(1000, min = -5, max = 5)
random_var2 = runif(1000, min = 0, max = 100)
X = runif(1000, min = 0, max = 1)
Y = runif(1000, min = 0, max = 1)
d = data.frame(voi, imp_var1, imp_var2, random_var1, random_var2, X, Y)

fo_lm = as.formula(voi ~ imp_var1 + imp_var2 + random_var1 + random_var2)
imp_vars_lm = all.vars(fo_lm)[-1]

# Create model function 
lm_fun = function(formula, data){
  lm_m = lm(formula, data)
  return(lm_m)
}

# Create prediction function
lm_pred_fun = function(object, newdata){
  predi = predict(object = object, newdata = newdata)
  return(predi)
}

# Perform the spatial cross-validation
sp_cv_MLR = sperrorest::sperrorest(formula = fo_lm, data = d, coords = c("X","Y"), 
                                   model_fun = lm_fun, 
                                   pred_fun = lm_pred_fun,
                                   smp_fun = partition_loo, 
                                   smp_args = list(buffer = 0),
                                   importance = TRUE, 
                                   imp_permutations = 10,
                                   imp_variables = imp_vars_lm,
                                   imp_sample_from = "all",
                                   distance = FALSE)
################################################################################
# End (test importance)
################################################################################


################################################################################
## Test area (data handling)
################################################################################
####
## Create local sub subsets
##
NuM_L_sub = sperrorest::partition_kmeans(data = NuM_L, 
                                         coords = c("X", "Y"), 
                                         nfold = 10,
                                         repetition = 1, 
                                         seed1 = 123, 
                                         return_factor = FALSE,
                                         balancing_steps = 1, 
                                         order_clusters = TRUE)

WuS_SuB_sub = sperrorest::partition_kmeans(data = WuS_SuB, 
                                           coords = c("X", "Y"), 
                                           nfold = 10,
                                           repetition = 1, 
                                           seed1 = 123, 
                                           return_factor = FALSE,
                                           balancing_steps = 1, 
                                           order_clusters = TRUE)

# Save the local sub subsets
save_local_subsets = function(data, resampling_object, name_fp){
  for (i in 1:length(resampling_object$"1")){
    rows = resampling_object[["1"]][[i]]$test
    sub_subset = data[rows,]
    name = paste(name_fp, i,".rda", sep = "")
    save(sub_subset, fo, file = name)
  }
}

save_local_subsets(data = NuM_L, resampling_object = NuM_L_sub, 
                   name_fp = "Data/NuM_L_sub_")

save_local_subsets(data = WuS_SuB, resampling_object = WuS_SuB_sub, 
                   name_fp = "Data/WuS_SuB_sub_")
##
## End (create local sub subsets)
####


####
## "Create" prediction areas (concave hull + 1 km buffer) and 
## calculate statistical parameters for the prediction distance 

# Create data vector
dv = c("NuM_L_sub_4")

# Create vector for the results
info_pd_vec = c()

# Loop
for (i in dv){
  # Load data
  load(paste("Data/", i, ".rda", sep = ""))
  d = sub_subset
  
  # "Create" prediction areas
  c_hull_buff(data = d, buffer_dist = 1000, c_r_s = "EPSG:25832", 
              coords_vec = c("X", "Y"), 
              out_path = paste("Data/", i, "_prediction_area", sep = ""))
  
  # Get information about the prediction distance 
  info_pd = info_predDist(path_predArea = paste("Data/", i, "_prediction_area.gpkg", sep = ""), 
                          dataPoints_df = d,
                          c_r_s = "EPSG:25832",
                          resolution = 100,
                          xy = c("X", "Y"))
  # Append results
  info_pd_vec = append(info_pd_vec, info_pd)
}
##
## End ("create" prediction areas (concave hull + 1 km buffer) and 
## calculate statistical parameters for the prediction distance )
####


####
## Subset data points (with external vector file)
##

# Convert the data df to a spatial vector
spv_dp = terra::vect(d, geom=c("X", "Y"))

# Set crs
terra::crs(spv_dp) <- "EPSG:25832"

# Load test area file
test_area = terra::vect("Data/.gpkg")

# Clip training data points
subset_dp = terra::crop(spv_dp, test_area)

# Convert the spatial vector to a df
subset_dp = as.data.frame(subset_dp, geom = "XY")

# Save the subset with the formula
save(subset_dp, fo, file = "Data/.rda")
##
## End (subset data points (with external vector file))
#### 
################################################################################
################################################################################


temp = d[d$GR_NAME == "Nord- und mitteldeutsches Lockergesteinsgebiet" | d$GR_NAME == "West- und süddeutsches Schichtstufen- und Bruchschollenland",] 
temp$nClass = temp$subMittelwert

temp$nClass[temp$nClass < 10] <- 0
temp$nClass[temp$nClass >= 10 & temp$nClass < 20] <- 1
temp$nClass[temp$nClass >= 20 & temp$nClass < 30] <- 2
temp$nClass[temp$nClass >= 30 & temp$nClass < 40] <- 3
temp$nClass[temp$nClass >= 40 & temp$nClass < 50] <- 4
temp$nClass[temp$nClass >= 50] <- 5

