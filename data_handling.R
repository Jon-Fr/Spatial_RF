################################################################################
## Preparation (strictly necessary)
################################################################################
# Load necessary packages
library("pacman")
p_load("sp")
p_load("sf")
p_load("terra")
p_load("sperrorest")
p_load("concaveman")
p_load("MASS")

# Additional functions that are not included in packages
source("auxiliary_functions.R", encoding = "UTF-8")

# Load data
load("Data/run_nitrate_de.rda")

d = d

# Add ID column for all data points
d$ID_all_DE = 1:nrow(d)

# Fewer decimal places, apply penalty on exponential notation 
options("scipen"= 999, "digits"=4)

# Compute tilted coordinates (formula: Møller et al. 2020) at 45 and 315 degree 
# 315 is choose instead of 135 beaus 135 will produce negative coordinates
d$tc45 = sqrt(d$Y^2 + d$X^2) * cos(45 - atan(d$Y / d$X))
d$tc315 = sqrt(d$Y^2 + d$X^2) * cos(315 - atan(d$Y / d$X))
################################################################################
## End (preparation)
################################################################################


################################################################################
## Data points, formula and subsets
################################################################################
####
## Export all data points
##

# Convert the data df to a spatial vector
spv_dp = terra::vect(d, geom=c("X", "Y"))

# Set crs
terra::crs(spv_dp) <- "EPSG:25832"

# Write file
terra::writeVector(spv_dp, "Data/data_points.gpkg", overwrite = TRUE)
##
## End (export all data points)
####


####
## Subset data points (based on Hydogeologische Großräume) and 
## adjust the formula
##

# Subset based on Hydogeologische Großräume
NuM_L = d[d$GR_NAME == "Nord- und mitteldeutsches Lockergesteinsgebiet",] 
WuS_SuB = d[d$GR_NAME == "West- und süddeutsches Schichtstufen- und Bruchschollenland",]


# Add ID column for the subsets
NuM_L$ID_NuM_L = 1:nrow(NuM_L)
WuS_SuB$ID_WuS_SuB = 1:nrow(WuS_SuB)


## Solve aea20 problem
# Get unique values and frequency 
aea_NuM_L = table(NuM_L$aea20)
print(aea_NuM_L)
aea_WuS_SuB = table(WuS_SuB$aea20)
print(aea_WuS_SuB)
# Merge (if there are similar levels) or drop levels with few (below 50) occurrences 
NuM_L$aea20[NuM_L$aea20 == 1] <- 2
NuM_L$aea20[NuM_L$aea20 == 4] <- 3
NuM_L = NuM_L[NuM_L$aea20 != 5, ] 
NuM_L = NuM_L[NuM_L$aea20 != 6, ] 
NuM_L$aea20[NuM_L$aea20 == 9] <- 8
NuM_L$aea20[NuM_L$aea20 == 13] <- 12
WuS_SuB$aea20[WuS_SuB$aea20 == 3] <- 2
WuS_SuB$aea20[WuS_SuB$aea20 == 7] <- 2
WuS_SuB$aea20[WuS_SuB$aea20 == 8] <- 2
WuS_SuB$aea20[WuS_SuB$aea20 == 14] <- 13
WuS_SuB = WuS_SuB[WuS_SuB$aea20 != 15, ]
WuS_SuB = WuS_SuB[WuS_SuB$aea20 != 20, ]
# Drop uncessary levels
NuM_L$aea20 = droplevels(NuM_L$aea20)
WuS_SuB$aea20 = droplevels(WuS_SuB$aea20)
# Check results
aea_NuM_L_new = table(NuM_L$aea20)
print(aea_NuM_L_new)
aea_WuS_SuB_new = table(WuS_SuB$aea20)
print(aea_WuS_SuB_new)
# Define indicator variables 
NuM_L$aea20_2 = (NuM_L$aea20 == "2") * 1
NuM_L$aea20_8 = (NuM_L$aea20 == "8") * 1
NuM_L$aea20_12 = (NuM_L$aea20 == "12") * 1
WuS_SuB$aea20_1 = (WuS_SuB$aea20 == "1") * 1
WuS_SuB$aea20_2 = (WuS_SuB$aea20 == "2") * 1
WuS_SuB$aea20_12 = (WuS_SuB$aea20 == "12") * 1
WuS_SuB$aea20_13 = (WuS_SuB$aea20 == "13") * 1
## Create adjusted formulas
# gr will not be used because the AOI is one gr
# kf_, lch_gwl/_gwl and gc_ will not be used because in the NuM_L AOI there is only 
# one level with a relevant number of (above 50) observations
# aea20 will be adjusted 
# Formula for linear models without the oblique coordinates
fo_lm_NuM_L = as.formula(bcNitrate ~ crestime + cgwn + cgeschw + log10carea + 
                     elevation + nfk + humus + cAckerland + log10_gwn + 
                     agrum_log10_restime + agrum_log10_gwn + agrum_log10_geschw +
                     Ackerland + lbm_class_Gruenland + lbm_class_Unbewachsen + 
                     lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + 
                     aea20_2 + aea20_8 + aea20_12 + X + Y)
# Formula for the RF models 
fo_RF_NuM_L = as.formula(bcNitrate ~ crestime + cgwn + cgeschw + log10carea + 
                     elevation + nfk + humus + cAckerland + log10_gwn + 
                     agrum_log10_restime + agrum_log10_gwn + agrum_log10_geschw +
                     Ackerland + lbm_class_Gruenland + lbm_class_Unbewachsen + 
                     lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + 
                     aea20_2 + aea20_8 + aea20_12 + X + Y + tc45 + tc315)
# Formula for linear models without the oblique coordinates
fo_lm_WuS_SuB = as.formula(bcNitrate ~ crestime + cgwn + cgeschw + log10carea + 
                           elevation + nfk + humus + cAckerland + log10_gwn + 
                           agrum_log10_restime + agrum_log10_gwn + agrum_log10_geschw +
                           Ackerland + lbm_class_Gruenland + lbm_class_Unbewachsen + 
                           lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + 
                           aea20_1 + aea20_2 + aea20_12 + aea20_13 + X + Y)
# Formula for the RF models 
fo_RF_WuS_SuB = as.formula(bcNitrate ~ crestime + cgwn + cgeschw + log10carea + 
                           elevation + nfk + humus + cAckerland + log10_gwn + 
                           agrum_log10_restime + agrum_log10_gwn + agrum_log10_geschw +
                           Ackerland + lbm_class_Gruenland + lbm_class_Unbewachsen + 
                           lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + 
                           aea20_1 + aea20_2 + aea20_12 + aea20_13 + X + Y + 
                           tc45 + tc315)  
# Box-Cox transformation of the variable of interest for the AOIs
fit = lm(subMittelwert ~ crestime + cgwn + cgeschw + log10carea + 
           elevation + nfk + humus + cAckerland + log10_gwn + 
           agrum_log10_restime + agrum_log10_gwn + agrum_log10_geschw +
           Ackerland + lbm_class_Gruenland + lbm_class_Unbewachsen + 
           lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + 
           aea20_2 + aea20_8 + aea20_12 + X + Y, data = NuM_L)
b = MASS::boxcox(fit)
lambda = b$x[which.max(b$y)]
lambda
NuM_L$bcNitrate = boxcox(NuM_L$subMittelwert, lambda = lambda)

fit = lm(subMittelwert ~ crestime + cgwn + cgeschw + log10carea + 
           elevation + nfk + humus + cAckerland + log10_gwn + 
           agrum_log10_restime + agrum_log10_gwn + agrum_log10_geschw +
           Ackerland + lbm_class_Gruenland + lbm_class_Unbewachsen + 
           lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + 
           aea20_1 + aea20_2 + aea20_12 + aea20_13 + X + Y, data = WuS_SuB)
b = MASS::boxcox(fit)
lambda = b$x[which.max(b$y)]
lambda
WuS_SuB$bcNitrate = boxcox(WuS_SuB$subMittelwert, lambda = lambda)

# Get information about the prediction distance and about the distance between 
# the data points (nearest neighbor distances)
info_d_NuM_L = info_dist(path_predArea = "Data/NuM_L_Gebiet.gpkg", 
                        dataPoints_df = NuM_L,
                        c_r_s = "EPSG:25832",
                        resolution = 100,
                        xy = c("X", "Y"))
info_d_WuS_SuB = info_dist(path_predArea = "Data/WuS_SuB_Gebiet.gpkg", 
                    dataPoints_df = WuS_SuB,
                    c_r_s = "EPSG:25832",
                    resolution = 100,
                    xy = c("X", "Y"))

# Save the subsets with the formulas and the distance information
save(NuM_L, fo_lm_NuM_L, fo_RF_NuM_L, info_d_NuM_L, file = "Data/NuM_L.rda")
save(WuS_SuB, fo_lm_WuS_SuB, fo_RF_WuS_SuB, info_d_WuS_SuB, file = "Data/WuS_SuB.rda")
##
## End (subset data points (based on Hydogeologische Großräume) and
## adjust the formula)
####
################################################################################
## End (data points, formula and subsets)
################################################################################


################################################################################
## Test area
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