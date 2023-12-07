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

p_load("RandomForestsGLS")

# Fewer decimal places
#options(digits=4)

# Load data and formula (for now use a subset)
load("data_points_subset.rda")
d = subset_dp

d$y2 = d$y
d$x2 = d$x

# Coordinate columns
coord_columns = c("x2", "y2")

# Observation column
obs_col = "bcNitrate"

# Covariate columns
covari_columns = c("crestime", "cgwn", "cgeschw", "log10carea", "elevation",
                   "cAckerland", "log10_gwn", "agrum_log10_restime", 
                   "Ackerland", "lbm_class_Gruenland", "lbm_class_Unbewachsen", 
                   "lbm_class_FeuchtgebieteWasser", "lbm_class_Siedlung", "x2", 
                   "y2")


vec1 = round(runif(n=758, min=0, max=1), 0)

num_of_rows = nrow(d)
coordinates_matrix = as.matrix(d[, coord_columns])
coordinates_matrix = matrix(coordinates_matrix,num_of_rows,2)
observations = d[, obs_col]
covariates_matrix = as.matrix(d[, covari_columns])
num_of_cols = ncol(covariates_matrix)
covariates_matrix = matrix(covariates_matrix, num_of_rows,num_of_cols)

set.seed(1)
 

est_known_short <- RFGLS_estimate_spatial(coords = coordinates_matrix[c(3:200),], y = observations[c(3:200)],
                                          X = matrix(covariates_matrix[c(3:200),],198,num_of_cols), ntree = 250, cov.model = "spherical",
                                          nthsize = 20, param_estimate = TRUE, mtry = 5)

RFGLS_predict_spatial <- RFGLS_predict_spatial(est_known_short, matrix(coordinates_matrix[2,],1,2),
                                               matrix(covariates_matrix[2,],1,num_of_cols))