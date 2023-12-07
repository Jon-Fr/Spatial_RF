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

# Fewer decimal places
options(digits=4)

# Load data and formula (for now use a subset)
load("data_points_subset.rda")
d = subset_dp
d1 = d[1:50, ]
d2 = d[51:54, ]

# Simplified formula for testing
fo = as.formula(bcNitrate ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                  cAckerland + log10_gwn + agrum_log10_restime + Ackerland + 
                  lbm_class_Gruenland + lbm_class_Unbewachsen + 
                  lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung)

# Create model function 
RF_fun = function(formula, data){
  RF_model = ranger::ranger(formula = formula, 
                            data = data,
                            num.trees = 3)
  return(RF_model)
}

# Create prediction function
RF_pred_fun = function(object, newdata){
  RF_prediction = predict(object = object,
                          data = newdata,
                          predict.all = TRUE)
  return(RF_prediction)
}

# Aplly functions
RF_m = RF_fun(formula = fo, data = d1)

RF_pred = RF_pred_fun(object = RF_m, newdata = d2)

# Get predictions of all tress
RF_pred_pred = t(RF_pred$predictions)

# PCA
pca = prcomp(RF_pred_pred, scale. = FALSE, center = FALSE)

pca_coeff = pca$x

eigenVec = pca$rotation

