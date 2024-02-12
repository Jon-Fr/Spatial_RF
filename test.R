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
file_name = "WuS_SuB_sp_cv_MLR_0_+all_10.rda"
load(paste("Results/WuS_SuB/",file_name, sep = ""))

RMSE = sp_cv_MLR$error_rep$test_rmse

RMSE
bygone_time

imp <- summary(sp_cv_OK_RF$importance)

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
