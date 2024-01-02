source("auxiliary_functions.R", encoding = "UTF-8")

# Number of potions with prediction distance between ... and ...

load("Data/WuS_SuB.rda")
load("Data/NuM_L.rda")

n_p_vec_w_ = c()
n_p_vec_n_ = c()
buffer_vec = c(0, 100, 400, 900, 1600, 3600, 6400, 10000, 16900, 25600, 40000)
tolerance_vec = c(50, 50, 100, 100, 100, 100, 100, 100, 100, 100, 100)

for (i in 10:length(buffer_vec)){
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
mean(nn_distances)


# Load result
file_name = "WuS_SuB_sp_cv_RF_0_+50_10.rda"
load(paste("Results/",file_name, sep = ""))

test_RMSE = sp_cv_RF$error_rep$test_rmse
imp <- summary(sp_cv_RF$importance)

# Create a barplot - looks better with greater importance at the top:
imp <- imp[order(imp$mean.rmse, decreasing = TRUE),]
par(mar = c(5,7,1,1)) # bottom, left, top, right margins
barplot(imp$mean.rmse, names.arg = rownames(imp), horiz = TRUE, las = 1, 
        xlab = "Mean ... in RMSE")

test_RMSE
bygone_time

