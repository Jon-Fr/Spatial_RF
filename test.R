# Calculate mean predcition distance
test1 = sp_cv_MLR$error_fold[[1]][[1]]$distance

nn_distances = c()
for (i in 1:length(sp_cv_MLR$error_fold[[1]])){
  nn_dist = sp_cv_MLR$error_fold[[1]][[i]]$distance
  nn_distances = append(nn_distances, nn_dist)
}
mean(nn_distances)
