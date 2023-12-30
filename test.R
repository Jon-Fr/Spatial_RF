source("auxiliary_functions.R", encoding = "UTF-8")

# Number of potions with prediction distance between ... and ...

load("Data/WuS_SuB.rda")

n_p = partition_tt_dist(data = WuS_SuB, coords = c("X", "Y"), buffer = 100, 
                        tolerance = 50)

# Calculate mean prediction distance
test1 = sp_cv_MLR$error_fold[[1]][[1]]$distance

nn_distances = c()
for (i in 1:length(sp_cv_MLR$error_fold[[1]])){
  nn_dist = sp_cv_MLR$error_fold[[1]][[i]]$distance
  nn_distances = append(nn_distances, nn_dist)
}
mean(nn_distances)


# Load result
file_name = "WuS_SuB_sp_cv_bRF_100_+50_10.rda"
load(paste("Results/",file_name, sep = ""))

test_RMSE = sp_cv_bRF$error_rep$test_rmse
imp <- summary(sp_cv_bRF$importance)

# Create a barplot - looks better with greater importances at the top:
imp <- imp[order(imp$mean.rmse, decreasing = TRUE),]
par(mar = c(5,7,1,1)) # bottom, left, top, right margins
barplot(imp$mean.rmse, names.arg = rownames(imp), horiz = TRUE, las = 1, 
        xlab = "Mean increase in RMSE")

test_RMSE
bygone_time

