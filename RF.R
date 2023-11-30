################################################################################
## Preparation (strictly necessary)
################################################################################
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

p_load("ranger")
p_load("dplyr")

# Additional functions that are not included in packages
source("auxiliary_functions.R", encoding = "UTF-8")
source("spdiagnostics-functions.R", encoding = "UTF-8") # Brenning 2022

# Fewer decimal places, apply penalty on exponential notation 
options("scipen"= 999, "digits"=4)

# Load data (for now use a subset)
load("Data/NuM_L_sub_4.rda")
d = sub_subset

# Get information about the prediction distance 
info_pd = info_predDist(path_predArea = "Data/NuM_L_sub_4_prediction_area.gpkg", 
                        dataPoints_df = d,
                        c_r_s = "EPSG:25832",
                        resolution = 100,
                        xy = c("X", "Y"))

pd_df = info_pd$predDist_df
hist(pd_df$lyr.1)
third_quartile = quantile(x = pd_df$lyr.1, probs = c(0.75))
tq_pd = third_quartile
max_pd = info_pd$max_predDist
mean_pd = info_pd$mean_predDist
sd_pd = info_pd$sd_predDist
med_pd = info_pd$med_predDist
mad_pd = info_pd$mad_predDist

# Adjusted formula 
fo = as.formula(bcNitrate ~ crestime + cgwn + cgeschw + log10carea + elevation + 
                  cAckerland + log10_gwn + agrum_log10_restime + Ackerland + 
                  lbm_class_Gruenland + lbm_class_Unbewachsen + 
                  lbm_class_FeuchtgebieteWasser + lbm_class_Siedlung + X + Y)
################################################################################
## End (preparation)
################################################################################


################################################################################
## Random Forest (RF) prediction 
################################################################################
################################################################################
## End (RF prediction) 
################################################################################


################################################################################
## RF spatial leave one out cross validation 
################################################################################

####
## Cross validation
## 

# Create model function 
RF_fun = function(formula, data){
  RF_model = ranger::ranger(formula = formula, 
                            data = data)
  return(RF_model)
}

# Create prediction function
RF_pred_fun = function(object, newdata){
  RF_prediction = predict(object = object,
                          data = newdata)
  return(RF_prediction$predictions)
}

# Start time measurement
start_time = Sys.time()
print(start_time)

# Perform the spatial cross-validation
# Future for parallelization
future::plan(future.callr::callr, workers = 10)
sp_cv_RF = sperrorest::sperrorest(formula = fo, data = d, coords = c("X","Y"), 
                                  model_fun = RF_fun, 
                                  pred_fun = RF_pred_fun,
                                  smp_fun = partition_loo, 
                                  smp_args = list(buffer = med_pd))

# Get test RMSE
test_RMSE = sp_cv_RF$error_rep$test_rmse
test_RMSE

# End time measurement
end_time = Sys.time()
bygone_time = end_time - start_time
##
## End (cross validation)
#### 
################################################################################
## End (Random Forest (RF) spatial leave one out cross validation) 
################################################################################


################################################################################
## RF hyperparameter tuning
################################################################################
#####
## Preparation
##

# Set path for storing the results
out_path = "C:\\Users\\J_F\\Desktop\\Uni\\Master\\Semester_6\\Masterarbeit\\R\\test_RF_tuning"

# Get the mean and median prediction distance 
#(for now use the test area as prediction area)
m_m_pd = mean_med_predDist(path_predArea = "test_area.gpkg", dataPoints_df = d,
                           c_r_s = "EPSG:25832")

# Set buffer for the spatial loo cv
buffer = m_m_pd$med_predDist
##
## End (preparation)
####


####
## Create tuning grid
##

# Hyperparameter
n_trees = c(100,200)
v_mtry = c(1:3)
min_node_size = c(1,5,10,20)
min_bucket = c(1,3,5)
max_depth = c(0,2,5,10)
sample_fraction = c(0.632,0.95,1)
splitrule_r = c("variance", "extratrees", "maxstat", "beta")
# Only for extratrees
num_random_splits = c(2,3)
# Only for maxstat
alpha_v = c(0.05,0.1)
minprop_qv = c(0.25,0.5)

# Create tuning grid
t_grid = expand.grid(n_trees=n_trees, v_mtry=v_mtry, 
                     min_node_size=min_node_size, min_bucket=min_bucket, 
                     sample_fraction=sample_fraction, max_depth=max_depth,
                     splitrule_r=splitrule_r, 
                     num_random_splits=num_random_splits, 
                     alpha_v=alpha_v, minprop_qv=minprop_qv)

# Steps to remove unnecessary rows
t_grid$num_random_splits[t_grid$splitrule_r == "variance" |
                           t_grid$splitrule_r == "beta" |
                           t_grid$splitrule_r == "maxstat"] = "99"

t_grid$alpha_v[t_grid$splitrule_r == "variance" |
                 t_grid$splitrule_r == "beta" |
                 t_grid$splitrule_r == "extratrees"] = "99"

t_grid$minprop_qv[t_grid$splitrule_r == "variance" |
                    t_grid$splitrule_r == "beta" |
                    t_grid$splitrule_r == "extratrees"] = "99"

t_grid = t_grid %>% dplyr::distinct()
##
## End (create tuning grid)
####


####
## Create tuning functions
## 

# Create model function for tuning
tuning_RF_fun = function(formula, data, h_p_c){
  if (h_p_c[,"splitrule_r"]=="variance" | h_p_c[,"splitrule_r"]=="beta"){
    RF_model = ranger(formula = formula,
                      data = data,
                      num.trees = h_p_c[,"n_trees"],
                      mtry = h_p_c[,"v_mtry"],
                      splitrule = h_p_c[,"splitrule_r"],
                      min.bucket = h_p_c[,"min_bucket"],
                      min.node.size = h_p_c[,"min_node_size"],
                      sample.fraction = h_p_c[,"sample_fraction"],
                      max.depth = h_p_c[,"max_depth"],
                      num.threads = 1,
                      quantreg = FALSE,
                      oob.error = FALSE)
  }else if (h_p_c[,"splitrule_r"]=="extratrees"){
    RF_model = ranger(formula = formula,
                      data = data,
                      num.trees = h_p_c[,"n_trees"],
                      mtry = h_p_c[,"v_mtry"],
                      splitrule = h_p_c[,"splitrule_r"],
                      min.bucket = h_p_c[,"min_bucket"],
                      min.node.size = h_p_c[,"min_node_size"],
                      sample.fraction = h_p_c[,"sample_fraction"],
                      max.depth = h_p_c[,"max_depth"],
                      num.threads = 1,
                      quantreg = FALSE,
                      oob.error = FALSE,
                      num.random.splits = h_p_c[,"num_random_splits"])    
  } else {
    RF_model = ranger(formula = formula,
                      data = data,
                      num.trees = h_p_c[,"n_trees"],
                      mtry = h_p_c[,"v_mtry"],
                      splitrule = h_p_c[,"splitrule_r"],
                      min.bucket = h_p_c[,"min_bucket"],
                      min.node.size = h_p_c[,"min_node_size"],
                      sample.fraction = h_p_c[,"sample_fraction"],
                      max.depth = h_p_c[,"max_depth"],
                      num.threads = 1,
                      quantreg = FALSE,
                      oob.error = FALSE,
                      alpha = h_p_c[,"alpha_v"],
                      minprop = h_p_c[,"minprop_qv"])
  }
  return(RF_model)
}

# Create prediction function for tuning
tuning_RF_pred_fun = function(object, newdata){
  RF_prediction = predict(object = object,
                          data = newdata,
                          num.threads = 1)
  return(RF_prediction$predictions)
}

n_trees            = c(200)
v_mtry             = c(3)
min_node_size      = c(5)
min_bucket         = c(2)
sample_fraction    = c(0.632)
max_depth          = c(10)
splitrule_r        = c("extratrees")
num_random_splits  = c(5)
alpha_v            = c(99)
minprop_qv         = c(99)
test0 = data.frame(n_trees, v_mtry, min_node_size, min_bucket, sample_fraction, 
                   max_depth, splitrule_r, num_random_splits, alpha_v, minprop_qv)
test = tuning_RF_fun(formula = fo, data = d[1:100,], h_p_c = test0)

test1 = tuning_RF_pred_fun(object = test, newdata = d[101,])


# Create tuning function
RF_tuning = function(formula, data, tuning_grid, tuning_pred_fun, 
                     tuning_model_fun, out_path, buffer){
  # Foreach for parallel processing
  foreach::foreach (i = 1:nrow(tuning_grid), 
                    .packages = c("sperrorest", "ranger")) %dopar%{
    h_p_c=tuning_grid[i,]
    # Spatial cross validation with different hyperparameter values
    sp_cv_RF = sperrorest(formula = formula, data = data, coords = c("X","Y"),
                          model_fun = tuning_model_fun,
                          model_args = list(h_p_c=h_p_c),
                          pred_fun = tuning_pred_fun, 
                          smp_fun = partition_loo, 
                          smp_args = list(buffer = buffer))
    # Get test RMSE
    test_RMSE = sp_cv_RF$error_rep$test_rmse
    # Write the results
    path = out_path
    name1 = paste("model",as.character(i),"hpc.csv", sep = "_")
    name2 = paste("model",as.character(i),"error_summary.csv", sep = "_")
    
    write.table(x=tuning_grid[i,], file = paste(path,name1, sep = "\\" ), 
                append = FALSE, sep = ",", dec = ".",
                row.names = FALSE, col.names = FALSE)
    write.table(x=test_RMSE, file = paste(path,name2, sep = "\\" ),
                append = FALSE, sep = ",", dec = ".",
                row.names = FALSE, col.names = FALSE)
  }
}
##
## Create tuning functions
####


####
## Hyperparameter tuning
##

# Start time measurement
start_time = Sys.time()

# Setup backend to use many processors
totalCores = parallel::detectCores()

# Leave two cores to reduce computer load
cluster = parallel::makeCluster(totalCores[1]-2) 
doParallel::registerDoParallel(cluster)

# Execute the tuning function
RF_tuning(formula = fo, data = d[1:100,], tuning_grid = t_grid, 
          tuning_model_fun = tuning_RF_fun, 
          tuning_pred_fun = tuning_RF_pred_fun, 
          out_path = out_path, buffer = buffer)

# Stop cluster
parallel::stopCluster(cluster)

# End time measurement
end_time = Sys.time()
print("bygone time")
print(end_time - start_time)
##
## End (hyperparameter tuning)
####


####
## Read results of hyperparameter tuning 
##
# Create dfs for the results
# h_y_c_df
columns_h_y_c = c("n_trees","v_mtry","min_node_size","max_depth",
                  "sample_fraction") 
h_y_c_df = data.frame(matrix(nrow = 0, ncol = length(columns_h_y_c))) 
# test_rmse_df
columns_error = c("RMSE") 
test_rmse_df = data.frame(matrix(nrow = 0, ncol = length(columns_error))) 

# Read the results
for (i in 1:nrow(t_grid)){
  
  path = out_path
  name1 = paste("model",as.character(i),"hpc.csv", sep = "_")
  name2 = paste("model",as.character(i),"error_summary.csv", sep = "_")
  print(i)
  temp_h_y_c_df = read.csv(file = paste(path,name1, sep = "\\" ),
                           header = FALSE)
  print("i")
  
  temp_rmse_df = try(read.csv(file = paste(path,name2, sep = "\\" ), 
                            header = FALSE))
  h_y_c_df = rbind(h_y_c_df, temp_h_y_c_df[1,])
  tryCatch({
    test_rmse_df = rbind(test_rmse_df, temp_rmse_df[1,])
  }, error = function(e){
    error_vec = c(999)
    test_rmse_df = rbind(test_rmse_df, error_vec)
  })
} 

# Set column names
colnames(h_y_c_df) = columns_h_y_c
colnames(test_rmse_df) = columns_error
##
## End (read results of hyperparameter tuning) 
####
################################################################################
## End (RF hyperparameter tuning)
################################################################################


################################################################################
## Test area
################################################################################

####
## Explore the relationship between buffer distance and RMSE
##

# Start time measurement
start_time = Sys.time()

# Setup backend to use many processors
totalCores = parallel::detectCores()

# Leave two cores to reduce computer load
cluster = parallel::makeCluster(totalCores[1]-2) 
doParallel::registerDoParallel(cluster)

# explore
test = data.frame(seq(0, 20000, 1000))

test2 = foreach::foreach(i = iter(test, by="row"), .combine=c, 
                 .packages = c("sperrorest", "ranger")) %dopar%{
  sp_cv_RF = sperrorest::sperrorest(formula = fo, data = d, coords = c("X","Y"), 
                                    model_fun = RF_fun, 
                                    pred_fun = RF_pred_fun,
                                    smp_fun = partition_loo,
                                    smp_args = list(buffer = i),
                                    mode_rep = "sequential", 
                                    mode_fold = "sequential")
  
  test_RMSE = sp_cv_RF$error_rep$test_rmse
  }

plot(test2~test[ ,1])

# Stop cluster
parallel::stopCluster(cluster)

# End time measurement
end_time = Sys.time()
bygone_time = end_time - start_time
##
## End (explore the relationship between buffer distance and RMSE)
####
################################################################################
## End (test area)
################################################################################
