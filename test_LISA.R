# Load necessary packages
library("pacman")
p_load("sp")
p_load("sf")
p_load("terra")
p_load("elsa")

# Load data (for now use a subset)
load("data_points_subset.rda")
d = subset_dp

# For each point calulate the distance to all other points
dist_ = dist(d[,c("x","y")])

dist_mat = as.matrix(dist_)

dist_df = as.data.frame(dist_mat)

# Get the distance to the 3th nearest neighbor
dist_3th_neig = c()
for (i in 1:ncol(dist_df)){
  dist_vec = dist_df[, i]
  dist_vec = sort(dist_vec,)
  dist_3th_neig = append(dist_3th_neig, dist_vec[4])
}

# Maximum distance to the 3th nearest neighbor + 1
max_dist = max(dist_3th_neig)+1

# Create a spatial points df 
sp_df = sp::SpatialPointsDataFrame(d[,c("x","y")], d)

# All neighbors between 0 and max_dist
neighbours = elsa::dneigh(sp_df, d1 = 0,d2 = max_dist,longlat = FALSE)

# Calculate local moran´s I
l_moran = elsa::lisa(sp_df, d1 = neighbours, statistic = "I", 
                     longlat = FALSE, zcol = "bcNitrate")
hist(l_moran$Ii, breaks = 50)





