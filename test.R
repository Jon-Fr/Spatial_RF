library("pacman")
p_load("terra")
p_load("ENMTools")

NuM_L_b = terra::rast("Results/orgNitrate/NuM_L_base_predciton_raster.tif")
NuM_L_o = terra::rast("Results/orgNitrate/NuM_L_base_predciton_raster.tif")

test = raster.cor(NuM_L_b, NuM_L_o, method = "pearson")

WuS_SuB_b = terra::rast("Results/orgNitrate/WuS_SuB_base_predciton_raster.tif")
WuS_SuB_o = terra::rast("Results/orgNitrate/WuS_SuB_base_predciton_raster.tif")