# Load data and formula
load("Data/NuM_L.rda")
load("Data/WuS_SuB.rda")

# N = NuM_L
# W = WuS_SuB

## Conduct multiple linear regression (MLR)
# NuM_L
MLR_model_N = lm(fo_lm_NuM_L, data = NuM_L)
summary(MLR_model_N)

# WuS_SuB
MLR_model_W = lm(fo_lm_WuS_SuB, data = WuS_SuB)
summary(MLR_model_W)

## Statistical parameters
# NuM_L
mean(NuM_L$subMittelwert)
median(NuM_L$subMittelwert)

sd(NuM_L$subMittelwert)
IQR(NuM_L$subMittelwert)

# WuS_SuB
mean(WuS_SuB$subMittelwert)
median(WuS_SuB$subMittelwert)

sd(WuS_SuB$subMittelwert)
IQR(WuS_SuB$subMittelwert)
