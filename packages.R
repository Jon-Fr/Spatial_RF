# Tools to more conveniently perform tasks associated with add-on packages.
(pacman)

# Classes and methods for spatial data; the classes document where the spatial 
# location information resides, for 2D or 3D data.
(sp)

# A package that provides simple features access for R.
(sf)

# Methods for spatial data analysis with vector (points, lines, polygons) and 
# raster (grid) data.
(terra)

# Implements spatial error estimation and permutation-based variable importance 
# measures for predictive models using spatial cross-validation and spatial 
# block bootstrap.
(sperrorest)

# Variogram modelling; simple, ordinary and universal point or block 
# (co)kriging ...
(gstat)

# Enables the automated creation of variogram models.
(automap)

# Fit and compare Gaussian linear and nonlinear mixed-effects models.
(nlme)

# Enhances R’s functional programming (FP) toolkit by providing a complete and 
# consistent set of tools for working with functions and vectors.
(purrr)

# Support for Parallel computation in R
(parallel)

# Foreach parallel adaptor for the 'parallel' package
(doParallel)

# Foreach is an idiom that allows for iterating over elements in a collection, 
# without the use of an explicit loop counter. 
(foreach)

# Spatio-temporal geostatistical mapping of data. 
# Random Forest Spatial Interpolation (RFSI)
# Sometimes it is necessary to install this package using devtools
library(devtools)
install_github("https://github.com/AleksandarSekulic/Rmeteo")

# Fits non-linear regression models on dependent data with Generalized Least 
# Square (GLS) based Random Forest (RF-GLS).
(RandomForestsGLS)

# Fast spatial regression using Moran eigenvectors
(spmoran)