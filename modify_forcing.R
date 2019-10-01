#' @title Make forcing file
#' @description  Uses NCO to slice netcdf forcing file
#' @details INPUT: 1) Deep Horizon oil forcing file from Ainsworth
#' @details OUTPUT: 1) sliced forcing file
#' @details NCO manual is in http://nco.sourceforge.net/nco.html#rx
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


forcing.file="OilSpatialForcingGOM.nc"


setwd("./GOMAtlantis_oil_1000Factor_363Thresh")

system(paste("ncks -v '^SDF+..|^DWF+..|^ODF+..' AtlantisSpatialForcing_1000Factor_363Thresh.nc ",forcing.file,sep=""))



