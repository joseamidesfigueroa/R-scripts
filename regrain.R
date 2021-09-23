library(devtools)
install_github("Diegoa16/RegRAIN/RegRAIN")

library(RegRAIN)

library(raster)
dem<-raster("C:/Dem_SAL_10m/dem_esa.tif")

plot(dem)