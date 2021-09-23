library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)
library(parallel)
library(dplyr)
library(spData)
library(spDataLarge)
library(maps)
library(maptools)
library(gdalUtils)

setwd("C:/R")
map<- readOGR(dsn=path.expand("C:/R/shape"),layer="TM_WORLD_BORDERS-0.3")
mapa <- readOGR(dsn=path.expand("C:/R/shape"),layer="TM_WORLD_BORDERS-0.3_geos")


proj_geos <- CRS("+proj=geos +h=35774290 +a=6378137 +b=6378137 +lon_0=-75 +units=m +no_defs")
proj_normal <-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
mollCRS <- CRS('+proj=moll')
behrmannCRS <- CRS('+proj=cea +lat_ts=30')

mapa_pojected<-spTransform(mapa,proj_geos)


banda14<-nc_open("OR_ABI-L2-CMIPF-M6C14_G16_s20192200220543_e20192200230251_c20192200230337.nc")
reflectancia_banda14<-t(raster(ncvar_get(banda14,'CMI')))
projection(reflectancia_banda14)<-CRS("+proj=geos +h=35774290 +a=6378137 +b=6378137 +lon_0=-75 +units=m +no_defs")

gdal_translate(
  src_dataset = reflectancia_banda14,
  dst_dataset = 'cmi.tif', 
  of = 'GTiff', 
  a_srs = '+proj=geos +a=6378137. +b=6356752.31414 +lon_0=-89.5 +f=.00335281068119356027 +h=35786023. +sweep=x', 
  a_ullr = c(-5434894.885056,5434894.885056,5434894.885056,-5434894.885056), 
  a_nodata = -1
)

plot(reflectancia_banda14)
plot(mapa_pojected)


cmi<-projectRaster(reflectancia_banda14,crs=proj_normal, method = "bilinear")
extent(cmi)<-c(-156.299499511719,6.29949998855591,-81.3282012939453,81.3282012939453)
#(xmn=-110, xmx=-90, ymn=40, ymx=60, ncols=40, nrows=40)

cmi_ext<- projectExtent(cmi, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")




imagen_plot<-function(i){
plot(cmi, col=gray.colors(255, start = 0,end = 1))
  plot(mapa,alpha=TRUE,col="#FFFFFFFF",border="green")
  plot(map,alpha=TRUE,col="#FFFFFFFF",border="green",add=TRUE)
}



lapply(1,imagen_plot)

numCores<-detectCores()
cl<-makeCluster(numCores)

mclapply(1,imagen_plot)

banda02