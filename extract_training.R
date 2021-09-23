setwd("C:/R/capacitacion/")
library(raster)
library(rgdal)
library(rgeos)
library(crs)
library(RColorBrewer)
mapa <- readOGR(dsn=path.expand("C:/R/capacitacion"),layer="gadm36_MEX_1")


lluvia1<-raster("lluvia_24h.tif")
lluvia2<-raster("lluvia_48h.tif")
lluvia3<-raster("lluvia_72h.tif")

lluvia<-lluvia1+lluvia2+lluvia3

prom<-mean(lluvia)


extent(lluvia)<-c(241.4275289256198676-360,277.0654970062248026-360,11.9350714203511785,33.8331000000000017)
projection(lluvia)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

plot(lluvia)
plot(mapa,add=TRUE)

coor<-cbind(-85.5,21)
dato<-extract(lluvia,coor,method='bilinear')
dato
