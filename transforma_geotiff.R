#Software para corregir

library(raster)


dir<-"C:/Users/Amides/Documents/Trabajo/Temporal/geotiff"




capa1<-raster("C:/R/temporal/27km/vv_10m_2.tif")
capa2<-raster("C:/R/temporal/27km/wd_10m_2.tif")
extent(capa1)<-c(264.9289200000000051-360,283.9326563748624608-360,5.8151366494649892,21.6104500000000002)
projection(capa1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

extent(capa2)<-c(264.9289200000000051-360,283.9326563748624608-360,5.8151366494649892,21.6104500000000002)
projection(capa2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

wrfclima<-raster("C:/R/temporal/27km/acumulado_lluvia_total_dia_29.tif")
extent(wrfclima)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
projection(wrfclima) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


ras<-raster("C:/R/temporal/27km/CHIRP.2021.01.tif")
ras<-crop(ras,capa1)

capa1<-resample(capa1,ras)
capa2<-resample(capa2,ras)
capa3<-resample(wrfclima,ras)

rf<-writeRaster(capa1, filename="C:/R/temporal/27km/capa1.tif", overwrite=TRUE)
rf<-writeRaster(capa2, filename="C:/R/temporal/27km/capa2.tif", overwrite=TRUE)
rf<-writeRaster(capa3, filename="C:/R/temporal/27km/capa3.tif", overwrite=TRUE)

plot(capa1)
plot(capa2)
plot(ras)