library(ncdf4)
library(raster)
library(sp)
library(rgdal)
library(viridis)
library(fields)


mapa <- readOGR(dsn=path.expand("C:/R/shape"),layer="TM_WORLD_BORDERS-0.3")
setwd("C:/R/")
nc<-nc_open("wrfout.nc")

lluvia<-ncvar_get(nc, nc$var$RAINC)
U<-ncvar_get(nc, nc$var$T)

U[2,2,1]
xlon<-ncvar_get(nc,nc$var$XLONG)
xlat<-ncvar_get(nc,nc$var$XLAT)

nc_close(nc)

lon<-xlon[,1]
lat<-xlat[1,]
U1<-U[,,]

min(lon)
max(lon)
min(lat)
max(lat)


var<-as.matrix(U1)
foo<-raster(var, xmn=-119.6855, xmx=-85.1305, ymn=12.32458, ymx=33.67536)

extent(foo)<-c(-119.6855,-85.1305,12.32458,33.67536)
projection(foo) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")



image(foo,
           main="Mapa de prueba para NetCDF",
           xlab="Longitud",
           ylab="Latitud",
           legend.lab="m/s",
           legen.line=2.5,
           col=rev(magma(200)))


plot(mapa, add=T,border="black")
mtext("El Salvador")