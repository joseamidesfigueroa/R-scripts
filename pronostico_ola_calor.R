library(raster)
library(rgdal)
library(RColorBrewer)

#Carga el mapa
mapa <- readOGR(dsn=path.expand("C:/R/shape"),layer="ESA_CA_wgs84")
#Establece el directorio de los percentiles
setwd("C:/R/percentiles_tmax/")

#Funcion condicional
Con=function(condition, trueValue, falseValue){
  return(condition * trueValue + (!condition)*falseValue)}

#Carga el percentil 90
p90<-raster("per90.tif")
projection(p90) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(p90)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)

#*******************************************************************************************************


#------------------------------------------------------------------------------------------------------>
#Declara los titulos y fechas

#Define las horas
fecha_actual<-as.Date(Sys.time())
fecha_3dias_mas <-as.Date(fecha_actual)+3

titulo="Pronóstico de ocurrencia de ola de calor"
texto=paste("Desde el ",fecha_actual," hasta el ",fecha_3dias_mas, sep = "")

#------------------------------------------------------------------------------------------------------>

#Establece el directorio de trabajo para pronostico
setwd("C:/R/percentiles_tmax/wrf")
#---------------------------------------------------------------------------------------->
t1<-raster("temperatura_18.tif")
t2<-raster("temperatura_19.tif")
t3<-raster("temperatura_20.tif")
t4<-raster("temperatura_21.tif")
t5<-raster("temperatura_22.tif")
#Hace una pila con los rasters a usar.
pila1<-stack(t1,t2,t3,t4,t5)
#---------------------------------------------------------------------------------------->

#---------------------------------------------------------------------------------------->
t1<-raster("temperatura_42.tif")
t2<-raster("temperatura_43.tif")
t3<-raster("temperatura_44.tif")
t4<-raster("temperatura_45.tif")
t5<-raster("temperatura_46.tif")
#Hace una pila con los rasters a usar.
pila2<-stack(t1,t2,t3,t4,t5)
#---------------------------------------------------------------------------------------->

#---------------------------------------------------------------------------------------->
t1<-raster("temperatura_66.tif")
t2<-raster("temperatura_67.tif")
t3<-raster("temperatura_68.tif")
t4<-raster("temperatura_69.tif")
t5<-raster("temperatura_70.tif")
#Hace una pila con los rasters a usar.
pila3<-stack(t1,t2,t3,t4,t5)
#---------------------------------------------------------------------------------------->

#Saca el maximo de los rasters en un solo 
tmaxd1<-calc(pila1,fun = max)
tmaxd2<-calc(pila2,fun = max)
tmaxd3<-calc(pila3,fun = max)


#tmaxd1<-raster("tmaxabs_d1.tif")
projection(tmaxd1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(tmaxd1)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

projection(tmaxd2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(tmaxd2)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

projection(tmaxd3) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(tmaxd3)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

#Recorta el raster de pronóstico
tmaxd1<-crop(tmaxd1,p90)
tmaxd2<-crop(tmaxd2,p90)
tmaxd3<-crop(tmaxd3,p90)

#Realiza un resample
tmaxd1<-resample(tmaxd1,p90)
tmaxd2<-resample(tmaxd2,p90)
tmaxd3<-resample(tmaxd3,p90)

#------------------------------------------------------------------------------->
#Genera 3 rasters a cero
a<-tmaxd1*0
b<-a
c<-b
ola1<-a

#Realiza la prueba si la temperatura maxima es mayor que el percentil 90
a<-Con(tmaxd1<=p90,a+0,a+1)
b<-Con(tmaxd2<=p90,b+0,b+1)
c<-Con(tmaxd3<=p90,c+0,c+1)

#Elabora un mapa de coincidencias
ola1<-Con((a==b & a==c & b==a & b==c & c==a & c==b),ola1+0,ola1+1)
#------------------------------------------------------------------------------->

#Define paleta de colores
paleta.colores<-colorRampPalette(c("white","red"))


#------------------------------------------------------------------------------->
setwd("C:/R/percentiles_tmax/")
jpeg("pronostico_olacalor.jpg", pointsize=8, width = 1300, height = 800, res=150)
plot(ola1,col=paleta.colores(2), main=titulo, ylab="Latitud", xlab="Longitud", legend=FALSE)
legend("topright",legend=c("Ola de calor"), fill = "red")
plot(mapa,add=TRUE)
mtext(texto,side = 3)
dev.off()