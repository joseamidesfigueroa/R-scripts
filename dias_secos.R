library(raster)
library(rgdal)
library(RColorBrewer)

#Define el directorio de trabajo
setwd("C:/R/temporal")

#Carga el mapa
mapa <- readOGR(dsn=path.expand("C:/R/shape"),layer="ESA_CA_wgs84")

#Carga el percentil 90
p90<-raster("per90.tif")
projection(p90) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(p90)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)


#------------------------------------------------------------------------------------------------------>
#Declara los titulos y fechas

#Define las horas
fecha_actual<-as.Date(Sys.time())
fecha_3dias_mas <-as.Date(fecha_actual)+15

titulo="Pronóstico de dias secos - Modelo GFS - 00 UTC"
texto=paste("Desde el ",fecha_actual," hasta el ",fecha_3dias_mas, sep = "")

#------------------------------------------------------------------------------------------------------>

#--------------------------------------> dia 1
#Carga lluvia
ti<-raster("lluvia_gfs_5.tif")
tf<-raster("lluvia_gfs_13.tif")

dia1<-(tf-ti)
#--------------------------------------->

#--------------------------------------> dia 2
#Carga lluvia
ti<-raster("lluvia_gfs_13.tif")
tf<-raster("lluvia_gfs_21.tif")

dia2<-(tf-ti)
#--------------------------------------->

#--------------------------------------> dia 3
#Carga lluvia
ti<-raster("lluvia_gfs_21.tif")
tf<-raster("lluvia_gfs_29.tif")

dia3<-(tf-ti)
#--------------------------------------->

#--------------------------------------> dia 4
#Carga lluvia
ti<-raster("lluvia_gfs_29.tif")
tf<-raster("lluvia_gfs_37.tif")

dia4<-(tf-ti)
#--------------------------------------->

#--------------------------------------> dia 5
#Carga lluvia
ti<-raster("lluvia_gfs_37.tif")
tf<-raster("lluvia_gfs_45.tif")

dia5<-(tf-ti)
#--------------------------------------->

#--------------------------------------> dia 6
#Carga lluvia
ti<-raster("lluvia_gfs_45.tif")
tf<-raster("lluvia_gfs_53.tif")

dia6<-(tf-ti)
#--------------------------------------->

#--------------------------------------> dia 7
#Carga lluvia
ti<-raster("lluvia_gfs_53.tif")
tf<-raster("lluvia_gfs_61.tif")

dia7<-(tf-ti)
#--------------------------------------->

#--------------------------------------> dia 8
#Carga lluvia
ti<-raster("lluvia_gfs_61.tif")
tf<-raster("lluvia_gfs_69.tif")

dia8<-(tf-ti)
#--------------------------------------->

#--------------------------------------> dia 9
#Carga lluvia
ti<-raster("lluvia_gfs_69.tif")
tf<-raster("lluvia_gfs_77.tif")

dia9<-(tf-ti)
#--------------------------------------->

#--------------------------------------> dia 10
#Carga lluvia
ti<-raster("lluvia_gfs_77.tif")
tf<-raster("lluvia_gfs_85.tif")

dia10<-(tf-ti)
#--------------------------------------->

#--------------------------------------> dia 11
#Carga lluvia
ti<-raster("lluvia_gfs_85.tif")
tf<-raster("lluvia_gfs_93.tif")

dia11<-(tf-ti)
#--------------------------------------->

#--------------------------------------> dia 12
#Carga lluvia
ti<-raster("lluvia_gfs_93.tif")
tf<-raster("lluvia_gfs_101.tif")

dia12<-(tf-ti)
#--------------------------------------->

#--------------------------------------> dia 13
#Carga lluvia
ti<-raster("lluvia_gfs_101.tif")
tf<-raster("lluvia_gfs_109.tif")

dia13<-(tf-ti)
#--------------------------------------->

#--------------------------------------> dia 14
#Carga lluvia
ti<-raster("lluvia_gfs_109.tif")
tf<-raster("lluvia_gfs_117.tif")

dia14<-(tf-ti)
#--------------------------------------->

#--------------------------------------> dia 15
#Carga lluvia
ti<-raster("lluvia_gfs_117.tif")
tf<-raster("lluvia_gfs_125.tif")

dia15<-(tf-ti)
#--------------------------------------->


lluvias<-stack(dia1,dia2,dia3,dia4,dia5,dia6,dia7,dia8,dia9,dia10,dia11,dia12,dia13,dia14,dia15)
projection(lluvias) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(lluvias)<-c(240.75-360,318.25-360,-1.25,37.25)

lluvias<-resample(lluvias,p90)

#--------------------------------------->
#Funcion condicional
Con=function(condition, trueValue, falseValue){
  return(condition * trueValue + (!condition)*falseValue)}
#--------------------------------------->

pentada1<-(lluvias$layer.1+lluvias$layer.2+lluvias$layer.3+lluvias$layer.4+lluvias$layer.5)
pentada2<-(lluvias$layer.6+lluvias$layer.7+lluvias$layer.8+lluvias$layer.9+lluvias$layer.10)
pentada3<-(lluvias$layer.11+lluvias$layer.12+lluvias$layer.13+lluvias$layer.14+lluvias$layer.15)

dummy<-(lluvias$layer.1)*0
sequia1<-(lluvias$layer.1)*0
sequia2<-(lluvias$layer.1)*0
sequia3<-(lluvias$layer.1)*0

sequia1<-Con(pentada1<=1,sequia1+1,sequia1+0)
sequia2<-Con((pentada1+pentada2)<=1,sequia2+1,sequia2+0)
sequia3<-Con((pentada1+pentada2+pentada3)<=1,sequia3+1,sequia3+0)

sequias<-sequia1+sequia2+sequia3

plot(sequias)

paleta.colores<-colorRampPalette(c("white","yellow","orange","red"))

if (all.equal(sequias,dummy)){
  
  plot(sequias, xlim=c(-90.2,-87.49),ylim=c(12.9,14.7), col=paleta.colores(4), legend=FALSE, breaks=c(0,1,2,3), 
       main=titulo)
  legend("topright", legend = c("Sin dias secos en 5 dias","5 dias secos","Sequia debil. Entre 6 - 10 dias secos","Sequia moderada. Entre 11 - 15 dias secos"), 
         fill = c("white","yellow","orange","red"), cex=0.7)
  plot(mapa, add=TRUE)
  mtext(texto,side = 3)
} else {
  plot(sequias, xlim=c(-90.2,-87.49),ylim=c(12.9,14.7), col=paleta.colores(0), legend=FALSE, 
       main=titulo)
  legend("topright", legend = c("Sin dias secos en 5 dias","5 dias secos","Sequia debil. Entre 6 - 10 dias secos","Sequia moderada. Entre 11 - 15 dias secos"), 
         fill = c("white","yellow","orange","red"), cex=0.7)
  plot(mapa, add=TRUE)
  mtext(texto,side = 3)
}
  





