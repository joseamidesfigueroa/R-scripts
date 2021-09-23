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
fecha_3dias_mas <-as.Date(fecha_actual)+5

titulo="Pronóstico de dias secos - Modelo GFS - 00 UTC"
titulo2="Pronóstico de dias secos consecutivos- Modelo GFS - 00 UTC"
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

#Prepara una pila con todos los dias de lluvia segun el modelo
lluvias<-stack(dia1,dia2,dia3,dia4,dia5,dia6,dia7,dia8,dia9,dia10,dia11,dia12,dia13,dia14,dia15)
projection(lluvias) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(lluvias)<-c(240.75-360,318.25-360,-1.25,37.25)

#Realiza un resample para suavizar
lluvias<-resample(lluvias,p90)

#--------------------------------------->
#Funcion condicional
Con=function(condition, trueValue, falseValue){
  return(condition * trueValue + (!condition)*falseValue)}
#--------------------------------------->

#Genera los rasters vacios a usar en los dias secos no consecutivos
ds1<-(lluvias$layer.1)*0
ds2<-(lluvias$layer.1)*0
ds3<-(lluvias$layer.1)*0
ds4<-(lluvias$layer.1)*0
ds5<-(lluvias$layer.1)*0
dsnc<-(lluvias$layer.1)*0

#Determina si hay dias secos
ds1<-Con(lluvias$layer.1<=1,ds1+1,ds1+0)
ds2<-Con(lluvias$layer.2<=1,ds2+1,ds2+0)
ds3<-Con(lluvias$layer.3<=1,ds3+1,ds3+0)
ds4<-Con(lluvias$layer.4<=1,ds4+1,ds4+0)
ds5<-Con(lluvias$layer.5<=1,ds5+1,ds5+0)

#Genera el mapa para plotear los dias secos del periodo
dsnc<-(ds1+ds2+ds3+ds4+ds5)


#Genera la paleta de colores
paleta.colores<-colorRampPalette(c("white","green","yellow","orange","red","brown"))
paleta.colores2<-colorRampPalette(c("white","green","yellow","orange","red"))

#Genera el gráfico
jpeg("dsnc.jpg", pointsize=8, width = 900, height = 788, res=150)
plot(dsnc, xlim=c(-90.2,-87.49),ylim=c(12.9,14.7), col=paleta.colores(6), legend=FALSE, breaks=c(0,1,2,3,4,5), 
     main=titulo)
legend("topright", legend = c("Sin dias secos","1 dia seco","2 dias secos","3 dias secos", "4 dias secos","5 dias secos"), 
       fill = c("white","green","yellow","orange","red","brown"), cex=0.7)
plot(mapa, add=TRUE)
mtext(texto,side = 3)
dev.off()

#Genera los rasters vacios a usar
dsc2<-(lluvias$layer.1)*0
dsc3<-(lluvias$layer.1)*0
dsc4<-(lluvias$layer.1)*0
dsc5<-(lluvias$layer.1)*0
dsc<-(lluvias$layer.1)*0

#Determina si los dias secos son consecutivos
dsc2<-Con(((ds1+ds2>=2)|(ds2+ds3>=2)|(ds3+ds4>=2)|(ds4+ds5>=2)),dsc2+1,dsc2+0)
dsc3<-Con(((dsc2+ds3>=2)|(dsc2+ds4>=2)|(dsc2+ds5>+2)),dsc3+1,dsc3+0)
dsc4<-Con(((dsc3+ds4>=2)|(dsc3+ds5>=2)),dsc4+1,dsc4+0)
dsc5<-Con((dsc4+ds5>2),dsc5+1,dsc5+0)

dsc<-dsc2+dsc3+dsc4+dsc5

#Genera el gráfico

jpeg("dsc.jpg", pointsize=8, width = 900, height = 788, res=150)
plot(dsc, xlim=c(-90.2,-87.49),ylim=c(12.9,14.7), col=paleta.colores2(5), legend=FALSE, breaks=c(0,1,2,3,4), 
     main=titulo2)
legend("topright", legend = c("Sin dias secos consecutivos","2 dias secos consecutivos","3 dias secos consecutivos","4 dias secos consecutivos", "5 dias secos consecutivos"), 
       fill = c("white","green","yellow","orange","red","brown"), cex=0.7)
plot(mapa, add=TRUE)
mtext(texto,side = 3)
dev.off()





