#Carga librerias necesarias
library(raster)
library(rgdal)
library(png)

#Define el directorio de trabajo
setwd("C:/R/temporal/")

#Define los textos
ciclo<-"Ciclo 00Z. \n"
texto1<-"Acumulado de 24horas entre\n 7:00 AM del dia 1 a 7:00AM del dia 2\n\n Grafico realizado el:\n"
texto2<-"Acumulado de 24horas entre\n 7:00 AM del dia 2 a 7:00AM del dia 3\n\n Grafico realizado el:\n"
texto3<-"Acumulado de 24horas entre\n 7:00 AM del dia 3 a 7:00AM del dia 3\n\n Grafico realizado el:\n"
texto4<-"Acumulado de 72horas entre\n 7:00 AM del dia 1 a 7:00AM del dia 3\n\n Grafico realizado el:\n"
time<-Sys.time()

#Carga el mapa
mapa <- readOGR(dsn=path.expand("C:/R/shape"),layer="ESA_CA_wgs84")


#*******************************************************************************************************
#Funcion para adicionar imagen
#---------------------------------------------------------------------------------------
addImg <- function(
  obj, # an image file imported as an array (e.g. png::readPNG, jpeg::readJPEG)
  x = NULL, # mid x coordinate for image
  y = NULL, # mid y coordinate for image
  width = NULL, # width of image (in x coordinate units)
  interpolate = TRUE # (passed to graphics::rasterImage) A logical vector (or scalar) indicating whether to apply linear interpolation to the image when drawing. 
){
  if(is.null(x) | is.null(y) | is.null(width)){stop("Must provide args 'x', 'y', and 'width'")}
  USR <- par()$usr # A vector of the form c(x1, x2, y1, y2) giving the extremes of the user coordinates of the plotting region
  PIN <- par()$pin # The current plot dimensions, (width, height), in inches
  DIM <- dim(obj) # number of x-y pixels for the image
  ARp <- DIM[1]/DIM[2] # pixel aspect ratio (y/x)
  WIDi <- width/(USR[2]-USR[1])*PIN[1] # convert width units to inches
  HEIi <- WIDi * ARp # height in inches
  HEIu <- HEIi/PIN[2]*(USR[4]-USR[3]) # height in units
  rasterImage(image = obj, 
              xleft = x-(width/2), xright = x+(width/2),
              ybottom = y-(HEIu/2), ytop = y+(HEIu/2), 
              interpolate = interpolate)
}
#---------------------------------------------------------------------------------------
#*******************************************************************************************************



#Carga el raster solo para efectos de recorte y resample, razones puramente esteticas
p90<-raster("p90.tif")

#Define la proyecion y extension de p90, no es usado mas que para recortar y hacer un resample. Solo con fines esteticos
projection(p90) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(p90)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)

#Carga los rasters acumulados dia 1
a1<-raster("24h_A.tif")
projection(a1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(a1)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

b1<-raster("24h_B.tif")
projection(b1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(b1)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

c1<-raster("24h_C.tif")
projection(c1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(c1)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

d1<-raster("24h_D.tif")
projection(d1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(d1)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

e1<-raster("24h_E.tif")
projection(e1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(e1)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

f1<-raster("24h_F.tif")
projection(f1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(f1)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

g1<-raster("24h_G.tif")
projection(g1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(g1)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

#Carga los rasters acumulados dia 2
a2<-raster("48h_A.tif")
projection(a2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(a2)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

b2<-raster("48h_B.tif")
projection(b2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(b2)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

c2<-raster("48h_C.tif")
projection(c2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(c2)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

d2<-raster("48h_D.tif")
projection(d2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(d2)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

e2<-raster("48h_E.tif")
projection(e2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(e2)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

f2<-raster("48h_F.tif")
projection(f2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(f2)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

g2<-raster("48h_G.tif")
projection(g2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(g2)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

#Carga los rasters acumulados dia 3
a3<-raster("72h_A.tif")
projection(a3) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(a3)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

b3<-raster("72h_B.tif")
projection(b3) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(b3)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

c3<-raster("72h_C.tif")
projection(c3) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(c3)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

d3<-raster("72h_D.tif")
projection(d3) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(d3)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

e3<-raster("72h_E.tif")
projection(e3) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(e3)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

f3<-raster("72h_F.tif")
projection(f3) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(f3)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

g3<-raster("72h_G.tif")
projection(g3) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(g3)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

#Recorta y resamplea
#Realiza los cortes y resamples
#------------------------------------------------------------------------------------------->dia1
a1<-crop(a1,p90)
a1<-resample(a1,p90)
projection(a1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(a1)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)

b1<-crop(b1,p90)
b1<-resample(b1,p90)
projection(b1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(b1)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)

c1<-crop(c1,p90)
c1<-resample(c1,p90)
projection(c1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(c1)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)

d1<-crop(d1,p90)
d1<-resample(d1,p90)
projection(d1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(d1)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)

e1<-crop(e1,p90)
e1<-resample(e1,p90)
projection(e1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(e1)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)

f1<-crop(f1,p90)
f1<-resample(f1,p90)
projection(f1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(f1)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)

g1<-crop(g1,p90)
g1<-resample(g1,p90)
projection(g1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(g1)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)
#----------------------------------------------------------------------------------------------->

#------------------------------------------------------------------------------------------->dia2
a2<-crop(a2,p90)
a2<-resample(a2,p90)
projection(a1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(a1)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)

b2<-crop(b2,p90)
b2<-resample(b2,p90)
projection(b2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(b2)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)

c2<-crop(c2,p90)
c2<-resample(c2,p90)
projection(c2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(c2)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)

d2<-crop(d2,p90)
d2<-resample(d2,p90)
projection(d2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(d2)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)

e2<-crop(e2,p90)
e2<-resample(e2,p90)
projection(e2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(e2)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)

f2<-crop(f2,p90)
f2<-resample(f2,p90)
projection(f2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(f2)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)

g2<-crop(g2,p90)
g2<-resample(g2,p90)
projection(g2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(g2)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)
#----------------------------------------------------------------------------------------------->

#------------------------------------------------------------------------------------------->dia2
a3<-crop(a3,p90)
a3<-resample(a3,p90)
projection(a3) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(a3)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)

b3<-crop(b3,p90)
b3<-resample(b3,p90)
projection(b3) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(b3)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)

c3<-crop(c3,p90)
c3<-resample(c3,p90)
projection(c3) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(c3)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)

d3<-crop(d3,p90)
d3<-resample(d3,p90)
projection(d3) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(d3)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)

e3<-crop(e3,p90)
e3<-resample(e3,p90)
projection(e3) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(e3)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)

f3<-crop(f3,p90)
f3<-resample(f3,p90)
projection(f3) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(f3)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)

g3<-crop(g3,p90)
g3<-resample(g3,p90)
projection(g3) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(g3)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)
#----------------------------------------------------------------------------------------------->

#Calcula los totales por miembro
a<-a1+a2+a3
b<-b1+b2+b3
c<-c1+c2+c3
d<-d1+d2+d3
e<-e1+e2+e3
f<-f1+f2+f3
g<-g1+g2+g3


#Define la extensión y la proyección
projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(a)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

#Define la extensión y la proyección
projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(b)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

#Define la extensión y la proyección
projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(c)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

#Define la extensión y la proyección
projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(d)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

#Define la extensión y la proyección
projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(e)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

#Define la extensión y la proyección
projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(f)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

#Define la extensión y la proyección
projection(g) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(g)<-c(-90.400895455,-87.321722453,12.627577633,14.964450000)

# #Realiza los cortes y resamples
# a<-crop(a,p90)
# a<-resample(a,p90)
 projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
 extent(a)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)
# 
# b<-crop(b,p90)
# b<-resample(b,p90)
 projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
 extent(b)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)
# 
# c<-crop(c,p90)
# c<-resample(c,p90)
 projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
 extent(c)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)
# 
# d<-crop(d,p90)
# d<-resample(d,p90)
 projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
 extent(d)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)
# 
# e<-crop(e,p90)
# e<-resample(e,p90)
 projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
 extent(e)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)
# 
# f<-crop(f,p90)
# f<-resample(f,p90)
 projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
 extent(f)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)
# 
# g<-crop(g,p90)
# g<-resample(g,p90)
 projection(g) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
 extent(g)<-c(-90.137197327,-87.674860016,13.148929568,14.449521736)


#*******************************************************************************************************
#Define las paletas de colores
#----------------------------------------------------------------------------------------->
paleta.colores<-colorRampPalette(c("#ffffff00","cornflowerblue","#68ff06","#effe08","#ff7000","#ff1f00","#710f02","#710f02","magenta"))


#Define las paletas de colores
#----------------------------------------------------------------------------------------->
#intervalos<-c(0,25,50,75,100,125,150,175,200,225,250,275,300,325,350,375,400,425,450,500)
intervalos<-seq(from=0, to=800, by=25)
#intervalos_80<-c(25,50,75,100,125,150,175,200,225,250,275,300,325,350,375,400,425,450,475,500,525,550,575,600)
#intervalos_poca<-seq(from=0, to=150, by=10)
intervalos_poca<-seq(from=0, to=650, by=25)
#intervalos_80<-seq(from=0, to=400, by=25)
intervalos_80<-seq(from=0, to=650, by=25)

#Prepara los graficos
#-----------------------------------------------------------------------------------------> dia 1
jpeg("acumulados_dia1_wrf.jpg", pointsize=12, res = 150, width = 2000, height = 2000)
par(mfrow=c(4,2),mai=c(1,1,1,1))

#Genera un grafico vacio
par(mar = rep(2, 4))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
logo<-"logo.png"
pic<-readPNG(logo)
addImg(pic,x=0.5,y=0.75,width = 0.75)
text(0.5,0.25,paste(ciclo,texto1,time),col = "black", cex=1.3)

par(mar = rep(2, 4))
plot(a1,main="Miembro A", width=5, heigth=5,col=paleta.colores(15),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos_poca,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(b1,main="Miembro B", width=5, heigth=5,col=paleta.colores(15),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos_poca,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(c1,main="Miembro C", width=5, heigth=5,col=paleta.colores(15),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos_poca,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(d1,main="Miembro D", width=5, heigth=5,col=paleta.colores(15),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos_poca,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(e1,main="Miembro E", width=5, heigth=5,col=paleta.colores(15),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos_poca,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(f1,main="Miembro F", width=5, heigth=5,col=paleta.colores(15),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos_poca,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(g1,main="Miembro G", width=5, heigth=5,col=paleta.colores(15),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos_poca,legend=TRUE)
plot(mapa,add = TRUE)

dev.off()
#----------------------------------------------------------------------------------------->

#Prepara los graficos
#-----------------------------------------------------------------------------------------> dia 2
jpeg("acumulados_dia2_wrf.jpg", pointsize=12, res = 150, width = 2000, height = 2000)
par(mfrow=c(4,2),mai=c(1,1,1,1))

#Genera un grafico vacio
par(mar = rep(2, 4))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
logo<-"logo.png"
pic<-readPNG(logo)
addImg(pic,x=0.5,y=0.75,width = 0.75)
text(0.5,0.25,paste(ciclo,texto2,time),col = "black", cex=1.3)

par(mar = rep(2, 4))
plot(a2,main="Miembro A", width=5, heigth=5,col=paleta.colores(15),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos_80,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(b2,main="Miembro B", width=5, heigth=5,col=paleta.colores(15),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos_80,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(c2,main="Miembro C", width=5, heigth=5,col=paleta.colores(15),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos_80,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(d2,main="Miembro D", width=5, heigth=5,col=paleta.colores(15),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos_80,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(e2,main="Miembro E", width=5, heigth=5,col=paleta.colores(15),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos_80,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(f2,main="Miembro F", width=5, heigth=5,col=paleta.colores(15),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos_80,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(g2,main="Miembro G", width=5, heigth=5,col=paleta.colores(15),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos_80,legend=TRUE)
plot(mapa,add = TRUE)

dev.off()
#----------------------------------------------------------------------------------------->

#Prepara los graficos
#-----------------------------------------------------------------------------------------> dia 3
jpeg("acumulados_dia3_wrf.jpg", pointsize=12, res = 150, width = 2000, height = 2000)
par(mfrow=c(4,2),mai=c(1,1,1,1))

#Genera un grafico vacio
par(mar = rep(2, 4))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
logo<-"logo.png"
pic<-readPNG(logo)
addImg(pic,x=0.5,y=0.75,width = 0.75)
text(0.5,0.25,paste(ciclo,texto3,time),col = "black", cex=1.3)

par(mar = rep(2, 4))
plot(a3,main="Miembro A", width=5, heigth=5,col=paleta.colores(25),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos_80,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(b3,main="Miembro B", width=5, heigth=5,col=paleta.colores(25),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos_80,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(c3,main="Miembro C", width=5, heigth=5,col=paleta.colores(25),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos_80,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(d3,main="Miembro D", width=5, heigth=5,col=paleta.colores(25),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos_80,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(e3,main="Miembro E", width=5, heigth=5,col=paleta.colores(25),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos_80,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(f3,main="Miembro F", width=5, heigth=5,col=paleta.colores(25),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos_80,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(g3,main="Miembro G", width=5, heigth=5,col=paleta.colores(25),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos_80,legend=TRUE)
plot(mapa,add = TRUE)

dev.off()
#----------------------------------------------------------------------------------------->


#-----------------------------------------------------------------------------------------> 3 dias
jpeg("acumulados_3dias_wrf.jpg", pointsize=12, res = 150, width = 2000, height = 2000)
par(mfrow=c(4,2),mai=c(1,1,1,1))

#Genera un grafico vacio
par(mar = rep(2, 4))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
logo<-"logo.png"
pic<-readPNG(logo)
addImg(pic,x=0.5,y=0.75,width = 0.75)
text(0.5,0.25,paste(ciclo,texto4,time),col = "black", cex=1.3)

par(mar = rep(2, 4))
plot(a,main="Miembro A", width=5, heigth=5,col=paleta.colores(35),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(b,main="Miembro B", width=5, heigth=5,col=paleta.colores(35),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(c,main="Miembro C", width=5, heigth=5,col=paleta.colores(35),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(d,main="Miembro D", width=5, heigth=5,col=paleta.colores(35),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(e,main="Miembro E", width=5, heigth=5,col=paleta.colores(35),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(f,main="Miembro F", width=5, heigth=5,col=paleta.colores(35),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
plot(mapa,add = TRUE)

par(mar = rep(2, 4))
plot(g,main="Miembro G", width=5, heigth=5,col=paleta.colores(35),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
plot(mapa,add = TRUE)

dev.off()
#----------------------------------------------------------------------------------------->
