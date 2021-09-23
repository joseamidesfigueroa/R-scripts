#GENERA PRONOSTICO DE LLUVIA ACUMULA MENSUAL AJUSTADA CON ERROR PROMEDIO MENSUAL DEL MODELO VS CHIRPS (LINEA BASE)
#POR JUAN JOSE AMIDES FIGUEROA URBANO , EL SALVADOR, 12 DE NOVIEMBRE 2018. -- joseamidesfigueroa@gmail.com

#------------------------------------------------------------------------------------------------------------
#Carga librerias necesarias
library(raster)
library(rgdal)
library(rgeos)
library(crs)
library(sp)

#Define algunas variables para ser usadas
setwd("C:/R")
#Matriz Latitud

#------------------------------------------------------------------------------------------------------------
#Lee los raster de trabajo
raw_acum<-raster("acum_general_03.tif")
desv<-raster("C:/R/desv/03.tif")

#------------------------------------------------------------------------------------------------------------
#Crea un raster con el area efectiva de trabajo (RASTER DUMMY)
m<-matrix(runif(400),20,20)
area_efectiva<-raster(m)
extent(area_efectiva)<-c(264.2-360,291.8855-360,6.849999,23.38065)
projection(area_efectiva) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#------------------------------------------------------------------------------------------------------------
#Define la extensi?n del raster y cambia a valores negativos la longitud (RASTER LLUVIA ACUMULADA 'CRUDA')
extent(raw_acum)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
projection(raw_acum) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Define longitudes negativas
extent(desv)<-c(264.2-360,291.8855-360,6.849999,23.38065)
projection(desv) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


#------------------------------------------------------------------------------------------------------------
#Recorta el raster de pronostico contra el raster del area efectiva de trabajo y hace un resample
acum<-crop(raw_acum,area_efectiva)
acum<-resample(acum,desv) #PRECIPITACION ACUMULADA RECORTADA "DATO CRUDO DEL MODELO"

#------------------------------------------------------------------------------------------------------------
#Genera un raster para guardar el pronostico ajustado (RASTER VACIO PARA GUARDAR EL RESULTADO DEL AJUSTE)
n<-matrix(runif(400),20,20)
pronostico_ajustado<-raster(n)
extent(pronostico_ajustado)<-c(264.2-360,291.8855-360,6.849999,23.38065)
projection(pronostico_ajustado) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
pronostico_ajustado<-resample(pronostico_ajustado,desv)

##############################################################################################################
##############################################################################################################
##############################################################################################################
#---- Ajuste del pron?stico

#------------------------------------------------------------------------------------------------------------
#Asigna el contenido de los rasters de lluvia pronosticada y desviaci?n promeido a matrices
mat_acum<-as.matrix(acum)
mat_acum
mat_desv<-as.matrix(desv)

#------------------------------------------------------------------------------------------------------------
#Creacion de un data Frame para exportar luego a GeoJson
latitud<-c(seq(from=6.849999, to=23.38065,by=0.06))
length(latitud)

lon<-c(seq(from=-95.8, to=-68.1145,by=0.1))
longitud<-lon[1:276]
length(longitud)
dato<-c(1:276)

i=1



#dato[2]<-cbind(extract(raw_acum,cbind(cbind(longitud[2],latitud[2])), method='bilinear'))
#latitud[2]
#longitud[2]
#dato[2]

while (i<=length(latitud)) {
   dato[i]<-cbind(extract(raw_acum,cbind(cbind(longitud[i],latitud[i])), method='bilinear'))
  i=i+1
}

length(dato)

datosgejason<-data.frame(latitud,longitud,dato)
datosgejason
names(datosgejason)

#Exportar a JSON
library(rgdal)

#dataMap is a dataframe with coordinates on cols 11 (LATITUDE) and 12 (LONGITUDE)
#Transfor coordinates to numeric
dataMap$LATITUDE  <- as.numeric(dataMap$LATITUDE)
dataMap$LONGITUDE  <- as.numeric(dataMap$LONGITUDE)
dataMap.SP  <- SpatialPointsDataFrame(dataMap[,c(12,11)],dataMap[,-c(12,11)])
str(dataMap.SP) # Now is class SpatialPointsDataFrame

#Write as geojson
writeOGR(dataMap.SP, 'dataMap.geojson','dataMap', driver='GeoJSON')






#------------------------------------------------------------------------------------------------------------
#Calcula la diferencia entre el raster de pronostico recortado y la desviacion promedio

#Funcion condicional
Con=function(condition, trueValue, falseValue){
  return(condition * trueValue + (!condition)*falseValue)}

#Condicional para ajuste de pron?stico:
#Evalua si la desviaci?n en negativa o positiva para realizar suma o resta seg?n sea el caso.
acum_ajustada=as.matrix(Con(mat_desv>0,mat_acum-mat_desv,mat_acum+mat_desv))

#Evalua si el resultado del ajuste es negativo, si lo fuere se deja el pron?stico sin ajuste.
acum_ajustada_final=as.matrix(Con(acum_ajustada<0,acum_ajustada_final<-mat_acum,acum_ajustada_final<-acum_ajustada))

#------------------------------------------------------------------------------------------------------------
#Aplica los valores resultantes a un raster y re ajusta la proyecci?n y extensi?n
#PRECIPITACION AJUSTADA POR LINEA BASE
pronostico_ajustado<-raster(acum_ajustada_final)
pronostico_ajustado

extent(pronostico_ajustado)<-c(264.2-360,291.8855-360,6.849999,23.38065)
projection(pronostico_ajustado) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

##############################################################################################################
##############################################################################################################
##############################################################################################################

#Intento de realizar plot:
#DEM@crs
#crs(DEM) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
#DEM@crs
#DEM@extent
#hist(DEM,main="Distribucion de la lluvia en el raster",col="black",maxpixels=6580)

#plot(DEM)
#plot(acum)

#image(DEM)
#image(DEM, zlim=c(0,600))

#col=terrain.colors(12)
#brk <- c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550)
#plot(DEM, , zlim=c(0,600), col=col, breaks=brk, main="DEM with more breaks")

#------------------------------------------------------------------------------------------------------------
#Definici?n de estaciones para extraer datos de precipitaci?n:

E01_Guija<-cbind(-89.46999,14.229286)
E02_Los_Andes<-cbind(-89.628326000000001,13.874305000000000)
E03_Candelaria_Frontera<-cbind(-89.651657999999998,14.120964000000001)
E04_Montecristo<-cbind(-89.359988999999999,14.399279000000000)
E05_Santa_Ana<-cbind(-89.548320000000004,13.982635999999999)
E06_Chorrea_Guayabo<-cbind(-89.548320000000004,13.987634000000000)
E07_Sensuntepeque<-cbind(-88.646636000000001,13.870975000000000)
E08_Cerron_Grande<-cbind(-88.926643999999996,13.93764000000000)
E09_Cojutepeque<-cbind(-88.926643999999996,13.720983000000000)
E10_Nueva_Concepcion<-cbind(-89.289987999999994,14.125966000000000)
E11_La_Palma<-cbind(-89.161653999999999,14.292623000000001)
E12_Las_Pilas<-cbind(-89.088307999999998,14.374274000000000)
E13_Ahuachapan<-cbind(-89.859994999999998,13.944305999999999)
E14_La_Hachadura<-cbind(-90.089990000000000,13.860979000000000)
E15_San_Andres<-cbind(-89.406654000000003,13.809312000000000)
E16_Chiltiupan<-cbind(-89.469984999999994,13.597661000000000)
E17_San_Miguel<-cbind(-88.158293999999998,13.439336000000001)
E18_La_Union<-cbind(-87.826779000000002,13.333684000000000)
E19_Ilopango<-cbind(-89.118313,13.699318)
E20_Acajutla<-cbind(-89.833333999999994,13.574325999999999)
E21_Los_Naranjos<-cbind(-89.674995999999993,13.875971000000000)
E22_Santiago_Maria<-cbind(-88.471638999999996,13.485992000000000)
E23_Puente_Cuscatlan<-cbind(-88.593303000000006,13.602655000000000)
E24_San_Francisco_Gotera<-cbind(-88.106623999999996,13.697651000000000)
E25_Perquin<-cbind(-88.162499999999994,13.960889000000000)

#------------------------------------------------------------------------------------------------------------
#Extrae el dato de pronostico por estaci?n del dato "crudo" del modelo:

pp01<-extract(acum,E01_Guija, method='bilinear')
pp02<-extract(acum,E02_Los_Andes, method='bilinear')
pp03<-extract(acum,E03_Candelaria_Frontera, method='bilinear')
pp04<-extract(acum,E04_Montecristo, method='bilinear')
pp05<-extract(acum,E05_Santa_Ana, method='bilinear')
pp06<-extract(acum,E06_Chorrea_Guayabo, method='bilinear')
pp07<-extract(acum,E07_Sensuntepeque, method='bilinear')
pp08<-extract(acum,E08_Cerron_Grande, method='bilinear')
pp09<-extract(acum,E09_Cojutepeque, method='bilinear')
pp10<-extract(acum,E10_Nueva_Concepcion, method='bilinear')
pp11<-extract(acum,E11_La_Palma, method='bilinear')
pp12<-extract(acum,E12_Las_Pilas, method='bilinear')
pp13<-extract(acum,E13_Ahuachapan, method='bilinear')
pp14<-extract(acum,E14_La_Hachadura, method='bilinear')
pp15<-extract(acum,E15_San_Andres, method='bilinear')
pp16<-extract(acum,E16_Chiltiupan, method='bilinear')
pp17<-extract(acum,E17_San_Miguel, method='bilinear')
pp18<-extract(acum,E18_La_Union, method='bilinear')
pp19<-extract(acum,E19_Ilopango, method='bilinear')
pp20<-extract(acum,E20_Acajutla, method='bilinear')
pp21<-extract(acum,E21_Los_Naranjos, method='bilinear')
pp22<-extract(acum,E22_Santiago_Maria, method='bilinear')
pp23<-extract(acum,E23_Puente_Cuscatlan, method='bilinear')
pp24<-extract(acum,E24_San_Francisco_Gotera, method='bilinear')
pp25<-extract(acum,E25_Perquin, method='bilinear')


#------------------------------------------------------------------------------------------------------------
#Extrae el dato de desviacion promedio por estaci?n
aa01<-extract(desv,E01_Guija, method='bilinear')
aa02<-extract(desv,E02_Los_Andes, method='bilinear')
aa03<-extract(desv,E03_Candelaria_Frontera, method='bilinear')
aa04<-extract(desv,E04_Montecristo, method='bilinear')
aa05<-extract(desv,E05_Santa_Ana, method='bilinear')
aa06<-extract(desv,E06_Chorrea_Guayabo, method='bilinear')
aa07<-extract(desv,E07_Sensuntepeque, method='bilinear')
aa08<-extract(desv,E08_Cerron_Grande, method='bilinear')
aa09<-extract(desv,E09_Cojutepeque, method='bilinear')
aa10<-extract(desv,E10_Nueva_Concepcion, method='bilinear')
aa11<-extract(desv,E11_La_Palma, method='bilinear')
aa12<-extract(desv,E12_Las_Pilas, method='bilinear')
aa13<-extract(desv,E13_Ahuachapan, method='bilinear')
aa14<-extract(desv,E14_La_Hachadura, method='bilinear')
aa15<-extract(desv,E15_San_Andres, method='bilinear')
aa16<-extract(desv,E16_Chiltiupan, method='bilinear')
aa17<-extract(desv,E17_San_Miguel, method='bilinear')
aa18<-extract(desv,E18_La_Union, method='bilinear')
aa19<-extract(desv,E19_Ilopango, method='bilinear')
aa20<-extract(desv,E20_Acajutla, method='bilinear')
aa21<-extract(desv,E21_Los_Naranjos, method='bilinear')
aa22<-extract(desv,E22_Santiago_Maria, method='bilinear')
aa23<-extract(desv,E23_Puente_Cuscatlan, method='bilinear')
aa24<-extract(desv,E24_San_Francisco_Gotera, method='bilinear')
aa25<-extract(desv,E25_Perquin, method='bilinear')

#------------------------------------------------------------------------------------------------------------
#Extrae el dato de pronostico por estaci?n del dato ajustado del modelo por linea base
pa01<-extract(pronostico_ajustado,E01_Guija, method='bilinear')
pa02<-extract(pronostico_ajustado,E02_Los_Andes, method='bilinear')
pa03<-extract(pronostico_ajustado,E03_Candelaria_Frontera, method='bilinear')
pa04<-extract(pronostico_ajustado,E04_Montecristo, method='bilinear')
pa05<-extract(pronostico_ajustado,E05_Santa_Ana, method='bilinear')
pa06<-extract(pronostico_ajustado,E06_Chorrea_Guayabo, method='bilinear')
pa07<-extract(pronostico_ajustado,E07_Sensuntepeque, method='bilinear')
pa08<-extract(pronostico_ajustado,E08_Cerron_Grande, method='bilinear')
pa09<-extract(pronostico_ajustado,E09_Cojutepeque, method='bilinear')
pa10<-extract(pronostico_ajustado,E10_Nueva_Concepcion, method='bilinear')
pa11<-extract(pronostico_ajustado,E11_La_Palma, method='bilinear')
pa12<-extract(pronostico_ajustado,E12_Las_Pilas, method='bilinear')
pa13<-extract(pronostico_ajustado,E13_Ahuachapan, method='bilinear')
pa14<-extract(pronostico_ajustado,E14_La_Hachadura, method='bilinear')
pa15<-extract(pronostico_ajustado,E15_San_Andres, method='bilinear')
pa16<-extract(pronostico_ajustado,E16_Chiltiupan, method='bilinear')
pa17<-extract(pronostico_ajustado,E17_San_Miguel, method='bilinear')
pa18<-extract(pronostico_ajustado,E18_La_Union, method='bilinear')
pa19<-extract(pronostico_ajustado,E19_Ilopango, method='bilinear')
pa20<-extract(pronostico_ajustado,E20_Acajutla, method='bilinear')
pa21<-extract(pronostico_ajustado,E21_Los_Naranjos, method='bilinear')
pa22<-extract(pronostico_ajustado,E22_Santiago_Maria, method='bilinear')
pa23<-extract(pronostico_ajustado,E23_Puente_Cuscatlan, method='bilinear')
pa24<-extract(pronostico_ajustado,E24_San_Francisco_Gotera, method='bilinear')
pa25<-extract(pronostico_ajustado,E25_Perquin, method='bilinear')

#------------------------------------------------------------------------------------------------------------
#Crea vector con la lluvia por estacion
Lluvia_mensual_cruda<-c(pp01,pp02,pp03,pp04,pp05,pp06,pp07,pp08,pp09,pp10,pp11,pp12,pp13,pp14,pp15,pp16,pp17,pp18,pp19,pp20,pp21,pp22,pp23,pp24,pp25)
Lluvia_mensual_ajustada<-c(pa01,pa02,pa03,pa04,pa05,pa06,pa07,pa08,pa09,pa10,pa11,pa12,pa13,pa14,pa15,pa16,pa17,pa18,pa19,pa20,pa21,pa22,pa23,pa24,pa25)
desviacion_promedio<-c(aa01,aa02,aa03,aa04,aa05,aa06,aa07,aa08,aa09,aa10,aa11,aa12,aa13,aa14,aa15,aa16,aa17,aa18,aa19,aa20,aa21,aa22,aa23,aa24,aa25)

#------------------------------------------------------------------------------------------------------------
#Crea matriz para transponer datos a una linea con datos separados por comas.
mat_Lluvia_mensual_cruda<-matrix(Lluvia_mensual_cruda,nrow = 1,ncol = 25)
mat_Lluvia_mensual_ajustada<-matrix(Lluvia_mensual_ajustada,nrow = 1,ncol = 25)
mat_desviacion_promedio<-matrix(desviacion_promedio,nrow = 1,ncol = 25)

#------------------------------------------------------------------------------------------------------------
#Escribe los valores en un CSV
write.table(mat_Lluvia_mensual_cruda, file="lluvia_mensual_raw.csv",row.names = FALSE, col.names = FALSE, sep =",")
write.table(mat_Lluvia_mensual_ajustada, file="lluvia_mensual_ajustada.csv",row.names = FALSE, col.names = FALSE, sep =",")
write.table(mat_desviacion_promedio, file="desviacion_promedio.csv",row.names = FALSE, col.names = FALSE, sep =",")

#------------------------------------------------------------------------------------------------------------
#Escribe los valores de modelo ajustado en un raster
rf<-writeRaster(pronostico_ajustado, filename="pronostico_ajustado.tif", overwrite=TRUE)