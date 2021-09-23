#GENERA PRONOSTICO DE LLUVIA ACUMULA MENSUAL AJUSTADA CON ERROR PROMEDIO MENSUAL DEL MODELO VS CHIRPS (LINEA BASE)
#POR JUAN JOSE AMIDES FIGUEROA URBANO , EL SALVADOR, 12 DE NOVIEMBRE 2018. -- joseamidesfigueroa@gmail.com

#------------------------------------------------------------------------------------------------------------
#Carga librerias necesarias
library(raster)
library(rgdal)
library(rgeos)
library(crs)
library(sp)
library(gglot2)
library(xlsx)

#Carga shapefile
#mapa <- readOGR(dsn=path.expand("D:/R/shape"),layer="ESA_CA_wgs84")

#carga geotiff de lluvia
lluvia_24_A<-raster("C:/R/Conjunto/06/A/lluvia_72h.tif")
lluvia_24_B<-raster("C:/R/Conjunto/06/B/lluvia_72h.tif")
lluvia_24_C<-raster("C:/R/Conjunto/06/C/lluvia_72h.tif")
lluvia_24_D<-raster("C:/R/Conjunto/06/D/lluvia_72h.tif")
lluvia_24_E<-raster("C:/R/Conjunto/06/E/lluvia_72h.tif")
lluvia_24_F<-raster("C:/R/Conjunto/06/F/lluvia_72h.tif")
lluvia_24_G<-raster("C:/R/Conjunto/06/G/lluvia_72h.tif")


#Define la extension del raster y cambia a valores negativos la longitud. A
extent(lluvia_24_A)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_24_A) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Define la extension del raster y cambia a valores negativos la longitud. B
extent(lluvia_24_B)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_24_B) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Define la extension del raster y cambia a valores negativos la longitud. C
extent(lluvia_24_C)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_24_C) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Define la extension del raster y cambia a valores negativos la longitud. D
extent(lluvia_24_D)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_24_D) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Define la extension del raster y cambia a valores negativos la longitud. E
extent(lluvia_24_E)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_24_E) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Define la extension del raster y cambia a valores negativos la longitud. F
extent(lluvia_24_F)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_24_F) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Define la extension del raster y cambia a valores negativos la longitud. G
extent(lluvia_24_G)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_24_G) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


#************************************************************************
#************************************************************************
#Define la estación del aeropuerto de Ilopango.
E01<-cbind(-89.46999,14.229286)
E02<-cbind(-89.628326000000001,13.874305000000000)
E03<-cbind(-89.651657999999998,14.120964000000001)
E04<-cbind(-89.359988999999999,14.399279000000000)
E05<-cbind(-89.548320000000004,13.982635999999999)
E06<-cbind(-89.548320000000004,13.987634000000000)
E07<-cbind(-88.646636000000001,13.870975000000000)
E08<-cbind(-88.926643999999996,13.93764000000000)
E09<-cbind(-88.926643999999996,13.720983000000000)
E10<-cbind(-89.289987999999994,14.125966000000000)
E11<-cbind(-89.161653999999999,14.292623000000001)
E12<-cbind(-89.088307999999998,14.374274000000000)
E13<-cbind(-89.859994999999998,13.944305999999999)
E14<-cbind(-90.089990000000000,13.860979000000000)
E15<-cbind(-89.406654000000003,13.809312000000000)
E16<-cbind(-89.469984999999994,13.597661000000000)
E17<-cbind(-88.158293999999998,13.439336000000001)
E18<-cbind(-87.826779000000002,13.333684000000000)
E19<-cbind(-89.118313,13.699318)
E20<-cbind(-89.833333999999994,13.574325999999999)
E21<-cbind(-89.674995999999993,13.875971000000000)
E22<-cbind(-88.471638999999996,13.485992000000000)
E23<-cbind(-88.593303000000006,13.602655000000000)
E24<-cbind(-88.106623999999996,13.697651000000000)
E25<-cbind(-88.162499999999994,13.960889000000000)

#Genera vector de estaciones
estaciones<-data.frame(E01,E02,E03,E04,E05,E06,E07,E08,E09,E10,E11,E12,E13,E14,E15,E16,E17,E18,E19,E20,
              E21,E22,E23,E24,E25)
E01
#*******************************************************************************************
#Extrae datos
#*******************************************************************************************
#*******************************************************************************************
#E01
E01_A<-extract(lluvia_24_A,E01, method='bilinear')
E01_B<-extract(lluvia_24_B,E01, method='bilinear')
E01_C<-extract(lluvia_24_C,E01, method='bilinear')
E01_D<-extract(lluvia_24_D,E01, method='bilinear')
E01_E<-extract(lluvia_24_E,E01, method='bilinear')
E01_F<-extract(lluvia_24_F,E01, method='bilinear')
E01_G<-extract(lluvia_24_G,E01, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E02
E02_A<-extract(lluvia_24_A,E02, method='bilinear')
E02_B<-extract(lluvia_24_B,E02, method='bilinear')
E02_C<-extract(lluvia_24_C,E02, method='bilinear')
E02_D<-extract(lluvia_24_D,E02, method='bilinear')
E02_E<-extract(lluvia_24_E,E02, method='bilinear')
E02_F<-extract(lluvia_24_F,E02, method='bilinear')
E02_G<-extract(lluvia_24_G,E02, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E03
E03_A<-extract(lluvia_24_A,E03, method='bilinear')
E03_B<-extract(lluvia_24_B,E03, method='bilinear')
E03_C<-extract(lluvia_24_C,E03, method='bilinear')
E03_D<-extract(lluvia_24_D,E03, method='bilinear')
E03_E<-extract(lluvia_24_E,E03, method='bilinear')
E03_F<-extract(lluvia_24_F,E03, method='bilinear')
E03_G<-extract(lluvia_24_G,E03, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E04
E04_A<-extract(lluvia_24_A,E04, method='bilinear')
E04_B<-extract(lluvia_24_B,E04, method='bilinear')
E04_C<-extract(lluvia_24_C,E04, method='bilinear')
E04_D<-extract(lluvia_24_D,E04, method='bilinear')
E04_E<-extract(lluvia_24_E,E04, method='bilinear')
E04_F<-extract(lluvia_24_F,E04, method='bilinear')
E04_G<-extract(lluvia_24_G,E04, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E05
E05_A<-extract(lluvia_24_A,E05, method='bilinear')
E05_B<-extract(lluvia_24_B,E05, method='bilinear')
E05_C<-extract(lluvia_24_C,E05, method='bilinear')
E05_D<-extract(lluvia_24_D,E05, method='bilinear')
E05_E<-extract(lluvia_24_E,E05, method='bilinear')
E05_F<-extract(lluvia_24_F,E05, method='bilinear')
E05_G<-extract(lluvia_24_G,E05, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E06
E06_A<-extract(lluvia_24_A,E06, method='bilinear')
E06_B<-extract(lluvia_24_B,E06, method='bilinear')
E06_C<-extract(lluvia_24_C,E06, method='bilinear')
E06_D<-extract(lluvia_24_D,E06, method='bilinear')
E06_E<-extract(lluvia_24_E,E06, method='bilinear')
E06_F<-extract(lluvia_24_F,E06, method='bilinear')
E06_G<-extract(lluvia_24_G,E06, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E07
E07_A<-extract(lluvia_24_A,E07, method='bilinear')
E07_B<-extract(lluvia_24_B,E07, method='bilinear')
E07_C<-extract(lluvia_24_C,E07, method='bilinear')
E07_D<-extract(lluvia_24_D,E07, method='bilinear')
E07_E<-extract(lluvia_24_E,E07, method='bilinear')
E07_F<-extract(lluvia_24_F,E07, method='bilinear')
E07_G<-extract(lluvia_24_G,E07, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E08
E08_A<-extract(lluvia_24_A,E08, method='bilinear')
E08_B<-extract(lluvia_24_B,E08, method='bilinear')
E08_C<-extract(lluvia_24_C,E08, method='bilinear')
E08_D<-extract(lluvia_24_D,E08, method='bilinear')
E08_E<-extract(lluvia_24_E,E08, method='bilinear')
E08_F<-extract(lluvia_24_F,E08, method='bilinear')
E08_G<-extract(lluvia_24_G,E08, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E09
E09_A<-extract(lluvia_24_A,E09, method='bilinear')
E09_B<-extract(lluvia_24_B,E09, method='bilinear')
E09_C<-extract(lluvia_24_C,E09, method='bilinear')
E09_D<-extract(lluvia_24_D,E09, method='bilinear')
E09_E<-extract(lluvia_24_E,E09, method='bilinear')
E09_F<-extract(lluvia_24_F,E09, method='bilinear')
E09_G<-extract(lluvia_24_G,E09, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E10
E10_A<-extract(lluvia_24_A,E10, method='bilinear')
E10_B<-extract(lluvia_24_B,E10, method='bilinear')
E10_C<-extract(lluvia_24_C,E10, method='bilinear')
E10_D<-extract(lluvia_24_D,E10, method='bilinear')
E10_E<-extract(lluvia_24_E,E10, method='bilinear')
E10_F<-extract(lluvia_24_F,E10, method='bilinear')
E10_G<-extract(lluvia_24_G,E10, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E11
E11_A<-extract(lluvia_24_A,E11, method='bilinear')
E11_B<-extract(lluvia_24_B,E11, method='bilinear')
E11_C<-extract(lluvia_24_C,E11, method='bilinear')
E11_D<-extract(lluvia_24_D,E11, method='bilinear')
E11_E<-extract(lluvia_24_E,E11, method='bilinear')
E11_F<-extract(lluvia_24_F,E11, method='bilinear')
E11_G<-extract(lluvia_24_G,E11, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E12
E12_A<-extract(lluvia_24_A,E12, method='bilinear')
E12_B<-extract(lluvia_24_B,E12, method='bilinear')
E12_C<-extract(lluvia_24_C,E12, method='bilinear')
E12_D<-extract(lluvia_24_D,E12, method='bilinear')
E12_E<-extract(lluvia_24_E,E12, method='bilinear')
E12_F<-extract(lluvia_24_F,E12, method='bilinear')
E12_G<-extract(lluvia_24_G,E12, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E13
E13_A<-extract(lluvia_24_A,E13, method='bilinear')
E13_B<-extract(lluvia_24_B,E13, method='bilinear')
E13_C<-extract(lluvia_24_C,E13, method='bilinear')
E13_D<-extract(lluvia_24_D,E13, method='bilinear')
E13_E<-extract(lluvia_24_E,E13, method='bilinear')
E13_F<-extract(lluvia_24_F,E13, method='bilinear')
E13_G<-extract(lluvia_24_G,E13, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E14
E14_A<-extract(lluvia_24_A,E14, method='bilinear')
E14_B<-extract(lluvia_24_B,E14, method='bilinear')
E14_C<-extract(lluvia_24_C,E14, method='bilinear')
E14_D<-extract(lluvia_24_D,E14, method='bilinear')
E14_E<-extract(lluvia_24_E,E14, method='bilinear')
E14_F<-extract(lluvia_24_F,E14, method='bilinear')
E14_G<-extract(lluvia_24_G,E14, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E15
E15_A<-extract(lluvia_24_A,E15, method='bilinear')
E15_B<-extract(lluvia_24_B,E15, method='bilinear')
E15_C<-extract(lluvia_24_C,E15, method='bilinear')
E15_D<-extract(lluvia_24_D,E15, method='bilinear')
E15_E<-extract(lluvia_24_E,E15, method='bilinear')
E15_F<-extract(lluvia_24_F,E15, method='bilinear')
E15_G<-extract(lluvia_24_G,E15, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E16
E17_A<-extract(lluvia_24_A,E17, method='bilinear')
E17_B<-extract(lluvia_24_B,E17, method='bilinear')
E17_C<-extract(lluvia_24_C,E17, method='bilinear')
E17_D<-extract(lluvia_24_D,E17, method='bilinear')
E17_E<-extract(lluvia_24_E,E17, method='bilinear')
E17_F<-extract(lluvia_24_F,E17, method='bilinear')
E17_G<-extract(lluvia_24_G,E17, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E18
E18_A<-extract(lluvia_24_A,E18, method='bilinear')
E18_B<-extract(lluvia_24_B,E18, method='bilinear')
E18_C<-extract(lluvia_24_C,E18, method='bilinear')
E18_D<-extract(lluvia_24_D,E18, method='bilinear')
E18_E<-extract(lluvia_24_E,E18, method='bilinear')
E18_F<-extract(lluvia_24_F,E18, method='bilinear')
E18_G<-extract(lluvia_24_G,E18, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E19
E19_A<-extract(lluvia_24_A,E19, method='bilinear')
E19_B<-extract(lluvia_24_B,E19, method='bilinear')
E19_C<-extract(lluvia_24_C,E19, method='bilinear')
E19_D<-extract(lluvia_24_D,E19, method='bilinear')
E19_E<-extract(lluvia_24_E,E19, method='bilinear')
E19_F<-extract(lluvia_24_F,E19, method='bilinear')
E19_G<-extract(lluvia_24_G,E19, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E20
E20_A<-extract(lluvia_24_A,E20, method='bilinear')
E20_B<-extract(lluvia_24_B,E20, method='bilinear')
E20_C<-extract(lluvia_24_C,E20, method='bilinear')
E20_D<-extract(lluvia_24_D,E20, method='bilinear')
E20_E<-extract(lluvia_24_E,E20, method='bilinear')
E20_F<-extract(lluvia_24_F,E20, method='bilinear')
E20_G<-extract(lluvia_24_G,E20, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E21
E21_A<-extract(lluvia_24_A,E21, method='bilinear')
E21_B<-extract(lluvia_24_B,E21, method='bilinear')
E21_C<-extract(lluvia_24_C,E21, method='bilinear')
E21_D<-extract(lluvia_24_D,E21, method='bilinear')
E21_E<-extract(lluvia_24_E,E21, method='bilinear')
E21_F<-extract(lluvia_24_F,E21, method='bilinear')
E21_G<-extract(lluvia_24_G,E21, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E22
E22_A<-extract(lluvia_24_A,E22, method='bilinear')
E22_B<-extract(lluvia_24_B,E22, method='bilinear')
E22_C<-extract(lluvia_24_C,E22, method='bilinear')
E22_D<-extract(lluvia_24_D,E22, method='bilinear')
E22_E<-extract(lluvia_24_E,E22, method='bilinear')
E22_F<-extract(lluvia_24_F,E22, method='bilinear')
E22_G<-extract(lluvia_24_G,E22, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E23
E23_A<-extract(lluvia_24_A,E23, method='bilinear')
E23_B<-extract(lluvia_24_B,E23, method='bilinear')
E23_C<-extract(lluvia_24_C,E23, method='bilinear')
E23_D<-extract(lluvia_24_D,E23, method='bilinear')
E23_E<-extract(lluvia_24_E,E23, method='bilinear')
E23_F<-extract(lluvia_24_F,E23, method='bilinear')
E23_G<-extract(lluvia_24_G,E23, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E24
E24_A<-extract(lluvia_24_A,E24, method='bilinear')
E24_B<-extract(lluvia_24_B,E24, method='bilinear')
E24_C<-extract(lluvia_24_C,E24, method='bilinear')
E24_D<-extract(lluvia_24_D,E24, method='bilinear')
E24_E<-extract(lluvia_24_E,E24, method='bilinear')
E24_F<-extract(lluvia_24_F,E24, method='bilinear')
E24_G<-extract(lluvia_24_G,E24, method='bilinear')

#******************************************************************************************
#*******************************************************************************************
#E25
E25_A<-extract(lluvia_24_A,E25, method='bilinear')
E25_B<-extract(lluvia_24_B,E25, method='bilinear')
E25_C<-extract(lluvia_24_C,E25, method='bilinear')
E25_D<-extract(lluvia_24_D,E25, method='bilinear')
E25_E<-extract(lluvia_24_E,E25, method='bilinear')
E25_F<-extract(lluvia_24_F,E25, method='bilinear')
E25_G<-extract(lluvia_24_G,E25, method='bilinear')

#******************************************************************************************
#******************************************************************************************
#******************************************************************************************
#******************************************************************************************


#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E01
#******************************************************************************************
#******************************************************************************************
probabilidad_100mm<-0
contador<-0
if(E01_A>=100){
  contador=contador+1
}
if(E01_B>=100){
  contador=contador+1
}
if(E01_C>=100){
  contador=contador+1
}
if(E01_D>=100){
  contador=contador+1
}
if(E01_E>=100){
  contador=contador+1
}
if(E01_F>=100){
  contador=contador+1
}
if(E01_G>=100){
  contador=contador+1
}
prob_E01_100mm<-contador/7
#prob_E01_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E02
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E02_A>=100){
  contador=contador+1
}
if(E02_B>=100){
  contador=contador+1
}
if(E02_C>=100){
  contador=contador+1
}
if(E02_D>=100){
  contador=contador+1
}
if(E02_E>=100){
  contador=contador+1
}
if(E02_F>=100){
  contador=contador+1
}
if(E02_G>=100){
  contador=contador+1
}
prob_E02_100mm<-contador/7
#prob_E02_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E03
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E03_A>=100){
  contador=contador+1
}
if(E03_B>=100){
  contador=contador+1
}
if(E03_C>=100){
  contador=contador+1
}
if(E03_D>=100){
  contador=contador+1
}
if(E03_E>=100){
  contador=contador+1
}
if(E03_F>=100){
  contador=contador+1
}
if(E03_G>=100){
  contador=contador+1
}
prob_E03_100mm<-contador/7
#prob_E03_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E04
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E04_A>=100){
  contador=contador+1
}
if(E04_B>=100){
  contador=contador+1
}
if(E04_C>=100){
  contador=contador+1
}
if(E04_D>=100){
  contador=contador+1
}
if(E04_E>=100){
  contador=contador+1
}
if(E04_F>=100){
  contador=contador+1
}
if(E04_G>=100){
  contador=contador+1
}
prob_E04_100mm<-contador/7
#prob_E04_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E05
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E05_A>=100){
  contador=contador+1
}
if(E05_B>=100){
  contador=contador+1
}
if(E05_C>=100){
  contador=contador+1
}
if(E05_D>=100){
  contador=contador+1
}
if(E05_E>=100){
  contador=contador+1
}
if(E05_F>=100){
  contador=contador+1
}
if(E05_G>=100){
  contador=contador+1
}
prob_E05_100mm<-contador/7
#prob_E05_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E06
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E06_A>=100){
  contador=contador+1
}
if(E06_B>=100){
  contador=contador+1
}
if(E06_C>=100){
  contador=contador+1
}
if(E06_D>=100){
  contador=contador+1
}
if(E06_E>=100){
  contador=contador+1
}
if(E06_F>=100){
  contador=contador+1
}
if(E06_G>=100){
  contador=contador+1
}
prob_E06_100mm<-contador/7
#prob_E06_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E07
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E07_A>=100){
  contador=contador+1
}
if(E07_B>=100){
  contador=contador+1
}
if(E07_C>=100){
  contador=contador+1
}
if(E07_D>=100){
  contador=contador+1
}
if(E07_E>=100){
  contador=contador+1
}
if(E07_F>=100){
  contador=contador+1
}
if(E07_G>=100){
  contador=contador+1
}
prob_E07_100mm<-contador/7
#prob_E07_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E08
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E08_A>=100){
  contador=contador+1
}
if(E08_B>=100){
  contador=contador+1
}
if(E08_C>=100){
  contador=contador+1
}
if(E08_D>=100){
  contador=contador+1
}
if(E08_E>=100){
  contador=contador+1
}
if(E08_F>=100){
  contador=contador+1
}
if(E08_G>=100){
  contador=contador+1
}
prob_E08_100mm<-contador/7
#prob_E08_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E09
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E09_A>=100){
  contador=contador+1
}
if(E09_B>=100){
  contador=contador+1
}
if(E09_C>=100){
  contador=contador+1
}
if(E09_D>=100){
  contador=contador+1
}
if(E09_E>=100){
  contador=contador+1
}
if(E09_F>=100){
  contador=contador+1
}
if(E09_G>=100){
  contador=contador+1
}
prob_E09_100mm<-contador/7
#prob_E09_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E10
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E10_A>=100){
  contador=contador+1
}
if(E10_B>=100){
  contador=contador+1
}
if(E10_C>=100){
  contador=contador+1
}
if(E10_D>=100){
  contador=contador+1
}
if(E10_E>=100){
  contador=contador+1
}
if(E10_F>=100){
  contador=contador+1
}
if(E10_G>=100){
  contador=contador+1
}
prob_E10_100mm<-contador/7
#prob_E10_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E11
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E11_A>=100){
  contador=contador+1
}
if(E11_B>=100){
  contador=contador+1
}
if(E11_C>=100){
  contador=contador+1
}
if(E11_D>=100){
  contador=contador+1
}
if(E11_E>=100){
  contador=contador+1
}
if(E11_F>=100){
  contador=contador+1
}
if(E11_G>=100){
  contador=contador+1
}
prob_E11_100mm<-contador/7
#prob_E11_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E12
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E12_A>=100){
  contador=contador+1
}
if(E12_B>=100){
  contador=contador+1
}
if(E12_C>=100){
  contador=contador+1
}
if(E12_D>=100){
  contador=contador+1
}
if(E12_E>=100){
  contador=contador+1
}
if(E12_F>=100){
  contador=contador+1
}
if(E12_G>=100){
  contador=contador+1
}
prob_E12_100mm<-contador/7
#prob_E12_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E13
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E13_A>=100){
  contador=contador+1
}
if(E13_B>=100){
  contador=contador+1
}
if(E13_C>=100){
  contador=contador+1
}
if(E13_D>=100){
  contador=contador+1
}
if(E13_E>=100){
  contador=contador+1
}
if(E13_F>=100){
  contador=contador+1
}
if(E13_G>=100){
  contador=contador+1
}
prob_E13_100mm<-contador/7
#prob_E13_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E14
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E14_A>=100){
  contador=contador+1
}
if(E14_B>=100){
  contador=contador+1
}
if(E14_C>=100){
  contador=contador+1
}
if(E14_D>=100){
  contador=contador+1
}
if(E14_E>=100){
  contador=contador+1
}
if(E14_F>=100){
  contador=contador+1
}
if(E14_G>=100){
  contador=contador+1
}
prob_E14_100mm<-contador/7
#prob_E14_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E15
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E15_A>=100){
  contador=contador+1
}
if(E15_B>=100){
  contador=contador+1
}
if(E15_C>=100){
  contador=contador+1
}
if(E15_D>=100){
  contador=contador+1
}
if(E15_E>=100){
  contador=contador+1
}
if(E15_F>=100){
  contador=contador+1
}
if(E15_G>=100){
  contador=contador+1
}
prob_E15_100mm<-contador/7
#prob_E15_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E16
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E16_A>=100){
  contador=contador+1
}
if(E16_B>=100){
  contador=contador+1
}
if(E16_C>=100){
  contador=contador+1
}
if(E16_D>=100){
  contador=contador+1
}
if(E16_E>=100){
  contador=contador+1
}
if(E16_F>=100){
  contador=contador+1
}
if(E16_G>=100){
  contador=contador+1
}
prob_E16_100mm<-contador/7
#prob_E16_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E17
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E17_A>=100){
  contador=contador+1
}
if(E17_B>=100){
  contador=contador+1
}
if(E17_C>=100){
  contador=contador+1
}
if(E17_D>=100){
  contador=contador+1
}
if(E17_E>=100){
  contador=contador+1
}
if(E17_F>=100){
  contador=contador+1
}
if(E17_G>=100){
  contador=contador+1
}
prob_E17_100mm<-contador/7
#prob_E17_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E18
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E18_A>=100){
  contador=contador+1
}
if(E18_B>=100){
  contador=contador+1
}
if(E18_C>=100){
  contador=contador+1
}
if(E18_D>=100){
  contador=contador+1
}
if(E18_E>=100){
  contador=contador+1
}
if(E18_F>=100){
  contador=contador+1
}
if(E18_G>=100){
  contador=contador+1
}
prob_E18_100mm<-contador/7
#prob_E18_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E19
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E19_A>=100){
  contador=contador+1
}
if(E19_B>=100){
  contador=contador+1
}
if(E19_C>=100){
  contador=contador+1
}
if(E19_D>=100){
  contador=contador+1
}
if(E19_E>=100){
  contador=contador+1
}
if(E19_F>=100){
  contador=contador+1
}
if(E19_G>=100){
  contador=contador+1
}
prob_E19_100mm<-contador/7
#prob_E19_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E20
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E20_A>=100){
  contador=contador+1
}
if(E20_B>=100){
  contador=contador+1
}
if(E20_C>=100){
  contador=contador+1
}
if(E20_D>=100){
  contador=contador+1
}
if(E20_E>=100){
  contador=contador+1
}
if(E20_F>=100){
  contador=contador+1
}
if(E20_G>=100){
  contador=contador+1
}
prob_E20_100mm<-contador/7
#prob_E20_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E21
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E21_A>=100){
  contador=contador+1
}
if(E21_B>=100){
  contador=contador+1
}
if(E21_C>=100){
  contador=contador+1
}
if(E21_D>=100){
  contador=contador+1
}
if(E21_E>=100){
  contador=contador+1
}
if(E21_F>=100){
  contador=contador+1
}
if(E21_G>=100){
  contador=contador+1
}
prob_E21_100mm<-contador/7
#prob_E21_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E22
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E22_A>=100){
  contador=contador+1
}
if(E22_B>=100){
  contador=contador+1
}
if(E22_C>=100){
  contador=contador+1
}
if(E22_D>=100){
  contador=contador+1
}
if(E22_E>=100){
  contador=contador+1
}
if(E22_F>=100){
  contador=contador+1
}
if(E22_G>=100){
  contador=contador+1
}
prob_E22_100mm<-contador/7
#prob_E22_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E23
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E23_A>=100){
  contador=contador+1
}
if(E23_B>=100){
  contador=contador+1
}
if(E23_C>=100){
  contador=contador+1
}
if(E23_D>=100){
  contador=contador+1
}
if(E23_E>=100){
  contador=contador+1
}
if(E23_F>=100){
  contador=contador+1
}
if(E23_G>=100){
  contador=contador+1
}
prob_E23_100mm<-contador/7
#prob_E23_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E24
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E24_A>=100){
  contador=contador+1
}
if(E24_B>=100){
  contador=contador+1
}
if(E24_C>=100){
  contador=contador+1
}
if(E24_D>=100){
  contador=contador+1
}
if(E24_E>=100){
  contador=contador+1
}
if(E24_F>=100){
  contador=contador+1
}
if(E24_G>=100){
  contador=contador+1
}
prob_E24_100mm<-contador/7
#prob_E24_100mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 100mm                               E25
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E25_A>=100){
  contador=contador+1
}
if(E25_B>=100){
  contador=contador+1
}
if(E25_C>=100){
  contador=contador+1
}
if(E25_D>=100){
  contador=contador+1
}
if(E25_E>=100){
  contador=contador+1
}
if(E25_F>=100){
  contador=contador+1
}
if(E25_G>=100){
  contador=contador+1
}
prob_E25_100mm<-contador/7
#prob_E25_100mm
#******************************************************************************************
#******************************************************************************************

###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################


#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E01
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E01_A>=300){
  contador=contador+1
}
if(E01_B>=300){
  contador=contador+1
}
if(E01_C>=300){
  contador=contador+1
}
if(E01_D>=300){
  contador=contador+1
}
if(E01_E>=300){
  contador=contador+1
}
if(E01_F>=300){
  contador=contador+1
}
if(E01_G>=300){
  contador=contador+1
}
prob_E01_300mm<-contador/7
#prob_E01_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E02
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E02_A>=300){
  contador=contador+1
}
if(E02_B>=300){
  contador=contador+1
}
if(E02_C>=300){
  contador=contador+1
}
if(E02_D>=300){
  contador=contador+1
}
if(E02_E>=300){
  contador=contador+1
}
if(E02_F>=300){
  contador=contador+1
}
if(E02_G>=300){
  contador=contador+1
}
prob_E02_300mm<-contador/7
#prob_E02_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E03
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E03_A>=300){
  contador=contador+1
}
if(E03_B>=300){
  contador=contador+1
}
if(E03_C>=300){
  contador=contador+1
}
if(E03_D>=300){
  contador=contador+1
}
if(E03_E>=300){
  contador=contador+1
}
if(E03_F>=300){
  contador=contador+1
}
if(E03_G>=300){
  contador=contador+1
}
prob_E03_300mm<-contador/7
#prob_E03_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E04
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E04_A>=300){
  contador=contador+1
}
if(E04_B>=300){
  contador=contador+1
}
if(E04_C>=300){
  contador=contador+1
}
if(E04_D>=300){
  contador=contador+1
}
if(E04_E>=300){
  contador=contador+1
}
if(E04_F>=300){
  contador=contador+1
}
if(E04_G>=300){
  contador=contador+1
}
prob_E04_300mm<-contador/7
#prob_E04_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E05
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E05_A>=300){
  contador=contador+1
}
if(E05_B>=300){
  contador=contador+1
}
if(E05_C>=300){
  contador=contador+1
}
if(E05_D>=300){
  contador=contador+1
}
if(E05_E>=300){
  contador=contador+1
}
if(E05_F>=300){
  contador=contador+1
}
if(E05_G>=300){
  contador=contador+1
}
prob_E05_300mm<-contador/7
#prob_E05_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E06
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E06_A>=300){
  contador=contador+1
}
if(E06_B>=300){
  contador=contador+1
}
if(E06_C>=300){
  contador=contador+1
}
if(E06_D>=300){
  contador=contador+1
}
if(E06_E>=300){
  contador=contador+1
}
if(E06_F>=300){
  contador=contador+1
}
if(E06_G>=300){
  contador=contador+1
}
prob_E06_300mm<-contador/7
#prob_E06_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E07
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E07_A>=300){
  contador=contador+1
}
if(E07_B>=300){
  contador=contador+1
}
if(E07_C>=300){
  contador=contador+1
}
if(E07_D>=300){
  contador=contador+1
}
if(E07_E>=300){
  contador=contador+1
}
if(E07_F>=300){
  contador=contador+1
}
if(E07_G>=300){
  contador=contador+1
}
prob_E07_300mm<-contador/7
#prob_E07_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E08
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E08_A>=300){
  contador=contador+1
}
if(E08_B>=300){
  contador=contador+1
}
if(E08_C>=300){
  contador=contador+1
}
if(E08_D>=300){
  contador=contador+1
}
if(E08_E>=300){
  contador=contador+1
}
if(E08_F>=300){
  contador=contador+1
}
if(E08_G>=300){
  contador=contador+1
}
prob_E08_300mm<-contador/7
#prob_E08_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E09
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E09_A>=300){
  contador=contador+1
}
if(E09_B>=300){
  contador=contador+1
}
if(E09_C>=300){
  contador=contador+1
}
if(E09_D>=300){
  contador=contador+1
}
if(E09_E>=300){
  contador=contador+1
}
if(E09_F>=300){
  contador=contador+1
}
if(E09_G>=300){
  contador=contador+1
}
prob_E09_300mm<-contador/7
#prob_E09_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E10
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E10_A>=300){
  contador=contador+1
}
if(E10_B>=300){
  contador=contador+1
}
if(E10_C>=300){
  contador=contador+1
}
if(E10_D>=300){
  contador=contador+1
}
if(E10_E>=300){
  contador=contador+1
}
if(E10_F>=300){
  contador=contador+1
}
if(E10_G>=300){
  contador=contador+1
}
prob_E10_300mm<-contador/7
#prob_E10_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E11
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E11_A>=300){
  contador=contador+1
}
if(E11_B>=300){
  contador=contador+1
}
if(E11_C>=300){
  contador=contador+1
}
if(E11_D>=300){
  contador=contador+1
}
if(E11_E>=300){
  contador=contador+1
}
if(E11_F>=300){
  contador=contador+1
}
if(E11_G>=300){
  contador=contador+1
}
prob_E11_300mm<-contador/7
#prob_E11_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E12
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E12_A>=300){
  contador=contador+1
}
if(E12_B>=300){
  contador=contador+1
}
if(E12_C>=300){
  contador=contador+1
}
if(E12_D>=300){
  contador=contador+1
}
if(E12_E>=300){
  contador=contador+1
}
if(E12_F>=300){
  contador=contador+1
}
if(E12_G>=300){
  contador=contador+1
}
prob_E12_300mm<-contador/7
#prob_E12_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E13
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E13_A>=300){
  contador=contador+1
}
if(E13_B>=300){
  contador=contador+1
}
if(E13_C>=300){
  contador=contador+1
}
if(E13_D>=300){
  contador=contador+1
}
if(E13_E>=300){
  contador=contador+1
}
if(E13_F>=300){
  contador=contador+1
}
if(E13_G>=300){
  contador=contador+1
}
prob_E13_300mm<-contador/7
#prob_E13_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E14
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E14_A>=300){
  contador=contador+1
}
if(E14_B>=300){
  contador=contador+1
}
if(E14_C>=300){
  contador=contador+1
}
if(E14_D>=300){
  contador=contador+1
}
if(E14_E>=300){
  contador=contador+1
}
if(E14_F>=300){
  contador=contador+1
}
if(E14_G>=300){
  contador=contador+1
}
prob_E14_300mm<-contador/7
#prob_E14_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E15
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E15_A>=300){
  contador=contador+1
}
if(E15_B>=300){
  contador=contador+1
}
if(E15_C>=300){
  contador=contador+1
}
if(E15_D>=300){
  contador=contador+1
}
if(E15_E>=300){
  contador=contador+1
}
if(E15_F>=300){
  contador=contador+1
}
if(E15_G>=300){
  contador=contador+1
}
prob_E15_300mm<-contador/7
#prob_E15_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E16
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E16_A>=300){
  contador=contador+1
}
if(E16_B>=300){
  contador=contador+1
}
if(E16_C>=300){
  contador=contador+1
}
if(E16_D>=300){
  contador=contador+1
}
if(E16_E>=300){
  contador=contador+1
}
if(E16_F>=300){
  contador=contador+1
}
if(E16_G>=300){
  contador=contador+1
}
prob_E16_300mm<-contador/7
#prob_E16_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E17
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E17_A>=300){
  contador=contador+1
}
if(E17_B>=300){
  contador=contador+1
}
if(E17_C>=300){
  contador=contador+1
}
if(E17_D>=300){
  contador=contador+1
}
if(E17_E>=300){
  contador=contador+1
}
if(E17_F>=300){
  contador=contador+1
}
if(E17_G>=300){
  contador=contador+1
}
prob_E17_300mm<-contador/7
#prob_E17_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E18
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E18_A>=300){
  contador=contador+1
}
if(E18_B>=300){
  contador=contador+1
}
if(E18_C>=300){
  contador=contador+1
}
if(E18_D>=300){
  contador=contador+1
}
if(E18_E>=300){
  contador=contador+1
}
if(E18_F>=300){
  contador=contador+1
}
if(E18_G>=300){
  contador=contador+1
}
prob_E18_300mm<-contador/7
#prob_E18_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E19
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E19_A>=300){
  contador=contador+1
}
if(E19_B>=300){
  contador=contador+1
}
if(E19_C>=300){
  contador=contador+1
}
if(E19_D>=300){
  contador=contador+1
}
if(E19_E>=300){
  contador=contador+1
}
if(E19_F>=300){
  contador=contador+1
}
if(E19_G>=300){
  contador=contador+1
}
prob_E19_300mm<-contador/7
#prob_E19_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E20
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E20_A>=300){
  contador=contador+1
}
if(E20_B>=300){
  contador=contador+1
}
if(E20_C>=300){
  contador=contador+1
}
if(E20_D>=300){
  contador=contador+1
}
if(E20_E>=300){
  contador=contador+1
}
if(E20_F>=300){
  contador=contador+1
}
if(E20_G>=300){
  contador=contador+1
}
prob_E20_300mm<-contador/7
#prob_E20_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E21
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E21_A>=300){
  contador=contador+1
}
if(E21_B>=300){
  contador=contador+1
}
if(E21_C>=300){
  contador=contador+1
}
if(E21_D>=300){
  contador=contador+1
}
if(E21_E>=300){
  contador=contador+1
}
if(E21_F>=300){
  contador=contador+1
}
if(E21_G>=300){
  contador=contador+1
}
prob_E21_300mm<-contador/7
#prob_E21_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E22
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E22_A>=300){
  contador=contador+1
}
if(E22_B>=300){
  contador=contador+1
}
if(E22_C>=300){
  contador=contador+1
}
if(E22_D>=300){
  contador=contador+1
}
if(E22_E>=300){
  contador=contador+1
}
if(E22_F>=300){
  contador=contador+1
}
if(E22_G>=300){
  contador=contador+1
}
prob_E22_300mm<-contador/7
#prob_E22_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E23
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E23_A>=300){
  contador=contador+1
}
if(E23_B>=300){
  contador=contador+1
}
if(E23_C>=300){
  contador=contador+1
}
if(E23_D>=300){
  contador=contador+1
}
if(E23_E>=300){
  contador=contador+1
}
if(E23_F>=300){
  contador=contador+1
}
if(E23_G>=300){
  contador=contador+1
}
prob_E23_300mm<-contador/7
#prob_E23_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E24
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E24_A>=300){
  contador=contador+1
}
if(E24_B>=300){
  contador=contador+1
}
if(E24_C>=300){
  contador=contador+1
}
if(E24_D>=300){
  contador=contador+1
}
if(E24_E>=300){
  contador=contador+1
}
if(E24_F>=300){
  contador=contador+1
}
if(E24_G>=300){
  contador=contador+1
}
prob_E24_300mm<-contador/7
#prob_E24_300mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 300mm                               E25
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E25_A>=300){
  contador=contador+1
}
if(E25_B>=300){
  contador=contador+1
}
if(E25_C>=300){
  contador=contador+1
}
if(E25_D>=300){
  contador=contador+1
}
if(E25_E>=300){
  contador=contador+1
}
if(E25_F>=300){
  contador=contador+1
}
if(E25_G>=300){
  contador=contador+1
}
prob_E25_300mm<-contador/7
#prob_E25_300mm
#******************************************************************************************
#******************************************************************************************

###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E01
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E01_A>=500){
  contador=contador+1
}
if(E01_B>=500){
  contador=contador+1
}
if(E01_C>=500){
  contador=contador+1
}
if(E01_D>=500){
  contador=contador+1
}
if(E01_E>=500){
  contador=contador+1
}
if(E01_F>=500){
  contador=contador+1
}
if(E01_G>=500){
  contador=contador+1
}
prob_E01_500mm<-contador/7
#prob_E01_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E02
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E02_A>=500){
  contador=contador+1
}
if(E02_B>=500){
  contador=contador+1
}
if(E02_C>=500){
  contador=contador+1
}
if(E02_D>=500){
  contador=contador+1
}
if(E02_E>=500){
  contador=contador+1
}
if(E02_F>=500){
  contador=contador+1
}
if(E02_G>=500){
  contador=contador+1
}
prob_E02_500mm<-contador/7
#prob_E02_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E03
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E03_A>=500){
  contador=contador+1
}
if(E03_B>=500){
  contador=contador+1
}
if(E03_C>=500){
  contador=contador+1
}
if(E03_D>=500){
  contador=contador+1
}
if(E03_E>=500){
  contador=contador+1
}
if(E03_F>=500){
  contador=contador+1
}
if(E03_G>=500){
  contador=contador+1
}
prob_E03_500mm<-contador/7
#prob_E03_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E04
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E04_A>=500){
  contador=contador+1
}
if(E04_B>=500){
  contador=contador+1
}
if(E04_C>=500){
  contador=contador+1
}
if(E04_D>=500){
  contador=contador+1
}
if(E04_E>=500){
  contador=contador+1
}
if(E04_F>=500){
  contador=contador+1
}
if(E04_G>=500){
  contador=contador+1
}
prob_E04_500mm<-contador/7
#prob_E04_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E05
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E05_A>=500){
  contador=contador+1
}
if(E05_B>=500){
  contador=contador+1
}
if(E05_C>=500){
  contador=contador+1
}
if(E05_D>=500){
  contador=contador+1
}
if(E05_E>=500){
  contador=contador+1
}
if(E05_F>=500){
  contador=contador+1
}
if(E05_G>=500){
  contador=contador+1
}
prob_E05_500mm<-contador/7
#prob_E05_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E06
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E06_A>=500){
  contador=contador+1
}
if(E06_B>=500){
  contador=contador+1
}
if(E06_C>=500){
  contador=contador+1
}
if(E06_D>=500){
  contador=contador+1
}
if(E06_E>=500){
  contador=contador+1
}
if(E06_F>=500){
  contador=contador+1
}
if(E06_G>=500){
  contador=contador+1
}
prob_E06_500mm<-contador/7
#prob_E06_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E07
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E07_A>=500){
  contador=contador+1
}
if(E07_B>=500){
  contador=contador+1
}
if(E07_C>=500){
  contador=contador+1
}
if(E07_D>=500){
  contador=contador+1
}
if(E07_E>=500){
  contador=contador+1
}
if(E07_F>=500){
  contador=contador+1
}
if(E07_G>=500){
  contador=contador+1
}
prob_E07_500mm<-contador/7
#prob_E07_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E08
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E08_A>=500){
  contador=contador+1
}
if(E08_B>=500){
  contador=contador+1
}
if(E08_C>=500){
  contador=contador+1
}
if(E08_D>=500){
  contador=contador+1
}
if(E08_E>=500){
  contador=contador+1
}
if(E08_F>=500){
  contador=contador+1
}
if(E08_G>=500){
  contador=contador+1
}
prob_E08_500mm<-contador/7
#prob_E08_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E09
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E09_A>=500){
  contador=contador+1
}
if(E09_B>=500){
  contador=contador+1
}
if(E09_C>=500){
  contador=contador+1
}
if(E09_D>=500){
  contador=contador+1
}
if(E09_E>=500){
  contador=contador+1
}
if(E09_F>=500){
  contador=contador+1
}
if(E09_G>=500){
  contador=contador+1
}
prob_E09_500mm<-contador/7
#prob_E09_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E10
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E10_A>=500){
  contador=contador+1
}
if(E10_B>=500){
  contador=contador+1
}
if(E10_C>=500){
  contador=contador+1
}
if(E10_D>=500){
  contador=contador+1
}
if(E10_E>=500){
  contador=contador+1
}
if(E10_F>=500){
  contador=contador+1
}
if(E10_G>=500){
  contador=contador+1
}
prob_E10_500mm<-contador/7
#prob_E10_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E11
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E11_A>=500){
  contador=contador+1
}
if(E11_B>=500){
  contador=contador+1
}
if(E11_C>=500){
  contador=contador+1
}
if(E11_D>=500){
  contador=contador+1
}
if(E11_E>=500){
  contador=contador+1
}
if(E11_F>=500){
  contador=contador+1
}
if(E11_G>=500){
  contador=contador+1
}
prob_E11_500mm<-contador/7
#prob_E11_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E12
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E12_A>=500){
  contador=contador+1
}
if(E12_B>=500){
  contador=contador+1
}
if(E12_C>=500){
  contador=contador+1
}
if(E12_D>=500){
  contador=contador+1
}
if(E12_E>=500){
  contador=contador+1
}
if(E12_F>=500){
  contador=contador+1
}
if(E12_G>=500){
  contador=contador+1
}
prob_E12_500mm<-contador/7
#prob_E12_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E13
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E13_A>=500){
  contador=contador+1
}
if(E13_B>=500){
  contador=contador+1
}
if(E13_C>=500){
  contador=contador+1
}
if(E13_D>=500){
  contador=contador+1
}
if(E13_E>=500){
  contador=contador+1
}
if(E13_F>=500){
  contador=contador+1
}
if(E13_G>=500){
  contador=contador+1
}
prob_E13_500mm<-contador/7
#prob_E13_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E14
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E14_A>=500){
  contador=contador+1
}
if(E14_B>=500){
  contador=contador+1
}
if(E14_C>=500){
  contador=contador+1
}
if(E14_D>=500){
  contador=contador+1
}
if(E14_E>=500){
  contador=contador+1
}
if(E14_F>=500){
  contador=contador+1
}
if(E14_G>=500){
  contador=contador+1
}
prob_E14_500mm<-contador/7
#prob_E14_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E15
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E15_A>=500){
  contador=contador+1
}
if(E15_B>=500){
  contador=contador+1
}
if(E15_C>=500){
  contador=contador+1
}
if(E15_D>=500){
  contador=contador+1
}
if(E15_E>=500){
  contador=contador+1
}
if(E15_F>=500){
  contador=contador+1
}
if(E15_G>=500){
  contador=contador+1
}
prob_E15_500mm<-contador/7
#prob_E15_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E16
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E16_A>=500){
  contador=contador+1
}
if(E16_B>=500){
  contador=contador+1
}
if(E16_C>=500){
  contador=contador+1
}
if(E16_D>=500){
  contador=contador+1
}
if(E16_E>=500){
  contador=contador+1
}
if(E16_F>=500){
  contador=contador+1
}
if(E16_G>=500){
  contador=contador+1
}
prob_E16_500mm<-contador/7
#prob_E16_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E17
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E17_A>=500){
  contador=contador+1
}
if(E17_B>=500){
  contador=contador+1
}
if(E17_C>=500){
  contador=contador+1
}
if(E17_D>=500){
  contador=contador+1
}
if(E17_E>=500){
  contador=contador+1
}
if(E17_F>=500){
  contador=contador+1
}
if(E17_G>=500){
  contador=contador+1
}
prob_E17_500mm<-contador/7
#prob_E17_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E18
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E18_A>=500){
  contador=contador+1
}
if(E18_B>=500){
  contador=contador+1
}
if(E18_C>=500){
  contador=contador+1
}
if(E18_D>=500){
  contador=contador+1
}
if(E18_E>=500){
  contador=contador+1
}
if(E18_F>=500){
  contador=contador+1
}
if(E18_G>=500){
  contador=contador+1
}
prob_E18_500mm<-contador/7
#prob_E18_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E19
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E19_A>=500){
  contador=contador+1
}
if(E19_B>=500){
  contador=contador+1
}
if(E19_C>=500){
  contador=contador+1
}
if(E19_D>=500){
  contador=contador+1
}
if(E19_E>=500){
  contador=contador+1
}
if(E19_F>=500){
  contador=contador+1
}
if(E19_G>=500){
  contador=contador+1
}
prob_E19_500mm<-contador/7
#prob_E19_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E20
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E20_A>=500){
  contador=contador+1
}
if(E20_B>=500){
  contador=contador+1
}
if(E20_C>=500){
  contador=contador+1
}
if(E20_D>=500){
  contador=contador+1
}
if(E20_E>=500){
  contador=contador+1
}
if(E20_F>=500){
  contador=contador+1
}
if(E20_G>=500){
  contador=contador+1
}
prob_E20_500mm<-contador/7
#prob_E20_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E21
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E21_A>=500){
  contador=contador+1
}
if(E21_B>=500){
  contador=contador+1
}
if(E21_C>=500){
  contador=contador+1
}
if(E21_D>=500){
  contador=contador+1
}
if(E21_E>=500){
  contador=contador+1
}
if(E21_F>=500){
  contador=contador+1
}
if(E21_G>=500){
  contador=contador+1
}
prob_E21_500mm<-contador/7
#prob_E21_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E22
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E22_A>=500){
  contador=contador+1
}
if(E22_B>=500){
  contador=contador+1
}
if(E22_C>=500){
  contador=contador+1
}
if(E22_D>=500){
  contador=contador+1
}
if(E22_E>=500){
  contador=contador+1
}
if(E22_F>=500){
  contador=contador+1
}
if(E22_G>=500){
  contador=contador+1
}
prob_E22_500mm<-contador/7
#prob_E22_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E23
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E23_A>=500){
  contador=contador+1
}
if(E23_B>=500){
  contador=contador+1
}
if(E23_C>=500){
  contador=contador+1
}
if(E23_D>=500){
  contador=contador+1
}
if(E23_E>=500){
  contador=contador+1
}
if(E23_F>=500){
  contador=contador+1
}
if(E23_G>=500){
  contador=contador+1
}
prob_E23_500mm<-contador/7
#prob_E23_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E24
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E24_A>=500){
  contador=contador+1
}
if(E24_B>=500){
  contador=contador+1
}
if(E24_C>=500){
  contador=contador+1
}
if(E24_D>=500){
  contador=contador+1
}
if(E24_E>=500){
  contador=contador+1
}
if(E24_F>=500){
  contador=contador+1
}
if(E24_G>=500){
  contador=contador+1
}
prob_E24_500mm<-contador/7
#prob_E24_500mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 500mm                               E25
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E25_A>=500){
  contador=contador+1
}
if(E25_B>=500){
  contador=contador+1
}
if(E25_C>=500){
  contador=contador+1
}
if(E25_D>=500){
  contador=contador+1
}
if(E25_E>=500){
  contador=contador+1
}
if(E25_F>=500){
  contador=contador+1
}
if(E25_G>=500){
  contador=contador+1
}
prob_E25_500mm<-contador/7
#prob_E25_500mm
#******************************************************************************************
#******************************************************************************************

###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################


###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E01
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E01_A>=50){
  contador=contador+1
}
if(E01_B>=50){
  contador=contador+1
}
if(E01_C>=50){
  contador=contador+1
}
if(E01_D>=50){
  contador=contador+1
}
if(E01_E>=50){
  contador=contador+1
}
if(E01_F>=50){
  contador=contador+1
}
if(E01_G>=50){
  contador=contador+1
}
prob_E01_50mm<-contador/7
#prob_E01_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E02
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E02_A>=50){
  contador=contador+1
}
if(E02_B>=50){
  contador=contador+1
}
if(E02_C>=50){
  contador=contador+1
}
if(E02_D>=50){
  contador=contador+1
}
if(E02_E>=50){
  contador=contador+1
}
if(E02_F>=50){
  contador=contador+1
}
if(E02_G>=50){
  contador=contador+1
}
prob_E02_50mm<-contador/7
#prob_E02_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E03
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E03_A>=50){
  contador=contador+1
}
if(E03_B>=50){
  contador=contador+1
}
if(E03_C>=50){
  contador=contador+1
}
if(E03_D>=50){
  contador=contador+1
}
if(E03_E>=50){
  contador=contador+1
}
if(E03_F>=50){
  contador=contador+1
}
if(E03_G>=50){
  contador=contador+1
}
prob_E03_50mm<-contador/7
#prob_E03_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E04
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E04_A>=50){
  contador=contador+1
}
if(E04_B>=50){
  contador=contador+1
}
if(E04_C>=50){
  contador=contador+1
}
if(E04_D>=50){
  contador=contador+1
}
if(E04_E>=50){
  contador=contador+1
}
if(E04_F>=50){
  contador=contador+1
}
if(E04_G>=50){
  contador=contador+1
}
prob_E04_50mm<-contador/7
#prob_E04_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E05
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E05_A>=50){
  contador=contador+1
}
if(E05_B>=50){
  contador=contador+1
}
if(E05_C>=50){
  contador=contador+1
}
if(E05_D>=50){
  contador=contador+1
}
if(E05_E>=50){
  contador=contador+1
}
if(E05_F>=50){
  contador=contador+1
}
if(E05_G>=50){
  contador=contador+1
}
prob_E05_50mm<-contador/7
#prob_E05_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E06
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E06_A>=50){
  contador=contador+1
}
if(E06_B>=50){
  contador=contador+1
}
if(E06_C>=50){
  contador=contador+1
}
if(E06_D>=50){
  contador=contador+1
}
if(E06_E>=50){
  contador=contador+1
}
if(E06_F>=50){
  contador=contador+1
}
if(E06_G>=50){
  contador=contador+1
}
prob_E06_50mm<-contador/7
#prob_E06_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E07
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E07_A>=50){
  contador=contador+1
}
if(E07_B>=50){
  contador=contador+1
}
if(E07_C>=50){
  contador=contador+1
}
if(E07_D>=50){
  contador=contador+1
}
if(E07_E>=50){
  contador=contador+1
}
if(E07_F>=50){
  contador=contador+1
}
if(E07_G>=50){
  contador=contador+1
}
prob_E07_50mm<-contador/7
#prob_E07_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E08
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E08_A>=50){
  contador=contador+1
}
if(E08_B>=50){
  contador=contador+1
}
if(E08_C>=50){
  contador=contador+1
}
if(E08_D>=50){
  contador=contador+1
}
if(E08_E>=50){
  contador=contador+1
}
if(E08_F>=50){
  contador=contador+1
}
if(E08_G>=50){
  contador=contador+1
}
prob_E08_50mm<-contador/7
#prob_E08_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E09
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E09_A>=50){
  contador=contador+1
}
if(E09_B>=50){
  contador=contador+1
}
if(E09_C>=50){
  contador=contador+1
}
if(E09_D>=50){
  contador=contador+1
}
if(E09_E>=50){
  contador=contador+1
}
if(E09_F>=50){
  contador=contador+1
}
if(E09_G>=50){
  contador=contador+1
}
prob_E09_50mm<-contador/7
#prob_E09_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E10
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E10_A>=50){
  contador=contador+1
}
if(E10_B>=50){
  contador=contador+1
}
if(E10_C>=50){
  contador=contador+1
}
if(E10_D>=50){
  contador=contador+1
}
if(E10_E>=50){
  contador=contador+1
}
if(E10_F>=50){
  contador=contador+1
}
if(E10_G>=50){
  contador=contador+1
}
prob_E10_50mm<-contador/7
#prob_E10_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E11
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E11_A>=50){
  contador=contador+1
}
if(E11_B>=50){
  contador=contador+1
}
if(E11_C>=50){
  contador=contador+1
}
if(E11_D>=50){
  contador=contador+1
}
if(E11_E>=50){
  contador=contador+1
}
if(E11_F>=50){
  contador=contador+1
}
if(E11_G>=50){
  contador=contador+1
}
prob_E11_50mm<-contador/7
#prob_E11_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E12
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E12_A>=50){
  contador=contador+1
}
if(E12_B>=50){
  contador=contador+1
}
if(E12_C>=50){
  contador=contador+1
}
if(E12_D>=50){
  contador=contador+1
}
if(E12_E>=50){
  contador=contador+1
}
if(E12_F>=50){
  contador=contador+1
}
if(E12_G>=50){
  contador=contador+1
}
prob_E12_50mm<-contador/7
#prob_E12_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E13
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E13_A>=50){
  contador=contador+1
}
if(E13_B>=50){
  contador=contador+1
}
if(E13_C>=50){
  contador=contador+1
}
if(E13_D>=50){
  contador=contador+1
}
if(E13_E>=50){
  contador=contador+1
}
if(E13_F>=50){
  contador=contador+1
}
if(E13_G>=50){
  contador=contador+1
}
prob_E13_50mm<-contador/7
#prob_E13_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E14
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E14_A>=50){
  contador=contador+1
}
if(E14_B>=50){
  contador=contador+1
}
if(E14_C>=50){
  contador=contador+1
}
if(E14_D>=50){
  contador=contador+1
}
if(E14_E>=50){
  contador=contador+1
}
if(E14_F>=50){
  contador=contador+1
}
if(E14_G>=50){
  contador=contador+1
}
prob_E14_50mm<-contador/7
#prob_E14_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E15
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E15_A>=50){
  contador=contador+1
}
if(E15_B>=50){
  contador=contador+1
}
if(E15_C>=50){
  contador=contador+1
}
if(E15_D>=50){
  contador=contador+1
}
if(E15_E>=50){
  contador=contador+1
}
if(E15_F>=50){
  contador=contador+1
}
if(E15_G>=50){
  contador=contador+1
}
prob_E15_50mm<-contador/7
#prob_E15_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E16
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E16_A>=50){
  contador=contador+1
}
if(E16_B>=50){
  contador=contador+1
}
if(E16_C>=50){
  contador=contador+1
}
if(E16_D>=50){
  contador=contador+1
}
if(E16_E>=50){
  contador=contador+1
}
if(E16_F>=50){
  contador=contador+1
}
if(E16_G>=50){
  contador=contador+1
}
prob_E16_50mm<-contador/7
#prob_E16_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E17
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E17_A>=50){
  contador=contador+1
}
if(E17_B>=50){
  contador=contador+1
}
if(E17_C>=50){
  contador=contador+1
}
if(E17_D>=50){
  contador=contador+1
}
if(E17_E>=50){
  contador=contador+1
}
if(E17_F>=50){
  contador=contador+1
}
if(E17_G>=50){
  contador=contador+1
}
prob_E17_50mm<-contador/7
#prob_E17_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E18
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E18_A>=50){
  contador=contador+1
}
if(E18_B>=50){
  contador=contador+1
}
if(E18_C>=50){
  contador=contador+1
}
if(E18_D>=50){
  contador=contador+1
}
if(E18_E>=50){
  contador=contador+1
}
if(E18_F>=50){
  contador=contador+1
}
if(E18_G>=50){
  contador=contador+1
}
prob_E18_50mm<-contador/7
#prob_E18_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E19
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E19_A>=50){
  contador=contador+1
}
if(E19_B>=50){
  contador=contador+1
}
if(E19_C>=50){
  contador=contador+1
}
if(E19_D>=50){
  contador=contador+1
}
if(E19_E>=50){
  contador=contador+1
}
if(E19_F>=50){
  contador=contador+1
}
if(E19_G>=50){
  contador=contador+1
}
prob_E19_50mm<-contador/7
#prob_E19_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E20
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E20_A>=50){
  contador=contador+1
}
if(E20_B>=50){
  contador=contador+1
}
if(E20_C>=50){
  contador=contador+1
}
if(E20_D>=50){
  contador=contador+1
}
if(E20_E>=50){
  contador=contador+1
}
if(E20_F>=50){
  contador=contador+1
}
if(E20_G>=50){
  contador=contador+1
}
prob_E20_50mm<-contador/7
#prob_E20_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E21
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E21_A>=50){
  contador=contador+1
}
if(E21_B>=50){
  contador=contador+1
}
if(E21_C>=50){
  contador=contador+1
}
if(E21_D>=50){
  contador=contador+1
}
if(E21_E>=50){
  contador=contador+1
}
if(E21_F>=50){
  contador=contador+1
}
if(E21_G>=50){
  contador=contador+1
}
prob_E21_50mm<-contador/7
#prob_E21_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E22
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E22_A>=50){
  contador=contador+1
}
if(E22_B>=50){
  contador=contador+1
}
if(E22_C>=50){
  contador=contador+1
}
if(E22_D>=50){
  contador=contador+1
}
if(E22_E>=50){
  contador=contador+1
}
if(E22_F>=50){
  contador=contador+1
}
if(E22_G>=50){
  contador=contador+1
}
prob_E22_50mm<-contador/7
#prob_E22_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E23
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E23_A>=50){
  contador=contador+1
}
if(E23_B>=50){
  contador=contador+1
}
if(E23_C>=50){
  contador=contador+1
}
if(E23_D>=50){
  contador=contador+1
}
if(E23_E>=50){
  contador=contador+1
}
if(E23_F>=50){
  contador=contador+1
}
if(E23_G>=50){
  contador=contador+1
}
prob_E23_50mm<-contador/7
#prob_E23_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E24
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E24_A>=50){
  contador=contador+1
}
if(E24_B>=50){
  contador=contador+1
}
if(E24_C>=50){
  contador=contador+1
}
if(E24_D>=50){
  contador=contador+1
}
if(E24_E>=50){
  contador=contador+1
}
if(E24_F>=50){
  contador=contador+1
}
if(E24_G>=50){
  contador=contador+1
}
prob_E24_50mm<-contador/7
#prob_E24_50mm
#******************************************************************************************
#******************************************************************************************

#******************************************************************************************
#******************************************************************************************
#--------- Establece la probabilidad de 50mm                               E25
#******************************************************************************************
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(E25_A>=50){
  contador=contador+1
}
if(E25_B>=50){
  contador=contador+1
}
if(E25_C>=50){
  contador=contador+1
}
if(E25_D>=50){
  contador=contador+1
}
if(E25_E>=50){
  contador=contador+1
}
if(E25_F>=50){
  contador=contador+1
}
if(E25_G>=50){
  contador=contador+1
}
prob_E25_50mm<-contador/7
#prob_E25_50mm
#******************************************************************************************
#******************************************************************************************

###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################

#Crea vector prob de mas de 50mm
probabilidad_50mm<-c(prob_E01_50mm,prob_E02_50mm,prob_E03_50mm,prob_E04_50mm,prob_E05_50mm,prob_E06_50mm,
                      prob_E07_50mm,prob_E08_50mm,prob_E09_50mm,prob_E10_50mm,prob_E11_50mm,prob_E12_50mm,
                      prob_E13_50mm,prob_E14_50mm,prob_E15_50mm,prob_E16_50mm,prob_E17_50mm,prob_E18_50mm,
                      prob_E19_50mm,prob_E20_50mm,prob_E21_50mm,prob_E22_50mm,prob_E23_50mm,prob_E24_50mm,
                      prob_E25_50mm)


#Crea vector prob de mas de 100mm
probabilidad_100mm<-c(prob_E01_100mm,prob_E02_100mm,prob_E03_100mm,prob_E04_100mm,prob_E05_100mm,prob_E06_100mm,
                      prob_E07_100mm,prob_E08_100mm,prob_E09_100mm,prob_E10_100mm,prob_E11_100mm,prob_E12_100mm,
                      prob_E13_100mm,prob_E14_100mm,prob_E15_100mm,prob_E16_100mm,prob_E17_100mm,prob_E18_100mm,
                      prob_E19_100mm,prob_E20_100mm,prob_E21_100mm,prob_E22_100mm,prob_E23_100mm,prob_E24_100mm,
                      prob_E25_100mm)

#Crea vector prob de mas de 300mm
probabilidad_300mm<-c(prob_E01_300mm,prob_E02_300mm,prob_E03_300mm,prob_E04_300mm,prob_E05_300mm,prob_E06_300mm,
                      prob_E07_300mm,prob_E08_300mm,prob_E09_300mm,prob_E10_300mm,prob_E11_300mm,prob_E12_300mm,
                      prob_E13_300mm,prob_E14_300mm,prob_E15_300mm,prob_E16_300mm,prob_E17_300mm,prob_E18_300mm,
                      prob_E19_300mm,prob_E20_300mm,prob_E21_300mm,prob_E22_300mm,prob_E23_300mm,prob_E24_300mm,
                      prob_E25_300mm)

#Crea vector prob de mas de 500mm
probabilidad_500mm<-c(prob_E01_500mm,prob_E02_500mm,prob_E03_500mm,prob_E04_500mm,prob_E05_500mm,prob_E06_500mm,
                      prob_E07_500mm,prob_E08_500mm,prob_E09_500mm,prob_E10_500mm,prob_E11_500mm,prob_E12_500mm,
                      prob_E13_500mm,prob_E14_500mm,prob_E15_500mm,prob_E16_500mm,prob_E17_500mm,prob_E18_500mm,
                      prob_E19_500mm,prob_E20_500mm,prob_E21_500mm,prob_E22_500mm,prob_E23_500mm,prob_E24_500mm,
                      prob_E25_500mm)



#Genera data frame con los datos obtenidos.

resultados<-data.frame(probabilidad_50mm,probabilidad_100mm,probabilidad_300mm,probabilidad_500mm)

#Exporta a excel los resultados
write.xlsx(resultados, "c:/R/probabilidades_72horas.xlsx")