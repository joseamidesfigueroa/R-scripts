#GENERA PRONOSTICO DE LLUVIA ACUMULA MENSUAL AJUSTADA CON ERROR PROMEDIO MENSUAL DEL MODELO VS CHIRPS (LINEA BASE)
#POR JUAN JOSE AMIDES FIGUEROA URBANO , EL SALVADOR, 12 DE NOVIEMBRE 2018. -- joseamidesfigueroa@gmail.com

#------------------------------------------------------------------------------------------------------------
#Carga librerias necesarias
library(raster)
library(rgdal)
library(rgeos)
library(crs)
library(sp)
library(xlsx)



#Carga shapefile
mapa <- readOGR(dsn=path.expand("D:/R/shape"),layer="ESA_CA_wgs84")

#Define algunas variables para ser usadas
setwd("D:/R/trabajo")

#Remueve todos los tiff antiguos
#lista<-list.files()
#lista
#file.remove(lista)

#Obtiene la hora del sistema para decidir que dato bajar.
#Sys.time()

#Ciclo GFS a usar
#ciclo="06"

#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#Baja los archivos corresponidentes
url1="http://srt.marn.gob.sv/salidasgfs/"
url2=ciclo
url3="/geotiff/"
url=paste(url1,url2,url3, sep = "")
fuente<-read_html("http://srt.marn.gob.sv/salidasgfs/12/geotiff/")%>% html_nodes("a") %>% html_text(trim = T)
limite<-length(fuente)

for (i in 6:limite) {

archivo<-paste(url,fuente[i], sep="")    
archivo
destino=fuente[i]
destino
#download.file(url = archivo,destfile =destino)
}

#########################################################################################################################
#########################################################################################################################
#########################################################################################################################

#Define la estación del aeropuerto de Ilopango.
Ilopango<-cbind(-89.118313,13.699318)
Ilopango_360<-cbind(270.881687,13.699318)

#------------------------------------------------------>
#carga los rasters en variables de R
vv_200_gfs<-c(raster("vv_200_1.tif"),raster("vv_200_2.tif"),raster("vv_200_3.tif"),raster("vv_200_4.tif"),raster("vv_200_5.tif"),
              raster("vv_200_6.tif"),raster("vv_200_7.tif"),raster("vv_200_8.tif"),raster("vv_200_9.tif"),raster("vv_200_10.tif"),
              raster("vv_200_11.tif"),raster("vv_200_12.tif"))

vv_200_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  vv_200_gfs_Ilopango[[i]]<-extract(vv_200_gfs[[i]],Ilopango_360, method='bilinear')
  signif((vv_200_gfs_Ilopango[[i]]),digits =4)
}


#------------------------------------------------------>

#------------------------------------------------------>
#carga los rasters en variables de R
wd_200_gfs<-c(raster("wd_200_1.tif"),raster("wd_200_2.tif"),raster("wd_200_3.tif"),raster("wd_200_4.tif"),raster("wd_200_5.tif"),
              raster("wd_200_6.tif"),raster("wd_200_7.tif"),raster("wd_200_8.tif"),raster("wd_200_9.tif"),raster("wd_200_10.tif"),
              raster("wd_200_11.tif"),raster("wd_200_12.tif"))

wd_200_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  wd_200_gfs_Ilopango[[i]]<-extract(wd_200_gfs[[i]],Ilopango_360, method='bilinear')
  signif((wd_200_gfs_Ilopango[[i]]),digits =4)
}
#------------------------------------------------------>

#------------------------------------------------------>
#carga los rasters en variables de R
vv_500_gfs<-c(raster("vv_500_1.tif"),raster("vv_500_2.tif"),raster("vv_500_3.tif"),raster("vv_500_4.tif"),raster("vv_500_5.tif"),
              raster("vv_500_6.tif"),raster("vv_500_7.tif"),raster("vv_500_8.tif"),raster("vv_500_9.tif"),raster("vv_500_10.tif"),
              raster("vv_500_11.tif"),raster("vv_500_12.tif"))

vv_500_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  vv_500_gfs_Ilopango[[i]]<-extract(vv_500_gfs[[i]],Ilopango_360, method='bilinear')
  signif((vv_500_gfs_Ilopango[[i]]),digits =4)
}

#------------------------------------------------------>

#------------------------------------------------------>
#carga los rasters en variables de R
wd_500_gfs<-c(raster("wd_500_1.tif"),raster("wd_500_2.tif"),raster("wd_500_3.tif"),raster("wd_500_4.tif"),raster("wd_500_5.tif"),
              raster("wd_500_6.tif"),raster("wd_500_7.tif"),raster("wd_500_8.tif"),raster("wd_500_9.tif"),raster("wd_500_10.tif"),
              raster("wd_500_11.tif"),raster("wd_500_12.tif"))

wd_500_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  wd_500_gfs_Ilopango[[i]]<-extract(wd_500_gfs[[i]],Ilopango_360, method='bilinear')
  signif((wd_500_gfs_Ilopango[[i]]),digits =4)
}
#------------------------------------------------------>

#------------------------------------------------------>
#carga los rasters en variables de R
vv_850_gfs<-c(raster("vv_850_1.tif"),raster("vv_850_2.tif"),raster("vv_850_3.tif"),raster("vv_850_4.tif"),raster("vv_850_5.tif"),
              raster("vv_850_6.tif"),raster("vv_850_7.tif"),raster("vv_850_8.tif"),raster("vv_850_9.tif"),raster("vv_850_10.tif"),
              raster("vv_850_11.tif"),raster("vv_850_12.tif"))

vv_850_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  vv_850_gfs_Ilopango[[i]]<-extract(vv_850_gfs[[i]],Ilopango_360, method='bilinear')
  signif((vv_850_gfs_Ilopango[[i]]),digits =4)
}

#------------------------------------------------------>

#------------------------------------------------------>
#carga los rasters en variables de R
wd_850_gfs<-c(raster("wd_850_1.tif"),raster("wd_850_2.tif"),raster("wd_850_3.tif"),raster("wd_850_4.tif"),raster("wd_850_5.tif"),
              raster("wd_850_6.tif"),raster("wd_850_7.tif"),raster("wd_850_8.tif"),raster("wd_850_9.tif"),raster("wd_850_10.tif"),
              raster("wd_850_11.tif"),raster("wd_850_12.tif"))

wd_850_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  wd_850_gfs_Ilopango[[i]]<-extract(wd_850_gfs[[i]],Ilopango_360, method='bilinear')
  signif((wd_850_gfs_Ilopango[[i]]),digits =4)
}
#------------------------------------------------------>

#------------------------------------------------------>
#carga los rasters en variables de R
vv_1000_gfs<-c(raster("vv_1000_1.tif"),raster("vv_1000_2.tif"),raster("vv_1000_3.tif"),raster("vv_1000_4.tif"),raster("vv_1000_5.tif"),
              raster("vv_1000_6.tif"),raster("vv_1000_7.tif"),raster("vv_1000_8.tif"),raster("vv_1000_9.tif"),raster("vv_1000_10.tif"),
              raster("vv_1000_11.tif"),raster("vv_1000_12.tif"))

vv_1000_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  vv_1000_gfs_Ilopango[[i]]<-extract(vv_1000_gfs[[i]],Ilopango_360, method='bilinear')
  signif((vv_1000_gfs_Ilopango[[i]]),digits =4)
}

#------------------------------------------------------>

#------------------------------------------------------>
#carga los rasters en variables de R
wd_1000_gfs<-c(raster("wd_1000_1.tif"),raster("wd_1000_2.tif"),raster("wd_1000_3.tif"),raster("wd_1000_4.tif"),raster("wd_1000_5.tif"),
              raster("wd_1000_6.tif"),raster("wd_1000_7.tif"),raster("wd_1000_8.tif"),raster("wd_1000_9.tif"),raster("wd_1000_10.tif"),
              raster("wd_1000_11.tif"),raster("wd_1000_12.tif"))

wd_1000_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  wd_1000_gfs_Ilopango[[i]]<-extract(wd_1000_gfs[[i]],Ilopango_360, method='bilinear')
  signif((wd_1000_gfs_Ilopango[[i]]),digits =4)
}
#------------------------------------------------------>

#------------------------------------------------------>
#carga los rasters en variables de R
vv_10m_gfs<-c(raster("vv_10m_1.tif"),raster("vv_10m_2.tif"),raster("vv_10m_3.tif"),raster("vv_10m_4.tif"),raster("vv_10m_5.tif"),
              raster("vv_10m_6.tif"),raster("vv_10m_7.tif"),raster("vv_10m_8.tif"),raster("vv_10m_9.tif"),raster("vv_10m_10.tif"),
              raster("vv_10m_11.tif"),raster("vv_10m_12.tif"))

vv_10m_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  vv_10m_gfs_Ilopango[[i]]<-extract(vv_10m_gfs[[i]],Ilopango_360, method='bilinear')
  signif((vv_10m_gfs_Ilopango[[i]]),digits =4)
}

#------------------------------------------------------>

#------------------------------------------------------>
#carga los rasters en variables de R
wd_10m_gfs<-c(raster("wd_10m_1.tif"),raster("wd_10m_2.tif"),raster("wd_10m_3.tif"),raster("wd_10m_4.tif"),raster("wd_10m_5.tif"),
              raster("wd_10m_6.tif"),raster("wd_10m_7.tif"),raster("wd_10m_8.tif"),raster("wd_10m_9.tif"),raster("wd_10m_10.tif"),
              raster("wd_10m_11.tif"),raster("wd_10m_12.tif"))

wd_10m_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  wd_10m_gfs_Ilopango[[i]]<-extract(wd_10m_gfs[[i]],Ilopango_360, method='bilinear')
  signif((wd_10m_gfs_Ilopango[[i]]),digits =4)
}
#------------------------------------------------------>


#------------------------------------------------------>
#carga los rasters en variables de R
presion_gfs<-c(raster("presion_gfs_1.tif"),raster("presion_gfs_2.tif"),raster("presion_gfs_3.tif"),raster("presion_gfs_4.tif"),raster("presion_gfs_5.tif"),
              raster("presion_gfs_6.tif"),raster("presion_gfs_7.tif"),raster("presion_gfs_8.tif"),raster("presion_gfs_9.tif"),raster("presion_gfs_10.tif"),
              raster("presion_gfs_11.tif"),raster("presion_gfs_12.tif"))

presion_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  presion_gfs_Ilopango[[i]]<-extract(presion_gfs[[i]],Ilopango_360, method='bilinear')
  signif((presion_gfs_Ilopango[[i]]),digits =4)
}
#------------------------------------------------------>

#------------------------------------------------------>
#carga los rasters en variables de R
temperatura_gfs<-c(raster("temperatura_gfs_1.tif"),raster("temperatura_gfs_2.tif"),raster("temperatura_gfs_3.tif"),raster("temperatura_gfs_4.tif"),raster("temperatura_gfs_5.tif"),
               raster("temperatura_gfs_6.tif"),raster("temperatura_gfs_7.tif"),raster("temperatura_gfs_8.tif"),raster("temperatura_gfs_9.tif"),raster("temperatura_gfs_10.tif"),
               raster("temperatura_gfs_11.tif"),raster("temperatura_gfs_12.tif"))

temperatura_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  temperatura_gfs_Ilopango[[i]]<-extract(temperatura_gfs[[i]],Ilopango_360, method='bilinear')
  signif((temperatura_gfs_Ilopango[[i]]),digits =4)
}
#------------------------------------------------------>

#------------------------------------------------------>
#carga los rasters en variables de R
humedad_2m_gfs<-c(raster("humedad_2m_gfs_1.tif"),raster("humedad_2m_gfs_2.tif"),raster("humedad_2m_gfs_3.tif"),raster("humedad_2m_gfs_4.tif"),raster("humedad_2m_gfs_5.tif"),
                   raster("humedad_2m_gfs_6.tif"),raster("humedad_2m_gfs_7.tif"),raster("humedad_2m_gfs_8.tif"),raster("humedad_2m_gfs_9.tif"),raster("humedad_2m_gfs_10.tif"),
                   raster("humedad_2m_gfs_11.tif"),raster("humedad_2m_gfs_12.tif"))

humedad_2m_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  humedad_2m_gfs_Ilopango[[i]]<-extract(humedad_2m_gfs[[i]],Ilopango_360, method='bilinear')
  signif((humedad_2m_gfs_Ilopango[[i]]),digits =4)
}
#------------------------------------------------------>

#------------------------------------------------------>
#carga los rasters en variables de R
humedad_500_gfs<-c(raster("humedad_500_gfs_1.tif"),raster("humedad_500_gfs_2.tif"),raster("humedad_500_gfs_3.tif"),raster("humedad_500_gfs_4.tif"),raster("humedad_500_gfs_5.tif"),
                  raster("humedad_500_gfs_6.tif"),raster("humedad_500_gfs_7.tif"),raster("humedad_500_gfs_8.tif"),raster("humedad_500_gfs_9.tif"),raster("humedad_500_gfs_10.tif"),
                  raster("humedad_500_gfs_11.tif"),raster("humedad_500_gfs_12.tif"))

humedad_500_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  humedad_500_gfs_Ilopango[[i]]<-extract(humedad_500_gfs[[i]],Ilopango_360, method='bilinear')
  signif((humedad_500_gfs_Ilopango[[i]]),digits =4)
}
#------------------------------------------------------>

#------------------------------------------------------>
#carga los rasters en variables de R
humedad_850_gfs<-c(raster("humedad_850_gfs_1.tif"),raster("humedad_850_gfs_2.tif"),raster("humedad_850_gfs_3.tif"),raster("humedad_850_gfs_4.tif"),raster("humedad_850_gfs_5.tif"),
                   raster("humedad_850_gfs_6.tif"),raster("humedad_850_gfs_7.tif"),raster("humedad_850_gfs_8.tif"),raster("humedad_850_gfs_9.tif"),raster("humedad_850_gfs_10.tif"),
                   raster("humedad_850_gfs_11.tif"),raster("humedad_850_gfs_12.tif"))

humedad_850_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  humedad_850_gfs_Ilopango[[i]]<-extract(humedad_850_gfs[[i]],Ilopango_360, method='bilinear')
  signif((humedad_850_gfs_Ilopango[[i]]),digits =4)
}
#------------------------------------------------------>

#------------------------------------------------------>
#carga los rasters en variables de R
lluvia_gfs<-c(raster("lluvia_gfs_2.tif"),raster("lluvia_gfs_2.tif"),raster("lluvia_gfs_3.tif"),raster("lluvia_gfs_4.tif"),raster("lluvia_gfs_5.tif"),
                   raster("lluvia_gfs_6.tif"),raster("lluvia_gfs_7.tif"),raster("lluvia_gfs_8.tif"),raster("lluvia_gfs_9.tif"),raster("lluvia_gfs_10.tif"),
                   raster("lluvia_gfs_11.tif"),raster("lluvia_gfs_12.tif"))

lluvia_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  lluvia_gfs_Ilopango[[i]]<-extract(lluvia_gfs[[i]],Ilopango_360, method='bilinear')
  signif((lluvia_gfs_Ilopango[[i]]),digits =4)
}
#------------------------------------------------------>

#------------------------------------------------------>
#carga los rasters en variables de R
vorticidad_gfs<-c(raster("vorticidad_gfs_1.tif"),raster("vorticidad_gfs_2.tif"),raster("vorticidad_gfs_3.tif"),raster("vorticidad_gfs_4.tif"),raster("vorticidad_gfs_5.tif"),
              raster("vorticidad_gfs_6.tif"),raster("vorticidad_gfs_7.tif"),raster("vorticidad_gfs_8.tif"),raster("vorticidad_gfs_9.tif"),raster("vorticidad_gfs_10.tif"),
              raster("vorticidad_gfs_11.tif"),raster("vorticidad_gfs_12.tif"))

vorticidad_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  vorticidad_gfs_Ilopango[[i]]<-extract(vorticidad_gfs[[i]],Ilopango_360, method='bilinear')
  signif((lluvia_gfs_Ilopango[[i]]),digits =4)
}
#------------------------------------------------------>

#------------------------------------------------------>
#carga los rasters en variables de R
gdi_gfs<-c(raster("gdi_gfs_1.tif"),raster("gdi_gfs_2.tif"),raster("gdi_gfs_3.tif"),raster("gdi_gfs_4.tif"),raster("gdi_gfs_5.tif"),
                  raster("gdi_gfs_6.tif"),raster("gdi_gfs_7.tif"),raster("gdi_gfs_8.tif"),raster("gdi_gfs_9.tif"),raster("gdi_gfs_10.tif"),
                  raster("gdi_gfs_11.tif"),raster("gdi_gfs_12.tif"))

gdi_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  gdi_gfs_Ilopango[[i]]<-extract(gdi_gfs[[i]],Ilopango_360, method='bilinear')
  signif((gdi_gfs_Ilopango[[i]]),digits =4)
}
#------------------------------------------------------>

#------------------------------------------------------>
#carga los rasters en variables de R
lift_gfs<-c(raster("lift_gfs_1.tif"),raster("lift_gfs_2.tif"),raster("lift_gfs_3.tif"),raster("lift_gfs_4.tif"),raster("lift_gfs_5.tif"),
           raster("lift_gfs_6.tif"),raster("lift_gfs_7.tif"),raster("lift_gfs_8.tif"),raster("lift_gfs_9.tif"),raster("lift_gfs_10.tif"),
           raster("lift_gfs_11.tif"),raster("lift_gfs_12.tif"))

lift_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  lift_gfs_Ilopango[[i]]<-extract(lift_gfs[[i]],Ilopango_360, method='bilinear')
  signif((lift_gfs_Ilopango[[i]]),digits =4)
}
#------------------------------------------------------>

#------------------------------------------------------>
#carga los rasters en variables de R
cape_gfs<-c(raster("cape_gfs_1.tif"),raster("cape_gfs_2.tif"),raster("cape_gfs_3.tif"),raster("cape_gfs_4.tif"),raster("cape_gfs_5.tif"),
            raster("cape_gfs_6.tif"),raster("cape_gfs_7.tif"),raster("cape_gfs_8.tif"),raster("cape_gfs_9.tif"),raster("cape_gfs_10.tif"),
            raster("cape_gfs_11.tif"),raster("cape_gfs_12.tif"))

cape_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  cape_gfs_Ilopango[[i]]<-extract(cape_gfs[[i]],Ilopango_360, method='bilinear')
  signif((cape_gfs_Ilopango[[i]]),digits =4)
}
#------------------------------------------------------>

#------------------------------------------------------>
#carga los rasters en variables de R
pwat_gfs<-c(raster("pwat_gfs_1.tif"),raster("pwat_gfs_2.tif"),raster("pwat_gfs_3.tif"),raster("pwat_gfs_4.tif"),raster("pwat_gfs_5.tif"),
            raster("pwat_gfs_6.tif"),raster("pwat_gfs_7.tif"),raster("pwat_gfs_8.tif"),raster("pwat_gfs_9.tif"),raster("pwat_gfs_10.tif"),
            raster("pwat_gfs_11.tif"),raster("pwat_gfs_12.tif"))

pwat_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  pwat_gfs_Ilopango[[i]]<-extract(pwat_gfs[[i]],Ilopango_360, method='bilinear')
  signif((pwat_gfs_Ilopango[[i]]),digits =4)
}
#------------------------------------------------------>

#------------------------------------------------------>
#carga los rasters en variables de R
cin_gfs<-c(raster("cin_gfs_1.tif"),raster("cin_gfs_2.tif"),raster("cin_gfs_3.tif"),raster("cin_gfs_4.tif"),raster("cin_gfs_5.tif"),
            raster("cin_gfs_6.tif"),raster("cin_gfs_7.tif"),raster("cin_gfs_8.tif"),raster("cin_gfs_9.tif"),raster("cin_gfs_10.tif"),
            raster("cin_gfs_11.tif"),raster("cin_gfs_12.tif"))

cin_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  cin_gfs_Ilopango[[i]]<-extract(cin_gfs[[i]],Ilopango_360, method='bilinear')
  signif((cin_gfs_Ilopango[[i]]),digits =4)
}
#------------------------------------------------------>


#------------------------------------------------------>
#carga los rasters en variables de R
nubosidad_gfs<-c(raster("nubosidad_gfs_2.tif"),raster("nubosidad_gfs_2.tif"),raster("nubosidad_gfs_3.tif"),raster("nubosidad_gfs_4.tif"),raster("nubosidad_gfs_5.tif"),
           raster("nubosidad_gfs_6.tif"),raster("nubosidad_gfs_7.tif"),raster("nubosidad_gfs_8.tif"),raster("nubosidad_gfs_9.tif"),raster("nubosidad_gfs_10.tif"),
           raster("nubosidad_gfs_11.tif"),raster("nubosidad_gfs_12.tif"))

nubosidad_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  nubosidad_gfs_Ilopango[[i]]<-extract(nubosidad_gfs[[i]],Ilopango_360, method='bilinear')
  signif((nubosidad_gfs_Ilopango[[i]]),digits =4)
}
#------------------------------------------------------>

#------------------------------------------------------>
#carga los rasters en variables de R
uvb_gfs<-c(raster("uvb_gfs_2.tif"),raster("uvb_gfs_2.tif"),raster("uvb_gfs_3.tif"),raster("uvb_gfs_4.tif"),raster("uvb_gfs_5.tif"),
                 raster("uvb_gfs_6.tif"),raster("uvb_gfs_7.tif"),raster("uvb_gfs_8.tif"),raster("uvb_gfs_9.tif"),raster("uvb_gfs_10.tif"),
                 raster("uvb_gfs_11.tif"),raster("uvb_gfs_12.tif"))

uvb_gfs_Ilopango<-c(1:12)
for (i in 1:12){
  uvb_gfs_Ilopango[[i]]<-extract(uvb_gfs[[i]],Ilopango_360, method='bilinear')
  signif((uvb_gfs_Ilopango[[i]]),digits =4)
}
#------------------------------------------------------>


#************************************************************************************
#************************************************************************************
#************************************************************************************
#************************************************************************************
#Se crean vectores para agrupar variables

#Crea los tiempos de pronóstico
horas_pronostico<-c("F06","F12","F18","F24","F30","F36","F42","F48","F54","F60","F66","F72")

#------------------------------------------------------------------------------ Construye el meteograma
meteograma_Ilopango_360<-data.frame(horas_pronostico,vv_200_gfs_Ilopango,wd_200_gfs_Ilopango,vv_500_gfs_Ilopango,wd_500_gfs_Ilopango,vv_850_gfs_Ilopango,
                                    wd_850_gfs_Ilopango,vv_1000_gfs_Ilopango,wd_1000_gfs_Ilopango,vv_10m_gfs_Ilopango,wd_10m_gfs_Ilopango,vorticidad_gfs_Ilopango,
                                    presion_gfs_Ilopango,temperatura_gfs_Ilopango,humedad_500_gfs_Ilopango,humedad_850_gfs_Ilopango,humedad_2m_gfs_Ilopango,
                                    lluvia_gfs_Ilopango,gdi_gfs_Ilopango,lift_gfs_Ilopango,cape_gfs_Ilopango,pwat_gfs_Ilopango,cin_gfs_Ilopango,
                                    nubosidad_gfs_Ilopango,uvb_gfs_Ilopango)



names(meteograma_Ilopango_360)<-c("Horas pronostico","Velocidad Viento 200 MB","Direccion Viento 200 MB","Velocidad Viento 500 MB","Direccion Viento 500 MB",
                                  "Velocidad Viento 850 MB","Dirección Viento 850 MB","Velocidad Viento 1000 MB","Dirección Viento 1000MB","Velocidad Viento 10m",
                                  "Dirección Viento 10m","Vorticidad absoluta","Presión MB","Temperatura Celsius","Humedad relativa 500MB","Humedad relativa 850MB",
                                  "Humedad Relativa 2m","Precipitacíón mm","GDI","LIFT","CAPE","Agua precipitable","CIN","Nubosidad %","UV-B")
trans<-t(meteograma_Ilopango_360)


#------------------------------------------------------------------------------------------------------------
#Exporta a Excel

wb<-createWorkbook(type="xlsx")

CellStyle(wb, dataFormat=NULL, alignment=NULL,
          border=NULL, fill=NULL, font=NULL)

# Define some cell styles
#++++++++++++++++++++
# Title and sub title styles
TITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=16, 
                                   color="blue", isBold=TRUE, underline=1)
SUB_TITLE_STYLE <- CellStyle(wb) + 
  Font(wb,  heightInPoints=14, 
       isItalic=TRUE, isBold=FALSE)
# Styles for the data table row/column names
TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE)
TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
  Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
  Border(color="black", position=c("TOP", "BOTTOM"), 
         pen=c("BORDER_THIN", "BORDER_THICK")) 

# Create a new sheet in the workbook
sheet <- createSheet(wb, sheetName = "Bitacora Pronóstico GFS")

#++++++++++++++++++++++++
# Helper function to add titles
#++++++++++++++++++++++++
# - sheet : sheet object to contain the title
# - rowIndex : numeric value indicating the row to 
#contain the title
# - title : the text to use as title
# - titleStyle : style object to use for title
xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
  rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=1)
  setCellValue(sheetTitle[[1,1]], title)
  setCellStyle(sheetTitle[[1,1]], titleStyle)
}

# Add title
xlsx.addTitle(sheet, rowIndex=1, title="BITACORA DE PRONOSTICO CON GFS-AUTO",
              titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet, rowIndex=2, 
              title="CPM-GM-MARN",
              titleStyle = SUB_TITLE_STYLE)

# Add a table into a worksheet
#++++++++++++++++++++++++++++++++++++
addDataFrame(trans, sheet, startRow=3, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE, col.names = FALSE)
# Change column width
setColumnWidth(sheet, colIndex=c(1:ncol(state.x77)), colWidth=11)


saveWorkbook(wb,"D:/R/Bitacora_Prono48H_GFS.xlsx")

