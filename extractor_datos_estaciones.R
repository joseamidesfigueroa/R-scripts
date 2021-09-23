#Carga la función para extracción de datos desde las estaciones telemétricas en la base de datos de Vielca
source( "/home/arw/scripts/R/funciones/extraer.R")
#source( "/home/arw/scripts_R/funciones/extraer.R")
library(kableExtra)
library(tidyr)
library(leafletR)

#Define el directorio de trabajo
setwd("/home/arw/trabajo/")
dir.destino<-"/var/www/html/monitor_telem/mapa"
#setwd("/var/www/html/monitor_telem")

#Extrae datos (i= indice de la estacion, j = String del nombre de la estacion, k= Variable a consultar)

#Extra las coordenadas en el orden del indice
latitudes <- indice$V3[1:length(indice$V3)]
longitudes <- indice$V4[1:length(indice$V4)]


#Extrae la temperatura ambiente de todas las estaciones
#------------------------------------------------------------------------------------------------------------>
x<-data.frame("NA","NA","NA","NA")
names(x)<-c("Estacion","fecha","Hora","Variable")

#Ciclo para extracción de datos por consultas
j=1
for (i in id_estacion) {
  a<-extrae.1(i,nombre_estacion[j],"AT")
  x<-rbind.data.frame(a,x)
  j=j+1
}

#Genera un reverse del data frame resultante
x<-as.data.frame(purrr::map_df(x,rev))

#Genera un nuevo dataframe para convertirlo a GeoJSON
temperatura<-data.frame(x$Estacion[2:length(x$Estacion)],latitudes[2:length(latitudes)],longitudes[2:length(longitudes)],
              x$fecha[2:length(x$fecha)],x$Hora[2:length(x$Hora)],x$Variable[2:length(x$Variable)])
names(temperatura)<-c("Nombre","Lat","Lon","Fecha","Hora","Temperatura °C")

#Filtra para la tabla
x<-subset(x,x!="NA")
x<-na.omit(x)

tabla<-kable(x,format = "html", table.attr = "id=\"tabla_telem\" class=\"display\" width=\"40%\"", caption = "Temperatura ambiente °C",
             digits = 2)
cat(tabla, file="tabla_temp_ambiente")

#Filtra df para el mapa
temperatura<-subset(temperatura,temperatura!="NA")
temperatura<-na.omit(temperatura)

#Genera el mapa desde un dataframe
  dat_local<-toGeoJSON(data = temperatura, dest = tempdir())
  map2 <- leaflet(data=dat_local,dest = dir.destino, size = c("100%",500), popup=c("*"), label="Nombre")

#Limpia las variables
rm(x)

#------------------------------------------------------------------------------------------------------------>


#Extrae la temperatura punto de rocio de todas las estaciones
#------------------------------------------------------------------------------------------------------------>
x<-data.frame("NA","NA","NA","NA")
names(x)<-c("Estacion","fecha","Hora","Variable")
j=1
for (i in id_estacion) {
  a<-extrae.1(i,nombre_estacion[j],"DP")
  x<-rbind.data.frame(a,x)
  j=j+1
}

#Genera un reverse del data frame resultante
x<-as.data.frame(purrr::map_df(x,rev))

#Genera un nuevo dataframe para convertirlo a GeoJSON
rocio<-data.frame(x$Estacion[2:length(x$Estacion)],latitudes[2:length(latitudes)],longitudes[2:length(longitudes)],
              x$fecha[2:length(x$fecha)],x$Hora[2:length(x$Hora)],x$Variable[2:length(x$Variable)])
names(rocio)<-c("Nombre","Lat","Lon","Fecha","Hora","Punto rocio °C")

#Filtra para la tabla
x<-subset(x,x!="NA")
x<-na.omit(x)

tabla<-kable(x,format = "html", table.attr = "id=\"tabla_telem\" class=\"display\" width=\"40%\"", caption = "Temperatura punto rocio °C",
             digits = 2)
cat(tabla, file="tabla_punto_rocio")

#Filtra df para el mapa
rocio<-subset(rocio,rocio!="NA")
rocio<-na.omit(rocio)

#Genera el mapa desde un dataframe
  dat_local<-toGeoJSON(data = rocio, dest = tempdir())
  map2 <- leaflet(data=dat_local,dest = dir.destino, size = c("100%",500), popup=c("*"), label="Nombre")

#Limpia las variables
rm(x)

#------------------------------------------------------------------------------------------------------------>

#Extrae la humedad relativa de todas las estaciones
#------------------------------------------------------------------------------------------------------------>
x<-data.frame("NA","NA","NA","NA")
names(x)<-c("Estacion","fecha","Hora","Variable")
j=1
for (i in id_estacion) {
  a<-extrae.1(i,nombre_estacion[j],"RH")
  x<-rbind.data.frame(a,x)
  j=j+1
}

#Genera un reverse del data frame resultante
x<-as.data.frame(purrr::map_df(x,rev))

#Genera un nuevo dataframe para convertirlo a GeoJSON
humedad<-data.frame(x$Estacion[2:length(x$Estacion)],latitudes[2:length(latitudes)],longitudes[2:length(longitudes)],
              x$fecha[2:length(x$fecha)],x$Hora[2:length(x$Hora)],x$Variable[2:length(x$Variable)])
names(humedad)<-c("Nombre","Lat","Lon","Fecha","Hora","Humedad relativa %")

#Filtra para la tabla
x<-subset(x,x!="NA")
x<-na.omit(x)

tabla<-kable(x,format = "html", table.attr = "id=\"tabla_telem\" class=\"display\" width=\"40%\"", caption = "Humedad relativa %",
             digits = 2)
cat(tabla, file="tabla_humedad_relativa")

#Filtra df para el mapa
humedad<-subset(humedad,humedad!="NA")
humedad<-na.omit(humedad)

#Genera el mapa desde un dataframe
  dat_local<-toGeoJSON(data = humedad, dest = tempdir())
  map2 <- leaflet(data=dat_local,dest = dir.destino, size = c("100%",500), popup=c("*"), label="Nombre")

#Limpia las variables
rm(x)
#------------------------------------------------------------------------------------------------------------>

#Extrae la presión de todas las estaciones
#------------------------------------------------------------------------------------------------------------>
x<-data.frame("NA","NA","NA","NA")
names(x)<-c("Estacion","fecha","Hora","Variable")
j=1
for (i in id_estacion) {
  a<-extrae.1(i,nombre_estacion[j],"BP")
  x<-rbind.data.frame(a,x)
  j=j+1
}

#Genera un reverse del data frame resultante
x<-as.data.frame(purrr::map_df(x,rev))

#Genera un nuevo dataframe para convertirlo a GeoJSON
presion<-data.frame(x$Estacion[2:length(x$Estacion)],latitudes[2:length(latitudes)],longitudes[2:length(longitudes)],
              x$fecha[2:length(x$fecha)],x$Hora[2:length(x$Hora)],x$Variable[2:length(x$Variable)])
names(presion)<-c("Nombre","Lat","Lon","Fecha","Hora","Presi&oacute;n hPa")

#Filtra para la tabla
x<-subset(x,x!="NA")
x<-na.omit(x)
tabla<-kable(x,format = "html", table.attr = "id=\"tabla_telem\" class=\"display\" width=\"40%\"", caption = "Presi&oacute;n en superficie hPa",
             digits = 2)
cat(tabla, file="tabla_presion")

#Filtra df para el mapa
presion<-subset(presion,presion!="NA")
presion<-na.omit(presion)

#Genera el mapa desde un dataframe
  dat_local<-toGeoJSON(data = presion, dest = tempdir())
  map2 <- leaflet(data=dat_local,dest = dir.destino, size = c("100%",500), popup=c("*"), label="Nombre")

#Limpia las variables
rm(x)
#------------------------------------------------------------------------------------------------------------>

#Extrae la direccion de viento promedio
#------------------------------------------------------------------------------------------------------------>
x<-data.frame("NA","NA","NA","NA")
names(x)<-c("Estacion","fecha","Hora","Variable")
j=1
for (i in id_estacion) {
  a<-extrae.1(i,nombre_estacion[j],"DA")
  x<-rbind.data.frame(a,x)
  j=j+1
}

#Genera un reverse del data frame resultante
x<-as.data.frame(purrr::map_df(x,rev))

#Genera un nuevo dataframe para convertirlo a GeoJSON
dir.viento.prom<-data.frame(x$Estacion[2:length(x$Estacion)],latitudes[2:length(latitudes)],longitudes[2:length(longitudes)],
              x$fecha[2:length(x$fecha)],x$Hora[2:length(x$Hora)],x$Variable[2:length(x$Variable)])
names(dir.viento.prom)<-c("Nombre","Lat","Lon","Fecha","Hora","Direcci&oacute;n °")

#Filtra para la tabla
x<-subset(x,x!="NA")
x<-na.omit(x)

tabla<-kable(x,format = "html", table.attr = "id=\"tabla_telem\" class=\"display\" width=\"40%\"", caption = "Direcci&oacute;n de viento promedio °",
             digits = 2)
cat(tabla, file="tabla_direccion_viento_prom")

#Filtra df para el mapa
dir.viento.prom<-subset(dir.viento.prom,dir.viento.prom!="NA")
dir.viento.prom<-na.omit(dir.viento.prom)

#Genera el mapa desde un dataframe
  dat_local<-toGeoJSON(data = dir.viento.prom, dest = tempdir())
  map2 <- leaflet(data=dat_local,dest = dir.destino, size = c("100%",500), popup=c("*"), label="Nombre")

#Limpia las variables
rm(x)
#------------------------------------------------------------------------------------------------------------>

#Extrae la velodicad del viento promedio
#------------------------------------------------------------------------------------------------------------>
x<-data.frame("NA","NA","NA","NA")
names(x)<-c("Estacion","fecha","Hora","Variable")
j=1
for (i in id_estacion) {
  a<-extrae.1(i,nombre_estacion[j],"SA")
  x<-rbind.data.frame(a,x)
  j=j+1
}

#Genera un reverse del data frame resultante
x<-as.data.frame(purrr::map_df(x,rev))

#Genera un nuevo dataframe para convertirlo a GeoJSON
vel.viento.prom<-data.frame(x$Estacion[2:length(x$Estacion)],latitudes[2:length(latitudes)],longitudes[2:length(longitudes)],
              x$fecha[2:length(x$fecha)],x$Hora[2:length(x$Hora)],x$Variable[2:length(x$Variable)])
names(vel.viento.prom)<-c("Nombre","Lat","Lon","Fecha","Hora","Velocidad KT")

#Filtra para la tabla
x<-subset(x,x!="NA")
x<-na.omit(x)

tabla<-kable(x,format = "html", table.attr = "id=\"tabla_telem\" class=\"display\" width=\"40%\"", caption = "Velocidad de viento promedio KT",
             digits = 2)
cat(tabla, file="tabla_velocidad_viento_prom")

#Filtra df para el mapa
vel.viento.prom<-subset(vel.viento.prom,vel.viento.prom!="NA")
vel.viento.prom<-na.omit(vel.viento.prom)

#Genera el mapa desde un dataframe
  dat_local<-toGeoJSON(data = vel.viento.prom, dest = tempdir())
  map2 <- leaflet(data=dat_local,dest = dir.destino, size = c("100%",500), popup=c("*"), label="Nombre")

#Limpia las variables
rm(x)
#------------------------------------------------------------------------------------------------------------>

#Extrae la dirección del viento instantanea
#------------------------------------------------------------------------------------------------------------>
x<-data.frame("NA","NA","NA","NA")
names(x)<-c("Estacion","fecha","Hora","Variable")
j=1
for (i in id_estacion) {
  a<-extrae.1(i,nombre_estacion[j],"DI")
  x<-rbind.data.frame(a,x)
  j=j+1
}

#Genera un reverse del data frame resultante
x<-as.data.frame(purrr::map_df(x,rev))

#Genera un nuevo dataframe para convertirlo a GeoJSON
dir.viento.inst<-data.frame(x$Estacion[2:length(x$Estacion)],latitudes[2:length(latitudes)],longitudes[2:length(longitudes)],
              x$fecha[2:length(x$fecha)],x$Hora[2:length(x$Hora)],x$Variable[2:length(x$Variable)])
names(dir.viento.inst)<-c("Nombre","Lat","Lon","Fecha","Hora","Direcci&oacute;n °")

#Filtra para la tabla
x<-subset(x,x!="NA")
x<-na.omit(x)

tabla<-kable(x,format = "html", table.attr = "id=\"tabla_telem\" class=\"display\" width=\"40%\"", caption = "Direcci&oacute;n de viento instant&aacute;neo °",
             digits = 2)
cat(tabla, file="tabla_direccion_viento_inst")

#Filtra df para el mapa
dir.viento.inst<-subset(dir.viento.inst,dir.viento.inst!="NA")
dir.viento.inst<-na.omit(dir.viento.inst)

#Genera el mapa desde un dataframe
  dat_local<-toGeoJSON(data = dir.viento.inst, dest = tempdir())
  map2 <- leaflet(data=dat_local,dest = dir.destino, size = c("100%",500), popup=c("*"), label="Nombre")

#Limpia las variables
rm(x)
#------------------------------------------------------------------------------------------------------------>

#Extrae la velocidad del viento instantanea
#------------------------------------------------------------------------------------------------------------>
x<-data.frame("NA","NA","NA","NA")
names(x)<-c("Estacion","fecha","Hora","Variable")
j=1
for (i in id_estacion) {
  a<-extrae.1(i,nombre_estacion[j],"SI")
  x<-rbind.data.frame(a,x)
  j=j+1
}

#Genera un reverse del data frame resultante
x<-as.data.frame(purrr::map_df(x,rev))

#Genera un nuevo dataframe para convertirlo a GeoJSON
vel.viento.inst<-data.frame(x$Estacion[2:length(x$Estacion)],latitudes[2:length(latitudes)],longitudes[2:length(longitudes)],
              x$fecha[2:length(x$fecha)],x$Hora[2:length(x$Hora)],x$Variable[2:length(x$Variable)])
names(vel.viento.inst)<-c("Nombre","Lat","Lon","Fecha","Hora","Velocidad KT")

#Filtra para la tabla
x<-subset(x,x!="NA")
x<-na.omit(x)

tabla<-kable(x,format = "html", table.attr = "id=\"tabla_telem\" class=\"display\" width=\"40%\"", caption = "Velocidad de viento instant&aacute;neo KT",
             digits = 2)
cat(tabla, file="tabla_velocidad_viento_inst")

#Filtra df para el mapa
vel.viento.inst<-subset(vel.viento.inst,vel.viento.inst!="NA")
vel.viento.inst<-na.omit(vel.viento.inst)

#Genera el mapa desde un dataframe
  dat_local<-toGeoJSON(data = vel.viento.inst, dest = tempdir())
  map2 <- leaflet(data=dat_local,dest = dir.destino, size = c("100%",500), popup=c("*"), label="Nombre")

#Limpia las variables
rm(x)

#Extrae la velocidad del viento ráfaga
#------------------------------------------------------------------------------------------------------------>
x<-data.frame("NA","NA","NA","NA")
names(x)<-c("Estacion","fecha","Hora","Variable")
j=1
for (i in id_estacion) {
  a<-extrae.1(i,nombre_estacion[j],"RA")
  x<-rbind.data.frame(a,x)
  j=j+1
}

#Genera un reverse del data frame resultante
x<-as.data.frame(purrr::map_df(x,rev))

#Genera un nuevo dataframe para convertirlo a GeoJSON
vel.rafaga<-data.frame(x$Estacion[2:length(x$Estacion)],latitudes[2:length(latitudes)],longitudes[2:length(longitudes)],
              x$fecha[2:length(x$fecha)],x$Hora[2:length(x$Hora)],x$Variable[2:length(x$Variable)])
names(vel.rafaga)<-c("Nombre","Lat","Lon","Fecha","Hora","Velocidad r&aacute;faga KT")

#Filtra para la tabla
x<-subset(x,x!="NA")
x<-na.omit(x)

tabla<-kable(x,format = "html", table.attr = "id=\"tabla_telem\" class=\"display\" width=\"40%\"", caption = "Velocidad raf&aacute;ga KT",
             digits = 2)
cat(tabla, file="tabla_velocidad_rafaga")

#Filtra df para el mapa
vel.rafaga<-subset(vel.rafaga,vel.rafaga!="NA")
vel.rafaga<-na.omit(vel.rafaga)

#Genera el mapa desde un dataframe
  dat_local<-toGeoJSON(data = vel.rafaga, dest = tempdir())
  map2 <- leaflet(data=dat_local,dest = dir.destino, size = c("100%",500), popup=c("*"), label="Nombre")

#Limpia las variables
rm(x)
#------------------------------------------------------------------------------------------------------------>

#Extrae la precipitación
#------------------------------------------------------------------------------------------------------------>
x<-data.frame("NA","NA","NA","NA")
names(x)<-c("Estacion","fecha","Hora","Variable")
j=1
for (i in id_estacion) {
  a<-extrae.1(i,nombre_estacion[j],"PP")
  x<-rbind.data.frame(a,x)
  j=j+1
}

#Genera un reverse del data frame resultante
x<-as.data.frame(purrr::map_df(x,rev))

#Genera un nuevo dataframe para convertirlo a GeoJSON
precipitacion<-data.frame(x$Estacion[2:length(x$Estacion)],latitudes[2:length(latitudes)],longitudes[2:length(longitudes)],
                       x$fecha[2:length(x$fecha)],x$Hora[2:length(x$Hora)],x$Variable[2:length(x$Variable)])
names(precipitacion)<-c("Nombre","Lat","Lon","Fecha","Hora","Precipitaci&oacute;n mm")

#Filtra para la tabla
x<-subset(x,x!="NA")
x<-na.omit(x)

tabla<-kable(x,format = "html", table.attr = "id=\"tabla_telem\" class=\"display\" width=\"40%\"", caption = "Precipitaci&oacute;n mm",
             digits = 2)
cat(tabla, file="tabla_precipitacion")

#Filtra df para el mapa
precipitacion<-subset(precipitacion,precipitacion!="NA")
precipitacion<-na.omit(precipitacion)

#Genera el mapa desde un dataframe
dat_local<-toGeoJSON(data = precipitacion, dest = tempdir())
map2 <- leaflet(data=dat_local,dest = dir.destino, size = c("100%",500), popup=c("*"), label="Nombre")

#Limpia las variables
rm(x)
#------------------------------------------------------------------------------------------------------------>

