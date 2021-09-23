#Calcula los percentiles 90, 95 y 99 de la temperatura máxima

#Abre los datos a trabajar
setwd("C:/R/climatol/Homogenizacion_climatol/tmax")
datos<-read.table("tmax_1975-2018_series_NH.csv",sep=",")

lista.estaciones<-c("Percentil","Guija","Finca los Andes","Candelaria de la frontera", "Planes de Montecristo", "Santa Ana",
                    "Chorrera del Guayabo", "Sensuntepeque", "Cerron Grande", "Cojutepeque", "Nueva Concepcion",
                    "La Palma", "Las Pilas", "Ahuachapan", "La Hachadura", "San Andres", "Chiltiuipan", "San Miguel",
                    "La Union", "Ilopango", "Acajutla", "Los Naranjos", "Santiago de Maria", "Puente Cuscatlan",
                    "San Francisco Gotera", "Perquin")

#Genera una lista de los percentiles para cada fila
percentil_num<-as.data.frame(c(0:100))
#Genera un data frame vacio para llenar con los percentiles de cada estacion
percentiles<-as.data.frame(seq(1:101))

#Ciclo para calcular los percentiles de cada estacion
j<-1
for(i in c(2:26)){
  percentiles[,j]<-as.data.frame(quantile(datos[,i],prob=seq(0,1,length=101)))
  j<-j+1
}

#Genera un segundo dataframe para concatenar la lista de percentiles y los percentiles calculados por estacion
percentiles2<-cbind.data.frame(percentil_num,percentiles)
#Renombra las columnas
names(percentiles2)<-lista.estaciones
#Escribe el resultado en un csv
write.csv(percentiles2,file="percentiles.csv",quote = FALSE)
