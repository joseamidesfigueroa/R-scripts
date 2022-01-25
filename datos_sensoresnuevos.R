library(lubridate)
datos<-read.table("/home/arw/SensorACUA01.csv",sep = ",",header = FALSE)
datos<-datos[-1,]
fechas<-as_datetime(datos$V3, tz="UTC")-hours(6)

nuevos_datos<-cbind(datos$V1,as.data.frame(fechas),datos$V2,datos$V4,datos$V5,datos$V6)
names(nuevos_datos)<-c("Estacion","Fecha","Parametro","Medicion","Promedio24h","Temperatura")
df_pm25<-nuevos_datos
df_pm10<-df_pm25
df_pm25<-df_pm25[(nuevos_datos$Parametro=="PM25"),]
df_pm10<-df_pm10[(nuevos_datos$Parametro=="PM10"),]