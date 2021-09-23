  library(RPostgreSQL)
library(raster)

#Define el directorio de trabajo.
dir.archivos<-"/home/arw/olas_calor"
setwd(dir.archivos)

#Declara la conexión a la base de datos
drv <- dbDriver("PostgreSQL")

#Define la conexión hacia Salvador_Desarrollo
con <- dbConnect(drv, dbname="Salvador_Desarrollo",host="192.168.1.164",port=5432,user="jjamides",password="MaRn_2020" )

#Define las horas
fecha_actual<-as.Date(Sys.time())

fecha_zero<-as.Date(fecha_actual)+0
fecha_3dias <-as.Date(fecha_actual)-3
fecha_3dias_mas <-as.Date(fecha_actual)+3

fecha_inicial<-paste(fecha_3dias," 7:00:00",sep = "")
fecha_final<-paste(fecha_actual," 7:00:00",sep = "")
fecha_final_mas<-paste(fecha_3dias_mas," 7:00:00",sep = "")

fecha_0<-as.POSIXct(fecha_final, "%Y-%m-%d %H:%M:%S", tz="El Salvador")
fecha_1<-as.POSIXct(fecha_zero, "%Y-%m-%d %d:%M:%S", tz="El Salvador")
fecha_2<-as.POSIXct(fecha_final_mas, "%Y-%m-%d %H:%M:%S", tz="El Salvador")

#Genera las horas de pronostico
fechas_prono<-seq(from=fecha_0, tz="El Salvador", to=fecha_2, tz="El Salvador", by="hour")



#UES es 61, Ilopango es 4
consulta2 <- paste("select horafecha as hora, medicion as temperatura from mediciones where estacionid = '61' and horafecha between '",fecha_inicial,"' and '",fecha_final,"' and parametroid = 'AT'")

S10_ts <- dbGetQuery(con,consulta2)
ts_maximo <-max(S10_ts$temperatura,na.rm = TRUE)

temperatura_tx <-ts(S10_ts$temperatura, start = fecha_3dias, end = fecha_actual)
#plot(S10_ts,type="l",ylim=c(15,35),main="Temperatura a 2 metros de la estación de Ilopango")
#abline(h=33,col="red")


#Define la estación de Ilopango
Ilopango<-cbind(-89.118313,13.699318)

#Define las horas del día
Horas<-rbind("07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00",
         "21.00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00",
         "12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21.00","22:00","23:00","00:00","01:00","02:00",
         "03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00",
         "18:00","19:00","20:00","21.00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00")

#Carga los rasters de pronóstico
#-------------------------------------------------------------------------------------------------------------------- Dia 0 (Actual)

lista.archivos<-list.files(dir.archivos)

datos_pronostico<-c()

j=1
for (i in lista.archivos){
  ratser_dummy<-raster()
  raster_dummy<-0
  raster_dummy<-raster(i)
  extent(raster_dummy)<-c(269.5991045454544519-360,272.6782775471975242-360,12.6275776326056857,14.9644500000000011)
  projection(raster_dummy)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  datos_pronostico[j]<-(extract(raster_dummy,Ilopango,method='bilinear'))
  j=j+1
}
  

length(fechas_prono)
length(datos_pronostico)



#plot(fechas_prono,datos_pronostico,type="l",ylim=c(15,35),main="Temperatura a 2 metros de la estación de Ilopango")
#abline(h=33,col="red")


texto1<-paste("Fecha de calculo: ", fecha_zero, sep = "")

#Genera gráfico de resultado
png(filename = "reg_pron_oc.png", width = 1080)
par(mfrow=c(1,2))
plot(S10_ts,type="l",ylim=c(15,35), xlab="" , ylab="Temperatura °C")
grid(15,35)
abline(h=33,col="red")
mtext("Temperatura Registrada\n San Salvador",side = 3)
mtext(texto1,side = 4, col = "red")

plot(fechas_prono,datos_pronostico,type="l",ylim=c(15,35), xlab="" , ylab="Temperatura °C")
abline(h=33,col="red")
grid(15,35)
mtext("Temperatura Pronosticada\n San Salvador",side = 3)
mtext(texto1,side = 4, col = "red")
dev.off()




