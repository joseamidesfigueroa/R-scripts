

#Declara la conexión a la base de datos
#---------------------------------------------------------------------------------------------------------------------------------------------
library(RPostgreSQL)
#Define el directorio de trabajo.
dir.archivos<-"/home/arw/temporal"
setwd(dir.archivos)
#Carga el indice de las estaciones
indice<-read.table("/home/arw/estaciones_vielca.csv",sep = ";",header = FALSE)
#Declara la conexión a la base de datos
drv <- dbDriver("PostgreSQL")
#Define las horas
#fecha_actual<-as.Date(Sys.time())
actualizacion<-Sys.time()
#fecha_zero<-as.Date(fecha_actual)+0
texto1<-paste("Actualización: ", actualizacion, sep = "")
#Define la conexión hacia Salvador_Desarrollo
con <- dbConnect(drv, dbname="Salvador_Desarrollo",host="192.168.1.164",port=5432,user="jjamides",password="MaRn_2020" )
#Extrae datos del indice como listas
id_estacion <- as.numeric(indice$V1[2:length(indice$V1)])
nombre_estacion <-matrix(indice$V2[2:length(indice$V2)])
df<-data.frame()
#---------------------------------------------------------------------------------------------------------------------------------------------




###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
#Funciones para extraer ultimos 5 datos.
#----------------------------------------------------------------------------------------------------------->
{
  #Funcion que extrae el último dato de una estación telemétrica
  revisa_estacion.5<-function(id_est,nombre,variable){
    set.seed(1000)
    n<-1
    k<-5
    dat<-rnorm(n*k)
    
    lista_datos <- data.frame(data=dat)
    lista_fechas<-data.frame(data=dat,data=dat,data=dat,data=dat)
    names(lista_fechas)<-c("año","mes","dia","hora")
    
    consulta.ultimas.fechas.estacion<-paste("select horafecha as hora, medicion as temperatura from mediciones where estacionid = '",id_est,"' and parametroid ='",variable,"' order by hora desc limit 5",sep = "")
    #consulta.ultimas.fechas.estacion<-"select horafecha as hora, medicion as temperatura from mediciones where estacionid = '61' and parametroid ='AT' order by hora desc limit 10"
    a<-dbGetQuery(con,consulta.ultimas.fechas.estacion)
    #Extrae fecha
    lista_fechas$año[1]<-substr(a$hora[1],1,4)
    lista_fechas$año[2]<-substr(a$hora[2],1,4)
    lista_fechas$año[3]<-substr(a$hora[3],1,4)
    lista_fechas$año[4]<-substr(a$hora[4],1,4)
    lista_fechas$año[5]<-substr(a$hora[5],1,4)
    lista_fechas$mes[1]<-substr(a$hora[1],6,7)
    lista_fechas$mes[2]<-substr(a$hora[2],6,7)
    lista_fechas$mes[3]<-substr(a$hora[3],6,7)
    lista_fechas$mes[4]<-substr(a$hora[4],6,7)
    lista_fechas$mes[5]<-substr(a$hora[5],6,7)
    lista_fechas$dia[1]<-substr(a$hora[1],9,10)
    lista_fechas$dia[2]<-substr(a$hora[2],9,10)
    lista_fechas$dia[3]<-substr(a$hora[3],9,10)
    lista_fechas$dia[4]<-substr(a$hora[4],9,10)
    lista_fechas$dia[5]<-substr(a$hora[5],9,10)
    lista_fechas$hora[1]<-substr(a$hora[1],12,19)
    lista_fechas$hora[2]<-substr(a$hora[2],12,19)
    lista_fechas$hora[3]<-substr(a$hora[3],12,19)
    lista_fechas$hora[4]<-substr(a$hora[4],12,19)
    lista_fechas$hora[5]<-substr(a$hora[5],12,19)
    
    #Extrae dato
    lista_datos$data[1]<-a$temperatura[1]
    lista_datos$data[2]<-a$temperatura[2]
    lista_datos$data[3]<-a$temperatura[3]
    lista_datos$data[4]<-a$temperatura[4]
    lista_datos$data[5]<-a$temperatura[5]
    
    dato<-data.frame(nombre,lista_fechas$año,lista_fechas$dia,lista_fechas$mes,lista_fechas$hora,lista_datos$data)
    names(dato)<-c("Estacion","Año","Día","Mes","Hora","Variable")
    
    #dato
    return(dato)
    returno(TRUE)
  }
  
  #Funcion para extraer datos con pruebas de errores Try y Catch
  #--------------------------------------------------------------------------------------------------
  #Ejecuta un Try & Catch para cuando la función no encuentre datos en alguna estación
  extrae.5<-function(i,j,k){
    tryCatch(
      expr = {
        a<-revisa_estacion.5(i,j,k)
        a
      },
      error = function(e){
        a<-"No habian datos"
        a
      },
      warning = function(w) {
        a<-"Hay algunos problemas pero puedo continuar"
        a
      },
      finally = {
        a<-"Terminando"
        a
      }
    )
  }
  #--------------------------------------------------------------------------------------------------
}
#----------------------------------------------------------------------------------------------------------->
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################