#Declara la conexion a la base de datos
#---------------------------------------------------------------------------------------------------------------------------------------------
library(RPostgreSQL)
#Define el directorio de trabajo.
dir.archivos<-"[tu_direcctorio]"
setwd(dir.archivos)

#Declara la conexion a la base de datos
drv <- dbDriver("PostgreSQL")

#Define los parametros de conexion
con <- dbConnect(drv, dbname="Nombre_BasedeDatos",host="IP_servidor",port=5432,user="usuario",password="contrasena" )

#Define la consulta 
consulta<-paste("select * from TABLA",sep="")

#Realiza la consulta y la guarda en una variable "a"
a<-dbGetQuery(con,consulta.ultimas.fechas.estacion)
