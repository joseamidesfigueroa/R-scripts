library(climatol)

#*************************************************************************************
setwd("C:/R/climatol/prueba")

data(Ttest)
write(dat,'Ttest_1981-2000.dat')
write.table(est.c,'Ttest_1981-2000.est', row.names=FALSE, col.names=FALSE)

homogen('Ttest', 1981, 2000, expl=TRUE)

#*************************************************************************************


#*************************************************************************************
#Define el directorio de trabajo
setwd("C:/R/climatol/Homogenizacion_climatol/tmax/")

dat<-as.matrix(read.table("datos_tmax_1975_2018.csv",sep = ","))
write(dat,'tmax_1975-2018.dat')
#Rellena datos solamente y genera graficos exploratorios
homogen('tmax',1975,2018, expl = TRUE)

#Extrae los datos rellenos
dahstat('tmax',1975,2018, stat='series')

#*************************************************************************************