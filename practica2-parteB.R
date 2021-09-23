dir.src="C:/CursoManagua/CURSO/Dia_1"
source(paste(dir.src, "PuntosASCIIEnContornoSp.R", sep="/"))


library(RDownRLM)

dir.datos="C:/CursoManagua/datos"
#puntos=PuntosASCIIEnContornoSp(paste(dir.datos, "/rejilla-t2mext-AC-era-interim/maestro_red_hr.txt", sep=""),
#                              paste(dir.datos, "/contornos/SA_CA_NOM_CAST__Nicaragua.shp", sep=""))


dir.datos="C:/CursoManagua/datos"
puntos=PuntosASCIIEnContornoSp(paste(dir.datos, "/rejilla-t2mext-AC-era-interim/maestro_red_hr.txt", sep=""),
                               paste(dir.datos, "/contornos/SA_CA_NOM_CAST__El Salvador.shp", sep=""))

names(puntos)

cc= rep("NULL", 52560)
cc[c(1, puntos$posicion+1)]="numeric"
tx=read.table(paste(dir.datos, "/rejilla-t2mext-AC-era-interim/tmax_red.txt",sep=""), colClasses = cc)

dim(tx)

head(tx[,1])

length(tx)

head(tx[1,])

tail(tx[,1])

ii=which(tx$V1 == 19800101 | tx$V1 ==20171231)
ii


fechas=as.PCICt(as.character(tx$V1[c(ii[1]:ii[2])]), cal="gregorian", format="%Y%m%d")

ltx=list(variable="tx", coordenadas=puntos$coordenadas, fechas=fechas, calendario="gregorian", datos=as.matrix(tx[c(ii[1]:ii[2]),2:ncol(tx)]))

names(ltx)


length(ltx$fechas)

dim(ltx$coordenadas)


dim(ltx$datos)


varEra=c("q", "t", "u", "v", "msl")

nivEra=c(850,700,500,250)

pgcerca_tx_nicaragua = PuntosRejillaMasCercanosNc(ltx$coordenadas, paste(dir.datos, "/nc/msl_ERA-Interim15_REANALYSIS_19800101-20181231-AC.nc", sep=""))


names(pgcerca_tx_nicaragua)

dim(pgcerca_tx_nicaragua$coor.pgrilpx.nrep)

pgcerca_tx_nicaragua$coor.pgrilpx.nrep

head(pgcerca_tx_nicaragua$pos.gril.pgrilpx)

pgcerca_tx_nicaragua$pgrilpx.nrep.obs

PredEra_nicaragua=LecturaVariablesPuntosNc(pgcerca_tx_nicaragua$coor.pgrilpx.nrep, varEra, nivEra, paste(dir.datos, "/nc/", sep=""),
                                           "ERA-Interim15_REANALYSIS_19800101-20181231-AC.nc", 1980, 2017)


names(PredEra_nicaragua)
length(PredEra_nicaragua$variables)



#CALIBRACION

MedSigPrdEra_nicaragua=lapply(1:dim(PredEra_nicaragua$datos)[2], function(i) MediaSigmaPuntos(PredEra_nicaragua$datos[,i,], 
                                                                                              PredEra_nicaragua$fechas, 1980, 2005, PredEra_nicaragua$calendario))

StandPredEra_nicaragua=PredEra_nicaragua

StandPredEra_nicaragua$datos=EstandarizacionVariables(PredEra_nicaragua$datos, MedSigPrdEra_nicaragua)

dim(PredEra_nicaragua$datos)


ModelosTx_nicaragua=ModelosNcRLM(ltx, StandPredEra_nicaragua, pgcerca_tx_nicaragua$pgrilpx.nrep.obs, pcalibra = c(1980,2017))

length(ModelosTx_nicaragua)

ModelosTx_nicaragua[[]]



modelo="MPI-ESM-MR"

varMod=c("hus", "ta",   "ua",   "va", "psl")

PredTx.nicaragua=VariablesSeleccionadas(ModelosTx_nicaragua, varEra, nivEra, varMod, nivEra*100)




#PredTx.nicaragua

#psl_MPI-ESM-MR_RCP85_r1i1p1_20060101-21001231-AC

##################################################################################################################################################################
##################################################################################################################################################################
#------------------------Desde aqui.

predModRCP85.nicaragua = LecturaVariablesPuntosNc(pgcerca_tx_nicaragua$coor.pgrilpx.nrep, PredTx.nicaragua$variables.sel.renom, PredTx.nicaragua$niveles.sel.renom, 
                                                 paste(dir.datos, "/nc/modelosclimaticos/", sep=""),paste(modelo,"_RCP85_r1i1p1_20060101-21001231-AC.nc", sep=""), 
                                                 206, 2100)

#-------------------------------------------
#Hay que mantener esto
MedSigPrdMPIMR_nicaragua=lapply(1:dim(predModRCP85.nicaragua$datos)[2], function(i) MediaSigmaPuntos(predModRCP85.nicaragua$datos[,i,], 
                                                                                                    predModRCP85.nicaragua$fechas, 2006, 2100, predModRCP85.nicaragua$calendario))

#-------------------------------------------

StandpredModRCP85.nicaragua=predModRCP85.nicaragua
StandpredModRCP85.nicaragua$datos=EstandarizacionVariables(predModRCP85.nicaragua$datos, MedSigPrdMPIMR_nicaragua)

# Cambio del nombre de los predictores según nomenclatura del reanálisis
StandpredModRCP85.nicaragua=CambiaNombreVariables(StandpredModRCP85.nicaragua,varMod, nivEra*100, varEra, nivEra)

# Obtención de proyecciones
ProyTx_nicaragua_MPIESMMR_RCP85=ProyectaNcRLM(ModelosTx_nicaragua, StandpredModRCP85.nicaragua, pgcerca_tx_nicaragua, modelo, "RCP85")


names(ProyTx_nicaragua_MPIESMMR_RCP85)
length(ProyTx_nicaragua_MPIESMMR_RCP85$puntos)



#Genero una lista para extraer las latitudes y longitudes
l1=seq(1:29)
l2=seq(30:58)


#Extraigo las longitudes
lon <- c(1:29)
lon[1]

for (i in l1) {
  lon[i] <- ProyTx_nicaragua_MPIESMMR_RCP85$puntos[[i]]
}

#lon <- ProyTx_nicaragua_MPIESMMR_RCP85$puntos[[29]]

#Extraigo las latitudes
lat <- c(1:29)
for (i in l2) {
  lat[i] <- ProyTx_nicaragua_MPIESMMR_RCP85$puntos[[i]]
}

#ProyTx_nicaragua_MPIESMMR_RCP85$puntos[[58]]
length(ProyTx_nicaragua_MPIESMMR_RCP85$proyeccion[])
length(ProyTx_nicaragua_MPIESMMR_RCP85$fechas[])

#library(akima)

#ras<-interp(lon,lat,ProyTx_nicaragua_MPIESMMR_RCP85$proyeccion[[1]])


jpeg(filename = "C:/CursoManagua/resultados/esa_hist.jpg",width = 800, height = 1500)
par(mfrow=c(2,2))
time_serie1<-ts(ProyTx_nicaragua_MPIESMMR_RCP85$proyeccion[[1]],start = c(2006,01,01),end = c(2100,12,31),frequency=365)
plot(time_serie1,main="Estacion 1",col="blue")

time_serie2<-ts(ProyTx_nicaragua_MPIESMMR_RCP85$proyeccion[[2]],start = c(2006,01,01),end = c(2100,12,31),frequency=365)
plot(time_serie1,main="Estacion 2",col="red")

time_serie3<-ts(ProyTx_nicaragua_MPIESMMR_RCP85$proyeccion[[3]],start = c(2006,01,01),end = c(2100,12,31),frequency=365)
plot(time_serie1,main="Estacion 3",col="black")

time_serie4<-ts(ProyTx_nicaragua_MPIESMMR_RCP85$proyeccion[[4]],start = c(2006,01,01),end = c(2100,12,31),frequency=365)
plot(time_serie1,main="Estacion 4",col="green")
dev.off()

estaciones <- c("e1","e2","e3","e4","e5","e6","e7","e8","e9","e10","e11","e12","e13","e14","e15","e16","e17","e18","e19","e20",
                "e21","e22","e23","e24","e25","e26","e27","e28","e29")

series <- as.data.frame(ProyTx_nicaragua_MPIESMMR_RCP85$proyeccion[])
colnames(series)=estaciones

promedio_nacional<-as.data.frame(rowMeans(series,dims = 1))
serie_temporal_promedio_nacional <- ts(promedio_nacional,start = c(2006,01,01),end = c(2100,12,31),frequency=365)

plot(serie_temporal_promedio_nacional)

plot(ProyTx_nicaragua_MPIESMMR_RCP85$proyeccion[[1]])
##################################################################################################################################################################
##################################################################################################################################################################

