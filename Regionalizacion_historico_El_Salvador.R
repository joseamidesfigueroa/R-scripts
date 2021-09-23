dir.src="C:/CursoManagua/CURSO/Dia_1"
source(paste(dir.src, "PuntosASCIIEnContornoSp.R", sep="/"))


library(RDownRLM)

#dir.datos="C:/CursoManagua/datos"
#puntos=PuntosASCIIEnContornoSp(paste(dir.datos, "/rejilla-t2mext-AC-era-interim/maestro_red_hr.txt", sep=""),
#                               paste(dir.datos, "/contornos/SA_CA_NOM_CAST__Nicaragua.shp", sep=""))

dir.datos="C:/CursoManagua/datos"
puntos=PuntosASCIIEnContornoSp(paste(dir.datos, "/rejilla-t2mext-AC-era-interim/maestro_red_hr.txt", sep=""),
                               paste(dir.datos, "/contornos/SA_CA_NOM_CAST__El Salvador.shp", sep=""))



names(puntos)

cc= rep("NULL", 52560)
cc[c(1, puntos$posicion+1)]="numeric"
tx=read.table(paste(dir.datos, "/rejilla-t2mext-AC-era-interim/tmax_red.txt",sep=""), colClasses = cc)

dim(tx)

head(tx[,1])

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

PredTx.nicaragua

predModHist.nicaragua = LecturaVariablesPuntosNc(pgcerca_tx_nicaragua$coor.pgrilpx.nrep, PredTx.nicaragua$variables.sel.renom, PredTx.nicaragua$niveles.sel.renom, 
                                                 paste(dir.datos, "/nc/modelosclimaticos/", sep=""),paste(modelo,"_HISTORICAL_r1i1p1_19610101-20051231-AC.nc", sep=""), 
                                                 1961, 2005)



MedSigPrdMPIMR_nicaragua=lapply(1:dim(predModHist.nicaragua$datos)[2], function(i) MediaSigmaPuntos(predModHist.nicaragua$datos[,i,], 
                                                                                                    predModHist.nicaragua$fechas, 1980, 2005, predModHist.nicaragua$calendario))


StandpredModHist.nicaragua=predModHist.nicaragua
StandpredModHist.nicaragua$datos=EstandarizacionVariables(predModHist.nicaragua$datos, MedSigPrdMPIMR_nicaragua)

# Cambio del nombre de los predictores según nomenclatura del reanálisis
StandpredModHist.nicaragua=CambiaNombreVariables(StandpredModHist.nicaragua,varMod, nivEra*100, varEra, nivEra)

ProyTx_nicaragua_MPIESMMR_HISTORICAL=ProyectaNcRLM(ModelosTx_nicaragua, StandpredModHist.nicaragua, pgcerca_tx_nicaragua, modelo, "historical")


jpeg(filename = "C:/CursoManagua/resultados/esa_hist.jpg",width = 1800, height = 1500)
par(mfrow=c(2,2))
time_serie1<-ts(ProyTx_nicaragua_MPIESMMR_HISTORICAL$proyeccion[[1]],start = c(1980,01,01),end = c(2017,12,31),frequency=12)
plot(time_serie1,main="Estacion 1",col="blue")

time_serie2<-ts(ProyTx_nicaragua_MPIESMMR_HISTORICAL$proyeccion[[2]],start = c(1980,01,01),end = c(2017,12,31),frequency=12)
plot(time_serie1,main="Estacion 2",col="red")

time_serie3<-ts(ProyTx_nicaragua_MPIESMMR_HISTORICAL$proyeccion[[3]],start = c(1980,01,01),end = c(2017,12,31),frequency=12)
plot(time_serie1,main="Estacion 3",col="black")

time_serie4<-ts(ProyTx_nicaragua_MPIESMMR_HISTORICAL$proyeccion[[4]],start = c(1980,01,01),end = c(2017,12,31),frequency=12)
plot(time_serie1,main="Estacion 4",col="green")
dev.off()

estaciones <- c("e1","e2","e3","e4","e5","e6","e7","e8","e9","e10","e11","e12","e13","e14","e15","e16","e17","e18","e19","e20",
                "e21","e22","e23","e24","e25","e26","e27","e28","e29")

series_hist <- as.data.frame(ProyTx_nicaragua_MPIESMMR_HISTORICAL$proyeccion[])
colnames(series_hist)=estaciones


jpeg("C:/CursoManagua/resultados/esa_prom_hist.jpg",width =1200, height = 800 )
promedio_nacional_hist<-as.data.frame(rowMeans(series_hist,dims = 1))
serie_temporal_promedio_nacional_hist <- ts(promedio_nacional_hist,start = c(1980,01,01),end = c(2017,12,31),frequency=365)
plot(serie_temporal_promedio_nacional_hist, main="Serie de tiempo de la temperatura máxima diaria promedio nacional para El Salvador desde 1980 a 2017\nModelo ERA-INTERIM", ylab="Temperaturas Celsius", xlab="Años",col="brown",
     ylim=c(23,40))
dev.off()



