dir.src="C:/CursoManagua/CURSO/Dia_1"
source(paste(dir.src, "PuntosASCIIEnContornoSp.R", sep="/"))


library(RDownRLM)

dir.datos="C:/CursoManagua/datos"
puntos=PuntosASCIIEnContornoSp(paste(dir.datos, "/rejilla-t2mext-AC-era-interim/maestro_red_hr.txt", sep=""),#indice de estaciones
                               paste(dir.datos, "/contornos/SA_CA_NOM_CAST__El Salvador.shp", sep="")) 


names(puntos)

cc= rep("NULL", 52560)
cc[c(1, puntos$posicion+1)]="numeric"
tx=read.table(paste(dir.datos, "/rejilla-t2mext-AC-era-interim/tmax_red.txt",sep=""), colClasses = cc)#Dato de temperatura

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


names(ProyTx_nicaragua_MPIESMMR_HISTORICAL)


length(ProyTx_nicaragua_MPIESMMR_HISTORICAL$proyeccion)


length(ProyTx_nicaragua_MPIESMMR_HISTORICAL$proyeccion[[1]])


ProyTx_nicaragua_MPIESMMR_HISTORICAL$proyeccion



