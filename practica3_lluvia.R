library(RDownRLM)

#Carga puntos y shape
dir.datos="C:/CursoManagua/datos"
puntos=PuntosNcEnContornoSp(paste(dir.datos, "/nc/AC_chirps-v2.0.1981-2018.days_p20.nc", sep=""), 
                            paste(dir.datos,"/contornos/SA_CA_NOM_CAST__Nicaragua.shp", sep=""))


#Obtiene la precipitacion de CHIRPS
prec=LecturaVariableSuperficiePuntosNc("precip", puntos$coordenadas, paste(dir.datos, "/nc/AC_chirps-v2.0.1981-2018.days_p20.nc", sep=""), 1981, 2010)

#Define las Variables predictoras de los modelos Globales (ERA)
varEra=c("q", "t", "u", "v", "msl")

#Define los niveles de los datos del modelo Global
nivEra=c(850,700,500,250)

#Obtiene los puntos del modelo global de reanalisis mas cercanos a los puntos de rejilla dentro del pais.
pgcerca_pr_nicaragua = PuntosRejillaMasCercanosNc(prec$coordenadas, paste(dir.datos, "/nc/msl_ERA-Interim15_REANALYSIS_19800101-20181231-AC.nc", sep=""))

#Se consiguen los datos del modelo para los niveles y variables por cada punto
PredEra_nicaragua=LecturaVariablesPuntosNc(pgcerca_pr_nicaragua$coor.pgrilpx.nrep, varEra, nivEra, paste(dir.datos, "/nc/", sep=""),"ERA-Interim15_REANALYSIS_19800101-20181231-AC.nc", 1981, 2010)


########################################################################
#CALIBRAR EL MODELO

#Estandarizacion para reanalisis
#Obtención de los valores medios y las desviaciones típicas de los campos en los puntos
MedSigPrdEra_nicaragua=lapply(1:dim(PredEra_nicaragua$datos)[2], function(i) MediaSigmaPuntos(PredEra_nicaragua$datos[,i,], PredEra_nicaragua$fechas, 1981, 2005, PredEra_nicaragua$calendario))

#Se copia la lista con los valores de los campos en otra donde se almacenarán los valores estandarizados
StandPredEra_nicaragua=PredEra_nicaragua

#Estandarización de los campos
StandPredEra_nicaragua$datos=EstandarizacionVariables(PredEra_nicaragua$datos, MedSigPrdEra_nicaragua)


#Calibracion del modelo
# Calibración de los modelos (uno por punto) 
ModelosPr_nicaragua=ModelosCdReg(prec, StandPredEra_nicaragua, pgcerca_pr_nicaragua$pgrilpx.nrep.obs, pcalibra = c(1981,2010))

########################################################################



########################################################################
#PROYECCIONES

#Escogo el modelo de cambio climatio a usar
modelo="MPI-ESM-MR"

#Defino las variables del modelo global con las que voy a trabajar (OJO que debo de ir a conocerlas en el nc correspondiente.)
varMod=c("hus", "ta",   "ua",   "va", "psl")

#Predictores seleccionados por los modelos calibrados previamente
PredPr.nicaragua=VariablesSeleccionadas(ModelosPr_nicaragua, varEra, nivEra, varMod, nivEra*100, tipo="C")

#-------------------------------------------------------------- (Historico)
#Lectura de los datos del modelo. (Historico)
predModHist.nicaragua = LecturaVariablesPuntosNc(pgcerca_pr_nicaragua$coor.pgrilpx.nrep, PredPr.nicaragua$variables.sel.renom, 
                                                 PredPr.nicaragua$niveles.sel.renom, paste(dir.datos, "/nc/modelosclimaticos/", sep=""),paste(modelo,"_HISTORICAL_r1i1p1_19610101-20051231-AC.nc", sep=""), 1961, 2005)


#****Estandarizacion de los datos leidos anteriormente****

#Valores medios y dispersión del periodo
MedSigPrdMPIMR_nicaragua=lapply(1:dim(predModHist.nicaragua$datos)[2], function(i) MediaSigmaPuntos(predModHist.nicaragua$datos[,i,], predModHist.nicaragua$fechas, 1981, 2005, predModHist.nicaragua$calendario))

#Creo el objeto para guardar la estandarizacion
StandpredModHist.nicaragua=predModHist.nicaragua

#Genero la estandarizacion
StandpredModHist.nicaragua$datos=EstandarizacionVariables(predModHist.nicaragua$datos, MedSigPrdMPIMR_nicaragua)

# Cambio del nombre de los predictores según nomenclatura del reanálisis

StandPredModHist.nicaragua=CambiaNombreVariables(StandpredModHist.nicaragua,varMod, nivEra*100, varEra, nivEra)

names(StandPredModHist.nicaragua)

StandPredModHist.nicaragua$variables

#OBTENCION DE PROYECCIONES REGIONALIZADAS
#proyecciones en el escenario de clima actual

#Falta que Petra corrija esto.
#ProyPr_nicaragua_MPIESMMR_HISTORICAL=ProyectaCdReg(ModelosPr_nicaragua, StandPredModHist.nicaragua, pgcerca_pr_nicaragua, modelo, "historical", random=T)


########################################################################
