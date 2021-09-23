#Extracción de lluvia por coordenadas
#Amides Figueroa - Julio 2021

library(raster)

#Define la extension del raster
extension_raster<-c(264.803840-360,283.816000-360,5.992718,21.731518)
dir.files<-"C:/R/temporal/"
archivos<-list.files("C:/R/temporal/")

total<-raster(paste(dir.files,archivos[1],sep=""))
total<-total*0

for (i in seq(1:length(archivos))) {
  var<-raster(paste(dir.files,archivos[i],sep = ""))
  total<-var+total
}


#Aplica la extension al raster
extent(total)<-extension_raster

#Aplica la proyeccion
projection(total) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Definición de estaciones
{
Estaciones <-list(
                  E01_Guija<-cbind(-89.46999,14.229286),E02_Los_Andes<-cbind(-89.628326000000001,13.874305000000000),
                  E03_Candelaria_Frontera<-cbind(-89.651657999999998,14.120964000000001),
                  E04_Montecristo<-cbind(-89.359988999999999,14.399279000000000),E05_Santa_Ana<-cbind(-89.548320000000004,13.982635999999999),
                  E06_Chorrea_Guayabo<-cbind(-89.548320000000004,13.987634000000000),E07_Sensuntepeque<-cbind(-88.646636000000001,13.870975000000000),
                  E08_Cerron_Grande<-cbind(-88.926643999999996,13.93764000000000),E09_Cojutepeque<-cbind(-88.926643999999996,13.720983000000000),
                  E10_Nueva_Concepcion<-cbind(-89.289987999999994,14.125966000000000),E11_La_Palma<-cbind(-89.161653999999999,14.292623000000001),
                  E12_Las_Pilas<-cbind(-89.088307999999998,14.374274000000000),E13_Ahuachapan<-cbind(-89.859994999999998,13.944305999999999),
                  E14_La_Hachadura<-cbind(-90.089990000000000,13.860979000000000),E15_San_Andres<-cbind(-89.406654000000003,13.809312000000000),
                  E16_Chiltiupan<-cbind(-89.469984999999994,13.597661000000000),E17_San_Miguel<-cbind(-88.158293999999998,13.439336000000001),
                  E18_La_Union<-cbind(-87.826779000000002,13.333684000000000),E19_Ilopango<-cbind(-89.118313,13.699318),E20_Acajutla<-cbind(-89.833333999999994,13.574325999999999),
                  E21_Los_Naranjos<-cbind(-89.674995999999993,13.875971000000000),E22_Santiago_Maria<-cbind(-88.471638999999996,13.485992000000000),
                  E23_Puente_Cuscatlan<-cbind(-88.593303000000006,13.602655000000000),E24_San_Francisco_Gotera<-cbind(-88.106623999999996,13.697651000000000),
                  E25_Perquin<-cbind(-88.162499999999994,13.960889000000000),E26_Acomunca<-cbind(-88.8830795,13.4446106),E27_Acahuapa<-cbind(-88.5808029,13.5377998),
                  E29_Cara_Sucia<-cbind(-90.0345001,13.7753),E30_Chalatenango<-cbind(-88.9693985,14.0422001),
                  E31_Chalchuapa<-cbind(-89.6677017,13.9855003),E32_El_Penon<-cbind(-89.9192963,13.6693001),E33_Oscicala<-cbind(-88.1490021,13.8052998),
                  E34_Conchagua<-cbind(-87.8339005,13.2763004),E35_MARN<-cbind(-89.231540,13.687524),E36_La_Carrera<-cbind(-88.49522313,13.327264),
                  E37_Rosario_Morazan<-cbind(-88.214600,13.870508),E38_Porrillo<-cbind(-88.815230,13.448568),E39_Puerto_Parada<-cbind(-88.439264,13.255757),
                  E40_AIES<-cbind(-89.0534364529,13.4378999151)
                  )
}
Nombres_estaciones<-list("Guija","Los Andes","Candelaria de la Frontera","Montecristo","Santa Ana","Chorrera del Guayabo","Sensuntepeque","Cerron Grande",
                         "Cojutepeque","Nueva Concepcion","La Palma","Las Pilas","Ahuachapan","La Hachadura","San Andres","Chiltiupan","San Miguel","La Union",
                         "Ilopango","Acajutla","Los Naranjos","Santiago de Maria","Puente Cuscatlan","San Francisco Gotera","Perquin","Acomunca","Acahuapa","Cara Sucia",
                         "Chalatenango","Chalchuapa","El Penon","Oscicala","Conchagua","MARN","La Carrera","Rosario Morazan","Porrillo","Puerto Parada",
                         "AIES"
)

length(Estaciones)
length(Nombres_estaciones)

#Extrae la lluvia de cada estacion.
Lluvia<-list()
for (i in seq(1:length(Estaciones)) ){
  Lluvia[i] <- (extract(total,Estaciones[[i]],method='bilinear'))
}

mat_Lluvia<-matrix(Lluvia,nrow = 1,ncol = length(Estaciones))
mat_Lluvia<-t(mat_Lluvia)
mat_nombre<-matrix(Nombres_estaciones,nrow = 1,ncol = length(Estaciones))
mat_nombre<-t(mat_nombre)
rownames(mat_Lluvia)<-mat_nombre



write.table(mat_Lluvia, file=paste(dir.files,"lluvia_10_dias.csv",sep = ""),row.names = TRUE, col.names = FALSE, sep =",")