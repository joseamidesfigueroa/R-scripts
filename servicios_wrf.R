#Script para resampleo y calculo de servicios climaticos con el WRF

#Carga la libreria raster
library(raster)
library(gtools)

#Carga capa para resample
ras<-raster("/home/arw/estaticos/global.tif")
ras1<-raster("/home/arw/estaticos/confort_4_d03.tif")
extent(ras1)<-c(269.585209-360,272.670000-360,12.628950,14.977950)

#Define los directorios a usar:
{
#----------------------------------------------------------------------------->
dir_A_d01<-"/home/arw/geotiff/repositorio/00/A/dominio1/"
dir_B_d01<-"/home/arw/geotiff/repositorio/00/B/dominio1/"
dir_C_d01<-"/home/arw/geotiff/repositorio/00/C/dominio1/"
dir_D_d01<-"/home/arw/geotiff/repositorio/00/D/dominio1/"
dir_E_d01<-"/home/arw/geotiff/repositorio/00/E/dominio1/"
dir_F_d01<-"/home/arw/geotiff/repositorio/00/F/dominio1/"
dir_G_d01<-"/home/arw/geotiff/repositorio/00/G/dominio1/"

dir_A_d02<-"/home/arw/geotiff/repositorio/00/A/dominio2/"
dir_B_d02<-"/home/arw/geotiff/repositorio/00/B/dominio2/"
dir_C_d02<-"/home/arw/geotiff/repositorio/00/C/dominio2/"
dir_D_d02<-"/home/arw/geotiff/repositorio/00/D/dominio2/"
dir_E_d02<-"/home/arw/geotiff/repositorio/00/E/dominio2/"
dir_F_d02<-"/home/arw/geotiff/repositorio/00/F/dominio2/"
dir_G_d02<-"/home/arw/geotiff/repositorio/00/G/dominio2/"

dir_A_d03<-"/home/arw/geotiff/repositorio/00/A/dominio3/"
dir_B_d03<-"/home/arw/geotiff/repositorio/00/B/dominio3/"
dir_C_d03<-"/home/arw/geotiff/repositorio/00/C/dominio3/"
dir_D_d03<-"/home/arw/geotiff/repositorio/00/D/dominio3/"
dir_E_d03<-"/home/arw/geotiff/repositorio/00/E/dominio3/"
dir_F_d03<-"/home/arw/geotiff/repositorio/00/F/dominio3/"
dir_G_d03<-"/home/arw/geotiff/repositorio/00/G/dominio3/"
#----------------------------------------------------------------------------->

dir.salida.dominio1.D<-"/home/arw/salidas_geotiff/dominio1/00/D/"
dir.salida.dominio2.D<-"/home/arw/salidas_geotiff/dominio2/00/D/"
dir.salida.dominio3.D<-"/home/arw/salidas_geotiff/dominio3/00/D/"
#----------------------------------------------------------------------------->
}

#Define el extent y proyeccion
{
  extension_d01<-c(264.803840-360,283.816000-360,5.992718,21.731518)
  proyeccion<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  extension_d02<-c(267.388612-360,277.145000-360,10.614341,18.833791)
  proyeccion<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  extension_d03<-c(269.585209-360,272.670000-360,12.628950,14.977950)
  proyeccion<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
}

#Define los periodos a obtener
{
  #----------------------------------------------------------------------------->
  periodos<-c("noche.d00","madrugada.d01","manana.d01","tarde.d01","noche.d01",
              "madrugada.d02","manana.d02","tarde.d02","noche.d02","madrugada.d03",
              "manana.d03","tarde.d03","noche.d03","madrugada.d04")
  #----------------------------------------------------------------------------->
}


#Defino funcion para calcular promedios por periodo
{
#Directorio es el directorio de entrada del miembro del conjunto de modelos
#Dir.salida = donde se guarda el raster de salida
#La extension = la extension del dominio que se trabaja de las ya existentes 
calc_por_periodos <- function(dir.entrada,dir.salida,extension,ras1){
  

  #Hace un listado de los archivos y los ordena del menor a mayor
  a<-mixedsort(sort(list.files(dir.entrada)))
  ras=ras1  
  
  #Calcula el promedio de las madrugadas
  i=1
  j=1
  k=1
  
    #for (i in 1:length(a)) {
    for (i in 1:((length(a)/84)*14)) {
      
      lista.periodos<-stack()
      for(l in 1:6){
        lista.periodos<- stack(lista.periodos,raster(paste(dir.entrada,a[j],sep="")))
        j=j+1
      }
      b<-calc(stack(lista.periodos), fun = mean)
      extent(b)<-extension
      projection(b) <- proyeccion
      ras<-crop(ras,b)
      b<-resample(b,ras)
      rf<-writeRaster(b, filename=paste(dir.salida,a[j-6],"_",periodos[k],".tif",sep = ""), overwrite=TRUE)
      i=i+5
      if (k == length(periodos)) {
        k=0
      }
      k=k+1
    }
  
  }
  
  
}  


#Defino funcion para calcular sumatorias por periodo LLUVIAS
{
  #Directorio es el directorio de entrada del miembro del conjunto de modelos
  #Dir.salida = donde se guarda el raster de salida
  #La extension = la extension del dominio que se trabaja de las ya existentes 
  calc_lluvia_por_periodos <- function(dir.entrada,dir.salida,extension,ras1){
    
    #Hace un listado de los archivos y los ordena del menor a mayor
    #a<-mixedsort(sort(list.files(dir.entrada)))
    a=mixedsort(sort(list.files(dir.entrada,pattern = "lluvia")))
    ras=ras1    
    #extent(ras)<-extension
    #Calcula el promedio de las madrugadas
    i=1
    j=1
    k=1
    
    #Aqui el for tiene como longitud maxima 13*numero de variables
    for (i in 1:13) {
        lista.periodos<-stack()
        for(l in 1:6){
          lista.periodos<- stack(lista.periodos,raster(paste(dir.entrada,a[j],sep="")))
          j=j+1
        }
        b<-calc(stack(lista.periodos), fun = sum)
        extent(b)<-extension
        projection(b) <- proyeccion
        ras<-crop(ras,b)
        b<-resample(b,ras)
        rf<-writeRaster(b, filename=paste(dir.salida,a[j-6],"_",periodos[k],".tif",sep = ""), overwrite=TRUE)
        i=i+5
        if (k == length(periodos)) {
          k=0
        }
        k=k+1
    }
  }
}  

#Defino funcion para hacer el resampleo de los datos cada hora
{
  #Directorio es el directorio de entrada del miembro del conjunto de modelos
  #Dir.salida = donde se guarda el raster de salida
  #La extension = la extension del dominio que se trabaja de las ya existentes 
  calc_por_hora <- function(dir.entrada,dir.salida,extension,ras1){
    
    
    #Hace un listado de los archivos y los ordena del menor a mayor
    a<-mixedsort(sort(list.files(dir.entrada)))
    ras=ras1    
    
    #Calcula el promedio de las madrugadas
    i=1
    j=1
    k=1
    
    #for (i in 1:length(a)) {
    for (i in 1:length(a)) {
      
      lista.periodos<-stack()
      b<- raster(paste(dir.entrada,a[i],sep=""))
      
      extent(b)<-extension
      projection(b) <- proyeccion
      ras<-crop(ras,b)
      b<-resample(b,ras)
      rf<-writeRaster(b, filename=paste(dir.salida,a[i],".tif",sep = ""), overwrite=TRUE)
    }
    
  }
}

#Se aplica la funcion para transformar todos los rasters por periodos :: Dominio 1, Miembro D
calc_por_periodos(dir_D_d01,dir.salida.dominio1.D,extension_d01,ras)
calc_lluvia_por_periodos(dir_D_d01,dir.salida.dominio1.D,extension_d01,ras)
calc_por_hora(dir_D_d01,dir.salida.dominio1.D,extension_d01,ras)

#Se aplica la funcion para transformar todos los rasters por periodos :: Dominio 2, Miembro D
calc_por_periodos(dir_D_d02,dir.salida.dominio2.D,extension_d02,ras)
calc_lluvia_por_periodos(dir_D_d02,dir.salida.dominio2.D,extension_d02,ras)
calc_por_hora(dir_D_d02,dir.salida.dominio2.D,extension_d02,ras)

#Se aplica la funcion para transformar todos los rasters por periodos :: Dominio 3, Miembro D
calc_por_periodos(dir_D_d03,dir.salida.dominio3.D,extension_d03,ras1)
calc_lluvia_por_periodos(dir_D_d03,dir.salida.dominio3.D,extension_d03,ras1)
calc_por_hora(dir_D_d03,dir.salida.dominio3.D,extension_d03,ras1)
