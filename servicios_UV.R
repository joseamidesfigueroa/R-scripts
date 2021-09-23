#Script para resampleo y calculo de servicios climaticos con el WRF

#Carga la libreria raster
library(raster)
library(gtools)


#Carga capa para resample
ras<-raster("C:/R/chirps_global_sample/global.tif")
ras2<-raster("C:/R/d01/gdi_1.tif")
extent(ras2)<-c(264.803840-360,283.816000-360,5.992718,21.731518)


#Define los directorios a usar:
{
  #----------------------------------------------------------------------------->
  
  dir_uv<-"C:/Users/arw/Documents/repositorio/uv/"
  #----------------------------------------------------------------------------->
  
  dir.salida.A<-"C:/Users/arw/Documents/repositorio/salidas/"
  
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
  
  extension_uv<-c(-0.250000-360,360.250000-360,-90.250000,90.250000)
  
}

#Define los períodos a obtener
{
  #----------------------------------------------------------------------------->
  periodos<-c("manana.d01","tarde.d01","noche.d01","madrugada.d02","manana.d02",
              "tarde.d02","noche.d02","madrugada.d03","manana.d03","tarde.d03",
              "noche.d03","madrugada.d04","manana.d04","tarde.d04")
  #----------------------------------------------------------------------------->
}

#Defino función para calcular promedios por período
{
  #Directorio es el directorio de entrada del miembro del conjunto de modelos
  #Dir.salida = donde se guarda el raster de salida
  #La extension = la extensión del dominio que se trabaja de las ya existentes 
  uv_por_periodos <- function(dir.entrada,dir.salida,extension){
    
    
    #Hace un listado de los archivos y los ordena del menor a mayor
    a<-mixedsort(sort(list.files(dir.entrada)))
    #v=mixedsort(sort(list.files(dir.entrada,pattern = "lluvia")))
    
    #Calcula el promedio de las madrugadas
    i=1
    j=1
    k=1
    
    
    #for (i in 1:length(a)) {
    for (i in 1:14) {
      
      lista.periodos<-stack()
      for(l in 1:6){
        lista.periodos<- stack(lista.periodos,raster(paste(dir.entrada,a[j],sep="")))
        j=j+1
      }
      b<-calc(stack(lista.periodos), fun = max)
      extent(b)<-extension
      b<-crop(b,ras2)
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

#Defino función para hacer el resampleo de los datos cada hora
{
  #Directorio es el directorio de entrada del miembro del conjunto de modelos
  #Dir.salida = donde se guarda el raster de salida
  #La extension = la extensión del dominio que se trabaja de las ya existentes 
  calc_por_hora <- function(dir.entrada,dir.salida,extension){
    
    
    #Hace un listado de los archivos y los ordena del menor a mayor
    a<-mixedsort(sort(list.files(dir.entrada)))
    #v=mixedsort(sort(list.files(dir.entrada,pattern = "lluvia")))
    
    
    #Calcula el promedio de las madrugadas
    i=1
    j=1
    k=1
    
    for (i in 1:length(a)) {
      lista.periodos<-stack()
      b<- raster(paste(dir.entrada,a[i],sep=""))
      extent(b)<-extension
      b<-crop(b,ras2)
      ras<-crop(ras,b)
      b<-resample(b,ras)
      rf<-writeRaster(b, filename=paste(dir.salida,a[i],".tif",sep = ""), overwrite=TRUE)

    }
    
  }
}

uv_por_periodos(dir_uv,dir.salida.A,extension_uv)

calc_por_hora(dir_uv,dir.salida.A,extension_uv)