#Script datos marino costero

library(raster)

setwd("C:/Users/Amides/Desktop/temp/temp")
#Carga los datos a usar segun las variables
#Nubosidad, velocidad y direccion del viento, humedad relativa y presion


#Define los puntos a extraer:
Acajutla_estacion<-cbind(-90.00,13.5)
Acajutla_Mar<-cbind(-90.25,12.63)
La_Union_estacion<-cbind(-88.0,13.0)
La_Union_mar<-cbind(-88.25,12.63)

lista.puntos<-list(Acajutla_estacion,Acajutla_Mar,La_Union_estacion,La_Union_mar)
#lista.puntos[[1]]

lista.periodos<-rbind("Manana","Tarde","Noche","Madrugada")
nombres<-c("Periodo","Prom Acajutla","Promedio Acajutla mar","Prom La Union","Prom La Union mar",
           "Max Acajutla","Max Acajutla mar","Max La Union","Max La Union mar",
           "Min Acajutla","Min Acajutla mar","Min La Union","Min La Union mar")

#----- Nubosidad
############################################################################################################################################################
{
  #**************************************************************************************************
  #Periodo de la manana 
  {
    #Carga los datos necesarios
    
    a<-raster("nubosidad_wrf_25.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("nubosidad_wrf_26.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("nubosidad_wrf_27.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("nubosidad_wrf_28.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("nubosidad_wrf_29.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("nubosidad_wrf_30.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_manana<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_manana<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_manana<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
  }
  #**************************************************************************************************
  
  #**************************************************************************************************
  #Periodo de la tarde
  {
    #Carga los datos necesarios
    #----- Nubosidad
    a<-raster("nubosidad_wrf_31.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("nubosidad_wrf_32.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("nubosidad_wrf_33.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("nubosidad_wrf_34.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("nubosidad_wrf_35.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("nubosidad_wrf_36.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_tarde<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_tarde<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_tarde<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
  }
  #**************************************************************************************************
  
  #**************************************************************************************************
  #Periodo de la noche
  {
    #Carga los datos necesarios
    #----- Nubosidad
    a<-raster("nubosidad_wrf_37.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("nubosidad_wrf_38.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("nubosidad_wrf_39.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("nubosidad_wrf_40.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("nubosidad_wrf_41.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("nubosidad_wrf_42.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_noche<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_noche<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_noche<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
  }
  #**************************************************************************************************
  
  #**************************************************************************************************
  #Periodo de la madrugada
  {
    #Carga los datos necesarios
    #----- Nubosidad
    a<-raster("nubosidad_wrf_43.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("nubosidad_wrf_44.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("nubosidad_wrf_45.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("nubosidad_wrf_46.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("nubosidad_wrf_47.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("nubosidad_wrf_48.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_madrugada<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_madrugada<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_madrugada<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
    
  }
  #**************************************************************************************************
  
  #---------------------------------------------------------------------------------------------------------------------------------------->
  {
    nubosidad_promedio<-rbind(prom_nubes_manana,prom_nubes_tarde,prom_nubes_noche,prom_nubes_madrugada)
    nubosidad_max<-rbind(max_nubes_manana,max_nubes_tarde,max_nubes_noche,max_nubes_madrugada)
    nubosidad_min<-rbind(min_nubes_manana,min_nubes_tarde,min_nubes_noche,min_nubes_madrugada)

    nubes<-cbind(lista.periodos,nubosidad_promedio,nubosidad_max,nubosidad_min)
    write.table(nubes,file="nubosidad.csv",sep = ";",quote = FALSE,row.names = FALSE)
  }
  #---------------------------------------------------------------------------------------------------------------------------------------->
}
############################################################################################################################################################

#----- Velocidad viento
############################################################################################################################################################
{
  #**************************************************************************************************
  #Periodo de la manana 
  {
    #Carga los datos necesarios
    
    a<-raster("vv_10m_25.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("vv_10m_26.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("vv_10m_27.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("vv_10m_28.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("vv_10m_29.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("vv_10m_30.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_manana<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_manana<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_manana<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
  }
  #**************************************************************************************************
  
  #**************************************************************************************************
  #Periodo de la tarde
  {
    #Carga los datos necesarios
    #----- Nubosidad
    a<-raster("vv_10m_31.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("vv_10m_32.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("vv_10m_33.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("vv_10m_34.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("vv_10m_35.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("vv_10m_36.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_tarde<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_tarde<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_tarde<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
  }
  #**************************************************************************************************
  
  #**************************************************************************************************
  #Periodo de la noche
  {
    #Carga los datos necesarios
    #----- Nubosidad
    a<-raster("vv_10m_37.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("vv_10m_38.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("vv_10m_39.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("vv_10m_40.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("vv_10m_41.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("vv_10m_42.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_noche<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_noche<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_noche<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
  }
  #**************************************************************************************************
  
  #**************************************************************************************************
  #Periodo de la madrugada
  {
    #Carga los datos necesarios
    #----- Nubosidad
    a<-raster("vv_10m_43.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("vv_10m_44.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("vv_10m_45.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("vv_10m_46.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("vv_10m_47.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("vv_10m_48.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_madrugada<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_madrugada<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_madrugada<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
    
  }
  #**************************************************************************************************
  
  #---------------------------------------------------------------------------------------------------------------------------------------->
  {
    nubosidad_promedio<-rbind(prom_nubes_manana,prom_nubes_tarde,prom_nubes_noche,prom_nubes_madrugada)
    nubosidad_max<-rbind(max_nubes_manana,max_nubes_tarde,max_nubes_noche,max_nubes_madrugada)
    nubosidad_min<-rbind(min_nubes_manana,min_nubes_tarde,min_nubes_noche,min_nubes_madrugada)
    
    nubes<-cbind(lista.periodos,nubosidad_promedio,nubosidad_max,nubosidad_min)
    write.table(nubes,file="velocidad_viento.csv",sep = ";",quote = FALSE,row.names = FALSE)
  }
  #---------------------------------------------------------------------------------------------------------------------------------------->
}
############################################################################################################################################################

#----- Direccion viento
############################################################################################################################################################
{
  #**************************************************************************************************
  #Periodo de la manana 
  {
    #Carga los datos necesarios
    
    a<-raster("wd_10m_25.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("wd_10m_26.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("wd_10m_27.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("wd_10m_28.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("wd_10m_29.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("wd_10m_30.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_manana<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_manana<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_manana<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
  }
  #**************************************************************************************************
  
  #**************************************************************************************************
  #Periodo de la tarde
  {
    #Carga los datos necesarios
    #----- Nubosidad
    a<-raster("wd_10m_31.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("wd_10m_32.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("wd_10m_33.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("wd_10m_34.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("wd_10m_35.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("wd_10m_36.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_tarde<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_tarde<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_tarde<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
  }
  #**************************************************************************************************
  
  #**************************************************************************************************
  #Periodo de la noche
  {
    #Carga los datos necesarios
    #----- Nubosidad
    a<-raster("wd_10m_37.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("wd_10m_38.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("wd_10m_39.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("wd_10m_40.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("wd_10m_41.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("wd_10m_42.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_noche<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_noche<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_noche<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
  }
  #**************************************************************************************************
  
  #**************************************************************************************************
  #Periodo de la madrugada
  {
    #Carga los datos necesarios
    #----- Nubosidad
    a<-raster("wd_10m_43.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("wd_10m_44.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("wd_10m_45.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("wd_10m_46.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("wd_10m_47.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("wd_10m_48.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_madrugada<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_madrugada<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_madrugada<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
    
  }
  #**************************************************************************************************
  
  #---------------------------------------------------------------------------------------------------------------------------------------->
  {
    nubosidad_promedio<-rbind(prom_nubes_manana,prom_nubes_tarde,prom_nubes_noche,prom_nubes_madrugada)
    nubosidad_max<-rbind(max_nubes_manana,max_nubes_tarde,max_nubes_noche,max_nubes_madrugada)
    nubosidad_min<-rbind(min_nubes_manana,min_nubes_tarde,min_nubes_noche,min_nubes_madrugada)
    
    nubes<-cbind(lista.periodos,nubosidad_promedio,nubosidad_max,nubosidad_min)
    write.table(nubes,file="direccion_viento.csv",sep = ";",quote = FALSE,row.names = FALSE)
  }
  #---------------------------------------------------------------------------------------------------------------------------------------->
}
############################################################################################################################################################

#----- Humedad relativa
############################################################################################################################################################
{
  #**************************************************************************************************
  #Periodo de la manana 
  {
    #Carga los datos necesarios
    
    a<-raster("humedad_25.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("humedad_26.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("humedad_27.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("humedad_28.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("humedad_29.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("humedad_30.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_manana<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_manana<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_manana<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
  }
  #**************************************************************************************************
  
  #**************************************************************************************************
  #Periodo de la tarde
  {
    #Carga los datos necesarios
    #----- Nubosidad
    a<-raster("humedad_31.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("humedad_32.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("humedad_33.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("humedad_34.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("humedad_35.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("humedad_36.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_tarde<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_tarde<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_tarde<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
  }
  #**************************************************************************************************
  
  #**************************************************************************************************
  #Periodo de la noche
  {
    #Carga los datos necesarios
    #----- Nubosidad
    a<-raster("humedad_37.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("humedad_38.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("humedad_39.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("humedad_40.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("humedad_41.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("humedad_42.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_noche<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_noche<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_noche<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
  }
  #**************************************************************************************************
  
  #**************************************************************************************************
  #Periodo de la madrugada
  {
    #Carga los datos necesarios
    #----- Nubosidad
    a<-raster("humedad_43.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("humedad_44.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("humedad_45.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("humedad_46.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("humedad_47.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("humedad_48.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_madrugada<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_madrugada<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_madrugada<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
    
  }
  #**************************************************************************************************
  
  #---------------------------------------------------------------------------------------------------------------------------------------->
  {
    nubosidad_promedio<-rbind(prom_nubes_manana,prom_nubes_tarde,prom_nubes_noche,prom_nubes_madrugada)
    nubosidad_max<-rbind(max_nubes_manana,max_nubes_tarde,max_nubes_noche,max_nubes_madrugada)
    nubosidad_min<-rbind(min_nubes_manana,min_nubes_tarde,min_nubes_noche,min_nubes_madrugada)
    
    nubes<-cbind(lista.periodos,nubosidad_promedio,nubosidad_max,nubosidad_min)
    write.table(nubes,file="humedad_relativa.csv",sep = ";",quote = FALSE,row.names = FALSE)
  }
  #---------------------------------------------------------------------------------------------------------------------------------------->
}
############################################################################################################################################################

#----- Presion
############################################################################################################################################################
{
  #**************************************************************************************************
  #Periodo de la manana 
  {
    #Carga los datos necesarios
    
    a<-raster("presion_25.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("presion_26.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("presion_27.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("presion_28.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("presion_29.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("presion_30.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_manana<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_manana<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_manana<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
  }
  #**************************************************************************************************
  
  #**************************************************************************************************
  #Periodo de la tarde
  {
    #Carga los datos necesarios
    #----- Nubosidad
    a<-raster("presion_31.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("presion_32.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("presion_33.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("presion_34.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("presion_35.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("presion_36.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_tarde<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_tarde<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_tarde<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
  }
  #**************************************************************************************************
  
  #**************************************************************************************************
  #Periodo de la noche
  {
    #Carga los datos necesarios
    #----- Nubosidad
    a<-raster("presion_37.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("presion_38.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("presion_39.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("presion_40.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("presion_41.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("presion_42.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_noche<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_noche<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_noche<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
  }
  #**************************************************************************************************
  
  #**************************************************************************************************
  #Periodo de la madrugada
  {
    #Carga los datos necesarios
    #----- Nubosidad
    a<-raster("presion_43.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("presion_44.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("presion_45.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("presion_46.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("presion_47.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("presion_48.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_madrugada<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_madrugada<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_madrugada<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
    
  }
  #**************************************************************************************************
  
  #---------------------------------------------------------------------------------------------------------------------------------------->
  {
    nubosidad_promedio<-rbind(prom_nubes_manana,prom_nubes_tarde,prom_nubes_noche,prom_nubes_madrugada)
    nubosidad_max<-rbind(max_nubes_manana,max_nubes_tarde,max_nubes_noche,max_nubes_madrugada)
    nubosidad_min<-rbind(min_nubes_manana,min_nubes_tarde,min_nubes_noche,min_nubes_madrugada)
    
    nubes<-cbind(lista.periodos,nubosidad_promedio,nubosidad_max,nubosidad_min)
    write.table(nubes,file="presion.csv",sep = ";",quote = FALSE,row.names = FALSE)
  }
  #---------------------------------------------------------------------------------------------------------------------------------------->
}
############################################################################################################################################################

#----- Lluvia
############################################################################################################################################################
{
  #**************************************************************************************************
  #Periodo de la manana 
  {
    #Carga los datos necesarios
    
    a<-raster("lluvia_25.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("lluvia_26.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("lluvia_27.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("lluvia_28.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("lluvia_29.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("lluvia_30.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_manana<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_manana<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_manana<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
  }
  #**************************************************************************************************
  
  #**************************************************************************************************
  #Periodo de la tarde
  {
    #Carga los datos necesarios
    #----- Nubosidad
    a<-raster("lluvia_31.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("lluvia_32.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("lluvia_33.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("lluvia_34.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("lluvia_35.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("lluvia_36.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_tarde<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_tarde<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_tarde<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
  }
  #**************************************************************************************************
  
  #**************************************************************************************************
  #Periodo de la noche
  {
    #Carga los datos necesarios
    #----- Nubosidad
    a<-raster("lluvia_37.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("lluvia_38.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("lluvia_39.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("lluvia_40.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("lluvia_41.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("lluvia_42.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_noche<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_noche<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_noche<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
  }
  #**************************************************************************************************
  
  #**************************************************************************************************
  #Periodo de la madrugada
  {
    #Carga los datos necesarios
    #----- Nubosidad
    a<-raster("lluvia_43.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("lluvia_44.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("lluvia_45.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("lluvia_46.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("lluvia_47.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("lluvia_48.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_madrugada<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_madrugada<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_madrugada<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
    
  }
  #**************************************************************************************************
  
  #---------------------------------------------------------------------------------------------------------------------------------------->
  {
    nubosidad_promedio<-rbind(prom_nubes_manana,prom_nubes_tarde,prom_nubes_noche,prom_nubes_madrugada)
    nubosidad_max<-rbind(max_nubes_manana,max_nubes_tarde,max_nubes_noche,max_nubes_madrugada)
    nubosidad_min<-rbind(min_nubes_manana,min_nubes_tarde,min_nubes_noche,min_nubes_madrugada)
    
    nubes<-cbind(lista.periodos,nubosidad_promedio,nubosidad_max,nubosidad_min)
    write.table(nubes,file="lluvia.csv",sep = ";",quote = FALSE,row.names = FALSE)
  }
  #---------------------------------------------------------------------------------------------------------------------------------------->
}
############################################################################################################################################################

#----- Temperatura
############################################################################################################################################################
{
  #**************************************************************************************************
  #Periodo de la manana 
  {
    #Carga los datos necesarios
    
    a<-raster("temperatura_25.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("temperatura_26.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("temperatura_27.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("temperatura_28.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("temperatura_29.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("temperatura_30.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_manana<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_manana<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_manana<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
  }
  #**************************************************************************************************
  
  #**************************************************************************************************
  #Periodo de la tarde
  {
    #Carga los datos necesarios
    #----- Nubosidad
    a<-raster("temperatura_31.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("temperatura_32.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("temperatura_33.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("temperatura_34.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("temperatura_35.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("temperatura_36.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_tarde<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_tarde<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_tarde<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
  }
  #**************************************************************************************************
  
  #**************************************************************************************************
  #Periodo de la noche
  {
    #Carga los datos necesarios
    #----- Nubosidad
    a<-raster("temperatura_37.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("temperatura_38.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("temperatura_39.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("temperatura_40.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("temperatura_41.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("temperatura_42.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_noche<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_noche<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_noche<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
  }
  #**************************************************************************************************
  
  #**************************************************************************************************
  #Periodo de la madrugada
  {
    #Carga los datos necesarios
    #----- Nubosidad
    a<-raster("temperatura_43.tif")
    extent(a)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(a) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.a.p1 <- extract(a,lista.puntos[[1]],method='bilinear')
    variable.a.p2 <- extract(a,lista.puntos[[2]],method='bilinear')
    variable.a.p3 <- extract(a,lista.puntos[[3]],method='bilinear')
    variable.a.p4 <- extract(a,lista.puntos[[4]],method='bilinear')
    
    b<-raster("temperatura_44.tif")
    extent(b)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(b) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.b.p1 <- extract(b,lista.puntos[[1]],method='bilinear')
    variable.b.p2 <- extract(b,lista.puntos[[2]],method='bilinear')
    variable.b.p3 <- extract(b,lista.puntos[[3]],method='bilinear')
    variable.b.p4 <- extract(b,lista.puntos[[4]],method='bilinear')
    
    c<-raster("temperatura_45.tif")
    extent(c)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(c) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.c.p1 <- extract(c,lista.puntos[[1]],method='bilinear')
    variable.c.p2 <- extract(c,lista.puntos[[2]],method='bilinear')
    variable.c.p3 <- extract(c,lista.puntos[[3]],method='bilinear')
    variable.c.p4 <- extract(c,lista.puntos[[4]],method='bilinear')
    
    d<-raster("temperatura_46.tif")
    extent(d)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(d) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.d.p1 <- extract(d,lista.puntos[[1]],method='bilinear')
    variable.d.p2 <- extract(d,lista.puntos[[2]],method='bilinear')
    variable.d.p3 <- extract(d,lista.puntos[[3]],method='bilinear')
    variable.d.p4 <- extract(d,lista.puntos[[4]],method='bilinear')
    
    e<-raster("temperatura_47.tif")
    extent(e)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(e) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.e.p1 <- extract(e,lista.puntos[[1]],method='bilinear')
    variable.e.p2 <- extract(e,lista.puntos[[2]],method='bilinear')
    variable.e.p3 <- extract(e,lista.puntos[[3]],method='bilinear')
    variable.e.p4 <- extract(e,lista.puntos[[4]],method='bilinear')
    
    f<-raster("temperatura_48.tif")
    extent(f)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
    projection(f) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    variable.f.p1 <- extract(f,lista.puntos[[1]],method='bilinear')
    variable.f.p2 <- extract(f,lista.puntos[[2]],method='bilinear')
    variable.f.p3 <- extract(f,lista.puntos[[3]],method='bilinear')
    variable.f.p4 <- extract(f,lista.puntos[[4]],method='bilinear')
    
    #Genera los estadisticos
    #Promedio
    promedio.variable.p1<-(variable.a.p1+variable.b.p1+variable.c.p1+variable.d.p1+variable.e.p1+variable.f.p1)/6
    promedio.variable.p2<-(variable.a.p2+variable.b.p2+variable.c.p2+variable.d.p2+variable.e.p2+variable.f.p2)/6
    promedio.variable.p3<-(variable.a.p3+variable.b.p3+variable.c.p3+variable.d.p3+variable.e.p3+variable.f.p3)/6
    promedio.variable.p4<-(variable.a.p4+variable.b.p4+variable.c.p4+variable.d.p4+variable.e.p4+variable.f.p4)/6
    
    #Maximos
    max_var.p1<-max(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    max_var.p2<-max(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    max_var.p3<-max(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    max_var.p4<-max(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Minimos
    min_var.p1<-min(variable.a.p1,variable.b.p1,variable.c.p1,variable.d.p1,variable.e.p1,variable.f.p1)
    min_var.p2<-min(variable.a.p2,variable.b.p2,variable.c.p2,variable.d.p2,variable.e.p2,variable.f.p2)
    min_var.p3<-min(variable.a.p3,variable.b.p3,variable.c.p3,variable.d.p3,variable.e.p3,variable.f.p3)
    min_var.p4<-min(variable.a.p4,variable.b.p4,variable.c.p4,variable.d.p4,variable.e.p4,variable.f.p4)
    
    #Resumen nubes
    prom_nubes_madrugada<-cbind(promedio.variable.p1,promedio.variable.p2,promedio.variable.p3,promedio.variable.p4)
    max_nubes_madrugada<-cbind(max_var.p1,max_var.p2,max_var.p3,max_var.p4)
    min_nubes_madrugada<-cbind(min_var.p1,min_var.p2,min_var.p3,min_var.p4)
    
  }
  #**************************************************************************************************
  
  #---------------------------------------------------------------------------------------------------------------------------------------->
  {
    nubosidad_promedio<-rbind(prom_nubes_manana,prom_nubes_tarde,prom_nubes_noche,prom_nubes_madrugada)
    nubosidad_max<-rbind(max_nubes_manana,max_nubes_tarde,max_nubes_noche,max_nubes_madrugada)
    nubosidad_min<-rbind(min_nubes_manana,min_nubes_tarde,min_nubes_noche,min_nubes_madrugada)
    
    nubes<-cbind(lista.periodos,nubosidad_promedio,nubosidad_max,nubosidad_min)
    write.table(nubes,file="temperatura.csv",sep = ";",quote = FALSE,row.names = FALSE)
  }
  #---------------------------------------------------------------------------------------------------------------------------------------->
}
############################################################################################################################################################
