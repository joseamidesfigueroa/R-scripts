library(raster)

#Mayo
#--------------------------------------------------------------------------------->
dir<-"C:/Users/Amides/Documents/Trabajo/2021/Abril/FORO/CI05_Abril2021/m1_mes1/"

#archivos<-list
#for (i in c(1:31)) {
  d1<-raster(paste(dir,"acumulado_lluvia_total_dia_1.tif",sep = ""))
  d2<-raster(paste(dir,"acumulado_lluvia_total_dia_2.tif",sep = ""))
  d3<-raster(paste(dir,"acumulado_lluvia_total_dia_3.tif",sep = ""))
  d4<-raster(paste(dir,"acumulado_lluvia_total_dia_4.tif",sep = ""))
  d5<-raster(paste(dir,"acumulado_lluvia_total_dia_5.tif",sep = ""))
  d6<-raster(paste(dir,"acumulado_lluvia_total_dia_6.tif",sep = ""))
  d7<-raster(paste(dir,"acumulado_lluvia_total_dia_7.tif",sep = ""))
  d8<-raster(paste(dir,"acumulado_lluvia_total_dia_8.tif",sep = ""))
  d9<-raster(paste(dir,"acumulado_lluvia_total_dia_9.tif",sep = ""))
  d10<-raster(paste(dir,"acumulado_lluvia_total_dia_10.tif",sep = ""))
  
  d11<-raster(paste(dir,"acumulado_lluvia_total_dia_11.tif",sep = ""))
  d12<-raster(paste(dir,"acumulado_lluvia_total_dia_12.tif",sep = ""))
  d13<-raster(paste(dir,"acumulado_lluvia_total_dia_13.tif",sep = ""))
  d14<-raster(paste(dir,"acumulado_lluvia_total_dia_14.tif",sep = ""))
  d15<-raster(paste(dir,"acumulado_lluvia_total_dia_15.tif",sep = ""))
  d16<-raster(paste(dir,"acumulado_lluvia_total_dia_16.tif",sep = ""))
  d17<-raster(paste(dir,"acumulado_lluvia_total_dia_17.tif",sep = ""))
  d18<-raster(paste(dir,"acumulado_lluvia_total_dia_18.tif",sep = ""))
  d19<-raster(paste(dir,"acumulado_lluvia_total_dia_19.tif",sep = ""))
  d20<-raster(paste(dir,"acumulado_lluvia_total_dia_20.tif",sep = ""))
  
  d21<-raster(paste(dir,"acumulado_lluvia_total_dia_21.tif",sep = ""))
  d22<-raster(paste(dir,"acumulado_lluvia_total_dia_22.tif",sep = ""))
  d23<-raster(paste(dir,"acumulado_lluvia_total_dia_23.tif",sep = ""))
  d24<-raster(paste(dir,"acumulado_lluvia_total_dia_24.tif",sep = ""))
  d25<-raster(paste(dir,"acumulado_lluvia_total_dia_25.tif",sep = ""))
  d26<-raster(paste(dir,"acumulado_lluvia_total_dia_26.tif",sep = ""))
  d27<-raster(paste(dir,"acumulado_lluvia_total_dia_27.tif",sep = ""))
  d28<-raster(paste(dir,"acumulado_lluvia_total_dia_28.tif",sep = ""))
  d29<-raster(paste(dir,"acumulado_lluvia_total_dia_29.tif",sep = ""))
  d30<-raster(paste(dir,"acumulado_lluvia_total_dia_30.tif",sep = ""))
  d31<-raster(paste(dir,"acumulado_lluvia_total_dia_31.tif",sep = ""))
  
#  plot(archivos[[i]])
#}

acumulado_mayo<-d1+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11+d12+d13+d14+d15+d16+d17+d18+d19+d20+d21+d22+d23+d24+d25+d26+d27+d28+d29+d30+d31
extent(acumulado_mayo)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
#--------------------------------------------------------------------------------->

plot(acumulado_mayo)

#Junio
#--------------------------------------------------------------------------------->
dir<-"C:/Users/Amides/Documents/Trabajo/2021/Abril/FORO/CI05_Abril2021/m1_mes2/"

#archivos<-list
#for (i in c(1:31)) {
d1<-raster(paste(dir,"acumulado_lluvia_total_dia_1.tif",sep = ""))
d2<-raster(paste(dir,"acumulado_lluvia_total_dia_2.tif",sep = ""))
d3<-raster(paste(dir,"acumulado_lluvia_total_dia_3.tif",sep = ""))
d4<-raster(paste(dir,"acumulado_lluvia_total_dia_4.tif",sep = ""))
d5<-raster(paste(dir,"acumulado_lluvia_total_dia_5.tif",sep = ""))
d6<-raster(paste(dir,"acumulado_lluvia_total_dia_6.tif",sep = ""))
d7<-raster(paste(dir,"acumulado_lluvia_total_dia_7.tif",sep = ""))
d8<-raster(paste(dir,"acumulado_lluvia_total_dia_8.tif",sep = ""))
d9<-raster(paste(dir,"acumulado_lluvia_total_dia_9.tif",sep = ""))
d10<-raster(paste(dir,"acumulado_lluvia_total_dia_10.tif",sep = ""))

d11<-raster(paste(dir,"acumulado_lluvia_total_dia_11.tif",sep = ""))
d12<-raster(paste(dir,"acumulado_lluvia_total_dia_12.tif",sep = ""))
d13<-raster(paste(dir,"acumulado_lluvia_total_dia_13.tif",sep = ""))
d14<-raster(paste(dir,"acumulado_lluvia_total_dia_14.tif",sep = ""))
d15<-raster(paste(dir,"acumulado_lluvia_total_dia_15.tif",sep = ""))
d16<-raster(paste(dir,"acumulado_lluvia_total_dia_16.tif",sep = ""))
d17<-raster(paste(dir,"acumulado_lluvia_total_dia_17.tif",sep = ""))
d18<-raster(paste(dir,"acumulado_lluvia_total_dia_18.tif",sep = ""))
d19<-raster(paste(dir,"acumulado_lluvia_total_dia_19.tif",sep = ""))
d20<-raster(paste(dir,"acumulado_lluvia_total_dia_20.tif",sep = ""))

d21<-raster(paste(dir,"acumulado_lluvia_total_dia_21.tif",sep = ""))
d22<-raster(paste(dir,"acumulado_lluvia_total_dia_22.tif",sep = ""))
d23<-raster(paste(dir,"acumulado_lluvia_total_dia_23.tif",sep = ""))
d24<-raster(paste(dir,"acumulado_lluvia_total_dia_24.tif",sep = ""))
d25<-raster(paste(dir,"acumulado_lluvia_total_dia_25.tif",sep = ""))
d26<-raster(paste(dir,"acumulado_lluvia_total_dia_26.tif",sep = ""))
d27<-raster(paste(dir,"acumulado_lluvia_total_dia_27.tif",sep = ""))
d28<-raster(paste(dir,"acumulado_lluvia_total_dia_28.tif",sep = ""))
d29<-raster(paste(dir,"acumulado_lluvia_total_dia_29.tif",sep = ""))
d30<-raster(paste(dir,"acumulado_lluvia_total_dia_30.tif",sep = ""))
#d31<-raster(paste(dir,"acumulado_lluvia_total_dia_31.tif",sep = ""))

#  plot(archivos[[i]])
#}

acumulado_junio<-d1+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11+d12+d13+d14+d15+d16+d17+d18+d19+d20+d21+d22+d23+d24+d25+d26+d27+d28+d29+d30#+d31
extent(acumulado_junio)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
#--------------------------------------------------------------------------------->


#Julio
#--------------------------------------------------------------------------------->
dir<-"C:/Users/Amides/Documents/Trabajo/2021/Abril/FORO/CI05_Abril2021/m1_mes3/"

#archivos<-list
#for (i in c(1:31)) {
d1<-raster(paste(dir,"acumulado_lluvia_total_dia_1.tif",sep = ""))
d2<-raster(paste(dir,"acumulado_lluvia_total_dia_2.tif",sep = ""))
d3<-raster(paste(dir,"acumulado_lluvia_total_dia_3.tif",sep = ""))
d4<-raster(paste(dir,"acumulado_lluvia_total_dia_4.tif",sep = ""))
d5<-raster(paste(dir,"acumulado_lluvia_total_dia_5.tif",sep = ""))
d6<-raster(paste(dir,"acumulado_lluvia_total_dia_6.tif",sep = ""))
d7<-raster(paste(dir,"acumulado_lluvia_total_dia_7.tif",sep = ""))
d8<-raster(paste(dir,"acumulado_lluvia_total_dia_8.tif",sep = ""))
d9<-raster(paste(dir,"acumulado_lluvia_total_dia_9.tif",sep = ""))
d10<-raster(paste(dir,"acumulado_lluvia_total_dia_10.tif",sep = ""))

d11<-raster(paste(dir,"acumulado_lluvia_total_dia_11.tif",sep = ""))
d12<-raster(paste(dir,"acumulado_lluvia_total_dia_12.tif",sep = ""))
d13<-raster(paste(dir,"acumulado_lluvia_total_dia_13.tif",sep = ""))
d14<-raster(paste(dir,"acumulado_lluvia_total_dia_14.tif",sep = ""))
d15<-raster(paste(dir,"acumulado_lluvia_total_dia_15.tif",sep = ""))
d16<-raster(paste(dir,"acumulado_lluvia_total_dia_16.tif",sep = ""))
d17<-raster(paste(dir,"acumulado_lluvia_total_dia_17.tif",sep = ""))
d18<-raster(paste(dir,"acumulado_lluvia_total_dia_18.tif",sep = ""))
d19<-raster(paste(dir,"acumulado_lluvia_total_dia_19.tif",sep = ""))
d20<-raster(paste(dir,"acumulado_lluvia_total_dia_20.tif",sep = ""))

d21<-raster(paste(dir,"acumulado_lluvia_total_dia_21.tif",sep = ""))
d22<-raster(paste(dir,"acumulado_lluvia_total_dia_22.tif",sep = ""))
d23<-raster(paste(dir,"acumulado_lluvia_total_dia_23.tif",sep = ""))
d24<-raster(paste(dir,"acumulado_lluvia_total_dia_24.tif",sep = ""))
d25<-raster(paste(dir,"acumulado_lluvia_total_dia_25.tif",sep = ""))
d26<-raster(paste(dir,"acumulado_lluvia_total_dia_26.tif",sep = ""))
d27<-raster(paste(dir,"acumulado_lluvia_total_dia_27.tif",sep = ""))
d28<-raster(paste(dir,"acumulado_lluvia_total_dia_28.tif",sep = ""))
d29<-raster(paste(dir,"acumulado_lluvia_total_dia_29.tif",sep = ""))
d30<-raster(paste(dir,"acumulado_lluvia_total_dia_30.tif",sep = ""))
d31<-raster(paste(dir,"acumulado_lluvia_total_dia_31.tif",sep = ""))

#  plot(archivos[[i]])
#}

acumulado_julio<-d1+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11+d12+d13+d14+d15+d16+d17+d18+d19+d20+d21+d22+d23+d24+d25+d26+d27+d28+d29+d30+d31
extent(acumulado_julio)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
#--------------------------------------------------------------------------------->



#Agosto 
#--------------------------------------------------------------------------------->
dir<-"C:/Users/Amides/Documents/Trabajo/2021/Abril/FORO/CI05/m1_mes5/"

#archivos<-list
#for (i in c(1:31)) {
d1<-raster(paste(dir,"acumulado_lluvia_total_dia_1.tif",sep = ""))
d2<-raster(paste(dir,"acumulado_lluvia_total_dia_2.tif",sep = ""))
d3<-raster(paste(dir,"acumulado_lluvia_total_dia_3.tif",sep = ""))
d4<-raster(paste(dir,"acumulado_lluvia_total_dia_4.tif",sep = ""))
d5<-raster(paste(dir,"acumulado_lluvia_total_dia_5.tif",sep = ""))
d6<-raster(paste(dir,"acumulado_lluvia_total_dia_6.tif",sep = ""))
d7<-raster(paste(dir,"acumulado_lluvia_total_dia_7.tif",sep = ""))
d8<-raster(paste(dir,"acumulado_lluvia_total_dia_8.tif",sep = ""))
d9<-raster(paste(dir,"acumulado_lluvia_total_dia_9.tif",sep = ""))
d10<-raster(paste(dir,"acumulado_lluvia_total_dia_10.tif",sep = ""))

d11<-raster(paste(dir,"acumulado_lluvia_total_dia_11.tif",sep = ""))
d12<-raster(paste(dir,"acumulado_lluvia_total_dia_12.tif",sep = ""))
d13<-raster(paste(dir,"acumulado_lluvia_total_dia_13.tif",sep = ""))
d14<-raster(paste(dir,"acumulado_lluvia_total_dia_14.tif",sep = ""))
d15<-raster(paste(dir,"acumulado_lluvia_total_dia_15.tif",sep = ""))
d16<-raster(paste(dir,"acumulado_lluvia_total_dia_16.tif",sep = ""))
d17<-raster(paste(dir,"acumulado_lluvia_total_dia_17.tif",sep = ""))
d18<-raster(paste(dir,"acumulado_lluvia_total_dia_18.tif",sep = ""))
d19<-raster(paste(dir,"acumulado_lluvia_total_dia_19.tif",sep = ""))
d20<-raster(paste(dir,"acumulado_lluvia_total_dia_20.tif",sep = ""))

d21<-raster(paste(dir,"acumulado_lluvia_total_dia_21.tif",sep = ""))
d22<-raster(paste(dir,"acumulado_lluvia_total_dia_22.tif",sep = ""))
d23<-raster(paste(dir,"acumulado_lluvia_total_dia_23.tif",sep = ""))
d24<-raster(paste(dir,"acumulado_lluvia_total_dia_24.tif",sep = ""))
d25<-raster(paste(dir,"acumulado_lluvia_total_dia_25.tif",sep = ""))
d26<-raster(paste(dir,"acumulado_lluvia_total_dia_26.tif",sep = ""))
d27<-raster(paste(dir,"acumulado_lluvia_total_dia_27.tif",sep = ""))
d28<-raster(paste(dir,"acumulado_lluvia_total_dia_28.tif",sep = ""))
d29<-raster(paste(dir,"acumulado_lluvia_total_dia_29.tif",sep = ""))
d30<-raster(paste(dir,"acumulado_lluvia_total_dia_30.tif",sep = ""))
d31<-raster(paste(dir,"acumulado_lluvia_total_dia_31.tif",sep = ""))

#  plot(archivos[[i]])
#}

acumulado_agosto<-d1+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11+d12+d13+d14+d15+d16+d17+d18+d19+d20+d21+d22+d23+d24+d25+d26+d27+d28+d29+d30+d31
extent(acumulado_agosto)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
#--------------------------------------------------------------------------------->

#------------------------------------------------------------------------------------------------------------
#Crea un raster con el area efectiva de trabajo (RASTER DUMMY)
m<-matrix(runif(400),20,20)
area_efectiva<-raster(m)
extent(area_efectiva)<-c(264.2-360,291.8855-360,6.849999,23.38065)
projection(area_efectiva) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#------------------------------------------------------------------------------------------------------------

trimestre<-acumulado_mayo+acumulado_junio+acumulado_julio
cuatrimestre<-acumulado_mayo+acumulado_junio+acumulado_julio+acumulado_agosto

ras<-raster("C:/R/temporal/27km/CHIRP.2021.01.tif")

trimestre<-crop(trimestre,area_efectiva)
resample(trimestre,ras)
cuatrimestre<-crop(cuatrimestre,area_efectiva)
resample(cuatrimestre,ras)

rf<-writeRaster(trimestre, filename="C:/Users/Amides/Documents/Trabajo/2021/Abril/FORO/trimestre_mjj_2021.tif", overwrite=TRUE)
rf<-writeRaster(cuatrimestre, filename="C:/Users/Amides/Documents/Trabajo/2021/Abril/FORO/cuattrimestre_mjja_2021.tif", overwrite=TRUE)
