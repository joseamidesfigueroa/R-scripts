#Función para crear la gráfica
g16.graph <- function(nomb, dat, lon, lat, band, shp){

  par(mar = c(3,6,1,3))
  
  breaks <- g16.brk(band)
  pal.goes <- g16.pal(band)
  
  #Gráfica de contornos
  filled.contour2(
  	lon,
  	lat,
  	dat[ , ],
  	xlim = c(min(lon), max(lon)),
  	ylim = c(min(lat), max(lat)),
  	zlim = range(breaks),
  	levels = breaks,
  	axes = TRUE,
  	col = colorRampPalette(pal.goes, space='Lab')(length(breaks)-1)
  )
  
  #Contorno de los países
  map(shp, add=TRUE, lwd= 0.9,
      col = ifelse(band <= 7, 'yellow', 'black'))
  
  #Poniendo texto en imagen (lili)
  showlat <- (max(lat) - min(lat))*0.05 + min(lat)
  showlon <- (max(lon) - min(lon))*0.25 + min(lon)
  text(showlon, showlat, g16.not(nomb), 
       col = ifelse(band <= 7, 'yellow', 'black'))
  
  #Leyenda de la gráfica
  par(mar = c(3,4,1,32))
  par(new = TRUE)
  image.scale(
  	dat,
  	zlim = range(dat),
  	breaks = breaks,
  	col = colorRampPalette(pal.goes, space='Lab')(length(breaks)-1),
  	horiz = FALSE,
  	xlab = '',
  	ylab = ifelse(band <= 6, 'Reflectancia (%)', expression('Temperatura de brillo ('*degree*'C)')),
  	yaxt = 'n',
  	las = 2
  )
  
  #Parámetros de la leyenda
  # axis(2, at = breaks[seq(1, length(breaks), 5)], las = 2)
  g16.ley(band, breaks)
  
  box()

}

#Función con los valores de escala y desfase de las bandas del G16
fact <- function(dat, band){

  if(band == 1){dat <- dat*0.0003175*100}
  else if(band == 2){dat <- dat*0.0002442*100}
  else if(band == 7){dat <- dat*0.0130962 + 197.31 - 273.15}
  else if(band == 8){dat <- dat*0.0422499 + 138.05 - 273.15}
  else if(band == 9){dat <- dat*0.0423391 + 137.70 - 273.15}
  else if(band == 10){dat <- dat*0.0498892 + 126.91 - 273.15}
  else if(band == 12){dat <- dat*0.0472703 + 117.49 - 273.15}
  else if(band == 13){dat <- dat*0.0614533 + 89.62 - 273.15}
  else if(band == 14){dat <- dat*0.0598507 + 96.19 - 273.15}
  else if(band == 15){dat <- dat*0.0595608 + 97.38 - 273.15}

  return(dat)
  
}

#Función para la selección de una paleta de colores
g16.pal <- function(band){

  if(band <= 6){
  
    pal.goes <- colorRampPalette(c('black', 'white'))(100)
  
  }else if(band == 7){
  
    cya <- colorRampPalette(c('cyan','black'))(90)
	gr2 <- colorRampPalette(c('black', 'white'))(80)
	pal.goes <- c(cya,gr2)

   }else if(band %in% 8:10){
  
    sko <- colorRampPalette(c('cyan','olivedrab'))(35)
    olw <- colorRampPalette(c('olivedrab','white'))(12)
    wbl <- colorRampPalette(c('white','navyblue'))(25)
    bly <- colorRampPalette(c('navyblue','yellow'))(14)
    yre <- colorRampPalette(c('yellow','red'))(25)
    blk <- colorRampPalette(c('black','black'))(87)
    pal.goes <- c(sko,olw,wbl,bly,yre,blk)
  
  }else if(band %in% 11:16){
  
    mor <- colorRampPalette(c('darkmagenta','mediumorchid1'))(10)
    gr1 <- colorRampPalette(c('white','black'))(10)
    red <- colorRampPalette(c('darkred','red'))(5)
    org <- colorRampPalette(c('red','yellow'))(10)
    gre <- colorRampPalette(c('yellow','green'))(10)
    tae <- colorRampPalette(c('green','navyblue'))(10)
    blu <- colorRampPalette(c('navyblue','cyan'))(15)
    gr2 <- grey.colors(120,star=1,end=0)
    pal.goes <- c(mor,gr1,red,org,gre,tae,blu,gr2)

  }
	
  return(pal.goes)
  
}

#Función para selección de rangos
g16.brk <- function(band){

  if(band %in% 1:6){breaks <- 0:100}
  else if(band %in% 7){breaks <- -110:60}
  else if(band %in% 8:10){breaks <- -98:100}
  else if(band %in% 11:15){breaks <- -87:100}

  return(breaks)
  
}

#Función para modificar la leyenda
g16.ley <- function(band, brk){

  if(band %in% 1:6){axis(2, at = brk[seq(1, length(brk), 10)], las = 2)}
  else if(band %in% 7){axis(2, at = brk[seq(1, length(brk), 30)], las = 2)}
  else if(band %in% 8:10){axis(2, at = brk[seq(9, length(brk), 20)], las = 2)}
  else if(band %in% 11:15){axis(2, at = brk[seq(8, length(brk), 20)], las = 2)}

}

#Función para generar el texto de las imágenes
g16.not <- function(nomb){

  Anio <- substr(nomb, 28, 31)
  Diaj <- substr(nomb, 32, 34)
  Hora <- substr(nomb, 35, 36)
  Minuto <- substr(nomb, 37, 38)
  Banda <- substr(nomb, 20, 21)
  
  Anio <- as.integer(Anio)
  
  Fecha <- as.Date(as.integer(Diaj), origin = paste0(Anio-1,'-12-31'))
  
  fechita <- paste0(Fecha, ' ', Hora, ':', Minuto, ' UTC Banda ', Banda)
  
  return(fechita)

}

