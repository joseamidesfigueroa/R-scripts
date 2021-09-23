#############################################################################
#																		 	#
# Procesamiento y visualización de imágenes G16							 	#
# Desarrollado por:                                                      	#
# Dante Castro Garro (dantecastro@lamolina.edu.pe)                       	#
# Shirley Contreras Acho                                                	#
# Lisbeth Villanueva García                                             	#
# Patricia Infante Castro                                                 	#
#                                                                         	#
# Versión adapatada de los scripts en Python del blog: GEONETCast-Americas	#
# https://geonetcast.wordpress.com/gnc-a-product-manipulation-tutorials/	#
#																			#
#############################################################################

#----------------------------
#LIBRERIAS

library(ncdf4)
library(RColorBrewer)
library(fields)
library(maps)
library(gdalUtils)
library(rgdal)

#-----------------------------
#FUNCIONES
source('aux_funcs.r')
source('g16_func.r')
source('g16_graphs.r')

#----------------------------
#PROCESAMIENTO DE IMÁGENES

#Ubicar los drivers del GDAL
ubicar_gdal(TRUE)

#Procesamiento de imagen
inp <- 'OR_ABI-L2-CMIPF-M3C13_G16_s20180501200390_e20180501211168_c20180501211254.nc'
out <- 'OR_ABI-L2-CMIPF-M3C13_G16_s20180501200390.nc'
corte <- c(-85,2,-65,-20)
g16_corte(inp, out, corte)

#----------------------------
#PLOTEO DE IMAGEN

#Abriendo el archivo de cortado, extrayendo las variables y luego cerrando el archivo
g16_nc <- nc_open(out)

g16_dat <- ncvar_get(g16_nc, 'Band1')
g16_lat <- ncvar_get(g16_nc, 'lat')
g16_lon <- ncvar_get(g16_nc, 'lon')

nc_close(g16_nc)

#Recortando para unas coordenadas determinadas 
min_lon <- -83
max_lon <- -67
min_lat <- -19
max_lat <- 1

g16_dat <- g16_dat[g16_lon >= min_lon & g16_lon <= max_lon, g16_lat >= min_lat & g16_lat <= max_lat]
g16_lon <- g16_lon[g16_lon >= min_lon & g16_lon <= max_lon]
g16_lat <- g16_lat[g16_lat >= min_lat & g16_lat <= max_lat]

#Numero de banda
bnd <- as.integer(substr(out, 20, 21))

#Modificando datos a temperatura de brillo
g16_dat <- fact(g16_dat, bnd)

#Shapes de los países
shp <- readOGR(dsn=path.expand("E:/VLAB/R/Shapefiles"), layer="ne_10m_admin_0_countries")

#----------------------------------
#GRAFICA
fig <- 'Band13.png'

#Archivo de salida
png(fig, width = (741*3), height = (638*3), res = 300)

g16.graph(out, g16_dat, g16_lon, g16_lat, bnd, shp)

dev.off()
