#Funciones para el corte de l
g16_corte <- function(input, output, corte, formato = 'netCDF'){

  arch <- paste0('HDF5:', input, '://CMI')

  #Asignando sistema de coordenadas geostacionario
  gdal_translate(
    src_dataset = arch,
    dst_dataset = 'G16_temp.nc', 
    of = 'netCDF', 
    a_srs = '+proj=geos +a=6378137. +b=6356752.31414 +lon_0=-89.5 +f=.00335281068119356027 +h=35786023. +sweep=x', 
    a_ullr = c(-5434894.885056,5434894.885056,5434894.885056,-5434894.885056), 
    a_nodata = -1
  )
  
  #Recortando 
  gdal_translate(
    src_dataset = 'G16_temp.nc',
    dst_dataset = 'G16_crop.nc', 
    of = 'netCDF', 
    projwin = corte,
    projwin_srs = '+proj=latlong +datum=WGS84'
  )
  
  file.remove('G16_temp.nc')
  
  #Convirtiendo de geoestacionario a geográfica-latlon
  gdalwarp(
    srcfile = 'G16_crop.nc', 
    dstfile = output, 
    of = formato, 
    t_srs = '+proj=latlong +datum=WGS84',
    overwrite = T
  )
  
  file.remove('G16_crop.nc')

}

#-----------------------------------------------------------------------
#Funcion para ubicar los drivers del GLDA de ser necesario
ubicar_gdal <- function(x){

  if(x){
    gdal_setInstallation(search_path = 'C:/Program Files/QGIS 2.18/bin')
  }else{
    print('Se asume que los drivers fueron ubicados previamente')
  }

}
