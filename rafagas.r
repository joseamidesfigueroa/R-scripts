library(ggplot2)
library(mapdata)

setwd("C:/R/")
dat<-read.table("raf.csv",sep = ";")
names(dat)<-c("Longitud","Latitud","KT")
mapa<-map_data("worldHires",region ="El Salvador")

titulo<-"Velocidad de ráfagas registradas el 29 de julio de 2020 en KT"

rafagas<-ggplot()+ geom_polygon(data = mapa, aes(x=long, y = lat, group = group), fill = NA, color = "black") +
        geom_point(aes(x=Longitud, y=Latitud, size=KT),data = dat)+
        ggtitle(titulo)+
        labs(x = "Longitud",y = "Latitud")+
        coord_map(projection = "mercator") +
        lims(x = c(-90.25, -87.4), y = c(13, 14.5))

rafagas 
