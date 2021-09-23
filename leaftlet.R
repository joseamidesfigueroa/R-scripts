library(raster)
library(sp)
library(leaflet)
library(htmlwidgets)


setwd("C:/R")
rast<-raster("acum_general_07.tif")
extent(rast)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
projection(rast) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

prob<-raster("prob_50mm_24h.tif")
prob


m <- leaflet() %>% setView(lng = -89, lat = 13, zoom = 7)
m %>% addProviderTiles(providers$Stamen.Toner)

mapa <- leaflet() %>% setView(lng = -89, lat = 13, zoom = 7)
mapa <- addTiles(mapa)
mapa <- addMarkers(mapa,lng = -89.5, lat = 13.89, popup = "Hola Mundo")

#!That ROCKS
#------------------------------------------------------------------------>
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(rast), alpha = TRUE,
                    na.color = "transparent")
m<- leaflet() %>% addTiles() %>%
  addRasterImage(rast, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(rast),
            title = "Precipitacion")

saveWidget(m, file="m.html")
#------------------------------------------------------------------------>


#------------------------------------------------------------------------>
pal <- colorNumeric(c("#FFFFFF00", "#0C2C84"), values(prob), alpha = TRUE,
                    na.color = "transparent")
pal2<- colorNumeric(c("#FFFFFF", "#0C2C84"), values(prob),
                    na.color = "transparent")
prob_map<- leaflet() %>% addTiles() %>%
  addRasterImage(prob, colors = pal, opacity = 0.95) %>%
  addLegend(pal = pal2, values = values(prob),
            title = "Precipitacion mayor de 50mm")
prob_map
saveWidget(prob_map, file="prob.html")
#------------------------------------------------------------------------>