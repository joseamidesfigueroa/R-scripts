plot(serie_temporal_promedio_nacional_rcp45, main="Serie de tiempo de la temperatura m�xima diaria promedio nacional para El Salvador desde 2006 a 2100\nModelo MPIESMMR_RCP45", ylab="Temperaturas Celsius", xlab="A�os",col="green",
     ylim=c(23,40))
plot(serie_temporal_promedio_nacional_rcp85, main="Serie de tiempo de la temperatura m�xima diaria promedio nacional para El Salvador desde 2006 a 2100\nModelo MPIESMMR_RCP45", ylab="Temperaturas Celsius", xlab="A�os",col="green",
     ylim=c(23,40),add=TRUE)

ts.plot(serie_temporal_promedio_nacional_hist,serie_temporal_promedio_nacional_rcp45,serie_temporal_promedio_nacional_rcp85,
        gpars = list(col = c("black","blue", "red")))

jpeg("C:/CursoManagua/resultados/hist_RCP45.jpg",width =1200, height = 800 )
ts.plot(serie_temporal_promedio_nacional_hist,serie_temporal_promedio_nacional_rcp45,
        gpars = list(col = c("black","blue")),main="Serie temporal de temperatura m�xima hist�rica 1980 a 2017 y proyecci�n RCP 4.5 \n desde 2006 a 2100")
dev.off()

jpeg("C:/CursoManagua/resultados/hist_RCP85.jpg",width =1200, height = 800 )
ts.plot(serie_temporal_promedio_nacional_hist,serie_temporal_promedio_nacional_rcp85,
        gpars = list(col = c("black","red")),main="Serie temporal hist�rica 1980 a 2017 y proyecci�n RCP 8.5")
dev.off()

