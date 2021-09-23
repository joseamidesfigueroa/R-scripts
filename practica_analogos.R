library(ncdf4)
setwd("C:/CursoManagua/datos")
#C:/Users/imn/Desktop/CURSO/datos/Binarios-R
dir.datos=("C:/CursoManagua/datos/Binarios-R/")
load(paste(dir.datos,"prec.r20.chirps.AC.RData",sep=""))
load(paste(dir.datos,"q_erainterim_AC.RData",sep=""))
load(paste(dir.datos,"u_erainterim_AC.RData",sep=""))
load(paste(dir.datos,"v_erainterim_AC.RData",sep=""))
objects()
library(PCICt)
dim(fech);head(fech);tail(fech)
dim(pr$datos)
dim(qz)# Lon Lat niveles tiempo

# Datos de precipitación
names(pr)
pr$variable




##############################Métrica para obterner análogos
dimqz=dim(qz)
distancia=sapply(2:2000, FUN= function(i) { kk=(qz[,,1,1]-qz[,,1,i])^2
return (sum(kk)/(ncol(kk)*nrow(kk)))})
d.orden=sort.int(distancia,index.return = T)
d.orden[1:10]
fech[d.orden$ix[1:10]]

library(maps)
require(RColorBrewer)

max(pr$coordenadas[,1])
min(pr$coordenadas[,1])

coord=expand.grid(x=pr$coordenadas[,1], y=pr$coordenadas[,2])
c(min(pr$datos), max(pr$datos))

#Caso de estudio o problema dia cambiar pr$datos[1,]
#plot(coord, pch=15, col=brewer.pal(11,name="RdYlGn")[as.numeric(cut(pr$datos[1,], breaks=seq(0, round(max(pr$datos[,1]), digits=0), length.out=12)))], ylab="latitud", xlab="longitud",  cex=1.9, xlim=c(-92.5, -75))
#legend(-77,18, legend=round(seq(0, round(max(pr$datos[,1]), digits=0), length.out=12), digits=0)[1:11], col= brewer.pal(11,name="RdYlGn"), pch=15, pt.cex =1.2)
#map("world",add=T, xlim=c(-92.5, -77))
#map("world",add=T, xlim=c(-86, -82.5))

pr$datos[2,]

plot(puntos.AC$coordenadas, pch=15, col=brewer.pal(11,name="RdYlGn")[as.numeric(cut(pr$datos[1,], breaks=seq(-1, round(max(pr$datos[1,]), digits=0), length.out=12)))], ylab="latitud", xlab="longitud",  cex=1.9, xlim=c(-92.5, -75))
legend(-77,18, legend=round(seq(0, round(max(pr$datos[1,]), digits=0), length.out=12), digits=0)[1:11], col= brewer.pal(11,name="RdYlGn"), pch=15, pt.cex =1.2)
map("world",add=T, xlim=c(-92.5, -77))
#El día que dista menos de él según la métrica usada

iday=which(as.vector(pr$fechas) %in% trunc(fech[d.orden$ix[1]],"days"))

plot(coord, pch=15, col=brewer.pal(11,name="RdYlGn")[as.numeric(cut(pr$datos[,iday], breaks=seq(-0, round(max(pr$datos[,iday]), digits=0), length.out=12)))], ylab="latitud", xlab="longitud",  cex=1.9, xlim=c(-92.5, -75))
legend(-77,18, legend=round(seq(0, round(max(pr$datos[,iday]), digits=0), length.out=12), digits=0)[1:11], col= brewer.pal(11,name="RdYlGn"), pch=15, pt.cex =1.5)
map("world",add=T, xlim=c(-92.5, -77))

plot(puntos.AC$coordenadas, pch=15, col=brewer.pal(11,name="RdYlGn")[as.numeric(cut(pr$datos[,iday], breaks=seq(-0, round(max(pr$datos[,iday]), digits=0), length.out=12)))], ylab="latitud", xlab="longitud",  cex=1.9, xlim=c(-92.5, -75))
legend(-77,18, legend=round(seq(0, round(max(pr$datos[,iday]), digits=0), length.out=12), digits=0)[1:11], col= brewer.pal(11,name="RdYlGn"), pch=15, pt.cex =1.5)
map("world",add=T, xlim=c(-92.5, -77))

#Diferencia
dpr=pr$datos[,1]-pr$datos[,iday]
plot(coord, pch=15, col=brewer.pal(11,name="RdYlGn")[as.numeric(cut(dpr, breaks=seq(-0, round(max(pr$datos[1,]), digits=0), length.out=12)))], ylab="latitud", xlab="longitud",  cex=1.9, xlim=c(-92.5, -75))
legend(-77,18, legend=round(seq(0, round(max(pr$datos[1,]), digits=0), length.out=12), digits=0)[1:11], col= brewer.pal(11,name="RdYlGn"), pch=15, pt.cex =1.5)
map("world",add=T, xlim=c(-92.5, -77))

dpr=pr$datos[,1]-pr$datos[,iday]
plot(puntos.AC$coordenadas, pch=15, col=brewer.pal(11,name="RdYlGn")[as.numeric(cut(pr$datos[1,], breaks=seq(-0, round(max(pr$datos[1,]), digits=0), length.out=12)))], ylab="latitud", xlab="longitud",  cex=1.9, xlim=c(-92.5, -75))
legend(-77,18, legend=round(seq(0, round(max(pr$datos[1,]), digits=0), length.out=12), digits=0)[1:11], col= brewer.pal(11,name="RdYlGn"), pch=15, pt.cex =1.5)
map("world",add=T, xlim=c(-92.5, -77))
    
    
    ################################################################################################################################################################################
    
    plot(puntos.AC$coordenadas[,1],puntos.AC$coordenadas[,2])
    #Pesos
    pesos=matrix(1,nrow=1,ncol=27)
    pesos[5:18,6:15]=2