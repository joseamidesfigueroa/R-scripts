library(fdth)


setwd("C:/R/")
#Ejercicio 1 parcial
datos<-c(900,2753,2595,6057,7624,6624,6362,6575,7760,7085,7272,5967,5256,6160,6238,6709,7193,5631,6490,6682,7829,7091,6871,6230,7253,5507,5676,6974,6915,4999,5689,6143,7086)

tabla_datos<-table(datos)

dist<-fdt(datos,breaks = "Sturges")
tabla<-dist
write.csv(tabla,file="C:/R/ejercicio1_parcial.csv")

jpeg(filename = "histo.jpg",width = 800, height = 800 )
hist(datos, breaks = "Sturges",xlab = "Descargas mensuales", ylab = "Frecuencia", main = "Grafico de barras de la frecuencia de descargas",col = "grey") #histograma utilizando el numero de clases según Sturge
dev.off()

#histograma de frecuencias simples
jpeg(filename = "fh.jpg",width = 800, height = 800 )
plot(dist, type="fh", xlab = "Clases", ylab = "Frecuencias", main = "Histograma de frecuencias absolutas")
dev.off()

#Poligono de frecuencias simples
jpeg(filename = "fp.jpg",width = 800, height = 800 )
plot(dist, breaks = "Sturges", type="fp", xlab = "Clases", ylab = "Frecuencias", main = "Polígono de frecuencias absolutas")
dev.off()

#Histograma de frecuencias relativas
jpeg(filename = "rfh.jpg",width = 800, height = 800 )
plot(dist, type="rfh", xlab = "Clases", ylab = "Frecuencias", main = "Histograma de frecuencias relativas")  
dev.off()

#Poligono de frecuencias relativas simples
jpeg(filename = "rfp.jpg",width = 800, height = 800 )
plot(dist, type="rfp", xlab = "Clases", ylab = "Frecuencias", main = "Poligono de frecuencias relativas simples")
dev.off()

#Historgrama de frecuencias relativas porcentuales
jpeg(filename = "rfph.jpg",width = 800, height = 800 )
plot(dist, type="rfph", xlab = "Clases", ylab = "Frecuencias", main = "Historgrama de frecuencias relativas porcentuales")  
dev.off()

#Poligono de frecuencias relativas porcentuales
jpeg(filename = "rfpp.jpg",width = 800, height = 800 )
plot(dist, type="rfpp", xlab = "Clases", ylab = "Frecuencias", main = "Poligono de frecuencias relativas porcentuales")  
dev.off()

#Historgrama de densidad
jpeg(filename = "d.jpg",width = 800, height = 800 )
plot(dist, type="d", xlab = "Clases", ylab = "Densidades", main = "Historgrama de densidad")  
dev.off()

#Historgrama de densidad acumulada
jpeg(filename = "cdh.jpg",width = 800, height = 800 )
plot(dist, type="cdh", xlab = "Clases", ylab = "Densidades", main = "Historgrama de densidad acumulada")
dev.off()

#Poligono de densidad acumulada
jpeg(filename = "cdp.jpg",width = 800, height = 800 )
plot(dist, type="cdp", xlab = "Clases", ylab = "Densidades", main = "Poligono de densidad acumulada") 
dev.off()

#histograma de frecuencias acumulada
jpeg(filename = "cfh.jpg",width = 800, height = 800 )
plot(dist, type="cfh", xlab = "Clases", ylab = "Frecuencias", main = "Histograma de frecuencias acumulada") 
dev.off()

#poligono de frecuencias acumulado.
jpeg(filename = "cfp.jpg",width = 800, height = 800 )
plot(dist, type="cfp", xlab = "Clases", ylab = "Frecuencias", main = "Poligono de frecuencias acumulado") 
dev.off()

#Histograma de frecuencias porcentuales acumuladas
jpeg(filename = "cfph.jpg",width = 800, height = 800 )
plot(dist, type="cfph", xlab = "Clases", ylab = "Frecuencias", main = "Histograma de frecuencias porcentuales acumuladas") 
dev.off()

#Poligono de frecuencias porcentuales acumuladas
jpeg(filename = "cfpp.jpg",width = 800, height = 800 )
plot(dist, type="cfpp", xlab = "Clases", ylab = "Frecuencias", main = "Poligono de frecuencias porcentuales acumuladas")  
dev.off()



trozos<-c(3.03,6.06,0,0,18.18,39.39,33.33)
etiquetas<-c("891 a 1893","1893 a 2896", "2896 a 3898", "3898 a 4900",
             "4900 a 5903", "5903 a 6905", "6905 a 7907")
pie(trozos,labels = etiquetas, main = "Porcentajes de descargas", cex=0.95)


datos2<-c(137,122,116,103,112,96,115,98,106,111,106,124,116,127,116,108,112,112,121,115,124,116,107,118,123,109,109,106)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

media<-mean(datos2)
media
mediana<-median(datos2)
mediana
Mode(datos2)