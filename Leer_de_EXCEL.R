#Cargas la libreria
library(readxl)

#Fijas la carpeta donde vas a trabajar y poner tus datos
setwd("C:/Users/Amides/Dropbox/R")

#Seteas los datos de EXCEL que vas a leer
datos <- read_excel("20190902-0300_nesdis_prod_obs_precip_ghe_24hr_panama.xlsx")

#Para acceder a la primera columna
datos[1]
#Para acceder a la segunda columna
datos[2]
#Para acceder a la tercera columna
datos[3]


#Para acceder a datos individuales puedes combinar con el signo de dolar la columna por nombre y el numero de elemento que quieres:
dato_de_prueba_extraido=datos$LON[1]
#Con el comando anterior guardas en una nueva variable el contenido de la fila de longitudes el valor de la primera fila.

#Puedes guardar una serie de datos indicando una secuencia siempre de una sola columna, por ejemplo de la tercera:
dato_de_prueba_extraido_columna3=datos$`24GHE2019090203`[seq(1:15)]

#Con el comando anterior guardaste los primeros 15 datos de la columna 3 en una variable.

#Finalmente puedes operar por ejemplo aplicar una funcion, calcular la media es sumamente facil, calculemos la media de la ultima variable:
media=mean(dato_de_prueba_extraido_columna3)

#Para mostrar el valor de la media solo llamas la variable en tu codigo:
media