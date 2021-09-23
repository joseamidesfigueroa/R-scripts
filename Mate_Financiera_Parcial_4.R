options(digits=8)

#Calculo del Valor Actual Neto (VAN) El mayor es el mas rentable
#Vidas iguales

Inversion_inicial<-15000
Ingresos_anuales<-3500
Valor_rescate<-2500
Vida_util<-10
tasa_min_retorno<-0.10

VAN<-(Ingresos_anuales*((1-((1+tasa_min_retorno)^-Vida_util))/tasa_min_retorno))+
      (Valor_rescate/((1+tasa_min_retorno)^Vida_util))-Inversion_inicial
VAN


#Beneficio anual uniforme equivalente (BAUE) El mayor es el mas rentable Serie Uniforme
BAUE<-VAN*((tasa_min_retorno)/(1-((1+tasa_min_retorno)^-Vida_util)))
BAUE



#VAN INVERSION EXTRA

Inversion_inicial<-15000
Ingresos_anuales<-3500
Valor_rescate<-2500
Incremento_anual<-0
Vida_util<-10
tasa_min_retorno<-0.10

VAN<-(Ingresos_anuales*((1-((1+tasa_min_retorno)^-Vida_util))/tasa_min_retorno))+
  (Valor_rescate/((1+tasa_min_retorno)^Vida_util))-Inversion_inicial
VAN

#Costo  anual uniforme equivalente (CAUE) El menor es el mas rentable
#La TIR de retorna es aquella tasa para la cual el VAN es cero.



#Tasa interna de retorno (TIR), el mayor es mas rentable
