
#---------------------------------------------------------------------
#Cargar datos
#---------------------------------------------------------------------

muestra=c("a tiempo", "retraso","a tiempo","a tiempo","a tiempo","a tiempo", "retraso",
         "retraso","a tiempo", "retraso","a tiempo","a tiempo","a tiempo","a tiempo",
         "retraso","retraso","a tiempo", "retraso","a tiempo","a tiempo", "retraso",
         "a tiempo","a tiempo","a tiempo","a tiempo","a tiempo","a tiempo","retraso",
         "a tiempo","a tiempo","retraso","a tiempo")

#-----------------------------------------------------------------
#Distribucion de frecuencia
#----------------------------------------------------------------

#Contrar "a tiempo"

a_tiempo=muestra

a_tiempo[a_tiempo=="a tiempo"]=1   
a_tiempo[a_tiempo=="retraso"]=0  

a_tiempo=as.numeric(a_tiempo)

#cantidad a tiempo
sum(a_tiempo)

#Contrar "retraso"

32-22


#Armar vector con frecuencia absoluta

frecuencia_absoluta=c(10,22)

#Armar vector con frecuencia relativa

frecuencia_relativa=frecuencia_absoluta/32


#Armar vector con clase

Clase=c("retraso","a tiempo")

#Armar frafico de barras

Distribucion_frecuencia=data.frame(Clase=Clase,frecuencia_absoluta=frecuencia_absoluta,
                                   frecuencia_relativa=frecuencia_relativa)

#Grafico de barra

barplot(frecuencia_relativa,names.arg =c("retraso","a tiempo"))


