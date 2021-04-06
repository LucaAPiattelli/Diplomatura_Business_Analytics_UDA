

  #---------------------------------------------------------------------
  #Cargar datos
  #---------------------------------------------------------------------
  
  Horas=c(107,54,66,62,74,92,75,65,81,83,78,90,96,66,68,85,83,74,73,
          73,73,65,62,116,85,78,90,81,62,70,66,78,75,86,72,67,68,91,
          77,63,68,71,79,65,73,88,62,75,79,70,66,71,64,96,77,87,72,
          76,79,63,97,70,86,88,80,77,89,62,83,81,94,101,76,89,60,80,
          67,83,94,94,89,76,84,68,64,68,103,71,94,93,77,77,78,72,81,
          87,84,92,66,63,82,79,88,74,79,78,88,71,71,61,72,63,43,77,
          71,84,93,89,68,59,64,94,62,61,78,89,63,74,85,65,84,66,59,
          82,61,69,75,85,74,62,85,59,61,82,82,79,72,68,70,84,62,67,
          75,67,65,99,77,76,96,73,71,92,98,65,79,58,77,88,74,83,92,
          59,68,61,82,59,51,89,77,72,81,64,57,98,98,86,69,81,70,63,
          65,58,76,71,86,92,45,75,102,76)
  

  #-----------------------------------------------------------------
  #Distribucion de frecuencia
  #----------------------------------------------------------------
      
  #Armar objeto 
      
  obj=hist(Horas,probability = T)
  
  #Graficar Histograma
  
  obj
  
  #Determinar cortes
  
  obj$breaks
  
  #Determinar frecuencia absoluta
  
  obj$counts
  
  #Determinar frecuencia relativa
  
  obj$density*10
  
  #Armar Tabla de frecuencia
  
  #1 categorias
  obj$breaks
  
  categorias=c("[40-50)","[50-60)","[60-70)","[70-80)","[80-90)","[90,100)","[100-110)","[110-120]")
  
  
  #2 Armar tabla
  
  tabla_frecuencia=data.frame(categorias=categorias,frecuencia_absoluta=obj$counts,frecuencia_relativa=obj$density*10)



  #----------------------------------------------------------------
  #Medidas de tendencia central
  #----------------------------------------------------------------
  
  #Media
  
  mean(Horas)
  
  #Mediana
  
  median(Horas)
  
  #Moda
  
  (70+80)/2
  
  #Cuantiles
  
  quantile(Horas,0.17)
  
  #Cuartiles
  
  Q1=quantile(Horas,0.25)
  Q1
  Q2=quantile(Horas,0.50)
  Q2
  Q3=quantile(Horas,0.75)
  Q3
  
  #Grafico caja y bigotes
  
  
  bp=boxplot(Horas)
  bp
  
  #Valor maximo y minimo
  max(Horas)
  min(Horas)
  
  #Rango intercuartil
  
  RIC=Q3-Q1
  
  #Valor maximo
  
  Q3+RIC*1.5
  
  #Valor minimo
  
  Q1-RIC*1.5
  
  #Valor atipico
  
  bp$out
  
  #Eliminar valor atipico
  
  Horas_l=Horas
  
  Horas_l[Horas_l==116]=NA
  Horas_l
  
  Horas_l=Horas_l[!is.na(Horas_l)]
  boxplot(Horas_l)
  

  #----------------------------------------------------------------
  #Medidas de dispersión
  #----------------------------------------------------------------
  
  #Rango
  
  max(Horas)-min(Horas)
  
  #Var
  
  var(Horas)
  
  #Desviacion
  
  sd(Horas)
  
  #Coeficiente de variación
  
  sd(Horas)/mean(Horas)*100
