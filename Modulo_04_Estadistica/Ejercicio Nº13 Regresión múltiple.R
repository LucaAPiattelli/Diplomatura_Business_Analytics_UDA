  #-----------------------------------------------------------------------
#Librerias
#----------------------------------------------------------------------
#Libreria grafica
library(ggplot2)
#Libreria test B-P
library(lmtest)


#-------------------------------------------------------------------

tiempo=c(9.3, 4.8, 8.9,6.5,4.2,6.2,7.4,6.0,7.6, 6.1)

km=c(100,50,100,100,50,80, 75,65,90,90)
     
     
g=ggplot(data.frame(tiempo=tiempo,km=km),aes(x=km,y=tiempo))
g=g+geom_smooth(method=lm,formula = y ~ poly(x, 1))
g=g+geom_point(shape=1)
g     
     
     
#-------------------------------------------------------------------------
#Modelo lineal simple

modelo_lineal=lm(tiempo~km)

summary(modelo_lineal)   
     
     
#-------------------------------------------------------------------------
#Modelo lineal múltiple

entregas=c(4,3,4,2,2,2,3,4,3,2)
  


modelo_lineal_multiple=lm(tiempo~km+entregas)

summary(modelo_lineal_multiple)       
        
        
#---------------------------------------------------------------------------
#Analisis supuestos
#---------------------------------------------------------------------------
#Test de shapiro-Wilks
#---------------------------------------------------------------------------

#H0: Es normal    vs    H1: no es normal

shapiro.test(rstandard(modelo_lineal_multiple))



#-----------------------------------------------------------------------------------
#test Breusch-Pagan test
#----------------------------------------------------------------------------------

#H0: todas las varianzas son iguales vs H1: al menos una es diferente

bptest(modelo_lineal_multiple)



#--------------------------------------------------------------------------------------
#Durbin-Watson
#--------------------------------------------------------------------------------------
#H0: errores son independientes  vs H1: no son independientes

dwtest(modelo_lineal_multiple)

         
         
#--------------------------------------------------------------------------------------
#Multicolinealidad
#--------------------------------------------------------------------------------------
     
multicolinealidad=lm(km~entregas)         

summary(multicolinealidad)         
        