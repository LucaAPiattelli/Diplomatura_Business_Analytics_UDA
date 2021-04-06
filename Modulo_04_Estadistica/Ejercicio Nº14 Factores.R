    #-----------------------------------------------------------------------
#Librerias
#----------------------------------------------------------------------
#Libreria grafica
library(ggplot2)
#Libreria test B-P
library(lmtest)



#-------------------------------------------------------------------

litros_vendidos=c(6754,8026,6954,6747,7077,7607,7451,7138,7314,6808,7048,6401 ,7248,6889,6864,7081,6803,7513,
                  6512,6575,7588,6614,6788,7056,6662,7358,6145,6865,6531,7750,6109,6739,6615,7829,7135,7479,
                  6763,7376,7333,6664,6356,7277,7112,6818,6576,6970,7031,7457,9876,10067,10086,9684,10108,10227,
                  9853,9971,9979,9337,10775,10917,9957,9504,10193,9648,9635,9937,10031,10668,11470,10098,9289,8724)
  
  
 
punto_venta=c("GC" ,"GC","GC","GC","GC","GC","GC","GC","GC","GC","GC","GC","GC","GC","GC","GC","GC","GC","GC","GC","GC",
              "GC","GC","GC","SR","SR","SR","SR","SR","SR","SR","SR","SR","SR","SR","SR","SR","SR","SR","SR","SR","SR",
             "SR","SR","SR","SR","SR","SR","SM","SM","SM","SM","SM","SM","SM","SM","SM","SM","SM","SM","SM","SM","SM",
              "SM","SM","SM","SM","SM","SM","SM","SM","SM")
  
#Transformar a factor

punto_venta=as.factor(punto_venta)

#------------------------------------------------------------------------

datos=data.frame(litros_vendidos=litros_vendidos,punto_venta=punto_venta)

#----------------------------------------------------------------------



g = ggplot(datos, aes(y=litros_vendidos,x=punto_venta))
g= g + geom_boxplot(aes(fill=punto_venta)) 
g=g+geom_jitter(alpha = 0.5, color = "tomato")
g=g+ylab("Litros vendidos")
g=g+xlab("Puntos de ventas")
g

#--------------------------------------------------------------------------

options(contrasts = c("contr.sum","contr.poly"))

modelo=lm(litros_vendidos~punto_venta)

summary(modelo)



#---------------------------------------------------------------------------------------
#Test de shapiro-Wilks
#-----------------------------------------------------------------------------------------

#H0: Es normal    vs    H1: no es normal

shapiro.test(rstandard(modelo))



#-----------------------------------------------------------------------------------
#test de bartlett
#----------------------------------------------------------------------------------
#H0: todas las varianzas son iguales vs H1: al menos una diferente


bartlett.test(rstandard(modelo)~punto_venta-1)


#--------------------------------------------------------------------------------------
#Durbin-Watson
#--------------------------------------------------------------------------------------
#H0: errores son independientes  vs H1: no son independientes

dwtest(modelo)
