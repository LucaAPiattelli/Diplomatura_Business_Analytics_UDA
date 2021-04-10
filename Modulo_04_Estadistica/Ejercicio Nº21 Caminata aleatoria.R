    
#-----------------------------------------------------------------------------------------
#Cargar librerias
#----------------------------------------------------------------------------------------

library(forecast)


#--------------------------------------------------------------------

tiempo=c(1:100)

x=rnorm(1,0,1)


t=2

while(t<=100){
  
  
  x[t]=1*x[t-1]+rnorm(1,0,1)
  
  
  t=t+1  
}


#-----------------------------------------------------------------------


plot(x, ylab="Xt", xlab="Tiempo" ,type="o",lwd=1,pch=1,cex=1,col="Green")
abline(h=0)


#-------------------------------------------------------------------


dx=diff(x,1)


plot(dx, ylab="dXt", xlab="Tiempo" ,type="o",lwd=1,pch=1,cex=1,col="Green")
abline(h=0)



fit=auto.arima(dx,seasonal = F,ic = c("aicc"), stepwise=T, allowdrift = F)
summary(fit)


#-----------------------------------------------------------------------

fit=auto.arima(x,seasonal = F,ic = c("aicc"), stepwise=T, allowdrift = T)
summary(fit)




