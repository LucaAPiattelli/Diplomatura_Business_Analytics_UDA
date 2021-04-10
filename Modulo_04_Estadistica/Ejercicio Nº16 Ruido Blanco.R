
#--------------------------------------------------------------------
  
  tiempo=c(1:100)
  
  x=NULL
  
  t=1
  
  while(t<=100){
    
  x[t]= rnorm(1,0,1) 
    
    
  t=t+1  
  }
  
  
  #-----------------------------------------------------------------------
  
  
  plot(x, ylab="Xt", xlab="Tiempo" ,type="o",lwd=1,pch=1,cex=1,col="Green")
  abline(h=0)

   #--------------------------------------------------------------------
  #Ver ejercicio 20
  
  fit=auto.arima(x,seasonal = F,ic = c("aicc"), stepwise=T, allowdrift = F)
  summary(fit)

