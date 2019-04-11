library(RColorBrewer)
plot_cow <- function(cow_id, data) {
  par(mar = rep(4,4))
  plot(data[data$Cow == cow_id,'DIM'], data[data$Cow == cow_id,'GP_pred_xgb'],axes=F,
       ylim=c(0,0.01),
       xlab="", ylab="",type="l", lwd = 3,col=rgb(1,0,1), main="",
       xlim=c(min(data[data$Cow == cow_id,'DIM'], na.rm = T),
              max(data[data$Cow == cow_id,'DIM'], na.rm = T)))
  # rect(xleft = data[data$Cow == cow_id,]$DIM-0.5,
  #      xright = data[data$Cow == cow_id,]$DIM+0.5, ybottom = min(data[data$Cow == cow_id,'GN_pred_xgb'], na.rm = T),
  #      ytop = max(data[data$Cow == cow_id,'GN_pred_xgb']+1,na.rm = T),
  #      col = data[data$Cow == cow_id,]$GN_pred_xgb > 0.5, lty = 0)
  par(new = T)
  plot(data[data$Cow == cow_id,'DIM'], data[data$Cow == cow_id,'MY'],axes=F,
       ylim=c(min(data[data$Cow == cow_id,'MY'], na.rm = T),
              max(data[data$Cow == cow_id,'MY']+1, na.rm = T)),
       xlab="", ylab="",type="l", lwd = 2,col=brewer.pal(n = 4, name = "Set1")[1], main="",
       xlim=c(min(data[data$Cow == cow_id,'DIM'], na.rm = T),
              max(data[data$Cow == cow_id,'DIM'], na.rm = T)))
  
  par(new=T)
  plot(data[data$Cow == cow_id,'DIM'], data[data$Cow == cow_id,'LAC'], axes=F,
       ylim=c(min(data[data$Cow == cow_id,'LAC'], na.rm = T),
              max(data[data$Cow == cow_id,'LAC']+1, na.rm = T)),
       xlab="", ylab="", 
       type="l",col = brewer.pal(n = 4, name = "Set1")[2], main="",
       xlim=c(min(data[data$Cow == cow_id,'DIM'], na.rm = T),max(data[data$Cow == cow_id,'DIM'],
                                                                 na.rm = T)),lwd=2)
  
  par(new=T)
  plot(data[data$Cow == cow_id,'DIM'], data[data$Cow == cow_id,'COND'], axes=F,
       ylim=c(min(data[data$Cow == cow_id,'COND'], na.rm = T),
              max(data[data$Cow == cow_id,'COND']+1, na.rm = T)),
       xlab="", ylab="", 
       type="l",col = brewer.pal(n = 7, name = "Set1")[7], main="",
       xlim=c(min(data[data$Cow == cow_id,'DIM'], na.rm = T),max(data[data$Cow == cow_id,'DIM'],
                                                                 na.rm = T)),
       lwd=2)
  
  par(new=T)
  plot(data[data$Cow == cow_id,'DIM'], data[data$Cow == cow_id,'Activity'], axes=F,
       ylim=c(min(data[data$Cow == cow_id,'Activity'], na.rm = T),
              max(data[data$Cow == cow_id,'Activity']+1, na.rm = T)),
       xlab="", ylab="", 
       type="l",col = brewer.pal(n = 4, name = "Set1")[4], main="",
       xlim=c(min(data[data$Cow == cow_id,'DIM'], na.rm = T),max(data[data$Cow == cow_id,'DIM'],
                                                                 na.rm = T)),
       lwd=2)
  
  axis(1,pretty(range(data[data$Cow == cow_id,'DIM'], na.rm = T),10))
  mtext("DIM",side=1,col="black",line=2)
  title(main = paste("Cow#",cow_id))
  legend('top', legend=c('Milk','Lactose','Conductivity','Activity'), ncol = 4,
         fill=c("#E41A1C", "#377EB8", "#A65628", "#984EA3"))
}
