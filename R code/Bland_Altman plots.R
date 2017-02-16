#BA plots

ba_info<-function(x,y){
  avg_r<-(x+y)/2
  diff_r<-x-y
  diff_sd<-sqrt(var(diff_r,na.rm=T))
  uci<-round(mean(diff_r,na.rm=T)+(2*diff_sd),0)
  lci<-round(mean(diff_r,na.rm=T)-(2*diff_sd),0)  #function to generate values for B-A plots
  mean_diff<-round(mean(diff_r,na.rm=T),0)
  
  return(list(avg_r,diff_r,uci,lci,mean_diff))
}

h<-ba_info(dat_m$drill,dat_m$probe)   #BA function 

#plot
#pdf("")

par(mgp=c(2,1,0))
par(mai=c(1,1,.8,1))

plot(h[[1]],h[[2]],xaxt='n',yaxt='n',ylim=c(-35,35), 
     ylab="difference",
     xlab="average")

abline(h=h[[4]],lwd=2)
abline(h=h[[3]],lwd=2)
abline(h=0,lty=2)
abline(h=h[[5]])

#axis(2,at=c(-30,-15,0,15,30), tck=-0.02,labels=NA,hadj=-1)
#axis(2,at=c(-30,-15,0,15,30),label=c(-30,-15,0,15,30),las=1,line=-.4,lwd=0)

axis(1, tck=-0.02,labels=NA,hadj=-1)
axis(1,las=1,line=-.5,lwd=0)

axis(4,at=c(h[[3]],h[[5]],h[[4]]),tck=0,las=1,hadj=.5)

#dev.off()