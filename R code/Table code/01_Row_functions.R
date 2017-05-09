####################################################################################################
#####################################   Row Functions for n groups       ###########################
####################################################################################################

#########################################Categorical Variables######################################

###################
# No. (percentage)#
###################
prop_row<- function(y,x) {
  
temp<-table(y,x)
temp.prop<-round(prop.table(temp,2)*100,0)
p<-round(chisq.test(temp)$p.value,4)

temp1<-paste0(table(y), ' (',round(prop.table(table(y))*100,0), '%)')

temp2<-c(Variable="",c(rep("",ncol(temp)+1)),Pval=p)
  
for(i in (1:nrow(temp))){
  temp2<-rbind(temp2,c(Variable=rownames(temp)[i],temp1[i],
  paste0(temp[i,]," (",temp.prop[i,],"%)"),Pval=" "))
  } 

t<-data.frame(temp2,row.names=NULL)
  
t$Pval<-as.character(t$Pval)
t$Pval[t$Pval==0]<-"<0.0001"
  
return(t)  
}

#####################################################
# No. (percentage) for binary outcome (one row only)#
#####################################################
prop_row_one<- function(y,x) {
  
  temp<-table(y,x)
  temp.prop<-round(prop.table(temp,2)*100,0)
  p<-round(chisq.test(temp)$p.value,4)
  
  temp_all<-paste0(table(y), ' (',round(prop.table(table(y))*100,0), '%)')
  
  m<-matrix(0,ncol=ncol(temp)+3,nrow=1)
  
  temp1<-data.frame(rbind(m,c(Variable="",temp_all[2],paste0(temp[2,]," (",temp.prop[2,],"%)"),Pval=p)))
  
  t<-temp1[-1,]
  
  t$Pval<-as.character(t$Pval)
  t$Pval[t$Pval==0]<-"<0.0001"
  
  return(t)
}

####################################################################################################
#########################################Continuous Variables#######################################

##############
# Mean +/- SD#
############## 
mean_row<-function(y,x){
  
  mean.all<-round(mean(y,na.rm=T))
  sd.all<-round(sd(y,na.rm=T))
  
  mean.y<-round(tapply(y,x,mean,na.rm=T))
  sd.y<-round(tapply(y,x,sd,na.rm=T))
  
  if (length(mean.y) == 2){
    p.y<-round(t.test(y~as.factor(x),data=dat)$p.value,4)
  } else{
    p.y<-round(summary(aov(y~as.factor(x)))[[1]][[1,"Pr(>F)"]],4)}
  
  m<-matrix(0,ncol=length(mean.y)+3,nrow=1)
  
  t<-data.frame(rbind(m,c(Variable="", paste0(mean.all,"±",sd.all), paste0(mean.y,"±",sd.y),Pval=p.y)))[-1,]
  
  t$Pval<-as.character(t$Pval)
  t$Pval[t$Pval==0]<-"<0.0001"
  
  return(t)
}

##########################
# Geometric Mean (95% CI)#
########################## 
geom_mean_row<-function(y,x){
  
  est.all<-round(exp(lm(log(y)~1,data=dat)$coef))
  ci.all<-round(exp(confint(lm(log(y)~1,data=dat))))
  
  est<-round(exp(lm(log(y)~as.factor(x)+0,data=dat)$coef))
  ci<-round(exp(confint(lm(log(y)~as.factor(x)+0,data=dat))),1)
  
  if (length(est) == 2){
    p.y<-round(t.test(log(y)~as.factor(x),data=dat)$p.value,4)
  } else{
    p.y<-round(summary(aov(log(y)~as.factor(x)))[[1]][[1,"Pr(>F)"]],4)}
  
  m<-matrix(0,ncol=length(est)+3,nrow=1)
  
  t<-data.frame(rbind(m,c(Variable="",paste0(est.all," (",ci.all[1],", ",ci.all[2],")"),
                          paste0(est," (",ci[,1],", ",ci[,2],")"),Pval=p.y)))[-1,]
  
  t$Pval<-as.character(t$Pval)
  t$Pval[t$Pval==0]<-"<0.0001"
  
  return(t)
}

#################################
# Geometric Mean +1, -1 (95% CI)#
################################# 

geom_mean_row_b<-function(y,x){
  
  est.all<-round(exp(lm(log(y+1)~1,data=dat)$coef)-1)
  ci.all<-round(exp(confint(lm(log(y+1)~1,data=dat)))-1)
  
  est<-round(exp(lm(log(y+1)~as.factor(x)+0,data=dat)$coef)-1)
  ci<-round(exp(confint(lm(log(y)~as.factor(x)+0,data=dat)))-1)
  
  if (length(est) == 2){
    p.y<-round(t.test(log(y+1)~as.factor(x),data=dat)$p.value,4)
  } else{
    p.y<-round(summary(aov(log(y+1)~as.factor(x)))[[1]][[1,"Pr(>F)"]],4)}
  
  m<-matrix(0,ncol=length(est)+3,nrow=1)
  
  t<-data.frame(rbind(m,c(Variable="", paste0(est.all," (",ci.all[1],", ",ci.all[2],")"),
                          paste0(est," (",ci[,1],", ",ci[,2],")"),Pval=p.y)))[-1,]
  
  t$Pval<-as.character(t$Pval)
  t$Pval[t$Pval==0]<-"<0.0001"
  
  return(t)
  
}

###############
# Median (IQR)#
############### 
median_row<-function(y,x){
  
  med.all<-round(median(y,na.rm=T))
  q1.all<-round(quantile(y,.25,na.rm=T))
  q2.all<-round(quantile(y,.75,na.rm=T))
  
  med.y<-round(tapply(y,x,median,na.rm=T))
  q1<-round(tapply(y,x,function(x) quantile(x,.25,na.rm=T)))
  q2<-round(tapply(y,x,function(x) quantile(x,.75,na.rm=T)))
  
  if (length(med.y) == 2){
    p.y<-round(wilcox.test(y~x)$p.value,4)
  } else{
    p.y<-round(kruskal.test(y~x)$p.value,4)}
  
  m<-matrix(0,ncol=length(med.y)+3,nrow=1)
  
  t<-data.frame(rbind(m,c(Variable="",paste0(med.all," (",q1.all,", ",q2.all,")"),
                          paste0(med.y," (",q1,", ",q2,")"),Pval=p.y)))[-1,]
  
  t$Pval<-as.character(t$Pval)
  t$Pval[t$Pval==0]<-"<0.0001"
  
  return(t)
}

#proportion of missing data
missing_row<-function(y,x){
  
  miss<-tapply(y,x, function(x) paste0(round((length(x[is.na(x)==T])/length(x))*100),'%'))
  
  m<-matrix(0,ncol=length(miss)+1,nrow=1)
  
  #put row together
  t<-data.frame(rbind(m,c(Variable="",miss)))
  
  return(t[-1,])
}

#function returns mean, geometric mean, median, proportion missing, histogram and histogram of the log outcome
dist_check<-function(var1){
  par(mfrow=c(2,2))
  hist(var1)
  hist(log(var1))
  avg<-mean(var1,na.rm=T)
  avg_exp<-exp(mean(log(var1),na.rm=T))
  med<-median(var1,na.rm=T)
  miss<-length(var1[is.na(var1)==T])/length(var1)
  m<-min(var1,na.rm=T)
  M<-max(var1,na.rm=T)
  
  return(list(mean=avg,geom_mean=avg_exp,median=med,missing=miss,minimum=m,maximum=M))
}