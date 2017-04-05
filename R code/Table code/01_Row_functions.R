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
  p<-round(fisher.test(temp)$p.value,4)
  
  m<-matrix(0,ncol=ncol(temp)+2,nrow=1)
  
  temp1<-rbind(m,c(Variable="",c(rep("",ncol(temp))),Pval=p.y))
  
  for(i in (1:nrow(temp))){
    temp1<-rbind(temp1,c(Variable="",paste0(temp[i,]," (",temp.prop[i,],")"),Pval=" "))
  }
  
  t<-data.frame(temp1[-1,])
  
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
  p<-round(fisher.test(temp)$p.value,4)
  
  m<-matrix(0,ncol=ncol(temp)+2,nrow=1)
  
  temp1<-rbind(m,c(Variable="",c(rep("",ncol(temp))),Pval=p.y),
               c(Variable="",paste0(temp[2,]," (",temp.prop[2,],")"),Pval=" "))
  
  t<-data.frame(temp1[-1,])
  
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
  
  mean.y<-round(tapply(y,x,mean,na.rm=T))
  sd.y<-round(tapply(y,x,sd,na.rm=T))
  
  if (length(mean.y) == 2){
    p.y<-round(t.test(y~as.factor(x),data=dat)$p.value,4)
  } else{
    p.y<-round(summary(aov(y~as.factor(x)))[[1]][[1,"Pr(>F)"]],4)}
  
  m<-matrix(0,ncol=length(mean.y)+2,nrow=1)
  
  t<-data.frame(rbind(m,c(Variable="", paste0(mean.y,"±",sd),Pval=p.y)))[-1,]
  
  t$Pval<-as.character(t$Pval)
  t$Pval[t$Pval==0]<-"<0.0001"
  
  return(t)
}

##########################
# Geometric Mean (95% CI)#
########################## 
geom_mean_row<-function(y,x){
  
  est<-round(exp(lm(log(y)~as.factor(x)+0,data=dat)$coef))
  ci<-round(exp(confint(lm(log(y)~as.factor(x)+0,data=dat))),1)
  
  if (length(est) == 2){
    p.y<-round(t.test(log(y)~as.factor(x),data=dat)$p.value,4)
  } else{
    p.y<-round(summary(aov(log(y)~as.factor(x)))[[1]][[1,"Pr(>F)"]],4)}
  
  m<-matrix(0,ncol=length(est)+2,nrow=1)
  
  t<-data.frame(rbind(m,c(Variable="", paste0(est," (",ci[,1],", ",ci[,2],")"),Pval=p.y)))[-1,]
  
  t$Pval<-as.character(t$Pval)
  t$Pval[t$Pval==0]<-"<0.0001"
  
  return(t)
}

#################################
# Geometric Mean +1, -1 (95% CI)#
################################# 

geom_mean_row_b<-function(y,x){
  
  est<-round(exp(lm(log(y+1)~as.factor(x)+0,data=dat)$coef)-1)
  ci<-round(exp(confint(lm(log(y)~as.factor(x)+0,data=dat)))-1)
  
  if (length(est) == 2){
    p.y<-round(t.test(log(y+1)~as.factor(x),data=dat)$p.value,4)
  } else{
    p.y<-round(summary(aov(log(y+1)~as.factor(x)))[[1]][[1,"Pr(>F)"]],4)}
  
  m<-matrix(0,ncol=length(est)+2,nrow=1)
  
  t<-data.frame(rbind(m,c(Variable="", paste0(est," (",ci[,1],", ",ci[,2],")"),Pval=p.y)))[-1,]
  
  t$Pval<-as.character(t$Pval)
  t$Pval[t$Pval==0]<-"<0.0001"
  
  return(t)
  
}

###############
# Median (IQR)#
############### 
median_row<-function(y,x){
  
  med.y<-round(tapply(y,x,median,na.rm=T),2)
  q1<-tapply(y,x,function(x) quantile(x,.25,na.rm=T))
  q2<-tapply(y,x,function(x) quantile(x,.75,na.rm=T))
  
  if (length(est) == 2){
    p.y<-round(wilcox.test(y~x)$p.value,4)
  } else{
    p.y<-round(kruskal.test(y~x)$p.value,4)}
  
  m<-matrix(0,ncol=length(med.y)+2,nrow=1)
  
  t<-data.frame(rbind(m,c(Variable="", paste0(med.y," (",q1[,1],", ",q2[,2],")"),Pval=p.y)))[-1,]
  
  t$Pval[t$Pval==0]<-"<0.0001"
  
  return(t)
}
####################################################################################################
#####################################   Row Functions for 2 groups       ###########################
####################################################################################################

###################
# No. (percentage)#
###################
prop_table<- function(y,x) {
  
  temp<-table(y,x)
  temp.prop<-round(prop.table(temp,2)*100,0)
  p<-round(fisher.test(temp)$p.value,4)
  
  #put rows together
  t<-data.frame(Variable=c("",paste0(levels(as.factor(y)))),
                
                one=c("",paste0(temp[,1]," (",temp.prop[,1],"%",")")),
                two=c("",paste0(temp[,2]," (",temp.prop[,2],"%",")")),
                
                Pval=c(p,rep("",length(levels(as.factor(y))))))
  
  t$Pval<-as.character(t$Pval)
  t$Pval[t$Pval==0]<-"<0.0001"
  
  return(t)
}
##################################################################################################################
#############################################
# Second row for binary categorical variable#
#############################################
prop_table_one<- function(y,x) {
  
  temp<-table(y,x)
  temp.prop<-prop.table(temp,2)
  p<-format(round(fisher.test(temp)$p.value,4),scientific=FALSE)
  
  #put row together
  t<-data.frame(Variable=c(""),
                one=c(paste(temp[2,1],paste0("(",round(temp.prop[2,1]*100,0),"%",")"))),
                two=c(paste(temp[2,2],paste0("(",round(temp.prop[2,2]*100,0),"%",")"))),
                Pval=c(p),row.names=NULL)
  
  t$Pval<-as.character(t$Pval)
  t$Pval[t$Pval==0]<-"<0.0001"
  
  return(t)
}
##############
# Mean +/- sd#
############## 

mean_table<-function(y,x){
  
  test.mean.y<-t.test(y~x)
  mean.y<-round(test.mean.y$estimate)
  sd<-round(tapply(y,x,sd,na.rm=T))
  p<-round(test.mean.y$p.value,4)
  
  #put row together
  t<-data.frame(Variable="",
                
                one=paste0(mean.y[1],"±",sd[1]),
                two=paste0(mean.y[2], "±",sd[2]),
                
                Pval=p)
  
  t$Pval[t$Pval==0]<-"<0.0001"
  
  return(t)
}
#################################################################################################################
##########################
# Geometric Mean (95% CI)#
########################## 

geom_table<-function(y,x){
  
  model.y<-lm(log(y) ~ 0 + x)
  mean.y<-round(exp(model.y$coeff))
  mean.y.ci<-round(exp(confint(model.y)))
  p<-round(t.test(log(y)~x)$p.value,4)
  
  #put row together
  t<-data.frame(Variable="",
                
                one=paste0(mean.y[1]," (",mean.y.ci[1,1],", ",mean.y.ci[1,2],")"),
                two=paste0(mean.y[2]," (",mean.y.ci[2,1],", ",mean.y.ci[2,2],")"),
                
                Pval=p)
  
  t$Pval[t$Pval==0]<-"<0.0001"
  
  return(t)
}
#############################################################################################################
#################################################################################################################
#################################
# Geometric Mean +1, -1 (95% CI)#
################################# 

geom_table_b<-function(y,x){
  
  model.y<-lm(log(y+1) ~ 0 + x)
  mean.y<-round(exp(model.y$coeff))-1
  mean.y.ci<-round(exp(confint(model.y)))-1
  p<-round(t.test(log(y+1)~x)$p.value,4)
  
  #put row together
  t<-data.frame(Variable="",
                
                one=paste0(mean.y[1]," (",mean.y.ci[1,1],", ",mean.y.ci[1,2],")"),
                two=paste0(mean.y[2]," (",mean.y.ci[2,1],", ",mean.y.ci[2,2],")"),
                
                Pval=p)
  
  t$Pval[t$Pval==0]<-"<0.0001"
  
  return(t)
}
#############################################################################################################
###############
# Median (IQR)#
############### 

wilcox_table<-function(y,x){
  
  med.y<-round(tapply(y,x,median,na.rm=T),2)
  q1<-tapply(y,x,function(x) quantile(x,.25,na.rm=T))
  q2<-tapply(y,x,function(x) quantile(x,.75,na.rm=T))
  p<-round(wilcox.test(y~x)$p.value,4)
  
  #put row together
  t<-data.frame(Variable="",
                
                one=paste0(med.y[1]," (",q1[1],", ",q2[1],")"),
                two=paste0(med.y[2]," (",q1[2],", ",q2[2],")"),
                
                Pval=p)
  
  t$Pval[t$Pval==0]<-"<0.0001"
  
  return(t)
}

#######################################################################################################

####################################################################################################
#####################################   Row Functions for 3 groups       ###########################
####################################################################################################

AOV_means<-function(y,x){
  mean.x<-round(tapply(y,x,mean,na.rm=T))
  sd<-round(tapply(y,x,sd,na.rm=T))
  
  fit<-aov(y~as.factor(x))
  
  #p.tuk<-TukeyHSD(fit)
  
  t<-data.frame(Variable="",
                one=paste0(mean.x[1],"±",sd[1]),
                two=paste0(mean.x[2],"±",sd[2]), 
                tre=paste0(mean.x[3],"±",sd[3]),
                
                Pval=round(summary(fit)[[1]][[1,"Pr(>F)"]],4))
  
  #                 pval2= round(p.tuk[[1]][[1,"p adj"]],4),
  #                 pval3= round(p.tuk[[1]][[2,"p adj"]],4),
  #                 pval4= round(p.tuk[[1]][[3,"p adj"]],4))
  
  t$Pval<-as.character(t$Pval)
  t$Pval[t$Pval==0]<-"<0.0001"
  
  return(t)
}

median_row<-function(y,x){
  median.x<-round(tapply(y,x,mean,na.rm=T))
  q1<-tapply(y,x,function(x) quantile(x,.25,na.rm=T))
  q2<-tapply(y,x,function(x) quantile(x,.75,na.rm=T))
  p<-round(kruskal.test(y~x)$p.value,4)
  
  t<-data.frame(Variable="",
                one=paste0(median.x[1]," (",q1[1],", ",q2[1],")"),
                two=paste0(median.x[2]," (",q1[2],", ",q2[2],")"), 
                tre=paste0(median.x[3]," (",q1[3],", ",q2[3],")"),
                Pval=p)
  
  t$Pval<-as.character(t$Pval)
  t$Pval[t$Pval==0]<-"<0.0001"
  
  return(t)
}

AOV_geom_means<-function(y,x){
  est<-lm(log(y)~as.factor(x)+0,data=dat)
  ci<-round(exp(confint(est)),1)
  
  fit<-aov(log(y)~as.factor(x))
  
  t<-data.frame(Variable="",
                one=paste0(round(exp(est$coef[1]))," (",ci[1,1],", ",ci[1,2],")" ),
                two=paste0(round(exp(est$coef[2]))," (",ci[2,1],", ",ci[2,2],")" ), 
                tre=paste0(round(exp(est$coef[3]))," (",ci[3,1],", ",ci[3,2],")" ),
                Pval=round(summary(fit)[[1]][[1,"Pr(>F)"]],4))
  
  t$Pval<-as.character(t$Pval)
  t$Pval[t$Pval==0]<-"<0.0001"
  
  return(t)
}

AOV_geom_meansb<-function(y,x){
  est<-lm(log(y+1)~as.factor(x)+0,data=dat)
  ci<-round(exp(confint(est)),1)-1
  
  fit<-aov(log(y+1)~as.factor(x))
  
  t<-data.frame(Variable="",
                one=paste0(round(exp(est$coef[1]))-1," (",ci[1,1],", ",ci[1,2],")" ),
                two=paste0(round(exp(est$coef[2]))-1," (",ci[2,1],", ",ci[2,2],")" ), 
                tre=paste0(round(exp(est$coef[3]))-1," (",ci[3,1],", ",ci[3,2],")" ),
                Pval=round(summary(fit)[[1]][[1,"Pr(>F)"]],4))
  
  t$Pval<-as.character(t$Pval)
  t$Pval[t$Pval==0]<-"<0.0001"
  
  return(t)
}

########################################################################################
prop_table_3<- function(y,x) {
  
  temp<-table(y,x)
  temp.prop<-prop.table(temp,2)
  p<-format(round(chisq.test(temp)$p.value,4),scientific=FALSE)
  
  #put row together
  t<-data.frame(Variable=c("",levels(as.factor(y))),
                one=c("",paste(temp[,1],paste0("(",round(temp.prop[,1]*100,0),"%",")"))),
                two=c("",paste(temp[,2],paste0("(",round(temp.prop[,2]*100,0),"%",")"))),
                tre=c("",paste(temp[,3],paste0("(",round(temp.prop[,3]*100,0),"%",")"))),
                Pval=c(p,rep("",length(levels(as.factor(y))))),row.names=NULL)
  
  t$Pval<-as.character(t$Pval)
  t$Pval[t$Pval==0]<-"<0.0001"
  
  return(t)
}

prop_table_one_3<- function(y,x) {
  
  temp<-table(y,x)
  temp.prop<-prop.table(temp,2)
  p<-format(round(fisher.test(temp)$p.value,4),scientific=FALSE)
  
  #put row together
  t<-data.frame(Variable=c(""),
                one=c(paste(temp[2,1],paste0("(",round(temp.prop[2,1]*100,0),"%",")"))),
                two=c(paste(temp[2,2],paste0("(",round(temp.prop[2,2]*100,0),"%",")"))),
                tre=c(paste(temp[2,3],paste0("(",round(temp.prop[2,3]*100,0),"%",")"))),
                Pval=c(p),row.names=NULL)
  
  t$Pval<-as.character(t$Pval)
  t$Pval[t$Pval==0]<-"<0.0001"
    
  return(t)
}
####################################################################################################

