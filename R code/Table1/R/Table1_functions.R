

####################################################################################################
#####################################   Row Functions for n groups       ###########################
####################################################################################################

#########################################Categorical Variables######################################

###################
# No. (percentage)#
###################
#' A row function
#'
#' This function creates no. and % for each level of the variable, p-value from Chi-sq test
#' @param Variable of interest and group variable 
#' @keywords Categorical, >2 levels  
#' @export
#' @examples
#' prop_row()
#' 
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
#' A row function
#' This function creates no. and % for the second level of the variable, p-value from Chi-sq test
#' @param Variable of interest and group variable 
#' @keywords Categorical, <3 levels  
#' @export
#' @examples
#' prop_row_one()
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

###################
# No. (percentage)#
###################
#' A row function
#'
#' This function creates no. and % for each level of the variable, % based on row total, p-value from Chi-sq test
#' @param Variable of interest and group variable 
#' @keywords Categorical, >2 levels  
#' @export
#' @examples
#' prop_rowb()
prop_rowb<- function(y,x) {
  
  temp<-table(y,x)
  temp.prop<-round(prop.table(temp,1)*100,0)
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
#' A row function
#' This function creates no. and % for the second level of the variable, % based on row total, p-value from Chi-sq test
#' @param Variable of interest and group variable 
#' @keywords Categorical, <3 levels  
#' @export
#' @examples
#' prop_row_oneb()
prop_row_oneb<- function(y,x) {
  
  temp<-table(y,x)
  temp.prop<-round(prop.table(temp,1)*100,0)
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
#' A row function
#'
#' This function creates mean and SD, p-value from t-test or ANOVA
#' @param Variable of interest and group variable 
#' @keywords Continuous  
#' @export
#' @examples
#' mean_row()
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
#' A row function
#'
#' This function creates geometric mean and 95% CI, p-value from t-test or ANOVA
#' @param Variable of interest and group variable 
#' @keywords Continuous  
#' @export
#' @examples
#' geom_mean_row()
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


###############
# Median (IQR)#
############### 
#' A row function
#'
#' This function creates median and IQR, p-value from Wilcox rank-sum or Kruskal-Wallis
#' @param Variable of interest and group variable 
#' @keywords Continuous, non-normal  
#' @export
#' @examples
#' median_row()
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
#' A function to quantify missing data
#'
#' This function creates no. and % of missing data
#' @param Variable of interest and group variable 
#' @keywords Continuous, non-normal  
#' @export
#' @examples
#' missing_row()
missing_row<-function(y,x){
  
  miss<-tapply(y,x, function(x) paste0(round((length(x[is.na(x)==T])/length(x))*100),'%'))
  
  m<-matrix(0,ncol=length(miss)+1,nrow=1)
  
  #put row together
  t<-data.frame(rbind(m,c(Variable="",miss)))
  
  return(t[-1,])
}

#' A function to describe the distribution of a variable 
#' Tabulates and displays mean, geometric mean, median, proportion missing, 
#' histogram and histogram of the log outcome
#' @param Variable of interest and group variable 
#' @keywords Continuous  
#' @export
#' @examples
#' dist_check()
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
########################################################################################################
#######################################################################################################
#These functions work regardless of number of columns in table (groups in dataset)#

#' A function to create column names with sample sizes
#' @param Grouping variable 
#' @keywords Column labels with sample size 
#' @export
#' @examples
#' column_label()
column_label<-function(x){
  c("Variable", paste0("All (n=",sum(table(x)),")"),
    paste0(levels(x)," (n=",table(x),")"),
    "P Value")}
###################################################################################
#' A function to determine which row function to employ
#' @param Variable of interest and group variable 
#' @keywords Categorical, continuous variables 
#' @export
#' @examples
#' fx_rows()
fx_rows<-function(var,group){
  
  if(is.numeric(var)==T){
    
    if(abs(skewness(var,na.rm=T))<3){
      temp<-mean_row(var,group)
    }
    
    else{
      temp<-median_row(var,group)
    }
  }
  
  else if(is.numeric(var)==F & length(levels(var))<3){
    temp<-prop_row_one(var,group)
  }
  
  else{
    temp<-prop_row(var,group)
  }
  
  return(temp)
}

#' A function to generate a * to add to the label for non-normally distributed continuous variables
#' @param Variable of interest
#' @keywords * indication
#' @export
#' @examples
#' fx_symbol()
fx_symbol<-function(var){
  
  if(is.numeric(var)==T){
    
    if(abs(skewness(var,na.rm=T))>3){
      symbol<-"*"
    }
    
    else{
      symbol<-""
    }
  }
  else{
    symbol<-""
  }
  return(symbol)
}
##################################################################################################
#' A function to create the final table
#' @param Variables of interest and group variable
#' @keywords Table 1
#' @export
#' @examples
#' final_table()
final_table<-function(var,group){
  
  #create table
  temp<-data.frame(do.call(rbind,lapply(dat[var],fx_rows,group)),row.names=NULL)
  temp$Variable<-as.character(temp$Variable)
  temp$Variable[temp$Variable==""]<-paste0(label(dat[var]),lapply(dat[(var)],fx_symbol))
  
  #set column names with sample sizes
  colnames(temp)<-column_label(group)
  
  #remove numbering from factor levels
  temp$Variable<-gsub(".*\\^", "", temp$Variable)
  
  return(temp)
}
###################################################################################
#' A function to determine which row function to employ, including row based percentages
#' @param Variable of interest and group variable 
#' @keywords Categorical, continuous variables 
#' @export
#' @examples
#' fx_rowsb()
fx_rowsb<-function(var,group){
  
  if(is.numeric(var)==T){
    
    if(abs(skewness(var,na.rm=T))<3){
      temp<-mean_row(var,group)
      return(temp)}
    
    else{
      temp<-median_row(var,group)
      return(temp)
    }
  }
  else if(is.numeric(var)==F & length(levels(var))<3){
    temp<-prop_row_oneb(var,group)
    return(temp)}
  
  else{
    temp<-prop_rowb(var,group)
    return(temp)}
}
#####################################################
#' A function to create the final table, with row percentages
#' @param Variables of interest and group variable
#' @keywords Table 1
#' @export
#' @examples
#' final_tableb()

final_tableb<-function(var,group){
  
  #create table
  temp<-data.frame(do.call(rbind,lapply(dat[var],fx_rowsb,group)),row.names=NULL)
  temp$Variable<-as.character(temp$Variable)
  temp$Variable[temp$Variable==""]<-paste0(label(dat[var]),lapply(dat[(var)],fx_symbol))
  
  #set column names with sample sizes
  colnames(temp)<-column_label(group)
  
  #remove numbering from factor levels
  temp$Variable<-gsub(".*\\^", "", temp$Variable)
  
  return(temp)
}