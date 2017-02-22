
####################################################
########      Linear regression      #############
####################################################
fx_model_linear<-function(y,x){
  
  f<-formula(paste0(y,"~",x))
  model<-lm(f,data)
  
  ret<-data.frame(Independent_variable=names(coef(model)),
                  Estimate=paste0(round(coef(model),3)," (",
                                  round(confint(model)[,1],3),", ",
                                  round(confint(model)[,2],3),")"),
                  Pvalue=round(summary(model)$coef[,4],4),
                  row.names=NULL)
  ret$Pvalue[ret$Pvalue==0]<-'<0.0001'
  colnames(ret)<-c("Predictor","Estimate (95% CI)","P Value")
  return(ret)
  
}
####################################################
########      Logistic regression      #############
####################################################
fx_model_logistic<-function(y,x){
  
  f<-formula(paste0(y,"~",x))
  model<-glm(f,data,family='binomial')
  
  ret<-data.frame(Independent_variable=substring(x,1,nchar(x)-1),
                  OR=paste0(round(exp(coef(model)[-c(1)]),2)," (",
                            round(exp(confint(model)[,1]),2)[-c(1)],", ",
                            round(exp(confint(model)[,2]),2)[-c(1)],")"),
                  Pvalue=round(summary(model)$coef[,4],4)[-c(1)],
                  row.names=NULL)
  colnames(ret)<-c("Parameter","OR (95% CI)","P Value")
  return(ret)
  
}