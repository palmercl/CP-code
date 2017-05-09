#bring in row functions 
#source('01_Row_functions.r')

#######################################################################################################
#these functions work regardless of number of columns in table

#set column names with sample sizes
column_label<-function(x){
  c("Variable", paste0("All (n=",sum(table(x)),")"),
    paste0(levels(x)," (n=",table(x),")"),
    "P Value")}

#Section function - specify variable list, group, 'row function' and any additional notation
fx_section<-function(var_list,x,fx_row,l){
  if (var_list == 'none'){
    temp<-NULL
    return(temp)}
  else{
    temp<-do.call(rbind,apply(dat[var_list],2,fx_row,x))    
    temp$Variable<-as.character(temp$Variable)
    temp$Variable[temp$Variable==""]<-paste0(label(dat[var_list]),l)
    return(temp)}
}
#######################################################################################################

############################################################################################################
                                      #N Column Table#
############################################################################################################

#########################################################################################################
tab<-function(cat_var,cat_var_one,cont_var,geom_var,geom_varb,w_var,x){
  
  #categorical - all rows
  temp<-fx_section(cat_var,x,prop_row,'')
  
  #categorical - last row of table only 
  temp1<-fx_section(cat_var_one,x,prop_row_one,'')
  
  #continuous rows
  temp2<-fx_section(cont_var,x,mean_row,'')
  
  #continuous skewed (geometric mean) rows
  temp3<-fx_section(geom_var,x,geom_mean_row,'*')
  
  #continuous geometric mean with 1 added
  temp4<-fx_section(geom_varb,x,geom_mean_row_b,'*')
  
  #Wilcox rank-sum test
  temp5<-fx_section(w_var,x,median_row,'**')
  
  #bind rows
  temp_final<-data.frame(rbind(temp,temp1,temp2,temp3,temp4,temp5),row.names=NULL)
  
  #set column names with sample sizes
  colnames(temp_final)<-column_label(x)
  
  return(temp_final)
}

