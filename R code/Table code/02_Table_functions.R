#bring in row functions 
#source('01_Row_functions.r')

#######################################################################################################
#these functions work regardless of number of columns in table

#set column names with sample sizes
column_label<-function(x){
  c("Variable",
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
                                         #Two Column Table#
############################################################################################################

#########################################################################################################
tab_2<-function(cat_var,cat_var_one,cont_var,geom_var,geom_varb,w_var,x){
  
  #categorical - all rows
  temp<-fx_section(cat_var,x,prop_table,'')
  
  #categorical - last row of table only 
  temp1<-fx_section(cat_var_one,x,prop_table_one,'')
  
  #continuous rows
  temp2<-fx_section(cont_var,x,mean_table,'')
  
  #continuous skewed (geometric mean) rows
  temp3<-fx_section(geom_var,x,geom_table,'*')
  
  #continuous geometric mean with 1 added
  temp4<-fx_section(geom_varb,x,geom_table_b,'*')
  
  #Wilcox rank-sum test
  temp5<-fx_section(w_var,x,wilcox_table,'**')
  
  #bind rows
  temp_final<-data.frame(rbind(temp,temp1,temp2,temp3,temp4,temp5),row.names=NULL)
  
  #set column names with sample sizes
  colnames(temp_final)<-column_label(x)
  
  return(temp_final)
}

############################################################################################################
                                          #Three Column Table for 3 groups#
############################################################################################################

tab_3<-function(cat_var,cat_var_one,cont_var,geom_var,geom_varb,x){
  
  #categorical - all rows
  temp<-fx_section(cat_var,x,prop_table,'')
  
  #categorical - last row of table only 
  temp1<-fx_section(cat_var_one,x,prop_table_one,'')
  
  #continuous rows
  temp2<-fx_section(cont_var,x,AOV_means,'')
  
  #continuous skewed (geometric mean) rows
  temp3<-fx_section(geom_var,x,AOV_geom_means,'*')
  
  #continuous geometric mean with 1 added
  temp4<-fx_section(geom_varb,x,AOV_geom_meansb,'*')
  
  #bind rows
  temp_final<-data.frame(rbind(temp,temp1,temp2,temp3,temp4),row.names=NULL)
  
  #set column names with sample sizes
  colnames(temp_final)<-column_label(x)
  
  return(temp_final)
}