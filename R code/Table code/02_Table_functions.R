#bring in row functions 
#source('01_Row_functions.r')

#######################################################################################################
#These functions work regardless of number of columns in table (groups in dataset)#

#set column names with sample sizes
column_label<-function(x){
  c("Variable", paste0("All (n=",sum(table(x)),")"),
    paste0(levels(x)," (n=",table(x),")"),
    "P Value")}

###################################################################################
#determine row function
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

#add * to label for non-normally distributed continuous variables
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
####################
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
###################################################################################
#row based percentages#
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
###########################################################################################
                                            #Old#

#Section function - specify variable list, group, 'row function' and any additional notation
# fx_section<-function(var_list,x,fx_row,l){
#   if (var_list == 'none'){
#     temp<-NULL
#     return(temp)}
#   else{
#     temp<-do.call(rbind,apply(dat[var_list],2,fx_row,x))    
#     temp$Variable<-as.character(temp$Variable)
#     temp$Variable[temp$Variable==""]<-paste0(label(dat[var_list]),l)
#     return(temp)}
# }
#######################################################################################################

############################################################################################################
#N Column Table#
############################################################################################################

#########################################################################################################
# tab<-function(cat_var,cat_var_one,cont_var,geom_var,geom_varb,w_var,x){
#   
#   #categorical - all rows
#   temp<-fx_section(cat_var,x,prop_row,'')
#   
#   #categorical - last row of table only 
#   temp1<-fx_section(cat_var_one,x,prop_row_one,'')
#   
#   #continuous rows
#   temp2<-fx_section(cont_var,x,mean_row,'')
#   
#   #continuous skewed (geometric mean) rows
#   temp3<-fx_section(geom_var,x,geom_mean_row,'*')
#   
#   #continuous geometric mean with 1 added
#   temp4<-fx_section(geom_varb,x,geom_mean_row_b,'*')
#   
#   #Wilcox rank-sum test
#   temp5<-fx_section(w_var,x,median_row,'**')
#   
#   #bind rows
#   temp_final<-data.frame(rbind(temp,temp1,temp2,temp3,temp4,temp5),row.names=NULL)
#   
#   #set column names with sample sizes
#   colnames(temp_final)<-column_label(x)
#   
#   # remove numbering from factor levels
#   temp_final$Variable<-gsub(".*\\^", "", temp_final$Variable)
#   
#   return(temp_final)
# }
# 
# 
# 
# 
