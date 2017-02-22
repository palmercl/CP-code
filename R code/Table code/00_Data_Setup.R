#####################################################################################################
#Load Hmisc library
library(Hmisc)

#dataset name to cite within analysis report and read in 
data_set_name<-'name'

#Read Data
#data<-read.csv(paste0('filepath',data_set_name),na.strings=c(""))

########################################################################################################
                                          #Labels#
########################################################################################################

# #Setting Factors(will create new variable for factors)
# data$maternal_race.factor = factor(data$maternal_race,levels=c("1","2","3","4","5","6","7"))
# 
# #assign labels to factor levels
# levels(data$maternal_race.factor)=c("American Indian or Alaska Native","Asian","African American",
#                                     "Hispanic or Latino","Native Hawaiian or Pacific Islander",
#                                     "White","More than 1 race")
# #Set Labels
# label(data$maternal_race.factor)="Maternal Race"
###################################################################
#use cabbage data for two group comparison
data<-iris

#create dichotomous vitamin C variable 
data$sepal_di<-as.factor((data$Sepal.Length<5)*1)

#set factor levels
levels(data$sepal_di)=c("<5",'>=5')
levels(data$Species)=c('Setosa','Versicolor','Virginica')

#Set Labels
label(data$sepal_di)='Sepal Length'
label(data$Sepal.Length)="Sepal Length"
label(data$Sepal.Width)='Sepal weight'
label(data$Petal.Length)='Petal Length'
label(data$Petal.Width)='Petal Width'



