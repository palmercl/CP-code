#####################################################################################################
#Load Hmisc library
library(Hmisc)
library(MASS)

#dataset name to cite within analysis report and read in 
data_set_name<-'iris'

#Read Data
#data<-read.csv(paste0('filepath',data_set_name),na.strings=c(""))

########################################################################################################
                                          #Labels#
########################################################################################################

# #Setting Factors(will create new variable for factors)
# data$maternal_race.factor = factor(data$maternal_race,levels=c("1","2","3","4","5","6","7"))
###################################################################

#use iris data
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



