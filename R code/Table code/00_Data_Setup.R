#####################################################################################################
#Load Hmisc library
library(Hmisc)

#dataset name to cite within analysis report and read in 
data_set_name<-'BackTransportOfPrema_DATA_2017-01-04_0811.csv'

#Read Data
data<-read.csv(paste0('filepath',data_set_name),na.strings=c(""))

########################################################################################################
                                          #Labels#
########################################################################################################

#Setting Factors(will create new variable for factors)
data$maternal_race.factor = factor(data$maternal_race,levels=c("1","2","3","4","5","6","7"))

#assign labels to factor levels
levels(data$maternal_race.factor)=c("American Indian or Alaska Native","Asian","African American",
                                    "Hispanic or Latino","Native Hawaiian or Pacific Islander",
                                    "White","More than 1 race")
#Set Labels
label(data$maternal_race.factor)="Maternal Race"