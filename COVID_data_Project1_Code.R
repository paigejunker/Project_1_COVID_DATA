##Cleaning the Dataset
library(readxl)
COVID_data <- read_excel("Desktop/Econ 497/DATA/COVID data.xlsx")
View(COVID_data)
head(COVID_data)
tail(COVID_data)
summary(COVID_data)

COVID_data_C<-COVID_data ##create new dataset from starting point COVID_data 
COVID_data_C$`Eastern Mediterranean Dummy`[COVID_data_C$`Eastern Mediterranean Dummy`==0]<-NA
COVID_data_C$`Europe Dummy`[COVID_data_C$`Europe Dummy`==0]<-NA
COVID_data_C$`Africa Dummy`[COVID_data_C$`Africa Dummy`==0]<-NA
COVID_data_C$`Americas Dummy`[COVID_data_C$`Americas Dummy`==0]<-NA
COVID_data_C$`Western Pacific Dummy`[COVID_data_C$`Western Pacific Dummy`==0]<-NA
COVID_data_C$`South-East Asia Dummy`[COVID_data_C$`South-East Asia Dummy`==0]<-NA
COVID_data_C$`Deaths / 100 Recovered`[COVID_data_C$`Deaths / 100 Recovered`=="inf"]<-NA
View(COVID_data_C)
dim(COVID_data_C) #no data lost-same as dim(COVID_data) eliminated the NA variables

COVID_data_D$`Deaths`[COVID_data_D$`Deaths`>=85000]<-NA ##got rid of the two highest values for a better visual
View(COVID_data_D)

##Exploratory Analysis
library(ggplot2)
class(COVID_data_D)
ggplot(COVID_data_C, aes(x=Deaths, y=Recovered))
ggplot(COVID_data_C, aes(x=Deaths, y=Recovered))+
  geom_point()

COVID_data_D<-COVID_data_C ##create new dataset from starting point COVID_data 
COVID_data_D$`Deaths`[COVID_data_D$`Deaths`>=85000]<-NA ##got rid of the two highest values for a better visual
View(COVID_data_D)

ggplot(COVID_data_D, aes(x=Deaths, y=Recovered)) ##using the new data set not including Brazil and the US
ggplot(COVID_data_D, aes(x=Deaths, y=Recovered))+
  geom_point()

ggplot(COVID_data_C, aes(x=Deaths, y=Active)) ##using the old data set
ggplot(COVID_data_C, aes(Deaths, Active, color=Recovered))  + ##points corr. to the Recovered variable
  geom_point()
  
##Exploratory Analysis (creating the datasets for each region)
COVID_data_Europe<-subset(COVID_data_C,`Europe Dummy`=="1")##create new dataset only containing the countries in Europe
View(COVID_data_Europe)
COVID_data_EasternMediterranean<-subset(COVID_data_C,`Eastern Mediterranean Dummy`=="1")##create new dataset only containing the countries in the Eastern Mediterranean Region
View(COVID_data_EasternMediterranean)
COVID_data_Africa<-subset(COVID_data_C,`Africa Dummy`=="1")##create new dataset only containing the countries in the Africa Region
View(COVID_data_Africa)
COVID_data_Americas<-subset(COVID_data_C,`Americas Dummy`=="1")##create new dataset only containing the countries in the Africa Region
View(COVID_data_Americas)
COVID_data_WesternPacific<-subset(COVID_data_C,`Western Pacific Dummy`=="1")##create new dataset only containing the countries in the Africa Region
View(COVID_data_WesternPacific)
COVID_data_SouthEastAsia<-subset(COVID_data_C,`South-East Asia Dummy`=="1")##create new dataset only containing the countries in the Africa Region
View(COVID_data_SouthEastAsia)



##Exploratory Analysis ( Analysis for each region without using a smoother)
ggplot(COVID_data_Europe, aes(x=Deaths, y=Recovered))+
  geom_point()
ggplot(COVID_data_EasternMediterranean, aes(x=Deaths, y=Recovered))+
  geom_point()
ggplot(COVID_data_Americas, aes(x=Deaths, y=Recovered))+
  geom_point()
ggplot(COVID_data_WesternPacific, aes(x=Deaths, y=Recovered))+
  geom_point()
ggplot(COVID_data_SouthEastAsia, aes(x=Deaths, y=Recovered))+
  geom_point()

ggplot(COVID_data_Europe, aes(Deaths, Active, color=Recovered))  + ##points corr. to the Recovered variable
  geom_point()
ggplot(COVID_data_EasternMediterranean, aes(Deaths, Active, color=Recovered))  + ##points corr. to the Recovered variable
  geom_point()
ggplot(COVID_data_Americas, aes(Deaths, Active, color=Recovered))  + ##points corr. to the Recovered variable
  geom_point()
ggplot(COVID_data_WesternPacific, aes(Deaths, Active, color=Recovered))  + ##points corr. to the Recovered variable
  geom_point()
ggplot(COVID_data_SouthEastAsia, aes(Deaths, Active, color=Recovered))  + ##points corr. to the Recovered variable
  geom_point()
ggplot(COVID_data_Africa, aes(Deaths, Active, color=Recovered))  + ##points corr. to the Recovered variable
  geom_point()

##Exploratory Analysis ( Analysis for each region using a smoother)
ggplot(COVID_data_Europe, aes(x=Deaths, y=Recovered))+
  geom_point()+ ggtitle("Relationship between Deaths and Recovered in Europe")+
  geom_smooth(span = 0.5) ##more flexible
ggplot(COVID_data_EasternMediterranean, aes(x=Deaths, y=Recovered))+
  geom_point()+ggtitle("Relationship between Deaths and Recovered in EasternMediterranean")+
  geom_smooth(span = 0.5) ##more flexible
ggplot(COVID_data_Americas, aes(x=Deaths, y=Recovered))+
  geom_point()+ggtitle("Relationship between Deaths and Recovered in Americas")+
  geom_smooth(span = 0.5) ##more flexible
ggplot(COVID_data_WesternPacific, aes(x=Deaths, y=Recovered))+
  geom_point()+ggtitle("Relationship between Deaths and Recovered in WesternPacific")+
  geom_smooth(span = 0.5) ##more flexible
ggplot(COVID_data_SouthEastAsia, aes(x=Deaths, y=Recovered))+
  geom_point()+ggtitle("Relationship between Deaths and Recovered in SouthEastAsia")+
  geom_smooth(span = 0.5) ##more flexible 

##Exploratory Analysis (violin plot)
ggplot(COVID_data_Americas, aes(x=Deaths, y=Recovered))+
  geom_violin()+ggtitle("Relationship between Deaths and Recovered in Americas")
ggplot(COVID_data_Europe, aes(x=Deaths, y=Recovered))+
  geom_violin()+ ggtitle("Relationship between Deaths and Recovered in Europe")
ggplot(COVID_data_EasternMediterranean, aes(x=Deaths, y=Recovered))+
  geom_violin()+ggtitle("Relationship between Deaths and Recovered in EasternMediterranean")
ggplot(COVID_data_WesternPacific, aes(x=Deaths, y=Recovered))+
  geom_violin()+ggtitle("Relationship between Deaths and Recovered in WesternPacific")
ggplot(COVID_data_SouthEastAsia, aes(x=Deaths, y=Recovered))+
  geom_violin()+ggtitle("Relationship between Deaths and Recovered in SouthEastAsia")
 
##Exploratory Analysis ( Analysis for each region using a smoother)
library(mgcv)
ggplot(COVID_data_Europe, aes(x=Deaths, y=Active))+
  geom_point()+ ggtitle("Relationship between Deaths and Active in Europe")+
  geom_smooth(method = "gam", formula = y ~ s(x)) ##generalized additive model
ggplot(COVID_data_EasternMediterranean, aes(x=Deaths, y=Active))+
  geom_point()+ggtitle("Relationship between Deaths and Active in EasternMediterranean")+
  geom_smooth(method = "gam", formula = y ~ s(x)) ##generalized additive model
ggplot(COVID_data_Americas, aes(x=Deaths, y=Active))+
  geom_point()+ggtitle("Relationship between Deaths and Active in Americas")+
  geom_smooth(method = "gam", formula = y ~ s(x)) ##generalized additive model
ggplot(COVID_data_WesternPacific, aes(x=Deaths, y=Active))+
  geom_point()+ggtitle("Relationship between Deaths and Active in WesternPacific")+
  geom_smooth(method = "gam", formula = y ~ s(x)) ##generalized additive model
ggplot(COVID_data_SouthEastAsia, aes(x=Deaths, y=Active))+
  geom_point()+ggtitle("Relationship between Deaths and Active in SouthEastAsia")+
  geom_smooth(method = "gam", formula = y ~ s(x)) ##generalized additive model 
ggplot(COVID_data_C, aes(x=Deaths, y=Active))+
  geom_point()+ggtitle("Relationship between Deaths and Active")+
  geom_smooth(method = "gam", formula = y ~ s(x)) ##generalized additive model 
ggplot(COVID_data_D, aes(x=Deaths, y=Active))+
  geom_point()+ggtitle("Relationship between Deaths and Active with the Removal of Outliers")+
  geom_smooth(method = "gam", formula = y ~ s(x)) ##generalized additive model 

##Exploratory Analysis (Frequency Polygons)

ggplot(COVID_data_SouthEastAsia, aes(Active)) + geom_histogram()+ggtitle("Active COVID Cases in SouthEastAsia")
ggplot(COVID_data_WesternPacific, aes(Active)) + geom_histogram()+ggtitle("Active COVID Cases in WesternPacific")
ggplot(COVID_data_Americas, aes(Active)) + geom_histogram()+ggtitle("Active COVID Cases in Americas")
ggplot(COVID_data_EasternMediterranean, aes(Active)) + geom_histogram()+ggtitle("Active COVID Cases in EasternMediterranean")
ggplot(COVID_data_Europe, aes(Active)) + geom_histogram()+ggtitle("Active COVID Cases in Europe")
ggplot(COVID_data_Africa, aes(Active)) + geom_histogram()+ggtitle("Active COVID Cases in Africa")

##Exploratory Analysis (replaces variables cases=0 with NA)
COVID_data_E<-COVID_data_C ##create new dataset from starting point COVID_data 
COVID_data_E$`Active`[COVID_data_D$`Active`== 0]<-
COVID_data_E$`Deaths`[COVID_data_D$`Deaths`== 0]<-NA 
COVID_data_E$`Recovered`[COVID_data_D$`Recovered`== 0]<-NA 
View(COVID_data_E)


##Exploratory Analysis (using data without 0 values)
ggplot(COVID_data_E, aes(x=Deaths, y=Recovered))+
  geom_point()+ ggtitle("Relationship between Deaths and Recovered with No 0 Values")+
  geom_smooth() ##more flexible
ggplot(COVID_data_E, aes(x=Deaths, y=Recovered))+
  geom_violin()+ggtitle("Relationship between Deaths and Recovered with No 0 Values")
ggplot(COVID_data_E, aes(x=Deaths, y=Active))+
  geom_point()+ ggtitle("Relationship between Deaths and Active with No 0 values")+
  geom_smooth(method = "gam", formula = y ~ s(x)) ##generalized additive model

View(COVID_Data_C)


