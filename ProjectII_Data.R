##Cleaning the Dataset
library(readxl)
library(mgcv)
COVID_data <- read_excel("Desktop/Econ 497/DATA/COVID data.xlsx")
View(COVID_data)


COVID_data_C<-COVID_data ##create new dataset from starting point COVID_data 
View(COVID_data_C)

##Project II:renaming variables
library(dplyr)
COVID_ProjectII<-COVID_data_C
COVID_ProjectII
colnames(COVID_ProjectII)
COVID_ProjectII<-COVID_ProjectII %>% rename(New_Cases = `New cases`, New_Deaths = `New deaths`, New_Recovered = `New recovered`)
COVID_ProjectII<-COVID_ProjectII %>% rename(Deaths_100_Cases = `Deaths / 100 Cases`, Recovered_100_Cases = `Recovered / 100 Cases`, Deaths_100_Recovered = `Deaths / 100 Recovered`)
COVID_ProjectII<-COVID_ProjectII %>% rename(Confirmed_Last_Week = `Confirmed last week`, Week_Change = `1 week change`, Week_Percent_Increase = `1 week % increase`)
COVID_ProjectII<-COVID_ProjectII %>% rename(Eastern_Mediterranean =`Eastern Mediterranean Dummy`)
COVID_ProjectII<-COVID_ProjectII %>% rename(Europe = `Europe Dummy`)
COVID_ProjectII<-COVID_ProjectII %>% rename(Africa = `Africa Dummy`)
COVID_ProjectII<-COVID_ProjectII %>% rename(Americas = `Americas Dummy`)
COVID_ProjectII<-COVID_ProjectII %>% rename(South_East_Asia = `South-East Asia Dummy`)
View(COVID_ProjectII)


##Covariance and Correlation Matrices
install.packages("corrplot")
library(tseries)
library(quantmod)

COVID_ProjectII_2 = subset(COVID_ProjectII, select = -c(Deaths_100_Recovered) )
View(COVID_ProjectII_2)
COV<-cov(COVID_ProjectII_2[, 2:13])
View(COV)
COR<-cor(COVID_ProjectII_2[, 2:13])
View(COR)


##Project II
library(ggplot2) #for ggplot system and preloaded datasets
library(plyr) #for ddply()
library(tseries) #for the J-B test

MODEL1<-lm(Deaths ~ Active + Recovered + Europe, COVID_ProjectII_2) #BUILD THE MODEL OBJECT USING lm()
summary(MODEL1) #REPORT SUMMARY OUTPUT OF THE MODEL OBJECT

MODEL1$coefficients #RETURNS BETA ESTIMATES
MODEL1$residuals #RETURNS RESIDUALS
MODEL1$fitted.values #RETURNS FITTED (PREDICTED) VALUES

#QUESTION:  ARE THE RESIDUALS NORMAL?
hist(MODEL1$residuals) #PLOT THEM!
jarque.bera.test(MODEL1$residuals) #TEST FOR NORMLAITY!

MODEL2<-lm(Active ~ Deaths + Confirmed + Recovered, df) #BUILD THE MODEL OBJECT USING lm()
summary(MODEL2) #REPORT SUMMARY OUTPUT OF THE MODEL OBJECT




