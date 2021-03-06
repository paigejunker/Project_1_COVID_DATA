##Project I
library(readxl)
COVID_data <- read_excel("Desktop/Econ 497/DATA/COVID data.xlsx")
View(COVID_data)
summary(COVID_data)
COVID_data_C<-COVID_data ##create new dataset from starting point COVID_data 
library(ggplot2)
library(mgcv)

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

##Covariance and Correlation Matrices: Training Data
install.packages("corrplot")
library(tseries)
library(quantmod)

COVID_ProjectII_2 = subset(COVID_ProjectII, select = -c(Deaths_100_Recovered) )
View(COVID_ProjectII_2)
COV<-cov(Training[, 2:13])
View(COV) 
COR<-cor(Training[, 2:13])
View(COR)

##Project II: Initital Exploration (NOT USED WHEN RUNNING MY ACTUAL REGRESSIONS)
library(ggplot2) #for ggplot system and preloaded datasets
library(plyr) #for ddply()
library(tseries) #for the J-B test

MODEL1<-lm( New_Cases ~ Deaths+Week_Percent_Increase+Week_Change+ Confirmed  + Recovered  + New_Deaths + Deaths_100_Cases + Recovered_100_Cases, COVID_ProjectII_2) #BUILD THE MODEL OBJECT USING lm()
summary(MODEL1) #REPORT SUMMARY OUTPUT OF Model 1

MODEL1$coefficients #RETURNS BETA ESTIMATES
MODEL1$residuals #RETURNS RESIDUALS
MODEL1$fitted.values #RETURNS FITTED VALUES

MODEL2<-lm( New_Cases ~0+Deaths+Week_Change+ Confirmed  + Recovered  + New_Deaths + Deaths_100_Cases, COVID_ProjectII_2) #BUILD THE MODEL OBJECT USING lm() forcing through the intercept
summary(MODEL2) #REPORT SUMMARY OUTPUT OF THE MODEL OBJECT

MODEL2$coefficients #RETURNS BETA ESTIMATES
MODEL2$residuals #RETURNS RESIDUALS
MODEL2$fitted.values #RETURNS FITTED VALUES

MODEL3<-lm( New_Cases ~0+Deaths+Week_Change+ Confirmed  + Recovered  + New_Deaths + Deaths_100_Cases+Europe+Americas+Africa+Eastern_Mediterranean+`Western Pacific Dummy`+South_East_Asia, COVID_ProjectII_2) #BUILD THE MODEL forcing through the intercept and including the dummy variables for region
summary(MODEL3) #REPORT SUMMARY OUTPUT OF THE MODEL OBJECT

MODEL4<-lm( New_Cases ~0+Deaths+Week_Change+ Confirmed  + Recovered  + New_Deaths +Europe+South_East_Asia, COVID_ProjectII_2) #BUILD THE MODEL using the statistically significant variables from the Model prior
summary(MODEL4) #REPORT SUMMARY OUTPUT OF THE MODEL 

##Partitioning the Data into Training and Testing
p<-.7
obs_count<-dim(COVID_ProjectII_2)[1]
training_size<-floor(p*obs_count)
training_size
set.seed(1234)
train_ind<-sample(obs_count, size=training_size)
Training<-COVID_ProjectII_2[train_ind, ]
Testing<-COVID_ProjectII_2[-train_ind,]
dim(Training)
dim(Testing)

##Building the Models: USED IN MY REGRESSION/MODEL ANALYSIS
M1<-lm( New_Cases ~ Deaths+Week_Percent_Increase+Week_Change+ Confirmed  + Recovered  + New_Deaths + Deaths_100_Cases + Recovered_100_Cases, Training) #using the training data
summary(M1) #REPORT SUMMARY OUTPUT OF THE MODEL OBJECT

M2<-lm( New_Cases ~0+Deaths+Week_Change+ Confirmed  + Recovered  + New_Deaths + Deaths_100_Cases, Training) #BUILD THE MODEL OBJECT USING lm()
summary(M2) #REPORT SUMMARY OUTPUT OF THE MODEL OBJECT

M3<-lm( New_Cases ~0+Deaths+Week_Change+ Confirmed  + Recovered  + New_Deaths + Deaths_100_Cases+Europe+Americas+Africa+Eastern_Mediterranean+`Western Pacific Dummy`+South_East_Asia, Training) #BUILD THE MODEL OBJECT USING lm()
summary(M3) #REPORT SUMMARY OUTPUT OF THE MODEL OBJECT

M4<-lm( New_Cases ~0+Deaths+Week_Change+ Confirmed  + Recovered  + New_Deaths +Europe+South_East_Asia, Training) #BUILD THE MODEL OBJECT USING lm()
summary(M4) #REPORT SUMMARY OUTPUT OF THE MODEL OBJECT

M5<-lm( New_Cases ~0+Week_Change+ Confirmed  + Recovered  + New_Deaths +Europe+South_East_Asia, Training) #BUILD THE MODEL OBJECT USING lm()
summary(M5) #REPORT SUMMARY OUTPUT OF THE MODEL OBJECT

##Predictions
#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_1_IN <- predict(M1, Training) #generate predictions on the (in-sample) training data
PRED_1_OUT <- predict(M1, Testing) #generate predictions on the (out-of-sample) testing data
RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$New_Cases)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$New_Cases)^2)/length(PRED_1_OUT)) #computes out-of-sample 
RMSE_1_IN #IN-SAMPLE ERROR
RMSE_1_OUT #OUT-OF-SAMPLE ERROR

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_2_IN <- predict(M2, Training) #generate predictions on the (in-sample) training data
PRED_2_OUT <- predict(M2, Testing) #generate predictions on the (out-of-sample) testing data
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$New_Cases)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$New_Cases)^2)/length(PRED_2_OUT)) #computes out-of-sample 
RMSE_2_IN #IN-SAMPLE ERROR
RMSE_2_OUT #OUT-OF-SAMPLE ERROR

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_3_IN <- predict(M3, Training) #generate predictions on the (in-sample) training data
PRED_3_OUT <- predict(M3, Testing) #generate predictions on the (out-of-sample) testing data
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$New_Cases)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$New_Cases)^2)/length(PRED_3_OUT)) #computes out-of-sample 
RMSE_3_IN #IN-SAMPLE ERROR
RMSE_3_OUT #OUT-OF-SAMPLE ERROR

#GENERATING PREDICTIONS ON THE TRAINING DATA
PRED_4_IN <- predict(M4, Training) #generate predictions on the (in-sample) training data
PRED_4_OUT <- predict(M4, Testing) #generate predictions on the (out-of-sample) testing data
RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$New_Cases)^2)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$New_Cases)^2)/length(PRED_4_OUT)) #computes out-of-sample 
RMSE_4_IN #IN-SAMPLE ERROR
RMSE_4_OUT #OUT-OF-SAMPLE ERROR

#Model Comparison

#COMPARISON OF IN-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_IN 
RMSE_2_IN 
RMSE_3_IN
RMSE_4_IN 

#COMPARISON OF OUT-OF-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_OUT 
RMSE_2_OUT 
RMSE_3_OUT 
RMSE_4_OUT
