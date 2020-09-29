#I read in the data from my working directory

Demographic <- readRDS("CDI_data.rda")
# The number of active physicians (Y) is regressed against 
# total population (X1), total personal income (X2), and
# geographic area (X3,X4,X5) 
head(Demographic)

#I write a csv file to my local machine and recode and clean the data to prepare for analysis


write.csv(Demographic, 'Demographic.csv')
Region<-read.csv("C:/Users/micha/OneDrive/Documents/Demographic/Demographic/Demographic.csv")
Region<-as.data.frame(Region)
attach(Region)
summary(Region)
#I make a first order regression model
model <- lm(Y ~ X1 + X2 + X3 + X4 + X5, data = Region)
summary(model)
#The overall model is significant and each regressor is significant at a 10% significance level.
#The Southern region is most statistically significant with a p-value of 1.731%
#The Northeast region is statistically significant at p-value 8.685%
#The North Central region is statistcally significant at p-value 8.817%
#The intercept of the model is close to zero
#If a randomly selected State and City are in the southern indexed region holding all other predictors constant, the number of predicted physicians increase by approximately 191.
#If a randomly selected State and City are in the Northeast region holding all other predictors constant, the number of predicted physicians increases by approximately 149.
#If a randomly selected State and City are in the North Central region holding all other predictors constant, the number of predicted physicians increases by approximately 145.5.
#The coefficient for X1(Total Population) is significant but close to zero, however, because it is associated with population, the interpretation is perhaps more important than its relative proximity to zero. According the the model, every 100,000 people in a population there is approximately 55.15 physicians
#The coefficient for X2(Total Personal Income) is significant. According to the model, for every additional $1000 in personal income the number of predicted physicians increases by 107.

par(mfrow=c(3,3))
Training<-read.csv("C:/Users/micha/OneDrive/Documents/Demographic/Demographic/Training.csv")
Training <- as.data.frame(Training)
attach(Training)
pairs(Training[1:4],col="black",pch=20)
pairs(Training[4:8],col="black",pch=20)
pairs(Training[8:12],col="black",pch=20)
cor(Training)
library(corrplot)
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(Training,histogram = TRUE, pch=20)

library(olsrr)
model3<-lm( Y ~ .,data=Training)
k <- ols_step_all_possible(model3)
plot(k)
k1 <- ols_step_best_subset(model3)
plot(k1)
k2<-ols_step_forward_p(model3)
plot(k2)
k2

full_model_Training <- lm(Y ~ NumHospitalBeds + TotalPIncome + PerCapitaIncome + NumPhysicians + PercentCollDeg + PercentBelowPov ,data=Training)
summary(full_model_Training)
par(mfrow=c(2,2))
plot(full_model_Training)
anova(full_model_Training)
Test<-read.csv("C:/Users/micha/OneDrive/Documents/Demographic/Demographic/Test.csv")
Test<-as.data.frame(Test)
attach(Test)

full_model_Test <- lm(Y ~ NumHospitalBeds + TotalPIncome + PerCapitaIncome + NumPhysicians + PercentCollDeg + PercentBelowPov, data = Test)
summary(full_model_Test)
anova(full_model_Test)
plot(full_model_Test)

full_model <- lm(Y ~ NumHospitalBeds + TotalPIncome + PerCapitaIncome + NumPhysicians + PercentCollDeg + PercentBelowPov,data = Demographic)
summary(full_model)
anova(full_model)
plot(full_model)




Regression_model <- lm(Y ~ PercentPopYoung + NumPhysicians + NumHospitalBeds + PercentBelowPov + PercentUnemploy + PerCapitaIncome, data = Training)
summary(Regression_model)
plot(Regression_model)
plot(Regression_model$fitted.values,Regression_model$residuals,col="blue", xlab="Fitted",ylab="Residuals",pch=20)
plot( PercentPopYoung, Regression_model$residuals,col="blue",xlab="Percent Population ages 18-34",ylab="Residuals",pch=20)
plot(NumPhysicians,Regression_model$residuals,col="blue",xlab="Number of Active Physicians",ylab="Residuals",pch=20)
plot(NumHospitalBeds,Regression_model$residuals,col="blue",xlab="Number of Hospital Beds",ylab="Residuals",pch=20)
plot(PercentBelowPov,Regression_model$residuals,col="blue",xlab="Percent Popultion Below Poverty Line",ylab="Residuals",pch=20)
plot(PercentUnemploy,Regression_model$residuals,col="blue",xlab="Percent Unemployed",ylab="Residuals",pch=20)
plot(PerCapitaIncome,Regression_model$residuals,col="blue",xlab="Per Capita Income",ylab="Residuals",pch=20)


Training_Modify <- cbind(Y,PercentPopYoung,NumPhysicians,NumHospitalBeds,PercentBelowPov,PercentUnemploy,PerCapitaIncome)
chart.Correlation(Training_Modify,histogram = TRUE, pch=20)
