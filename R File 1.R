library(dplyr)
library(readr)
library(ggplot2)

Data <- read_csv("E:/Accenture/T1Q_master_train_FT.csv")

Data_updated <- Data[Data$Current_quarter != 1  ,]
Data_updated_1 <- Data_updated[is.na(Data_updated$Perc_inc) == FALSE,]


Train <- Data_updated_1[Data_updated_1$Year != '2017',]
Out <- Data_updated_1[Data_updated_1$Year == '2017',]

table(is.na(Train$Current_quarter))
#0 #0

#scatter plot
scatter.smooth(x=Train$Current_quarter, y=Train$Perc_inc, main="perc inc ~ current quat")

#box plot 
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(Train$Current_quarter, main="Current_quarter", sub=paste("Outlier rows: ", boxplot.stats(Train$Current_quarter)$out))  
boxplot(Train$Perc_inc, main="Perc_inc", sub=paste("Outlier rows: ", boxplot.stats(Train$Perc_inc)$out))  

#outlier analysis
out <- boxplot.stats(Train$Perc_inc)$out
out_out <- boxplot.stats(out)$out
boxplot(out_out)

iqr <- IQR(Train$Perc_inc)
quartiles <- quantile(Train$Perc_inc)   

lb <- quartiles[2] - 2* iqr
ub <- quartiles[4] + 2* iqr

newperc_inc <- Train$Perc_inc[Train$Perc_inc >= lb & Train$Perc_inc <= ub]

boxplot(newperc_inc)

#updated data

Train_1 <- Train[Train$Perc_inc >= lb & Train$Perc_inc <= ub,]

#normality check


#install.packages('e1071')
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(Train_1$Perc_inc), main="Density Plot: Perc_inc", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Train_1$Perc_inc), 2)))  # density plot for 'speed'
polygon(density(Train_1$Train$Perc_inc), col="red")
plot(density(Train_1$Current_quarter), main="Density Plot: Current_quarter", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Train_1$Current_quarter), 2)))  # density plot for 'dist'
polygon(density(Train_1$Current_quarter), col="red")

#corelation
cor(Train_1$Perc_inc, Train_1$Current_quarter) 
#-0.1293548
cor(Train_1$Perc_inc, Train_1$Year) 
#-0.08839116
cor(Train_1$Perc_inc, Train_1$FMSDepartment) 
#-0.01355216


#BenefitsPlan encoding
Train_1$BenefitsPlan[Train_1$BenefitsPlan == 'Police'] <- 1 
Train_1$BenefitsPlan[Train_1$BenefitsPlan == 'City'] <- 2 
Train_1$BenefitsPlan[Train_1$BenefitsPlan == 'DWP'] <- 3 
Train_1$BenefitsPlan[Train_1$BenefitsPlan == 'Fire'] <- 4 

Out$BenefitsPlan[Out$BenefitsPlan == 'Police'] <- 1 
Out$BenefitsPlan[Out$BenefitsPlan == 'City'] <- 2 
Out$BenefitsPlan[Out$BenefitsPlan == 'DWP'] <- 3 
Out$BenefitsPlan[Out$BenefitsPlan == 'Fire'] <- 4

Train_1$BenefitsPlan <- as.numeric(Train_1$BenefitsPlan)
Out$BenefitsPlan <- as.numeric(Out$BenefitsPlan)

cor(Train_1$Perc_inc, Train_1$BenefitsPlan) 
#-0.01934686


#linear model
linearMod <- lm(Perc_inc ~ Current_quarter + BenefitsPlan + Year + FMSDepartment, data=Train_1)  # build linear regression model on full data
print(linearMod)

Test <- (data.frame(Out))
actual <- (data.frame(Out$Perc_inc))

LM_Pred <- predict(linearMod, Test)

actuals_preds <- data.frame(cbind(actual*100, LM_Pred*100))
cor(actuals_preds)


min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  



AIC(linearMod)
------------------------------------------------------

  na.test <-  function (x) {
  w <- sapply(x, function(x)any(is.na(x)))
  paste(x[w])
}

na.test(Train)


x <- sapply(Train, function(x)any(is.na(x)))
x[x==TRUE]
# Payroll Department          
#Record Number   
#Hourly or Event Rate 
#Payments Over Base Pay 
#% Over Base Pay           
#Lump Sum Pay           
#Overtime Pay                    
#MOU
#MOU Title              
#Pay Grade          
#Benefits Plan 


table(is.na(Train$`Payroll Department`))
#56271
table(is.na(Train$`Payroll Department`))

table(is.na(Train$`MOU`))
#29144
table(is.na(Train$`MOU Title`))
#21021
table(is.na(Train$`% Over Base Pay`))
#56271




