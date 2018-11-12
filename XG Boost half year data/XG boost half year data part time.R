

library(dplyr)
library(readr)
library(ggplot2)
library(xgboost)
library(Metrics)

## Load data
half_data <- read_csv("E:/Accenture/part_time_half_data.csv")
New_half <-  half_data[half_data$Prev_Half_avg_Pay > 0 & half_data$H_avg_Pay > 0,]  


#outlier analysis
#----------------------
#for Prev_Half_avg_Pay
iqr <- IQR(New_half$Prev_Half_avg_Pay)


quartiles <- quantile(New_half$Prev_Half_avg_Pay)   

lb <- quartiles[2] - 2* iqr
ub <- quartiles[4] + 2* iqr

New_half <- New_half[New_half$Prev_Half_avg_Pay >= lb & New_half$Prev_Half_avg_Pay <= ub,]




#test<- New_half[New_half$Year == '2017',]

train <- New_half
test <- read_csv("E:/Accenture/part time 2018 update.csv")

train_x <- (as.matrix(data.frame(train$Prev_Half_avg_Pay , train$Year) ))

set.seed(20181111)
mod1 <- xgboost(data = train_x, label = as.matrix(train$H_avg_Pay),
                nrounds=2200,verbose=FALSE,objective='reg:linear',
                eval_metric='rmse',nthread=8,eta=0.01,gamma=0.0468,
                max_depth=6,min_child_weight=1.7817,subsample=0.5213,
                colsample_bytree=0.4603)



#pred1 <- predict(mod1, newdata = as.matrix(test$Prev_Half_avg_Pay) )

pred1 <- predict(mod1, newdata = as.matrix(test$Pay) )


out <- test
out$Pred <- (pred1)

View(out) 

plot(out$Pay, out$Pred)
#plot(out$Prev_Half_avg_Pay, out$Pred)

write_csv(out,  "E:/Accenture/2018_parttime.csv")

#######################################################
actual1 <- test$H_avg_Pay
rmse(actual1, pred1)
evaluate <- (data.frame(pred1, actual1,  tf = pred1 <= actual1+500 &  pred1 >= actual1-500))
table(evaluate$tf)


