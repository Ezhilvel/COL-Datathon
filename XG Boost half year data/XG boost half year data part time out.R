

library(dplyr)
library(readr)
library(ggplot2)
library(xgboost)
library(Metrics)

## Load data
half_data <- read_csv("E:/Accenture/part_time_half_data.csv")
New_half <-  half_data[half_data$Prev_Half_avg_Pay > 0 & half_data$H_avg_Pay > 0,]  



#test<- New_half[New_half$Year == '2017',]

train <- New_half[New_half$Prev_Half_avg_Pay >= 10000,]
test <- read_csv("E:/Accenture/part time 2018 update.csv")
test <- test[test$Pay >= 10000,]

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

write_csv(out,  "E:/Accenture/2018_parttime_out.csv")

#######################################################



