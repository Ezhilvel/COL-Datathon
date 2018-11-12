
library(dplyr)
library(readr)
library(ggplot2)
library(xgboost)
library(Metrics)
  
  ## Load data

half_data <- read_csv("E:/Accenture/hald data.csv")

New_half <-  half_data[half_data$Prev_Half_avg_Pay != 0 & half_data$H_avg_pay != 0,]  


New_half <- New_half[New_half$Prev_Half_avg_Pay >= 59999,]


New_half_u <- New_half


#plot(New_half_u$H_avg_pay[New_half_u$Year == '2017' | New_half_u$Year == '2016'] , New_half_u$Prev_Half_avg_Pay[New_half_u$Year == '2017' | New_half_u$Year == '2016'] )


   
train <- New_half_u
test <- read_csv("E:/Accenture/fulltime_output.csv")


test <- test[test$Prev_Half_Pay > 5999]
#View(test)

train_x <- (as.matrix(data.frame(train$Prev_Half_avg_Pay , train$Year) ))

set.seed(20181111)
#XG boost (best so far)

mod1 <- xgboost(data = train_x, label = as.matrix(train$H_avg_pay),
                nrounds=2200,verbose=FALSE,objective='reg:linear',
                eval_metric='rmse',nthread=8,eta=0.01,gamma=0.0468,
                max_depth=6,min_child_weight=1.7817,subsample=0.5213,
                colsample_bytree=0.4603)




pred1 <- predict(mod1, newdata = as.matrix(test$Prev_Half_Pay) )

actual1 <- test$Prev_Half_Pay
rmse(actual1, pred1)
evaluate <- (data.frame(pred1, actual1,  tf = pred1 <= actual1+500 &  pred1 >= actual1-500))
table(evaluate$tf)


out <- test
out$Pred <- (pred1)
  
View(out) 

plot(out$Prev_Half_Pay, out$Pred)

write_csv(out,  "E:/Accenture/2018_Fulltime_out.csv")

pred1<- ceiling(max(pred1)/500)*500
evaluate <- (data.frame(pred1, actual1,  half = test$Half ,tf = pred1 <= actual1+500 &  pred1 >= actual1-500))
table(evaluate$tf)





  