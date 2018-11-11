
library(dplyr)
library(readr)
library(ggplot2)
  
  ## Load data
half_data <- read_csv("E:/Accenture/half_yearly_data.csv")
New_half <-  half_data[half_data$Prev_Half_avg_Pay != 0 & half_data$H_avg_pay != 0,-c(2,6)]  




#outlier analysis
#----------------------
#for Prev_Half_avg_Pay
iqr <- IQR(New_half$Prev_Half_avg_Pay)
quartiles <- quantile(New_half$Prev_Half_avg_Pay)   

lb <- quartiles[2] - 2* iqr
ub <- quartiles[4] + 2* iqr

New_half <- New_half[New_half$Prev_Half_avg_Pay >= lb & New_half$Prev_Half_avg_Pay <= ub,]


#outlier analysis
#----------------------
#for Prev_Half_avg_Pay
iqr <- IQR(New_half$H_avg_pay)
quartiles <- quantile(New_half$H_avg_pay)   

lb <- quartiles[2] - 2* iqr
ub <- quartiles[4] + 2* iqr

New_half <- New_half[New_half$H_avg_pay >= lb & New_half$H_avg_pay <= ub,]

New_half_u <- New_half[ New_half$Prev_Half_avg_Pay <= New_half$H_avg_pay +30000 ,]
New_half_u <- New_half_u[ New_half_u$Prev_Half_avg_Pay >= New_half_u$H_avg_pay - 40000 ,]


plot(New_half_u$H_avg_pay[New_half_u$Year == '2017' | New_half_u$Year == '2016'] , New_half_u$Prev_Half_avg_Pay[New_half_u$Year == '2017' | New_half_u$Year == '2016'] )

--------------------------------------------------------------
#range of prev for actual prediction
range(New_half$H_avg_pay[New_half$Year == '2017' & New_half$Half == 2])
    
train <- New_half_u[New_half_u$Year != '2017',]
test <- New_half_u[New_half_u$Year == '2017',]

#XG boost
mod1 <- xgboost(data = as.matrix(train$Prev_Half_avg_Pay), label = as.matrix(train$H_avg_pay),
                nrounds=2200,verbose=FALSE,objective='reg:linear',
                eval_metric='rmse',nthread=8,eta=0.01,gamma=0.0468,
                max_depth=6,min_child_weight=1.7817,subsample=0.5213,
                colsample_bytree=0.4603)

summary(mod1)

pred1 <- predict(mod1, newdata = as.matrix(test$Prev_Half_avg_Pay))
#install.packages("Metrics")
library(Metrics)
actual1 <- test$H_avg_pay
rmse(actual1, pred1)

evaluate <- (data.frame(pred1, actual1,  tf = pred1 <= actual1+500 &  pred1 >= actual1-500))
table(evaluate$tf)
100*22161/(22161+69116)



#-----------------------------------------------------------------------------------

#XG boost
mod3 <- xgboost(data = as.matrix(train$Prev_Half_avg_Pay), nfold = 5 , label = as.matrix(train$H_avg_pay),
                nrounds=2200,verbose=FALSE,objective='reg:linear',
                eval_metric='rmse',nthread=8,eta=0.01,gamma=0.0468,
                max_depth=6,min_child_weight=1.7817,subsample=0.5213,
                colsample_bytree=0.4603)

summary(mod3)

pred3 <- predict(mod3, newdata = as.matrix(test$Prev_Half_avg_Pay))

actual3 <- test$H_avg_pay
rmse(actual3, pred3)

evaluate <- (data.frame(pred3, actual3,  tf = pred3 <= actual3+500 &  pred3 >= actual3-500))
table(evaluate$tf)
100*22161/(22161+69116)



#-----------------------------------------------------------------------------------

#install.packages("glmnet")

#lasoo
#View(train)
library(glmnet)
mod2 <- cv.glmnet(as.matrix(train[,c(6,7)]), as.matrix(train$H_avg_pay))


?glmnet

summary(mod2)

pred2 <- predict(mod2, newx = as.matrix(test[,c(6,7)]), s = "lambda.min")

actual2 <- test$H_avg_pay
rmse(actual2, pred2)

#plot(pred2, actual2)
evaluate <- data.frame(pred2, actual2,  tf = pred2 <= actual2+500 &  pred2 >= actual2-500)
table(pred2 <= actual2+500 &  pred2 >= actual2-500)
100*18132/(18132+73145)
#19.86481




#-----------------------------------------------------------------------------------


## Data pre-processing

```{r}
## Save the ID column so that we can drop it from merged dataset (combi)
train_ID=train$Id
test_ID=test$Id

## test doesn't have SalePrice column, so add it.
test$SalePrice=NA
```

**Removing outliers** - A scatterplot between SalePrice and GrLivArea shows a couple of outliers. Let us get rid of them.

```{r}
library(ggplot2)
qplot(train$GrLivArea,train$SalePrice,main="With Outliers")
train<-train[-which(train$GrLivArea>4000 & train$SalePrice<300000),]

## Check again after removal.
qplot(train$GrLivArea,train$SalePrice,main="Without Outliers")
```

**Log Transformation of SalePrice Variable** - In order to make the distribution of the target variable normal, we need to transform it by taking log.

```{r}
## Plot histogram of SalePrice Variable - Right skewed
qplot(SalePrice,data=train,bins=50,main="Right skewed distribution")

## Log transformation of the target variable
train$SalePrice <- log(train$SalePrice + 1)

## Normal distribution after transformation
qplot(SalePrice,data=train,bins=50,main="Normal distribution after log transformation")
```

**Combine train and test datasets**.

```{r}
## Combine train and test
combi=rbind(train,test)

## Dropping Id as it is unnecessary for the prediction process.
combi=combi[,-1]
```

# Data Processing and Analysis

## Checking Missing data

Let us check the number of rows of data missing for each variable out of 2917 rows.

```{r}
colSums(is.na(combi))
```

Clearly, there are a lot of missing values. PoolQC, MiscFeature, Alley and Fence have 90% of the data as NA. 

## Imputing Missing data

We will be handling each variable separately. 

1. For most of the **categorical features**, NA values will be imputed as **'None'**, because referring to the **data_description.txt** file, **the NA of these variables represent values such as 'No Garage','No Basement', etc.**
  
  2. For most of the **numerical features**, NA values will be replaced by 0, for variables like GarageArea, GarageCars, etc.

3. For some categorical features like Functional and Electrical, the NA values will be replaced by the most frequently occuring value for that variable.

```{r}
## For some variables, fill NA with "None" 
for(x in c("Alley","PoolQC","MiscFeature","Fence","FireplaceQu","GarageType","GarageFinish","GarageQual",'GarageCond','BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2',"MasVnrType")){
  combi[is.na(combi[,x]),x]="None"
}

#Group by neighborhood and fill in missing value by the median LotFrontage of all the neighborhood
temp=aggregate(LotFrontage~Neighborhood,data=combi,median)
temp2=c()
for(str in combi$Neighborhood[is.na(combi$LotFrontage)]){temp2=c(temp2,which(temp$Neighborhood==str))}
combi$LotFrontage[is.na(combi$LotFrontage)]=temp[temp2,2]

## Replacing missing data with 0
for(col in c('GarageYrBlt', 'GarageArea', 'GarageCars','BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF','TotalBsmtSF', 'BsmtFullBath', 'BsmtHalfBath',"MasVnrArea")){
  combi[is.na(combi[,col]),col]=0
}

## Replace missing MSZoning values by "RL"
combi$MSZoning[is.na(combi$MSZoning)]="RL"

## Remove Utilities as it has zero variance
combi=combi[,-9]

## Replace missing Functional values with "Typ"
combi$Functional[is.na(combi$Functional)]="Typ"

## Replace missing Electrical values with "SBrkr"
combi$Electrical[is.na(combi$Electrical)]="SBrkr"

## Replace missing KitchenQual values by "TA"
combi$KitchenQual[is.na(combi$KitchenQual)]="TA"

## Replace missing SaleType values by "WD"
combi$SaleType[is.na(combi$SaleType)]="WD"

## Replace missing Exterior1st and Exterior2nd values by "VinylSd"
combi$Exterior1st[is.na(combi$Exterior1st)]="VinylSd"
combi$Exterior2nd[is.na(combi$Exterior2nd)]="VinylSd"

## All NAs should be gone, except the test portion of SalePrice variable, which we ourselves had initialized to NA earlier.
colSums(is.na(combi))
```

## Transforming some numerical variables that are really categorical

```{r}
combi$MSSubClass=as.character(combi$MSSubClass)
combi$OverallCond=as.character(combi$OverallCond)
combi$YrSold=as.character(combi$YrSold)
combi$MoSold=as.character(combi$MoSold)
```

## Label Encoding some categorical variables that may contain information in their ordering set

**We will also specify the order of the levels (mapping), while label encoding (converting categories to integer ranks - 1 to n) the categorical variables.**
  
  ```{r}
cols = c('FireplaceQu', 'BsmtQual', 'BsmtCond', 'GarageQual', 'GarageCond', 'ExterQual', 'ExterCond','HeatingQC', 'PoolQC', 'KitchenQual', 'BsmtFinType1', 'BsmtFinType2', 'Functional', 'Fence', 'BsmtExposure', 'GarageFinish', 'LandSlope','LotShape', 'PavedDrive', 'Street', 'Alley', 'CentralAir', 'MSSubClass', 'OverallCond', 'YrSold', 'MoSold')

FireplaceQu=c('None','Po','Fa','TA','Gd','Ex')
BsmtQual=c('None','Po','Fa','TA','Gd','Ex')
BsmtCond=c('None','Po','Fa','TA','Gd','Ex')
GarageQual=c('None','Po','Fa','TA','Gd','Ex')
GarageCond=c('None','Po','Fa','TA','Gd','Ex')
ExterQual=c('Po','Fa','TA','Gd','Ex')
ExterCond=c('Po','Fa','TA','Gd','Ex')
HeatingQC=c('Po','Fa','TA','Gd','Ex')
PoolQC=c('None','Fa','TA','Gd','Ex')
KitchenQual=c('Po','Fa','TA','Gd','Ex')
BsmtFinType1=c('None','Unf','LwQ','Rec','BLQ','ALQ','GLQ')
BsmtFinType2=c('None','Unf','LwQ','Rec','BLQ','ALQ','GLQ')
Functional=c('Sal','Sev','Maj2','Maj1','Mod','Min2','Min1','Typ')
Fence=c('None','MnWw','GdWo','MnPrv','GdPrv')
BsmtExposure=c('None','No','Mn','Av','Gd')
GarageFinish=c('None','Unf','RFn','Fin')
LandSlope=c('Sev','Mod','Gtl')
LotShape=c('IR3','IR2','IR1','Reg')
PavedDrive=c('N','P','Y')
Street=c('Pave','Grvl')
Alley=c('None','Pave','Grvl')
MSSubClass=c('20','30','40','45','50','60','70','75','80','85','90','120','150','160','180','190')
OverallCond=NA
MoSold=NA
YrSold=NA
CentralAir=NA
levels=list(FireplaceQu, BsmtQual, BsmtCond, GarageQual, GarageCond, ExterQual, ExterCond,HeatingQC, PoolQC, KitchenQual, BsmtFinType1, BsmtFinType2, Functional, Fence, BsmtExposure, GarageFinish, LandSlope,LotShape, PavedDrive, Street, Alley, CentralAir, MSSubClass, OverallCond, YrSold, MoSold)
i=1
for (c in cols){
  if(c=='CentralAir'|c=='OverallCond'|c=='YrSold'|c=='MoSold'){
    combi[,c]=as.numeric(factor(combi[,c]))}
  else
    combi[,c]=as.numeric(factor(combi[,c],levels=levels[[i]]))
  i=i+1
}
```

## Adding an important feature - Total area of basement

```{r}
combi$TotalSF=combi$TotalBsmtSF+combi$X1stFlrSF+combi$X2ndFlrSF
```

## Getting dummy categorical features

```{r}
# first get data type for each feature
feature_classes <- sapply(names(combi),function(x){class(combi[[x]])})
numeric_feats <-names(feature_classes[feature_classes != "character"])

# get names of categorical features
categorical_feats <- names(feature_classes[feature_classes == "character"])

# use caret dummyVars function for hot one encoding for categorical features
library(caret)
dummies <- dummyVars(~.,combi[categorical_feats])
categorical_1_hot <- predict(dummies,combi[categorical_feats])
```

## Fixing Skewed features

**We will transform the skewed features with BoxCox Transformation.**
  
  ```{r}
## Determine skew for each numeric feature
library(moments)
library(MASS)
skewed_feats <- sapply(numeric_feats,function(x){skewness(combi[[x]],na.rm=TRUE)})

## Keep only features that exceed a threshold (0.75) for skewness
skewed_feats <- skewed_feats[abs(skewed_feats) > 0.75]

## Transform skewed features with boxcox transformation
for(x in names(skewed_feats)) {
  bc=BoxCoxTrans(combi[[x]],lambda = .15)
  combi[[x]]=predict(bc,combi[[x]])
  #combi[[x]] <- log(combi[[x]] + 1)
}
```

## Reconstruct all data with pre-processed data.

```{r}
combi <- cbind(combi[numeric_feats],categorical_1_hot)

## Let us look at the dimensions of combi.
dim(combi)
```

# Model building and evaluation

## Splitting train dataset further into Training and Validation in order to evaluate the models

```{r}
training<-combi[1:1458,]
testing<-combi[1459:2917,]
set.seed(222)
inTrain<-createDataPartition(y=training$SalePrice,p=.7,list=FALSE)
Training<-training[inTrain,]
Validation<-training[-inTrain,]
```

## Models

## Lasso - Regularized Regression

**Build model, predict SalePrice for Validation set and evaluate the RMSE score.**
  
  ```{r}
library(glmnet)
library(Metrics)
set.seed(123)
cv_lasso=cv.glmnet(as.matrix(Training[,-59]),Training[,59])

## Predictions
preds<-predict(cv_lasso,newx=as.matrix(Validation[,-59]),s="lambda.min")
rmse(Validation$SalePrice,preds)
```
## GBM

**Build model, predict SalePrice for Validation set and evaluate the RMSE score.**
  
  ```{r}
library(iterators)
library(parallel)
library(doMC)
set.seed(222)
## detectCores() returns 16 cpus
registerDoMC(16)
## Set up caret model training parameters
CARET.TRAIN.CTRL <-trainControl(method="repeatedcv",number=5,repeats=5,verboseIter=FALSE,allowParallel=TRUE)
gbmFit<-train(SalePrice~.,method="gbm",metric="RMSE",maximize=FALSE,trControl=CARET.TRAIN.CTRL,tuneGrid=expand.grid(n.trees=(4:10)*50,interaction.depth=c(5),shrinkage=c(0.05),n.minobsinnode=c(10)),data=Training,verbose=FALSE)

##print(gbmFit)

## Predictions
preds1 <- predict(gbmFit,newdata=Validation)
rmse(Validation$SalePrice,preds1)
```

## XGBOOST

**Build model, predict SalePrice for Validation set and evaluate the RMSE score.**
  
  ```{r}
library(xgboost)
set.seed(123)
## Model parameters trained using xgb.cv function
xgbFit=xgboost(data=as.matrix(Training[,-59]),nfold=5,label=as.matrix(Training$SalePrice),nrounds=2200,verbose=FALSE,objective='reg:linear',eval_metric='rmse',nthread=8,eta=0.01,gamma=0.0468,max_depth=6,min_child_weight=1.7817,subsample=0.5213,colsample_bytree=0.4603)
##print(xgbFit)

## Predictions
preds2 <- predict(xgbFit,newdata=as.matrix(Validation[,-59]))
rmse(Validation$SalePrice,preds2)
```
## RMSE score for Simple Average of the three models

```{r}
rmse(Validation$SalePrice,(preds+preds1+preds2)/3)
```

## RMSE score for Weighted Average of the three models

```{r}
rmse(Validation$SalePrice,(0.6*preds+0.1*preds1+0.3*preds2))
```

**So, the wighted average of the models scores better than simple average. Let us retrain the models on the whole training dataset and submit the weighted average solution.** 
  
  ## Retraining on whole training set and Final Submission
  
  ## Models
  
  ## Lasso - Regularized Regression
  
  ```{r}
set.seed(123)
cv_lasso=cv.glmnet(as.matrix(training[,-59]),training[,59])

## Predictions
preds=data.frame(exp(predict(cv_lasso,newx=as.matrix(testing[,-59]),s="lambda.min"))-1)
```
## GBM

```{r}
set.seed(222)
registerDoMC(16)
gbmFit<-train(SalePrice~.,method="gbm",metric="RMSE",maximize=FALSE,trControl=CARET.TRAIN.CTRL,tuneGrid=expand.grid(n.trees=(2:10)*50,interaction.depth=c(3:5),shrinkage=c(0.05),n.minobsinnode=c(10)),data=training,verbose=FALSE)

##print(gbmFit)

## Predictions
preds1 <- exp(predict(gbmFit,newdata=testing)) - 1
```

## XGBOOST

```{r}
## Model parameters tuned using xgb.cv function
set.seed(123)
xgbFit=xgboost(data=as.matrix(training[,-59]),nfold=5,label=as.matrix(training$SalePrice),nrounds=2200,verbose=FALSE,objective='reg:linear',eval_metric='rmse',nthread=8,eta=0.01,gamma=0.0468,max_depth=6,min_child_weight=1.7817,subsample=0.5213,colsample_bytree=0.4603)
##print(xgbFit)

## Predictions
preds2 <- exp(predict(xgbFit,newdata=as.matrix(testing[,-59]))) - 1
```

## Weighted Average of Lasso + GBM + XGBOOST and Final Submission

```{r}
df <- data.frame(Id=test_ID,SalePrice=0.6*preds$X1+0.16*preds1+.24*preds2)
write.csv(df,"submission.csv",row.names=FALSE)
```

This submission scores **0.12039 (Top 20%)** on the Leaderboard. 

**Thank you, everyone! Comments and suggestions for improvement are welcome!** 
  
  **Please upvote if you found it useful. Thanks!**
  