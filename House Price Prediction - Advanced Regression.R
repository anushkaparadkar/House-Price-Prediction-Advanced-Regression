
# Importing training and testing data into R

#Train = read.csv("train.csv")Test = read.csv("test.csv")

# Identitying the important independent variables for linear regression

#newcol = c(4,5,6,9,11,13,15,18,19,23,27,28,31,33,35,37,38,44,45,54, 81)
#Train2 = Train[,newcol]
#Train2$SalePrice = log(Train2$SalePrice)
#newcol1 = c(4,5,6,9,11,13,15,18,19,23,27,28,31,33,35,37,38,44,45,54)
#Test2 = Test[,newcol1]

# Using mice function to impute data to fill NA's

#library(mice)

#temptrain = mice(Train2, seed = 123)
#temptest = mice(Test2 , seed = 123)

#Trainnew = complete(temptrain)

#Testnew = complete(temptest)

#Testnew$SalePrice <- 0
#Getting data
df <- read.csv("train.csv")
test_df <- read.csv("test.csv") 



test_df$SalePrice <- 0
final_df <-rbind(df,test_df)
#EDA
head(df)
library(ggplot2)
ggplot(df,aes(SalePrice)) + geom_histogram(bins = 20 , alpha = 0.5 ,fill ="blue")

#Combining final_dfs
test_df$SalePrice <- 0 # Adding Sales 
final_df <- rbind(df, test_df)

#sum of null values in each column
nullval <-lapply(Trainnew,function(x) { length(which(is.na(x)))}) 

#list of columns containing null values
list_na <- (final_df)[ apply(final_df, 2, anyNA) ] 
list_na

#replacing null values with mean/mode (Numerical data with null values <0 ; Replaced with 0)
final_df$LotFrontage <- ifelse(is.na(final_df$LotFrontage), mean(final_df$LotFrontage, na.rm=TRUE), final_df$LotFrontage) 
final_df$BsmtFinSF1[is.na(final_df$BsmtFinSF1)] <- 0
final_df$BsmtFinSF2[is.na(final_df$BsmtFinSF2)] <- 0
final_df$BsmtUnfSF[is.na(final_df$BsmtUnfSF)] <- 0
final_df$TotalBsmtSF[is.na(final_df$TotalBsmtSF)] <- 0
final_df$BsmtFullBath[is.na(final_df$BsmtFullBath)] <- 0
final_df$BsmtHalfBath[is.na(final_df$BsmtHalfBath)] <- 0
final_df$GarageCars[is.na(final_df$GarageCars)] <- 0
final_df$GarageArea[is.na(final_df$GarageArea)] <- 0
final_df$MasVnrArea[is.na(final_df$MasVnrArea)] <- 0

#Categorical Data
final_df$KitchenQual[is.na(final_df$KitchenQual)] <- names(sort(-table(final_df$KitchenQual)))[1]
final_df$MSZoning[is.na(final_df$MSZoning)] <- names(sort(-table(final_df$MSZoning)))[1]
final_df$SaleType[is.na(final_df$SaleType)] <- names(sort(-table(final_df$SaleType)))[1]
final_df$Exterior1st[is.na(final_df$Exterior1st)] <- names(sort(-table(final_df$Exterior1st)))[1]
final_df$Exterior2nd[is.na(final_df$Exterior2nd)] <- names(sort(-table(final_df$Exterior2nd)))[1]
final_df$Functional[is.na(final_df$Functional)] <- names(sort(-table(final_df$Functional)))[1]
# Bsmt : NA for basement features is "no basement"
final_df$BsmtQual = factor(final_df$BsmtQual, levels=c(levels(final_df$BsmtQual), "No"))
final_df$BsmtQual[is.na(final_df$BsmtQual)] = "No"
final_df$BsmtCond = factor(final_df$BsmtCond, levels=c(levels(final_df$BsmtCond), "No"))
final_df$BsmtCond[is.na(final_df$BsmtCond)] = "No"
final_df$BsmtExposure[is.na(final_df$BsmtExposure)] = "No"
final_df$BsmtFinType1 = factor(final_df$BsmtFinType1, levels=c(levels(final_df$BsmtFinType1), "No"))
final_df$BsmtFinType1[is.na(final_df$BsmtFinType1)] = "No"
final_df$BsmtFinType2 = factor(final_df$BsmtFinType2, levels=c(levels(final_df$BsmtFinType2), "No"))
final_df$BsmtFinType2[is.na(final_df$BsmtFinType2)] = "No"
# Garage : NA for garage features is "no garage"
final_df$GarageType = factor(final_df$GarageType, levels=c(levels(final_df$GarageType), "No"))
final_df$GarageType[is.na(final_df$GarageType)] = "No"
final_df$GarageFinish = factor(final_df$GarageFinish, levels=c(levels(final_df$GarageFinish), "No"))
final_df$GarageFinish[is.na(final_df$GarageFinish)] = "No"
final_df$GarageQual = factor(final_df$GarageQual, levels=c(levels(final_df$GarageQual), "No"))
final_df$GarageQual[is.na(final_df$GarageQual)] = "No"
final_df$GarageCond = factor(final_df$GarageCond, levels=c(levels(final_df$GarageCond), "No"))
final_df$GarageCond[is.na(final_df$GarageCond)] = "No"
# MasVnrType : NA most likely means no veneer
final_df$MasVnrType = factor(final_df$MasVnrType, levels=c(levels(final_df$MasVnrType), "No"))
final_df$MasVnrType[is.na(final_df$MasVnrType)] = "No"
# Electrical : NA means "UNK"
final_df$Electrical = factor(final_df$Electrical, levels=c(levels(final_df$Electrical), "UNK"))
final_df$Electrical[is.na(final_df$Electrical)] = "UNK"
# GarageYrBlt: It seems reasonable that most houses would build a garage when the house itself was built.
idx <- which(is.na(final_df$GarageYrBlt))
final_df[idx, 'GarageYrBlt'] <- final_df[idx, 'YearBuilt']

##Dropping columns with very large no of null values
final_df$Alley <- NULL
final_df$FireplaceQu<- NULL
final_df$PoolQC <-NULL
final_df$Fence <- NULL
final_df$MiscFeature <- NULL
final_df$Utilities <- NULL

str(final_df)
#Factorizing
final_df$MSZoning<-  (factor(final_df$MSZoning))
final_df$Street <-  (factor(final_df$Street))
final_df$LotShape <- (factor(final_df$LotShape ))
final_df$LandContour<- (factor(final_df$LandContour))
final_df$LotConfig<- (factor(final_df$LotConfig))
final_df$LandSlope<- (factor(final_df$LandSlope))
final_df$Neighborhood<- (factor(final_df$Neighborhood))
final_df$Condition1<- (factor(final_df$Condition1))
final_df$Condition2<- (factor(final_df$Condition2))
final_df$BldgType<- (factor(final_df$BldgType))
final_df$HouseStyle<- (factor(final_df$HouseStyle))
final_df$RoofStyle<- (factor(final_df$RoofStyle))
final_df$RoofMatl<- (factor(final_df$RoofMatl))
final_df$Exterior1st<- (factor(final_df$Exterior1st))
final_df$Exterior2nd<- (factor(final_df$Exterior2nd))
final_df$ExterQual<- (factor(final_df$ExterQual))
final_df$ExterCond<- (factor(final_df$ExterCond))
final_df$Foundation<- (factor(final_df$Foundation))
final_df$Heating<- (factor(final_df$Heating))
final_df$HeatingQC<- (factor(final_df$HeatingQC))
final_df$CentralAir<- (factor(final_df$CentralAir))
final_df$KitchenQual<- (factor(final_df$KitchenQual))
final_df$Functional<- (factor(final_df$Functional))
final_df$PavedDrive<- (factor(final_df$PavedDrive))
final_df$SaleType<- (factor(final_df$SaleType))
final_df$SaleCondition<- (factor(final_df$SaleCondition))
str(final_df)
final_df
#Split into train and test
final_df<- as.data.frame(final_df)
library(caTools)
set.seed(12345)
sample <- sample.split(final_df$SalePrice,SplitRatio = 0.7)
train <- subset(final_df,sample == TRUE)
test<-subset(final_df,sample == FALSE)

train$SalePrice <- ifelse(is.na(train$SalePrice), mean(train$SalePrice, na.rm=TRUE), train$SalePrice) 
#Building the Model
library(xgboost)
library(glmnet)
set.seed(123)
fit<-model.matrix( ~.-1, train)
as.data.frame(fit)
lasso <- cv.glmnet(x = data.matrix(train[, - which(names(train) %in% c('SalePrice'))]), y =train$SalePrice, nfolds = 10)
plot(lasso)
#Predict model
lasso_pred <-  (exp(predict(lasso, newx = data.matrix(Testnew[, - which(names(Testnew) %in% c('SalePrice'))]), s = "lambda.min"))-1)

results <- cbind(lasso_pred,test$SalePrice)
colnames(results) <- cbind('predicted','actual')
results <- as.data.frame(results)
head(results)





# Evaluation RMSE function

RMSE <- function(x,y){
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}
RMSE1 <- RMSE(lasso_pred, test$SalePrice)
RMSE1
RMSE1 <- round(RMSE1, digits = 5)

lasso_pred[which(is.na(lasso_pred))] <- mean(lasso_pred,na.rm=T)
submit <- rbind(Id=test$Id,SalePrice=house_model)
write.csv(submit,file="House_Price.csv",row.names=F)

