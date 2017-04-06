# TODO make auto sel feature
library(randomForest)
library(plyr)


rmse <- function(data) {
    pred <- predict(rf, newdata=data)
    rmse <- sqrt(mean((pred - data$SalePrice)^2))
    cat('RMSE:', rmse, '\n')
}


train <- read.csv('data/input/train.csv')
test  <- read.csv('data/input/test.csv')

test$SalePrice <- as.integer(0)
combi <- rbind(train, test)

# fill na with most freq category
catCols <- c('Exterior1st', 'Exterior2nd', 'MasVnrType', 'Utilities', 'Alley', 'MSZoning', 'SaleType', 'GarageCond', 'GarageQual', 'GarageFinish', 'Functional', 'GarageType', 'KitchenQual', 'Electrical', 'KitchenQual', 'BsmtFinType2','BsmtFinType1')
for (col in catCols) {
    # combi$GarageCond[is.na(combi$)] <- names(sort(table(combi$GarageCond), decreasing=TRUE))[1]
    combi[[col]][is.na(combi[[col]])] <- names(sort(table(combi[[col]]), decreasing=TRUE))[1]
}


numericCols <- c('MasVnrArea', 'MSSubClass', 'LotFrontage', 'GarageArea','GarageCars','GarageYrBlt','BsmtHalfBath','BsmtFullBath','TotalBsmtSF','BsmtUnfSF','BsmtFinSF1','BsmtFinSF2','BsmtExposure')
for (col in numericCols) {
    # combi$GarageArea[is.na(combi$GarageArea)] <- mean(combi$GarageArea, na.rm=TRUE)
    combi[[col]][is.na(combi[[col]])] <- mean(combi[[col]], na.rm=TRUE)
}

summary(combi)



train <- combi[1:nrow(train), ]
test  <- combi[nrow(train)+1:nrow(test), ]

# CV
set.seed(1)
sample <- sample.int(nrow(train), floor(0.75 * nrow(train)), replace=FALSE)
valid <- train[-sample, ]
train <- train[sample, ]

summary(train)
summary(test)

# formula <- SalePrice ~ SaleType + YrSold + MoSold + PoolArea + WoodDeckSF + PavedDrive + GarageCond
# formula <- SalePrice ~ Exterior1st+ Exterior2nd+ MasVnrType+ Utilities+ Alley+ MSZoning+ SaleType+ GarageCond+ GarageQual+ GarageFinish+ Functional+ GarageType+ KitchenQual+ Electrical+ KitchenQual+ BsmtFinType2+BsmtFinType1+ MasVnrArea+ MSSubClass+ LotFrontage+ GarageArea+GarageCars+GarageYrBlt+BsmtHalfBath+BsmtFullBath+TotalBsmtSF+BsmtUnfSF+BsmtFinSF1+BsmtFinSF2+BsmtExposure
formula <- SalePrice ~ Alley + MSZoning+ SaleType+ GarageCond+ GarageQual+ GarageFinish+ Functional+ GarageType+ KitchenQual + YrSold + GarageCars + HouseStyle +OverallCond + YearBuilt + Street + Alley + MSZoning + Neighborhood + YearRemodAdd + PoolArea + GarageArea

rf <- randomForest(formula=formula, data=train, na.action=na.omit)
print(rf)


rmse(train)
rmse(valid)

# pred <- predict(rf, newdata=test)
# write.csv(pred, 'data/output/submission.csv', quote=FALSE)

