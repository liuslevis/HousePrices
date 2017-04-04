library('randomForest')

train <- read.csv('data/input/train.csv')
test  <- read.csv('data/input/test.csv')

summary(train)
summary(test)

formula <- SalePrice ~ MSSubClass + MSZoning + LotArea# Alley#LotFrontage + LotArea + Street + Alley + LotShape
randomForest(formula, data=train, na.action=na.fail)