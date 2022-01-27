library(readr)
library(stats)
require(ggplot2)
library(caret)
library(Metrics)
library(rpart)
train_data <- read_csv("UNI/ravesh/Project_Dataset_1400(1)/Project_Dataset_1400/train.csv")
test <- read_csv("UNI/ravesh/Project_Dataset_1400(1)/Project_Dataset_1400/test1.csv")

sum(is.na(train_data))
cato_ans <- catto_freq(train=train_data,test=test, verbose = TRUE)
train_data = cato_ans$train
test = cato_ans$test

###### MSSubClass
ggplot(data=train_data, mapping = aes(x = MSSubClass, y = SalePrice))+
  geom_point(alpha=0.2, aes(color = Id))
res <- cor(train_data$MSSubClass, train_data$SalePrice)
res


###### MSZoning
ggplot(data=train_data, mapping = aes(x = MSZoning, y = SalePrice))+
  geom_point(alpha=0.2, aes(color = Id))
res <- cor(train_data$MSZoning, train_data$SalePrice)
res

#### LotArea
ggplot(data=train_data, mapping = aes(x = LotArea, y = SalePrice))+
  geom_point(alpha=0.2, aes(color = Id))
res <- cor(train_data$LotArea, train_data$SalePrice)
res

### SaleCondition
ggplot(data=train_data, mapping = aes(x = SaleCondition, y = SalePrice))+
  geom_point(alpha=0.2, aes(color = Id))
res <- cor(train_data$SaleCondition, train_data$SalePrice)
res


train_data <-train_data[train_data$LotArea < 200000, ]
control = rpart.control(minsplit = 3, maxdepth = 30, cp=0, maxcompete = 10)
model <-rpart(SalePrice~., data=train_data, method = 'anova', control = control)

summary(model)
p = predict(model, test)
sqrt(mse(test$SalePrice, p))

write.csv(p, "UNI/ravesh/Project_Dataset_1400(1)/abhari-610398092-Project/response.csv")
