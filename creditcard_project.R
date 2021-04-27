library(readr)
library(tree)
library(caret)
library(pROC) 
library(rpart)
library(Metrics)
creditcard <- read_csv("creditcard.csv")

str(creditcard)
summary(creditcard$Class)

creditcard$Class <- as.factor(creditcard$Class)
class(creditcard$Class)

## Prediction of the Class

attach(creditcard)
set.seed(22)
train_index <- sample(284807,284807*0.8)
train <- creditcard[train_index,]
test <- creditcard[-train_index,]
y <- Class[-train_index]

# 1. Decision Trees (Classification)

tree_class <- rpart(Class~.,creditcard,subset=train_index,method = 'class')
summary(tree_class)
plot(tree_class)
text(tree_class,pretty=0,cex=0.5) 

predictions <- predict(tree_class,test,type = 'class')

xtab <- table(y,predictions)
xtab

conmat <- confusionMatrix(xtab,positive = "1",)
conmat

# Sensitivity : 0.8947
# The model predicts correctly almost 87% of creditcard's cases

predictionsWithProb <- predict(tree_class,test,type = 'prob')
auc <- auc(test$Class,predictionsWithProb[,2])
auc
par(pty = 's')
roc.info <- roc(test$Class,predictionsWithProb[,2],plot = T, col="#377eb8",lwd=4,print.auc = T) 
str(roc)

# When AUC is 0.936, it means there is a 93% chance that
# the model will be able to distinguish between positive 
# class and negative class.



