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

## 1. Decision Trees (Classification)

tree_class <- rpart(Class~.,creditcard,subset=train_index,method = 'class')
summary(tree_class)
plot(tree_class)
text(tree_class,pretty=0,cex=0.5) 


cv.class <- cv.tree(tree_class,FUN=prune.misclass)
cv.class

plot(cv.class$size ,cv.class$dev ,type="b",
     ylab = "cross-validation error rate", xlab = "size")
plot(cv.class$k ,cv.class$dev ,type="b",
     ylab = "cost-complexity parameter k", xlab = "size")

# The tree doesn't need pruning !

predictions <- predict(tree_class,test,type = 'class')

xtab <- table(y,predictions)
xtab

conmat <- confusionMatrix(xtab,positive = "1",)
conmat

# Sensitivity : 0.8947
# The model predicts correctly almost 87% of creditcard's cases

model.predict <- predict(log.fit,newdata = test, probability = TRUE)
predictionsWithProb <- predict(tree_class,test,type = 'prob')
auc <- auc(test$Class,predictionsWithProb[,2])
auc
par(pty = 's')
roc.info <- roc(test$Class,predictionsWithProb[,2],plot = T, col="#377eb8",lwd=4,print.auc = T) 
str(roc)

# When AUC is 0.936, it means there is a 93% chance that
# the model will be able to distinguish between positive 
# class and negative class.

## 2. Logistic Regression

log.fit <- glm(Class~.,family = 'binomial',data = train)
summary(log.fit)


model.predict <- predict(log.fit,newdata = test, probability = TRUE)

model.predict <- ifelse(model.predict > 0.5,1,0)
misClasificError <- mean(model.predict != test$Class)
print(paste('Accuracy',1-misClasificError))

# In this case accuracy is not a good metric !

par(pty = 's')
plot(roc(test$Class,model.predict),col="#377eb8",lwd = 4,print.auc = T)

# To compare the two models we created, we will use AUC-ROC
plot(roc(test$Class,predictionsWithProb[,2]),add = T,col = '#4daf4a',lwd = 4,print.auc = T)
legend("bottomright", legend=c("Logisitic Regression", "Decision Trees"), col=c("#377eb8", "#4daf4a"),lwd=4)

# So Logistic Regression model seems to have better results
# since auc-roc is greater than the decision tree model's one.
