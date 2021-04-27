#install.packages('tree')
#install.packages('tidyverse')
library(tree)
library(tidyverse)

test$Survived <- NA
data <- rbind(train,test)
View(data)

## Data Cleaning

sum(is.na(data))
sum(is.na(data[,"Embarked"]))
sum(is.na(data[,"Cabin"]))
sum(is.na(data[,"Age"]))
sum(is.na(data[,"Fare"]))

# Embarked
table(data$Embarked)
data$Embarked[which(is.na(data$Embarked))] <- 'S'

# Age
median(data$Age,na.rm = T)
data$Age[which(is.na(data$Age))] <- 28

# Fare
median(data$Fare,na.rm = T)
data$Fare[which(is.na(data$Fare))] <- 14.4542

# Categorical Variables
data$Survived<-as.factor(data$Survived)
data$Pclass<-as.factor(data$Pclass)
data$Sex<-as.factor(data$Sex)
data$Embarked<-as.factor(data$Embarked)

train <- data[1:891,]
test <- data[892:1309,]
test <- test[,-2]

str(train)

# For the predictive model use : Survival~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked
attach(train)

titanic_tree<-tree(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data = train)
summary(titanic_tree)

plot(titanic_tree)
text(titanic_tree,pretty = 0,cex=0.9)

titanic_predict<-predict(titanic_tree,test,type = 'class')
cv_titanic<-cv.tree(titanic_tree,FUN = prune.misclass)
summary(cv_titanic)

plot(cv_titanic$size ,cv_titanic$dev ,type="b",
     ylab = "cross-validation error rate", xlab = "size")
plot(cv_titanic$k ,cv_titanic$dev ,type="b",
     ylab = "cost-complexity parameter k", xlab = "size")

cvtable<-cbind(cv_titanic$size,cv_titanic$dev)
colnames(cvtable)<-c('size','deviance')
cvtable
min(cvtable[,2])

prune_titanic<-prune.misclass(titanic_tree,best = 8)
plot(prune_titanic)
text(prune_titanic,pretty=0)
titanic_predict2 <- predict(prune_titanic,test,type = 'class')

results <- data.frame(test$PassengerId,titanic_predict2)
View(results)
