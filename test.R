library(C50)
data(churn)
train<-churnTrain
test<-churnTest
str(train)
train<-train[,-3]
test<-test[,-3]
train$churn<-factor(train$churn,levels = c("no","yes"),order=TRUE)
test$churn <- factor(test$churn, ,levels = c('no','yes'), order = TRUE)
model<-glm(churn~.,data=train,family = "binomial")