
fraud<-read.csv("F:/Excelr/Assignments/dataset/decision tree/Fraud_check.csv")
library(C50)
str(fraud)
sum(is.na(fraud))
summary(fraud)


Risky_Good = ifelse(fraud$Taxable.Income<= 30000, "Risky", "Good")

fraud_check<- data.frame(fraud,Risky_Good)

str(fraud_check)
summary(fraud_check)



#exploratory data analysis

mean(fraud_check$Taxable.Income)
mean(fraud_check$City.Population)
mean(fraud_check$Work.Experience)

median(fraud_check$Taxable.Income)
median(fraud_check$City.Population)
median(fraud_check$Work.Experience)

getmode<-function(x){
  uniquv<-unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}

getmode(fraud_check$Taxable.Income)
getmode(fraud_check$City.Population)
getmode(fraud_check$Work.Experience)

var(fraud_check$Taxable.Income)
var(fraud_check$City.Population)
var(fraud_check$Work.Experience)

sd(fraud_check$Taxable.Income)
sd(fraud_check$City.Population)
sd(fraud_check$Work.Experience)

library(moments)

skewness(fraud_check$Taxable.Income)
skewness(fraud_check$City.Population)
skewness(fraud_check$Work.Experience)


hist(fraud_check$Taxable.Income)
hist(fraud_check$City.Population)
hist(fraud_check$Work.Experience)

kurtosis(fraud_check$Taxable.Income)
kurtosis(fraud_check$City.Population)
kurtosis(fraud_check$Work.Experience)

boxplot(fraud_check$Taxable.Income)
boxplot(fraud_check$City.Population)
boxplot(fraud_check$Work.Experience)

barplot(fraud_check$Taxable.Income)
barplot(fraud_check$City.Population)
barplot(fraud_check$Work.Experience)


library(ggplot2)


fraud_check_train<-fraud_check[1:300,]
fraud_check_test<-fraud_check[301:600,]
summary(fraud_check_train)

#building model on training data

fraud_check_C5.0train<-C5.0(fraud_check_train[,-7],fraud_check_train$Risky_Good)
plot(fraud_check_C5.0train)#tree graph

#training accuracy
pred_train<-predict(fraud_check_C5.0train,fraud_check_train)

mean(fraud_check_train$Risky_Good==pred_train)


library(caret)
confusionMatrix(pred_train,fraud_check_train$Risky_Good)

predc5.0_test <-predict(fraud_check_C5.0train,newdata=fraud_check_test)
table(predc5.0_test,fraud_check_test$Risky_Good)


mean(predc5.0_test==fraud_check_train$Risky_Good)


recall(fraud_check_test$Risky_Good,predc5.0_test)

precision(fraud_check_test$Risky_Good,predc5.0_test)

#################using tree########

library(tree)
fraud_check_tree<-tree(Risky_Good~.,data=fraud_check_train)
plot(fraud_check_tree)
text(fraud_check_tree,pretty=0)

#predicting the test data
pred_tree<-as.data.frame(predict(fraud_check_tree,newdata = fraud_check_test))
pred_tree["final"]<-NULL
fraud_check_tree_test<-predict(fraud_check_tree,newdata = fraud_check_test)


pred_tree$final <- colnames(fraud_check_tree_test)[apply(fraud_check_tree_test,1,which.max)]


mean(pred_tree$final==fraud_check_test$Risky_Good) # Accuracy = 94.66%
table(fraud_check_test$Risky_Good,pred_tree$final)


library(rpart)
model_1<-rpart(Risky_Good~.,data =fraud_check,method = "class" )
predict(model_1, type = "class")  # factor
rpart.plot(model_1)
summary(model_1)








