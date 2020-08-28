company<-read.csv("F:/Excelr/Assignments/dataset/decision tree/Company_Data.csv")
library(C50)
View(company)
str(company)

str(company)
summary(company)
sum(is.na(company))


High <- ifelse(company$Sales <= 8, "No", "Yes")
company_1 <- data.frame(company, High)
head(company_1)
summary(company_1)
str(company_1)





#exploratory data analysis
mean(company_1$CompPrice)
mean(company_1$Income)
mean(company_1$Advertising)
mean(company_1$Population)
mean(company_1$Price)
mean(company_1$Age)
mean(company_1$Education)

median(company_1$CompPrice)
median(company_1$Income)
median(company_1$Advertising)
median(company_1$Population)
median(company_1$Price)
median(company_1$Age)
median(company_1$Education)

getmode<-function(x){
  uniquv<-unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(company_1$CompPrice)
getmode(company_1$Income)
getmode(company_1$Advertising)
getmode(company_1$Population)
getmode(company_1$Price)
getmode(company_1$Age)
getmode(company_1$Education)

var(company_1$CompPrice)
var(company_1$Income)
var(company_1$Advertising)
var(company_1$Population)
var(company_1$Price)
var(company_1$Age)
var(company_1$Education)

sd(company_1$CompPrice)
sd(company_1$Income)
sd(company_1$Advertising)
sd(company_1$Population)
sd(company_1$Price)
sd(company_1$Age)
sd(company_1$Education)

library(moments)

skewness(company_1$CompPrice)
skewness(company_1$Income)
skewness(company_1$Advertising)
skewness(company_1$Population)
skewness(company_1$Price)
skewness(company_1$Age)
skewness(company_1$Education)



kurtosis(company_1$CompPrice)
kurtosis(company_1$Income)
kurtosis(company_1$Advertising)
kurtosis(company_1$Population)
kurtosis(company_1$Price)
kurtosis(company_1$Age)
kurtosis(company_1$Education)


hist(company_1$CompPrice)
hist(company_1$Income)
hist(company_1$Advertising)
hist(company_1$Population)
hist(company_1$Price)
hist(company_1$Age)
hist(company_1$Education)

boxplot(company_1$CompPrice)
boxplot(company_1$Income)
boxplot(company_1$Advertising)
boxplot(company_1$Population)
boxplot(company_1$Price)
boxplot(company_1$Age)
boxplot(company_1$Education)

barplot(company_1$CompPrice)
barplot(company_1$Income)
barplot(company_1$Advertising)
barplot(company_1$Population)
barplot(company_1$Price)
barplot(company_1$Age)
barplot(company_1$Education)

library(ggplot2)
summary(company_1)


#to check pure corelation between variables
library(corpcor)
plot(company_1$CompPrice,company_1$Sales)
cor(company_1$CompPrice,company_1$Sales)
plot(company_1$Income,company_1$Sales)
cor(company_1$Income,company_1$Sales)
plot(company_1$Population,company_1$Sales)
cor(company_1$Population,company_1$Sales)
plot(company_1$Price,company_1$Sales)
cor(company_1$Price,company_1$Sales)
plot(company_1$Advertising,company_1$Sales)
cor(company_1$Advertising,company_1$Sales)
plot(company_1$Age,company_1$Sales)
cor(company_1$age,company_1Sales)
plot(company_1$Education,company_1$Sales)



company_1_train<-company_1[1:280,]
company_1_test<-company_1[281:400,]
summary(company_1_test)

#building model on training data


company_c5.0_train<-C5.0(company_1_train[,-12],company_1_train$High)
plot(company_c5.0_train)
#training accuracy
pred_train<-predict(company_c5.0_train,company_1_train)

mean(company_1_train$High==pred_train)
library(caret)
confusionMatrix(pred_train,company_1_train$High)

pred_test<-predict(company_c5.0_train,newdata=company_1_test)
table1<-table(pred_test,company_1_test$High)
mean(pred_test==company_1_test$High)

accuracy1<-(sum(diag(table1))/sum(table1))

##########using tree#######
library(tree)
company_1_tree<-tree(High~.,data = company_1)
plot(company_1_tree)
text(company_1_tree,pretty = 0)
class(company_1_tree)

company_1_pred<-as.data.frame(predict(company_1_tree,newdata = company_1_test))
class(company_1_pred)
company_1_pred["final"]<-NULL
company_1_pred_test<-predict(company_1_tree,newdata=company_1_test)

company_1_pred$final<-colnames(company_1_pred_test)[apply(company_1_pred_test,1,which.max)]

mean(company_1_pred$final==company_1_test$High)
table(company_1_pred$final,company_1_test$High)

library(rpart)
model_1<-rpart(High~.,data =company_1,method = "class" )
predict(model_1, type = "class") 
rpart.plot(model_1)
summary(model_1)
