#Problem
#a Predict the no of comments in next H hrs
#b Use regression technique
#c Report the training accuracy and test accuracy


#Answers a) & b)
#reading the dataset and viewing

comment <- read.csv("D:\\acadgild\\comment.csv")
comment1<- comment
View(comment1)

#features
dim(comment1)
str(comment1)

library(psych)
describe(comment1)
summary(comment1)

#visualization

hist(comment1$Advt ,xlab = "advt", ylab = "Frequency",main="Histogram of advt",col="red")
hist(comment1$Sales ,xlab = "sales", ylab = "Frequency",main="Histogram of sales",col="blue")

plot(comment1$Advt,comment1$Sales)

#using linear regression technique, using comment1 dataset

model<- lm(comment1$Advt~comment1$Sales)
model

#Important features

#F stats
#p value of slope test
#multiple r squared value

#predicting 
Pred<- predict(lm(comment1$Sales~comment1$Advt))
Pred

pred<- predict(model,newdata= comment1Test,type = "response")
table(comment1$Advt,pred>= 0.5)

conf<- table(comment1$Advt,pred)
conf

predict(model)
Pred=predict(model)
comment1$predicted =NA
comment1$predicted =Pred

comment1$error =model$residuals

#verfify residuals
error<- residuals(lm(comment1$Sales~comment1$Advt))
error

summary(error)

#check and interpreting the summary
summary(model)


#thus by multiple r squared value it can be concluded that the model is good as p value of slope test is <0.05. 
#Also adjusted r squared value is 0.891 and f stats value of 90.93 making model accuracy 0.9009

#result of all of our models 
summary(model)
summary(model1)
summary(model2)

#where model,model1,model2 are model coefficients

comment1$coefficients<- NA
comment1$coefficients<- model$coefficients
comment1$coefficients


#Answers c)
#test and training accuracy
#dataset comment1

set.seed(1)
split<- sample.split(comment1$Advt,SplitRatio = 0.70)
comment1Train <- subset(comment1,split == TRUE)
comment1Test<- subset(comment1, split == FALSE)

#train
model1<- lm(comment1Train$Advt~comment1Train$Sales)
model1

summary(model1)
#accuracy is 0.926

#test
model2<- lm(comment1Test$Advt~comment1Test$Sales)
model2

summary(model2)
#accuracy is 0.871