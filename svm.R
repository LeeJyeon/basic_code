setwd("C:/Users/JYEON/Desktop/��û��/�з� ���")
train<-read.csv("train.csv",header=T)
test<-read.csv("test.csv",header=T)

str(train)

train_aver<-(tapply(train$TNMS,train$���,mean))
test_aver<-tapply(test$TNMS,test$���,mean)


#�󺧸�

names(train_aver[train_aver<=13.10])
names(train_aver[train_aver<8.9])
names(train_aver[train_aver<7.1])
names(train_aver[train_aver<5.45])

train[train$���%in%names(train_aver[train_aver<=13.10]),2]<-"excellent"
train[train$���%in%names(train_aver[train_aver<8.9]),2]<-"good"
train[train$���%in%names(train_aver[train_aver<7.1]),2]<-"bad"
train[train$���%in%names(train_aver[train_aver<5.45]),2]<-"terrible"

train$����<-as.factor(train$����)


names(test_aver[test_aver<=13.10])
names(test_aver[test_aver<8.9])
names(test_aver[test_aver<7.1])
names(test_aver[test_aver<5.45])

test[test$���%in%names(test_aver[test_aver<=16.10]),2]<-"excellent"
test[test$���%in%names(test_aver[test_aver<8.9]),2]<-"good"
test[test$���%in%names(test_aver[test_aver<7.1]),2]<-"bad"
test[test$���%in%names(test_aver[test_aver<5.45]),2]<-"terrible"

test$����<-as.factor(test$����)


#####


library(rpart)
library(caret)
library(e1071)


str(train[,-c(1,3:13,23,28)])
str(test[,-c(1,3:16,26,31)])


#svm

model<-svm(����~.,data=train[,-c(1,3:13,23,28)])
summary(model)

tune.svm<-tune(svm,����~.,data=train[,-c(1,3:13,23,28)],ranges=list(gamma=seq(0.001,0.01,0.002),cost=seq(1,10,1)))
summary(tune.svm)

model<-svm(����~.,data=train[,-c(1,3:13,23,28)],gamma=0.003,cost=10)

predict(model,test[,-c(1,3:16,26,31)])


confusionMatrix(predict(model,test[,-c(1,3:16,26,31)]),test$����)



#
recent<-read.csv("C:/Users/JYEON/Desktop/4-2/�ý���/������.csv")

recent
str(recent)

unique(recent$X)
recent[c(1,9,29,49,69,85),]
predict(model,recent[c(1,9,29,49,69,85),])


data.frame(unique(recent$X),predict(model,recent[c(1,9,29,49,69,85),]))