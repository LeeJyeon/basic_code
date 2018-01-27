str(total)


#PCA 결과
total<-rbind(train,test)
head(total)


PCA1<-princomp(total[,20:25],cor=T)
summary(PCA1)
PCA1$loadings
screeplot(PCA1)


PCA2<-princomp(total[,26:34],cor=T)
summary(PCA2)
PCA2$loadings
screeplot(PCA2)

newtotal<-total[,-c(20:34)]
newtotal<-cbind(newtotal,PCA1$scores[,1:2],PCA2$scores[,1:4])
head(newtotal)


#lm
mape<-list(NA)
pb<-progress_bar$new(total=50)
for(i in 1:50){
  x<-newtrain[newtrain$회차==i,-c(1,2,4)]
  tempfit<-lm(TNMS~.,data=x)
  
  mape[[i]]<-mean(abs((newtest[newtest$회차==i,3]-predict(tempfit,newtest[newtest$회차==i,-c(1:4)]))/newtest[newtest$회차==i,3])*100)
  pb$tick()
  
}




x<-newtrain[newtrain$회차==1,-c(1,2,4)]
tempfit<-lm(TNMS~.,data=x)
summary(tempfit)





#lasso


#회차별 모델생성 _ lasso
library(progress)
pb<-progress_bar$new(total=50)

for(i in 1:50){
  assign(paste("fit",i,sep=""),cv.glmnet(as.matrix(newtrain[newtrain$회차==i,-c(1:4)]),newtrain[newtrain$회차==i,3],type.measure="mae",
                                         alpha=0,family="gaussian"))
  pb$tick()
  
}



library(glmnet)
#전체로_first
mart<-train[,-c(1,2,5,27,32)]

str(mart)
first_lm<-lm(TNMS~.,data=mart)
first_step<-step(first_lm)

str(mart)
x<-as.matrix(mart[,-2])
y<-mart[,2]
lasso<-glmnet(x,y,alpha = 1)
plot(lasso,xvar = "lambda")
cv.lasso<-cv.glmnet(x,y,alpha=1)
plot(cv.lasso)
cv.lasso
first_lasso<-glmnet(x,y,alpha=1,lambda=c(cv.lasso$lambda.min,cv.lasso$lambda.1se))

test_x<-test[,-c(1,2,4,5,27,32)]

first_yhat<-cbind(test[,2:3],predict(first_lm,test),predict(first_step,test),predict(first_lasso,as.matrix(test_x)))

write.csv(first_yhat,"전체로.csv")
