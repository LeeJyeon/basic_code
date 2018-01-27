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




#predict
yhat1<-predict(fit1, s=fit1$lambda.1se, newx=as.matrix(newtest[newtest$회차==1,-c(1:4)]))
yhat2<-predict(fit2, s=fit2$lambda.1se, newx=as.matrix(test[test$회차==2,-c(1:4)]))
yhat3<-predict(fit3, s=fit3$lambda.1se, newx=as.matrix(test[test$회차==3,-c(1:4)]))
yhat4<-predict(fit4, s=fit4$lambda.1se, newx=as.matrix(test[test$회차==4,-c(1:4)]))
yhat5<-predict(fit5, s=fit5$lambda.1se, newx=as.matrix(test[test$회차==5,-c(1:4)]))
yhat6<-predict(fit6, s=fit6$lambda.1se, newx=as.matrix(test[test$회차==6,-c(1:4)]))
yhat7<-predict(fit7, s=fit7$lambda.1se, newx=as.matrix(test[test$회차==7,-c(1:4)]))
yhat8<-predict(fit8, s=fit8$lambda.1se, newx=as.matrix(newtest[newtest$회차==8,-c(1:4)]))
yhat9<-predict(fit9, s=fit9$lambda.1se, newx=as.matrix(test[test$회차==9,-c(1:4)]))
yhat10<-predict(fit10, s=fit10$lambda.1se, newx=as.matrix(test[test$회차==10,-c(1:4)]))


mape1<-mean(abs((test[test$회차==1,3]-yhat1)/test[test$회차==1,3])*100)
mape2<-mean(abs((test[test$회차==2,3]-yhat1)/test[test$회차==2,3])*100)
mape3<-mean(abs((test[test$회차==3,3]-yhat1)/test[test$회차==3,3])*100)
mape4<-mean(abs((test[test$회차==4,3]-yhat1)/test[test$회차==4,3])*100)
mape5<-mean(abs((test[test$회차==5,3]-yhat1)/test[test$회차==5,3])*100)
mape6<-mean(abs((test[test$회차==6,3]-yhat1)/test[test$회차==6,3])*100)
mape7<-mean(abs((test[test$회차==7,3]-yhat1)/test[test$회차==7,3])*100)
mape8<-mean(abs((test[test$회차==8,3]-yhat1)/test[test$회차==8,3])*100)
mape9<-mean(abs((test[test$회차==9,3]-yhat1)/test[test$회차==9,3])*100)
mape10<-mean(abs((test[test$회차==10,3]-yhat1)/test[test$회차==10,3])*100)


