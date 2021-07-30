library(rpart) #classification and regression trees
library(partykit) #treeplots
library(MASS) #breast and pima indiandata
install.packages("randomForest")
library(randomForest) #random forests
install.packages("gbm")
library(gbm) #gradient boosting
install.packages("caret")
library(caret) #tune hyper-parameters

bcancer=read.table("D:\\KULIAHAN\\sem 7\\DSS\\Project 1\\breastcancer.txt",header=T)
bcanc = sample(2, nrow(bcancer), replace=TRUE, prob=c(0.7,0.3))
trainData = bcancer[bcanc==1,]
testData = bcancer[bcanc==2,]
set.seed(123)
bcanc_rf = randomForest(as.factor(diagnosis)~., data=trainData, ntree=100, proximity=T)
print(bcanc_rf)
importance(bcanc_rf)
plot(bcanc_rf)
which.min(bcanc_rf$err.rate[,1])
bcanc_rf.2 = randomForest(as.factor(diagnosis)~., data=trainData, ntree=23)
print(bcanc_rf.2)
bcanc_rf_test= predict(bcanc_rf.2, newdata=testData, type="response")
table(bcanc_rf_test, testData$diagnosis)
(100+61)/170 #acc
(61/(100+61)) #TPR
(5/(4+5)) #FPR
library(pROC)
roc_df <- data.frame(
	TPR=(61/(100+61)), #TPR
	FPR=(5/(4+5))) #FPR
rectangle <- function(x, y, width, height, density=12, angle=-45, ...) 
  polygon(c(x,x,x+width,x+width), c(y,y+height,y+height,y), 
          density=density, angle=angle, ...)
roc_df <- transform(roc_df, 
  dFPR = c(diff(FPR), 0),
  dTPR = c(diff(TPR), 0))
plot(0:10/10, 0:10/10, type='n', xlab="FPR", ylab="TPR")
abline(h=0:10/10, col="lightblue")
abline(v=0:10/10, col="lightblue")
with(roc_df, {
  mapply(rectangle, x=FPR, y=0,   
         width=dFPR, height=TPR, col="green", lwd=2)
  mapply(rectangle, x=FPR, y=TPR, 
         width=dFPR, height=dTPR, col="blue", lwd=2)
  lines(FPR, TPR, type='b', lwd=3, col="red")
})
	