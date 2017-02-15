#reading data

#data.v1 <- read.csv("data v1.csv")
dataset=data.v1
View(dataset)
heart1<-subset(dataset,dataset$Condition=="HF")
View(heart1)
################################################## DESCRIPTIVE STATISTICS ##############################################
table(heart1$Income.Range)
table(heart1$Gender)
table(heart1$Marital.Status)
## removing unwanted columns
new<-heart1[,-c(1,3,4,6,7,8,9,10,11,13,14,28,29,32,33,36,37,39,41,45)]
View(new)
##  sum of no.of specialist
new$SP<-rep(0,nrow(new))
new$SP<-rowSums(new[, c(4:16)])
new<-new[,-c(4:16)]
View(new)
new$SP = as.ordered(new$SP)
##  deriving Ccirisk variable

new$CciRisk<-rep(0,nrow(new))
new$CciRisk[new$Charlson.Co.morbidity.Index..CCI.>=2 & new$Charlson.Co.morbidity.Index..CCI<=8]<-"1"
new$CciRisk[new$Charlson.Co.morbidity.Index..CCI.>=9 & new$Charlson.Co.morbidity.Index..CCI<=25]<-"2"   
new$CciRisk[new$Charlson.Co.morbidity.Index..CCI.>=26 & new$Charlson.Co.morbidity.Index..CCI<=43]<-"3"
new$CciRisk = as.factor(new$CciRisk)

# subsetting numeric and categorical  variables
numeric <- new[sapply(new, is.numeric)]
cat<-new[sapply(new, is.factor)]


## performing PCA
p1 <- princomp(numeric[,-1], cor = TRUE)  ## using correlation matrix
summary(p1)
loadings(p1)
plot(p1)
screeplot(p1, type = 'line', main = 'Screeplot')
p1$scores[,c(1:4)]
p2<-data.frame(p1$scores[,c(1:4)])

## rpart
RF<-cbind(p2,cat)
library(rpart)
fit = rpart(CciRisk~., method="class", data=RF[,-c(10,14)])
printcp(fit)
summary(fit)

## logistic regresssion
library(nnet)
logistic<-multinom(CciRisk~Comp.3+Comp.1+Comp.4+Comp.2+Race +Admission.Type+Gender+Marital.Status  ,data=RF)

##PREDICTION OF SCORES
score_logistic<-data.frame(predict(logistic, newdata=RF,type="probs"))
score_logistic$sc<-apply(score_logistic,1,max)
colnames(score_logistic)<-c("1","2","3","risk")
score_logistic$category<-names(score_logistic)[max.col(score_logistic,ties.method="first")]


 #write.csv(score_logistic,"C:\\Users\\pullannagari.manasa\\Desktop\\logisticoutput.csv")
## EVALUATION
cm1<-table(score_logistic$category,RF$CciRisk)
print(cm1)
1-sum(diag(cm1))/sum(cm1)
library(caret) 
library(e1071)
confusionMatrix(cm1)


## random forest
library(rpart)
library(randomForest)
arf<-rpart(CciRisk~Comp.3+Comp.2+Discharge.Disposition+Race +Admission.Type+Marital.Status+Gender,data=RF,method="class")
summary(arf)
## PREDICTION OF SCORES
score_randomforest<-data.frame(predict(arf, newdata=RF,type="prob"))
score_randomforest$sc<-apply(score_randomforest,1,max)
colnames(score_randomforest)<-c("1","2","3","risk")
score_randomforest$category<-names(score_randomforest)[max.col(score_randomforest,ties.method="first")]
#write.csv(score_randomforest,"C:\\Users\\pullannagari.manasa\\Desktop\\randomforestoutput.csv")
## EVALUATION
library(caret)
confusionMatrix(RF$CciRisk,score_randomforest$category)


## naiveBayes
library(e1071)
model.naiveBayes <- naiveBayes(CciRisk ~ Comp.3+Comp.2+Discharge.Disposition+Race +Admission.Type+Marital.Status+Gender, data = RF, laplace = 3)
RF$pred.naiveBayes <- predict(model.naiveBayes, RF)
## PREDICTION OF SCORES
score_naiveBayes<-data.frame(predict(model.naiveBayes , newdata=RF,type="raw"))
score_naiveBayes$sc<-apply(score_naiveBayes,1,max)
colnames(score_naiveBayes)<-c("1","2","3","risk")
score_naiveBayes$category<-names(score_naiveBayes)[max.col(score_naiveBayes,ties.method="first")]
write.csv(score_randomforest,"C:\\Users\\pullannagari.manasa\\Desktop\\naiveoutput.csv")
## evaluation
library(caret)
tab <- table(pred =score_naiveBayes$category, true = RF$CciRisk)
confusionMatrix(tab)


##ensemble
pc1<-cbind(RF[,c(13,14)],score_logistic$category,score_randomforest$category)
pc1$ensemble <- rep(0,nrow(pc1))

colnames(pc1)<-c("CciRisk","pred.naiveBayes" ,"logistic_score","score_randomforest...5.", "ensemble")



for(i in 1:nrow(pc1)){
  if(pc1$logistic_score[i]== pc1$pred.naiveBayes[i]){
    pc1$ensemble[i] <- pc1$logistic_score[i]
  }
  
  if(pc1$logistic_score[i]==pc1$score_randomforest...5.[i]){
    pc1$ensemble[i] <- pc1$logistic_score[i]
  }
  
  if(pc1$score_randomforest...5.[i]==pc1$pred.naiveBayes[i]){
    pc1$ensemble[i] <- pc1$score_randomforest...5.[i]
  }
  
}

write.csv(pc1,"C:\\Users\\pullannagari.manasa\\Desktop\\outputdata.csv")

table(pc1$ensemble)
pc1$ensemble[pc1$ensemble=="0"] <-3
table(pc1$ensemble)

##evaluation
e<-table(pc1$ensemble,pc1$CciRisk)
library(caret)
print(e)
library(caret) 
confusionMatrix(e)
## ROC#####--------------------------------------------------------------
library(pROC)
##logistic##
preds=predict(logistic)
preds<-as.numeric(preds)
roc1=roc(RF$CciRisk ~ preds)
plot(roc1, col='red')

##randomforest####

pc1$score_randomforest...5.<-as.numeric(pc1$score_randomforest...5.)
roc2=roc(RF$CciRisk ~ pc1$score_randomforest...5.)
plot(roc2, add=TRUE,col='blue')
### naivebayes###
score_naiveBayes$category<-as.numeric(score_naiveBayes$category)
roc3=roc(RF$CciRisk ~ score_naiveBayes$category)
plot(roc3,add=TRUE, col='black')



### ensemble##
pc1$ensemble<-as.numeric(pc1$ensemble)
roc5=roc(RF$CciRisk ~ pc1$ensemble)
plot(roc5,add=TRUE, col='yellow',legend=TRUE)





