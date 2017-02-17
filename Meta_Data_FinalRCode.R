##############################################################################################################################################
###################################      Investor Churn Analysis-Predictive Modeling     #####################################################
##############################################################################################################################################

#Read the Data
raw_data<-read.csv("C:/Users/gogineni.saiteja/Desktop/Churn Models/Final/Trxn_Details.csv")

#Storing data into anothe variable
raw_data1<-raw_data

#Converting categorical variable-SCHM_ID to factor data type
raw_data1$SCHM_ID<-as.factor(raw_data1$SCHM_ID)

#String manipulations 
library(stringr)
#Cleaning tXx_id_num variable i.e PAN Number
raw_data1$tXx_id_num<-str_trim(as.character(raw_data1$tXx_id_num)) #Removing leading and trailing spaces in PAN Number

#Excluding the transactions which are not having PAN number
raw_data1=subset(raw_data1,tXx_id_num!="")
raw_data1$tXx_id_num<-as.factor(raw_data1$tXx_id_num)

#Excluding the transactions which are not having segment information i.e.""
raw_data1$segment=as.character(raw_data1$segment)
raw_data1=subset(raw_data1,segment!="")
raw_data1$segment<-as.factor(raw_data1$segment)

#Checking for the NA values
sapply(raw_data1, function(x) sum(is.na(x)))

#Excluding the transactions which are having PRCH_RDMPTN_CD="Z"
raw_data1$PRCHS_RDMPTN_CD=as.character(raw_data1$PRCHS_RDMPTN_CD)
raw_data1=subset(raw_data1,PRCHS_RDMPTN_CD!="Z")

#Assigning Positive sign to Purchase Transactions and Negative sign to Redemption Transactions
raw_data1$TXN_AMT=ifelse(raw_data1$PRCHS_RDMPTN_CD=="P",raw_data1$TXN_AMT,-raw_data1$TXN_AMT)
raw_data1$TXN_UNIT_CNT=ifelse(raw_data1$PRCHS_RDMPTN_CD=="P",raw_data1$TXN_UNIT_CNT,-raw_data1$TXN_UNIT_CNT)

#Converting Categorical variable-PRCHS_RDMPTN_CD to Factor Data type
raw_data1$PRCHS_RDMPTN_CD=as.factor(raw_data1$PRCHS_RDMPTN_CD)

#Dplyr package for data manipulation
library(dplyr)

final_data<-raw_data1

#Converting TXN_DT and BIRTH_dT variables to Date data type
final_data$TXN_DT<-as.Date(as.character(final_data$TXN_DT), "%m/%d/%Y")
final_data$BIRTH_DT<-as.Date(as.character(final_data$BIRTH_DT), "%m/%d/%Y")

#Excluding columns which are not useful
final_data1<-final_data[,-c(1:4,6,7:10,12:15,18:20,23:32,34,36,38,39,41,42)]

#Calculating Age for Investors
final_data1$Age=round(as.numeric((Sys.Date()-final_data1$BIRTH_DT)/365))

#Filtering Investors whose Age<117
final_data1<-subset(final_data1,Age<117)

#Identifying Unique PAN number and Scheme ID Combinations
pan_4<-data.frame(unique(final_data1[,c(7,1)]))

#Meta Data(Independent variables and Dependent variable)
Meta_Data=data.frame()
for(i in 1:nrow(pan_4)){
  x<-subset(final_data1,as.character(tXx_id_num)==as.character(pan_4[i,1]) & as.character(SCHM_ID)==as.character(pan_4[i,2]))
  x<-data.frame(x %>% group_by(tXx_id_num,SCHM_ID) %>% arrange(TXN_DT))
  if(nrow(x)>3){
    PAMT1=round(x[nrow(x),4]/sum(x[x$PRCHS_RDMPTN_CD=="P",4]),2) 
    PAMT2=round(x[nrow(x)-1,4]/sum(x[x$PRCHS_RDMPTN_CD=="P",4]),2)
    PAMT3=round(x[nrow(x)-2,4]/sum(x[x$PRCHS_RDMPTN_CD=="P",4]),2)
    TD1=as.numeric(x[nrow(x),2]-x[nrow(x)-1,2])
    TD2=as.numeric(x[nrow(x)-1,2]-x[nrow(x)-2,2])
    TD3=as.numeric(x[nrow(x)-2,2]-x[nrow(x)-3,2])
    NAV1=round((x[nrow(x),3]-x[nrow(x)-1,3])/x[nrow(x)-1,3],2)
    NAV2=round((x[nrow(x)-1,3]-x[nrow(x)-2,3])/x[nrow(x)-2,3],2)
    NAV3=round((x[nrow(x)-2,3]-x[nrow(x)-3,3])/x[nrow(x)-3,3],2)
    PUNIT1=round(x[nrow(x),5]/sum(x[x$PRCHS_RDMPTN_CD=="P",5]),2)
    PUNIT2=round(x[nrow(x)-1,5]/sum(x[x$PRCHS_RDMPTN_CD=="P",5]),2)
    PUNIT3=round(x[nrow(x)-2,5]/sum(x[x$PRCHS_RDMPTN_CD=="P",5]),2)
    print(i)
    churn=1
    if((sum(x$TXN_UNIT_CNT)/sum(x[x$PRCHS_RDMPTN_CD=="P",5]))*100>1){
      churn=0
    }
  }
  Meta_Data<-rbind(Meta_Data,data.frame(PAN=pan_4[i,1],SCHM_ID=pan_4[i,2],PAMT1=PAMT1,PAMT2=PAMT2,PAMT3=PAMT3,TD1=TD1,TD2=TD2,TD3=TD3,NAV1=NAV1,NAV2=NAV2,NAV3=NAV3,PUNIT1=PUNIT1,PUNIT2=PUNIT2,PUNIT3=PUNIT3,State=x[1,9],Occupation_Type=x[1,10],Segment=x[1,11],Age=x[1,12],Churn=churn))
}

#Data Cleaning-State Variable
Meta_Data$State<-as.character(Meta_Data$State)
Meta_Data$State<-tolower(as.character(Meta_Data$State))
Meta_Data<-subset(Meta_Data,!State %in% c("","27","19","22","7","24","29","33","9","37","28","3","6","8","21"," ","zz","new york","dubai","others","oman","tx","north carolina","england","abu dhabi","us","bangkok","singapore","newyork","belgium","ontario","kenya","sharjah","nsw","lincolnshi","germany","qatar","uk","go","doha","muscat","thailand","jeddah","d n h","london","united kin","dammam","western pr","tanzania","selongor","kuwait","saudi arab","baku","ghana","giza","hong kong","japan","union territory","pennsylvan","varginia","new jersey","hong kong ","illinois","madagascar","nairobi"))
Meta_Data$State<-ifelse(Meta_Data$State %in% c("ap","andhara pr","andhraprad"),"andhra pradesh",ifelse(Meta_Data$State %in% c("chhattisgh","chhattisgarh","chhatisgar","ch"),"chattisgarh",Meta_Data$State))
Meta_Data$State<-ifelse(Meta_Data$State %in% c("new delhi","noida","dlehi","dl"),"delhi",ifelse(Meta_Data$State %in% c("gu","gujrat"),"gujarat",Meta_Data$State))
Meta_Data$State<-ifelse(Meta_Data$State %in% c("ha"),"haryana",ifelse(Meta_Data$State %in% c("jammu  and  ka","jummu & kashmir","j and k"),"jammu and kashmir",Meta_Data$State))
Meta_Data$State<-ifelse(Meta_Data$State %in% c("ka","karanataka","karnatka"),"karnataka",ifelse(Meta_Data$State %in% c("ke","keralap"),"kerala",Meta_Data$State))
Meta_Data$State<-ifelse(Meta_Data$State %in% c("m.p","mp"),"madhya pradesh",ifelse(Meta_Data$State %in% c("ma","maharshtra","maharastra","maharashit","mumbai","maharahstr","maharashtraa","maharashta"),"maharashtra",Meta_Data$State))
Meta_Data$State<-ifelse(Meta_Data$State %in% c("orissa","odisa"),"odisha",ifelse(Meta_Data$State %in% c("chandigarh"),"punjab",Meta_Data$State))
Meta_Data$State<-ifelse(Meta_Data$State %in% c("tamilnadu","tn","tami nadu"),"tamil nadu",ifelse(Meta_Data$State %in% c("telanagana"),"telangana",Meta_Data$State))
Meta_Data$State<-ifelse(Meta_Data$State %in% c("up","u.p","u.p.","ut","utter prad"),"uttar pradesh",ifelse(Meta_Data$State %in% c("west banga","west  beng","wb","westbengal"),"west bengal",Meta_Data$State))
Meta_Data<-subset(Meta_Data,!State %in% c("sultanate","uae"))
Meta_Data$State<-as.factor(Meta_Data$State)

#Excluding/Omitting NA Values because they are very less in number <
Meta_Data_Final1=na.omit(Meta_Data)

#Converting Categorical variables-Churn and Occupation Type to Factor data type
Meta_Data_Final2<-Meta_Data_Final1[,-1]
Meta_Data_Final2$Churn<-as.factor(as.character(Meta_Data_Final2$Churn))
Meta_Data_Final2$Occupation_Type<-as.factor(as.character(Meta_Data_Final2$Occupation_Type))

#Considering Transactions with Most Frequent SCHM_ID's
Meta_Data_Final2$SCHM_ID<-as.character(Meta_Data_Final2$SCHM_ID)
Meta_Data_Final2<-subset(Meta_Data_Final2,SCHM_ID %in% c("1559","1578","1731","1982","3140"))
Meta_Data_Final2$SCHM_ID<-as.factor(Meta_Data_Final2$SCHM_ID)

#Removing specific occupation type on account of low frequency(<20 obs)
Meta_Data_Final2$Occupation_Type<-as.character(Meta_Data_Final2$Occupation_Type)
Meta_Data_Final2<-subset(Meta_Data_Final2,!Occupation_Type %in% c("0","11","12","13","14","16","41","9"))
Meta_Data_Final2$Occupation_Type<-as.factor(Meta_Data_Final2$Occupation_Type)

#Removing specific state on account of low frequency(<20 obs)
x<-data.frame(table(Meta_Data_Final2$State))
x$Var1<-as.character(x$Var1)
state_excl=x[x$Freq<20,"Var1"]
Meta_Data_Final2$State<-as.character(Meta_Data_Final2$State)
Meta_Data_Final2<-subset(Meta_Data_Final2,!State %in% state_excl)
Meta_Data_Final2$State<-as.factor(Meta_Data_Final2$State)

#Removing specific segment on account of low frequency(<16 obs)
y = data.frame(table(Meta_Data_Final2$Segment))
y$Var1<-as.character(y$Var1)
segment_excl=y[y$Freq<16,"Var1"]
Meta_Data_Final2$Segment<-as.character(Meta_Data_Final2$Segment)
Meta_Data_Final2<-subset(Meta_Data_Final2,!Segment %in% segment_excl)
Meta_Data_Final2$Segment<-as.factor(Meta_Data_Final2$Segment)

#Subsetting Numeric variables
Meta_Data_Final2_Numeric=Meta_Data_Final2[,c(2:13,17,18)]
Meta_Data_Final2_Numeric[,14]<-as.numeric(as.character(Meta_Data_Final2_Numeric[,14]))
Meta_Data_Final2_Binned=Meta_Data_Final2_Numeric

#smbinning - Optimal binning will perform based on Weight of Evidence and Information Value(Recursive Partitioning)
#For More Details, Please refer below links 
# https://cran.r-project.org/web/packages/smbinning/smbinning.pdf
# http://blog.revolutionanalytics.com/2015/03/r-package-smbinning-optimal-binning-for-scoring-modeling.html
library(smbinning) 

#Creating Variables on the fly in data frame
create.new_var <- function(x,value){
  assign('Meta_Data_Final2_Numeric', `[[<-`(Meta_Data_Final2_Numeric, x, value=value), envir=.GlobalEnv)
}
create.new_var1 <- function(x,value){
  assign('Meta_Data_Final2_Binned', `[[<-`(Meta_Data_Final2_Binned, x, value=value), envir=.GlobalEnv)
}


#Optimal binning for numerical variables using smbinning
for(i in 2:length(colnames(Meta_Data_Final2_Numeric))){
  binning=smbinning(df=Meta_Data_Final2_Numeric,y="Churn",x=colnames(Meta_Data_Final2_Numeric)[i-1],p=0.01)
  if(is.list(binning)){
    Meta_Data_Final2_Numeric <- create.new_var(paste0(colnames(Meta_Data_Final2_Numeric)[i-1],"_","Bin"), cut(Meta_Data_Final2_Numeric[,which(colnames(Meta_Data_Final2_Numeric)==colnames(Meta_Data_Final2_Numeric)[i-1])],c(-Inf,binning$cuts,Inf),labels = c(1:(length(c(-Inf,binning$cuts,Inf))-1))))
    Meta_Data_Final2_Binned <- create.new_var1(paste0(colnames(Meta_Data_Final2_Binned)[i-1],"_","Bin"), cut(Meta_Data_Final2_Binned[,which(colnames(Meta_Data_Final2_Binned)==colnames(Meta_Data_Final2_Binned)[i-1])],c(-Inf,binning$cuts,Inf)))
    
  }
  
}

#Quantile Based Binning for NAV1 - quantile(Meta_Data_Final2_Binned$NAV1,prob=seq(0,1,length=11),type=5)
Meta_Data_Final2_Binned$NAV1_Bin<-cut(Meta_Data_Final2_Binned$NAV1,breaks=c(-Inf,-0.61,-0.01,0,0.02,0.06,2.15))
Meta_Data_Final2_Numeric$NAV1_Bin<-cut(Meta_Data_Final2_Numeric$NAV1,breaks=c(-Inf,-0.61,-0.01,0,0.02,0.06,2.15),labels = c(1:(length(c(-Inf,-0.61,-0.01,0,0.02,0.06,2.15))-1)))

#Quantile Based Binning for NAV3 - quantile(Meta_Data_Final2_Binned$NAV3,prob=seq(0,1,length=11),type=5)
Meta_Data_Final2_Binned$NAV3_Bin<-cut(Meta_Data_Final2_Binned$NAV3,breaks=c(-Inf,-0.58,-0.02,0,0.04,0.09,2.37))
Meta_Data_Final2_Numeric$NAV3_Bin<-cut(Meta_Data_Final2_Numeric$NAV3,breaks=c(-Inf,-0.58,-0.02,0,0.04,0.09,2.37),labels = c(1:(length(c(-Inf,-0.58,-0.02,0,0.04,0.09,2.37))-1)))

#Binning Ranges for different Numeric Variables
Bin_Ranges=data.frame()
for(i in 15:ncol(Meta_Data_Final2_Binned)){
  Bin_Ranges<-rbind(Bin_Ranges,data.frame(Variable_Name=colnames(Meta_Data_Final2_Binned)[i],Bin=1:length(levels(Meta_Data_Final2_Binned[,i])),Range=levels(Meta_Data_Final2_Binned[,i])))
}

#Gathering/Preparing Data for Modeling
Meta_Data_Final2_Model_Binned=cbind(Meta_Data_Final2[,c(1,14:16)],Meta_Data_Final2_Binned[,c(15:27)],Churn=Meta_Data_Final2[,18])
Meta_Data_Final2_Model_Numeric=cbind(Meta_Data_Final2[,c(1,14:16)],Meta_Data_Final2_Numeric[,c(15:27)],Churn=Meta_Data_Final2[,18])

#Calculating Information tables using information package
library("Information")
Meta_Data_Final2_Model_Numeric1<-Meta_Data_Final2_Model_Numeric
Meta_Data_Final2_Model_Numeric1$Churn<-as.numeric(as.character(Meta_Data_Final2_Model_Numeric1$Churn))

#Creating Infotables
IV <- Information::create_infotables(data=Meta_Data_Final2_Model_Numeric1, y="Churn", parallel=FALSE)

#Plotting Infotables
Information::plot_infotables(IV, IV$Summary$Variable[1:6], same_scale=TRUE)

#Saving Information Tables for different variables into one folder
IV_df=data.frame()
IV_Tab=IV$Tables
IV_Tab$Summary=IV$Summary
for(i in 1:length(IV_Tab)){
  write.csv(IV_Tab[[i]],paste0("C:/Users/gogineni.saiteja/Desktop/Churn Models/Final/OutPut/IV_",colnames(IV_Tab[[i]])[1],".csv"))
}

#Performing Bivariate analysis of all variables with resepect to Response variable
BivariateAnalysis<-data.frame()
for(i in 1:(ncol(Meta_Data_Final2_Model_Numeric)-1)){
  x=subset(Meta_Data_Final2_Model_Numeric,Churn=="0")
  x$Churn=as.character(x$Churn)
  x1=data.frame(table(x[,i],x[,18]))
  x1<-x1[,-2]
  colnames(x1)<-c("Levels","Freq_0")
  y=subset(Meta_Data_Final2_Model_Numeric,Churn=="1")
  y$Churn=as.character(y$Churn)
  x2=data.frame(table(y[,i],y[,18]))
  x2<-x2[,-2]
  colnames(x2)<-c("Levels","Freq_1")
  x1$Varb_Name=colnames(Meta_Data_Final2_Model_Numeric)[i]
  x1<-cbind(x1,Freq_1=x2[,-1])
  x1<-cbind(Varb_Name=x1[,3],x1[,c(1,2)],Freq_1=x1[,4])
  x1$Percnt_Churn=round((x1$Freq_1/(x1$Freq_0+x1$Freq_1))*100,3)
  BivariateAnalysis<-rbind(BivariateAnalysis,x1)
}


#Saving Meta Data, Binning Ranges and Bivariate analysis results in path
write.csv(Meta_Data_Final2_Model_Numeric,"C:/Users/gogineni.saiteja/Desktop/Churn Models/Final/OutPut/MetaData_InvestorChurnAnalysis.csv")
write.csv(Bin_Ranges,"C:/Users/gogineni.saiteja/Desktop/Churn Models/Final/OutPut/Variables_Binning_Ranges.csv")
write.csv(BivariateAnalysis,"C:/Users/gogineni.saiteja/Desktop/Churn Models/Final/OutPut/BivariateAnalysis.csv")


#Multicollinearity Test for categorical Variables
Categorical<-Meta_Data_Final2_Model_Numeric[,-18]
combos <- combn(ncol(Categorical),2)

library(MASS)
library(plyr)

#Performing Chi square test for different combinations of categorical independent variables and storing results in data frame
IV_Chisqaure<-adply(combos, 2, function(x) {
  test <- chisq.test(Categorical[, x[1]], Categorical[, x[2]])
  out <- data.frame("Row"=colnames(Categorical)[x[1]],"Column"=colnames(Categorical[x[2]]),"Chi.Square" = round(test$statistic,3),"df"= test$parameter,"p.value" = test$p.value)
  return(out)
})
IV_Chisqaure<-IV_Chisqaure[,-1]

#We are not able to find multicollinearity with distinct clarity while using chi square since p value is zero for almost all
#CV Test is another approach to detect multicollinearity among categorical variables
#CV Test function
cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /(length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  return(as.numeric(CV))
}

#Performing CramerV/Phi test for different combinations of categorical independent variables and storing results in data frame
IV_CVtest<-data.frame("Indep_Var1"= character(),"Indep_Var2"= character(),"CramerV/Phi_Value"= numeric())
for(i in 1:nrow(IV_Chisqaure)){
  CPValue<-cv.test(Meta_Data_Final2_Model_Numeric[,which(colnames(Meta_Data_Final2_Model_Numeric)==IV_Chisqaure[i,1])],Meta_Data_Final2_Model_Numeric[,which(colnames(Meta_Data_Final2_Model_Numeric)==IV_Chisqaure[i,2])])
  CPValue<-data.frame(IV_Chisqaure[i,1],IV_Chisqaure[i,2],CPValue)
  IV_CVtest<-rbind(IV_CVtest,CPValue)
}
colnames(IV_CVtest)<-c("Categ_Indep_Var1","Categ_Indep_Var2","CramerV_Phi_Value")
IV_CVtest$CramerV_Phi_Value=round(IV_CVtest$CramerV_Phi_Value,2)

#Saving MultiCollinearity Results into path
write.csv(IV_CVtest,"C:/Users/gogineni.saiteja/Desktop/Churn Models/Final/OutPut/MultiCollinearity_CV1.csv")

#Meta Data for Modelling - After Excluding Correlated predictors
MetaData_RF=Meta_Data_Final2_Model_Numeric[,-c(5:7)]


## 75% of the sample size
smp_size <- floor(0.7 * nrow(MetaData_RF))

## set the seed to make your partition reproductible
set.seed(123)

#Generating Sample Index values using sample size
train_ind <- sample(seq_len(nrow(MetaData_RF)), size = smp_size)

#Dividing Data into Train and Test
train <- MetaData_RF[train_ind, ]
test <- MetaData_RF[-train_ind, ]

################################# Investor Churn Pedictive Models - Binary Classification ############################

########### Predictive Model1-Random Forest #################
#Fitting Random Forest Model - Before Paramter Tuning
library(randomForest)
RandomForest_ChurnModel=randomForest(Churn~.,train,importance=TRUE)
summary(RandomForest_ChurnModel)
(VI_F=importance(RandomForest_ChurnModel))
library(caret)
varImp(RandomForest_ChurnModel)
varImpPlot(RandomForest_ChurnModel,type=2)

#Tuning Random Forest Model using TunePareto package 

#TunePareto-Generic methods for parameter tuning of classification algorithms using multiple scoring functions
#The methods of this package allow to assess the performance of classifiers with respect to certain parameter values and multiple scoring functions,
# such as the cross-validation error or the sensitivity.
#The method tests combinations of the supplied classifier parameters according to the supplied scoring functions and 
# calculates the Pareto front of optimal parameter configurations.
#For More Details, Refere Below Links.
#   https://www.jstatsoft.org/article/view/v046i05/v46i05.pdf
#   https://cran.r-project.org/web/packages/TunePareto/TunePareto.pdf
library(TunePareto)
rfFit<-tunePareto(data = train[,-15], labels = train[,15],classifier = tunePareto.randomForest(), mtry=seq(3,5,by=1),ntree = seq(200,600,50),
                  objectiveFunctions = list(cvError(nfold = 10, ntimes = 10, stratified = TRUE),cvSensitivity(nfold = 10, ntimes = 10, stratified = TRUE,
                                                                                                              caseClass = 1)))

#Fitting Random Forest using Optimal Parameters - After Parameter Tuning
rfFit1<-randomForest(Churn~.,train,mtry=rfFit$bestCombinations[[2]]$mtry,ntree=rfFit$bestCombinations[[2]]$ntree,importance=TRUE)
summary(rfFit1)
RandomForest_ChurnModel=rfFit1
(VI_F=importance(rfFit1))
library(caret)
varImp(rfFit1)
varImpPlot(RandomForest_ChurnModel,type=2)

#Model Evaluation Error Metrics-Random Forest
#Confusion Matrix
test$Churn_Predict_RF=predict(rfFit1,test[,-15])
churn_probs=data.frame(predict(rfFit1,test[,-15],type=c("prob")))
test$Churn_PredictProb_RF=churn_probs[,2]
confusionMatrix(test$Churn_Predict_RF,test$Churn,positive = '1')

#ROC Curve
library(ROCR)
pred <- prediction(test$Churn_PredictProb_RF,test$Churn)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
par(bg="lightblue", mai=c(1.2,1.5,1,1))
plot(perf,main="Investor Churn Analysis-ROC Curve")
plot(perf,main="Investor Churn Analysis-ROC Curve", colorize=TRUE)
abline(a=0, b= 1)

## Precision/Recall curve (x-axis: recall, y-axis: precision)
perf1 <- performance(pred, "prec", "rec")
plot(perf1,main="Investor Churn Analysis-Recall vs Precision")
plot(perf1,main="Investor Churn Analysis-Recall vs Precision",colorize=TRUE)

## Sensitivity/Specificity curve (x-axis: Specificity,y-axis: Sensitivity)
perf2 <- performance(pred, "sens", "spec")
plot(perf2,main="Investor Churn Analysis-Specificity vs Sensitivity")
plot(perf2,main="Investor Churn Analysis-Specificity vs Sensitivity",colorize=TRUE)

########### Predictive Model2-Naive Bayes #################
#Fitting Naive Bayes Model
library(e1071)
NaiveBayes_ChurnModel=naiveBayes(Churn~.,train)

#Model Evaluation Error Metrics-Naive Bayes
#Confusion Matrix
test$Churn_Predict_NaiveBayes=predict(NaiveBayes_ChurnModel,test[,-15])
churn_probs_naiveBayes=data.frame(predict(NaiveBayes_ChurnModel,test[,-15],type=c("raw")))
test$Churn_PredictProb_NaiveBayes=round(churn_probs_naiveBayes[,2],3)
confusionMatrix(test$Churn_Predict_NaiveBayes,test$Churn,positive = '1')

#ROC Curve
library(ROCR)
pred_naivBayes <- prediction(test$Churn_PredictProb_NaiveBayes,test$Churn)
perf_naiveBayes <- performance(pred_naivBayes, measure = "tpr", x.measure = "fpr")
par(bg="lightblue", mai=c(1.2,1.5,1,1))
plot(perf_naiveBayes,main="Investor Churn Analysis-ROC Curve")
plot(perf_naiveBayes,main="Investor Churn Analysis-ROC Curve", colorize=TRUE)
abline(a=0, b= 1)

## Precision/Recall curve (x-axis: recall, y-axis: precision)
perf1_naiveBayes <- performance(pred_naivBayes, "prec", "rec")
plot(perf1_naiveBayes,main="Investor Churn Analysis-Recall vs Precision")
plot(perf1_naiveBayes,main="Investor Churn Analysis-Recall vs Precision",colorize=TRUE)

## Sensitivity/Specificity curve (x-axis: Specificity,y-axis: Sensitivity)
perf2_naiveBayes <- performance(pred_naivBayes, "sens", "spec")
plot(perf2_naiveBayes,main="Investor Churn Analysis-Specificity vs Sensitivity")
plot(perf2_naiveBayes,main="Investor Churn Analysis-Specificity vs Sensitivity",colorize=TRUE)
