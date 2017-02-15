
library(gmodels)
library(gtools)
library(corrplot)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(gplots)
library(dplyr)
library(lazyeval)
library(plyr)
library(arules)
library(arulesViz)
library(datasets)
library(ggplot2)
require(zoo)
library(zoo)
library(xgboost)
library(randomForest)
library(Hmisc)
library(stringr)

raw_data=read.csv("C:/Users/lakshmirekha.k/Desktop/MF/edelwiess/trxn_details_edelwiess.csv")

raw_data <- trxn_details_edelwiess

rm(trxn_details_edelwiess)

colnames(raw_data)[33] = "tXx_id_num"

unique_investors_in_raw_data <- sum(count(data.frame(unique(raw_data$tXx_id_num)))$freq)
unique_investors_in_raw_data

raw_data$BIRTH_DT=as.Date(raw_data$BIRTH_DT, "%m/%d/%Y %M:%S")
raw_data$BIRTH_DT<-as.numeric(format(raw_data$BIRTH_DT, "%Y")) 
raw_data$TXN_DT=as.Date(raw_data$TXN_DT, "%m/%d/%Y")


# added 11202016

##unique_pan generation
unique_PAN <- unique(raw_data$tXx_id_num)
unique_PAN <- data.frame(pan=unique(raw_data$tXx_id_num))
unique_PAN$pan = as.character(unique_PAN$pan)

raw_data_2011=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(raw_data,raw_data$tXx_id_num==unique_PAN$pan[i])
  e=subset(e,e$TXN_DT==min(e$TXN_DT))
  raw_data_2011=rbind(e,raw_data_2011)
}

nrow(raw_data_2011)

raw_data_2011 <- subset(raw_data_2011, raw_data_2011$TXN_DT > "2011-01-01")
nrow(raw_data_2011)
raw_data_2011_PAN <- raw_data_2011[c(33)]

raw_data_2011_PAN <- as.data.frame(unique(raw_data_2011$tXx_id_num))

nrow(raw_data_2011_PAN)
colnames(raw_data_2011_PAN)[1] <- "tXx_id_num"

raw_data_merged <- subset(raw_data, (tXx_id_num %in% raw_data_2011_PAN$tXx_id_num))

# raw_data_merged <- merge(raw_data_2011_PAN, raw_data, by = "tXx_id_num", all = FALSE)
x <- count(unique(raw_data_merged$tXx_id_num))
sum(x$freq)
# added 11202016

raw_data <- raw_data_merged

unique_investors_in_raw_data_from2011 <- sum(count(data.frame(unique(raw_data$tXx_id_num)))$freq)
unique_investors_in_raw_data_from2011

raw_data_pln=subset(raw_data,!(PLAN_ID  %in% c("4618", "5084", "6293", "4842", "4972", "12265", "4740","4613","12266","6286")))

raw_data_pln_unique <- as.data.frame(unique(raw_data_pln$tXx_id_num))
colnames(raw_data_pln_unique)[1] <- "tXx_id_num"
raw_data2 <- subset(raw_data, !(tXx_id_num %in% raw_data_pln_unique$tXx_id_num))

unique_investors_in_raw_data_from2011_select_plan <- sum(count(data.frame(unique(raw_data2$tXx_id_num)))$freq)
unique_investors_in_raw_data_from2011_select_plan

raw_data <- raw_data2

# raw_data =read.csv("C:/Users/sarang.venukala/Desktop/Edelwiess/raw_data.csv")

# raw_data <- raw_data[c(-1)]
#######################################################################################################################
#############################  metadata generation  ###################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
rm(DT, analysis_dataset, analysis_dataset, analysis_dataset_0_DV, analysis_dataset_0_DV1, analysis_dataset_0_IV, analysis_dataset_0_IV, analysis_dataset_0_IV1, analysis_dataset1)

# BASE PARAMETER TUNING: 

NET_BALANCE_THRESHOLD_FOR_SILENT_CHURN <- 15 #
TXN_NUMBER <- 9# min of range of the number of transactions considered of the investors

#######################################################################################################################

# transactions more than (note: this is linked to the generation of data above a particular transaction count)
TXN_ABOVE <- TXN_NUMBER - 1
TXN_BELOW <- TXN_NUMBER + 8

TXN_ABOVE_DV <- TXN_ABOVE + 1

##################################################################################################################################
analysis_dataset <- raw_data

#remove all the variables
analysis_dataset <- analysis_dataset[-c(1,2,3,4,7,9, 10, 12,15,18,19,20,23,24,27,28,29,25,26,30,31,32,34,39,41)]

analysis_dataset22 <- subset(analysis_dataset, analysis_dataset$segment != 'INDIVIDUAL')
unique(analysis_dataset22$tXx_id_num)
table(analysis_dataset22$segment)

analysis_dataset3 <- as.data.frame(unique(analysis_dataset22$tXx_id_num))
colnames(analysis_dataset3)[1] <- "tXx_id_num"

#here we need to take the opposite of subset http://stackoverflow.com/questions/9852832/r-not-in-subset
analysis_dataset <- subset(analysis_dataset, !(tXx_id_num %in% analysis_dataset3$tXx_id_num))

#subset stats
unique_investors_after_selecting_only_individuals <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_selecting_only_individuals


analysis_dataset$TXN_DT1=(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT=as.numeric(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT4= analysis_dataset$TXN_DT


#removing NAs and spaces
analysis_dataset <- na.omit(analysis_dataset)
colnames(analysis_dataset)[11]="tXx_id_num"

#subset stats
unique_investors_after_removing_NAs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_NAs


#converting transaction types to factor to remove the levels
analysis_dataset$TXN_TYPE_CD <- as.factor(analysis_dataset$TXN_TYPE_CD)
analysis_dataset1 <- subset(analysis_dataset, TXN_TYPE_CD %in% c("ADD", "ADD", "ADDR", "DIR", "DIRR", "DSPI", "IPO", "IPOR", "LTIA", "LTIAR", "LTIN", "LTINR", "LTOF", "LTOF", "LTOFR", "LTOP", "LTOP", "LTOPR", "NEWR", "RED", "RED", "REDR", "SWD", "SWDR", "SWIA", "SWIAR", "SWIN", "SWINR", "SWOF", "SWOF", "SWOFR", "SWOP", "SWOP", "SWOPR", "TRFI", "TRFO", "TRFO", "TRMI", "TRMO"))

analysis_dataset1 <- analysis_dataset1[11]
unique_PAN <- as.data.frame(unique(analysis_dataset1$tXx_id_num))
analysis_dataset <- subset(analysis_dataset, tXx_id_num %in% unique(analysis_dataset1$tXx_id_num))

#subset stats
unique_investors_after_removing_SIPs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_SIPs

#subset 2
#removing 'z' in purchase_redemption_code from the dataframe
analysis_dataset<-analysis_dataset[analysis_dataset$PRCHS_RDMPTN_CD != "Z", ]

#subset stats
unique_investors_after_removing_Z <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_Z


##convert TXN_AMT variables to -ve and +ve according to P/R
analysis_dataset$TXN_AMT<- with(analysis_dataset, ifelse(analysis_dataset$PRCHS_RDMPTN_CD == "R", -TXN_AMT, TXN_AMT))
analysis_dataset$TXN_DT3 <- as.Date(analysis_dataset$TXN_DT1, "%m/%d/%Y")
unique_silent_investors_before_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_before_txn3


#######################################################################################################################11/18/2016
# library(plyr)
train1 = ddply(analysis_dataset,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3 <- nrow(trainC1)
unique_investors_more_than_3

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_more_than_3_less_than_25 <- nrow(trainC1)
unique_investors_more_than_3_less_than_25
########################################################################11/18/2016

analysis_dataset <- merge(analysis_dataset, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset <-subset(analysis_dataset,analysis_dataset$count == 1)
#subset stats
unique_silent_investors_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_txn3

########################################################################################   changes made: 11/23/2016 ##############

# change the study period according to the period considered for the analysis
time_period <- 0
study_period<- time_period * 30
DV_Month <- max(analysis_dataset$TXN_DT3) - study_period
IV_Month <- DV_Month - 60

colnames(analysis_dataset)
analysis_dataset <- analysis_dataset[-c(22,23)]

IV_DV_Month_Diff <- 0
IV_Month_select_threshold <-  IV_Month - 60

investors_DV <- subset(analysis_dataset, analysis_dataset$TXN_DT < DV_Month & analysis_dataset$TXN_DT > IV_Month)
investors_IV <- subset(analysis_dataset, analysis_dataset$TXN_DT < (IV_Month-IV_DV_Month_Diff) & analysis_dataset$TXN_DT > (IV_Month_select_threshold-IV_DV_Month_Diff))

investors_DV2 <- as.data.frame(unique(investors_DV$tXx_id_num))
colnames(investors_DV2)[1] <- "tXx_id_num"


investors_IV2 <- as.data.frame(unique(investors_IV$tXx_id_num))
colnames(investors_IV2)[1] <- "tXx_id_num"

analysis_dataset_0_DV1 <- merge(analysis_dataset, investors_DV2, by = "tXx_id_num", all = FALSE)
analysis_dataset_0_IV1 <- merge(analysis_dataset, investors_IV2, by = "tXx_id_num", all = FALSE)


analysis_dataset_0_DV <- analysis_dataset_0_DV1
analysis_dataset_0_IV <- analysis_dataset_0_IV1

########################################################################################   changes made: 11/23/2016 ##############
##################################################################################################################################

#library(plyr)
train1 = ddply(analysis_dataset_0_DV,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE_DV, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_DV <- nrow(trainC1)
unique_investors_DV

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_DV2 <- nrow(trainC1)
unique_investors_DV2
#######################################################################################################################11/18/2016

analysis_dataset_0_DV <- merge(analysis_dataset_0_DV, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset_0_DV <-subset(analysis_dataset_0_DV,analysis_dataset_0_DV$count == 1)

#subset stats
unique_silent_investors_DV <- sum(count(data.frame(unique(analysis_dataset_0_DV$tXx_id_num)))$freq)
unique_silent_investors_DV
colnames(analysis_dataset_0_DV)
analysis_dataset_0_DV <- analysis_dataset_0_DV[-c(22,23)]
########################################################

##unique_pan generation
unique_PAN <- unique(analysis_dataset_0_DV$tXx_id_num)
unique_PAN <- data.frame(pan=unique(analysis_dataset_0_DV$tXx_id_num))
unique_PAN$pan = as.character(unique_PAN$pan)



# generating dependent variable 
dataframe_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(analysis_dataset_0_DV,analysis_dataset_0_DV$tXx_id_num==unique_PAN$pan[i])
  e$net_amount <- sum(e$TXN_AMT)
  b=subset(e,e$PRCHS_RDMPTN_CD=="P")
  e$avg_sum_purchases<-mean(b$TXN_AMT)
  e$perc_net_amount_with_avg_sum_purchases <-  (e$net_amount/e$avg_sum_purchases)*100
  e$silent_investors <- ifelse(e$perc_net_amount_with_avg_sum_purchases < NET_BALANCE_THRESHOLD_FOR_SILENT_CHURN, 1, 0)
  dataframe_dependent=rbind(e,dataframe_dependent)
}

##################################################################################################################################
final_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(dataframe_dependent,dataframe_dependent$tXx_id_num==unique_PAN$pan[i])
  b=subset(e,e$TXN_DT1== max(e$TXN_DT1))
  final_dependent=rbind(b,final_dependent)
}

colnames(final_dependent)[1]="pan"
# final_dependent <- as.data.frame(unique(final_dependent$pan))
final_dependent <- final_dependent[!duplicated(final_dependent$pan), ]
final_dependent <- final_dependent[,c(1,19,25)]

x <- count(unique(final_dependent$pan))
DV_investors <- sum(x$freq)
DV_investors

########################## metadata generation for independent variables ######### 
########################## metadata generation for independent variables ######### 

#library(plyr)
train1 = ddply(analysis_dataset_0_IV,.(tXx_id_num),nrow)
########################################## 
# change made 11192016

TXN_BELOW_IV <- TXN_ABOVE + 4

train1$count <- ifelse(train1$V1< TXN_BELOW_IV & train1$V1> TXN_ABOVE, 1,0)
########################################## 

trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3_IV <- nrow(trainC1)
unique_investors_more_than_3_IV

# merge function changed
analysis_dataset_0_IV <- merge(analysis_dataset_0_IV, trainC1, by = "tXx_id_num", all = FALSE)

unique_PAN <- as.data.frame(unique(analysis_dataset_0_IV$tXx_id_num))
colnames(unique_PAN)[1] = "pan"
unique_PAN$pan = as.character(unique_PAN$pan)
transaction_values = list()
for(i in 1:nrow(unique_PAN)){
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  print (i)
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  ftd$TXN_Diff <- ftd$TXN_DT - min(ftd$TXN_DT)
  b=c(ftd$TXN_Diff)
  c =c(ftd$TXN_AMT)
  e =append(b,c)
  e = append(e,c(ftd$NAV_UNIT_RT))
  e = append(e,c(ftd$TXN_UNIT_CNT))
  d = append(unique_PAN$pan[i],e)
  transaction_values[[i]]=d
}

max.length <- max(sapply(transaction_values, length))
## Add NA values to list elements
sp <- lapply(transaction_values, function(v) { c(v, rep(NA, max.length-length(v)))})
sp=as.data.frame(transaction_values)

sp1 = as.data.frame(t(sp))
colnames(sp1) = c("pan","TD1","TD2","TD3","TD4","TD5","TD6","TD7","TD8","TD9","AMT1","AMT2","AMT3","AMT4","AMT5","AMT6","AMT7","AMT8","AMT9","NAV1","NAV2","NAV3", "NAV4","NAV5","NAV6","NAV7","NAV8","NAV9","UNIT_CNT1", "UNIT_CNT2", "UNIT_CNT3","UNIT_CNT4","UNIT_CNT5","UNIT_CNT6","UNIT_CNT7","UNIT_CNT8","UNIT_CNT9")


sum(is.na(sp1))
#############################

Avg_amounts = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  purchases =  subset(unique_single,PRCHS_RDMPTN_CD == 'P')
  avg_amount = mean(purchases$TXN_AMT)
  sum_amount = sum(unique_single$TXN_AMT)
  max_TD=max(unique_single$TXN_DT1)
  min_TD = min(unique_single$TXN_DT1)
  a = append(avg_amount,sum_amount)
  b = append(unique_PAN$pan[i],a)
  c = append(max_TD,min_TD)
  e = append(b,as.character(c))
  Avg_amounts[[i]] = e 
}


sample=as.data.frame(Avg_amounts)
sample1 = as.data.frame(t(sample))
colnames(sample1) = c("pan","avg_amount","sum_amount","Max_TD","Min_TD")
sum(is.na(sample1))
rownames(sample1) = c(1:nrow(sample1))
rownames(sp1) = c(1:nrow(sp1))

# merge function changed
final_independent_IV <- merge(sp1, sample1, by = "pan", all = FALSE)

# change made for TXN change

data_numeric = final_independent_IV[2:39]
data_numeric = apply(data_numeric,2, as.numeric)

# change made for TXN change

final_independent_IV = cbind(final_independent_IV[-c(2:39)],data_numeric)

# change made for TXN change

final_independent_IV$PAMT7 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT8+final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*1000
final_independent_IV$PAMT8 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*100
final_independent_IV$percentage_holding_IV_month = (((final_independent_IV$sum_amount))/final_independent_IV$avg_amount)*100

final_independent_IV$pan=as.character(final_independent_IV$pan)
final_dependent$pan=as.character(final_dependent$pan)

############################################## Adding Categorical Variables to the data #########################################\

analysis_dataset_0_IV$SCHM_ID <- as.character(analysis_dataset_0_IV$SCHM_ID)
analysis_dataset_0_IV$PLAN_ID <- as.character(analysis_dataset_0_IV$PLAN_ID)
analysis_dataset_0_IV$CITY_NM <- as.character(analysis_dataset_0_IV$CITY_NM)
analysis_dataset_0_IV$OCCPTN_TYPE_CD <- as.character(analysis_dataset_0_IV$OCCPTN_TYPE_CD)

categorical_list = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  unique_single_1=unique_single[order(unique_single$TXN_DT),]
  ltd  = unique_single_1[0,]
  td = rbind(ftd,ltd)
  a = td$SCHM_ID
  b = td$PLAN_ID
  c = td$TXN_TYPE_CD
  occupation = td$OCCPTN_TYPE_CD[1]
  city = td$CITY_NM[1]
  e = append(a,b)
  e = append(e,as.character(c))
  e = append(e,occupation)
  e = append(e,as.character(city))
  e = append(unique_PAN$pan[i],e)
  categorical_list[[i]] = e
}
categorical_variables <- as.data.frame(categorical_list)
categorical_variables <- as.data.frame(t(categorical_variables))

sum(is.na(categorical_variables))

# change made for TXN change

colnames(categorical_variables) = c("pan","SCH1","SCH2","SCH3","SCH4","SCH5","SCH6","SCH7","SCH8","SCH9","PL1","PL2","PL3","PL4","PL5","PL6","PL7","PL8","PL9","TX_CD1","TX_CD2","TX_CD3","TX_CD4","TX_CD5","TX_CD6","TX_CD7","TX_CD8","TX_CD9","OCCPTN_TYPE","CITY")
# del2

apply(categorical_variables, 2, as.factor)
colnames(categorical_variables)
sum(is.na(categorical_variables))
row.names(categorical_variables) = c(1:nrow(categorical_variables))
colnames(categorical_variables)

categorical_variables_unique_investors <- nrow(categorical_variables)
categorical_variables_unique_investors
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

final_independent_IV <- merge(final_independent_IV, categorical_variables, by = "pan", all = FALSE)
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

########### removing duplicates
final_independent_IV <- final_independent_IV[!duplicated(final_independent_IV$pan), ]

final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors
######## merging independent and dependent variables

final_IV_DV <- merge(final_independent_IV, final_dependent, by="pan", all=TRUE)
final_IV_DV <- final_IV_DV[complete.cases(final_IV_DV$TX_CD2), ]
final_IV_DV_unique_investors <- nrow(final_IV_DV)
final_IV_DV_unique_investors
metadata_1_0_2 <- final_IV_DV

table(final_IV_DV$silent_investors)
table(metadata_1_0_2$silent_investors)
sum(is.na(metadata_1_0_2))

period_0 <- metadata_1_0_2

TXN_ABOVE
TXN_BELOW_IV
TXN_BELOW
TXN_NUMBER

##################################################################################################################################
################################################        NEXT PERIOD        #######################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

# transactions more than (note: this is linked to the generation of data above a particular transaction count)
TXN_ABOVE <- TXN_NUMBER - 1
TXN_BELOW <- TXN_NUMBER + 8

##################################################################################################################################
analysis_dataset <- raw_data

#remove all the variables
analysis_dataset <- analysis_dataset[-c(1,2,3,4,7,9, 10, 12,15,18,19,20,23,24,27,28,29,25,26,30,31,32,34,39,41)]

analysis_dataset22 <- subset(analysis_dataset, analysis_dataset$segment != 'INDIVIDUAL')
unique(analysis_dataset22$tXx_id_num)
table(analysis_dataset22$segment)

analysis_dataset3 <- as.data.frame(unique(analysis_dataset22$tXx_id_num))
colnames(analysis_dataset3)[1] <- "tXx_id_num"

#here we need to take the opposite of subset http://stackoverflow.com/questions/9852832/r-not-in-subset
analysis_dataset <- subset(analysis_dataset, !(tXx_id_num %in% analysis_dataset3$tXx_id_num))

#subset stats
unique_investors_after_selecting_only_individuals <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_selecting_only_individuals


analysis_dataset$TXN_DT1=(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT=as.numeric(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT4= analysis_dataset$TXN_DT


#removing NAs and spaces
analysis_dataset <- na.omit(analysis_dataset)
colnames(analysis_dataset)[11]="tXx_id_num"

#subset stats
unique_investors_after_removing_NAs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_NAs


#converting transaction types to factor to remove the levels
analysis_dataset$TXN_TYPE_CD <- as.factor(analysis_dataset$TXN_TYPE_CD)
analysis_dataset1 <- subset(analysis_dataset, TXN_TYPE_CD %in% c("ADD", "ADD", "ADDR", "DIR", "DIRR", "DSPI", "IPO", "IPOR", "LTIA", "LTIAR", "LTIN", "LTINR", "LTOF", "LTOF", "LTOFR", "LTOP", "LTOP", "LTOPR", "NEWR", "RED", "RED", "REDR", "SWD", "SWDR", "SWIA", "SWIAR", "SWIN", "SWINR", "SWOF", "SWOF", "SWOFR", "SWOP", "SWOP", "SWOPR", "TRFI", "TRFO", "TRFO", "TRMI", "TRMO"))

analysis_dataset1 <- analysis_dataset1[11]
unique_PAN <- as.data.frame(unique(analysis_dataset1$tXx_id_num))
analysis_dataset <- subset(analysis_dataset, tXx_id_num %in% unique(analysis_dataset1$tXx_id_num))

#subset stats
unique_investors_after_removing_SIPs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_SIPs

#subset 2
#removing 'z' in purchase_redemption_code from the dataframe
analysis_dataset<-analysis_dataset[analysis_dataset$PRCHS_RDMPTN_CD != "Z", ]

#subset stats
unique_investors_after_removing_Z <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_Z


##convert TXN_AMT variables to -ve and +ve according to P/R
analysis_dataset$TXN_AMT<- with(analysis_dataset, ifelse(analysis_dataset$PRCHS_RDMPTN_CD == "R", -TXN_AMT, TXN_AMT))
analysis_dataset$TXN_DT3 <- as.Date(analysis_dataset$TXN_DT1, "%m/%d/%Y")
unique_silent_investors_before_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_before_txn3

#######################################################################################################################11/18/2016
# library(plyr)
train1 = ddply(analysis_dataset,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3 <- nrow(trainC1)
unique_investors_more_than_3

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_more_than_3_less_than_25 <- nrow(trainC1)
unique_investors_more_than_3_less_than_25
########################################################################11/18/2016

analysis_dataset <- merge(analysis_dataset, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset <-subset(analysis_dataset,analysis_dataset$count == 1)
#subset stats
unique_silent_investors_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_txn3

########################################################################################   changes made: 11/23/2016 ##############

# change the study period according to the period considered for the analysis
time_period <- time_period + 1
study_period<- time_period * 30
DV_Month <- max(analysis_dataset$TXN_DT3) - study_period
IV_Month <- DV_Month - 60
IV_Month_select_threshold <-  IV_Month - 90

colnames(analysis_dataset)
analysis_dataset <- analysis_dataset[-c(22,23)]

investors_DV <- subset(analysis_dataset, analysis_dataset$TXN_DT < DV_Month & analysis_dataset$TXN_DT > IV_Month)
investors_IV <- subset(analysis_dataset, analysis_dataset$TXN_DT < (IV_Month-IV_DV_Month_Diff) & analysis_dataset$TXN_DT > (IV_Month_select_threshold-IV_DV_Month_Diff))  

investors_DV2 <- as.data.frame(unique(investors_DV$tXx_id_num))
colnames(investors_DV2)[1] <- "tXx_id_num"


investors_IV2 <- as.data.frame(unique(investors_IV$tXx_id_num))
colnames(investors_IV2)[1] <- "tXx_id_num"

analysis_dataset_0_DV1 <- merge(analysis_dataset, investors_DV2, by = "tXx_id_num", all = FALSE)
analysis_dataset_0_IV1 <- merge(analysis_dataset, investors_IV2, by = "tXx_id_num", all = FALSE)


analysis_dataset_0_DV <- analysis_dataset_0_DV1
analysis_dataset_0_IV <- analysis_dataset_0_IV1

########################################################################################   changes made: 11/23/2016 ##############
##################################################################################################################################

#library(plyr)
train1 = ddply(analysis_dataset_0_DV,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE_DV, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_DV <- nrow(trainC1)
unique_investors_DV

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_DV2 <- nrow(trainC1)
unique_investors_DV2
#######################################################################################################################11/18/2016

analysis_dataset_0_DV <- merge(analysis_dataset_0_DV, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset_0_DV <-subset(analysis_dataset_0_DV,analysis_dataset_0_DV$count == 1)

#subset stats
unique_silent_investors_DV <- sum(count(data.frame(unique(analysis_dataset_0_DV$tXx_id_num)))$freq)
unique_silent_investors_DV
colnames(analysis_dataset_0_DV)
analysis_dataset_0_DV <- analysis_dataset_0_DV[-c(22,23)]
########################################################

##unique_pan generation
unique_PAN <- unique(analysis_dataset_0_DV$tXx_id_num)
unique_PAN <- data.frame(pan=unique(analysis_dataset_0_DV$tXx_id_num))
unique_PAN$pan = as.character(unique_PAN$pan)

# generating dependent variable 
dataframe_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(analysis_dataset_0_DV,analysis_dataset_0_DV$tXx_id_num==unique_PAN$pan[i])
  e$net_amount <- sum(e$TXN_AMT)
  b=subset(e,e$PRCHS_RDMPTN_CD=="P")
  e$avg_sum_purchases<-mean(b$TXN_AMT)
  e$perc_net_amount_with_avg_sum_purchases <-  (e$net_amount/e$avg_sum_purchases)*100
  e$silent_investors <- ifelse(e$perc_net_amount_with_avg_sum_purchases < NET_BALANCE_THRESHOLD_FOR_SILENT_CHURN, 1, 0)
  dataframe_dependent=rbind(e,dataframe_dependent)
}

##################################################################################################################################
final_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(dataframe_dependent,dataframe_dependent$tXx_id_num==unique_PAN$pan[i])
  b=subset(e,e$TXN_DT1== max(e$TXN_DT1))
  final_dependent=rbind(b,final_dependent)
}

colnames(final_dependent)[1]="pan"
# final_dependent <- as.data.frame(unique(final_dependent$pan))
final_dependent <- final_dependent[!duplicated(final_dependent$pan), ]
final_dependent <- final_dependent[,c(1,19,25)]

x <- count(unique(final_dependent$pan))
DV_investors <- sum(x$freq)
DV_investors

########################## metadata generation for independent variables ######### 
########################## metadata generation for independent variables ######### 

#library(plyr)
train1 = ddply(analysis_dataset_0_IV,.(tXx_id_num),nrow)
########################################## 
# change made 11192016

TXN_BELOW_IV <- TXN_ABOVE + 4

train1$count <- ifelse(train1$V1< TXN_BELOW_IV & train1$V1> TXN_ABOVE, 1,0)
########################################## 

trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3_IV <- nrow(trainC1)
unique_investors_more_than_3_IV

# merge function changed
analysis_dataset_0_IV <- merge(analysis_dataset_0_IV, trainC1, by = "tXx_id_num", all = FALSE)

unique_PAN <- as.data.frame(unique(analysis_dataset_0_IV$tXx_id_num))
colnames(unique_PAN)[1] = "pan"
unique_PAN$pan = as.character(unique_PAN$pan)
transaction_values = list()
for(i in 1:nrow(unique_PAN)){
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  print (i)
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  ftd$TXN_Diff <- ftd$TXN_DT - min(ftd$TXN_DT)
  b=c(ftd$TXN_Diff)
  c =c(ftd$TXN_AMT)
  e =append(b,c)
  e = append(e,c(ftd$NAV_UNIT_RT))
  e = append(e,c(ftd$TXN_UNIT_CNT))
  d = append(unique_PAN$pan[i],e)
  transaction_values[[i]]=d
}

max.length <- max(sapply(transaction_values, length))
## Add NA values to list elements
sp <- lapply(transaction_values, function(v) { c(v, rep(NA, max.length-length(v)))})
sp=as.data.frame(transaction_values)

# change made for TXN change

sp1 = as.data.frame(t(sp))
colnames(sp1) = c("pan","TD1","TD2","TD3","TD4","TD5","TD6","TD7","TD8","TD9","AMT1","AMT2","AMT3","AMT4","AMT5","AMT6","AMT7","AMT8","AMT9","NAV1","NAV2","NAV3", "NAV4","NAV5","NAV6","NAV7","NAV8","NAV9","UNIT_CNT1", "UNIT_CNT2", "UNIT_CNT3","UNIT_CNT4","UNIT_CNT5","UNIT_CNT6","UNIT_CNT7","UNIT_CNT8","UNIT_CNT9")
# del

sum(is.na(sp1))
#############################

Avg_amounts = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  purchases =  subset(unique_single,PRCHS_RDMPTN_CD == 'P')
  avg_amount = mean(purchases$TXN_AMT)
  sum_amount = sum(unique_single$TXN_AMT)
  max_TD=max(unique_single$TXN_DT1)
  min_TD = min(unique_single$TXN_DT1)
  a = append(avg_amount,sum_amount)
  b = append(unique_PAN$pan[i],a)
  c = append(max_TD,min_TD)
  e = append(b,as.character(c))
  Avg_amounts[[i]] = e 
}


sample=as.data.frame(Avg_amounts)
sample1 = as.data.frame(t(sample))
colnames(sample1) = c("pan","avg_amount","sum_amount","Max_TD","Min_TD")
sum(is.na(sample1))
rownames(sample1) = c(1:nrow(sample1))
rownames(sp1) = c(1:nrow(sp1))

# merge function changed
final_independent_IV <- merge(sp1, sample1, by = "pan", all = FALSE)

# change made for TXN change

data_numeric = final_independent_IV[2:39]
data_numeric = apply(data_numeric,2, as.numeric)

# change made for TXN change

final_independent_IV = cbind(final_independent_IV[-c(2:39)],data_numeric)

final_independent_IV$PAMT7 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT8+final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*1000
final_independent_IV$PAMT8 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*100
final_independent_IV$percentage_holding_IV_month = (((final_independent_IV$sum_amount))/final_independent_IV$avg_amount)*100

final_independent_IV$pan=as.character(final_independent_IV$pan)
final_dependent$pan=as.character(final_dependent$pan)

############################################## Adding Categorical Variables to the data #########################################\

analysis_dataset_0_IV$SCHM_ID <- as.character(analysis_dataset_0_IV$SCHM_ID)
analysis_dataset_0_IV$PLAN_ID <- as.character(analysis_dataset_0_IV$PLAN_ID)
analysis_dataset_0_IV$CITY_NM <- as.character(analysis_dataset_0_IV$CITY_NM)
analysis_dataset_0_IV$OCCPTN_TYPE_CD <- as.character(analysis_dataset_0_IV$OCCPTN_TYPE_CD)

categorical_list = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  unique_single_1=unique_single[order(unique_single$TXN_DT),]
  ltd  = unique_single_1[0,]
  td = rbind(ftd,ltd)
  a = td$SCHM_ID
  b = td$PLAN_ID
  c = td$TXN_TYPE_CD
  occupation = td$OCCPTN_TYPE_CD[1]
  city = td$CITY_NM[1]
  e = append(a,b)
  e = append(e,as.character(c))
  e = append(e,occupation)
  e = append(e,as.character(city))
  e = append(unique_PAN$pan[i],e)
  categorical_list[[i]] = e
}
categorical_variables <- as.data.frame(categorical_list)
categorical_variables <- as.data.frame(t(categorical_variables))

sum(is.na(categorical_variables))

# change made for TXN change

colnames(categorical_variables) = c("pan","SCH1","SCH2","SCH3","SCH4","SCH5","SCH6","SCH7","SCH8","SCH9","PL1","PL2","PL3","PL4","PL5","PL6","PL7","PL8","PL9","TX_CD1","TX_CD2","TX_CD3","TX_CD4","TX_CD5","TX_CD6","TX_CD7","TX_CD8","TX_CD9","OCCPTN_TYPE","CITY")
# del2

apply(categorical_variables, 2, as.factor)
colnames(categorical_variables)
sum(is.na(categorical_variables))
row.names(categorical_variables) = c(1:nrow(categorical_variables))
colnames(categorical_variables)

categorical_variables_unique_investors <- nrow(categorical_variables)
categorical_variables_unique_investors
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

final_independent_IV <- merge(final_independent_IV, categorical_variables, by = "pan", all = FALSE)
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

########### removing duplicates
final_independent_IV <- final_independent_IV[!duplicated(final_independent_IV$pan), ]

final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors
######## merging independent and dependent variables

final_IV_DV <- merge(final_independent_IV, final_dependent, by="pan", all=TRUE)
final_IV_DV <- final_IV_DV[complete.cases(final_IV_DV$TX_CD2), ]
final_IV_DV_unique_investors <- nrow(final_IV_DV)
final_IV_DV_unique_investors
metadata_1_0_2 <- final_IV_DV

table(final_IV_DV$silent_investors)
table(metadata_1_0_2$silent_investors)
sum(is.na(metadata_1_0_2))

period_1 <- metadata_1_0_2

TXN_ABOVE
TXN_BELOW_IV
TXN_BELOW
TXN_NUMBER


##################################################################################################################################
################################################        NEXT PERIOD        #######################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################


# transactions more than (note: this is linked to the generation of data above a particular transaction count)
TXN_ABOVE <- TXN_NUMBER - 1
TXN_BELOW <- TXN_NUMBER + 8

##################################################################################################################################
analysis_dataset <- raw_data

#remove all the variables
analysis_dataset <- analysis_dataset[-c(1,2,3,4,7,9, 10, 12,15,18,19,20,23,24,27,28,29,25,26,30,31,32,34,39,41)]

analysis_dataset22 <- subset(analysis_dataset, analysis_dataset$segment != 'INDIVIDUAL')
unique(analysis_dataset22$tXx_id_num)
table(analysis_dataset22$segment)

analysis_dataset3 <- as.data.frame(unique(analysis_dataset22$tXx_id_num))
colnames(analysis_dataset3)[1] <- "tXx_id_num"

#here we need to take the opposite of subset http://stackoverflow.com/questions/9852832/r-not-in-subset
analysis_dataset <- subset(analysis_dataset, !(tXx_id_num %in% analysis_dataset3$tXx_id_num))

#subset stats
unique_investors_after_selecting_only_individuals <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_selecting_only_individuals


analysis_dataset$TXN_DT1=(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT=as.numeric(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT4= analysis_dataset$TXN_DT


#removing NAs and spaces
analysis_dataset <- na.omit(analysis_dataset)
colnames(analysis_dataset)[11]="tXx_id_num"

#subset stats
unique_investors_after_removing_NAs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_NAs


#converting transaction types to factor to remove the levels
analysis_dataset$TXN_TYPE_CD <- as.factor(analysis_dataset$TXN_TYPE_CD)
analysis_dataset1 <- subset(analysis_dataset, TXN_TYPE_CD %in% c("ADD", "ADD", "ADDR", "DIR", "DIRR", "DSPI", "IPO", "IPOR", "LTIA", "LTIAR", "LTIN", "LTINR", "LTOF", "LTOF", "LTOFR", "LTOP", "LTOP", "LTOPR", "NEWR", "RED", "RED", "REDR", "SWD", "SWDR", "SWIA", "SWIAR", "SWIN", "SWINR", "SWOF", "SWOF", "SWOFR", "SWOP", "SWOP", "SWOPR", "TRFI", "TRFO", "TRFO", "TRMI", "TRMO"))

analysis_dataset1 <- analysis_dataset1[11]
unique_PAN <- as.data.frame(unique(analysis_dataset1$tXx_id_num))
analysis_dataset <- subset(analysis_dataset, tXx_id_num %in% unique(analysis_dataset1$tXx_id_num))

#subset stats
unique_investors_after_removing_SIPs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_SIPs

#subset 2
#removing 'z' in purchase_redemption_code from the dataframe
analysis_dataset<-analysis_dataset[analysis_dataset$PRCHS_RDMPTN_CD != "Z", ]

#subset stats
unique_investors_after_removing_Z <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_Z


##convert TXN_AMT variables to -ve and +ve according to P/R
analysis_dataset$TXN_AMT<- with(analysis_dataset, ifelse(analysis_dataset$PRCHS_RDMPTN_CD == "R", -TXN_AMT, TXN_AMT))
analysis_dataset$TXN_DT3 <- as.Date(analysis_dataset$TXN_DT1, "%m/%d/%Y")
unique_silent_investors_before_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_before_txn3

#######################################################################################################################11/18/2016
# library(plyr)
train1 = ddply(analysis_dataset,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3 <- nrow(trainC1)
unique_investors_more_than_3

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_more_than_3_less_than_25 <- nrow(trainC1)
unique_investors_more_than_3_less_than_25
########################################################################11/18/2016

analysis_dataset <- merge(analysis_dataset, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset <-subset(analysis_dataset,analysis_dataset$count == 1)
#subset stats
unique_silent_investors_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_txn3

########################################################################################   changes made: 11/23/2016 ##############

# change the study period according to the period considered for the analysis
time_period <- time_period + 1
study_period<- time_period * 30
DV_Month <- max(analysis_dataset$TXN_DT3) - study_period
IV_Month <- DV_Month - 60
IV_Month_select_threshold <-  IV_Month - 90

colnames(analysis_dataset)
analysis_dataset <- analysis_dataset[-c(22,23)]

investors_DV <- subset(analysis_dataset, analysis_dataset$TXN_DT < DV_Month & analysis_dataset$TXN_DT > IV_Month)
investors_IV <- subset(analysis_dataset, analysis_dataset$TXN_DT < (IV_Month-IV_DV_Month_Diff) & analysis_dataset$TXN_DT > (IV_Month_select_threshold-IV_DV_Month_Diff))  

investors_DV2 <- as.data.frame(unique(investors_DV$tXx_id_num))
colnames(investors_DV2)[1] <- "tXx_id_num"


investors_IV2 <- as.data.frame(unique(investors_IV$tXx_id_num))
colnames(investors_IV2)[1] <- "tXx_id_num"

analysis_dataset_0_DV1 <- merge(analysis_dataset, investors_DV2, by = "tXx_id_num", all = FALSE)
analysis_dataset_0_IV1 <- merge(analysis_dataset, investors_IV2, by = "tXx_id_num", all = FALSE)


analysis_dataset_0_DV <- analysis_dataset_0_DV1
analysis_dataset_0_IV <- analysis_dataset_0_IV1

########################################################################################   changes made: 11/23/2016 ##############
##################################################################################################################################

#library(plyr)
train1 = ddply(analysis_dataset_0_DV,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE_DV, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_DV <- nrow(trainC1)
unique_investors_DV

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_DV2 <- nrow(trainC1)
unique_investors_DV2
#######################################################################################################################11/18/2016

analysis_dataset_0_DV <- merge(analysis_dataset_0_DV, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset_0_DV <-subset(analysis_dataset_0_DV,analysis_dataset_0_DV$count == 1)

#subset stats
unique_silent_investors_DV <- sum(count(data.frame(unique(analysis_dataset_0_DV$tXx_id_num)))$freq)
unique_silent_investors_DV
colnames(analysis_dataset_0_DV)
analysis_dataset_0_DV <- analysis_dataset_0_DV[-c(22,23)]
########################################################

##unique_pan generation
unique_PAN <- unique(analysis_dataset_0_DV$tXx_id_num)
unique_PAN <- data.frame(pan=unique(analysis_dataset_0_DV$tXx_id_num))
unique_PAN$pan = as.character(unique_PAN$pan)

# generating dependent variable 
dataframe_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(analysis_dataset_0_DV,analysis_dataset_0_DV$tXx_id_num==unique_PAN$pan[i])
  e$net_amount <- sum(e$TXN_AMT)
  b=subset(e,e$PRCHS_RDMPTN_CD=="P")
  e$avg_sum_purchases<-mean(b$TXN_AMT)
  e$perc_net_amount_with_avg_sum_purchases <-  (e$net_amount/e$avg_sum_purchases)*100
  e$silent_investors <- ifelse(e$perc_net_amount_with_avg_sum_purchases < NET_BALANCE_THRESHOLD_FOR_SILENT_CHURN, 1, 0)
  dataframe_dependent=rbind(e,dataframe_dependent)
}

##################################################################################################################################
final_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(dataframe_dependent,dataframe_dependent$tXx_id_num==unique_PAN$pan[i])
  b=subset(e,e$TXN_DT1== max(e$TXN_DT1))
  final_dependent=rbind(b,final_dependent)
}

colnames(final_dependent)[1]="pan"
# final_dependent <- as.data.frame(unique(final_dependent$pan))
final_dependent <- final_dependent[!duplicated(final_dependent$pan), ]
final_dependent <- final_dependent[,c(1,19,25)]

x <- count(unique(final_dependent$pan))
DV_investors <- sum(x$freq)
DV_investors

########################## metadata generation for independent variables ######### 
########################## metadata generation for independent variables ######### 

#library(plyr)
train1 = ddply(analysis_dataset_0_IV,.(tXx_id_num),nrow)
########################################## 
# change made 11192016

TXN_BELOW_IV <- TXN_ABOVE + 4

train1$count <- ifelse(train1$V1< TXN_BELOW_IV & train1$V1> TXN_ABOVE, 1,0)
########################################## 

trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3_IV <- nrow(trainC1)
unique_investors_more_than_3_IV

# merge function changed
analysis_dataset_0_IV <- merge(analysis_dataset_0_IV, trainC1, by = "tXx_id_num", all = FALSE)

unique_PAN <- as.data.frame(unique(analysis_dataset_0_IV$tXx_id_num))
colnames(unique_PAN)[1] = "pan"
unique_PAN$pan = as.character(unique_PAN$pan)
transaction_values = list()
for(i in 1:nrow(unique_PAN)){
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  print (i)
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  ftd$TXN_Diff <- ftd$TXN_DT - min(ftd$TXN_DT)
  b=c(ftd$TXN_Diff)
  c =c(ftd$TXN_AMT)
  e =append(b,c)
  e = append(e,c(ftd$NAV_UNIT_RT))
  e = append(e,c(ftd$TXN_UNIT_CNT))
  d = append(unique_PAN$pan[i],e)
  transaction_values[[i]]=d
}

max.length <- max(sapply(transaction_values, length))
## Add NA values to list elements
sp <- lapply(transaction_values, function(v) { c(v, rep(NA, max.length-length(v)))})
sp=as.data.frame(transaction_values)

sp1 = as.data.frame(t(sp))
colnames(sp1) = c("pan","TD1","TD2","TD3","TD4","TD5","TD6","TD7","TD8","TD9","AMT1","AMT2","AMT3","AMT4","AMT5","AMT6","AMT7","AMT8","AMT9","NAV1","NAV2","NAV3", "NAV4","NAV5","NAV6","NAV7","NAV8","NAV9","UNIT_CNT1", "UNIT_CNT2", "UNIT_CNT3","UNIT_CNT4","UNIT_CNT5","UNIT_CNT6","UNIT_CNT7","UNIT_CNT8","UNIT_CNT9")
# del

sum(is.na(sp1))
#############################

Avg_amounts = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  purchases =  subset(unique_single,PRCHS_RDMPTN_CD == 'P')
  avg_amount = mean(purchases$TXN_AMT)
  sum_amount = sum(unique_single$TXN_AMT)
  max_TD=max(unique_single$TXN_DT1)
  min_TD = min(unique_single$TXN_DT1)
  a = append(avg_amount,sum_amount)
  b = append(unique_PAN$pan[i],a)
  c = append(max_TD,min_TD)
  e = append(b,as.character(c))
  Avg_amounts[[i]] = e 
}


sample=as.data.frame(Avg_amounts)
sample1 = as.data.frame(t(sample))
colnames(sample1) = c("pan","avg_amount","sum_amount","Max_TD","Min_TD")
sum(is.na(sample1))
rownames(sample1) = c(1:nrow(sample1))
rownames(sp1) = c(1:nrow(sp1))

# merge function changed
final_independent_IV <- merge(sp1, sample1, by = "pan", all = FALSE)

data_numeric = final_independent_IV[2:39]
data_numeric = apply(data_numeric,2, as.numeric)
final_independent_IV = cbind(final_independent_IV[-c(2:39)],data_numeric)

final_independent_IV$PAMT7 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT8+final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*1000
final_independent_IV$PAMT8 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*100
final_independent_IV$percentage_holding_IV_month = (((final_independent_IV$sum_amount))/final_independent_IV$avg_amount)*100

final_independent_IV$pan=as.character(final_independent_IV$pan)
final_dependent$pan=as.character(final_dependent$pan)

############################################## Adding Categorical Variables to the data #########################################\

analysis_dataset_0_IV$SCHM_ID <- as.character(analysis_dataset_0_IV$SCHM_ID)
analysis_dataset_0_IV$PLAN_ID <- as.character(analysis_dataset_0_IV$PLAN_ID)
analysis_dataset_0_IV$CITY_NM <- as.character(analysis_dataset_0_IV$CITY_NM)
analysis_dataset_0_IV$OCCPTN_TYPE_CD <- as.character(analysis_dataset_0_IV$OCCPTN_TYPE_CD)

categorical_list = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  unique_single_1=unique_single[order(unique_single$TXN_DT),]
  ltd  = unique_single_1[0,]
  td = rbind(ftd,ltd)
  a = td$SCHM_ID
  b = td$PLAN_ID
  c = td$TXN_TYPE_CD
  occupation = td$OCCPTN_TYPE_CD[1]
  city = td$CITY_NM[1]
  e = append(a,b)
  e = append(e,as.character(c))
  e = append(e,occupation)
  e = append(e,as.character(city))
  e = append(unique_PAN$pan[i],e)
  categorical_list[[i]] = e
}
categorical_variables <- as.data.frame(categorical_list)
categorical_variables <- as.data.frame(t(categorical_variables))

sum(is.na(categorical_variables))
colnames(categorical_variables) = c("pan","SCH1","SCH2","SCH3","SCH4","SCH5","SCH6","SCH7","SCH8","SCH9","PL1","PL2","PL3","PL4","PL5","PL6","PL7","PL8","PL9","TX_CD1","TX_CD2","TX_CD3","TX_CD4","TX_CD5","TX_CD6","TX_CD7","TX_CD8","TX_CD9","OCCPTN_TYPE","CITY")
# del2

apply(categorical_variables, 2, as.factor)
colnames(categorical_variables)
sum(is.na(categorical_variables))
row.names(categorical_variables) = c(1:nrow(categorical_variables))
colnames(categorical_variables)

categorical_variables_unique_investors <- nrow(categorical_variables)
categorical_variables_unique_investors
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

final_independent_IV <- merge(final_independent_IV, categorical_variables, by = "pan", all = FALSE)
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

########### removing duplicates
final_independent_IV <- final_independent_IV[!duplicated(final_independent_IV$pan), ]

final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors
######## merging independent and dependent variables

final_IV_DV <- merge(final_independent_IV, final_dependent, by="pan", all=TRUE)
final_IV_DV <- final_IV_DV[complete.cases(final_IV_DV$TX_CD2), ]
final_IV_DV_unique_investors <- nrow(final_IV_DV)
final_IV_DV_unique_investors
metadata_1_0_2 <- final_IV_DV

table(final_IV_DV$silent_investors)
table(metadata_1_0_2$silent_investors)
sum(is.na(metadata_1_0_2))

period_2 <- metadata_1_0_2

TXN_ABOVE
TXN_BELOW_IV
TXN_BELOW
TXN_NUMBER


##################################################################################################################################
################################################        NEXT PERIOD        #######################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

# transactions more than (note: this is linked to the generation of data above a particular transaction count)
TXN_ABOVE <- TXN_NUMBER - 1
TXN_BELOW <- TXN_NUMBER + 8

##################################################################################################################################
analysis_dataset <- raw_data

#remove all the variables
analysis_dataset <- analysis_dataset[-c(1,2,3,4,7,9, 10, 12,15,18,19,20,23,24,27,28,29,25,26,30,31,32,34,39,41)]

analysis_dataset22 <- subset(analysis_dataset, analysis_dataset$segment != 'INDIVIDUAL')
unique(analysis_dataset22$tXx_id_num)
table(analysis_dataset22$segment)

analysis_dataset3 <- as.data.frame(unique(analysis_dataset22$tXx_id_num))
colnames(analysis_dataset3)[1] <- "tXx_id_num"

#here we need to take the opposite of subset http://stackoverflow.com/questions/9852832/r-not-in-subset
analysis_dataset <- subset(analysis_dataset, !(tXx_id_num %in% analysis_dataset3$tXx_id_num))

#subset stats
unique_investors_after_selecting_only_individuals <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_selecting_only_individuals


analysis_dataset$TXN_DT1=(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT=as.numeric(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT4= analysis_dataset$TXN_DT


#removing NAs and spaces
analysis_dataset <- na.omit(analysis_dataset)
colnames(analysis_dataset)[11]="tXx_id_num"

#subset stats
unique_investors_after_removing_NAs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_NAs


#converting transaction types to factor to remove the levels
analysis_dataset$TXN_TYPE_CD <- as.factor(analysis_dataset$TXN_TYPE_CD)
analysis_dataset1 <- subset(analysis_dataset, TXN_TYPE_CD %in% c("ADD", "ADD", "ADDR", "DIR", "DIRR", "DSPI", "IPO", "IPOR", "LTIA", "LTIAR", "LTIN", "LTINR", "LTOF", "LTOF", "LTOFR", "LTOP", "LTOP", "LTOPR", "NEWR", "RED", "RED", "REDR", "SWD", "SWDR", "SWIA", "SWIAR", "SWIN", "SWINR", "SWOF", "SWOF", "SWOFR", "SWOP", "SWOP", "SWOPR", "TRFI", "TRFO", "TRFO", "TRMI", "TRMO"))

analysis_dataset1 <- analysis_dataset1[11]
unique_PAN <- as.data.frame(unique(analysis_dataset1$tXx_id_num))
analysis_dataset <- subset(analysis_dataset, tXx_id_num %in% unique(analysis_dataset1$tXx_id_num))

#subset stats
unique_investors_after_removing_SIPs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_SIPs

#subset 2
#removing 'z' in purchase_redemption_code from the dataframe
analysis_dataset<-analysis_dataset[analysis_dataset$PRCHS_RDMPTN_CD != "Z", ]

#subset stats
unique_investors_after_removing_Z <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_Z


##convert TXN_AMT variables to -ve and +ve according to P/R
analysis_dataset$TXN_AMT<- with(analysis_dataset, ifelse(analysis_dataset$PRCHS_RDMPTN_CD == "R", -TXN_AMT, TXN_AMT))
analysis_dataset$TXN_DT3 <- as.Date(analysis_dataset$TXN_DT1, "%m/%d/%Y")
unique_silent_investors_before_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_before_txn3

#######################################################################################################################11/18/2016
# library(plyr)
train1 = ddply(analysis_dataset,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3 <- nrow(trainC1)
unique_investors_more_than_3

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_more_than_3_less_than_25 <- nrow(trainC1)
unique_investors_more_than_3_less_than_25
########################################################################11/18/2016

analysis_dataset <- merge(analysis_dataset, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset <-subset(analysis_dataset,analysis_dataset$count == 1)
#subset stats
unique_silent_investors_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_txn3

########################################################################################   changes made: 11/23/2016 ##############

# change the study period according to the period considered for the analysis
time_period <- time_period + 1
study_period<- time_period * 30
DV_Month <- max(analysis_dataset$TXN_DT3) - study_period
IV_Month <- DV_Month - 60
IV_Month_select_threshold <-  IV_Month - 90

colnames(analysis_dataset)
analysis_dataset <- analysis_dataset[-c(22,23)]

investors_DV <- subset(analysis_dataset, analysis_dataset$TXN_DT < DV_Month & analysis_dataset$TXN_DT > IV_Month)
investors_IV <- subset(analysis_dataset, analysis_dataset$TXN_DT < (IV_Month-IV_DV_Month_Diff) & analysis_dataset$TXN_DT > (IV_Month_select_threshold-IV_DV_Month_Diff))  

investors_DV2 <- as.data.frame(unique(investors_DV$tXx_id_num))
colnames(investors_DV2)[1] <- "tXx_id_num"


investors_IV2 <- as.data.frame(unique(investors_IV$tXx_id_num))
colnames(investors_IV2)[1] <- "tXx_id_num"

analysis_dataset_0_DV1 <- merge(analysis_dataset, investors_DV2, by = "tXx_id_num", all = FALSE)
analysis_dataset_0_IV1 <- merge(analysis_dataset, investors_IV2, by = "tXx_id_num", all = FALSE)


analysis_dataset_0_DV <- analysis_dataset_0_DV1
analysis_dataset_0_IV <- analysis_dataset_0_IV1

########################################################################################   changes made: 11/23/2016 ##############
##################################################################################################################################

#library(plyr)
train1 = ddply(analysis_dataset_0_DV,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE_DV, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_DV <- nrow(trainC1)
unique_investors_DV

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_DV2 <- nrow(trainC1)
unique_investors_DV2
#######################################################################################################################11/18/2016

analysis_dataset_0_DV <- merge(analysis_dataset_0_DV, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset_0_DV <-subset(analysis_dataset_0_DV,analysis_dataset_0_DV$count == 1)

#subset stats
unique_silent_investors_DV <- sum(count(data.frame(unique(analysis_dataset_0_DV$tXx_id_num)))$freq)
unique_silent_investors_DV
colnames(analysis_dataset_0_DV)
analysis_dataset_0_DV <- analysis_dataset_0_DV[-c(22,23)]
########################################################

##unique_pan generation
unique_PAN <- unique(analysis_dataset_0_DV$tXx_id_num)
unique_PAN <- data.frame(pan=unique(analysis_dataset_0_DV$tXx_id_num))
unique_PAN$pan = as.character(unique_PAN$pan)

# generating dependent variable 
dataframe_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(analysis_dataset_0_DV,analysis_dataset_0_DV$tXx_id_num==unique_PAN$pan[i])
  e$net_amount <- sum(e$TXN_AMT)
  b=subset(e,e$PRCHS_RDMPTN_CD=="P")
  e$avg_sum_purchases<-mean(b$TXN_AMT)
  e$perc_net_amount_with_avg_sum_purchases <-  (e$net_amount/e$avg_sum_purchases)*100
  e$silent_investors <- ifelse(e$perc_net_amount_with_avg_sum_purchases < NET_BALANCE_THRESHOLD_FOR_SILENT_CHURN, 1, 0)
  dataframe_dependent=rbind(e,dataframe_dependent)
}

##################################################################################################################################
final_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(dataframe_dependent,dataframe_dependent$tXx_id_num==unique_PAN$pan[i])
  b=subset(e,e$TXN_DT1== max(e$TXN_DT1))
  final_dependent=rbind(b,final_dependent)
}

colnames(final_dependent)[1]="pan"
# final_dependent <- as.data.frame(unique(final_dependent$pan))
final_dependent <- final_dependent[!duplicated(final_dependent$pan), ]
final_dependent <- final_dependent[,c(1,19,25)]

x <- count(unique(final_dependent$pan))
DV_investors <- sum(x$freq)
DV_investors

########################## metadata generation for independent variables ######### 
########################## metadata generation for independent variables ######### 

#library(plyr)
train1 = ddply(analysis_dataset_0_IV,.(tXx_id_num),nrow)
########################################## 
# change made 11192016

TXN_BELOW_IV <- TXN_ABOVE + 4

train1$count <- ifelse(train1$V1< TXN_BELOW_IV & train1$V1> TXN_ABOVE, 1,0)
########################################## 

trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3_IV <- nrow(trainC1)
unique_investors_more_than_3_IV

# merge function changed
analysis_dataset_0_IV <- merge(analysis_dataset_0_IV, trainC1, by = "tXx_id_num", all = FALSE)

unique_PAN <- as.data.frame(unique(analysis_dataset_0_IV$tXx_id_num))
colnames(unique_PAN)[1] = "pan"
unique_PAN$pan = as.character(unique_PAN$pan)
transaction_values = list()
for(i in 1:nrow(unique_PAN)){
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  print (i)
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  ftd$TXN_Diff <- ftd$TXN_DT - min(ftd$TXN_DT)
  b=c(ftd$TXN_Diff)
  c =c(ftd$TXN_AMT)
  e =append(b,c)
  e = append(e,c(ftd$NAV_UNIT_RT))
  e = append(e,c(ftd$TXN_UNIT_CNT))
  d = append(unique_PAN$pan[i],e)
  transaction_values[[i]]=d
}

max.length <- max(sapply(transaction_values, length))
## Add NA values to list elements
sp <- lapply(transaction_values, function(v) { c(v, rep(NA, max.length-length(v)))})
sp=as.data.frame(transaction_values)

sp1 = as.data.frame(t(sp))
colnames(sp1) = c("pan","TD1","TD2","TD3","TD4","TD5","TD6","TD7","TD8","TD9","AMT1","AMT2","AMT3","AMT4","AMT5","AMT6","AMT7","AMT8","AMT9","NAV1","NAV2","NAV3", "NAV4","NAV5","NAV6","NAV7","NAV8","NAV9","UNIT_CNT1", "UNIT_CNT2", "UNIT_CNT3","UNIT_CNT4","UNIT_CNT5","UNIT_CNT6","UNIT_CNT7","UNIT_CNT8","UNIT_CNT9")
# del

sum(is.na(sp1))
#############################

Avg_amounts = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  purchases =  subset(unique_single,PRCHS_RDMPTN_CD == 'P')
  avg_amount = mean(purchases$TXN_AMT)
  sum_amount = sum(unique_single$TXN_AMT)
  max_TD=max(unique_single$TXN_DT1)
  min_TD = min(unique_single$TXN_DT1)
  a = append(avg_amount,sum_amount)
  b = append(unique_PAN$pan[i],a)
  c = append(max_TD,min_TD)
  e = append(b,as.character(c))
  Avg_amounts[[i]] = e 
}


sample=as.data.frame(Avg_amounts)
sample1 = as.data.frame(t(sample))
colnames(sample1) = c("pan","avg_amount","sum_amount","Max_TD","Min_TD")
sum(is.na(sample1))
rownames(sample1) = c(1:nrow(sample1))
rownames(sp1) = c(1:nrow(sp1))

# merge function changed
final_independent_IV <- merge(sp1, sample1, by = "pan", all = FALSE)

data_numeric = final_independent_IV[2:39]
data_numeric = apply(data_numeric,2, as.numeric)
final_independent_IV = cbind(final_independent_IV[-c(2:39)],data_numeric)

final_independent_IV$PAMT7 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT8+final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*1000
final_independent_IV$PAMT8 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*100
final_independent_IV$percentage_holding_IV_month = (((final_independent_IV$sum_amount))/final_independent_IV$avg_amount)*100

final_independent_IV$pan=as.character(final_independent_IV$pan)
final_dependent$pan=as.character(final_dependent$pan)

############################################## Adding Categorical Variables to the data #########################################\

analysis_dataset_0_IV$SCHM_ID <- as.character(analysis_dataset_0_IV$SCHM_ID)
analysis_dataset_0_IV$PLAN_ID <- as.character(analysis_dataset_0_IV$PLAN_ID)
analysis_dataset_0_IV$CITY_NM <- as.character(analysis_dataset_0_IV$CITY_NM)
analysis_dataset_0_IV$OCCPTN_TYPE_CD <- as.character(analysis_dataset_0_IV$OCCPTN_TYPE_CD)

categorical_list = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  unique_single_1=unique_single[order(unique_single$TXN_DT),]
  ltd  = unique_single_1[0,]
  td = rbind(ftd,ltd)
  a = td$SCHM_ID
  b = td$PLAN_ID
  c = td$TXN_TYPE_CD
  occupation = td$OCCPTN_TYPE_CD[1]
  city = td$CITY_NM[1]
  e = append(a,b)
  e = append(e,as.character(c))
  e = append(e,occupation)
  e = append(e,as.character(city))
  e = append(unique_PAN$pan[i],e)
  categorical_list[[i]] = e
}
categorical_variables <- as.data.frame(categorical_list)
categorical_variables <- as.data.frame(t(categorical_variables))

sum(is.na(categorical_variables))
colnames(categorical_variables) = c("pan","SCH1","SCH2","SCH3","SCH4","SCH5","SCH6","SCH7","SCH8","SCH9","PL1","PL2","PL3","PL4","PL5","PL6","PL7","PL8","PL9","TX_CD1","TX_CD2","TX_CD3","TX_CD4","TX_CD5","TX_CD6","TX_CD7","TX_CD8","TX_CD9","OCCPTN_TYPE","CITY")
# del2

apply(categorical_variables, 2, as.factor)
colnames(categorical_variables)
sum(is.na(categorical_variables))
row.names(categorical_variables) = c(1:nrow(categorical_variables))
colnames(categorical_variables)

categorical_variables_unique_investors <- nrow(categorical_variables)
categorical_variables_unique_investors
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

final_independent_IV <- merge(final_independent_IV, categorical_variables, by = "pan", all = FALSE)
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

########### removing duplicates
final_independent_IV <- final_independent_IV[!duplicated(final_independent_IV$pan), ]

final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors
######## merging independent and dependent variables

final_IV_DV <- merge(final_independent_IV, final_dependent, by="pan", all=TRUE)
final_IV_DV <- final_IV_DV[complete.cases(final_IV_DV$TX_CD2), ]
final_IV_DV_unique_investors <- nrow(final_IV_DV)
final_IV_DV_unique_investors
metadata_1_0_2 <- final_IV_DV

table(final_IV_DV$silent_investors)
table(metadata_1_0_2$silent_investors)
sum(is.na(metadata_1_0_2))

period_3 <- metadata_1_0_2

TXN_ABOVE
TXN_BELOW_IV
TXN_BELOW
TXN_NUMBER


##################################################################################################################################
################################################        NEXT PERIOD        #######################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

# transactions more than (note: this is linked to the generation of data above a particular transaction count)
TXN_ABOVE <- TXN_NUMBER - 1
TXN_BELOW <- TXN_NUMBER + 8

##################################################################################################################################
analysis_dataset <- raw_data

#remove all the variables
analysis_dataset <- analysis_dataset[-c(1,2,3,4,7,9, 10, 12,15,18,19,20,23,24,27,28,29,25,26,30,31,32,34,39,41)]

analysis_dataset22 <- subset(analysis_dataset, analysis_dataset$segment != 'INDIVIDUAL')
unique(analysis_dataset22$tXx_id_num)
table(analysis_dataset22$segment)

analysis_dataset3 <- as.data.frame(unique(analysis_dataset22$tXx_id_num))
colnames(analysis_dataset3)[1] <- "tXx_id_num"

#here we need to take the opposite of subset http://stackoverflow.com/questions/9852832/r-not-in-subset
analysis_dataset <- subset(analysis_dataset, !(tXx_id_num %in% analysis_dataset3$tXx_id_num))

#subset stats
unique_investors_after_selecting_only_individuals <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_selecting_only_individuals


analysis_dataset$TXN_DT1=(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT=as.numeric(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT4= analysis_dataset$TXN_DT


#removing NAs and spaces
analysis_dataset <- na.omit(analysis_dataset)
colnames(analysis_dataset)[11]="tXx_id_num"

#subset stats
unique_investors_after_removing_NAs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_NAs


#converting transaction types to factor to remove the levels
analysis_dataset$TXN_TYPE_CD <- as.factor(analysis_dataset$TXN_TYPE_CD)
analysis_dataset1 <- subset(analysis_dataset, TXN_TYPE_CD %in% c("ADD", "ADD", "ADDR", "DIR", "DIRR", "DSPI", "IPO", "IPOR", "LTIA", "LTIAR", "LTIN", "LTINR", "LTOF", "LTOF", "LTOFR", "LTOP", "LTOP", "LTOPR", "NEWR", "RED", "RED", "REDR", "SWD", "SWDR", "SWIA", "SWIAR", "SWIN", "SWINR", "SWOF", "SWOF", "SWOFR", "SWOP", "SWOP", "SWOPR", "TRFI", "TRFO", "TRFO", "TRMI", "TRMO"))

analysis_dataset1 <- analysis_dataset1[11]
unique_PAN <- as.data.frame(unique(analysis_dataset1$tXx_id_num))
analysis_dataset <- subset(analysis_dataset, tXx_id_num %in% unique(analysis_dataset1$tXx_id_num))

#subset stats
unique_investors_after_removing_SIPs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_SIPs

#subset 2
#removing 'z' in purchase_redemption_code from the dataframe
analysis_dataset<-analysis_dataset[analysis_dataset$PRCHS_RDMPTN_CD != "Z", ]

#subset stats
unique_investors_after_removing_Z <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_Z


##convert TXN_AMT variables to -ve and +ve according to P/R
analysis_dataset$TXN_AMT<- with(analysis_dataset, ifelse(analysis_dataset$PRCHS_RDMPTN_CD == "R", -TXN_AMT, TXN_AMT))
analysis_dataset$TXN_DT3 <- as.Date(analysis_dataset$TXN_DT1, "%m/%d/%Y")
unique_silent_investors_before_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_before_txn3

#######################################################################################################################11/18/2016
# library(plyr)
train1 = ddply(analysis_dataset,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3 <- nrow(trainC1)
unique_investors_more_than_3

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_more_than_3_less_than_25 <- nrow(trainC1)
unique_investors_more_than_3_less_than_25
########################################################################11/18/2016

analysis_dataset <- merge(analysis_dataset, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset <-subset(analysis_dataset,analysis_dataset$count == 1)
#subset stats
unique_silent_investors_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_txn3

########################################################################################   changes made: 11/23/2016 ##############

# change the study period according to the period considered for the analysis
time_period <- time_period + 1
study_period<- time_period * 30
DV_Month <- max(analysis_dataset$TXN_DT3) - study_period
IV_Month <- DV_Month - 60
IV_Month_select_threshold <-  IV_Month - 90

colnames(analysis_dataset)
analysis_dataset <- analysis_dataset[-c(22,23)]

investors_DV <- subset(analysis_dataset, analysis_dataset$TXN_DT < DV_Month & analysis_dataset$TXN_DT > IV_Month)
investors_IV <- subset(analysis_dataset, analysis_dataset$TXN_DT < (IV_Month-IV_DV_Month_Diff) & analysis_dataset$TXN_DT > (IV_Month_select_threshold-IV_DV_Month_Diff))  

investors_DV2 <- as.data.frame(unique(investors_DV$tXx_id_num))
colnames(investors_DV2)[1] <- "tXx_id_num"


investors_IV2 <- as.data.frame(unique(investors_IV$tXx_id_num))
colnames(investors_IV2)[1] <- "tXx_id_num"

analysis_dataset_0_DV1 <- merge(analysis_dataset, investors_DV2, by = "tXx_id_num", all = FALSE)
analysis_dataset_0_IV1 <- merge(analysis_dataset, investors_IV2, by = "tXx_id_num", all = FALSE)


analysis_dataset_0_DV <- analysis_dataset_0_DV1
analysis_dataset_0_IV <- analysis_dataset_0_IV1

########################################################################################   changes made: 11/23/2016 ##############
##################################################################################################################################

#library(plyr)
train1 = ddply(analysis_dataset_0_DV,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE_DV, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_DV <- nrow(trainC1)
unique_investors_DV

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_DV2 <- nrow(trainC1)
unique_investors_DV2
#######################################################################################################################11/18/2016

analysis_dataset_0_DV <- merge(analysis_dataset_0_DV, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset_0_DV <-subset(analysis_dataset_0_DV,analysis_dataset_0_DV$count == 1)

#subset stats
unique_silent_investors_DV <- sum(count(data.frame(unique(analysis_dataset_0_DV$tXx_id_num)))$freq)
unique_silent_investors_DV
colnames(analysis_dataset_0_DV)
analysis_dataset_0_DV <- analysis_dataset_0_DV[-c(22,23)]
########################################################

##unique_pan generation
unique_PAN <- unique(analysis_dataset_0_DV$tXx_id_num)
unique_PAN <- data.frame(pan=unique(analysis_dataset_0_DV$tXx_id_num))
unique_PAN$pan = as.character(unique_PAN$pan)

# generating dependent variable 
dataframe_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(analysis_dataset_0_DV,analysis_dataset_0_DV$tXx_id_num==unique_PAN$pan[i])
  e$net_amount <- sum(e$TXN_AMT)
  b=subset(e,e$PRCHS_RDMPTN_CD=="P")
  e$avg_sum_purchases<-mean(b$TXN_AMT)
  e$perc_net_amount_with_avg_sum_purchases <-  (e$net_amount/e$avg_sum_purchases)*100
  e$silent_investors <- ifelse(e$perc_net_amount_with_avg_sum_purchases < NET_BALANCE_THRESHOLD_FOR_SILENT_CHURN, 1, 0)
  dataframe_dependent=rbind(e,dataframe_dependent)
}

##################################################################################################################################
final_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(dataframe_dependent,dataframe_dependent$tXx_id_num==unique_PAN$pan[i])
  b=subset(e,e$TXN_DT1== max(e$TXN_DT1))
  final_dependent=rbind(b,final_dependent)
}

colnames(final_dependent)[1]="pan"
# final_dependent <- as.data.frame(unique(final_dependent$pan))
final_dependent <- final_dependent[!duplicated(final_dependent$pan), ]
final_dependent <- final_dependent[,c(1,19,25)]

x <- count(unique(final_dependent$pan))
DV_investors <- sum(x$freq)
DV_investors

########################## metadata generation for independent variables ######### 
########################## metadata generation for independent variables ######### 

#library(plyr)
train1 = ddply(analysis_dataset_0_IV,.(tXx_id_num),nrow)
########################################## 
# change made 11192016

TXN_BELOW_IV <- TXN_ABOVE + 4

train1$count <- ifelse(train1$V1< TXN_BELOW_IV & train1$V1> TXN_ABOVE, 1,0)
########################################## 

trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3_IV <- nrow(trainC1)
unique_investors_more_than_3_IV

# merge function changed
analysis_dataset_0_IV <- merge(analysis_dataset_0_IV, trainC1, by = "tXx_id_num", all = FALSE)

unique_PAN <- as.data.frame(unique(analysis_dataset_0_IV$tXx_id_num))
colnames(unique_PAN)[1] = "pan"
unique_PAN$pan = as.character(unique_PAN$pan)
transaction_values = list()
for(i in 1:nrow(unique_PAN)){
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  print (i)
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  ftd$TXN_Diff <- ftd$TXN_DT - min(ftd$TXN_DT)
  b=c(ftd$TXN_Diff)
  c =c(ftd$TXN_AMT)
  e =append(b,c)
  e = append(e,c(ftd$NAV_UNIT_RT))
  e = append(e,c(ftd$TXN_UNIT_CNT))
  d = append(unique_PAN$pan[i],e)
  transaction_values[[i]]=d
}

max.length <- max(sapply(transaction_values, length))
## Add NA values to list elements
sp <- lapply(transaction_values, function(v) { c(v, rep(NA, max.length-length(v)))})
sp=as.data.frame(transaction_values)

sp1 = as.data.frame(t(sp))
colnames(sp1) = c("pan","TD1","TD2","TD3","TD4","TD5","TD6","TD7","TD8","TD9","AMT1","AMT2","AMT3","AMT4","AMT5","AMT6","AMT7","AMT8","AMT9","NAV1","NAV2","NAV3", "NAV4","NAV5","NAV6","NAV7","NAV8","NAV9","UNIT_CNT1", "UNIT_CNT2", "UNIT_CNT3","UNIT_CNT4","UNIT_CNT5","UNIT_CNT6","UNIT_CNT7","UNIT_CNT8","UNIT_CNT9")
# del

sum(is.na(sp1))
#############################

Avg_amounts = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  purchases =  subset(unique_single,PRCHS_RDMPTN_CD == 'P')
  avg_amount = mean(purchases$TXN_AMT)
  sum_amount = sum(unique_single$TXN_AMT)
  max_TD=max(unique_single$TXN_DT1)
  min_TD = min(unique_single$TXN_DT1)
  a = append(avg_amount,sum_amount)
  b = append(unique_PAN$pan[i],a)
  c = append(max_TD,min_TD)
  e = append(b,as.character(c))
  Avg_amounts[[i]] = e 
}


sample=as.data.frame(Avg_amounts)
sample1 = as.data.frame(t(sample))
colnames(sample1) = c("pan","avg_amount","sum_amount","Max_TD","Min_TD")
sum(is.na(sample1))
rownames(sample1) = c(1:nrow(sample1))
rownames(sp1) = c(1:nrow(sp1))

# merge function changed
final_independent_IV <- merge(sp1, sample1, by = "pan", all = FALSE)

data_numeric = final_independent_IV[2:39]
data_numeric = apply(data_numeric,2, as.numeric)
final_independent_IV = cbind(final_independent_IV[-c(2:39)],data_numeric)

final_independent_IV$PAMT7 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT8+final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*1000
final_independent_IV$PAMT8 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*100
final_independent_IV$percentage_holding_IV_month = (((final_independent_IV$sum_amount))/final_independent_IV$avg_amount)*100

final_independent_IV$pan=as.character(final_independent_IV$pan)
final_dependent$pan=as.character(final_dependent$pan)

############################################## Adding Categorical Variables to the data #########################################\

analysis_dataset_0_IV$SCHM_ID <- as.character(analysis_dataset_0_IV$SCHM_ID)
analysis_dataset_0_IV$PLAN_ID <- as.character(analysis_dataset_0_IV$PLAN_ID)
analysis_dataset_0_IV$CITY_NM <- as.character(analysis_dataset_0_IV$CITY_NM)
analysis_dataset_0_IV$OCCPTN_TYPE_CD <- as.character(analysis_dataset_0_IV$OCCPTN_TYPE_CD)

categorical_list = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  unique_single_1=unique_single[order(unique_single$TXN_DT),]
  ltd  = unique_single_1[0,]
  td = rbind(ftd,ltd)
  a = td$SCHM_ID
  b = td$PLAN_ID
  c = td$TXN_TYPE_CD
  occupation = td$OCCPTN_TYPE_CD[1]
  city = td$CITY_NM[1]
  e = append(a,b)
  e = append(e,as.character(c))
  e = append(e,occupation)
  e = append(e,as.character(city))
  e = append(unique_PAN$pan[i],e)
  categorical_list[[i]] = e
}
categorical_variables <- as.data.frame(categorical_list)
categorical_variables <- as.data.frame(t(categorical_variables))

sum(is.na(categorical_variables))
colnames(categorical_variables) = c("pan","SCH1","SCH2","SCH3","SCH4","SCH5","SCH6","SCH7","SCH8","SCH9","PL1","PL2","PL3","PL4","PL5","PL6","PL7","PL8","PL9","TX_CD1","TX_CD2","TX_CD3","TX_CD4","TX_CD5","TX_CD6","TX_CD7","TX_CD8","TX_CD9","OCCPTN_TYPE","CITY")
# del2

apply(categorical_variables, 2, as.factor)
colnames(categorical_variables)
sum(is.na(categorical_variables))
row.names(categorical_variables) = c(1:nrow(categorical_variables))
colnames(categorical_variables)

categorical_variables_unique_investors <- nrow(categorical_variables)
categorical_variables_unique_investors
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

final_independent_IV <- merge(final_independent_IV, categorical_variables, by = "pan", all = FALSE)
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

########### removing duplicates
final_independent_IV <- final_independent_IV[!duplicated(final_independent_IV$pan), ]

final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors
######## merging independent and dependent variables

final_IV_DV <- merge(final_independent_IV, final_dependent, by="pan", all=TRUE)
final_IV_DV <- final_IV_DV[complete.cases(final_IV_DV$TX_CD2), ]
final_IV_DV_unique_investors <- nrow(final_IV_DV)
final_IV_DV_unique_investors
metadata_1_0_2 <- final_IV_DV

table(final_IV_DV$silent_investors)
table(metadata_1_0_2$silent_investors)
sum(is.na(metadata_1_0_2))

period_4 <- metadata_1_0_2

TXN_ABOVE
TXN_BELOW_IV
TXN_BELOW
TXN_NUMBER


##################################################################################################################################
################################################        NEXT PERIOD        #######################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

# transactions more than (note: this is linked to the generation of data above a particular transaction count)
TXN_ABOVE <- TXN_NUMBER - 1
TXN_BELOW <- TXN_NUMBER + 8

##################################################################################################################################
analysis_dataset <- raw_data

#remove all the variables
analysis_dataset <- analysis_dataset[-c(1,2,3,4,7,9, 10, 12,15,18,19,20,23,24,27,28,29,25,26,30,31,32,34,39,41)]

analysis_dataset22 <- subset(analysis_dataset, analysis_dataset$segment != 'INDIVIDUAL')
unique(analysis_dataset22$tXx_id_num)
table(analysis_dataset22$segment)

analysis_dataset3 <- as.data.frame(unique(analysis_dataset22$tXx_id_num))
colnames(analysis_dataset3)[1] <- "tXx_id_num"

#here we need to take the opposite of subset http://stackoverflow.com/questions/9852832/r-not-in-subset
analysis_dataset <- subset(analysis_dataset, !(tXx_id_num %in% analysis_dataset3$tXx_id_num))

#subset stats
unique_investors_after_selecting_only_individuals <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_selecting_only_individuals


analysis_dataset$TXN_DT1=(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT=as.numeric(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT4= analysis_dataset$TXN_DT


#removing NAs and spaces
analysis_dataset <- na.omit(analysis_dataset)
colnames(analysis_dataset)[11]="tXx_id_num"

#subset stats
unique_investors_after_removing_NAs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_NAs


#converting transaction types to factor to remove the levels
analysis_dataset$TXN_TYPE_CD <- as.factor(analysis_dataset$TXN_TYPE_CD)
analysis_dataset1 <- subset(analysis_dataset, TXN_TYPE_CD %in% c("ADD", "ADD", "ADDR", "DIR", "DIRR", "DSPI", "IPO", "IPOR", "LTIA", "LTIAR", "LTIN", "LTINR", "LTOF", "LTOF", "LTOFR", "LTOP", "LTOP", "LTOPR", "NEWR", "RED", "RED", "REDR", "SWD", "SWDR", "SWIA", "SWIAR", "SWIN", "SWINR", "SWOF", "SWOF", "SWOFR", "SWOP", "SWOP", "SWOPR", "TRFI", "TRFO", "TRFO", "TRMI", "TRMO"))

analysis_dataset1 <- analysis_dataset1[11]
unique_PAN <- as.data.frame(unique(analysis_dataset1$tXx_id_num))
analysis_dataset <- subset(analysis_dataset, tXx_id_num %in% unique(analysis_dataset1$tXx_id_num))

#subset stats
unique_investors_after_removing_SIPs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_SIPs

#subset 2
#removing 'z' in purchase_redemption_code from the dataframe
analysis_dataset<-analysis_dataset[analysis_dataset$PRCHS_RDMPTN_CD != "Z", ]

#subset stats
unique_investors_after_removing_Z <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_Z


##convert TXN_AMT variables to -ve and +ve according to P/R
analysis_dataset$TXN_AMT<- with(analysis_dataset, ifelse(analysis_dataset$PRCHS_RDMPTN_CD == "R", -TXN_AMT, TXN_AMT))
analysis_dataset$TXN_DT3 <- as.Date(analysis_dataset$TXN_DT1, "%m/%d/%Y")
unique_silent_investors_before_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_before_txn3

#######################################################################################################################11/18/2016
# library(plyr)
train1 = ddply(analysis_dataset,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3 <- nrow(trainC1)
unique_investors_more_than_3

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_more_than_3_less_than_25 <- nrow(trainC1)
unique_investors_more_than_3_less_than_25
########################################################################11/18/2016

analysis_dataset <- merge(analysis_dataset, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset <-subset(analysis_dataset,analysis_dataset$count == 1)
#subset stats
unique_silent_investors_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_txn3

########################################################################################   changes made: 11/23/2016 ##############

# change the study period according to the period considered for the analysis
time_period <- time_period + 1
study_period<- time_period * 30
DV_Month <- max(analysis_dataset$TXN_DT3) - study_period
IV_Month <- DV_Month - 60
IV_Month_select_threshold <-  IV_Month - 90

colnames(analysis_dataset)
analysis_dataset <- analysis_dataset[-c(22,23)]

investors_DV <- subset(analysis_dataset, analysis_dataset$TXN_DT < DV_Month & analysis_dataset$TXN_DT > IV_Month)
investors_IV <- subset(analysis_dataset, analysis_dataset$TXN_DT < (IV_Month-IV_DV_Month_Diff) & analysis_dataset$TXN_DT > (IV_Month_select_threshold-IV_DV_Month_Diff))  

investors_DV2 <- as.data.frame(unique(investors_DV$tXx_id_num))
colnames(investors_DV2)[1] <- "tXx_id_num"


investors_IV2 <- as.data.frame(unique(investors_IV$tXx_id_num))
colnames(investors_IV2)[1] <- "tXx_id_num"

analysis_dataset_0_DV1 <- merge(analysis_dataset, investors_DV2, by = "tXx_id_num", all = FALSE)
analysis_dataset_0_IV1 <- merge(analysis_dataset, investors_IV2, by = "tXx_id_num", all = FALSE)


analysis_dataset_0_DV <- analysis_dataset_0_DV1
analysis_dataset_0_IV <- analysis_dataset_0_IV1

########################################################################################   changes made: 11/23/2016 ##############
##################################################################################################################################

#library(plyr)
train1 = ddply(analysis_dataset_0_DV,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE_DV, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_DV <- nrow(trainC1)
unique_investors_DV

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_DV2 <- nrow(trainC1)
unique_investors_DV2
#######################################################################################################################11/18/2016

analysis_dataset_0_DV <- merge(analysis_dataset_0_DV, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset_0_DV <-subset(analysis_dataset_0_DV,analysis_dataset_0_DV$count == 1)

#subset stats
unique_silent_investors_DV <- sum(count(data.frame(unique(analysis_dataset_0_DV$tXx_id_num)))$freq)
unique_silent_investors_DV
colnames(analysis_dataset_0_DV)
analysis_dataset_0_DV <- analysis_dataset_0_DV[-c(22,23)]
########################################################

##unique_pan generation
unique_PAN <- unique(analysis_dataset_0_DV$tXx_id_num)
unique_PAN <- data.frame(pan=unique(analysis_dataset_0_DV$tXx_id_num))
unique_PAN$pan = as.character(unique_PAN$pan)

# generating dependent variable 
dataframe_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(analysis_dataset_0_DV,analysis_dataset_0_DV$tXx_id_num==unique_PAN$pan[i])
  e$net_amount <- sum(e$TXN_AMT)
  b=subset(e,e$PRCHS_RDMPTN_CD=="P")
  e$avg_sum_purchases<-mean(b$TXN_AMT)
  e$perc_net_amount_with_avg_sum_purchases <-  (e$net_amount/e$avg_sum_purchases)*100
  e$silent_investors <- ifelse(e$perc_net_amount_with_avg_sum_purchases < NET_BALANCE_THRESHOLD_FOR_SILENT_CHURN, 1, 0)
  dataframe_dependent=rbind(e,dataframe_dependent)
}

##################################################################################################################################
final_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(dataframe_dependent,dataframe_dependent$tXx_id_num==unique_PAN$pan[i])
  b=subset(e,e$TXN_DT1== max(e$TXN_DT1))
  final_dependent=rbind(b,final_dependent)
}

colnames(final_dependent)[1]="pan"
# final_dependent <- as.data.frame(unique(final_dependent$pan))
final_dependent <- final_dependent[!duplicated(final_dependent$pan), ]
final_dependent <- final_dependent[,c(1,19,25)]

x <- count(unique(final_dependent$pan))
DV_investors <- sum(x$freq)
DV_investors

########################## metadata generation for independent variables ######### 
########################## metadata generation for independent variables ######### 

#library(plyr)
train1 = ddply(analysis_dataset_0_IV,.(tXx_id_num),nrow)
########################################## 
# change made 11192016

TXN_BELOW_IV <- TXN_ABOVE + 4

train1$count <- ifelse(train1$V1< TXN_BELOW_IV & train1$V1> TXN_ABOVE, 1,0)
########################################## 

trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3_IV <- nrow(trainC1)
unique_investors_more_than_3_IV

# merge function changed
analysis_dataset_0_IV <- merge(analysis_dataset_0_IV, trainC1, by = "tXx_id_num", all = FALSE)

unique_PAN <- as.data.frame(unique(analysis_dataset_0_IV$tXx_id_num))
colnames(unique_PAN)[1] = "pan"
unique_PAN$pan = as.character(unique_PAN$pan)
transaction_values = list()
for(i in 1:nrow(unique_PAN)){
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  print (i)
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  ftd$TXN_Diff <- ftd$TXN_DT - min(ftd$TXN_DT)
  b=c(ftd$TXN_Diff)
  c =c(ftd$TXN_AMT)
  e =append(b,c)
  e = append(e,c(ftd$NAV_UNIT_RT))
  e = append(e,c(ftd$TXN_UNIT_CNT))
  d = append(unique_PAN$pan[i],e)
  transaction_values[[i]]=d
}

max.length <- max(sapply(transaction_values, length))
## Add NA values to list elements
sp <- lapply(transaction_values, function(v) { c(v, rep(NA, max.length-length(v)))})
sp=as.data.frame(transaction_values)

sp1 = as.data.frame(t(sp))
colnames(sp1) = c("pan","TD1","TD2","TD3","TD4","TD5","TD6","TD7","TD8","TD9","AMT1","AMT2","AMT3","AMT4","AMT5","AMT6","AMT7","AMT8","AMT9","NAV1","NAV2","NAV3", "NAV4","NAV5","NAV6","NAV7","NAV8","NAV9","UNIT_CNT1", "UNIT_CNT2", "UNIT_CNT3","UNIT_CNT4","UNIT_CNT5","UNIT_CNT6","UNIT_CNT7","UNIT_CNT8","UNIT_CNT9")
# del

sum(is.na(sp1))
#############################

Avg_amounts = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  purchases =  subset(unique_single,PRCHS_RDMPTN_CD == 'P')
  avg_amount = mean(purchases$TXN_AMT)
  sum_amount = sum(unique_single$TXN_AMT)
  max_TD=max(unique_single$TXN_DT1)
  min_TD = min(unique_single$TXN_DT1)
  a = append(avg_amount,sum_amount)
  b = append(unique_PAN$pan[i],a)
  c = append(max_TD,min_TD)
  e = append(b,as.character(c))
  Avg_amounts[[i]] = e 
}


sample=as.data.frame(Avg_amounts)
sample1 = as.data.frame(t(sample))
colnames(sample1) = c("pan","avg_amount","sum_amount","Max_TD","Min_TD")
sum(is.na(sample1))
rownames(sample1) = c(1:nrow(sample1))
rownames(sp1) = c(1:nrow(sp1))

# merge function changed
final_independent_IV <- merge(sp1, sample1, by = "pan", all = FALSE)

data_numeric = final_independent_IV[2:39]
data_numeric = apply(data_numeric,2, as.numeric)
final_independent_IV = cbind(final_independent_IV[-c(2:39)],data_numeric)

final_independent_IV$PAMT7 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT8+final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*1000
final_independent_IV$PAMT8 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*100
final_independent_IV$percentage_holding_IV_month = (((final_independent_IV$sum_amount))/final_independent_IV$avg_amount)*100

final_independent_IV$pan=as.character(final_independent_IV$pan)
final_dependent$pan=as.character(final_dependent$pan)

############################################## Adding Categorical Variables to the data #########################################\

analysis_dataset_0_IV$SCHM_ID <- as.character(analysis_dataset_0_IV$SCHM_ID)
analysis_dataset_0_IV$PLAN_ID <- as.character(analysis_dataset_0_IV$PLAN_ID)
analysis_dataset_0_IV$CITY_NM <- as.character(analysis_dataset_0_IV$CITY_NM)
analysis_dataset_0_IV$OCCPTN_TYPE_CD <- as.character(analysis_dataset_0_IV$OCCPTN_TYPE_CD)

categorical_list = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  unique_single_1=unique_single[order(unique_single$TXN_DT),]
  ltd  = unique_single_1[0,]
  td = rbind(ftd,ltd)
  a = td$SCHM_ID
  b = td$PLAN_ID
  c = td$TXN_TYPE_CD
  occupation = td$OCCPTN_TYPE_CD[1]
  city = td$CITY_NM[1]
  e = append(a,b)
  e = append(e,as.character(c))
  e = append(e,occupation)
  e = append(e,as.character(city))
  e = append(unique_PAN$pan[i],e)
  categorical_list[[i]] = e
}
categorical_variables <- as.data.frame(categorical_list)
categorical_variables <- as.data.frame(t(categorical_variables))

sum(is.na(categorical_variables))
colnames(categorical_variables) = c("pan","SCH1","SCH2","SCH3","SCH4","SCH5","SCH6","SCH7","SCH8","SCH9","PL1","PL2","PL3","PL4","PL5","PL6","PL7","PL8","PL9","TX_CD1","TX_CD2","TX_CD3","TX_CD4","TX_CD5","TX_CD6","TX_CD7","TX_CD8","TX_CD9","OCCPTN_TYPE","CITY")
# del2

apply(categorical_variables, 2, as.factor)
colnames(categorical_variables)
sum(is.na(categorical_variables))
row.names(categorical_variables) = c(1:nrow(categorical_variables))
colnames(categorical_variables)

categorical_variables_unique_investors <- nrow(categorical_variables)
categorical_variables_unique_investors
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

final_independent_IV <- merge(final_independent_IV, categorical_variables, by = "pan", all = FALSE)
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

########### removing duplicates
final_independent_IV <- final_independent_IV[!duplicated(final_independent_IV$pan), ]

final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors
######## merging independent and dependent variables

final_IV_DV <- merge(final_independent_IV, final_dependent, by="pan", all=TRUE)
final_IV_DV <- final_IV_DV[complete.cases(final_IV_DV$TX_CD2), ]
final_IV_DV_unique_investors <- nrow(final_IV_DV)
final_IV_DV_unique_investors
metadata_1_0_2 <- final_IV_DV

table(final_IV_DV$silent_investors)
table(metadata_1_0_2$silent_investors)
sum(is.na(metadata_1_0_2))

period_5 <- metadata_1_0_2

TXN_ABOVE
TXN_BELOW_IV
TXN_BELOW
TXN_NUMBER


##################################################################################################################################
################################################        NEXT PERIOD        #######################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

# transactions more than (note: this is linked to the generation of data above a particular transaction count)
TXN_ABOVE <- TXN_NUMBER - 1
TXN_BELOW <- TXN_NUMBER + 8

##################################################################################################################################
analysis_dataset <- raw_data

#remove all the variables
analysis_dataset <- analysis_dataset[-c(1,2,3,4,7,9, 10, 12,15,18,19,20,23,24,27,28,29,25,26,30,31,32,34,39,41)]

analysis_dataset22 <- subset(analysis_dataset, analysis_dataset$segment != 'INDIVIDUAL')
unique(analysis_dataset22$tXx_id_num)
table(analysis_dataset22$segment)

analysis_dataset3 <- as.data.frame(unique(analysis_dataset22$tXx_id_num))
colnames(analysis_dataset3)[1] <- "tXx_id_num"

#here we need to take the opposite of subset http://stackoverflow.com/questions/9852832/r-not-in-subset
analysis_dataset <- subset(analysis_dataset, !(tXx_id_num %in% analysis_dataset3$tXx_id_num))

#subset stats
unique_investors_after_selecting_only_individuals <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_selecting_only_individuals


analysis_dataset$TXN_DT1=(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT=as.numeric(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT4= analysis_dataset$TXN_DT


#removing NAs and spaces
analysis_dataset <- na.omit(analysis_dataset)
colnames(analysis_dataset)[11]="tXx_id_num"

#subset stats
unique_investors_after_removing_NAs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_NAs


#converting transaction types to factor to remove the levels
analysis_dataset$TXN_TYPE_CD <- as.factor(analysis_dataset$TXN_TYPE_CD)
analysis_dataset1 <- subset(analysis_dataset, TXN_TYPE_CD %in% c("ADD", "ADD", "ADDR", "DIR", "DIRR", "DSPI", "IPO", "IPOR", "LTIA", "LTIAR", "LTIN", "LTINR", "LTOF", "LTOF", "LTOFR", "LTOP", "LTOP", "LTOPR", "NEWR", "RED", "RED", "REDR", "SWD", "SWDR", "SWIA", "SWIAR", "SWIN", "SWINR", "SWOF", "SWOF", "SWOFR", "SWOP", "SWOP", "SWOPR", "TRFI", "TRFO", "TRFO", "TRMI", "TRMO"))

analysis_dataset1 <- analysis_dataset1[11]
unique_PAN <- as.data.frame(unique(analysis_dataset1$tXx_id_num))
analysis_dataset <- subset(analysis_dataset, tXx_id_num %in% unique(analysis_dataset1$tXx_id_num))

#subset stats
unique_investors_after_removing_SIPs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_SIPs

#subset 2
#removing 'z' in purchase_redemption_code from the dataframe
analysis_dataset<-analysis_dataset[analysis_dataset$PRCHS_RDMPTN_CD != "Z", ]

#subset stats
unique_investors_after_removing_Z <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_Z


##convert TXN_AMT variables to -ve and +ve according to P/R
analysis_dataset$TXN_AMT<- with(analysis_dataset, ifelse(analysis_dataset$PRCHS_RDMPTN_CD == "R", -TXN_AMT, TXN_AMT))
analysis_dataset$TXN_DT3 <- as.Date(analysis_dataset$TXN_DT1, "%m/%d/%Y")
unique_silent_investors_before_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_before_txn3

#######################################################################################################################11/18/2016
# library(plyr)
train1 = ddply(analysis_dataset,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3 <- nrow(trainC1)
unique_investors_more_than_3

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_more_than_3_less_than_25 <- nrow(trainC1)
unique_investors_more_than_3_less_than_25
########################################################################11/18/2016

analysis_dataset <- merge(analysis_dataset, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset <-subset(analysis_dataset,analysis_dataset$count == 1)
#subset stats
unique_silent_investors_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_txn3

########################################################################################   changes made: 11/23/2016 ##############

# change the study period according to the period considered for the analysis
time_period <- time_period + 1
study_period<- time_period * 30
DV_Month <- max(analysis_dataset$TXN_DT3) - study_period
IV_Month <- DV_Month - 60
IV_Month_select_threshold <-  IV_Month - 90

colnames(analysis_dataset)
analysis_dataset <- analysis_dataset[-c(22,23)]

investors_DV <- subset(analysis_dataset, analysis_dataset$TXN_DT < DV_Month & analysis_dataset$TXN_DT > IV_Month)
investors_IV <- subset(analysis_dataset, analysis_dataset$TXN_DT < (IV_Month-IV_DV_Month_Diff) & analysis_dataset$TXN_DT > (IV_Month_select_threshold-IV_DV_Month_Diff))  

investors_DV2 <- as.data.frame(unique(investors_DV$tXx_id_num))
colnames(investors_DV2)[1] <- "tXx_id_num"


investors_IV2 <- as.data.frame(unique(investors_IV$tXx_id_num))
colnames(investors_IV2)[1] <- "tXx_id_num"

analysis_dataset_0_DV1 <- merge(analysis_dataset, investors_DV2, by = "tXx_id_num", all = FALSE)
analysis_dataset_0_IV1 <- merge(analysis_dataset, investors_IV2, by = "tXx_id_num", all = FALSE)


analysis_dataset_0_DV <- analysis_dataset_0_DV1
analysis_dataset_0_IV <- analysis_dataset_0_IV1

########################################################################################   changes made: 11/23/2016 ##############
##################################################################################################################################

#library(plyr)
train1 = ddply(analysis_dataset_0_DV,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE_DV, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_DV <- nrow(trainC1)
unique_investors_DV

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_DV2 <- nrow(trainC1)
unique_investors_DV2
#######################################################################################################################11/18/2016

analysis_dataset_0_DV <- merge(analysis_dataset_0_DV, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset_0_DV <-subset(analysis_dataset_0_DV,analysis_dataset_0_DV$count == 1)

#subset stats
unique_silent_investors_DV <- sum(count(data.frame(unique(analysis_dataset_0_DV$tXx_id_num)))$freq)
unique_silent_investors_DV
colnames(analysis_dataset_0_DV)
analysis_dataset_0_DV <- analysis_dataset_0_DV[-c(22,23)]
########################################################

##unique_pan generation
unique_PAN <- unique(analysis_dataset_0_DV$tXx_id_num)
unique_PAN <- data.frame(pan=unique(analysis_dataset_0_DV$tXx_id_num))
unique_PAN$pan = as.character(unique_PAN$pan)

# generating dependent variable 
dataframe_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(analysis_dataset_0_DV,analysis_dataset_0_DV$tXx_id_num==unique_PAN$pan[i])
  e$net_amount <- sum(e$TXN_AMT)
  b=subset(e,e$PRCHS_RDMPTN_CD=="P")
  e$avg_sum_purchases<-mean(b$TXN_AMT)
  e$perc_net_amount_with_avg_sum_purchases <-  (e$net_amount/e$avg_sum_purchases)*100
  e$silent_investors <- ifelse(e$perc_net_amount_with_avg_sum_purchases < NET_BALANCE_THRESHOLD_FOR_SILENT_CHURN, 1, 0)
  dataframe_dependent=rbind(e,dataframe_dependent)
}

##################################################################################################################################
final_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(dataframe_dependent,dataframe_dependent$tXx_id_num==unique_PAN$pan[i])
  b=subset(e,e$TXN_DT1== max(e$TXN_DT1))
  final_dependent=rbind(b,final_dependent)
}

colnames(final_dependent)[1]="pan"
# final_dependent <- as.data.frame(unique(final_dependent$pan))
final_dependent <- final_dependent[!duplicated(final_dependent$pan), ]
final_dependent <- final_dependent[,c(1,19,25)]

x <- count(unique(final_dependent$pan))
DV_investors <- sum(x$freq)
DV_investors

########################## metadata generation for independent variables ######### 
########################## metadata generation for independent variables ######### 

#library(plyr)
train1 = ddply(analysis_dataset_0_IV,.(tXx_id_num),nrow)
########################################## 
# change made 11192016

TXN_BELOW_IV <- TXN_ABOVE + 4

train1$count <- ifelse(train1$V1< TXN_BELOW_IV & train1$V1> TXN_ABOVE, 1,0)
########################################## 

trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3_IV <- nrow(trainC1)
unique_investors_more_than_3_IV

# merge function changed
analysis_dataset_0_IV <- merge(analysis_dataset_0_IV, trainC1, by = "tXx_id_num", all = FALSE)

unique_PAN <- as.data.frame(unique(analysis_dataset_0_IV$tXx_id_num))
colnames(unique_PAN)[1] = "pan"
unique_PAN$pan = as.character(unique_PAN$pan)
transaction_values = list()
for(i in 1:nrow(unique_PAN)){
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  print (i)
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  ftd$TXN_Diff <- ftd$TXN_DT - min(ftd$TXN_DT)
  b=c(ftd$TXN_Diff)
  c =c(ftd$TXN_AMT)
  e =append(b,c)
  e = append(e,c(ftd$NAV_UNIT_RT))
  e = append(e,c(ftd$TXN_UNIT_CNT))
  d = append(unique_PAN$pan[i],e)
  transaction_values[[i]]=d
}

max.length <- max(sapply(transaction_values, length))
## Add NA values to list elements
sp <- lapply(transaction_values, function(v) { c(v, rep(NA, max.length-length(v)))})
sp=as.data.frame(transaction_values)

sp1 = as.data.frame(t(sp))
colnames(sp1) = c("pan","TD1","TD2","TD3","TD4","TD5","TD6","TD7","TD8","TD9","AMT1","AMT2","AMT3","AMT4","AMT5","AMT6","AMT7","AMT8","AMT9","NAV1","NAV2","NAV3", "NAV4","NAV5","NAV6","NAV7","NAV8","NAV9","UNIT_CNT1", "UNIT_CNT2", "UNIT_CNT3","UNIT_CNT4","UNIT_CNT5","UNIT_CNT6","UNIT_CNT7","UNIT_CNT8","UNIT_CNT9")
# del

sum(is.na(sp1))
#############################

Avg_amounts = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  purchases =  subset(unique_single,PRCHS_RDMPTN_CD == 'P')
  avg_amount = mean(purchases$TXN_AMT)
  sum_amount = sum(unique_single$TXN_AMT)
  max_TD=max(unique_single$TXN_DT1)
  min_TD = min(unique_single$TXN_DT1)
  a = append(avg_amount,sum_amount)
  b = append(unique_PAN$pan[i],a)
  c = append(max_TD,min_TD)
  e = append(b,as.character(c))
  Avg_amounts[[i]] = e 
}


sample=as.data.frame(Avg_amounts)
sample1 = as.data.frame(t(sample))
colnames(sample1) = c("pan","avg_amount","sum_amount","Max_TD","Min_TD")
sum(is.na(sample1))
rownames(sample1) = c(1:nrow(sample1))
rownames(sp1) = c(1:nrow(sp1))

# merge function changed
final_independent_IV <- merge(sp1, sample1, by = "pan", all = FALSE)

data_numeric = final_independent_IV[2:39]
data_numeric = apply(data_numeric,2, as.numeric)
final_independent_IV = cbind(final_independent_IV[-c(2:39)],data_numeric)

final_independent_IV$PAMT7 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT8+final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*1000
final_independent_IV$PAMT8 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*100
final_independent_IV$percentage_holding_IV_month = (((final_independent_IV$sum_amount))/final_independent_IV$avg_amount)*100

final_independent_IV$pan=as.character(final_independent_IV$pan)
final_dependent$pan=as.character(final_dependent$pan)

############################################## Adding Categorical Variables to the data #########################################\

analysis_dataset_0_IV$SCHM_ID <- as.character(analysis_dataset_0_IV$SCHM_ID)
analysis_dataset_0_IV$PLAN_ID <- as.character(analysis_dataset_0_IV$PLAN_ID)
analysis_dataset_0_IV$CITY_NM <- as.character(analysis_dataset_0_IV$CITY_NM)
analysis_dataset_0_IV$OCCPTN_TYPE_CD <- as.character(analysis_dataset_0_IV$OCCPTN_TYPE_CD)

categorical_list = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  unique_single_1=unique_single[order(unique_single$TXN_DT),]
  ltd  = unique_single_1[0,]
  td = rbind(ftd,ltd)
  a = td$SCHM_ID
  b = td$PLAN_ID
  c = td$TXN_TYPE_CD
  occupation = td$OCCPTN_TYPE_CD[1]
  city = td$CITY_NM[1]
  e = append(a,b)
  e = append(e,as.character(c))
  e = append(e,occupation)
  e = append(e,as.character(city))
  e = append(unique_PAN$pan[i],e)
  categorical_list[[i]] = e
}
categorical_variables <- as.data.frame(categorical_list)
categorical_variables <- as.data.frame(t(categorical_variables))

sum(is.na(categorical_variables))
colnames(categorical_variables) = c("pan","SCH1","SCH2","SCH3","SCH4","SCH5","SCH6","SCH7","SCH8","SCH9","PL1","PL2","PL3","PL4","PL5","PL6","PL7","PL8","PL9","TX_CD1","TX_CD2","TX_CD3","TX_CD4","TX_CD5","TX_CD6","TX_CD7","TX_CD8","TX_CD9","OCCPTN_TYPE","CITY")
# del2

apply(categorical_variables, 2, as.factor)
colnames(categorical_variables)
sum(is.na(categorical_variables))
row.names(categorical_variables) = c(1:nrow(categorical_variables))
colnames(categorical_variables)

categorical_variables_unique_investors <- nrow(categorical_variables)
categorical_variables_unique_investors
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

final_independent_IV <- merge(final_independent_IV, categorical_variables, by = "pan", all = FALSE)
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

########### removing duplicates
final_independent_IV <- final_independent_IV[!duplicated(final_independent_IV$pan), ]

final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors
######## merging independent and dependent variables

final_IV_DV <- merge(final_independent_IV, final_dependent, by="pan", all=TRUE)
final_IV_DV <- final_IV_DV[complete.cases(final_IV_DV$TX_CD2), ]
final_IV_DV_unique_investors <- nrow(final_IV_DV)
final_IV_DV_unique_investors
metadata_1_0_2 <- final_IV_DV

table(final_IV_DV$silent_investors)
table(metadata_1_0_2$silent_investors)
sum(is.na(metadata_1_0_2))

period_6 <- metadata_1_0_2

TXN_ABOVE
TXN_BELOW_IV
TXN_BELOW
TXN_NUMBER


##################################################################################################################################
################################################        NEXT PERIOD        #######################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

# transactions more than (note: this is linked to the generation of data above a particular transaction count)
TXN_ABOVE <- TXN_NUMBER - 1
TXN_BELOW <- TXN_NUMBER + 8

##################################################################################################################################
analysis_dataset <- raw_data

#remove all the variables
analysis_dataset <- analysis_dataset[-c(1,2,3,4,7,9, 10, 12,15,18,19,20,23,24,27,28,29,25,26,30,31,32,34,39,41)]

analysis_dataset22 <- subset(analysis_dataset, analysis_dataset$segment != 'INDIVIDUAL')
unique(analysis_dataset22$tXx_id_num)
table(analysis_dataset22$segment)

analysis_dataset3 <- as.data.frame(unique(analysis_dataset22$tXx_id_num))
colnames(analysis_dataset3)[1] <- "tXx_id_num"

#here we need to take the opposite of subset http://stackoverflow.com/questions/9852832/r-not-in-subset
analysis_dataset <- subset(analysis_dataset, !(tXx_id_num %in% analysis_dataset3$tXx_id_num))

#subset stats
unique_investors_after_selecting_only_individuals <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_selecting_only_individuals


analysis_dataset$TXN_DT1=(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT=as.numeric(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT4= analysis_dataset$TXN_DT


#removing NAs and spaces
analysis_dataset <- na.omit(analysis_dataset)
colnames(analysis_dataset)[11]="tXx_id_num"

#subset stats
unique_investors_after_removing_NAs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_NAs


#converting transaction types to factor to remove the levels
analysis_dataset$TXN_TYPE_CD <- as.factor(analysis_dataset$TXN_TYPE_CD)
analysis_dataset1 <- subset(analysis_dataset, TXN_TYPE_CD %in% c("ADD", "ADD", "ADDR", "DIR", "DIRR", "DSPI", "IPO", "IPOR", "LTIA", "LTIAR", "LTIN", "LTINR", "LTOF", "LTOF", "LTOFR", "LTOP", "LTOP", "LTOPR", "NEWR", "RED", "RED", "REDR", "SWD", "SWDR", "SWIA", "SWIAR", "SWIN", "SWINR", "SWOF", "SWOF", "SWOFR", "SWOP", "SWOP", "SWOPR", "TRFI", "TRFO", "TRFO", "TRMI", "TRMO"))

analysis_dataset1 <- analysis_dataset1[11]
unique_PAN <- as.data.frame(unique(analysis_dataset1$tXx_id_num))
analysis_dataset <- subset(analysis_dataset, tXx_id_num %in% unique(analysis_dataset1$tXx_id_num))

#subset stats
unique_investors_after_removing_SIPs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_SIPs

#subset 2
#removing 'z' in purchase_redemption_code from the dataframe
analysis_dataset<-analysis_dataset[analysis_dataset$PRCHS_RDMPTN_CD != "Z", ]

#subset stats
unique_investors_after_removing_Z <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_Z


##convert TXN_AMT variables to -ve and +ve according to P/R
analysis_dataset$TXN_AMT<- with(analysis_dataset, ifelse(analysis_dataset$PRCHS_RDMPTN_CD == "R", -TXN_AMT, TXN_AMT))
analysis_dataset$TXN_DT3 <- as.Date(analysis_dataset$TXN_DT1, "%m/%d/%Y")
unique_silent_investors_before_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_before_txn3

#######################################################################################################################11/18/2016
# library(plyr)
train1 = ddply(analysis_dataset,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3 <- nrow(trainC1)
unique_investors_more_than_3

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_more_than_3_less_than_25 <- nrow(trainC1)
unique_investors_more_than_3_less_than_25
########################################################################11/18/2016

analysis_dataset <- merge(analysis_dataset, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset <-subset(analysis_dataset,analysis_dataset$count == 1)
#subset stats
unique_silent_investors_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_txn3

########################################################################################   changes made: 11/23/2016 ##############

# change the study period according to the period considered for the analysis
time_period <- time_period + 1
study_period<- time_period * 30
DV_Month <- max(analysis_dataset$TXN_DT3) - study_period
IV_Month <- DV_Month - 60
IV_Month_select_threshold <-  IV_Month - 90

colnames(analysis_dataset)
analysis_dataset <- analysis_dataset[-c(22,23)]

investors_DV <- subset(analysis_dataset, analysis_dataset$TXN_DT < DV_Month & analysis_dataset$TXN_DT > IV_Month)
investors_IV <- subset(analysis_dataset, analysis_dataset$TXN_DT < (IV_Month-IV_DV_Month_Diff) & analysis_dataset$TXN_DT > (IV_Month_select_threshold-IV_DV_Month_Diff))  

investors_DV2 <- as.data.frame(unique(investors_DV$tXx_id_num))
colnames(investors_DV2)[1] <- "tXx_id_num"


investors_IV2 <- as.data.frame(unique(investors_IV$tXx_id_num))
colnames(investors_IV2)[1] <- "tXx_id_num"

analysis_dataset_0_DV1 <- merge(analysis_dataset, investors_DV2, by = "tXx_id_num", all = FALSE)
analysis_dataset_0_IV1 <- merge(analysis_dataset, investors_IV2, by = "tXx_id_num", all = FALSE)


analysis_dataset_0_DV <- analysis_dataset_0_DV1
analysis_dataset_0_IV <- analysis_dataset_0_IV1

########################################################################################   changes made: 11/23/2016 ##############
##################################################################################################################################

#library(plyr)
train1 = ddply(analysis_dataset_0_DV,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE_DV, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_DV <- nrow(trainC1)
unique_investors_DV

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_DV2 <- nrow(trainC1)
unique_investors_DV2
#######################################################################################################################11/18/2016

analysis_dataset_0_DV <- merge(analysis_dataset_0_DV, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset_0_DV <-subset(analysis_dataset_0_DV,analysis_dataset_0_DV$count == 1)

#subset stats
unique_silent_investors_DV <- sum(count(data.frame(unique(analysis_dataset_0_DV$tXx_id_num)))$freq)
unique_silent_investors_DV
colnames(analysis_dataset_0_DV)
analysis_dataset_0_DV <- analysis_dataset_0_DV[-c(22,23)]
########################################################

##unique_pan generation
unique_PAN <- unique(analysis_dataset_0_DV$tXx_id_num)
unique_PAN <- data.frame(pan=unique(analysis_dataset_0_DV$tXx_id_num))
unique_PAN$pan = as.character(unique_PAN$pan)

# generating dependent variable 
dataframe_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(analysis_dataset_0_DV,analysis_dataset_0_DV$tXx_id_num==unique_PAN$pan[i])
  e$net_amount <- sum(e$TXN_AMT)
  b=subset(e,e$PRCHS_RDMPTN_CD=="P")
  e$avg_sum_purchases<-mean(b$TXN_AMT)
  e$perc_net_amount_with_avg_sum_purchases <-  (e$net_amount/e$avg_sum_purchases)*100
  e$silent_investors <- ifelse(e$perc_net_amount_with_avg_sum_purchases < NET_BALANCE_THRESHOLD_FOR_SILENT_CHURN, 1, 0)
  dataframe_dependent=rbind(e,dataframe_dependent)
}

##################################################################################################################################
final_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(dataframe_dependent,dataframe_dependent$tXx_id_num==unique_PAN$pan[i])
  b=subset(e,e$TXN_DT1== max(e$TXN_DT1))
  final_dependent=rbind(b,final_dependent)
}

colnames(final_dependent)[1]="pan"
# final_dependent <- as.data.frame(unique(final_dependent$pan))
final_dependent <- final_dependent[!duplicated(final_dependent$pan), ]
final_dependent <- final_dependent[,c(1,19,25)]

x <- count(unique(final_dependent$pan))
DV_investors <- sum(x$freq)
DV_investors

########################## metadata generation for independent variables ######### 
########################## metadata generation for independent variables ######### 

#library(plyr)
train1 = ddply(analysis_dataset_0_IV,.(tXx_id_num),nrow)
########################################## 
# change made 11192016

TXN_BELOW_IV <- TXN_ABOVE + 4

train1$count <- ifelse(train1$V1< TXN_BELOW_IV & train1$V1> TXN_ABOVE, 1,0)
########################################## 

trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3_IV <- nrow(trainC1)
unique_investors_more_than_3_IV

# merge function changed
analysis_dataset_0_IV <- merge(analysis_dataset_0_IV, trainC1, by = "tXx_id_num", all = FALSE)

unique_PAN <- as.data.frame(unique(analysis_dataset_0_IV$tXx_id_num))
colnames(unique_PAN)[1] = "pan"
unique_PAN$pan = as.character(unique_PAN$pan)
transaction_values = list()
for(i in 1:nrow(unique_PAN)){
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  print (i)
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  ftd$TXN_Diff <- ftd$TXN_DT - min(ftd$TXN_DT)
  b=c(ftd$TXN_Diff)
  c =c(ftd$TXN_AMT)
  e =append(b,c)
  e = append(e,c(ftd$NAV_UNIT_RT))
  e = append(e,c(ftd$TXN_UNIT_CNT))
  d = append(unique_PAN$pan[i],e)
  transaction_values[[i]]=d
}

max.length <- max(sapply(transaction_values, length))
## Add NA values to list elements
sp <- lapply(transaction_values, function(v) { c(v, rep(NA, max.length-length(v)))})
sp=as.data.frame(transaction_values)

sp1 = as.data.frame(t(sp))
colnames(sp1) = c("pan","TD1","TD2","TD3","TD4","TD5","TD6","TD7","TD8","TD9","AMT1","AMT2","AMT3","AMT4","AMT5","AMT6","AMT7","AMT8","AMT9","NAV1","NAV2","NAV3", "NAV4","NAV5","NAV6","NAV7","NAV8","NAV9","UNIT_CNT1", "UNIT_CNT2", "UNIT_CNT3","UNIT_CNT4","UNIT_CNT5","UNIT_CNT6","UNIT_CNT7","UNIT_CNT8","UNIT_CNT9")
# del

sum(is.na(sp1))
#############################

Avg_amounts = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  purchases =  subset(unique_single,PRCHS_RDMPTN_CD == 'P')
  avg_amount = mean(purchases$TXN_AMT)
  sum_amount = sum(unique_single$TXN_AMT)
  max_TD=max(unique_single$TXN_DT1)
  min_TD = min(unique_single$TXN_DT1)
  a = append(avg_amount,sum_amount)
  b = append(unique_PAN$pan[i],a)
  c = append(max_TD,min_TD)
  e = append(b,as.character(c))
  Avg_amounts[[i]] = e 
}


sample=as.data.frame(Avg_amounts)
sample1 = as.data.frame(t(sample))
colnames(sample1) = c("pan","avg_amount","sum_amount","Max_TD","Min_TD")
sum(is.na(sample1))
rownames(sample1) = c(1:nrow(sample1))
rownames(sp1) = c(1:nrow(sp1))

# merge function changed
final_independent_IV <- merge(sp1, sample1, by = "pan", all = FALSE)

data_numeric = final_independent_IV[2:39]
data_numeric = apply(data_numeric,2, as.numeric)
final_independent_IV = cbind(final_independent_IV[-c(2:39)],data_numeric)

final_independent_IV$PAMT7 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT8+final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*1000
final_independent_IV$PAMT8 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*100
final_independent_IV$percentage_holding_IV_month = (((final_independent_IV$sum_amount))/final_independent_IV$avg_amount)*100

final_independent_IV$pan=as.character(final_independent_IV$pan)
final_dependent$pan=as.character(final_dependent$pan)

############################################## Adding Categorical Variables to the data #########################################\

analysis_dataset_0_IV$SCHM_ID <- as.character(analysis_dataset_0_IV$SCHM_ID)
analysis_dataset_0_IV$PLAN_ID <- as.character(analysis_dataset_0_IV$PLAN_ID)
analysis_dataset_0_IV$CITY_NM <- as.character(analysis_dataset_0_IV$CITY_NM)
analysis_dataset_0_IV$OCCPTN_TYPE_CD <- as.character(analysis_dataset_0_IV$OCCPTN_TYPE_CD)

categorical_list = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  unique_single_1=unique_single[order(unique_single$TXN_DT),]
  ltd  = unique_single_1[0,]
  td = rbind(ftd,ltd)
  a = td$SCHM_ID
  b = td$PLAN_ID
  c = td$TXN_TYPE_CD
  occupation = td$OCCPTN_TYPE_CD[1]
  city = td$CITY_NM[1]
  e = append(a,b)
  e = append(e,as.character(c))
  e = append(e,occupation)
  e = append(e,as.character(city))
  e = append(unique_PAN$pan[i],e)
  categorical_list[[i]] = e
}
categorical_variables <- as.data.frame(categorical_list)
categorical_variables <- as.data.frame(t(categorical_variables))

sum(is.na(categorical_variables))
colnames(categorical_variables) = c("pan","SCH1","SCH2","SCH3","SCH4","SCH5","SCH6","SCH7","SCH8","SCH9","PL1","PL2","PL3","PL4","PL5","PL6","PL7","PL8","PL9","TX_CD1","TX_CD2","TX_CD3","TX_CD4","TX_CD5","TX_CD6","TX_CD7","TX_CD8","TX_CD9","OCCPTN_TYPE","CITY")
# del2

apply(categorical_variables, 2, as.factor)
colnames(categorical_variables)
sum(is.na(categorical_variables))
row.names(categorical_variables) = c(1:nrow(categorical_variables))
colnames(categorical_variables)

categorical_variables_unique_investors <- nrow(categorical_variables)
categorical_variables_unique_investors
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

final_independent_IV <- merge(final_independent_IV, categorical_variables, by = "pan", all = FALSE)
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

########### removing duplicates
final_independent_IV <- final_independent_IV[!duplicated(final_independent_IV$pan), ]

final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors
######## merging independent and dependent variables

final_IV_DV <- merge(final_independent_IV, final_dependent, by="pan", all=TRUE)
final_IV_DV <- final_IV_DV[complete.cases(final_IV_DV$TX_CD2), ]
final_IV_DV_unique_investors <- nrow(final_IV_DV)
final_IV_DV_unique_investors
metadata_1_0_2 <- final_IV_DV

table(final_IV_DV$silent_investors)
table(metadata_1_0_2$silent_investors)
sum(is.na(metadata_1_0_2))

period_7 <- metadata_1_0_2

TXN_ABOVE
TXN_BELOW_IV
TXN_BELOW
TXN_NUMBER


##################################################################################################################################
################################################        NEXT PERIOD        #######################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

# transactions more than (note: this is linked to the generation of data above a particular transaction count)
TXN_ABOVE <- TXN_NUMBER - 1
TXN_BELOW <- TXN_NUMBER + 8

##################################################################################################################################
analysis_dataset <- raw_data

#remove all the variables
analysis_dataset <- analysis_dataset[-c(1,2,3,4,7,9, 10, 12,15,18,19,20,23,24,27,28,29,25,26,30,31,32,34,39,41)]

analysis_dataset22 <- subset(analysis_dataset, analysis_dataset$segment != 'INDIVIDUAL')
unique(analysis_dataset22$tXx_id_num)
table(analysis_dataset22$segment)

analysis_dataset3 <- as.data.frame(unique(analysis_dataset22$tXx_id_num))
colnames(analysis_dataset3)[1] <- "tXx_id_num"

#here we need to take the opposite of subset http://stackoverflow.com/questions/9852832/r-not-in-subset
analysis_dataset <- subset(analysis_dataset, !(tXx_id_num %in% analysis_dataset3$tXx_id_num))

#subset stats
unique_investors_after_selecting_only_individuals <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_selecting_only_individuals


analysis_dataset$TXN_DT1=(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT=as.numeric(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT4= analysis_dataset$TXN_DT


#removing NAs and spaces
analysis_dataset <- na.omit(analysis_dataset)
colnames(analysis_dataset)[11]="tXx_id_num"

#subset stats
unique_investors_after_removing_NAs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_NAs


#converting transaction types to factor to remove the levels
analysis_dataset$TXN_TYPE_CD <- as.factor(analysis_dataset$TXN_TYPE_CD)
analysis_dataset1 <- subset(analysis_dataset, TXN_TYPE_CD %in% c("ADD", "ADD", "ADDR", "DIR", "DIRR", "DSPI", "IPO", "IPOR", "LTIA", "LTIAR", "LTIN", "LTINR", "LTOF", "LTOF", "LTOFR", "LTOP", "LTOP", "LTOPR", "NEWR", "RED", "RED", "REDR", "SWD", "SWDR", "SWIA", "SWIAR", "SWIN", "SWINR", "SWOF", "SWOF", "SWOFR", "SWOP", "SWOP", "SWOPR", "TRFI", "TRFO", "TRFO", "TRMI", "TRMO"))

analysis_dataset1 <- analysis_dataset1[11]
unique_PAN <- as.data.frame(unique(analysis_dataset1$tXx_id_num))
analysis_dataset <- subset(analysis_dataset, tXx_id_num %in% unique(analysis_dataset1$tXx_id_num))

#subset stats
unique_investors_after_removing_SIPs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_SIPs

#subset 2
#removing 'z' in purchase_redemption_code from the dataframe
analysis_dataset<-analysis_dataset[analysis_dataset$PRCHS_RDMPTN_CD != "Z", ]

#subset stats
unique_investors_after_removing_Z <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_Z


##convert TXN_AMT variables to -ve and +ve according to P/R
analysis_dataset$TXN_AMT<- with(analysis_dataset, ifelse(analysis_dataset$PRCHS_RDMPTN_CD == "R", -TXN_AMT, TXN_AMT))
analysis_dataset$TXN_DT3 <- as.Date(analysis_dataset$TXN_DT1, "%m/%d/%Y")
unique_silent_investors_before_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_before_txn3

#######################################################################################################################11/18/2016
# library(plyr)
train1 = ddply(analysis_dataset,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3 <- nrow(trainC1)
unique_investors_more_than_3

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_more_than_3_less_than_25 <- nrow(trainC1)
unique_investors_more_than_3_less_than_25
########################################################################11/18/2016

analysis_dataset <- merge(analysis_dataset, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset <-subset(analysis_dataset,analysis_dataset$count == 1)
#subset stats
unique_silent_investors_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_txn3

########################################################################################   changes made: 11/23/2016 ##############

# change the study period according to the period considered for the analysis
time_period <- time_period + 1
study_period<- time_period * 30
DV_Month <- max(analysis_dataset$TXN_DT3) - study_period
IV_Month <- DV_Month - 60
IV_Month_select_threshold <-  IV_Month - 90

colnames(analysis_dataset)
analysis_dataset <- analysis_dataset[-c(22,23)]

investors_DV <- subset(analysis_dataset, analysis_dataset$TXN_DT < DV_Month & analysis_dataset$TXN_DT > IV_Month)
investors_IV <- subset(analysis_dataset, analysis_dataset$TXN_DT < (IV_Month-IV_DV_Month_Diff) & analysis_dataset$TXN_DT > (IV_Month_select_threshold-IV_DV_Month_Diff))  

investors_DV2 <- as.data.frame(unique(investors_DV$tXx_id_num))
colnames(investors_DV2)[1] <- "tXx_id_num"


investors_IV2 <- as.data.frame(unique(investors_IV$tXx_id_num))
colnames(investors_IV2)[1] <- "tXx_id_num"

analysis_dataset_0_DV1 <- merge(analysis_dataset, investors_DV2, by = "tXx_id_num", all = FALSE)
analysis_dataset_0_IV1 <- merge(analysis_dataset, investors_IV2, by = "tXx_id_num", all = FALSE)


analysis_dataset_0_DV <- analysis_dataset_0_DV1
analysis_dataset_0_IV <- analysis_dataset_0_IV1

########################################################################################   changes made: 11/23/2016 ##############
##################################################################################################################################

#library(plyr)
train1 = ddply(analysis_dataset_0_DV,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE_DV, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_DV <- nrow(trainC1)
unique_investors_DV

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_DV2 <- nrow(trainC1)
unique_investors_DV2
#######################################################################################################################11/18/2016

analysis_dataset_0_DV <- merge(analysis_dataset_0_DV, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset_0_DV <-subset(analysis_dataset_0_DV,analysis_dataset_0_DV$count == 1)

#subset stats
unique_silent_investors_DV <- sum(count(data.frame(unique(analysis_dataset_0_DV$tXx_id_num)))$freq)
unique_silent_investors_DV
colnames(analysis_dataset_0_DV)
analysis_dataset_0_DV <- analysis_dataset_0_DV[-c(22,23)]
########################################################

##unique_pan generation
unique_PAN <- unique(analysis_dataset_0_DV$tXx_id_num)
unique_PAN <- data.frame(pan=unique(analysis_dataset_0_DV$tXx_id_num))
unique_PAN$pan = as.character(unique_PAN$pan)

# generating dependent variable 
dataframe_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(analysis_dataset_0_DV,analysis_dataset_0_DV$tXx_id_num==unique_PAN$pan[i])
  e$net_amount <- sum(e$TXN_AMT)
  b=subset(e,e$PRCHS_RDMPTN_CD=="P")
  e$avg_sum_purchases<-mean(b$TXN_AMT)
  e$perc_net_amount_with_avg_sum_purchases <-  (e$net_amount/e$avg_sum_purchases)*100
  e$silent_investors <- ifelse(e$perc_net_amount_with_avg_sum_purchases < NET_BALANCE_THRESHOLD_FOR_SILENT_CHURN, 1, 0)
  dataframe_dependent=rbind(e,dataframe_dependent)
}

##################################################################################################################################
final_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(dataframe_dependent,dataframe_dependent$tXx_id_num==unique_PAN$pan[i])
  b=subset(e,e$TXN_DT1== max(e$TXN_DT1))
  final_dependent=rbind(b,final_dependent)
}

colnames(final_dependent)[1]="pan"
# final_dependent <- as.data.frame(unique(final_dependent$pan))
final_dependent <- final_dependent[!duplicated(final_dependent$pan), ]
final_dependent <- final_dependent[,c(1,19,25)]

x <- count(unique(final_dependent$pan))
DV_investors <- sum(x$freq)
DV_investors

########################## metadata generation for independent variables ######### 
########################## metadata generation for independent variables ######### 

#library(plyr)
train1 = ddply(analysis_dataset_0_IV,.(tXx_id_num),nrow)
########################################## 
# change made 11192016

TXN_BELOW_IV <- TXN_ABOVE + 4

train1$count <- ifelse(train1$V1< TXN_BELOW_IV & train1$V1> TXN_ABOVE, 1,0)
########################################## 

trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3_IV <- nrow(trainC1)
unique_investors_more_than_3_IV

# merge function changed
analysis_dataset_0_IV <- merge(analysis_dataset_0_IV, trainC1, by = "tXx_id_num", all = FALSE)

unique_PAN <- as.data.frame(unique(analysis_dataset_0_IV$tXx_id_num))
colnames(unique_PAN)[1] = "pan"
unique_PAN$pan = as.character(unique_PAN$pan)
transaction_values = list()
for(i in 1:nrow(unique_PAN)){
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  print (i)
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  ftd$TXN_Diff <- ftd$TXN_DT - min(ftd$TXN_DT)
  b=c(ftd$TXN_Diff)
  c =c(ftd$TXN_AMT)
  e =append(b,c)
  e = append(e,c(ftd$NAV_UNIT_RT))
  e = append(e,c(ftd$TXN_UNIT_CNT))
  d = append(unique_PAN$pan[i],e)
  transaction_values[[i]]=d
}

max.length <- max(sapply(transaction_values, length))
## Add NA values to list elements
sp <- lapply(transaction_values, function(v) { c(v, rep(NA, max.length-length(v)))})
sp=as.data.frame(transaction_values)

sp1 = as.data.frame(t(sp))
colnames(sp1) = c("pan","TD1","TD2","TD3","TD4","TD5","TD6","TD7","TD8","TD9","AMT1","AMT2","AMT3","AMT4","AMT5","AMT6","AMT7","AMT8","AMT9","NAV1","NAV2","NAV3", "NAV4","NAV5","NAV6","NAV7","NAV8","NAV9","UNIT_CNT1", "UNIT_CNT2", "UNIT_CNT3","UNIT_CNT4","UNIT_CNT5","UNIT_CNT6","UNIT_CNT7","UNIT_CNT8","UNIT_CNT9")
# del

sum(is.na(sp1))
#############################

Avg_amounts = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  purchases =  subset(unique_single,PRCHS_RDMPTN_CD == 'P')
  avg_amount = mean(purchases$TXN_AMT)
  sum_amount = sum(unique_single$TXN_AMT)
  max_TD=max(unique_single$TXN_DT1)
  min_TD = min(unique_single$TXN_DT1)
  a = append(avg_amount,sum_amount)
  b = append(unique_PAN$pan[i],a)
  c = append(max_TD,min_TD)
  e = append(b,as.character(c))
  Avg_amounts[[i]] = e 
}


sample=as.data.frame(Avg_amounts)
sample1 = as.data.frame(t(sample))
colnames(sample1) = c("pan","avg_amount","sum_amount","Max_TD","Min_TD")
sum(is.na(sample1))
rownames(sample1) = c(1:nrow(sample1))
rownames(sp1) = c(1:nrow(sp1))

# merge function changed
final_independent_IV <- merge(sp1, sample1, by = "pan", all = FALSE)

data_numeric = final_independent_IV[2:39]
data_numeric = apply(data_numeric,2, as.numeric)
final_independent_IV = cbind(final_independent_IV[-c(2:39)],data_numeric)

final_independent_IV$PAMT7 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT8+final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*1000
final_independent_IV$PAMT8 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*100
final_independent_IV$percentage_holding_IV_month = (((final_independent_IV$sum_amount))/final_independent_IV$avg_amount)*100

final_independent_IV$pan=as.character(final_independent_IV$pan)
final_dependent$pan=as.character(final_dependent$pan)

############################################## Adding Categorical Variables to the data #########################################\

analysis_dataset_0_IV$SCHM_ID <- as.character(analysis_dataset_0_IV$SCHM_ID)
analysis_dataset_0_IV$PLAN_ID <- as.character(analysis_dataset_0_IV$PLAN_ID)
analysis_dataset_0_IV$CITY_NM <- as.character(analysis_dataset_0_IV$CITY_NM)
analysis_dataset_0_IV$OCCPTN_TYPE_CD <- as.character(analysis_dataset_0_IV$OCCPTN_TYPE_CD)

categorical_list = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  unique_single_1=unique_single[order(unique_single$TXN_DT),]
  ltd  = unique_single_1[0,]
  td = rbind(ftd,ltd)
  a = td$SCHM_ID
  b = td$PLAN_ID
  c = td$TXN_TYPE_CD
  occupation = td$OCCPTN_TYPE_CD[1]
  city = td$CITY_NM[1]
  e = append(a,b)
  e = append(e,as.character(c))
  e = append(e,occupation)
  e = append(e,as.character(city))
  e = append(unique_PAN$pan[i],e)
  categorical_list[[i]] = e
}
categorical_variables <- as.data.frame(categorical_list)
categorical_variables <- as.data.frame(t(categorical_variables))

sum(is.na(categorical_variables))
colnames(categorical_variables) = c("pan","SCH1","SCH2","SCH3","SCH4","SCH5","SCH6","SCH7","SCH8","SCH9","PL1","PL2","PL3","PL4","PL5","PL6","PL7","PL8","PL9","TX_CD1","TX_CD2","TX_CD3","TX_CD4","TX_CD5","TX_CD6","TX_CD7","TX_CD8","TX_CD9","OCCPTN_TYPE","CITY")
# del2

apply(categorical_variables, 2, as.factor)
colnames(categorical_variables)
sum(is.na(categorical_variables))
row.names(categorical_variables) = c(1:nrow(categorical_variables))
colnames(categorical_variables)

categorical_variables_unique_investors <- nrow(categorical_variables)
categorical_variables_unique_investors
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

final_independent_IV <- merge(final_independent_IV, categorical_variables, by = "pan", all = FALSE)
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

########### removing duplicates
final_independent_IV <- final_independent_IV[!duplicated(final_independent_IV$pan), ]

final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors
######## merging independent and dependent variables

final_IV_DV <- merge(final_independent_IV, final_dependent, by="pan", all=TRUE)
final_IV_DV <- final_IV_DV[complete.cases(final_IV_DV$TX_CD2), ]
final_IV_DV_unique_investors <- nrow(final_IV_DV)
final_IV_DV_unique_investors
metadata_1_0_2 <- final_IV_DV

table(final_IV_DV$silent_investors)
table(metadata_1_0_2$silent_investors)
sum(is.na(metadata_1_0_2))

period_8 <- metadata_1_0_2

TXN_ABOVE
TXN_BELOW_IV
TXN_BELOW
TXN_NUMBER


##################################################################################################################################
################################################        NEXT PERIOD        #######################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

# transactions more than (note: this is linked to the generation of data above a particular transaction count)
TXN_ABOVE <- TXN_NUMBER - 1
TXN_BELOW <- TXN_NUMBER + 8

##################################################################################################################################
analysis_dataset <- raw_data

#remove all the variables
analysis_dataset <- analysis_dataset[-c(1,2,3,4,7,9, 10, 12,15,18,19,20,23,24,27,28,29,25,26,30,31,32,34,39,41)]

analysis_dataset22 <- subset(analysis_dataset, analysis_dataset$segment != 'INDIVIDUAL')
unique(analysis_dataset22$tXx_id_num)
table(analysis_dataset22$segment)

analysis_dataset3 <- as.data.frame(unique(analysis_dataset22$tXx_id_num))
colnames(analysis_dataset3)[1] <- "tXx_id_num"

#here we need to take the opposite of subset http://stackoverflow.com/questions/9852832/r-not-in-subset
analysis_dataset <- subset(analysis_dataset, !(tXx_id_num %in% analysis_dataset3$tXx_id_num))

#subset stats
unique_investors_after_selecting_only_individuals <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_selecting_only_individuals


analysis_dataset$TXN_DT1=(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT=as.numeric(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT4= analysis_dataset$TXN_DT


#removing NAs and spaces
analysis_dataset <- na.omit(analysis_dataset)
colnames(analysis_dataset)[11]="tXx_id_num"

#subset stats
unique_investors_after_removing_NAs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_NAs


#converting transaction types to factor to remove the levels
analysis_dataset$TXN_TYPE_CD <- as.factor(analysis_dataset$TXN_TYPE_CD)
analysis_dataset1 <- subset(analysis_dataset, TXN_TYPE_CD %in% c("ADD", "ADD", "ADDR", "DIR", "DIRR", "DSPI", "IPO", "IPOR", "LTIA", "LTIAR", "LTIN", "LTINR", "LTOF", "LTOF", "LTOFR", "LTOP", "LTOP", "LTOPR", "NEWR", "RED", "RED", "REDR", "SWD", "SWDR", "SWIA", "SWIAR", "SWIN", "SWINR", "SWOF", "SWOF", "SWOFR", "SWOP", "SWOP", "SWOPR", "TRFI", "TRFO", "TRFO", "TRMI", "TRMO"))

analysis_dataset1 <- analysis_dataset1[11]
unique_PAN <- as.data.frame(unique(analysis_dataset1$tXx_id_num))
analysis_dataset <- subset(analysis_dataset, tXx_id_num %in% unique(analysis_dataset1$tXx_id_num))

#subset stats
unique_investors_after_removing_SIPs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_SIPs

#subset 2
#removing 'z' in purchase_redemption_code from the dataframe
analysis_dataset<-analysis_dataset[analysis_dataset$PRCHS_RDMPTN_CD != "Z", ]

#subset stats
unique_investors_after_removing_Z <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_Z


##convert TXN_AMT variables to -ve and +ve according to P/R
analysis_dataset$TXN_AMT<- with(analysis_dataset, ifelse(analysis_dataset$PRCHS_RDMPTN_CD == "R", -TXN_AMT, TXN_AMT))
analysis_dataset$TXN_DT3 <- as.Date(analysis_dataset$TXN_DT1, "%m/%d/%Y")
unique_silent_investors_before_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_before_txn3

#######################################################################################################################11/18/2016
# library(plyr)
train1 = ddply(analysis_dataset,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3 <- nrow(trainC1)
unique_investors_more_than_3

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_more_than_3_less_than_25 <- nrow(trainC1)
unique_investors_more_than_3_less_than_25
########################################################################11/18/2016

analysis_dataset <- merge(analysis_dataset, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset <-subset(analysis_dataset,analysis_dataset$count == 1)
#subset stats
unique_silent_investors_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_txn3

########################################################################################   changes made: 11/23/2016 ##############

# change the study period according to the period considered for the analysis
time_period <- time_period + 1
study_period<- time_period * 30
DV_Month <- max(analysis_dataset$TXN_DT3) - study_period
IV_Month <- DV_Month - 60
IV_Month_select_threshold <-  IV_Month - 90

colnames(analysis_dataset)
analysis_dataset <- analysis_dataset[-c(22,23)]

investors_DV <- subset(analysis_dataset, analysis_dataset$TXN_DT < DV_Month & analysis_dataset$TXN_DT > IV_Month)
investors_IV <- subset(analysis_dataset, analysis_dataset$TXN_DT < (IV_Month-IV_DV_Month_Diff) & analysis_dataset$TXN_DT > (IV_Month_select_threshold-IV_DV_Month_Diff))  

investors_DV2 <- as.data.frame(unique(investors_DV$tXx_id_num))
colnames(investors_DV2)[1] <- "tXx_id_num"


investors_IV2 <- as.data.frame(unique(investors_IV$tXx_id_num))
colnames(investors_IV2)[1] <- "tXx_id_num"

analysis_dataset_0_DV1 <- merge(analysis_dataset, investors_DV2, by = "tXx_id_num", all = FALSE)
analysis_dataset_0_IV1 <- merge(analysis_dataset, investors_IV2, by = "tXx_id_num", all = FALSE)


analysis_dataset_0_DV <- analysis_dataset_0_DV1
analysis_dataset_0_IV <- analysis_dataset_0_IV1

########################################################################################   changes made: 11/23/2016 ##############
##################################################################################################################################

#library(plyr)
train1 = ddply(analysis_dataset_0_DV,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE_DV, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_DV <- nrow(trainC1)
unique_investors_DV

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_DV2 <- nrow(trainC1)
unique_investors_DV2
#######################################################################################################################11/18/2016

analysis_dataset_0_DV <- merge(analysis_dataset_0_DV, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset_0_DV <-subset(analysis_dataset_0_DV,analysis_dataset_0_DV$count == 1)

#subset stats
unique_silent_investors_DV <- sum(count(data.frame(unique(analysis_dataset_0_DV$tXx_id_num)))$freq)
unique_silent_investors_DV
colnames(analysis_dataset_0_DV)
analysis_dataset_0_DV <- analysis_dataset_0_DV[-c(22,23)]
########################################################

##unique_pan generation
unique_PAN <- unique(analysis_dataset_0_DV$tXx_id_num)
unique_PAN <- data.frame(pan=unique(analysis_dataset_0_DV$tXx_id_num))
unique_PAN$pan = as.character(unique_PAN$pan)

# generating dependent variable 
dataframe_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(analysis_dataset_0_DV,analysis_dataset_0_DV$tXx_id_num==unique_PAN$pan[i])
  e$net_amount <- sum(e$TXN_AMT)
  b=subset(e,e$PRCHS_RDMPTN_CD=="P")
  e$avg_sum_purchases<-mean(b$TXN_AMT)
  e$perc_net_amount_with_avg_sum_purchases <-  (e$net_amount/e$avg_sum_purchases)*100
  e$silent_investors <- ifelse(e$perc_net_amount_with_avg_sum_purchases < NET_BALANCE_THRESHOLD_FOR_SILENT_CHURN, 1, 0)
  dataframe_dependent=rbind(e,dataframe_dependent)
}

##################################################################################################################################
final_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(dataframe_dependent,dataframe_dependent$tXx_id_num==unique_PAN$pan[i])
  b=subset(e,e$TXN_DT1== max(e$TXN_DT1))
  final_dependent=rbind(b,final_dependent)
}

colnames(final_dependent)[1]="pan"
# final_dependent <- as.data.frame(unique(final_dependent$pan))
final_dependent <- final_dependent[!duplicated(final_dependent$pan), ]
final_dependent <- final_dependent[,c(1,19,25)]

x <- count(unique(final_dependent$pan))
DV_investors <- sum(x$freq)
DV_investors

########################## metadata generation for independent variables ######### 
########################## metadata generation for independent variables ######### 

#library(plyr)
train1 = ddply(analysis_dataset_0_IV,.(tXx_id_num),nrow)
########################################## 
# change made 11192016

TXN_BELOW_IV <- TXN_ABOVE + 4

train1$count <- ifelse(train1$V1< TXN_BELOW_IV & train1$V1> TXN_ABOVE, 1,0)
########################################## 

trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3_IV <- nrow(trainC1)
unique_investors_more_than_3_IV

# merge function changed
analysis_dataset_0_IV <- merge(analysis_dataset_0_IV, trainC1, by = "tXx_id_num", all = FALSE)

unique_PAN <- as.data.frame(unique(analysis_dataset_0_IV$tXx_id_num))
colnames(unique_PAN)[1] = "pan"
unique_PAN$pan = as.character(unique_PAN$pan)
transaction_values = list()
for(i in 1:nrow(unique_PAN)){
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  print (i)
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  ftd$TXN_Diff <- ftd$TXN_DT - min(ftd$TXN_DT)
  b=c(ftd$TXN_Diff)
  c =c(ftd$TXN_AMT)
  e =append(b,c)
  e = append(e,c(ftd$NAV_UNIT_RT))
  e = append(e,c(ftd$TXN_UNIT_CNT))
  d = append(unique_PAN$pan[i],e)
  transaction_values[[i]]=d
}

max.length <- max(sapply(transaction_values, length))
## Add NA values to list elements
sp <- lapply(transaction_values, function(v) { c(v, rep(NA, max.length-length(v)))})
sp=as.data.frame(transaction_values)

sp1 = as.data.frame(t(sp))
colnames(sp1) = c("pan","TD1","TD2","TD3","TD4","TD5","TD6","TD7","TD8","TD9","AMT1","AMT2","AMT3","AMT4","AMT5","AMT6","AMT7","AMT8","AMT9","NAV1","NAV2","NAV3", "NAV4","NAV5","NAV6","NAV7","NAV8","NAV9","UNIT_CNT1", "UNIT_CNT2", "UNIT_CNT3","UNIT_CNT4","UNIT_CNT5","UNIT_CNT6","UNIT_CNT7","UNIT_CNT8","UNIT_CNT9")
# del

sum(is.na(sp1))
#############################

Avg_amounts = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  purchases =  subset(unique_single,PRCHS_RDMPTN_CD == 'P')
  avg_amount = mean(purchases$TXN_AMT)
  sum_amount = sum(unique_single$TXN_AMT)
  max_TD=max(unique_single$TXN_DT1)
  min_TD = min(unique_single$TXN_DT1)
  a = append(avg_amount,sum_amount)
  b = append(unique_PAN$pan[i],a)
  c = append(max_TD,min_TD)
  e = append(b,as.character(c))
  Avg_amounts[[i]] = e 
}


sample=as.data.frame(Avg_amounts)
sample1 = as.data.frame(t(sample))
colnames(sample1) = c("pan","avg_amount","sum_amount","Max_TD","Min_TD")
sum(is.na(sample1))
rownames(sample1) = c(1:nrow(sample1))
rownames(sp1) = c(1:nrow(sp1))

# merge function changed
final_independent_IV <- merge(sp1, sample1, by = "pan", all = FALSE)

data_numeric = final_independent_IV[2:39]
data_numeric = apply(data_numeric,2, as.numeric)
final_independent_IV = cbind(final_independent_IV[-c(2:39)],data_numeric)

final_independent_IV$PAMT7 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT8+final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*1000
final_independent_IV$PAMT8 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*100
final_independent_IV$percentage_holding_IV_month = (((final_independent_IV$sum_amount))/final_independent_IV$avg_amount)*100

final_independent_IV$pan=as.character(final_independent_IV$pan)
final_dependent$pan=as.character(final_dependent$pan)

############################################## Adding Categorical Variables to the data #########################################\

analysis_dataset_0_IV$SCHM_ID <- as.character(analysis_dataset_0_IV$SCHM_ID)
analysis_dataset_0_IV$PLAN_ID <- as.character(analysis_dataset_0_IV$PLAN_ID)
analysis_dataset_0_IV$CITY_NM <- as.character(analysis_dataset_0_IV$CITY_NM)
analysis_dataset_0_IV$OCCPTN_TYPE_CD <- as.character(analysis_dataset_0_IV$OCCPTN_TYPE_CD)

categorical_list = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  unique_single_1=unique_single[order(unique_single$TXN_DT),]
  ltd  = unique_single_1[0,]
  td = rbind(ftd,ltd)
  a = td$SCHM_ID
  b = td$PLAN_ID
  c = td$TXN_TYPE_CD
  occupation = td$OCCPTN_TYPE_CD[1]
  city = td$CITY_NM[1]
  e = append(a,b)
  e = append(e,as.character(c))
  e = append(e,occupation)
  e = append(e,as.character(city))
  e = append(unique_PAN$pan[i],e)
  categorical_list[[i]] = e
}
categorical_variables <- as.data.frame(categorical_list)
categorical_variables <- as.data.frame(t(categorical_variables))

sum(is.na(categorical_variables))
colnames(categorical_variables) = c("pan","SCH1","SCH2","SCH3","SCH4","SCH5","SCH6","SCH7","SCH8","SCH9","PL1","PL2","PL3","PL4","PL5","PL6","PL7","PL8","PL9","TX_CD1","TX_CD2","TX_CD3","TX_CD4","TX_CD5","TX_CD6","TX_CD7","TX_CD8","TX_CD9","OCCPTN_TYPE","CITY")
# del2

apply(categorical_variables, 2, as.factor)
colnames(categorical_variables)
sum(is.na(categorical_variables))
row.names(categorical_variables) = c(1:nrow(categorical_variables))
colnames(categorical_variables)

categorical_variables_unique_investors <- nrow(categorical_variables)
categorical_variables_unique_investors
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

final_independent_IV <- merge(final_independent_IV, categorical_variables, by = "pan", all = FALSE)
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

########### removing duplicates
final_independent_IV <- final_independent_IV[!duplicated(final_independent_IV$pan), ]

final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors
######## merging independent and dependent variables

final_IV_DV <- merge(final_independent_IV, final_dependent, by="pan", all=TRUE)
final_IV_DV <- final_IV_DV[complete.cases(final_IV_DV$TX_CD2), ]
final_IV_DV_unique_investors <- nrow(final_IV_DV)
final_IV_DV_unique_investors
metadata_1_0_2 <- final_IV_DV

table(final_IV_DV$silent_investors)
table(metadata_1_0_2$silent_investors)
sum(is.na(metadata_1_0_2))

period_9 <- metadata_1_0_2

TXN_ABOVE
TXN_BELOW_IV
TXN_BELOW
TXN_NUMBER


##################################################################################################################################
################################################        NEXT PERIOD        #######################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

# transactions more than (note: this is linked to the generation of data above a particular transaction count)
TXN_ABOVE <- TXN_NUMBER - 1
TXN_BELOW <- TXN_NUMBER + 8

##################################################################################################################################
analysis_dataset <- raw_data

#remove all the variables
analysis_dataset <- analysis_dataset[-c(1,2,3,4,7,9, 10, 12,15,18,19,20,23,24,27,28,29,25,26,30,31,32,34,39,41)]

analysis_dataset22 <- subset(analysis_dataset, analysis_dataset$segment != 'INDIVIDUAL')
unique(analysis_dataset22$tXx_id_num)
table(analysis_dataset22$segment)

analysis_dataset3 <- as.data.frame(unique(analysis_dataset22$tXx_id_num))
colnames(analysis_dataset3)[1] <- "tXx_id_num"

#here we need to take the opposite of subset http://stackoverflow.com/questions/9852832/r-not-in-subset
analysis_dataset <- subset(analysis_dataset, !(tXx_id_num %in% analysis_dataset3$tXx_id_num))

#subset stats
unique_investors_after_selecting_only_individuals <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_selecting_only_individuals


analysis_dataset$TXN_DT1=(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT=as.numeric(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT4= analysis_dataset$TXN_DT


#removing NAs and spaces
analysis_dataset <- na.omit(analysis_dataset)
colnames(analysis_dataset)[11]="tXx_id_num"

#subset stats
unique_investors_after_removing_NAs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_NAs


#converting transaction types to factor to remove the levels
analysis_dataset$TXN_TYPE_CD <- as.factor(analysis_dataset$TXN_TYPE_CD)
analysis_dataset1 <- subset(analysis_dataset, TXN_TYPE_CD %in% c("ADD", "ADD", "ADDR", "DIR", "DIRR", "DSPI", "IPO", "IPOR", "LTIA", "LTIAR", "LTIN", "LTINR", "LTOF", "LTOF", "LTOFR", "LTOP", "LTOP", "LTOPR", "NEWR", "RED", "RED", "REDR", "SWD", "SWDR", "SWIA", "SWIAR", "SWIN", "SWINR", "SWOF", "SWOF", "SWOFR", "SWOP", "SWOP", "SWOPR", "TRFI", "TRFO", "TRFO", "TRMI", "TRMO"))

analysis_dataset1 <- analysis_dataset1[11]
unique_PAN <- as.data.frame(unique(analysis_dataset1$tXx_id_num))
analysis_dataset <- subset(analysis_dataset, tXx_id_num %in% unique(analysis_dataset1$tXx_id_num))

#subset stats
unique_investors_after_removing_SIPs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_SIPs

#subset 2
#removing 'z' in purchase_redemption_code from the dataframe
analysis_dataset<-analysis_dataset[analysis_dataset$PRCHS_RDMPTN_CD != "Z", ]

#subset stats
unique_investors_after_removing_Z <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_Z


##convert TXN_AMT variables to -ve and +ve according to P/R
analysis_dataset$TXN_AMT<- with(analysis_dataset, ifelse(analysis_dataset$PRCHS_RDMPTN_CD == "R", -TXN_AMT, TXN_AMT))
analysis_dataset$TXN_DT3 <- as.Date(analysis_dataset$TXN_DT1, "%m/%d/%Y")
unique_silent_investors_before_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_before_txn3

#######################################################################################################################11/18/2016
# library(plyr)
train1 = ddply(analysis_dataset,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3 <- nrow(trainC1)
unique_investors_more_than_3

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_more_than_3_less_than_25 <- nrow(trainC1)
unique_investors_more_than_3_less_than_25
########################################################################11/18/2016

analysis_dataset <- merge(analysis_dataset, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset <-subset(analysis_dataset,analysis_dataset$count == 1)
#subset stats
unique_silent_investors_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_txn3

########################################################################################   changes made: 11/23/2016 ##############

# change the study period according to the period considered for the analysis
time_period <- time_period + 1
study_period<- time_period * 30
DV_Month <- max(analysis_dataset$TXN_DT3) - study_period
IV_Month <- DV_Month - 60
IV_Month_select_threshold <-  IV_Month - 90

colnames(analysis_dataset)
analysis_dataset <- analysis_dataset[-c(22,23)]

investors_DV <- subset(analysis_dataset, analysis_dataset$TXN_DT < DV_Month & analysis_dataset$TXN_DT > IV_Month)
investors_IV <- subset(analysis_dataset, analysis_dataset$TXN_DT < (IV_Month-IV_DV_Month_Diff) & analysis_dataset$TXN_DT > (IV_Month_select_threshold-IV_DV_Month_Diff))  

investors_DV2 <- as.data.frame(unique(investors_DV$tXx_id_num))
colnames(investors_DV2)[1] <- "tXx_id_num"


investors_IV2 <- as.data.frame(unique(investors_IV$tXx_id_num))
colnames(investors_IV2)[1] <- "tXx_id_num"

analysis_dataset_0_DV1 <- merge(analysis_dataset, investors_DV2, by = "tXx_id_num", all = FALSE)
analysis_dataset_0_IV1 <- merge(analysis_dataset, investors_IV2, by = "tXx_id_num", all = FALSE)


analysis_dataset_0_DV <- analysis_dataset_0_DV1
analysis_dataset_0_IV <- analysis_dataset_0_IV1

########################################################################################   changes made: 11/23/2016 ##############
##################################################################################################################################

#library(plyr)
train1 = ddply(analysis_dataset_0_DV,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE_DV, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_DV <- nrow(trainC1)
unique_investors_DV

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_DV2 <- nrow(trainC1)
unique_investors_DV2
#######################################################################################################################11/18/2016

analysis_dataset_0_DV <- merge(analysis_dataset_0_DV, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset_0_DV <-subset(analysis_dataset_0_DV,analysis_dataset_0_DV$count == 1)

#subset stats
unique_silent_investors_DV <- sum(count(data.frame(unique(analysis_dataset_0_DV$tXx_id_num)))$freq)
unique_silent_investors_DV
colnames(analysis_dataset_0_DV)
analysis_dataset_0_DV <- analysis_dataset_0_DV[-c(22,23)]
########################################################

##unique_pan generation
unique_PAN <- unique(analysis_dataset_0_DV$tXx_id_num)
unique_PAN <- data.frame(pan=unique(analysis_dataset_0_DV$tXx_id_num))
unique_PAN$pan = as.character(unique_PAN$pan)

# generating dependent variable 
dataframe_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(analysis_dataset_0_DV,analysis_dataset_0_DV$tXx_id_num==unique_PAN$pan[i])
  e$net_amount <- sum(e$TXN_AMT)
  b=subset(e,e$PRCHS_RDMPTN_CD=="P")
  e$avg_sum_purchases<-mean(b$TXN_AMT)
  e$perc_net_amount_with_avg_sum_purchases <-  (e$net_amount/e$avg_sum_purchases)*100
  e$silent_investors <- ifelse(e$perc_net_amount_with_avg_sum_purchases < NET_BALANCE_THRESHOLD_FOR_SILENT_CHURN, 1, 0)
  dataframe_dependent=rbind(e,dataframe_dependent)
}

##################################################################################################################################
final_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(dataframe_dependent,dataframe_dependent$tXx_id_num==unique_PAN$pan[i])
  b=subset(e,e$TXN_DT1== max(e$TXN_DT1))
  final_dependent=rbind(b,final_dependent)
}

colnames(final_dependent)[1]="pan"
# final_dependent <- as.data.frame(unique(final_dependent$pan))
final_dependent <- final_dependent[!duplicated(final_dependent$pan), ]
final_dependent <- final_dependent[,c(1,19,25)]

x <- count(unique(final_dependent$pan))
DV_investors <- sum(x$freq)
DV_investors

########################## metadata generation for independent variables ######### 
########################## metadata generation for independent variables ######### 

#library(plyr)
train1 = ddply(analysis_dataset_0_IV,.(tXx_id_num),nrow)
########################################## 
# change made 11192016

TXN_BELOW_IV <- TXN_ABOVE + 4

train1$count <- ifelse(train1$V1< TXN_BELOW_IV & train1$V1> TXN_ABOVE, 1,0)
########################################## 

trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3_IV <- nrow(trainC1)
unique_investors_more_than_3_IV

# merge function changed
analysis_dataset_0_IV <- merge(analysis_dataset_0_IV, trainC1, by = "tXx_id_num", all = FALSE)

unique_PAN <- as.data.frame(unique(analysis_dataset_0_IV$tXx_id_num))
colnames(unique_PAN)[1] = "pan"
unique_PAN$pan = as.character(unique_PAN$pan)
transaction_values = list()
for(i in 1:nrow(unique_PAN)){
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  print (i)
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  ftd$TXN_Diff <- ftd$TXN_DT - min(ftd$TXN_DT)
  b=c(ftd$TXN_Diff)
  c =c(ftd$TXN_AMT)
  e =append(b,c)
  e = append(e,c(ftd$NAV_UNIT_RT))
  e = append(e,c(ftd$TXN_UNIT_CNT))
  d = append(unique_PAN$pan[i],e)
  transaction_values[[i]]=d
}

max.length <- max(sapply(transaction_values, length))
## Add NA values to list elements
sp <- lapply(transaction_values, function(v) { c(v, rep(NA, max.length-length(v)))})
sp=as.data.frame(transaction_values)

sp1 = as.data.frame(t(sp))
colnames(sp1) = c("pan","TD1","TD2","TD3","TD4","TD5","TD6","TD7","TD8","TD9","AMT1","AMT2","AMT3","AMT4","AMT5","AMT6","AMT7","AMT8","AMT9","NAV1","NAV2","NAV3", "NAV4","NAV5","NAV6","NAV7","NAV8","NAV9","UNIT_CNT1", "UNIT_CNT2", "UNIT_CNT3","UNIT_CNT4","UNIT_CNT5","UNIT_CNT6","UNIT_CNT7","UNIT_CNT8","UNIT_CNT9")
# del

sum(is.na(sp1))
#############################

Avg_amounts = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  purchases =  subset(unique_single,PRCHS_RDMPTN_CD == 'P')
  avg_amount = mean(purchases$TXN_AMT)
  sum_amount = sum(unique_single$TXN_AMT)
  max_TD=max(unique_single$TXN_DT1)
  min_TD = min(unique_single$TXN_DT1)
  a = append(avg_amount,sum_amount)
  b = append(unique_PAN$pan[i],a)
  c = append(max_TD,min_TD)
  e = append(b,as.character(c))
  Avg_amounts[[i]] = e 
}


sample=as.data.frame(Avg_amounts)
sample1 = as.data.frame(t(sample))
colnames(sample1) = c("pan","avg_amount","sum_amount","Max_TD","Min_TD")
sum(is.na(sample1))
rownames(sample1) = c(1:nrow(sample1))
rownames(sp1) = c(1:nrow(sp1))

# merge function changed
final_independent_IV <- merge(sp1, sample1, by = "pan", all = FALSE)

data_numeric = final_independent_IV[2:39]
data_numeric = apply(data_numeric,2, as.numeric)
final_independent_IV = cbind(final_independent_IV[-c(2:39)],data_numeric)

final_independent_IV$PAMT7 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT8+final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*1000
final_independent_IV$PAMT8 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*100
final_independent_IV$percentage_holding_IV_month = (((final_independent_IV$sum_amount))/final_independent_IV$avg_amount)*100

final_independent_IV$pan=as.character(final_independent_IV$pan)
final_dependent$pan=as.character(final_dependent$pan)

############################################## Adding Categorical Variables to the data #########################################\

analysis_dataset_0_IV$SCHM_ID <- as.character(analysis_dataset_0_IV$SCHM_ID)
analysis_dataset_0_IV$PLAN_ID <- as.character(analysis_dataset_0_IV$PLAN_ID)
analysis_dataset_0_IV$CITY_NM <- as.character(analysis_dataset_0_IV$CITY_NM)
analysis_dataset_0_IV$OCCPTN_TYPE_CD <- as.character(analysis_dataset_0_IV$OCCPTN_TYPE_CD)

categorical_list = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  unique_single_1=unique_single[order(unique_single$TXN_DT),]
  ltd  = unique_single_1[0,]
  td = rbind(ftd,ltd)
  a = td$SCHM_ID
  b = td$PLAN_ID
  c = td$TXN_TYPE_CD
  occupation = td$OCCPTN_TYPE_CD[1]
  city = td$CITY_NM[1]
  e = append(a,b)
  e = append(e,as.character(c))
  e = append(e,occupation)
  e = append(e,as.character(city))
  e = append(unique_PAN$pan[i],e)
  categorical_list[[i]] = e
}
categorical_variables <- as.data.frame(categorical_list)
categorical_variables <- as.data.frame(t(categorical_variables))

sum(is.na(categorical_variables))
colnames(categorical_variables) = c("pan","SCH1","SCH2","SCH3","SCH4","SCH5","SCH6","SCH7","SCH8","SCH9","PL1","PL2","PL3","PL4","PL5","PL6","PL7","PL8","PL9","TX_CD1","TX_CD2","TX_CD3","TX_CD4","TX_CD5","TX_CD6","TX_CD7","TX_CD8","TX_CD9","OCCPTN_TYPE","CITY")
# del2

apply(categorical_variables, 2, as.factor)
colnames(categorical_variables)
sum(is.na(categorical_variables))
row.names(categorical_variables) = c(1:nrow(categorical_variables))
colnames(categorical_variables)

categorical_variables_unique_investors <- nrow(categorical_variables)
categorical_variables_unique_investors
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

final_independent_IV <- merge(final_independent_IV, categorical_variables, by = "pan", all = FALSE)
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

########### removing duplicates
final_independent_IV <- final_independent_IV[!duplicated(final_independent_IV$pan), ]

final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors
######## merging independent and dependent variables

final_IV_DV <- merge(final_independent_IV, final_dependent, by="pan", all=TRUE)
final_IV_DV <- final_IV_DV[complete.cases(final_IV_DV$TX_CD2), ]
final_IV_DV_unique_investors <- nrow(final_IV_DV)
final_IV_DV_unique_investors
metadata_1_0_2 <- final_IV_DV

table(final_IV_DV$silent_investors)
table(metadata_1_0_2$silent_investors)
sum(is.na(metadata_1_0_2))

period_10 <- metadata_1_0_2

TXN_ABOVE
TXN_BELOW_IV
TXN_BELOW
TXN_NUMBER


##################################################################################################################################
################################################        NEXT PERIOD        #######################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

# transactions more than (note: this is linked to the generation of data above a particular transaction count)
TXN_ABOVE <- TXN_NUMBER - 1
TXN_BELOW <- TXN_NUMBER + 8

##################################################################################################################################
analysis_dataset <- raw_data

#remove all the variables
analysis_dataset <- analysis_dataset[-c(1,2,3,4,7,9, 10, 12,15,18,19,20,23,24,27,28,29,25,26,30,31,32,34,39,41)]

analysis_dataset22 <- subset(analysis_dataset, analysis_dataset$segment != 'INDIVIDUAL')
unique(analysis_dataset22$tXx_id_num)
table(analysis_dataset22$segment)

analysis_dataset3 <- as.data.frame(unique(analysis_dataset22$tXx_id_num))
colnames(analysis_dataset3)[1] <- "tXx_id_num"

#here we need to take the opposite of subset http://stackoverflow.com/questions/9852832/r-not-in-subset
analysis_dataset <- subset(analysis_dataset, !(tXx_id_num %in% analysis_dataset3$tXx_id_num))

#subset stats
unique_investors_after_selecting_only_individuals <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_selecting_only_individuals


analysis_dataset$TXN_DT1=(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT=as.numeric(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT4= analysis_dataset$TXN_DT


#removing NAs and spaces
analysis_dataset <- na.omit(analysis_dataset)
colnames(analysis_dataset)[11]="tXx_id_num"

#subset stats
unique_investors_after_removing_NAs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_NAs


#converting transaction types to factor to remove the levels
analysis_dataset$TXN_TYPE_CD <- as.factor(analysis_dataset$TXN_TYPE_CD)
analysis_dataset1 <- subset(analysis_dataset, TXN_TYPE_CD %in% c("ADD", "ADD", "ADDR", "DIR", "DIRR", "DSPI", "IPO", "IPOR", "LTIA", "LTIAR", "LTIN", "LTINR", "LTOF", "LTOF", "LTOFR", "LTOP", "LTOP", "LTOPR", "NEWR", "RED", "RED", "REDR", "SWD", "SWDR", "SWIA", "SWIAR", "SWIN", "SWINR", "SWOF", "SWOF", "SWOFR", "SWOP", "SWOP", "SWOPR", "TRFI", "TRFO", "TRFO", "TRMI", "TRMO"))

analysis_dataset1 <- analysis_dataset1[11]
unique_PAN <- as.data.frame(unique(analysis_dataset1$tXx_id_num))
analysis_dataset <- subset(analysis_dataset, tXx_id_num %in% unique(analysis_dataset1$tXx_id_num))

#subset stats
unique_investors_after_removing_SIPs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_SIPs

#subset 2
#removing 'z' in purchase_redemption_code from the dataframe
analysis_dataset<-analysis_dataset[analysis_dataset$PRCHS_RDMPTN_CD != "Z", ]

#subset stats
unique_investors_after_removing_Z <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_Z


##convert TXN_AMT variables to -ve and +ve according to P/R
analysis_dataset$TXN_AMT<- with(analysis_dataset, ifelse(analysis_dataset$PRCHS_RDMPTN_CD == "R", -TXN_AMT, TXN_AMT))
analysis_dataset$TXN_DT3 <- as.Date(analysis_dataset$TXN_DT1, "%m/%d/%Y")
unique_silent_investors_before_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_before_txn3

#######################################################################################################################11/18/2016
# library(plyr)
train1 = ddply(analysis_dataset,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3 <- nrow(trainC1)
unique_investors_more_than_3

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_more_than_3_less_than_25 <- nrow(trainC1)
unique_investors_more_than_3_less_than_25
########################################################################11/18/2016

analysis_dataset <- merge(analysis_dataset, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset <-subset(analysis_dataset,analysis_dataset$count == 1)
#subset stats
unique_silent_investors_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_txn3

########################################################################################   changes made: 11/23/2016 ##############

# change the study period according to the period considered for the analysis
time_period <- time_period + 1
study_period<- time_period * 30
DV_Month <- max(analysis_dataset$TXN_DT3) - study_period
IV_Month <- DV_Month - 60
IV_Month_select_threshold <-  IV_Month - 90

colnames(analysis_dataset)
analysis_dataset <- analysis_dataset[-c(22,23)]

investors_DV <- subset(analysis_dataset, analysis_dataset$TXN_DT < DV_Month & analysis_dataset$TXN_DT > IV_Month)
investors_IV <- subset(analysis_dataset, analysis_dataset$TXN_DT < (IV_Month-IV_DV_Month_Diff) & analysis_dataset$TXN_DT > (IV_Month_select_threshold-IV_DV_Month_Diff))  

investors_DV2 <- as.data.frame(unique(investors_DV$tXx_id_num))
colnames(investors_DV2)[1] <- "tXx_id_num"


investors_IV2 <- as.data.frame(unique(investors_IV$tXx_id_num))
colnames(investors_IV2)[1] <- "tXx_id_num"

analysis_dataset_0_DV1 <- merge(analysis_dataset, investors_DV2, by = "tXx_id_num", all = FALSE)
analysis_dataset_0_IV1 <- merge(analysis_dataset, investors_IV2, by = "tXx_id_num", all = FALSE)


analysis_dataset_0_DV <- analysis_dataset_0_DV1
analysis_dataset_0_IV <- analysis_dataset_0_IV1

########################################################################################   changes made: 11/23/2016 ##############
##################################################################################################################################

#library(plyr)
train1 = ddply(analysis_dataset_0_DV,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE_DV, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_DV <- nrow(trainC1)
unique_investors_DV

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_DV2 <- nrow(trainC1)
unique_investors_DV2
#######################################################################################################################11/18/2016

analysis_dataset_0_DV <- merge(analysis_dataset_0_DV, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset_0_DV <-subset(analysis_dataset_0_DV,analysis_dataset_0_DV$count == 1)

#subset stats
unique_silent_investors_DV <- sum(count(data.frame(unique(analysis_dataset_0_DV$tXx_id_num)))$freq)
unique_silent_investors_DV
colnames(analysis_dataset_0_DV)
analysis_dataset_0_DV <- analysis_dataset_0_DV[-c(22,23)]
########################################################

##unique_pan generation
unique_PAN <- unique(analysis_dataset_0_DV$tXx_id_num)
unique_PAN <- data.frame(pan=unique(analysis_dataset_0_DV$tXx_id_num))
unique_PAN$pan = as.character(unique_PAN$pan)

# generating dependent variable 
dataframe_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(analysis_dataset_0_DV,analysis_dataset_0_DV$tXx_id_num==unique_PAN$pan[i])
  e$net_amount <- sum(e$TXN_AMT)
  b=subset(e,e$PRCHS_RDMPTN_CD=="P")
  e$avg_sum_purchases<-mean(b$TXN_AMT)
  e$perc_net_amount_with_avg_sum_purchases <-  (e$net_amount/e$avg_sum_purchases)*100
  e$silent_investors <- ifelse(e$perc_net_amount_with_avg_sum_purchases < NET_BALANCE_THRESHOLD_FOR_SILENT_CHURN, 1, 0)
  dataframe_dependent=rbind(e,dataframe_dependent)
}

##################################################################################################################################
final_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(dataframe_dependent,dataframe_dependent$tXx_id_num==unique_PAN$pan[i])
  b=subset(e,e$TXN_DT1== max(e$TXN_DT1))
  final_dependent=rbind(b,final_dependent)
}

colnames(final_dependent)[1]="pan"
# final_dependent <- as.data.frame(unique(final_dependent$pan))
final_dependent <- final_dependent[!duplicated(final_dependent$pan), ]
final_dependent <- final_dependent[,c(1,19,25)]

x <- count(unique(final_dependent$pan))
DV_investors <- sum(x$freq)
DV_investors

########################## metadata generation for independent variables ######### 
########################## metadata generation for independent variables ######### 

#library(plyr)
train1 = ddply(analysis_dataset_0_IV,.(tXx_id_num),nrow)
########################################## 
# change made 11192016

TXN_BELOW_IV <- TXN_ABOVE + 4

train1$count <- ifelse(train1$V1< TXN_BELOW_IV & train1$V1> TXN_ABOVE, 1,0)
########################################## 

trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3_IV <- nrow(trainC1)
unique_investors_more_than_3_IV

# merge function changed
analysis_dataset_0_IV <- merge(analysis_dataset_0_IV, trainC1, by = "tXx_id_num", all = FALSE)

unique_PAN <- as.data.frame(unique(analysis_dataset_0_IV$tXx_id_num))
colnames(unique_PAN)[1] = "pan"
unique_PAN$pan = as.character(unique_PAN$pan)
transaction_values = list()
for(i in 1:nrow(unique_PAN)){
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  print (i)
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  ftd$TXN_Diff <- ftd$TXN_DT - min(ftd$TXN_DT)
  b=c(ftd$TXN_Diff)
  c =c(ftd$TXN_AMT)
  e =append(b,c)
  e = append(e,c(ftd$NAV_UNIT_RT))
  e = append(e,c(ftd$TXN_UNIT_CNT))
  d = append(unique_PAN$pan[i],e)
  transaction_values[[i]]=d
}

max.length <- max(sapply(transaction_values, length))
## Add NA values to list elements
sp <- lapply(transaction_values, function(v) { c(v, rep(NA, max.length-length(v)))})
sp=as.data.frame(transaction_values)

sp1 = as.data.frame(t(sp))
colnames(sp1) = c("pan","TD1","TD2","TD3","TD4","TD5","TD6","TD7","TD8","TD9","AMT1","AMT2","AMT3","AMT4","AMT5","AMT6","AMT7","AMT8","AMT9","NAV1","NAV2","NAV3", "NAV4","NAV5","NAV6","NAV7","NAV8","NAV9","UNIT_CNT1", "UNIT_CNT2", "UNIT_CNT3","UNIT_CNT4","UNIT_CNT5","UNIT_CNT6","UNIT_CNT7","UNIT_CNT8","UNIT_CNT9")
# del

sum(is.na(sp1))
#############################

Avg_amounts = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  purchases =  subset(unique_single,PRCHS_RDMPTN_CD == 'P')
  avg_amount = mean(purchases$TXN_AMT)
  sum_amount = sum(unique_single$TXN_AMT)
  max_TD=max(unique_single$TXN_DT1)
  min_TD = min(unique_single$TXN_DT1)
  a = append(avg_amount,sum_amount)
  b = append(unique_PAN$pan[i],a)
  c = append(max_TD,min_TD)
  e = append(b,as.character(c))
  Avg_amounts[[i]] = e 
}


sample=as.data.frame(Avg_amounts)
sample1 = as.data.frame(t(sample))
colnames(sample1) = c("pan","avg_amount","sum_amount","Max_TD","Min_TD")
sum(is.na(sample1))
rownames(sample1) = c(1:nrow(sample1))
rownames(sp1) = c(1:nrow(sp1))

# merge function changed
final_independent_IV <- merge(sp1, sample1, by = "pan", all = FALSE)

data_numeric = final_independent_IV[2:39]
data_numeric = apply(data_numeric,2, as.numeric)
final_independent_IV = cbind(final_independent_IV[-c(2:39)],data_numeric)

final_independent_IV$PAMT7 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT8+final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*1000
final_independent_IV$PAMT8 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*100
final_independent_IV$percentage_holding_IV_month = (((final_independent_IV$sum_amount))/final_independent_IV$avg_amount)*100

final_independent_IV$pan=as.character(final_independent_IV$pan)
final_dependent$pan=as.character(final_dependent$pan)

############################################## Adding Categorical Variables to the data #########################################\

analysis_dataset_0_IV$SCHM_ID <- as.character(analysis_dataset_0_IV$SCHM_ID)
analysis_dataset_0_IV$PLAN_ID <- as.character(analysis_dataset_0_IV$PLAN_ID)
analysis_dataset_0_IV$CITY_NM <- as.character(analysis_dataset_0_IV$CITY_NM)
analysis_dataset_0_IV$OCCPTN_TYPE_CD <- as.character(analysis_dataset_0_IV$OCCPTN_TYPE_CD)

categorical_list = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  unique_single_1=unique_single[order(unique_single$TXN_DT),]
  ltd  = unique_single_1[0,]
  td = rbind(ftd,ltd)
  a = td$SCHM_ID
  b = td$PLAN_ID
  c = td$TXN_TYPE_CD
  occupation = td$OCCPTN_TYPE_CD[1]
  city = td$CITY_NM[1]
  e = append(a,b)
  e = append(e,as.character(c))
  e = append(e,occupation)
  e = append(e,as.character(city))
  e = append(unique_PAN$pan[i],e)
  categorical_list[[i]] = e
}
categorical_variables <- as.data.frame(categorical_list)
categorical_variables <- as.data.frame(t(categorical_variables))

sum(is.na(categorical_variables))
colnames(categorical_variables) = c("pan","SCH1","SCH2","SCH3","SCH4","SCH5","SCH6","SCH7","SCH8","SCH9","PL1","PL2","PL3","PL4","PL5","PL6","PL7","PL8","PL9","TX_CD1","TX_CD2","TX_CD3","TX_CD4","TX_CD5","TX_CD6","TX_CD7","TX_CD8","TX_CD9","OCCPTN_TYPE","CITY")
# del2

apply(categorical_variables, 2, as.factor)
colnames(categorical_variables)
sum(is.na(categorical_variables))
row.names(categorical_variables) = c(1:nrow(categorical_variables))
colnames(categorical_variables)

categorical_variables_unique_investors <- nrow(categorical_variables)
categorical_variables_unique_investors
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

final_independent_IV <- merge(final_independent_IV, categorical_variables, by = "pan", all = FALSE)
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

########### removing duplicates
final_independent_IV <- final_independent_IV[!duplicated(final_independent_IV$pan), ]

final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors
######## merging independent and dependent variables

final_IV_DV <- merge(final_independent_IV, final_dependent, by="pan", all=TRUE)
final_IV_DV <- final_IV_DV[complete.cases(final_IV_DV$TX_CD2), ]
final_IV_DV_unique_investors <- nrow(final_IV_DV)
final_IV_DV_unique_investors
metadata_1_0_2 <- final_IV_DV

table(final_IV_DV$silent_investors)
table(metadata_1_0_2$silent_investors)
sum(is.na(metadata_1_0_2))

period_11 <- metadata_1_0_2

TXN_ABOVE
TXN_BELOW_IV
TXN_BELOW
TXN_NUMBER


##################################################################################################################################
################################################        NEXT PERIOD        #######################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

# transactions more than (note: this is linked to the generation of data above a particular transaction count)
TXN_ABOVE <- TXN_NUMBER - 1
TXN_BELOW <- TXN_NUMBER + 8

##################################################################################################################################
analysis_dataset <- raw_data

#remove all the variables
analysis_dataset <- analysis_dataset[-c(1,2,3,4,7,9, 10, 12,15,18,19,20,23,24,27,28,29,25,26,30,31,32,34,39,41)]

analysis_dataset22 <- subset(analysis_dataset, analysis_dataset$segment != 'INDIVIDUAL')
unique(analysis_dataset22$tXx_id_num)
table(analysis_dataset22$segment)

analysis_dataset3 <- as.data.frame(unique(analysis_dataset22$tXx_id_num))
colnames(analysis_dataset3)[1] <- "tXx_id_num"

#here we need to take the opposite of subset http://stackoverflow.com/questions/9852832/r-not-in-subset
analysis_dataset <- subset(analysis_dataset, !(tXx_id_num %in% analysis_dataset3$tXx_id_num))

#subset stats
unique_investors_after_selecting_only_individuals <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_selecting_only_individuals


analysis_dataset$TXN_DT1=(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT=as.numeric(analysis_dataset$TXN_DT)
analysis_dataset$TXN_DT4= analysis_dataset$TXN_DT


#removing NAs and spaces
analysis_dataset <- na.omit(analysis_dataset)
colnames(analysis_dataset)[11]="tXx_id_num"

#subset stats
unique_investors_after_removing_NAs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_NAs


#converting transaction types to factor to remove the levels
analysis_dataset$TXN_TYPE_CD <- as.factor(analysis_dataset$TXN_TYPE_CD)
analysis_dataset1 <- subset(analysis_dataset, TXN_TYPE_CD %in% c("ADD", "ADD", "ADDR", "DIR", "DIRR", "DSPI", "IPO", "IPOR", "LTIA", "LTIAR", "LTIN", "LTINR", "LTOF", "LTOF", "LTOFR", "LTOP", "LTOP", "LTOPR", "NEWR", "RED", "RED", "REDR", "SWD", "SWDR", "SWIA", "SWIAR", "SWIN", "SWINR", "SWOF", "SWOF", "SWOFR", "SWOP", "SWOP", "SWOPR", "TRFI", "TRFO", "TRFO", "TRMI", "TRMO"))

analysis_dataset1 <- analysis_dataset1[11]
unique_PAN <- as.data.frame(unique(analysis_dataset1$tXx_id_num))
analysis_dataset <- subset(analysis_dataset, tXx_id_num %in% unique(analysis_dataset1$tXx_id_num))

#subset stats
unique_investors_after_removing_SIPs <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_SIPs

#subset 2
#removing 'z' in purchase_redemption_code from the dataframe
analysis_dataset<-analysis_dataset[analysis_dataset$PRCHS_RDMPTN_CD != "Z", ]

#subset stats
unique_investors_after_removing_Z <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_investors_after_removing_Z


##convert TXN_AMT variables to -ve and +ve according to P/R
analysis_dataset$TXN_AMT<- with(analysis_dataset, ifelse(analysis_dataset$PRCHS_RDMPTN_CD == "R", -TXN_AMT, TXN_AMT))
analysis_dataset$TXN_DT3 <- as.Date(analysis_dataset$TXN_DT1, "%m/%d/%Y")
unique_silent_investors_before_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_before_txn3

#######################################################################################################################11/18/2016
# library(plyr)
train1 = ddply(analysis_dataset,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3 <- nrow(trainC1)
unique_investors_more_than_3

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_more_than_3_less_than_25 <- nrow(trainC1)
unique_investors_more_than_3_less_than_25
########################################################################11/18/2016

analysis_dataset <- merge(analysis_dataset, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset <-subset(analysis_dataset,analysis_dataset$count == 1)
#subset stats
unique_silent_investors_txn3 <- sum(count(data.frame(unique(analysis_dataset$tXx_id_num)))$freq)
unique_silent_investors_txn3

########################################################################################   changes made: 11/23/2016 ##############

# change the study period according to the period considered for the analysis
time_period <- time_period + 1
study_period<- time_period * 30
DV_Month <- max(analysis_dataset$TXN_DT3) - study_period
IV_Month <- DV_Month - 60
IV_Month_select_threshold <-  IV_Month - 90

colnames(analysis_dataset)
analysis_dataset <- analysis_dataset[-c(22,23)]

investors_DV <- subset(analysis_dataset, analysis_dataset$TXN_DT < DV_Month & analysis_dataset$TXN_DT > IV_Month)
investors_IV <- subset(analysis_dataset, analysis_dataset$TXN_DT < (IV_Month-IV_DV_Month_Diff) & analysis_dataset$TXN_DT > (IV_Month_select_threshold-IV_DV_Month_Diff))  

investors_DV2 <- as.data.frame(unique(investors_DV$tXx_id_num))
colnames(investors_DV2)[1] <- "tXx_id_num"


investors_IV2 <- as.data.frame(unique(investors_IV$tXx_id_num))
colnames(investors_IV2)[1] <- "tXx_id_num"

analysis_dataset_0_DV1 <- merge(analysis_dataset, investors_DV2, by = "tXx_id_num", all = FALSE)
analysis_dataset_0_IV1 <- merge(analysis_dataset, investors_IV2, by = "tXx_id_num", all = FALSE)


analysis_dataset_0_DV <- analysis_dataset_0_DV1
analysis_dataset_0_IV <- analysis_dataset_0_IV1

########################################################################################   changes made: 11/23/2016 ##############
##################################################################################################################################

#library(plyr)
train1 = ddply(analysis_dataset_0_DV,.(tXx_id_num),nrow)
# change made
train1$count <- ifelse(TXN_BELOW > train1$V1 & train1$V1 > TXN_ABOVE_DV, 1,0)
trainC1 <-subset(train1,train1$count==1)
unique_investors_DV <- nrow(trainC1)
unique_investors_DV

#######################################################################################################################11/18/2016
trainC1 <- subset(trainC1,trainC1$V1 < 35)
unique_investors_DV2 <- nrow(trainC1)
unique_investors_DV2
#######################################################################################################################11/18/2016

analysis_dataset_0_DV <- merge(analysis_dataset_0_DV, trainC1, by = "tXx_id_num", all = FALSE)
rm(train1,trainC1)
analysis_dataset_0_DV <-subset(analysis_dataset_0_DV,analysis_dataset_0_DV$count == 1)

#subset stats
unique_silent_investors_DV <- sum(count(data.frame(unique(analysis_dataset_0_DV$tXx_id_num)))$freq)
unique_silent_investors_DV
colnames(analysis_dataset_0_DV)
analysis_dataset_0_DV <- analysis_dataset_0_DV[-c(22,23)]
########################################################

##unique_pan generation
unique_PAN <- unique(analysis_dataset_0_DV$tXx_id_num)
unique_PAN <- data.frame(pan=unique(analysis_dataset_0_DV$tXx_id_num))
unique_PAN$pan = as.character(unique_PAN$pan)

# generating dependent variable 
dataframe_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(analysis_dataset_0_DV,analysis_dataset_0_DV$tXx_id_num==unique_PAN$pan[i])
  e$net_amount <- sum(e$TXN_AMT)
  b=subset(e,e$PRCHS_RDMPTN_CD=="P")
  e$avg_sum_purchases<-mean(b$TXN_AMT)
  e$perc_net_amount_with_avg_sum_purchases <-  (e$net_amount/e$avg_sum_purchases)*100
  e$silent_investors <- ifelse(e$perc_net_amount_with_avg_sum_purchases < NET_BALANCE_THRESHOLD_FOR_SILENT_CHURN, 1, 0)
  dataframe_dependent=rbind(e,dataframe_dependent)
}

##################################################################################################################################
final_dependent=data.frame()
for(i in 1:nrow(unique_PAN)){
  print(i)
  e=subset(dataframe_dependent,dataframe_dependent$tXx_id_num==unique_PAN$pan[i])
  b=subset(e,e$TXN_DT1== max(e$TXN_DT1))
  final_dependent=rbind(b,final_dependent)
}

colnames(final_dependent)[1]="pan"
# final_dependent <- as.data.frame(unique(final_dependent$pan))
final_dependent <- final_dependent[!duplicated(final_dependent$pan), ]
final_dependent <- final_dependent[,c(1,19,25)]

x <- count(unique(final_dependent$pan))
DV_investors <- sum(x$freq)
DV_investors

########################## metadata generation for independent variables ######### 
########################## metadata generation for independent variables ######### 

#library(plyr)
train1 = ddply(analysis_dataset_0_IV,.(tXx_id_num),nrow)
########################################## 
# change made 11192016

TXN_BELOW_IV <- TXN_ABOVE + 4

train1$count <- ifelse(train1$V1< TXN_BELOW_IV & train1$V1> TXN_ABOVE, 1,0)
########################################## 

trainC1 <-subset(train1,train1$count==1)
unique_investors_more_than_3_IV <- nrow(trainC1)
unique_investors_more_than_3_IV

# merge function changed
analysis_dataset_0_IV <- merge(analysis_dataset_0_IV, trainC1, by = "tXx_id_num", all = FALSE)

unique_PAN <- as.data.frame(unique(analysis_dataset_0_IV$tXx_id_num))
colnames(unique_PAN)[1] = "pan"
unique_PAN$pan = as.character(unique_PAN$pan)
transaction_values = list()
for(i in 1:nrow(unique_PAN)){
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  print (i)
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  ftd$TXN_Diff <- ftd$TXN_DT - min(ftd$TXN_DT)
  b=c(ftd$TXN_Diff)
  c =c(ftd$TXN_AMT)
  e =append(b,c)
  e = append(e,c(ftd$NAV_UNIT_RT))
  e = append(e,c(ftd$TXN_UNIT_CNT))
  d = append(unique_PAN$pan[i],e)
  transaction_values[[i]]=d
}

max.length <- max(sapply(transaction_values, length))
## Add NA values to list elements
sp <- lapply(transaction_values, function(v) { c(v, rep(NA, max.length-length(v)))})
sp=as.data.frame(transaction_values)

sp1 = as.data.frame(t(sp))
colnames(sp1) = c("pan","TD1","TD2","TD3","TD4","TD5","TD6","TD7","TD8","TD9","AMT1","AMT2","AMT3","AMT4","AMT5","AMT6","AMT7","AMT8","AMT9","NAV1","NAV2","NAV3", "NAV4","NAV5","NAV6","NAV7","NAV8","NAV9","UNIT_CNT1", "UNIT_CNT2", "UNIT_CNT3","UNIT_CNT4","UNIT_CNT5","UNIT_CNT6","UNIT_CNT7","UNIT_CNT8","UNIT_CNT9")
# del

sum(is.na(sp1))
#############################

Avg_amounts = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  purchases =  subset(unique_single,PRCHS_RDMPTN_CD == 'P')
  avg_amount = mean(purchases$TXN_AMT)
  sum_amount = sum(unique_single$TXN_AMT)
  max_TD=max(unique_single$TXN_DT1)
  min_TD = min(unique_single$TXN_DT1)
  a = append(avg_amount,sum_amount)
  b = append(unique_PAN$pan[i],a)
  c = append(max_TD,min_TD)
  e = append(b,as.character(c))
  Avg_amounts[[i]] = e 
}


sample=as.data.frame(Avg_amounts)
sample1 = as.data.frame(t(sample))
colnames(sample1) = c("pan","avg_amount","sum_amount","Max_TD","Min_TD")
sum(is.na(sample1))
rownames(sample1) = c(1:nrow(sample1))
rownames(sp1) = c(1:nrow(sp1))

# merge function changed
final_independent_IV <- merge(sp1, sample1, by = "pan", all = FALSE)

data_numeric = final_independent_IV[2:39]
data_numeric = apply(data_numeric,2, as.numeric)
final_independent_IV = cbind(final_independent_IV[-c(2:39)],data_numeric)

final_independent_IV$PAMT7 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT8+final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*1000
final_independent_IV$PAMT8 = (((final_independent_IV$sum_amount-(final_independent_IV$AMT9)))/final_independent_IV$avg_amount)*100
final_independent_IV$percentage_holding_IV_month = (((final_independent_IV$sum_amount))/final_independent_IV$avg_amount)*100

final_independent_IV$pan=as.character(final_independent_IV$pan)
final_dependent$pan=as.character(final_dependent$pan)

############################################## Adding Categorical Variables to the data #########################################\

analysis_dataset_0_IV$SCHM_ID <- as.character(analysis_dataset_0_IV$SCHM_ID)
analysis_dataset_0_IV$PLAN_ID <- as.character(analysis_dataset_0_IV$PLAN_ID)
analysis_dataset_0_IV$CITY_NM <- as.character(analysis_dataset_0_IV$CITY_NM)
analysis_dataset_0_IV$OCCPTN_TYPE_CD <- as.character(analysis_dataset_0_IV$OCCPTN_TYPE_CD)

categorical_list = list()
for(i in 1:nrow(unique_PAN)){
  print(i)
  unique_single = subset(analysis_dataset_0_IV,tXx_id_num == unique_PAN$pan[i])
  
  unique_single=unique_single[order(unique_single$TXN_DT),]
  ftd  = unique_single[1:TXN_NUMBER,]
  unique_single_1=unique_single[order(unique_single$TXN_DT),]
  ltd  = unique_single_1[0,]
  td = rbind(ftd,ltd)
  a = td$SCHM_ID
  b = td$PLAN_ID
  c = td$TXN_TYPE_CD
  occupation = td$OCCPTN_TYPE_CD[1]
  city = td$CITY_NM[1]
  e = append(a,b)
  e = append(e,as.character(c))
  e = append(e,occupation)
  e = append(e,as.character(city))
  e = append(unique_PAN$pan[i],e)
  categorical_list[[i]] = e
}
categorical_variables <- as.data.frame(categorical_list)
categorical_variables <- as.data.frame(t(categorical_variables))

sum(is.na(categorical_variables))
colnames(categorical_variables) = c("pan","SCH1","SCH2","SCH3","SCH4","SCH5","SCH6","SCH7","SCH8","SCH9","PL1","PL2","PL3","PL4","PL5","PL6","PL7","PL8","PL9","TX_CD1","TX_CD2","TX_CD3","TX_CD4","TX_CD5","TX_CD6","TX_CD7","TX_CD8","TX_CD9","OCCPTN_TYPE","CITY")
# del2

apply(categorical_variables, 2, as.factor)
colnames(categorical_variables)
sum(is.na(categorical_variables))
row.names(categorical_variables) = c(1:nrow(categorical_variables))
colnames(categorical_variables)

categorical_variables_unique_investors <- nrow(categorical_variables)
categorical_variables_unique_investors
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

final_independent_IV <- merge(final_independent_IV, categorical_variables, by = "pan", all = FALSE)
final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors

########### removing duplicates
final_independent_IV <- final_independent_IV[!duplicated(final_independent_IV$pan), ]

final_independent_IV_unique_investors <- nrow(final_independent_IV)
final_independent_IV_unique_investors
######## merging independent and dependent variables

final_IV_DV <- merge(final_independent_IV, final_dependent, by="pan", all=TRUE)
final_IV_DV <- final_IV_DV[complete.cases(final_IV_DV$TX_CD2), ]
final_IV_DV_unique_investors <- nrow(final_IV_DV)
final_IV_DV_unique_investors
metadata_1_0_2 <- final_IV_DV

table(final_IV_DV$silent_investors)
table(metadata_1_0_2$silent_investors)
sum(is.na(metadata_1_0_2))

period_12 <- metadata_1_0_2

TXN_ABOVE
TXN_BELOW_IV
TXN_BELOW
TXN_NUMBER


##################################################################################################################################
################################################        NEXT PERIOD        #######################################################
##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

period_0_2 <- rbind(period_0, period_1, period_2)
period_3_12 <- rbind(period_3, period_4, period_5, period_6, period_7, period_8, period_9, period_10, period_11, period_12)

period_0_2$TXN_DT1 <- as.integer(period_0_2$TXN_DT1)
period_0_2[is.na(period_0_2)] <- 2

period_3_12$TXN_DT1 <- as.integer(period_3_12$TXN_DT1)
period_3_12[is.na(period_3_12)] <- 2



##############################################################################################
##############################################################################################


train <- period_3_12

na.omit(train)
nrow(train)
table(train$silent_investors)

train <- subset(train, train$AMT1 > 0)
nrow(train)
table(train$silent_investors)

train_NEW1 <- subset(train, train$TX_CD1 == "NEW")
train_NEW2 <- subset(train, train$TX_CD1 == "NEW  ")
train_NEW3 <- subset(train, train$TX_CD1 == "IPO")

train <- rbind(train_NEW1, train_NEW2, train_NEW3)
nrow(train)
table(train$silent_investors)

# train_1 <- subset(train, train$silent_investors == 1)
# train_0 <- subset(train, train$silent_investors == 0)
# train <- rbind(train_1, train_1, train_0)
# table(train$silent_investors)

train$plan_subset1 <- with(train, ifelse(as.character(PL1) == as.character(PL2), 1, 0))
train$plan_subset2 <- with(train, ifelse(as.character(PL2) == as.character(PL3), 1, 0))
train$plan_subset3 <- with(train, ifelse(as.character(PL3) == as.character(PL4), 1, 0))
train$plan_subset4 <- with(train, ifelse(as.character(PL4) == as.character(PL5), 1, 0))
train$plan_subset5 <- with(train, ifelse(as.character(PL5) == as.character(PL6), 1, 0))

train$plan_subset_final=train$plan_subset1*train$plan_subset2*train$plan_subset3*train$plan_subset4*train$plan_subset5

train_plan_1=subset(train,train$plan_subset_final ==1) 
train_plan_1=train_plan_1[,-c(76:80)]

# changed till here

train_plan_1$real_change_NAV2 <- ((train_plan_1$NAV2-train_plan_1$NAV1)/(train_plan_1$NAV1))*100
train_plan_1$real_change_NAV3 <- ((train_plan_1$NAV3-train_plan_1$NAV2)/(train_plan_1$NAV2))*100
train_plan_1$real_change_NAV4 <- ((train_plan_1$NAV4-train_plan_1$NAV3)/(train_plan_1$NAV3))*100
train_plan_1$real_change_NAV5 <- ((train_plan_1$NAV5-train_plan_1$NAV4)/(train_plan_1$NAV4))*100
train_plan_1$real_change_NAV6 <- ((train_plan_1$NAV6-train_plan_1$NAV5)/(train_plan_1$NAV5))*100
train_plan_1$real_change_NAV7 <- ((train_plan_1$NAV7-train_plan_1$NAV6)/(train_plan_1$NAV6))*100
train_plan_1$real_change_NAV8 <- ((train_plan_1$NAV8-train_plan_1$NAV7)/(train_plan_1$NAV7))*100
train_plan_1$real_change_NAV9 <- ((train_plan_1$NAV9-train_plan_1$NAV8)/(train_plan_1$NAV8))*100


# train_plan_1$real_chng_TXN1=(((train_plan_1$NAV2[1]-train_plan_1$NAV1[1])/(train_plan_1$NAV1[1]+1))*(train_plan_1$AMT1[1]))
# train_plan_1$real_chng_TXN2=(((train_plan_1$NAV3-train_plan_1$NAV2)/(train_plan_1$NAV2+1))*(train_plan_1$AMT2))

train_plan_1$total_amt_invest <- (train_plan_1$AMT1+train_plan_1$AMT2+train_plan_1$AMT3+train_plan_1$AMT4+train_plan_1$AMT5+train_plan_1$AMT6+train_plan_1$AMT7+train_plan_1$AMT8+train_plan_1$AMT9)
# train_plan_1$total_amt_return <- (train_plan_1$AMT1*train_plan_1$real_change_NAV2)+((train_plan_1$AMT1+train_plan_1$AMT2)*train_plan_1$real_change_NAV3)+((train_plan_1$AMT1+train_plan_1$AMT2+train_plan_1$AMT3)*train_plan_1$real_change_NAV4)+((train_plan_1$AMT1+train_plan_1$AMT2+train_plan_1$AMT3+train_plan_1$AMT4)*train_plan_1$real_change_NAV5)+((train_plan_1$AMT1+train_plan_1$AMT2+train_plan_1$AMT3+train_plan_1$AMT4+train_plan_1$AMT5)*train_plan_1$real_change_NAV6)

train_plan_1$holding_purchases_2 <- ifelse(train_plan_1$AMT2 > 0, (train_plan_1$AMT1+train_plan_1$AMT2), train_plan_1$AMT1)
train_plan_1$holding_purchases_3 <- ifelse(train_plan_1$AMT3 > 0, (train_plan_1$holding_purchases_2+train_plan_1$AMT3), train_plan_1$holding_purchases_2)
train_plan_1$holding_purchases_4 <- ifelse(train_plan_1$AMT4 > 0, (train_plan_1$holding_purchases_3+train_plan_1$AMT4), train_plan_1$holding_purchases_3)
train_plan_1$holding_purchases_5 <- ifelse(train_plan_1$AMT5 > 0, (train_plan_1$holding_purchases_4+train_plan_1$AMT5), train_plan_1$holding_purchases_4)
train_plan_1$holding_purchases_6 <- ifelse(train_plan_1$AMT6 > 0, (train_plan_1$holding_purchases_5+train_plan_1$AMT6), train_plan_1$holding_purchases_5)
train_plan_1$holding_purchases_7 <- ifelse(train_plan_1$AMT7 > 0, (train_plan_1$holding_purchases_6+train_plan_1$AMT7), train_plan_1$holding_purchases_6)
train_plan_1$holding_purchases_8 <- ifelse(train_plan_1$AMT8 > 0, (train_plan_1$holding_purchases_7+train_plan_1$AMT8), train_plan_1$holding_purchases_7)
train_plan_1$holding_purchases_9 <- ifelse(train_plan_1$AMT9 > 0, (train_plan_1$holding_purchases_8+train_plan_1$AMT9), train_plan_1$holding_purchases_8)





train_plan_1$holding_redemption_2 <- ifelse(train_plan_1$AMT2 < 0, (0+train_plan_1$AMT2), 0)
train_plan_1$holding_redemption_3 <- ifelse(train_plan_1$AMT3 < 0, (train_plan_1$holding_redemption_2+train_plan_1$AMT3), train_plan_1$holding_redemption_2)
train_plan_1$holding_redemption_4 <- ifelse(train_plan_1$AMT4 < 0, (train_plan_1$holding_redemption_3+train_plan_1$AMT4), train_plan_1$holding_redemption_3)
train_plan_1$holding_redemption_5 <- ifelse(train_plan_1$AMT5 < 0, (train_plan_1$holding_redemption_4+train_plan_1$AMT5), train_plan_1$holding_redemption_4)
train_plan_1$holding_redemption_6 <- ifelse(train_plan_1$AMT6 < 0, (train_plan_1$holding_redemption_5+train_plan_1$AMT6), train_plan_1$holding_redemption_5)
train_plan_1$holding_redemption_7 <- ifelse(train_plan_1$AMT7 < 0, (train_plan_1$holding_redemption_6+train_plan_1$AMT7), train_plan_1$holding_redemption_6)
train_plan_1$holding_redemption_8 <- ifelse(train_plan_1$AMT8 < 0, (train_plan_1$holding_redemption_7+train_plan_1$AMT8), train_plan_1$holding_redemption_7)
train_plan_1$holding_redemption_9 <- ifelse(train_plan_1$AMT9 < 0, (train_plan_1$holding_redemption_8+train_plan_1$AMT9), train_plan_1$holding_redemption_8)





train_plan_1$sum_purchases <- (train_plan_1$holding_purchases_2+train_plan_1$holding_purchases_3+train_plan_1$holding_purchases_4+train_plan_1$holding_purchases_5+
                                 train_plan_1$holding_purchases_6+train_plan_1$holding_purchases_7 +train_plan_1$holding_purchases_8+train_plan_1$holding_purchases_9)
train_plan_1$sum_redemptions <- (train_plan_1$holding_redemption_2+train_plan_1$holding_redemption_3+train_plan_1$holding_redemption_4+train_plan_1$holding_redemption_5+
                                   train_plan_1$holding_redemption_6+train_plan_1$holding_redemption_7+train_plan_1$holding_redemption_8+train_plan_1$holding_redemption_9)
train_plan_1$total_amt_return_P_R_percentage <- ((train_plan_1$sum_redemptions/train_plan_1$sum_purchases)*100)*-1

train <- train_plan_1

write.csv(train_plan_1,"C:/Users/lakshmirekha.k/Desktop/MF/edelwiess/train_metadata_9_11.csv")

##############################################################################################

dim(evaluation_data)
dim(train)

evaluation_data <- period_0_2

na.omit(evaluation_data)
nrow(evaluation_data)
table(evaluation_data$silent_investors)

evaluation_data <- subset(evaluation_data, evaluation_data$AMT1 > 0)
nrow(evaluation_data)
table(evaluation_data$silent_investors)

evaluation_data_NEW1 <- subset(evaluation_data, evaluation_data$TX_CD1 == "NEW")
evaluation_data_NEW2 <- subset(evaluation_data, evaluation_data$TX_CD1 == "NEW  ")
evaluation_data_NEW3 <- subset(evaluation_data, evaluation_data$TX_CD1 == "IPO")

evaluation_data <- rbind(evaluation_data_NEW1, evaluation_data_NEW2, evaluation_data_NEW3)
nrow(evaluation_data)
table(evaluation_data$silent_investors)

# evaluation_data_1 <- subset(evaluation_data, evaluation_data$silent_investors == 1)
# evaluation_data_0 <- subset(evaluation_data, evaluation_data$silent_investors == 0)
# evaluation_data <- rbind(evaluation_data_1, evaluation_data_1, evaluation_data_0)
# table(evaluation_data$silent_investors)

evaluation_data$plan_subset1 <- with(evaluation_data, ifelse(as.character(PL1) == as.character(PL2), 1, 0))
evaluation_data$plan_subset2 <- with(evaluation_data, ifelse(as.character(PL2) == as.character(PL3), 1, 0))
evaluation_data$plan_subset3 <- with(evaluation_data, ifelse(as.character(PL3) == as.character(PL4), 1, 0))
evaluation_data$plan_subset4 <- with(evaluation_data, ifelse(as.character(PL4) == as.character(PL5), 1, 0))
evaluation_data$plan_subset5 <- with(evaluation_data, ifelse(as.character(PL5) == as.character(PL6), 1, 0))

evaluation_data$plan_subset_final=evaluation_data$plan_subset1*evaluation_data$plan_subset2*evaluation_data$plan_subset3*evaluation_data$plan_subset4*evaluation_data$plan_subset5

evaluation_data_plan_1=subset(evaluation_data,evaluation_data$plan_subset_final ==1) 
evaluation_data_plan_1=evaluation_data_plan_1[,-c(76:80)]

# changed till here

evaluation_data_plan_1$real_change_NAV2 <- ((evaluation_data_plan_1$NAV2-evaluation_data_plan_1$NAV1)/(evaluation_data_plan_1$NAV1))*100
evaluation_data_plan_1$real_change_NAV3 <- ((evaluation_data_plan_1$NAV3-evaluation_data_plan_1$NAV2)/(evaluation_data_plan_1$NAV2))*100
evaluation_data_plan_1$real_change_NAV4 <- ((evaluation_data_plan_1$NAV4-evaluation_data_plan_1$NAV3)/(evaluation_data_plan_1$NAV3))*100
evaluation_data_plan_1$real_change_NAV5 <- ((evaluation_data_plan_1$NAV5-evaluation_data_plan_1$NAV4)/(evaluation_data_plan_1$NAV4))*100
evaluation_data_plan_1$real_change_NAV6 <- ((evaluation_data_plan_1$NAV6-evaluation_data_plan_1$NAV5)/(evaluation_data_plan_1$NAV5))*100
evaluation_data_plan_1$real_change_NAV7 <- ((evaluation_data_plan_1$NAV7-evaluation_data_plan_1$NAV6)/(evaluation_data_plan_1$NAV6))*100
evaluation_data_plan_1$real_change_NAV8 <- ((evaluation_data_plan_1$NAV8-evaluation_data_plan_1$NAV7)/(evaluation_data_plan_1$NAV7))*100
evaluation_data_plan_1$real_change_NAV9 <- ((evaluation_data_plan_1$NAV9-evaluation_data_plan_1$NAV8)/(evaluation_data_plan_1$NAV8))*100




# evaluation_data_plan_1$real_chng_TXN1=(((evaluation_data_plan_1$NAV2[1]-evaluation_data_plan_1$NAV1[1])/(evaluation_data_plan_1$NAV1[1]+1))*(evaluation_data_plan_1$AMT1[1]))
# evaluation_data_plan_1$real_chng_TXN2=(((evaluation_data_plan_1$NAV3-evaluation_data_plan_1$NAV2)/(evaluation_data_plan_1$NAV2+1))*(evaluation_data_plan_1$AMT2))

evaluation_data_plan_1$total_amt_invest <- (evaluation_data_plan_1$AMT1+evaluation_data_plan_1$AMT2+evaluation_data_plan_1$AMT3+
                                              evaluation_data_plan_1$AMT4+evaluation_data_plan_1$AMT5+evaluation_data_plan_1$AMT6+
                                              evaluation_data_plan_1$real_change_NAV7+evaluation_data_plan_1$real_change_NAV8+evaluation_data_plan_1$real_change_NAV9)
# evaluation_data_plan_1$total_amt_return <- (evaluation_data_plan_1$AMT1*evaluation_data_plan_1$real_change_NAV2)+((evaluation_data_plan_1$AMT1+evaluation_data_plan_1$AMT2)*evaluation_data_plan_1$real_change_NAV3)+((evaluation_data_plan_1$AMT1+evaluation_data_plan_1$AMT2+evaluation_data_plan_1$AMT3)*evaluation_data_plan_1$real_change_NAV4)+((evaluation_data_plan_1$AMT1+evaluation_data_plan_1$AMT2+evaluation_data_plan_1$AMT3+evaluation_data_plan_1$AMT4)*evaluation_data_plan_1$real_change_NAV5)+((evaluation_data_plan_1$AMT1+evaluation_data_plan_1$AMT2+evaluation_data_plan_1$AMT3+evaluation_data_plan_1$AMT4+evaluation_data_plan_1$AMT5)*evaluation_data_plan_1$real_change_NAV6)

evaluation_data_plan_1$holding_purchases_2 <- ifelse(evaluation_data_plan_1$AMT2 > 0, (evaluation_data_plan_1$AMT1+evaluation_data_plan_1$AMT2), evaluation_data_plan_1$AMT1)
evaluation_data_plan_1$holding_purchases_3 <- ifelse(evaluation_data_plan_1$AMT3 > 0, (evaluation_data_plan_1$holding_purchases_2+evaluation_data_plan_1$AMT3), evaluation_data_plan_1$holding_purchases_2)
evaluation_data_plan_1$holding_purchases_4 <- ifelse(evaluation_data_plan_1$AMT4 > 0, (evaluation_data_plan_1$holding_purchases_3+evaluation_data_plan_1$AMT4), evaluation_data_plan_1$holding_purchases_3)
evaluation_data_plan_1$holding_purchases_5 <- ifelse(evaluation_data_plan_1$AMT5 > 0, (evaluation_data_plan_1$holding_purchases_4+evaluation_data_plan_1$AMT5), evaluation_data_plan_1$holding_purchases_4)
evaluation_data_plan_1$holding_purchases_6 <- ifelse(evaluation_data_plan_1$AMT6 > 0, (evaluation_data_plan_1$holding_purchases_5+evaluation_data_plan_1$AMT6), evaluation_data_plan_1$holding_purchases_5)
evaluation_data_plan_1$holding_purchases_7 <- ifelse(evaluation_data_plan_1$AMT7 > 0, (evaluation_data_plan_1$holding_purchases_6+evaluation_data_plan_1$AMT7), evaluation_data_plan_1$holding_purchases_6)
evaluation_data_plan_1$holding_purchases_8 <- ifelse(evaluation_data_plan_1$AMT8 > 0, (evaluation_data_plan_1$holding_purchases_7+evaluation_data_plan_1$AMT8), evaluation_data_plan_1$holding_purchases_7)
evaluation_data_plan_1$holding_purchases_9 <- ifelse(evaluation_data_plan_1$AMT9 > 0, (evaluation_data_plan_1$holding_purchases_8+evaluation_data_plan_1$AMT9), evaluation_data_plan_1$holding_purchases_8)




evaluation_data_plan_1$holding_redemption_2 <- ifelse(evaluation_data_plan_1$AMT2 < 0, (0+evaluation_data_plan_1$AMT2), 0)
evaluation_data_plan_1$holding_redemption_3 <- ifelse(evaluation_data_plan_1$AMT3 < 0, (evaluation_data_plan_1$holding_redemption_2+evaluation_data_plan_1$AMT3), evaluation_data_plan_1$holding_redemption_2)
evaluation_data_plan_1$holding_redemption_4 <- ifelse(evaluation_data_plan_1$AMT4 < 0, (evaluation_data_plan_1$holding_redemption_3+evaluation_data_plan_1$AMT4), evaluation_data_plan_1$holding_redemption_3)
evaluation_data_plan_1$holding_redemption_5 <- ifelse(evaluation_data_plan_1$AMT5 < 0, (evaluation_data_plan_1$holding_redemption_4+evaluation_data_plan_1$AMT5), evaluation_data_plan_1$holding_redemption_4)
evaluation_data_plan_1$holding_redemption_6 <- ifelse(evaluation_data_plan_1$AMT6 < 0, (evaluation_data_plan_1$holding_redemption_5+evaluation_data_plan_1$AMT6), evaluation_data_plan_1$holding_redemption_5)
evaluation_data_plan_1$holding_redemption_7 <- ifelse(evaluation_data_plan_1$AMT7 < 0, (evaluation_data_plan_1$holding_redemption_6+evaluation_data_plan_1$AMT7), evaluation_data_plan_1$holding_redemption_6)
evaluation_data_plan_1$holding_redemption_8 <- ifelse(evaluation_data_plan_1$AMT8 < 0, (evaluation_data_plan_1$holding_redemption_7+evaluation_data_plan_1$AMT8), evaluation_data_plan_1$holding_redemption_7)
evaluation_data_plan_1$holding_redemption_9 <- ifelse(evaluation_data_plan_1$AMT9 < 0, (evaluation_data_plan_1$holding_redemption_8+evaluation_data_plan_1$AMT9), evaluation_data_plan_1$holding_redemption_8)







evaluation_data_plan_1$sum_purchases <- (evaluation_data_plan_1$holding_purchases_2+evaluation_data_plan_1$holding_purchases_3+evaluation_data_plan_1$holding_purchases_4+
                                           evaluation_data_plan_1$holding_purchases_5+evaluation_data_plan_1$holding_purchases_6+evaluation_data_plan_1$holding_purchases_7+
                                           evaluation_data_plan_1$holding_purchases_8+evaluation_data_plan_1$holding_purchases_9)
evaluation_data_plan_1$sum_redemptions <- (evaluation_data_plan_1$holding_redemption_2+evaluation_data_plan_1$holding_redemption_3+evaluation_data_plan_1$holding_redemption_4+
                                             evaluation_data_plan_1$holding_redemption_5+evaluation_data_plan_1$holding_redemption_6+evaluation_data_plan_1$holding_redemption_7+
                                             evaluation_data_plan_1$holding_redemption_8+evaluation_data_plan_1$holding_redemption_9)
evaluation_data_plan_1$total_amt_return_P_R_percentage <- ((evaluation_data_plan_1$sum_redemptions/evaluation_data_plan_1$sum_purchases)*100)*-1

evaluation_data <- evaluation_data_plan_1

##############################################################################################
levels(evaluation_data$SCH1) <- levels(train$SCH1)
levels(evaluation_data$SCH2) <- levels(train$SCH2)
levels(evaluation_data$SCH3) <- levels(train$SCH3)
levels(evaluation_data$PL1) <- levels(train$PL1)
levels(evaluation_data$PL2) <- levels(train$PL2)
levels(evaluation_data$PL3) <- levels(train$PL3)
levels(evaluation_data$TX_CD1) <- levels(train$TX_CD1)
levels(evaluation_data$TX_CD2) <- levels(train$TX_CD2)
levels(evaluation_data$TX_CD3) <- levels(train$TX_CD3)
levels(evaluation_data$OCCPTN_TYPE) <- levels(train$OCCPTN_TYPE)

levels(evaluation_data$TD1) <- levels(train$TD1)
levels(evaluation_data$TD2) <- levels(train$TD2)
levels(evaluation_data$TD3) <- levels(train$TD3)
levels(evaluation_data$AMT1) <- levels(train$AMT1)
levels(evaluation_data$AMT2) <- levels(train$AMT2)
levels(evaluation_data$AMT3) <- levels(train$AMT3)
levels(evaluation_data$NAV1) <- levels(train$NAV1)
levels(evaluation_data$NAV2) <- levels(train$NAV2)
levels(evaluation_data$NAV3) <- levels(train$NAV3)
levels(evaluation_data$UNIT_CNT1) <- levels(train$UNIT_CNT1)
levels(evaluation_data$UNIT_CNT2) <- levels(train$UNIT_CNT2)
levels(evaluation_data$UNIT_CNT3) <- levels(train$UNIT_CNT3)
levels(evaluation_data$avg_amount) <- levels(train$avg_amount)
levels(evaluation_data$sum_amount) <- levels(train$sum_amount)
levels(evaluation_data$CITY) <- levels(train$CITY)
levels(evaluation_data$real_chng_TXN1) <- levels(train$real_chng_TXN1)
levels(evaluation_data$real_chng_TXN2) <- levels(train$real_chng_TXN2)
levels(evaluation_data$T_amnt_invstd) <- levels(train$T_amnt_invstd)
levels(evaluation_data$T_amnt_returnd) <- levels(train$T_amnt_returnd)
levels(evaluation_data$ROI) <- levels(train$ROI)

# evaluation_data$subset_TXNCD2 <- ifelse(evaluation_data$TX_CD2 == "REDR", 1, 0)
# evaluation_data <- subset(evaluation_data, evaluation_data$subset_TXNCD2 == 0)
# evaluation_data$subset_TXNCD2 <- ifelse(evaluation_data$TX_CD2 == "TRFO", 1, 0)
# evaluation_data <- subset(evaluation_data, evaluation_data$subset_TXNCD2 == 0)

# evaluation_data <- evaluation_data[-c(41)]

##############################################################################################
# 
# library(rpart.plot)
# library(caret)
# library(party)
# library(e1071)
# library(rpart)
# library(tree)
# 
# train$CITY <- as.factor(train$CITY)
# colnames(train)
# ##############################################################################################
# 
# model_tree <- tree(silent_investors ~ NAV9 + percentage_holding_IV_month + PAMT7 + PAMT8 + NAV8 + real_change_NAV9 + real_change_NAV5, data = train[-c(1:3, 74, 73)],method="class")
# # model_DT <- prune(model_DT, cp= 0.224)
# plot(model_tree)
# text(model_tree, pretty = 50)
# # evaluation_data_high <- High[evaluation_data]
# tree_pred <- as.data.frame(predict(model_tree, newdata=evaluation_data))
# colnames(tree_pred)[1] <- "predicted"
# 
# # tuning parameters (weights)
# 
# max_1 <- 0.25
# min_2 <- max_1
# max_2 <- 1.8
# min_3 <- max_2
# 
# tree_pred$predicted2 <-  ifelse(tree_pred$predicted < max_1, 0, tree_pred$predicted)
# tree_pred$predicted2 <-  ifelse(tree_pred$predicted > min_2 & tree_pred$predicted < max_2,  1, tree_pred$predicted2)
# tree_pred$predicted2 <-  ifelse(tree_pred$predicted > min_3, 2, tree_pred$predicted2)
# tree_pred2 <- as.data.frame(cbind(evaluation_data$silent_investors, tree_pred$predicted2))
# 
# colnames(tree_pred2)[1] <- "actual"
# colnames(tree_pred2)[2] <- "predicted"
# 
# overall_error_txn_6_8_without_converting_0_and_2_to_0 <- 1- (sum((tree_pred2$actual) == (tree_pred2$predicted))/nrow(tree_pred2))
# overall_error_txn_6_8_without_converting_0_and_2_to_0
# 
# 
# tree_pred3 <- subset(tree_pred2, tree_pred2$actual == 1)
# classification_error_1_txn_6_8 <- 1- (sum((tree_pred3$actual) == (tree_pred3$predicted))/nrow(tree_pred3))
# classification_error_1_txn_6_8
# 
# tree_pred4 <- subset(tree_pred2, tree_pred2$predicted == 1)
# misclassification_error_1_txn_6_8 <- 1-(sum((tree_pred4$predicted) == (tree_pred4$actual))/nrow(tree_pred4))
# misclassification_error_1_txn_6_8
# 
# tree_pred5 <- tree_pred2
# tree_pred5$actual <- ifelse(tree_pred2$actual == 1, 1, 0)
# tree_pred5$predicted <- ifelse(tree_pred2$predicted == 1, 1, 0)
# overall_error_txn_6_8_with_converting_0_and_2_to_0 <- 1- (sum((tree_pred5$actual) == (tree_pred5$predicted))/nrow(tree_pred5))
# overall_error_txn_6_8_with_converting_0_and_2_to_0
# 
# ####################### Accuracy/Error rates #################################################
# 
# overall_error_txn_6_8_without_converting_0_and_2_to_0
# classification_error_1_txn_6_8
# misclassification_error_1_txn_6_8
# overall_error_txn_6_8_with_converting_0_and_2_to_0
# 
# # dataframe:
# # dataframe without_converting_0_and_2_to_0
# 
# View(tree_pred2)
# 
# ##############################################################################################
# ##############################################################################################
# ##############################################################################################
# ##############################################################################################
# ##############################################################################################
# ##############################################################################################
# ##############################################################################################
# ##############################################################################################
# ##############################################################################################
# ##############################################################################################
# ##############################################################################################
# ##############################################################################################
# ##############################################################################################
# ##############################################################################################
# ##############################################################################################
# ##############################################################################################
# 
# CV_data <- as.data.frame(cbind(evaluation_data$silent_investors, tree_pred$predicted2))
# 
# ##############################################################################################
# ##############################################################################################
# ##############################################################################################
# 
# rm(DT)
# # model_DT <- rpart(silent_investors ~ ., data = data = train[-c(1:3, 53)],method="class")
# # model_DT <- prune(model_DT, cp= 0.224)
# 
# pred <- as.data.frame(predict(model_DT, newdata=evaluation_data))
# 
# colnames(pred) <- c("zero","one")
# pred$pred <- ifelse(pred$zero > pred$one, 0, 1)
# evaluation_data$silent_investors <- as.numeric(evaluation_data$silent_investors)
# 
# DT <- confusionMatrix(pred$pred, evaluation_data$silent_investors, positive = NULL, 
#                       dnn = c("Predicted", "Actual"), 
#                       prevalence = NULL)
# 
# table(train$silent_investors)
# DT
# 
# table(train$silent_investors)
# table(evaluation_data$silent_investors)
# 
# prp(model_DT)
# plotcp(model_DT)
# printcp(model_DT)
# 
# View(cbind(pred$pred, evaluation_data$silent_investors))
# 
# ##############################################################################################
# library(Boruta)
# 
# set.seed(123)
# boruta.train <- Boruta(silent_investors~., data = train[-c(1:3, 74, 73)], doTrace = 2)
# print(boruta.train)
# plot(boruta.train, xlab = "", xaxt = "n")
# 
# plot(boruta.train, xlab = "", xaxt = "n")
# lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
#   boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
# names(lz) <- colnames(boruta.train$ImpHistory)
# Labels <- sort(sapply(lz,median))
# axis(side = 1,las=2,labels = names(Labels),
#      at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
# 
# final.boruta <- TentativeRoughFix(boruta.train)
# print(final.boruta)
# getSelectedAttributes(final.boruta, withTentative = F)
# boruta.df <- attStats(final.boruta)
# class(boruta.df)
# print(boruta.df)
# 
# ##############################################################################################
# table(period_0$silent_investors)
# table(period_1$silent_investors)
# table(period_2$silent_investors)
# 
# table(evaluation_data$silent_investors)
# table(train$silent_investors)
# 
# 
# colnames(evaluation_data)
# evaluation_data2 <- evaluation_data
# evaluation_data2 <- subset(evaluation_data2, evaluation_data2$silent_investors == 1)
# evaluation_data2 <- evaluation_data2[c(1)]
# colnames(evaluation_data2)[1] <- "tXx_id_num"
# 
# evaluation_data3 <- merge(raw_data, evaluation_data2, by = "tXx_id_num", all = FALSE)
# 
# library(PerformanceAnalytics)
# 
# AAEPC7167Q <- subset(evaluation_data3, evaluation_data3$tXx_id_num == "AAEPC7167Q")
# AAEPC7167Q=AAEPC7167Q[order(AAEPC7167Q$TXN_DT),]
# plot.ts(AAEPC7167Q$TXN_AMT)
# nrow(AAEPC7167Q)
# 
# AEDPD9958D <- subset(evaluation_data3, evaluation_data3$tXx_id_num == "AEDPD9958D")
# AEDPD9958D=AEDPD9958D[order(AEDPD9958D$TXN_DT),]
# plot.ts(AEDPD9958D$TXN_AMT)
# nrow(AEDPD9958D)
# 
# 
# ADFPP1991D <- subset(evaluation_data3, evaluation_data3$tXx_id_num == "ADFPP1991D")
# ADFPP1991D=ADFPP1991D[order(ADFPP1991D$TXN_DT),]
# plot.ts(ADFPP1991D$TXN_AMT)
# nrow(ADFPP1991D)
# 
# 
# 
# 
# # to make changes
# 
# # final_IV_DV <- na.omit(final_IV_DV)
# # final_IV_DV <- final_IV_DV[complete.cases(final_IV_DV$TX_CD2), ]
# 
# # investors_IV <- subset(analysis_dataset, analysis_dataset$TXN_DT < IV_Month & analysis_dataset$TXN_DT > IV_Month_select_threshold)
# # investors_IV <- subset(analysis_dataset, analysis_dataset$TXN_DT < (IV_Month-90) & analysis_dataset$TXN_DT > (IV_Month_select_threshold-90)
# #                       
# # investors_IV <- subset(analysis_dataset, analysis_dataset$TXN_DT < (IV_Month-180) & analysis_dataset$TXN_DT > (IV_Month_select_threshold-180)
# # investors_IV <- subset(analysis_dataset, analysis_dataset$TXN_DT < (IV_Month-IV_DV_Month_Diff) & analysis_dataset$TXN_DT > (IV_Month_select_threshold-IV_DV_Month_Diff)            
# 
# train2 <- train
# 
# train2$silent_investors <- ifelse(train2$silent_investors == 2, 0, train2$silent_investors)
# 
# library(ipred)
# set.seed(131)
# error.tree <- numeric(10)
# for(i in 1:10) error.tree[i] <- errorest(silent_investors ~ ., data = train2[-c(1:3, 74, 73)], model = tree)$error
# summary(error.tree)
# 
# prediction_tree= errorest(silent_investors ~ ., data = train2[-c(1:3, 74, 73)], model = svm, cost = 10, gamma = 1.5,
#                                        est.para=control.errorest(k=10, predictions = TRUE))$predictions
# 
# 
# 
# ##################################################################################################
# 
# library(tree)
# library(randomForest)
# library(ipred)
# set.seed(131)
# error.RF <- numeric(10)
# for(i in 1:10) error.RF[i] <- errorest(silent_investors ~ ., data = train2[-c(1:3, 74, 73)], model = randomForest, mtry = 2)$error
# summary(error.RF)
# 
# prediction_RF= errorest(silent_investors ~ ., data = train2[-c(1:3, 74, 73)], model = randomForest,
#                           est.para=control.errorest(k=10, predictions = TRUE))$predictions
# 
# 
# 
# ###########################################################################################################
# 
# 
# library(e1071)
# set.seed(563)
# error.SVM <- numeric(10)
# for (i in 1:10) error.SVM[i] <- errorest(silent_investors ~ ., data = train2[-c(1:3, 74, 73)], model = svm,  cost = 10, gamma = 1.5)$error
# summary(error.SVM)  
# 
# prediction_SVM= errorest(silent_investors ~ ., data = train2[-c(1:3, 74, 73)], model = svm,  cost = 10, gamma = 1.5,
#                         est.para=control.errorest(k=10, predictions = TRUE))$predictions
# 
# comparision=as.data.frame(cbind(prediction_RF,prediction_tree,prediction_SVM,train2$silent_investors))
# 
# 
# ####################################################    ROC   ###################
# 
# roc(train2$silent_investors, prediction_RF)
# plot.roc(train2$silent_investors, prediction_RF)
# 
# roc(train2$silent_investors, prediction_SVM)
# plot.roc(train2$silent_investors, prediction_SVM)
# 
# roc(train2$silent_investors, prediction_tree)
# plot.roc(train2$silent_investors, prediction_tree)


####################################################################################################
#####################################################################################################

library(rpart.plot)
library(caret)
library(party)
library(e1071)
library(rpart)

train2 <- train
train2$silent_investors <- ifelse(train2$silent_investors == 2, 0, train2$silent_investors)
train2$silent_investors <- as.integer(train2$silent_investors)

model_tree <- tree(silent_investors ~ NAV9+percentage_holding_IV_month+PAMT7+PAMT8+NAV8+real_change_NAV9+
                     real_change_NAV5+real_change_NAV8+real_change_NAV7+NAV7, data = train2[-c(1:3, 74, 73)],method="class")
model_tree <- prune(model_tree, cp= 0.2)
plot(model_tree)
text(model_tree, cex=.75)

evaluation_data2 <- evaluation_data
evaluation_data2$silent_investors <- ifelse(evaluation_data2$silent_investors == 2, 0, evaluation_data2$silent_investors)
evaluation_data2$silent_investors <- as.integer(evaluation_data2$silent_investors)

tree_pred <- as.data.frame(predict(model_tree, newdata=evaluation_data2))
colnames(tree_pred)[1] <- "predicted"

cv1 <- as.data.frame(cbind(evaluation_data2$silent_investors, tree_pred$predicted))
colnames(cv1)[1] <- "actual"
colnames(cv1)[2] <- "predicted"

tree_pred$predicted <- ifelse(tree_pred$predicted > 0.5, 1, 0)

multiclass.roc(evaluation_data2$silent_investors, tree_pred$predicted)
plot.roc(evaluation_data2$silent_investors, tree_pred$predicted)

cv <- as.data.frame(cbind(evaluation_data2$silent_investors, tree_pred$predicted))
colnames(cv)[1] <- "actual"
colnames(cv)[2] <- "predicted"

# rm(DT)
DT11 <- confusionMatrix(cv$predicted, cv$actual, positive = NULL,
                        dnn = c("Predicted", "Actual"),
                        prevalence = NULL)

DT11

############################################## DT10 #############################

model_tree <- tree(silent_investors ~ NAV9+percentage_holding_IV_month+PAMT7+PAMT8+NAV8+real_change_NAV9+
                     real_change_NAV5+real_change_NAV8+real_change_NAV7+NAV7, data = train2[-c(1:3, 74, 73)],method="class")
model_tree <- prune(model_tree, cp= 0.2)
plot(model_tree)
text(model_tree, cex=.75)

evaluation_data2 <- evaluation_data
evaluation_data2$silent_investors <- ifelse(evaluation_data2$silent_investors == 2, 0, evaluation_data2$silent_investors)
evaluation_data2$silent_investors <- as.integer(evaluation_data2$silent_investors)

tree_pred <- as.data.frame(predict(model_tree, newdata=evaluation_data2))
colnames(tree_pred)[1] <- "predicted"

cv1 <- as.data.frame(cbind(evaluation_data2$silent_investors, tree_pred$predicted))
colnames(cv1)[1] <- "actual"
colnames(cv1)[2] <- "predicted"

tree_pred$predicted <- ifelse(tree_pred$predicted > 0.5, 1, 0)

multiclass.roc(evaluation_data2$silent_investors, tree_pred$predicted)
plot.roc(evaluation_data2$silent_investors, tree_pred$predicted)

cv <- as.data.frame(cbind(evaluation_data2$silent_investors, tree_pred$predicted))
colnames(cv)[1] <- "actual"
colnames(cv)[2] <- "predicted"

# rm(DT)
DT10<- confusionMatrix(cv$predicted, cv$actual, positive = NULL,
                       dnn = c("Predicted", "Actual"),
                       prevalence = NULL)

DT10


################################################  DT9        #####################

model_tree <- tree(silent_investors ~ NAV9+percentage_holding_IV_month+PAMT7+PAMT8+NAV8+real_change_NAV9+
                     real_change_NAV5+real_change_NAV8+real_change_NAV7, data = train2[-c(1:3, 74, 73)],method="class")
model_tree <- prune(model_tree, cp= 0.2)
plot(model_tree)
text(model_tree, cex=.75)

evaluation_data2 <- evaluation_data
evaluation_data2$silent_investors <- ifelse(evaluation_data2$silent_investors == 2, 0, evaluation_data2$silent_investors)
evaluation_data2$silent_investors <- as.integer(evaluation_data2$silent_investors)

tree_pred <- as.data.frame(predict(model_tree, newdata=evaluation_data2))
colnames(tree_pred)[1] <- "predicted"

cv1 <- as.data.frame(cbind(evaluation_data2$silent_investors, tree_pred$predicted))
colnames(cv1)[1] <- "actual"
colnames(cv1)[2] <- "predicted"

tree_pred$predicted <- ifelse(tree_pred$predicted > 0.5, 1, 0)

multiclass.roc(evaluation_data2$silent_investors, tree_pred$predicted)
plot.roc(evaluation_data2$silent_investors, tree_pred$predicted)

cv <- as.data.frame(cbind(evaluation_data2$silent_investors, tree_pred$predicted))
colnames(cv)[1] <- "actual"
colnames(cv)[2] <- "predicted"

# rm(DT)
DT9<- confusionMatrix(cv$predicted, cv$actual, positive = NULL,
                      dnn = c("Predicted", "Actual"),
                      prevalence = NULL)

DT9

#################################################     DT8           ###################

model_tree <- tree(silent_investors ~ NAV9+percentage_holding_IV_month+PAMT7+PAMT8+NAV8+real_change_NAV9+
                     real_change_NAV5+real_change_NAV8, data = train2[-c(1:3, 74, 73)],method="class")
model_tree <- prune(model_tree, cp= 0.2)
plot(model_tree)
text(model_tree, cex=.75)

evaluation_data2 <- evaluation_data
evaluation_data2$silent_investors <- ifelse(evaluation_data2$silent_investors == 2, 0, evaluation_data2$silent_investors)
evaluation_data2$silent_investors <- as.integer(evaluation_data2$silent_investors)

tree_pred <- as.data.frame(predict(model_tree, newdata=evaluation_data2))
colnames(tree_pred)[1] <- "predicted"

cv1 <- as.data.frame(cbind(evaluation_data2$silent_investors, tree_pred$predicted))
colnames(cv1)[1] <- "actual"
colnames(cv1)[2] <- "predicted"

tree_pred$predicted <- ifelse(tree_pred$predicted > 0.5, 1, 0)

multiclass.roc(evaluation_data2$silent_investors, tree_pred$predicted)
plot.roc(evaluation_data2$silent_investors, tree_pred$predicted)

cv <- as.data.frame(cbind(evaluation_data2$silent_investors, tree_pred$predicted))
colnames(cv)[1] <- "actual"
colnames(cv)[2] <- "predicted"

# rm(DT)
DT8<- confusionMatrix(cv$predicted, cv$actual, positive = NULL,
                      dnn = c("Predicted", "Actual"),
                      prevalence = NULL)

DT8

#################################################     DT7          ###################

model_tree <- tree(silent_investors ~ NAV9+percentage_holding_IV_month+PAMT7+PAMT8+NAV8+real_change_NAV9+
                     real_change_NAV5, data = train2[-c(1:3, 74, 73)],method="class")
model_tree <- prune(model_tree, cp= 0.2)
plot(model_tree)
text(model_tree, cex=.75)

evaluation_data2 <- evaluation_data
evaluation_data2$silent_investors <- ifelse(evaluation_data2$silent_investors == 2, 0, evaluation_data2$silent_investors)
evaluation_data2$silent_investors <- as.integer(evaluation_data2$silent_investors)

tree_pred <- as.data.frame(predict(model_tree, newdata=evaluation_data2))
colnames(tree_pred)[1] <- "predicted"

cv1 <- as.data.frame(cbind(evaluation_data2$silent_investors, tree_pred$predicted))
colnames(cv1)[1] <- "actual"
colnames(cv1)[2] <- "predicted"

tree_pred$predicted <- ifelse(tree_pred$predicted > 0.5, 1, 0)

multiclass.roc(evaluation_data2$silent_investors, tree_pred$predicted)
plot.roc(evaluation_data2$silent_investors, tree_pred$predicted)

cv <- as.data.frame(cbind(evaluation_data2$silent_investors, tree_pred$predicted))
colnames(cv)[1] <- "actual"
colnames(cv)[2] <- "predicted"

# rm(DT)
DT7<- confusionMatrix(cv$predicted, cv$actual, positive = NULL,
                      dnn = c("Predicted", "Actual"),
                      prevalence = NULL)

DT7



#################################################     DT6         ###################

model_tree <- tree(silent_investors ~ NAV9+percentage_holding_IV_month+PAMT7+PAMT8+NAV8+real_change_NAV9, data = train2[-c(1:3, 74, 73)],method="class")
model_tree <- prune(model_tree, cp= 0.2)
plot(model_tree)
text(model_tree, cex=.75)

evaluation_data2 <- evaluation_data
evaluation_data2$silent_investors <- ifelse(evaluation_data2$silent_investors == 2, 0, evaluation_data2$silent_investors)
evaluation_data2$silent_investors <- as.integer(evaluation_data2$silent_investors)

tree_pred <- as.data.frame(predict(model_tree, newdata=evaluation_data2))
colnames(tree_pred)[1] <- "predicted"

cv1 <- as.data.frame(cbind(evaluation_data2$silent_investors, tree_pred$predicted))
colnames(cv1)[1] <- "actual"
colnames(cv1)[2] <- "predicted"

tree_pred$predicted <- ifelse(tree_pred$predicted > 0.5, 1, 0)

multiclass.roc(evaluation_data2$silent_investors, tree_pred$predicted)
plot.roc(evaluation_data2$silent_investors, tree_pred$predicted)

cv <- as.data.frame(cbind(evaluation_data2$silent_investors, tree_pred$predicted))
colnames(cv)[1] <- "actual"
colnames(cv)[2] <- "predicted"

# rm(DT)
DT6<- confusionMatrix(cv$predicted, cv$actual, positive = NULL,
                      dnn = c("Predicted", "Actual"),
                      prevalence = NULL)

DT6

#################################################     DT5        ###################

model_tree <- tree(silent_investors ~ NAV9+percentage_holding_IV_month+PAMT7+PAMT8+NAV8
                   , data = train2[-c(1:3, 74, 73)],method="class")
model_tree <- prune(model_tree, cp= 0.2)
plot(model_tree)
text(model_tree, cex=.75)

evaluation_data2 <- evaluation_data
evaluation_data2$silent_investors <- ifelse(evaluation_data2$silent_investors == 2, 0, evaluation_data2$silent_investors)
evaluation_data2$silent_investors <- as.integer(evaluation_data2$silent_investors)

tree_pred <- as.data.frame(predict(model_tree, newdata=evaluation_data2))
colnames(tree_pred)[1] <- "predicted"

cv1 <- as.data.frame(cbind(evaluation_data2$silent_investors, tree_pred$predicted))
colnames(cv1)[1] <- "actual"
colnames(cv1)[2] <- "predicted"

tree_pred$predicted <- ifelse(tree_pred$predicted > 0.5, 1, 0)

multiclass.roc(evaluation_data2$silent_investors, tree_pred$predicted)
plot.roc(evaluation_data2$silent_investors, tree_pred$predicted)

cv <- as.data.frame(cbind(evaluation_data2$silent_investors, tree_pred$predicted))
colnames(cv)[1] <- "actual"
colnames(cv)[2] <- "predicted"

# rm(DT)
DT5<- confusionMatrix(cv$predicted, cv$actual, positive = NULL,
                      dnn = c("Predicted", "Actual"),
                      prevalence = NULL)

DT5

#################################################     DT4         ###################

model_tree <- tree(silent_investors ~ NAV9+percentage_holding_IV_month+PAMT7+PAMT8, data = train2[-c(1:3, 74, 73)],method="class")
model_tree <- prune(model_tree, cp= 0.2)
plot(model_tree)
text(model_tree, cex=.75)

evaluation_data2 <- evaluation_data
evaluation_data2$silent_investors <- ifelse(evaluation_data2$silent_investors == 2, 0, evaluation_data2$silent_investors)
evaluation_data2$silent_investors <- as.integer(evaluation_data2$silent_investors)

tree_pred <- as.data.frame(predict(model_tree, newdata=evaluation_data2))
colnames(tree_pred)[1] <- "predicted"

cv1 <- as.data.frame(cbind(evaluation_data2$silent_investors, tree_pred$predicted))
colnames(cv1)[1] <- "actual"
colnames(cv1)[2] <- "predicted"

tree_pred$predicted <- ifelse(tree_pred$predicted > 0.5, 1, 0)

multiclass.roc(evaluation_data2$silent_investors, tree_pred$predicted)
plot.roc(evaluation_data2$silent_investors, tree_pred$predicted)

cv <- as.data.frame(cbind(evaluation_data2$silent_investors, tree_pred$predicted))
colnames(cv)[1] <- "actual"
colnames(cv)[2] <- "predicted"

# rm(DT)
DT4<- confusionMatrix(cv$predicted, cv$actual, positive = NULL,
                      dnn = c("Predicted", "Actual"),
                      prevalence = NULL)

DT4

#################################################     DT3       ###################

model_tree <- tree(silent_investors ~ NAV9+percentage_holding_IV_month+PAMT7, data = train2[-c(1:3, 74, 73)],method="class")
model_tree <- prune(model_tree, cp= 0.2)
plot(model_tree)
text(model_tree, cex=.75)

evaluation_data2 <- evaluation_data
evaluation_data2$silent_investors <- ifelse(evaluation_data2$silent_investors == 2, 0, evaluation_data2$silent_investors)
evaluation_data2$silent_investors <- as.integer(evaluation_data2$silent_investors)

tree_pred <- as.data.frame(predict(model_tree, newdata=evaluation_data2))
colnames(tree_pred)[1] <- "predicted"

cv1 <- as.data.frame(cbind(evaluation_data2$silent_investors, tree_pred$predicted))
colnames(cv1)[1] <- "actual"
colnames(cv1)[2] <- "predicted"

tree_pred$predicted <- ifelse(tree_pred$predicted > 0.5, 1, 0)

multiclass.roc(evaluation_data2$silent_investors, tree_pred$predicted)
plot.roc(evaluation_data2$silent_investors, tree_pred$predicted)

cv <- as.data.frame(cbind(evaluation_data2$silent_investors, tree_pred$predicted))
colnames(cv)[1] <- "actual"
colnames(cv)[2] <- "predicted"

# rm(DT)
DT3<- confusionMatrix(cv$predicted, cv$actual, positive = NULL,
                      dnn = c("Predicted", "Actual"),
                      prevalence = NULL)

DT3

#################################################     DT2        ###################

model_tree <- tree(silent_investors ~ NAV9+percentage_holding_IV_month, data = train2[-c(1:3, 74, 73)],method="class")
model_tree <- prune(model_tree, cp= 0.2)
plot(model_tree)
text(model_tree, cex=.75)

evaluation_data2 <- evaluation_data
evaluation_data2$silent_investors <- ifelse(evaluation_data2$silent_investors == 2, 0, evaluation_data2$silent_investors)
evaluation_data2$silent_investors <- as.integer(evaluation_data2$silent_investors)

tree_pred <- as.data.frame(predict(model_tree, newdata=evaluation_data2))
colnames(tree_pred)[1] <- "predicted"

cv1 <- as.data.frame(cbind(evaluation_data2$silent_investors, tree_pred$predicted))
colnames(cv1)[1] <- "actual"
colnames(cv1)[2] <- "predicted"

tree_pred$predicted <- ifelse(tree_pred$predicted > 0.5, 1, 0)

multiclass.roc(evaluation_data2$silent_investors, tree_pred$predicted)
plot.roc(evaluation_data2$silent_investors, tree_pred$predicted)

cv <- as.data.frame(cbind(evaluation_data2$silent_investors, tree_pred$predicted))
colnames(cv)[1] <- "actual"
colnames(cv)[2] <- "predicted"

# rm(DT)
DT2<- confusionMatrix(cv$predicted, cv$actual, positive = NULL,
                      dnn = c("Predicted", "Actual"),
                      prevalence = NULL)

DT2

# comparing different levels of variables

rbind(DT11$table, DT10$table, DT9$table, DT8$table, DT7$table, DT6$table, DT5$table, DT4$table, DT3$table, DT2$table)
rbind(DT11$overall, DT10$overall, DT9$overall, DT8$overall, DT7$overall, DT6$overall, DT5$overall, DT4$overall, DT3$overall, DT2$overall)
