Dashboard_MutualFunds <- read.csv("C:/Users/leelavathi.a/Desktop/Dashboard_MutualFunds.csv")

db<-data.frame(unique(Dashboard_MutualFunds[,c(1,2)]))

new<-Dashboard_MutualFunds[,c(1,5)]

db<-data.frame(unique(merge(db,new, by="Distributor.Name",all.x=TRUE)))

levels(factor(db$Distributor.Category))

#table(factor(db$Distributor.Name))

db$Distributor.Category=ifelse(as.character(db$Distributor.Category) %in% c("Bank-Others","Bank-PSU","Bank-Pvt/Foreign"),"Bank",as.character(db$Distributor.Category))

db$Distributor.Category=ifelse(as.character(db$Distributor.Category) %in% c("National /Regional Distributor","ND","ND-RD"),"ND/RD",as.character(db$Distributor.Category))

db=subset(db,Distributor.Category %in% c("Bank","IFA","ND/RD"))

db$SchemeType=sample(c("Large Cap","Small & Mid Cap","Diversified Equity","ELSS","Balanced"),nrow(db),replace = TRUE)


db$SIP=sample(c("SIP","One-Time"),nrow(db),replace = TRUE)


db$City=sample(c("Hyderabad","Itangar","Dispur","Patna","Raipur","Panaji","Gandhinagar","Chandigarh","Shimla","Srinagar and Jammu","Ranchi","Bangalore","Thiruvananthapuram",
                 "Bhopal","Mumbai","Imphal","Shillong","Aizawi","Kohima","Bhubaneshwar","Chandigarh","Jaipur","Gangtok","Chennai","Agartala","Dehradun","Lucknow","Kolkata",
                 "Chandigarh","Silvassa","Daman","Delhi","Kavaratti","Pondicherry","Port Blair"),nrow(db),replace = TRUE)


db$InvestorCategory=sample(as.character(unique(Dashboard_MutualFunds$Investor.Category)),nrow(db),replace = TRUE)

db$AssetClass=sample(as.character(unique(Dashboard_MutualFunds$Asset.Class)),nrow(db),replace = TRUE)

df<-data.frame(Distributor.Name=character(),Distributor.Code=character(),Distributor.Category=character(),SchemeType=character(),SIP=character(),City=character(),InvestorCategory=character(),AssetClass=character(),Time=character(),AUM=numeric())
for(i in 1:nrow(db)){
  Current_AUM=sample(20000:90000,1)
  NextMonth_AUM=Current_AUM+sample(c(-1000:-3000,1000:3000),1)
  NextQuarter_AUM=NextMonth_AUM+sample(c(-1000:-3000,3500:6800),1)
  NextYear_AUM=NextQuarter_AUM+sample(c(-1000:-3000,9000:15000),1)
  d1<-data.frame(db[i,],Time="Current",AUM=Current_AUM)
  d2=data.frame(db[i,],Time="NextMonth",AUM=NextMonth_AUM)
  d3<-data.frame(db[i,],Time="NextQuarter",AUM=NextQuarter_AUM)
  d4<-data.frame(db[i,],Time="NextYear",AUM=NextYear_AUM)
  df<-rbind(df,d1,d2,d3,d4)
  
}




write.csv(db,"C:/Users/leelavathi.a/Desktop/new.csv")

write.csv(df,"C:/Users/leelavathi.a/Desktop/MutualFunds_DB.csv")

# Dashboard_MutualFunds$City=sample(c("Hyderabad","Chennai","Pune","Mumbai","Delhi","Vijayawada","Bangalore","Baroda","Kolkata","Patna","Ahmedabad","Bhopal","Jaipur","Bhuvaneswar","Gangtok","Lucknow","Kanpur","Vadodara","Agra","Rajkot","Varanasi","Allahabad","Guwahati","Mysore","Indore","Nagpur"),nrow(Dashboard_MutualFunds),replace = TRUE)

