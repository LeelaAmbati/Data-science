Meta_Data_DashBoard <- read.csv("C:/Users/leelavathi.a/Desktop/Mirae_CS_US_Data.csv")
str(Meta_Data_DashBoard)

Meta_Data_DashBoard<-Meta_Data_DashBoard[,-c(1,18:24)]
Meta_Data_DashBoard[,c(2,4,6:16)]<-lapply(Meta_Data_DashBoard[,c(2,4,6:16)],function(x) as.factor(x))

unique_pan=unique(Meta_Data_DashBoard$PAN)

investor_schmid<-data.frame()
for (i in 1:length(unique_pan)){
  x<-subset(Meta_Data_DashBoard,as.character(PAN)==as.character(unique_pan[i]))
  if(nrow(x) %in%c(2,3)){
    investor_schmid<-rbind(investor_schmid,data.frame(PAN=as.character(unique_pan[i]),SCHM_ID1=x[1,2],SCHM_ID2=x[2,2],SCHM_ID3=x[3,2]))
  }
}

colnames(investor_schmid)[2]<-"SCHM_ID"

crosssell_apriori_data=merge(investor_schmid,Meta_Data_DashBoard,by=c("PAN","SCHM_ID"))
colnames(crosssell_apriori_data)[2]<-"SCHM_ID1"
str(crosssell_apriori_data)

library(arules)

#Apriori algorithm
apriori_rules <- apriori(crosssell_apriori_data, parameter = list(supp = 0.25, conf = 0.8, target = "rules"))

#Summarizing rules
summary(apriori_rules)
# inspect(apriori_rules)

#Converting rules into data frame
apriori_rules_df_1 = data.frame(lhs = labels(lhs(apriori_rules)),rhs = labels(rhs(apriori_rules)),apriori_rules@quality)
# apriori_rules_df_quality_1<-apriori_rules_df_1[apriori_rules_df_1$lift>=2,]


write.csv(apriori_rules_df_1,"C:/Users/leelavathi.a/Desktop/CS_US_AprioriRules_without_APP.csv")



apriori_rules2 <- apriori(crosssell_apriori_data, parameter = list(supp = 0.002, conf = 0.4, target = "rules"),appearance = list(rhs=c(paste0("SCHM_ID1=",levels(crosssell_apriori_data$SCHM_ID1))),default="lhs"))

#Summarizing rules
summary(apriori_rules2)
# inspect(apriori_rules2)

#Converting rules into data frame
apriori_rules_df = data.frame(lhs = labels(lhs(apriori_rules2)),rhs = labels(rhs(apriori_rules2)),apriori_rules2@quality)
# apriori_rules_df_quality<-apriori_rules_df[apriori_rules_df$lift>=2,]
write.csv(apriori_rules_df,"C:/Users/leelavathi.a/Desktop/CS_US_AprioriRules_with_APP.csv")
