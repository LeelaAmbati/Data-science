add<-data.frame(x=c(1,23,34),name=c("LEELA","SNEHA",NA),height=c(5.5,NA,5.2))
## checking count of misding values #####
sum(is.na(add))

######### Excluding missing values with the help of compelte.cases######
I<-complete.cases(add)
Add1<-add[I,]



sd<-data.frame(x=c(1,23,34),name=c("LEELA","SNEHA",NA),height=c(5.5,NA,5.2))
sum(is.na(sd))
######### To get hte percentage of missing values ###########
paste(100*sum(is.na(sd))/length(sd),"%",sep="")

####### TO get Coloumn wise percentage of missing values #####
missSummary <- function(sd){
  VarName <- names(sd)
  Observations <- sapply(sd,function(x) length(x))
  missCount <-sapply(sd,function(x) sum(is.na(x)))
  missPercent <-sapply(sd,function(x) paste(100*sum(is.na(x))/length(x),"%",sep="") )
                      
  out.DF <- cbind(VarName,Observations, missCount,missPercent)
  row.names(out.DF) <- NULL              
  out.DF                
}                  
missings <-missSummary(add) 
  
######## Replacing Missing values ########

# Replace all variables with zero
df1 <- data.frame(sapply(sd,function(x) ifelse(is.na(x)==T, 0,x)))

# Replace all variables with their mean
sd1<-data.frame(x=c(1,NA,34),name=c(1,2,NA),height=c(5.5,NA,5.2))
df2 <- data.frame(sapply(sd1,function(x) ifelse(is.na(x)==T, mean(x,na.rm =T),x)))

