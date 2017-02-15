mydata <- read.csv("C:/Users/leelavathi.a/Desktop/sampledata.csv")
al<-sample_n(mydata,10)
###### distict ######is used to remove duplicates
x1<-distinct(mydata)
x2<-select(mydata,3:5)

 ####### filter the data############
my<-filter(mydata,Index %in%  c("A","C"))
my2<-filter(mydata,!Index %in%  c("A","C"))
my1<-filter(mydata,Index %in%  c("A","C")& Y2008<1945229)

########### SELECT ##########
NN<-select(mydata,contains("N")) 
### SELECTING RANDOMLY BASED ON MULTIPLE COLOUMNS ####
selecting<-mydata %>% select(Index,State,Y2008) %>% sample_n(15)

###### filtering data using a specific keyword" #############
st<-filter(mydata,grepl("Ar",State))
bnk<-filter(UTI_Distributer_2015_16324536,grepl("Bank",Name.of.the.ARN.Holder,ignore.case=TRUE))

######### ARRANGE FUNCTION FOR SORTING THE DATA ###
ascending<-mydata %>% arrange(Index)
descending<-mydata %>% arrange(desc(Index))
### SORTING MULTIPLE COLOUMNS #######
asc1<-mydata %>% arrange(Index,State,Y2008)
## SORTING HTE DATA IN DESCENDING ORDER FOR MULTIPLE VARIABLES  ###
DESC1<-mydata %>% arrange(desc(Index,State,Y2008,Y2009))


GR<-mydata %>% group_by(Index,State) %>%   summarise_at(Y2002)

#################Filter Data within a Categorical Variable###########

al<-mydata %>% filter(Index %in% c("A","c")) %>% group_by(Index) %>% do(head(.,2))

###################  Selecting 3rd Maximum Value by Categorical Variable #########

third<-mydata %>% select(Index,Y2015) %>% filter(Index %in% c("A","C","I")) %>% group_by(Index) %>%  arrange(desc(Y2015)) %>% slice(1)

#third1<-mydata %>% select(Index,Y2015) %>% filter(Index %in% c("A","C","I")) %>% group_by(Index) %>% filter(min_rank(desc(Y2015)) == 3)

#########USING MUTATE FOR PERFORMIMNG ARTHIMATIC OPERATIONS #################
nwer<-mutate(mydata,change=Y2006-Y2007)
nwww<-data.frame(mydata$Y2002/mydata$Y2003)
##########
#mydata11 = mutate_all(mydata, funs("new" = .* 1000))

############### Calculate Rank for Variables ####################
mydata12 = mutate_at(mydata, vars(Y2008:Y2010), funs(Rank=min_rank(.)))

################ Select State that generated highest income among the variable 'Index' #######################
out = mydata %>% group_by(Index) %>% filter(min_rank(desc(Y2015)) == 1) %>% select(Index, Y2015)


############ Cumulative Income of 'Index' variable ##########
out2 = mydata %>% group_by(Index) %>% mutate(Total=cumsum(Y2015)) %>% select(Index,Y2013:Y2015)

############## JOINS IN R ###################
df1 <- data.frame(ID = c(1, 2, 3, 4, 5),
                  w = c('a', 'b', 'c', 'd', 'e'),
                  x = c(1, 1, 0, 0, 1),
                  y=rnorm(5),
                  z=letters[1:5])
df2 <- data.frame(ID = c(1, 7, 3, 6, 8),
                  a = c('z', 'b', 'k', 'd', 'l'),
                  b = c(1, 2, 3, 0, 4),
                  c =rnorm(5),
                  d =letters[2:6])
### INNER JOIN #########
df3 = inner_join(df1, df2, by = "ID")


########LEFT_JOIN ##########
## RETURNS ALL THE ROWS IN FIRST DATA FRAME AND ALL COLOUMNS IN SECOND DF ####
LEFTJOIN<-left_join(df1, df2, by = "ID")

#################UNINON ###############
#COMBINES DATA VERTICALLY#
x=data.frame(ID = 1:6, ID1= 1:6)
y=data.frame(ID = 1:6,  ID1 = 1:6,ID2=1:6)
UN<-union(x,y)
un1<-union_all(x,y)


