data.frame(rep(1:4, each = 2) )
state<-data.frame(rep(c("Andaman and Nicobar Islands"," Andhra Pradesh"," Arunachal Pradesh"," Assam ","Bihar"," Chandigarh"," Chhattisgarh"," Dadra and Nagar Haveli"," Daman and Diu"," Delhi",
                        " Goa"," Gujarat"," Haryana"," Himachal Pradesh"," Jammu and Kashmir"," Jharkhand"," Karnataka"," Kerala"," Lakshadweep"," Madhya Pradesh"," Maharashtra"," Manipur"," Meghalaya"," Mizoram"," Nagaland"," Orissa"," Pondicherry"," Punjab"," Rajasthan",
                        " Sikkim"," Tamil Nadu"," Telangana"," Tripura ","Uttar Pradesh"," West Bengal"),each=40))

colnames(state)[1]<-"state"


range<-data.frame(rep(c(-10,-5,0,5,10),each=8))
ss<-data.frame(lapply(range, rep, 35))

meadia<-data.frame(c("tv","radio","billboards","magazines","no.of.distributors","promotion.stores","price","newspaper"))
colnames(meadia)[1]<-"meadia"
md1<-data.frame(lapply(meadia, rep, 175))
fianld<-cbind(final,md1)



########Sales Forecasting############
sales<-data.frame(seq(as.Date("2014/1/1"), by = "months", length.out = 36))
colnames(sales)<-"Date"
md1<-data.frame(lapply(sales, rep, 21))
md1$Product_sub_cateegory<-rep(c("Fragnance","Hair Care","Body Wash","Literature & Ficton" ,"Cooking","Education","Men Clothing", "Women Clothing" ,"Kids Clothing" , "DVD & BLU Ray Players", "Laptops","Mobiles",    
                             "Exercise Bikes", "Tred MIll","Weights", "Iron Box","Jiucers & Blenders" ,"Refrigrators","Foot Ball", "Tennis","Base Ball" ) ,each=36)                             
                                                                  
md1$PRoduct_category<-rep(c("Beauty","Books"  ,"Clothing","Electronics","Exercise & Fitness","Home Appliances","Sports" ),each=108) 

new<-data.frame(lapply(md1, rep, 4))
new$zone<-rep(c("East","West","North","South"),each=756) 

aggregate(final_ret_dashbrd1$sales, by = list(final_ret_dashbrd1$Product.SubCategory), max)
aggregate(final_ret_dashbrd1$sales, by = list(final_ret_dashbrd1$Product.SubCategory), min)

fragnace<-data.frame(sample(3332: 85680, 36, replace=FALSE))
colnames(fragnace)<-"sales" 
                                                                           

ahircare<-data.frame(sample(4760: 80172, 36, replace=FALSE))
colnames(ahircare)<-"sales"                           
                            
ahircare<-data.frame(sample(4760: 80172, 36, replace=FALSE))
colnames(ahircare)<-"sales"

Bodywash<-data.frame(sample(4760: 84320, 36, replace=FALSE))
colnames(Bodywash)<-"sales"
                             
Literature<-data.frame(sample(6596: 66368, 36, replace=FALSE))
colnames(Literature)<-"sales"    
  

cooking<-data.frame(sample(5780:71604, 36, replace=FALSE))
colnames(cooking)<-"sales"   

Education<-data.frame(sample(3440:75276, 36, replace=FALSE))
colnames(Education)<-"sales"

Mens_c<-data.frame(sample(4556:83232, 36, replace=FALSE))
colnames(Mens_c)<-"sales"

women_clo<-data.frame(sample(6936:87536, 36, replace=FALSE))
colnames(women_clo)<-"sales"

kida_c<-data.frame(sample(5508:86292, 36, replace=FALSE))
colnames(kida_c)<-"sales"

DVD<-data.frame(sample(9656:90576, 36, replace=FALSE))
colnames(DVD)<-"sales"

saa<-rbind(fragnace,ahircare,Bodywash,Literature,cooking,Education,Mens_c,women_clo,kida_c,DVD)

Laptops<-data.frame(sample(5304:80172, 36, replace=FALSE))
colnames(Laptops)<-"sales"

Mobiles<-data.frame(sample(3808:78948, 36, replace=FALSE))
colnames(Mobiles)<-"sales"


ExerciseBikes<-data.frame(sample(5372:69476, 36, replace=FALSE))
colnames(ExerciseBikes)<-"sales"

TredMIll<-data.frame(sample(7888:96696, 36, replace=FALSE))
colnames(TredMIll)<-"sales"


Weights<-data.frame(sample(5780:87516, 36, replace=FALSE))
colnames(Weights)<-"sales"

IronBox<-data.frame(sample(7276:72949, 36, replace=FALSE))
colnames(IronBox)<-"sales"

Jiucers<-data.frame(sample(3876:72949, 36, replace=FALSE))

