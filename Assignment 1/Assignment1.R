##PROBLEM 1
setwd("C:/Users/loaus/OneDrive - stevens.edu/STEVENS/Foundations of Financial Data Science/Assignment/HW1_F23")
getwd()
library(openxlsx)
brooklyn<-read.xlsx("rollingsales_brooklyn.xlsx", 1,startRow=5)
bronx<-read.xlsx("rollingsales_bronx.xlsx", 1,startRow=5)
manhattan<-read.xlsx("rollingsales_manhattan.xlsx", 1,startRow=5)
statenisland<-read.xlsx("rollingsales_statenisland.xlsx", 1,startRow=5)
queens<-read.xlsx("rollingsales_queens.xlsx", 1,startRow=5)

newyork<-rbind(brooklyn,bronx,manhattan,statenisland,queens)

head(newyork)
summary(newyork)

newyork$SALE.PRICE
summary(newyork$SALE.PRICE)

## missing values
length(newyork[is.na(newyork$SALE.PRICE),1])
length(newyork[is.na(newyork$GROSS.SQUARE.FEET),1])
length(newyork[is.na(newyork$DATE),1])
length(newyork[is.na(newyork$BOROUGH),1])
length(newyork[is.na(newyork$BUILDING.CLASS.CATEGORY),1])

length(newyork[newyork$SALE.PRICE==0,1])
length(newyork[newyork$GROSS.SQUARE.FEET==0,1])
length(newyork[newyork$DATE==0,1])
length(newyork[newyork$BOROUGH==0,1])
length(newyork[newyork$BUILDING.CLASS.CATEGORY==0,1])

length(newyork[newyork$SALE.PRICE==0,1])/length(newyork[,1])
length(newyork[newyork$GROSS.SQUARE.FEET==0,1])/length(newyork[,1])

names(newyork)
names(newyork) <- tolower(names(newyork))
names(newyork)


## clean/format the data with regular expressions
newyork$sale.date <- convertToDateTime(as.numeric(newyork$sale.date))
newyork$sale.date
newyork$sale.datewindow <-cut(newyork$sale.date, breaks = "quarter")
newyork$borough<-as.factor(newyork$borough)
newyork$building.class.category<-as.factor(newyork$building.class.category)
summary(newyork)
class(newyork$sale.price)
class(newyork$gross.square.feet)

## keep only the actual sales
plot(newyork$gross.square.feet,newyork$sale.price)
plot(log(newyork$gross.square.feet),log(newyork$sale.price),main="Scatter Plot w/ missing values")

newyork.sale <- newyork[newyork$sale.price!=0,]
plot(newyork.sale$gross.square.feet,newyork.sale$sale.price)
plot(log(newyork.sale$gross.square.feet),log(newyork.sale$sale.price),main="Scatter Plot without missing values (Actual Sales)")

## remove outliers
newyork.sale$outliers <- (log(newyork.sale$sale.price) <= 6) + 0
newyork.sale$outliers2 <- (log(newyork.sale$gross.square.feet) <= 4) + 0

newyork.sale <- newyork.sale[which(newyork.sale$outliers==0 & newyork.sale$outliers2==0),]
summary(newyork.sale)
plot(log(newyork.sale$gross.square.feet),log(newyork.sale$sale.price),main="Scatter Plot w/out Outliers")

##let's look at 1, 2, 3 family homes,coops, and condos
unique(newyork.sale$building.class.category)
newyork.homes<-newyork.sale[which(grepl("FAMILY",newyork.sale$building.class.category)),]
newyork.homes<-rbind(newyork.homes, newyork.sale[which(grepl("COND",newyork.sale$building.class.category)),])
newyork.homes<-rbind(newyork.homes, newyork.sale[which(grepl("COOPS",newyork.sale$building.class.category)),])
unique(newyork.homes$building.class.category)

#exploratory data analysis: statistics and visual graphs
library(doBy)
library(ggplot2)

quant_analysis<-function(x){
  c(length(x),min(x),max(x),mean(x))
}

ggplot(newyork.homes,aes(x=building.class.category, y=log(sale.price), fill=building.class.category))+geom_boxplot()+labs(title = "Boxplot of Price",subtitle="segment: Building Category")
ggplot(newyork.homes,aes(x=borough, y=log(sale.price), fill=borough))+geom_boxplot()+labs(title = "Boxplot of Price",subtitle="segment: Borough")
ggplot(newyork.homes,aes(x=sale.datewindow, y=log(sale.price), fill=sale.datewindow))+geom_boxplot()+labs(title = "Boxplot of Price",subtitle="segment: Sale Date Window")

summaryBy(sale.price~building.class.category, data=newyork.homes, FUN=quant_analysis)
summaryBy(sale.price~borough, data=newyork.homes, FUN=quant_analysis)
summaryBy(sale.price~sale.datewindow, data=newyork.homes, FUN=quant_analysis)


summaryBy(sale.price~building.class.category+borough, data=newyork.homes, FUN=quant_analysis)
summaryBy(sale.price~building.class.category+sale.datewindow, data=newyork.homes, FUN=quant_analysis)

ggplot(newyork.homes,aes(x=building.class.category, y=log(sale.price), fill=borough))+geom_boxplot()
ggplot(newyork.homes,aes(x=borough, y=log(sale.price), fill=building.class.category))+geom_boxplot()

ggplot(newyork.homes,aes(x=building.class.category, y=log(sale.price), fill=sale.datewindow))+geom_boxplot()
ggplot(newyork.homes,aes(x=sale.datewindow, y=log(sale.price), fill=building.class.category))+geom_boxplot()

ggplot(newyork.homes,aes(x=borough, y=log(sale.price), fill=sale.datewindow))+geom_boxplot()
ggplot(newyork.homes,aes(x=sale.datewindow, y=log(sale.price), fill=borough))+geom_boxplot()

## models
manhattan<-newyork.homes[newyork.homes$borough==1,]
plot(log(manhattan$gross.square.feet),log(manhattan$sale.price))

brooklyn<-newyork.homes[newyork.homes$borough==3,]
plot(log(brooklyn$gross.square.feet),log(brooklyn$sale.price))

bronx<-newyork.homes[newyork.homes$borough==2,]
plot(log(bronx$gross.square.feet),log(bronx$sale.price))

queens<-newyork.homes[newyork.homes$borough==4,]
plot(log(queens$gross.square.feet),log(queens$sale.price))

statenisland<-newyork.homes[newyork.homes$borough==5,]
plot(log(statenisland$gross.square.feet),log(statenisland$sale.price))

model1=lm(log(newyork.homes$sale.price)~log(newyork.homes$gross.square.feet))
summary(model1)
plot(log(newyork.homes$gross.square.feet), log(newyork.homes$sale.price),main="New York: Linear Model SQFT vs Price")
abline(model1, col='red', lwd=2)

model2=lm(log(bronx$sale.price)~log(bronx$gross.square.feet))
summary(model2)
plot(log(bronx$gross.square.feet), log(bronx$sale.price),main="Bronx: Linear Model SQFT vs Price")
abline(model2, col='red', lwd=2)

model3=lm(log(manhattan$sale.price)~log(manhattan$gross.square.feet))
summary(model3)
plot(log(manhattan$gross.square.feet), log(manhattan$sale.price),main="Manhattan: Linear Model SQFT vs Price")
abline(model3, col='red', lwd=2)

model4=lm(log(brooklyn$sale.price)~log(brooklyn$gross.square.feet))
summary(model4)
plot(log(brooklyn$gross.square.feet), log(brooklyn$sale.price),main="Brooklyn: Linear Model SQFT vs Price")
abline(model4, col='red', lwd=2)

model5=lm(log(queens$sale.price)~log(queens$gross.square.feet))
summary(model5)
plot(log(queens$gross.square.feet), log(queens$sale.price),main="Queens: Linear Model SQFT vs Price")
abline(model5, col='red', lwd=2)

model6=lm(log(statenisland$sale.price)~log(statenisland$gross.square.feet))
summary(model6)
plot(log(statenisland$gross.square.feet), log(statenisland$sale.price),main="Staten Island: Linear Model SQFT vs Price")
abline(model6, col='red', lwd=2)


##PROBLEM 2
setwd("C:/Users/loaus/OneDrive - stevens.edu/STEVENS/Foundations of Financial Data Science/Assignment/HW1_F23")
getwd()

## Import Data
data1<-read.csv("nyt1.csv")
data2<-read.csv("nyt2.csv")
data3<-read.csv("nyt3.csv")

## clean/format the data with regular expressions
data1$Gender<-as.factor(data1$Gender)
data2$Gender<-as.factor(data2$Gender)
data3$Gender<-as.factor(data3$Gender)

data1$Signed_In<-as.factor(data1$Signed_In)
data2$Signed_In<-as.factor(data2$Signed_In)
data3$Signed_In<-as.factor(data3$Signed_In)

class(data1$Impressions)
class(data2$Impressions)
class(data3$Impressions)
class(data1$Age)
class(data2$Age)
class(data3$Age)

head(data1)
head(data2)
head(data3)
summary(data1)
summary(data2)
summary(data3)

##Categorize by age
data1$age_group<-cut(data1$Age, c(-Inf,19,29,39,49,59,69,Inf))
head(data1)
summary(data1)

data2$age_group<-cut(data2$Age, c(-Inf,19,29,39,49,59,69,Inf))
head(data2)
summary(data2)

data3$age_group<-cut(data3$Age, c(-Inf,19,29,39,49,59,69,Inf))
head(data3)
summary(data3)

## Click-through-rate
library(ggplot2)
ggplot(data1,aes(x=Impressions, colour=age_group))+geom_density()+labs(title = "Distribution of number of impressions",subtitle="Day 1")
ggplot(data2,aes(x=Impressions, colour=age_group))+geom_density()+labs(title="Distribution of number of impressions",subtitle= "Day 2")
ggplot(data3,aes(x=Impressions, colour=age_group))+geom_density()+labs(title="Distribution of number of impressions",subtitle= "Day 3")

ggplot(subset(data1, Clicks>0),aes(x=Clicks/Impressions, colour=age_group))+geom_density()+labs(title="Distribution of CTR",subtitle= "Day 1")
ggplot(subset(data2, Clicks>0),aes(x=Clicks/Impressions, colour=age_group))+geom_density()+labs(title="Distribution of CTR",subtitle="Day 2")
ggplot(subset(data3, Clicks>0),aes(x=Clicks/Impressions, colour=age_group))+geom_density()+labs(title="Distribution of CTR",subtitle="Day 3")

##Categorize by Clicks behavior
data1$clicks_cat<-NA
data1$clicks_cat[data1$Impressions==0]<-"NoImps"
data1$clicks_cat[data1$Impressions>0]<-"Imps"
data1$clicks_cat[data1$Clicks>0]<-"Clicks"

data2$clicks_cat<-NA
data2$clicks_cat[data2$Impressions==0]<-"NoImps"
data2$clicks_cat[data2$Impressions>0]<-"Imps"
data2$clicks_cat[data2$Clicks>0]<-"Clicks"

data3$clicks_cat<-NA
data3$clicks_cat[data3$Impressions==0]<-"NoImps"
data3$clicks_cat[data3$Impressions>0]<-"Imps"
data3$clicks_cat[data3$Clicks>0]<-"Clicks"

data1$clicks_cat<- as.factor(data1$clicks_cat)
data2$clicks_cat<- as.factor(data2$clicks_cat)
data3$clicks_cat<- as.factor(data3$clicks_cat)

summary(data1)
summary(data2)
summary(data3)

library(doBy)
quant_analysis<-function(x){
  c(length(x),min(x),max(x),mean(x))
}

summaryBy(Impressions~Gender+age_group, data=data1, FUN=quant_analysis)
summaryBy(Impressions~Gender+age_group, data=data2, FUN=quant_analysis)
summaryBy(Impressions~Gender+age_group, data=data3, FUN=quant_analysis)

summaryBy(Impressions~clicks_cat+Signed_In, data=data1, FUN=quant_analysis)
summaryBy(Impressions~clicks_cat+Signed_In, data=data2, FUN=quant_analysis)
summaryBy(Impressions~clicks_cat+Signed_In, data=data3, FUN=quant_analysis)

ggplot(data1,aes(x=age_group, y=Impressions, fill=Gender))+geom_boxplot()+labs(title = "Boxplot of impressions",subtitle="Day 1 - segment: Age Group and Gender")
ggplot(data1,aes(x=clicks_cat, y=Impressions, fill=Signed_In))+geom_boxplot()+labs(title = "Boxplot of impressions",subtitle="Day 1 - segment: Clicks Cat and Signed In")

ggplot(data2,aes(x=age_group, y=Impressions, fill=Gender))+geom_boxplot()+labs(title = "Boxplot of impressions",subtitle="Day 2 - segment: Age Group and Gender")
ggplot(data2,aes(x=clicks_cat, y=Impressions, fill=Signed_In))+geom_boxplot()+labs(title = "Boxplot of impressions",subtitle="Day 2 - segment: Clicks Cat and Signed In")

ggplot(data3,aes(x=age_group, y=Impressions, fill=Gender))+geom_boxplot()+labs(title = "Boxplot of impressions",subtitle="Day 3 - segment: Age Group and Gender")
ggplot(data3,aes(x=clicks_cat, y=Impressions, fill=Signed_In))+geom_boxplot()+labs(title = "Boxplot of impressions",subtitle="Day 3 - segment: Clicks Cat and Signed In")

## Analysis across days
summary(data1)
summary(data2)
summary(data3)

data1$weekday<-1
data2$weekday<-2
data3$weekday<-3
data_tot<-rbind(data1,data2,data3)
data_tot$weekday<-as.factor(data_tot$weekday)

summary(data_tot)
quant_analysis2<-function(x){
  c(min(x),max(x),mean(x))
}

summaryBy(Impressions~weekday, data=data_tot, FUN=quant_analysis2)
summaryBy(Impressions~weekday+Signed_In, data=data_tot, FUN=quant_analysis2)
summaryBy(Impressions~weekday+clicks_cat, data=data_tot, FUN=quant_analysis2)
summaryBy(Impressions~weekday+Gender, data=data_tot, FUN=quant_analysis2)
summaryBy(Age~weekday, data=data_tot,FUN=quant_analysis2)


ggplot(data_tot,aes(x=clicks_cat,y=Impressions, fill=weekday))+geom_boxplot()+labs(title="Boxplot of Impressions",subtitle= "segment: Clicks Cat and Weekday")
ggplot(data_tot,aes(x=Signed_In,y=Impressions, fill=weekday))+geom_boxplot()+labs(title="Boxplot of Impressions",subtitle= "segment:  Signed In and Weekday")
ggplot(data_tot,aes(x=Gender,y=Impressions, fill=weekday))+geom_boxplot()+labs(title="Boxplot of Impressions",subtitle= "segment: Gender and Weekday")
ggplot(data_tot,aes(x=weekday,y=Age, fill=weekday))+geom_boxplot()+labs(title="Boxplot of Age",subtitle= "segment: Weekday")
ggplot(subset(data_tot, Clicks>0),aes(x=Clicks/Impressions, colour=weekday))+geom_density()+labs(title="Distribution of CTR",subtitle= "segment: Weekday")


