## Example 1

getwd() 
data1 <- read.csv("nyt1.csv")
dim(data1)
length(data1)
length(data1$Age)
#categorize
head(data1)
tail(data1)
summary(data1)

data1$Gender = as.factor(data1$Gender)
summary(data1)

data1$agecat <- cut(data1$Age, c(-Inf,0,18,24,34,44,54,64,Inf))
data1$agecat
#view
summary(data1)
#brakets

library("doBy")
siterange <- function(x){c(length(x),min(x),mean(x),max(x))}


summaryBy(Age~agecat, data=data1, FUN=siterange)
# only signed in users have ages and genders
head(data1)
View(data1)

data1[1,]
data1[,1]
data1[1:10,1]
data1[c(1,20,50),]

data1[data1$Age<=0,]

summaryBy(Gender+Signed_In+Impressions+Clicks~agecat, data=data1)
#plot
library("ggplot2")
ggplot(data1,aes(x=agecat, y=Impressions, fill=agecat))+geom_boxplot()
# create click thru rate
# we don't consider clicks if there are no impressions

data1$hasimp <- cut(data1$Impressions,c(-Inf,0,Inf))
head(data1)
summaryBy(Clicks~hasimp, data=data1, FUN=siterange)
ggplot(subset(data1, Impressions >0),aes(x=Clicks/Impressions,colour=agecat))+geom_density()
ggplot(subset(data1, Clicks >0),aes(x=Clicks/Impressions,colour=agecat))+geom_density()
ggplot(subset(data1, Clicks >0),aes(x=agecat, y=Clicks,fill=agecat))+geom_boxplot()
ggplot(subset(data1, Clicks >0),aes(x=Clicks,colour=agecat))+geom_density()
# create categories
data1$scode[data1$Impressions==0] <- "NoImps"
data1$scode[data1$Impressions >0] <- "Imps"
data1$scode[data1$Clicks >0] <- "Clicks"

# Covert the column to a factor
data1$scode <- factor(data1$scode)
head(data1)

# look at levels
clen <- function(x){c(length(x))}
etable <- summaryBy(Impressions~scode+Gender+agecat, data=data1, FUN=clen)
etable

## Example 2
#install.packages('xlsx')
library('xlsx')
#res <- read.xlsx("rollingsales_brooklyn.xls", 1)
#res <- read.xlsx("rollingsales_brooklyn.xls", 1, startRow=5)
#summary(res)

#install.packages('plyr')
library('plyr') 

library('gdata')
#setwd("C:/dragos/SIT/TEACHING/Spring_2016/FE582/Lectures/Lecture_1/Lecture_1_R_code/")
getwd()
#bk <- read.xls("rollingsales_brooklyn.xls",perl = "C:\\Perl64\\bin\\perl.exe", pattern="BOROUGH")

#Using PERL
#bk <- read.xls("rollingsales_brooklyn.xls", pattern="BOROUGH")

#No PERL
#bk <- read.xlsx2("rollingsales_brooklyn.xls", 1, startRow=5) 

library(openxlsx)
bk <- read.xlsx("rollingsales_brooklyn.xlsx", 1, startRow=5)

head(bk)
tail(bk)
summary(bk)

unique(bk$BUILDING.CLASS.CATEGORY)

bk$SALE.PRICE
summary(bk$SALE.PRICE)
bk$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","",bk$SALE.PRICE))
summary(bk$SALE.PRICE.N)

bk$SALE.PRICE.N
count(is.na(bk$SALE.PRICE.N))

selection = bk[bk$SALE.PRICE.N==0,]
dim(selection)

length(bk[bk$SALE.PRICE.N==0,][,1])/length(bk[,1])

names(bk)
names(bk) <- tolower(names(bk))
names(bk)

unique(bk$building.class.category)

## clean/format the data with regular expressions
bk$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",bk$gross.square.feet))
bk$gross.sqft

bk$land.sqft <- as.numeric(gsub("[^[:digit:]]","",bk$land.square.feet))
bk$land.sqft


library('openxlsx')
#bk$sale.date <- as.Date(bk$sale.date)
#bk$sale.date <- as.Date(as.numeric(bk$sale.date),origin="1899-12-30")
bk$sale.date <- convertToDateTime(as.numeric(bk$sale.date))
bk$sale.date

bk$year.built <- as.numeric(as.character(bk$year.built))
bk$year.built

## do a bit of exploration to make sure the data is ok

attach(bk)

hist(sale.price.n)
summary(sale.price.n)
hist(sale.price.n[sale.price.n>0])
sale.price.n[sale.price.n>0]
hist(gross.sqft[sale.price.n==0])

detach(bk)

## keep only the actual sales
bk[bk$sale.price.n!=0,]
bk.sale <- bk[bk$sale.price.n!=0,]
plot(bk.sale$gross.sqft,bk.sale$sale.price.n)
plot(log(bk.sale$gross.sqft),log(bk.sale$sale.price.n))

## for now, let's look at 1, 2, and 3 family homes
bk.sale[1:5,]
bk.sale[c(1,3),]

bk.homes <- bk.sale [which(grepl("FAMILY",bk.sale$building.class.category)),]
plot(log(bk.homes$gross.sqft),log(bk.homes$sale.price.n))

bk.homes[which(bk.homes$sale.price.n<100000),][order(bk.homes[which(bk.homes$sale.price.n<100000),]$sale.price.n),]

#remove outliers that seem like they weren't actual sales

bk.homes$outliers <- (log(bk.homes$sale.price.n) <= 5) + 0
bk.homes1 <- bk.homes[which(bk.homes$outlier==0 & bk.homes$gross.sqft>0),]
summary(bk.homes1)

plot(log(bk.homes1$gross.sqft),log(bk.homes1$sale.price.n))

model1=lm(log(bk.homes1$sale.price.n)~log(bk.homes1$gross.sqft))
summary(model1)

plot(log(bk.homes1$gross.sqft), log(bk.homes1$sale.price.n))
abline(model1, col='red', lwd=2)

