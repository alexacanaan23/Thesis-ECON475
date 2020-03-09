#Alexa Canaan
#Metrics/Thesis DRAFT

#read AHS data
dat.n<-read.csv("~/Desktop/ahs2013n.csv")
dat.m<-read.csv("~/Desktop/ahs2013m.csv")

#test cleaning the data
dat.nt<-dat.n[1:100, ]
class(dat.nt$HHSEX)
typeof(dat.nt$HHSEX)
attributes(dat.nt$HHSEX)
head(dat.nt$HHSEX)

#Y var
class(dat.n$PTPUBTRN)
typeof(dat.n$PTPUBTURN)
attributes(dat.n$PTPUBTRN)
head(dat.n$PTPUBTRN)
summary(dat.n$PTPUBTRN)
as.character(dat.n$PTPUBTRN)
as.numeric(dat.n$PTPUBTRN)

dat.n$PTPUBTRN <- factor(dat.n$PTPUBTRN, labels = c("NA", "NA", "NA", "1", "2"))

dat.n.omit<-na.omit(dat.n)

#small model
mod1<-lm(formula= as.numeric(PTPUBTRN) ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB, data = dat.n.omit)
summary(mod1)
dat.n.omit$PTPUBTRN[as.numeric(dat.n.omit$PTPUBTRN)==2]<-0
mod2<-glm(formula= as.numeric(PTPUBTRN) ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB, data = dat.n.omit, family = "binomial")
summary(mod2)
#mod + neighborhood amenities
mod3<-lm(formula= as.numeric(PTPUBTRN) ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB + DRUGSTORE + GROCERY, data = dat.n.omit)
mod4<-glm(formula= as.numeric(PTPUBTRN) ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB, data = dat.n.omit, family = "binomial")

#metro
class(dat.n$METRO3)
typeof(dat.n$METRO3)
attributes(dat.n$METRO3)
head(dat.n$METRO3)
summary(dat.n$METRO3)
as.character(dat.n$METRO3)
as.numeric(dat.n$METRO3)
dat.n$METRO3 <- factor(dat.n$METRO3, labels = c("1", "2", "3", "4", "5"))

#cars
class(dat.n$CARS)
typeof(dat.n$CARS)
attributes(dat.n$CARS)
head(dat.n$CARS)
summary(dat.n$CARS)
as.character(dat.n$CARS)
as.numeric(dat.n$CARS)
dat.n$CARS <- factor(dat.n$CARS, labels = c("NA", "0", "1", "2", "3", "4", "5"))

#trucks
class(dat.n$TRUCKS)
typeof(dat.n$TRUCKS)
attributes(dat.n$TRUCKS)
head(dat.n$TRUCKS)
summary(dat.n$TRUCKS)
as.character(dat.n$TRUCKS)
as.numeric(dat.n$TRUCKS)
dat.n$TRUCKS <- factor(dat.n$TRUCKS, labels = c("NA", "0", "1", "2", "3", "4", "5"))

#Zinc2
class(dat.n$ZINC2)
typeof(dat.n$ZINC2)
attributes(dat.nt$ZINC2)
head(dat.n$ZINC2)
summary(dat.n$ZINC2)
as.character(dat.n$ZINC2)
as.numeric(dat.n$ZINC2)

#ptdisbus
class(dat.n$PTDISBUS)
typeof(dat.n$PTDISBUS)
attributes(dat.n$PTDISBUS)
head(dat.n$PTDISBUS)
summary(dat.n$PTDISBUS)
as.character(dat.n$PTDISBUS)
as.numeric(dat.n$pt)
dat.n$PTDISBUS <- factor(dat.n$PTDISBUS, labels = c("NA", "NA", "NA", "1", "2", "3", "4"))

#ptdispub
class(dat.n$PTDISPUB)
typeof(dat.n$PTDISPUB)
attributes(dat.n$PTDISPUB)
head(dat.n$PTDISPUB)
summary(dat.n$PTDISPUB)
as.character(dat.n$PTDISPUB)
as.numeric(dat.n$PTDISPUB)
dat.n$PTDISPUB <- factor(dat.n$PTDISPUB, labels = c("NA", "NA", "NA", "1", "2", "3", "4"))

#ptdisrail
class(dat.n$PTDISRAIL)
typeof(dat.n$PTDISRAIL)
attributes(dat.n$PTDISRAIL)
head(dat.n$PTDISRAIL)
summary(dat.n$PTDISRAIL)
as.character(dat.n$PTDISRAIL)
as.numeric(dat.n$PTDISRAIL)
dat.n$PTDISRAIL <- factor(dat.n$PTDISRAIL, labels = c("NA", "NA", "1", "2", "3", "4"))

#ptdisshut
class(dat.n$PTDISSHUT)
typeof(dat.n$PTDISSHUT)
attributes(dat.n$PTDISSHUT)
head(dat.n$PTDISSHUT)
summary(dat.n$PTDISSHUT)
as.character(dat.n$PTDISSHUT)
as.numeric(dat.n$PTDISSHUT)
dat.n$PTDISSHUT <- factor(dat.n$PTDISSHUT, labels = c("NA", "NA", "NA", "1", "2", "3", "4"))

#ptdissub
class(dat.n$PTDISSUB)
typeof(dat.n$PTDISSUB)
attributes(dat.n$PTDISSUB)
head(dat.n$PTDISSUB)
summary(dat.n$PTDISSUB)
as.character(dat.n$PTDISSUB)
as.numeric(dat.n$PTDISSUB)
dat.n$PTDISSUB <- factor(dat.n$PTDISSUB, labels = c("NA", "NA", "NA", "1", "2", "3", "4"))

#drugstore
class(dat.n$DRUGSTORE)
typeof(dat.n$DRUGSTORE)
attributes(dat.n$DRUGSTORE)
head(dat.n$DRUGSTORE)
summary(dat.n$DRUGSTORE)
as.character(dat.n$DRUGSTORE)
as.numeric(dat.n$DRUGSTORE)
dat.n$DRUGSTORE <- factor(dat.n$DRUGSTORE, labels = c("NA", "NA", "NA", "1", "2"))

#grocery
class(dat.n$GROCERY)
typeof(dat.n$GROCERY)
attributes(dat.n$GROCERY)
head(dat.n$GROCERY)
summary(dat.n$GROCERY)
as.character(dat.n$GROCERY)
as.numeric(dat.n$GROCERY)
dat.n$GROCERY <- factor(dat.n$GROCERY, labels = c("NA", "NA", "NA", "1", "2", "3"))

#wmclos
class(dat.n$WMCLOS)
typeof(dat.n$WMCLOS)
attributes(dat.n$WMCLOS)
head(dat.n$WMCLOS)
summary(dat.n$WMCLOS)
as.character(dat.n$WMCLOS)
as.numeric(dat.n$WMCLOS)
dat.n$WMCLOS <- factor(dat.n$WMCLOS, labels = c("NA", "NA", "NA", "NA", "1", "2"))

#wntran
class(dat.n$WNTRAN)
typeof(dat.n$WNTRAN)
attributes(dat.n$WNTRAN)
head(dat.n$WNTRAN)
summary(dat.n$WNTRAN)
as.character(dat.n$WNTRAN)
as.numeric(dat.n$WNTRAN)
dat.n$WNTRAN <- factor(dat.n$WNTRAN, labels = c("NA", "NA", "NA", "NA", "1", "2"))



#sex
dat.nt$HHSEX <- factor(dat.nt$HHSEX, labels = c("NA", "1", "2"))
as.numeric(as.character(dat.nt$HHSEX))
summary(dat.nt$HHSEX)

#age
class(dat.nt$HHAGE)
typeof(dat.nt$HHAGE)
library(dplyr)
na_if(dat.nt$HHAGE, -6)
dat.nt[dat.nt$HHAGE<=0]<-NA
summary(dat.nt$HHAGE)

for (i in length(dat.nt$HHSEX)) {
  if (dat.nt$HHSEX[i]==-6) {
    dat.nt[-c(i),]
  }
}

dat.m<-read.csv("~/Desktop/ahs2013m.csv")

#preliminary plots
library(ggplot2)
library(gridExtra)

#sex
ggdat<-data.frame(Sex=dat.nt$HHSEX)
ggplot(data=ggdat,aes(x=Sex,fill=Sex))+
  geom_bar()+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Sex")+
  ylab("Count")+
  ggtitle("National Sex Demographic")
  
#age
ggdat<-data.frame(Age=dat.nt$HHAGE)
ggplot(data=ggdat,aes(x=Age))+
  geom_histogram(color="black", fill="purple")+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Age")+
  ylab("Count")+
  ggtitle("National Age Demographic")

#marital status
ggdat<-data.frame(Marital_Status=dat.n$HHMAR)
ggplot(data=ggdat,aes(x=Marital_Status, fill=Marital_Status))+
  geom_bar()+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Marital Status")+
  ylab("Count")+
  ggtitle("National Marital Status Demographic")

#race
ggdat<-data.frame(Race=dat.n$HHRACE)
ggplot(data=ggdat,aes(x=Race, fill=Race))+
  geom_bar()+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Race")+
  ylab("Count")+
  ggtitle("National Racial Demographic")

#education level
ggdat<-data.frame(EDU=dat.n$HHGRAD)
ggplot(data=ggdat,aes(x=EDU))+
  geom_histogram(color="black", fill="pink")+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Education Level (in Years)")+
  ylab("Count")+
  ggtitle("National Education Level Demographic")+
  theme(legend.title = element_blank())

#household income
ggdat<-data.frame(Income=dat.n$ZINC2)
ggplot(data=ggdat,aes(x=Income))+
  geom_histogram(color="black", fill="blue")+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Household Income")+
  ylab("Count")+
  ggtitle("National Household Income Demographic")+
  theme(legend.title = element_blank())

#current value of unit
ggdat<-data.frame(Value=dat.n$PVALUE)
ggplot(data=ggdat,aes(x=Value))+
  geom_histogram(color="black", fill="Green")+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Current Value of Housing Unit")+
  ylab("Count")+
  ggtitle("National Spread of Current Value of Housing Units")+
  theme(legend.title = element_blank())

#public transportation variables

#monthly household spending on public transportation
ggdat<-data.frame(MonthlyCost=dat.n$PTCOSTPTR)
ggplot(data=ggdat,aes(x=MonthlyCost))+
  geom_histogram(color="black", fill="Green")+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Monthly Cost of Public Transportation per Household")+
  ylab("Count")+
  ggtitle("National Monthly Cost of Public Transportation per Household")+
  theme(legend.title = element_blank())

#household uses public transportation
ggdat<-data.frame(Use=dat.n$PTPUBTRN)
ggplot(data=ggdat,aes(x=Use, fill=Use))+
  geom_bar()+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Indicator of Public Transportation Use")+
  ylab("Count")+
  ggtitle("National Indicator of Public Transportation Use")

#how often public transportation is used
ggdat<-data.frame(Frequency=dat.n$PTWKSCHL)
ggplot(data=ggdat,aes(x=Frequency, fill=Frequency))+
  geom_bar()+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Indicator of Frequency Public Transportation Use")+
  ylab("Count")+
  ggtitle("National Indicator of Frequency of Public Transportation Use")


#LASSO - load the library
library(glmnet)
library(tidyverse)
library(caret)
#remove NAs
dat.n2<- na.omit(dat.n)
#inspect the data
sample_n(dat.n2, 3)

#split the data into training and test set
set.seed(69)
training.sample <- dat.n$PTPUBTRN %>%
  createDataPartition(p = 0.8, list=FALSE)
train.data <- dat.n2[training.sample, ]
test.data <-dat.n2[-training.sample, ]

#dummy code categorical predictor variables
x <- model.matrix(PTPUBTRN~., train.data)[,-1]
y <- ifelse(train.data$PTPUBTRN == "pos", 1, 0)

glmnet(x, y, family = "binomial", alpha = 1, lambda = NULL)
