#Alexa Canaan
#Metrics/Thesis Code

#read AHS data
dat.n<-read.csv("~/Desktop/ahs2013n.csv")
dat.m<-read.csv("~/Desktop/ahs2013m.csv")

#preliminary plots
library(ggplot2)
library(gridExtra)

#sex
ggdat<-data.frame(Sex=dat.n$HHSEX)
ggplot(data=ggdat,aes(x=Sex,fill=Sex))+
  geom_bar()+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Sex")+
  ylab("Count")+
  ggtitle("National Sex Demographic")
  
#age
subset(dat.n$HHAGE, dat.n$HHAGE >= 0)
ggdat<-data.frame(Age=dat.n$HHAGE)
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