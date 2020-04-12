#Alexa Canaan
#Metrics/Thesis Code

#read AHS data
dat.n<-read.csv("~/Desktop/ahs2013n.csv")

#DATA CLEANING - BASIC

#outcome variable = indicator of pub trans use
class(dat.n$PTPUBTRN)
typeof(dat.n$PTPUBTURN)
attributes(dat.n$PTPUBTRN)
head(dat.n$PTPUBTRN)
summary(dat.n$PTPUBTRN)
#change to true indicator between 0 and 1 where 1 = yes and 0 = no
dat.n$PTPUBTRN <- factor(dat.n$PTPUBTRN, labels = c(NA, NA, NA, "1", "0"))

#metro
class(dat.n$METRO3)
typeof(dat.n$METRO3)
attributes(dat.n$METRO3)
head(dat.n$METRO3)
summary(dat.n$METRO3)
dat.n$METRO3 <- factor(dat.n$METRO3, labels = c("1", "2", "3", "2", "3"))

#cars
class(dat.n$CARS)
typeof(dat.n$CARS)
attributes(dat.n$CARS)
head(dat.n$CARS)
summary(dat.n$CARS)
dat.n$CARS <- factor(dat.n$CARS, labels = c(NA, "0", "1", "2", "3", "4", "5"))

#trucks
class(dat.n$TRUCKS)
typeof(dat.n$TRUCKS)
attributes(dat.n$TRUCKS)
head(dat.n$TRUCKS)
summary(dat.n$TRUCKS)
dat.n$TRUCKS <- factor(dat.n$TRUCKS, labels = c(NA, "0", "1", "2", "3", "4", "5"))

#Zinc2
class(dat.n$ZINC2)
typeof(dat.n$ZINC2)
attributes(dat.nt$ZINC2)
head(dat.n$ZINC2)
summary(dat.n$ZINC2)
dat.n$ZINC2[dat.n$ZINC2 == -6] <- NA

#ptdisbus
class(dat.n$PTDISBUS)
typeof(dat.n$PTDISBUS)
attributes(dat.n$PTDISBUS)
head(dat.n$PTDISBUS)
summary(dat.n$PTDISBUS)
dat.n$PTDISBUS <- factor(dat.n$PTDISBUS, labels = c(NA, NA, NA, "1", "2", "3", "4"))

#ptdispub
class(dat.n$PTDISPUB)
typeof(dat.n$PTDISPUB)
attributes(dat.n$PTDISPUB)
head(dat.n$PTDISPUB)
summary(dat.n$PTDISPUB)
dat.n$PTDISPUB <- factor(dat.n$PTDISPUB, labels = c(NA, NA, NA, "1", "2", "3", "4"))

#ptdisrail
class(dat.n$PTDISRAIL)
typeof(dat.n$PTDISRAIL)
attributes(dat.n$PTDISRAIL)
head(dat.n$PTDISRAIL)
summary(dat.n$PTDISRAIL)
dat.n$PTDISRAIL <- factor(dat.n$PTDISRAIL, labels = c(NA, NA, "1", "2", "3", "4"))

#ptdisshut
class(dat.n$PTDISSHUT)
typeof(dat.n$PTDISSHUT)
attributes(dat.n$PTDISSHUT)
head(dat.n$PTDISSHUT)
summary(dat.n$PTDISSHUT)
dat.n$PTDISSHUT <- factor(dat.n$PTDISSHUT, labels = c(NA, NA, NA, "1", "2", "3", "4"))

#ptdissub
class(dat.n$PTDISSUB)
typeof(dat.n$PTDISSUB)
attributes(dat.n$PTDISSUB)
head(dat.n$PTDISSUB)
summary(dat.n$PTDISSUB)
dat.n$PTDISSUB <- factor(dat.n$PTDISSUB, labels = c(NA, NA, NA, "1", "2", "3", "4"))

#DATA CLEANING - MODEL LEVEL 2
#wntran
class(dat.n$WNTRAN)
typeof(dat.n$WNTRAN)
attributes(dat.n$WNTRAN)
head(dat.n$WNTRAN)
summary(dat.n$WNTRAN)
dat.n$WNTRAN <- factor(dat.n$WNTRAN, labels = c(NA, NA, NA, NA, "1", "0"))

#wnamen
class(dat.n$WNAMEN)
typeof(dat.n$WNAMEN)
attributes(dat.n$WNAMEN)
head(dat.n$WNAMEN)
summary(dat.n$WNAMEN)
dat.n$WNAMEN <- factor(dat.n$WNAMEN, labels = c(NA, NA, NA, NA, "1", "0"))

#wnjob
class(dat.n$WNJOB)
typeof(dat.n$WNJOB)
attributes(dat.n$WNJOB)
head(dat.n$WNJOB)
summary(dat.n$WNJOB)
dat.n$WNJOB <- factor(dat.n$WNJOB, labels = c(NA, NA, NA, NA, "1", "0"))

#DATA CLEANING - SUMMARY STATISTICS
#sex
class(dat.n$HHSEX)
typeof(dat.n$HHSEX)
attributes(dat.n$HHSEX)
head(dat.n$HHSEX)
summary(dat.n$HHSEX)
dat.n$HHSEX <- factor(dat.n$HHSEX, labels = c(NA, "0", "1"))

#age
class(dat.n$HHAGE)
typeof(dat.n$HHAGE)
attributes(dat.n$HHAGE)
head(dat.n$HHAGE)
summary(dat.n$HHAGE)
dat.n$HHAGE[dat.n$HHAGE == -6] <- NA

#marriage
class(dat.n$HHMAR)
typeof(dat.n$HHMAR)
attributes(dat.n$HHMAR)
head(dat.n$HHMAR)
summary(dat.n$HHMAR)
dat.n$HHMAR <- factor(dat.n$HHMAR, labels = c(NA, "1", "2", "3", "4", "5", "6"))

#race
class(dat.n$HHRACE)
typeof(dat.n$HHRACE)
attributes(dat.n$HHRACE)
head(dat.n$HHRACE)
summary(dat.n$HHRACE)
dat.n$HHRACE[dat.n$HHRACE == "-6"] <- NA

#grad
class(dat.n$HHGRAD)
typeof(dat.n$HHGRAD)
attributes(dat.n$HHGRAD)
head(dat.n$HHGRAD)
summary(dat.n$HHGRAD)
dat.n$HHGRAD[dat.n$HHGRAD == -6] <- NA

#remove NA's
dat.n2<-na.omit(dat.n)

#remove variables that have less than 2 factors
for (i in names(dat.n2)) {
  if (nlevels(dat.n2[[i]]) < 2 & is.factor(dat.n2[[i]])==TRUE) {
    dat.n2[[i]]<-NULL
  }
}

#LARGE ANALYSIS N=60096###################################################
#only omitted full NA obs
dat.t<-dat.n2

#SUMMARY STATISTICS FOR POPULATION
library(skimr)
library(dplyr)
sum_stats_y<-filter(dat.t, PTPUBTRN==1)
sum_stats_y_pop<-data.frame(Sex=sum_stats_y$HHSEX, Age=sum_stats_y$HHAGE, Marriage=sum_stats_y$HHMAR, Race=sum_stats_y$HHRACE, Grad=sum_stats_y$HHGRAD, Income=sum_stats_y$ZINC2)
sum_stats_gen_pop<-data.frame(Sex=dat.t$HHSEX, Age=dat.t$HHAGE, Marriage=dat.t$HHMAR, Race=dat.t$HHRACE, Grad=dat.t$HHGRAD, Income=dat.t$ZINC2)
skim(sum_stats_y_pop)
skim(sum_stats_gen_pop)
#SUMMARY STATISTICS FOR PUBLIC TRANSPORTATION
sum_stats_y_pt<-data.frame(Metro=sum_stats_y$METRO3, Cars=sum_stats_y$CARS, Trucks=sum_stats_y$TRUCKS, Bus=sum_stats_y$PTDISBUS, Public_Transportation=sum_stats_y$PTDISPUB, Rail=sum_stats_y$PTDISRAIL, Shuttle=sum_stats_y$PTDISSHUT, Subway=sum_stats_y$PTDISSUB, Move_Public_Transport=sum_stats_y$WNTRAN, Move_Amenities=sum_stats_y$WNAMEN, Move_Job=sum_stats_y$WNJOB)
sum_stats_gen_pt<-data.frame(Metro=dat.t$METRO3, Cars=dat.t$CARS, Trucks=dat.t$TRUCKS, Bus=dat.t$PTDISBUS, Public_Transportation=dat.t$PTDISPUB, Rail=dat.t$PTDISRAIL, Shuttle=dat.t$PTDISSHUT, Subway=dat.t$PTDISSUB, Move_Public_Transport=dat.t$WNTRAN, Move_Amenities=dat.t$WNAMEN, Move_Job=dat.t$WNJOB)
skim(sum_stats_y_pt)
skim(sum_stats_gen_pt)

#REGRESSION ANALYSIS

#BASE model
#ols
mod1<-lm(formula= as.numeric(PTPUBTRN) ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB, data = dat.t)
summary(mod1)

#BASE + neighborhood amenities
#ols
mod3<-lm(formula= as.numeric(PTPUBTRN) ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB  + WNAMEN + WNJOB + WNTRAN, data = dat.t)
summary(mod3)

#BASE model
#logit 
dat.t$PTPUBTRN<-factor(dat.t$PTPUBTRN, labels = c(1, 0))
mod2<-glm(formula= PTPUBTRN ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB, data = dat.t, family = "binomial"(link = "logit"))
summary(mod2)

#BASE + neighborhood amenities
#logit
mod4<-glm(formula= PTPUBTRN ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB + WNAMEN + WNJOB + WNTRAN, data = dat.t, family = "binomial"(link = "logit"))
summary(mod4)

#produce regression output
library(stargazer)
stargazer(mod1, mod3, mod2, mod4, type = "text",
          dep.var.labels = c("Public Transport Use", "Public Transport Use"),
          out = "regmodels.txt")
stargazer(mod1, mod3, mod2, mod4, type = "html",
          dep.var.labels = c("Public Transport Use", "Public Transport Use"),
          out = "regmodels.htm")


#CLASSIFICATION TREE
#split the data into training and test set
reg.dat<-data.frame(sum_stats_gen_pt,CONTROL=dat.t$CONTROL, PTPUBTRN=dat.t$PTPUBTRN)
set.seed(4)
training.sample <- reg.dat$PTPUBTRN %>%
  createDataPartition(p = 0.8, list=FALSE)
train.data <- reg.dat[training.sample, ]
test.data <-reg.dat[-training.sample, ]
test.data <-data.frame(test.data)
y.test <-test.data$PTPUBTRN

library(tree)
#fit a classification tree
tree.ptpubtrn=tree(PTPUBTRN~.-CONTROL,data = reg.dat)
summary(tree.ptpubtrn)
plot(tree.ptpubtrn)
text(tree.ptpubtrn, pretty=0)
tree.ptpubtrn

tree.ptpubtrn.train=tree(PTPUBTRN~.-CONTROL,data = train.data)
tree.ptpubtrn.pred=predict(tree.ptpubtrn.train,test.data,type="class")
table(tree.ptpubtrn.pred, y.test)
(1309+4551)/(1309+4551+55)
#correct predictions for around 99.1% of locations in test data

cv.ptpubtrn=cv.tree(tree.ptpubtrn.train,FUN=prune.misclass)
names(cv.ptpubtrn)
cv.ptpubtrn
#dev is the cross-validation error rate 
#plot error rate as a fn of size and k folds
par(mfrow=c(1,2))
plot(cv.ptpubtrn$size, cv.ptpubtrn$dev, type="b")
plot(cv.ptpubtrn$k, cv.ptpubtrn$dev, type="b")

#apply prune.misclass to prune tree to lowest error rate
prune.ptpubtrn = prune.misclass(tree.ptpubtrn.train, best=6)
plot(prune.ptpubtrn)
text(prune.ptpubtrn, pretty=0)

#test pruned tree performance
tree.ptpubtrn.test=predict(prune.ptpubtrn, test.data, type = "class")
o<-table(tree.ptpubtrn.test, y.test)

# N=29580 #############################################################################
dat.t1<-subset(dat.t, dat.t$PTPUBTRN==1 | dat.t$PTPUBTRN==0)

#SUMMARY STATISTICS FOR POPULATION
library(skimr)
library(dplyr)
sum_stats_y<-filter(dat.t1, PTPUBTRN==1)
sum_stats_y_pop<-data.frame(Sex=sum_stats_y$HHSEX, Age=sum_stats_y$HHAGE, Marriage=sum_stats_y$HHMAR, Race=sum_stats_y$HHRACE, Grad=sum_stats_y$HHGRAD, Income=sum_stats_y$ZINC2)
sum_stats_gen_pop<-data.frame(Sex=dat.t1$HHSEX, Age=dat.t1$HHAGE, Marriage=dat.t1$HHMAR, Race=dat.t1$HHRACE, Grad=dat.t1$HHGRAD, Income=dat.t1$ZINC2)
skim(sum_stats_y_pop)
skim(sum_stats_gen_pop)
#SUMMARY STATISTICS FOR PUBLIC TRANSPORTATION
sum_stats_y_pt<-data.frame(Metro=sum_stats_y$METRO3, Cars=sum_stats_y$CARS, Trucks=sum_stats_y$TRUCKS, Bus=sum_stats_y$PTDISBUS, Public_Transportation=sum_stats_y$PTDISPUB, Rail=sum_stats_y$PTDISRAIL, Shuttle=sum_stats_y$PTDISSHUT, Subway=sum_stats_y$PTDISSUB, Move_Public_Transport=sum_stats_y$WNTRAN, Move_Amenities=sum_stats_y$WNAMEN, Move_Job=sum_stats_y$WNJOB)
sum_stats_gen_pt<-data.frame(Metro=dat.t1$METRO3, Cars=dat.t1$CARS, Trucks=dat.t1$TRUCKS, Public_Transport=dat.t1$PTDISPUB, Bus=dat.t1$PTDISBUS, Rail=dat.t1$PTDISRAIL, Shuttle=dat.t1$PTDISSHUT, Subway=dat.t1$PTDISSUB, Move_Public_Transport=dat.t1$WNTRAN, Move_Amenities=dat.t1$WNAMEN, Move_Job=dat.t1$WNJOB)
skim(sum_stats_y_pt)
skim(sum_stats_gen_pt)

#REGRESSION ANALYSIS

#BASE model
#ols
mod1<-lm(formula= as.numeric(PTPUBTRN) ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB, data = dat.t1)
summary(mod1)

#BASE + neighborhood amenities
#ols
mod3<-lm(formula= as.numeric(PTPUBTRN) ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB  + WNAMEN + WNJOB + WNTRAN, data = dat.t1)
summary(mod3)

#BASE model
#logit 
dat.t1$PTPUBTRN<-factor(dat.t1$PTPUBTRN, labels = c(1, 0))
mod2<-glm(formula= PTPUBTRN ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB, data = dat.t1, family = "binomial"(link = "logit"))
summary(mod2)

#BASE + neighborhood amenities
#logit
mod4<-glm(formula= PTPUBTRN ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB + WNAMEN + WNJOB + WNTRAN, data = dat.t1, family = "binomial"(link = "logit"))
summary(mod4)

#produce regression output
library(stargazer)
stargazer(mod1, mod3, mod2, mod4, type = "text",
          dep.var.labels = c("Public Transport Use", "Public Transport Use"),
          out = "regmodels1.txt")
stargazer(mod1, mod3, mod2, mod4, type = "html",
          dep.var.labels = c("Public Transport Use", "Public Transport Use"),
          out = "regmodels1.htm")

#CLASSIFICATION TREE
#split the data into training and test set
reg.dat<-data.frame(sum_stats_gen_pt,CONTROL=dat.t1$CONTROL, PTPUBTRN=dat.t1$PTPUBTRN)
training.sample <- reg.dat$PTPUBTRN %>%
  createDataPartition(p = 0.8, list=FALSE)
train.data <- reg.dat[training.sample, ]
test.data <-reg.dat[-training.sample, ]
test.data <-data.frame(test.data)
y.test <-test.data$PTPUBTRN

library(tree)
#fit a classification tree
tree.ptpubtrn=tree(PTPUBTRN~.-CONTROL,data = reg.dat)
summary(tree.ptpubtrn)
par(mfrow=c(1,1))
plot(tree.ptpubtrn)
text(tree.ptpubtrn, pretty=0)
tree.ptpubtrn

tree.ptpubtrn.train=tree(PTPUBTRN~.-CONTROL,data = train.data)
tree.ptpubtrn.pred=predict(tree.ptpubtrn.train,test.data,type="class")
table(tree.ptpubtrn.pred, y.test)
(1325+4551)/(1310+4551+54)
#correct predictions for around 99.1% of locations in test data

cv.ptpubtrn=cv.tree(tree.ptpubtrn.train,FUN=prune.misclass)
names(cv.ptpubtrn)
cv.ptpubtrn
#dev is the cross-validation error rate 
#plot error rate as a fn of size and k folds
par(mfrow=c(1,2))
plot(cv.ptpubtrn$size, cv.ptpubtrn$dev, type="b")
plot(cv.ptpubtrn$k, cv.ptpubtrn$dev, type="b")

#apply prune.misclass to prune tree to lowest error rate
prune.ptpubtrn = prune.misclass(tree.ptpubtrn.train, best=6)
par(mfrow=c(1,1))
plot(prune.ptpubtrn)
text(prune.ptpubtrn, pretty=0)
title(main = "Pruned Classification Tree")

#test pruned tree performance
tree.ptpubtrn.test=predict(prune.ptpubtrn, test.data, type = "class")
table(tree.ptpubtrn.test, y.test)
(1313+4551)/(1310+4551+54)
# FIGURES ###############################################################
library(ggplot2)
Use<-prop.table(table(reg.dat$PTPUBTRN))
ggdat<-data.frame(Use)
ggdat<-na.omit(ggdat)
names(ggdat)<-c("Use", "Frequency")
ggdat$Use<-factor(ggdat$Use, levels = c(1,0), labels = c("Yes","No"))
ggplot(data=ggdat,aes(x=Use, y=Frequency, fill=Use))+
  geom_bar(stat="identity")+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Indicator of Public Transportation Use")+
  ylab("Count")+
  ggtitle("National Indicator of Public Transportation Use")

reg.dat.1<-subset(reg.dat, PTPUBTRN==1)

#metro
#MSA - metropolitan statistical area
metro<-prop.table(table(reg.dat$Metro))
metro<-as.data.frame(metro)
metro<-prop.table(table(reg.dat.1$Metro))
ggdat<-data.frame(Metro=metro)
ggdat<-na.omit(ggdat)
names(ggdat)<-c("Metro", "Frequency")
ggdat$Metro<-factor(ggdat$Metro, levels = c(1,2,3), labels = c("Central City", "Greater Metropolitan Area", "Rural"))
ggplot(data=ggdat,aes(x=Metro, y=Frequency, fill=Metro))+
  geom_bar(stat="identity")+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("")+
  ylab("Frequency")+
  ggtitle("Population Density Overall")

#cars
Cars<-prop.table(table(reg.dat$Cars))
Cars<-prop.table(table(reg.dat.1$Cars))
ggdat<-data.frame(Cars)
ggdat<-na.omit(ggdat)
names(ggdat)<-c("Cars", "Frequency")
ggdat$Cars<-factor(ggdat$Cars, levels = c(0,1,2,3,4,5), labels = c(0,1,2,3,4,"5+"))
ggplot(data=ggdat,aes(x=Cars, y= Frequency, fill=Cars))+
  geom_bar(stat="identity")+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Number of Cars")+
  ylab("Frequency")+
  ggtitle("Number of Cars per Household Overall")

#trucks
Trucks<-prop.table(table(reg.dat$Trucks))
Trucks<-prop.table(table(reg.dat.1$Trucks))
ggdat<-data.frame(Trucks)
ggdat<-na.omit(ggdat)
names(ggdat)<-c("Trucks", "Frequency")
ggdat$Trucks<-factor(ggdat$Trucks, levels = c(0,1,2,3,4,5), labels = c(0,1,2,3,4,"5+"))
ggplot(data=ggdat,aes(x=Trucks, y=Frequency, fill=Trucks))+
  geom_bar(stat="identity")+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Number of Trucks")+
  ylab("Frequency")+
  ggtitle("Number of Trucks per Household for Riders")

#PT
Distance<-prop.table(table(reg.dat$Public_Transportation))
Distance<-prop.table(table(reg.dat.1$Public_Transportation))
ggdat<-data.frame(Distance)
ggdat<-na.omit(ggdat)
names(ggdat)<-c("Distance", "Frequency")
ggdat$Distance<-factor(ggdat$Distance, levels = c(1,2,3,4), labels = c("< 1/4 Mile", "1/4 - 1/2 Mile", "1/2 - 1 Mile", "1+ Miles"))
ggdat<-na.omit(ggdat)
ggplot(data=ggdat,aes(x=Distance, y= Frequency, fill=Distance))+
  geom_bar(stat="identity")+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Distance from Public Transportation")+
  ylab("Frequency")+
  ggtitle("General Distance from Public Transportation for Riders")

#rail distance 
ggdat<-data.frame(Distance=reg.dat$Rail)
ggdat<-na.omit(ggdat)
ggdat$Distance<-factor(ggdat$Distance, levels = c(1,2,3,4), labels = c("< 1/4 Mile", "1/4 - 1/2 Mile", "1/2 - 1 Mile", "1+ Miles"))
ggplot(data=ggdat,aes(x=Distance, fill=Distance))+
  geom_bar()+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Distance from Rail Stop")+
  ylab("Count")+
  ggtitle("Distance from Rail Stop")

#shuttle distance 
ggdat<-data.frame(Distance=reg.dat$Shuttle)
ggdat<-na.omit(ggdat)
ggdat$Distance<-factor(ggdat$Distance, levels = c(1,2,3,4), labels = c("< 1/4 Mile", "1/4 - 1/2 Mile", "1/2 - 1 Mile", "1+ Miles"))
ggplot(data=ggdat,aes(x=Distance, fill=Distance))+
  geom_bar()+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Distance from Shuttle Stop")+
  ylab("Count")+
  ggtitle("Distance from Shuttle Stop")

#subway distance 
ggdat<-data.frame(Distance=reg.dat$Subway)
ggdat<-na.omit(ggdat)
ggdat$Distance<-factor(ggdat$Distance, levels = c(1,2,3,4), labels = c("< 1/4 Mile", "1/4 - 1/2 Mile", "1/2 - 1 Mile", "1+ Miles"))
ggplot(data=ggdat,aes(x=Distance, fill=Distance))+
  geom_bar()+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Distance from Subway Stop")+
  ylab("Count")+
  ggtitle("Distance from Subway Stop")

ggdat<-data.frame(Distance=reg.dat$Bus)
ggdat<-na.omit(ggdat)
ggdat$Distance<-factor(ggdat$Distance, levels = c(1,2,3,4), labels = c("< 1/4 Mile", "1/4 - 1/2 Mile", "1/2 - 1 Mile", "1+ Miles"))
ggplot(data=ggdat,aes(x=Distance, fill=Distance))+
  geom_bar()+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Distance from Bus Stop")+
  ylab("Count")+
  ggtitle("Distance from Bus Stop")

#wnamen
Amenities<-prop.table(table(reg.dat.1$Move_Amenities))
ggdat<-data.frame(Amenities)
ggdat<-na.omit(ggdat)
names(ggdat)<-c("Amenities", "Frequency")
ggdat$Amenities<-factor(ggdat$Amenities, levels = c(1,0), labels = c("Yes", "No"))
ggplot(data=ggdat,aes(x=Amenities, y=Frequency, fill=Amenities))+
  geom_bar(stat="identity")+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Moved to be Closer to Amenities")+
  ylab("Frequency")+
  ggtitle("Moved to be Closer to Amenities for Riders")

#wnjob
Work<-prop.table(table(reg.dat.1$Move_Job))
ggdat<-data.frame(Work)
ggdat<-na.omit(ggdat)
names(ggdat)<-c("Work", "Frequency")
ggdat$Work<-factor(ggdat$Work, levels = c(1,0), labels = c("Yes", "No"))
ggplot(data=ggdat,aes(x=Work, y=Frequency, fill=Work))+
  geom_bar(stat="identity")+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Moved to be Closer to Work")+
  ylab("Frequency")+
  ggtitle("Moved to be Closer to Work for Riders")

#wntrain
Transport<-prop.table(table(reg.dat.1$Move_Public_Transport))
ggdat<-data.frame(Transport)
ggdat<-na.omit(ggdat)
names(ggdat)<-c("Transport", "Frequency")
ggdat$Transport<-factor(ggdat$Transport, levels = c(1,0), labels = c("Yes", "No"))
ggplot(data=ggdat,aes(x=Transport, y=Frequency, fill=Transport))+
  geom_bar(stat="identity")+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Moved to be Closer to Transportation")+
  ylab("Frequency")+
  ggtitle("Moved to be Closer to Transportation for Riders")
