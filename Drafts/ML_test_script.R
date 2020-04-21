#Alexa Canaan
#Presentation Script
#ECON475

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

set.seed(13)
tree.ptpubtrn.train=tree(PTPUBTRN~.-CONTROL,data = train.data)
tree.ptpubtrn.pred=predict(tree.ptpubtrn.train,test.data,type="class")
table(tree.ptpubtrn.pred, y.test)
(1309+4551)/(1309+4551+55)
#correct predictions for around 99.1% of locations in test data

set.seed(24)
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
table(tree.ptpubtrn.test, y.test)

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
sum_stats_gen_pt<-data.frame(Metro=dat.t1$METRO3, Cars=dat.t1$CARS, Trucks=dat.t1$TRUCKS, Bus=dat.t1$PTDISBUS, Public_Transportation=dat.t1$PTDISPUB, Rail=dat.t1$PTDISRAIL, Shuttle=dat.t1$PTDISSHUT, Subway=dat.t1$PTDISSUB, Move_Public_Transport=dat.t1$WNTRAN, Move_Amenities=dat.t1$WNAMEN, Move_Job=dat.t1$WNJOB)
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

set.seed(13)
tree.ptpubtrn.train=tree(PTPUBTRN~.-CONTROL,data = train.data)
tree.ptpubtrn.pred=predict(tree.ptpubtrn.train,test.data,type="class")
table(tree.ptpubtrn.pred, y.test)
(1310+4551)/(1310+4551+54)
#correct predictions for around 99.1% of locations in test data

set.seed(24)
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
table(tree.ptpubtrn.test, y.test)

# N=21517 #############################################################################
dat.t2<-subset(dat.t1, dat.t1$PTDISPUB==1 | dat.t1$PTDISPUB==2 |dat.t1$PTDISPUB==3 |dat.t1$PTDISPUB==4)

#SUMMARY STATISTICS FOR POPULATION
library(skimr)
library(dplyr)
sum_stats_y<-filter(dat.t2, PTPUBTRN==1)
sum_stats_y_pop<-data.frame(Sex=sum_stats_y$HHSEX, Age=sum_stats_y$HHAGE, Marriage=sum_stats_y$HHMAR, Race=sum_stats_y$HHRACE, Grad=sum_stats_y$HHGRAD, Income=sum_stats_y$ZINC2)
sum_stats_gen_pop<-data.frame(Sex=dat.t2$HHSEX, Age=dat.t2$HHAGE, Marriage=dat.t2$HHMAR, Race=dat.t2$HHRACE, Grad=dat.t2$HHGRAD, Income=dat.t2$ZINC2)
skim(sum_stats_y_pop)
skim(sum_stats_gen_pop)
#SUMMARY STATISTICS FOR PUBLIC TRANSPORTATION
sum_stats_y_pt<-data.frame(Metro=sum_stats_y$METRO3, Cars=sum_stats_y$CARS, Trucks=sum_stats_y$TRUCKS, Bus=sum_stats_y$PTDISBUS, Public_Transportation=sum_stats_y$PTDISPUB, Rail=sum_stats_y$PTDISRAIL, Shuttle=sum_stats_y$PTDISSHUT, Subway=sum_stats_y$PTDISSUB, Move_Public_Transport=sum_stats_y$WNTRAN, Move_Amenities=sum_stats_y$WNAMEN, Move_Job=sum_stats_y$WNJOB)
sum_stats_gen_pt<-data.frame(Metro=dat.t2$METRO3, Cars=dat.t2$CARS, Trucks=dat.t2$TRUCKS, Bus=dat.t2$PTDISBUS, Public_Transportation=dat.t2$PTDISPUB, Rail=dat.t2$PTDISRAIL, Shuttle=dat.t2$PTDISSHUT, Subway=dat.t2$PTDISSUB, Move_Public_Transport=dat.t2$WNTRAN, Move_Amenities=dat.t2$WNAMEN, Move_Job=dat.t2$WNJOB)
skim(sum_stats_y_pt)
skim(sum_stats_gen_pt)

#REGRESSION ANALYSIS

#BASE model
#ols
mod1<-lm(formula= as.numeric(PTPUBTRN) ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB, data = dat.t2)
summary(mod1)

#BASE + neighborhood amenities
#ols
mod3<-lm(formula= as.numeric(PTPUBTRN) ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB  + WNAMEN + WNJOB + WNTRAN, data = dat.t2)
summary(mod3)

#BASE model
#logit 
dat.t2$PTPUBTRN<-factor(dat.t2$PTPUBTRN, labels = c(1, 0))
mod2<-glm(formula= PTPUBTRN ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB, data = dat.t2, family = "binomial"(link = "logit"))
summary(mod2)

#BASE + neighborhood amenities
#logit
mod4<-glm(formula= PTPUBTRN ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB + WNAMEN + WNJOB + WNTRAN, data = dat.t2, family = "binomial"(link = "logit"))
summary(mod4)

#produce regression output
library(stargazer)
stargazer(mod1, mod3, mod2, mod4, type = "text",
          dep.var.labels = c("Public Transport Use", "Public Transport Use"),
          out = "regmodels2.txt")
stargazer(mod1, mod3, mod2, mod4, type = "html",
          dep.var.labels = c("Public Transport Use", "Public Transport Use"),
          out = "regmodels2.htm")

#CLASSIFICATION TREE
#split the data into training and test set
reg.dat<-data.frame(sum_stats_gen_pt,CONTROL=dat.t2$CONTROL, PTPUBTRN=dat.t2$PTPUBTRN)
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

set.seed(13)
tree.ptpubtrn.train=tree(PTPUBTRN~.-CONTROL,data = train.data)
tree.ptpubtrn.pred=predict(tree.ptpubtrn.train,test.data,type="class")
table(tree.ptpubtrn.pred, y.test)
(4259)/(4259+44)
#correct predictions for around 98.98% of locations in test data

set.seed(24)
cv.ptpubtrn=cv.tree(tree.ptpubtrn.train,FUN=prune.misclass)
names(cv.ptpubtrn)
cv.ptpubtrn
#dev is the cross-validation error rate 
#plot error rate as a fn of size and k folds
par(mfrow=c(1,2))
plot(cv.ptpubtrn$size, cv.ptpubtrn$dev, type="b")
plot(cv.ptpubtrn$k, cv.ptpubtrn$dev, type="b")

#apply prune.misclass to prune tree to lowest error rate
prune.ptpubtrn = prune.misclass(tree.ptpubtrn.train, best=4)
plot(prune.ptpubtrn)
text(prune.ptpubtrn, pretty=0)

#test pruned tree performance
tree.ptpubtrn.test=predict(prune.ptpubtrn, test.data, type = "class")
table(tree.ptpubtrn.test, y.test)

# N = 4469 ###########################################################################

dat.t3<-subset(dat.t2, dat.t2$WNTRAN==1 | dat.t2$WNTRAN==0)

#SUMMARY STATISTICS FOR POPULATION
library(skimr)
library(dplyr)
sum_stats_y<-filter(dat.t3, PTPUBTRN==1)
sum_stats_y_pop<-data.frame(Sex=sum_stats_y$HHSEX, Age=sum_stats_y$HHAGE, Marriage=sum_stats_y$HHMAR, Race=sum_stats_y$HHRACE, Grad=sum_stats_y$HHGRAD, Income=sum_stats_y$ZINC2)
sum_stats_gen_pop<-data.frame(Sex=dat.t3$HHSEX, Age=dat.t3$HHAGE, Marriage=dat.t3$HHMAR, Race=dat.t3$HHRACE, Grad=dat.t3$HHGRAD, Income=dat.t3$ZINC2)
skim(sum_stats_y_pop)
skim(sum_stats_gen_pop)
#SUMMARY STATISTICS FOR PUBLIC TRANSPORTATION
sum_stats_y_pt<-data.frame(Metro=sum_stats_y$METRO3, Cars=sum_stats_y$CARS, Trucks=sum_stats_y$TRUCKS, Bus=sum_stats_y$PTDISBUS, Public_Transportation=sum_stats_y$PTDISPUB, Rail=sum_stats_y$PTDISRAIL, Shuttle=sum_stats_y$PTDISSHUT, Subway=sum_stats_y$PTDISSUB, Move_Public_Transport=sum_stats_y$WNTRAN, Move_Amenities=sum_stats_y$WNAMEN, Move_Job=sum_stats_y$WNJOB)
sum_stats_gen_pt<-data.frame(Metro=dat.t3$METRO3, Cars=dat.t3$CARS, Trucks=dat.t3$TRUCKS, Bus=dat.t3$PTDISBUS, Public_Transportation=dat.t3$PTDISPUB, Rail=dat.t3$PTDISRAIL, Shuttle=dat.t3$PTDISSHUT, Subway=dat.t3$PTDISSUB, Move_Public_Transport=dat.t3$WNTRAN, Move_Amenities=dat.t3$WNAMEN, Move_Job=dat.t3$WNJOB)
skim(sum_stats_y_pt)
skim(sum_stats_gen_pt)

#REGRESSION ANALYSIS

#BASE model
#ols
mod1<-lm(formula= as.numeric(PTPUBTRN) ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB, data = dat.t3)
summary(mod1)

#BASE + neighborhood amenities
#ols
mod3<-lm(formula= as.numeric(PTPUBTRN) ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB  + WNAMEN + WNJOB + WNTRAN, data = dat.t3)
summary(mod3)

#BASE model
#logit 
dat.t3$PTPUBTRN<-factor(dat.t3$PTPUBTRN, labels = c(1, 0))
mod2<-glm(formula= PTPUBTRN ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB, data = dat.t3, family = "binomial"(link = "logit"))
summary(mod2)

#BASE + neighborhood amenities
#logit
mod4<-glm(formula= PTPUBTRN ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB + WNAMEN + WNJOB + WNTRAN, data = dat.t3, family = "binomial"(link = "logit"))
summary(mod4)

#produce regression output
library(stargazer)
stargazer(mod1, mod3, mod2, mod4, type = "text",
          dep.var.labels = c("Public Transport Use", "Public Transport Use"),
          out = "regmodelst3.txt")
stargazer(mod1, mod3, mod2, mod4, type = "html",
          dep.var.labels = c("Public Transport Use", "Public Transport Use"),
          out = "regmodelst3.htm")

#CLASSIFICATION TREE
#split the data into training and test set
reg.dat<-data.frame(sum_stats_gen_pt,CONTROL=dat.t3$CONTROL, PTPUBTRN=dat.t3$PTPUBTRN)
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

set.seed(13)
tree.ptpubtrn.train=tree(PTPUBTRN~.-CONTROL,data = train.data)
tree.ptpubtrn.pred=predict(tree.ptpubtrn.train,test.data,type="class")
table(tree.ptpubtrn.pred, y.test)
(0+884)/(9+884)
#correct predictions for around 98.99% of locations in test data

set.seed(24)
cv.ptpubtrn=cv.tree(tree.ptpubtrn.train,FUN=prune.misclass)
names(cv.ptpubtrn)
cv.ptpubtrn
#dev is the cross-validation error rate 
#plot error rate as a fn of size and k folds
par(mfrow=c(1,2))
plot(cv.ptpubtrn$size, cv.ptpubtrn$dev, type="b")
plot(cv.ptpubtrn$k, cv.ptpubtrn$dev, type="b")

#apply prune.misclass to prune tree to lowest error rate
prune.ptpubtrn = prune.misclass(tree.ptpubtrn.train, best=8)
plot(prune.ptpubtrn)
text(prune.ptpubtrn, pretty=0)

#test pruned tree performance
tree.ptpubtrn.test=predict(prune.ptpubtrn, test.data, type = "class")
table(tree.ptpubtrn.test, y.test)

#LASSO - load the library
library(glmnet)
library(tidyverse)
library(caret)
library(Matrix)
library(foreach)

#split the data into training and test set
set.seed(13)
training.sample <- dat.t3$PTPUBTRN %>%
  createDataPartition(p = 0.8, list=FALSE)
train.data <- dat.t3[training.sample, ]
test.data <-dat.t3[-training.sample, ]
x <- model.matrix(as.numeric(PTPUBTRN)~.- CONTROL, dat.t3)[,-1]
y <- as.numeric(dat.t3$PTPUBTRN)

#dummy code categorical predictor variables
x.train <- model.matrix(as.numeric(PTPUBTRN)~.- CONTROL, train.data)[,-1]
x.test <- model.matrix(as.numeric(PTPUBTRN)~.- CONTROL, test.data)[,-1]
y.train <- as.numeric(train.data$PTPUBTRN)
y.test <- as.numeric(test.data$PTPUBTRN)
# Find the best lambda using cross-validation
cv.lasso <- cv.glmnet(x.train, y.train, alpha = 1, family = "binomial")
plot(cv.lasso)
lasso_mod = glmnet(x.train, y.train, alpha = 1)
bestlam = cv.lasso$lambda.min
lasso_pred = predict(lasso_mod, s = bestlam, newx = x.test)
mean((lasso_pred - y.test)^2)
out = glmnet(x, y, alpha = 1, lambda = bestlam)
lasso_coef = predict(out, type = "coefficients", s = bestlam)# Display coefficients using lambda chosen by CV
lasso_coef
lasso_coef[lasso_coef != 0]

#fit the model on the training data
model<- cv.glmnet(x, y, family = "binomial", alpha = 1) 
#lambda = cv.lasso$lambda.min, maxit = 1000000)
#display regression coefficients
coef(model)
model.coefs<-coef(model)

# Make predictions on the test data
x.test <- model.matrix(PTPUBTRN ~. -CONTROL, test.data)[,-1]
probabilities <- model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
# Model accuracy
observed.classes <- test.data$PTPUBTRN
mean(predicted.classes == observed.classes)

# grab reduced variables
vars_kept <- names(model.coefs[,1][which(model.coefs[,1] > 0)])
vars_kept

# fit model
mod5 <- glm(as.numeric(PTPUBTRN) ~ ., data = train.data, family="binomial")
summary(mod5)