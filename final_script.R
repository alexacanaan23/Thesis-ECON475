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

#ptbank
class(dat.n$PTBANK)
typeof(dat.n$PTBANK)
attributes(dat.n$PTBANK)
head(dat.n$PTBANK)
summary(dat.n$PTBANK)
dat.n$PTBANK <- factor(dat.n$PTBANK, labels = c(NA, NA, NA, "1", "0"))

#ptentmnt
class(dat.n$PTENTMNT)
typeof(dat.n$PTENTMNT)
attributes(dat.n$PTENTMNT)
head(dat.n$PTENTMNT)
summary(dat.n$PTENTMNT)
dat.n$PTENTMNT <- factor(dat.n$PTENTMNT, labels = c(NA, NA, NA, "1", "0"))

#ptgrocer
class(dat.n$PTGROCER)
typeof(dat.n$PTGROCER)
attributes(dat.n$PTGROCER)
head(dat.n$PTGROCER)
summary(dat.n$PTGROCER)
dat.n$PTGROCER <- factor(dat.n$PTGROCER, labels = c(NA, NA, NA, "1", "0"))

#pthealth
class(dat.n$PTHEALTH)
typeof(dat.n$PTHEALTH)
attributes(dat.n$PTHEALTH)
head(dat.n$PTHEALTH)
summary(dat.n$PTHEALTH)
dat.n$PTHEALTH <- factor(dat.n$PTHEALTH, labels = c(NA, NA, NA, "1", "0"))

#ptretail
class(dat.n$PTRETAIL)
typeof(dat.n$PTRETAIL)
attributes(dat.n$PTRETAIL)
head(dat.n$PTRETAIL)
summary(dat.n$PTRETAIL)
dat.n$PTRETAIL <- factor(dat.n$PTRETAIL, labels = c(NA, NA, NA, "1", "0"))

#ptservic
class(dat.n$PTSERVIC)
typeof(dat.n$PTSERVIC)
attributes(dat.n$PTSERVIC)
head(dat.n$PTSERVIC)
summary(dat.n$PTSERVIC)
dat.n$PTSERVIC <- factor(dat.n$PTSERVIC, labels = c(NA, NA, NA, "1", "0"))

#ptgetbus
class(dat.n$PTGETBUS)
typeof(dat.n$PTGETBUS)
attributes(dat.n$PTGETBUS)
head(dat.n$PTGETBUS)
summary(dat.n$PTGETBUS)
dat.n$PTGETBUS <- factor(dat.n$PTGETBUS, labels = c(NA, NA, NA, "1", "2", "3", "4", "5", "6", "7", "8", "9"))

#ptgetrail
class(dat.n$PTGETRAIL)
typeof(dat.n$PTGETRAIL)
attributes(dat.n$PTGETRAIL)
head(dat.n$PTGETRAIL)
summary(dat.n$PTGETRAIL)
dat.n$PTGETRAIL <- factor(dat.n$PTGETRAIL, labels = c(NA, NA, "1", "2", "3", "4", "5", "6", "7", "8", "9"))

#ptgetshut
class(dat.n$PTGETSHUT)
typeof(dat.n$PTGETSHUT)
attributes(dat.n$PTGETSHUT)
head(dat.n$PTGETSHUT)
summary(dat.n$PTGETSHUT)
dat.n$PTGETSHUT <- factor(dat.n$PTGETSHUT, labels = c(NA, NA, "1", "2", "3", "4", "5", "6", "7", "8", "9"))

#ptgetsub
class(dat.n$PTGETSUB)
typeof(dat.n$PTGETSUB)
attributes(dat.n$PTGETSUB)
head(dat.n$PTGETSUB)
summary(dat.n$PTGETSUB)
dat.n$PTGETSUB <- factor(dat.n$PTGETSUB, labels = c(NA, NA, NA, "1", "2", "3", "4", "5", "6", "7", "8", "9"))

#ptoftbus
class(dat.n$PTOFTBUS)
typeof(dat.n$PTOFTBUS)
attributes(dat.n$PTOFTBUS)
head(dat.n$PTOFTBUS)
summary(dat.n$PTOFTBUS)
dat.n$PTOFTBUS <- factor(dat.n$PTOFTBUS, labels = c(NA, NA, NA, "1", "2", "3", "4", "5", "6"))

#ptoftrail
class(dat.n$PTOFTRAIL)
typeof(dat.n$PTOFTRAIL)
attributes(dat.n$PTOFTRAIL)
head(dat.n$PTOFTRAIL)
summary(dat.n$PTOFTRAIL)
dat.n$PTOFTRAIL <- factor(dat.n$PTOFTRAIL, labels = c(NA, NA, NA, "1", "2", "3", "4", "5", "6"))

#ptoftshut
class(dat.n$PTOFTSHUT)
typeof(dat.n$PTOFTSHUT)
attributes(dat.n$PTOFTSHUT)
head(dat.n$PTOFTSHUT)
summary(dat.n$PTOFTSHUT)
dat.n$PTOFTSHUT <- factor(dat.n$PTOFTSHUT, labels = c(NA, NA, NA, "1", "2", "3", "4", "5", "6"))

#ptoftsub
class(dat.n$PTOFTSUB)
typeof(dat.n$PTOFTSUB)
attributes(dat.n$PTOFTSUB)
head(dat.n$PTOFTSUB)
summary(dat.n$PTOFTSUB)
dat.n$PTOFTSUB <- factor(dat.n$PTOFTSUB, labels = c(NA, NA, NA, "1", "2", "3", "4", "5", "6"))

#ptwkschl
class(dat.n$PTWKSCHL)
typeof(dat.n$PTWKSCHL)
attributes(dat.n$PTWKSCHL)
head(dat.n$PTWKSCHL)
summary(dat.n$PTWKSCHL)
dat.n$PTWKSCHL <- factor(dat.n$PTWKSCHL, labels = c(NA, NA, NA, "1", "2", "3", "4", "5"))

#remove NA's
dat.n2<-na.omit(dat.n)

#remove variables that have less than 2 factors
for (i in names(dat.n2)) {
  if (nlevels(dat.n2[[i]]) < 2 & is.factor(dat.n2[[i]])==TRUE) {
    dat.n2[[i]]<-NULL
  }
}

dat.t<-dat.n2
dat.t1<-subset(dat.t, dat.t$PTPUBTRN==1 | dat.t$PTPUBTRN==0)

#SUMMARY STATISTICS FOR PUBLIC TRANSPORTATION
library(skimr)
library(dplyr)
basic_model<-data.frame(Metro=dat.t1$METRO3, Cars=dat.t1$CARS, Trucks=dat.t1$TRUCKS, Public_Transport=dat.t1$PTDISPUB, Bus=dat.t1$PTDISBUS, Rail=dat.t1$PTDISRAIL, Shuttle=dat.t1$PTDISSHUT, Subway=dat.t1$PTDISSUB, Move_Public_Transport=dat.t1$WNTRAN, Move_Amenities=dat.t1$WNAMEN, Move_Job=dat.t1$WNJOB)
no_get_vars<-data.frame(Metro=dat.t1$METRO3, Cars=dat.t1$CARS, Trucks=dat.t1$TRUCKS, Public_Transport=dat.t1$PTDISPUB, Bus=dat.t1$PTDISBUS, Rail=dat.t1$PTDISRAIL, Shuttle=dat.t1$PTDISSHUT, Subway=dat.t1$PTDISSUB, Move_Public_Transport=dat.t1$WNTRAN, Move_Amenities=dat.t1$WNAMEN, Move_Job=dat.t1$WNJOB, 
                        Bank=dat.t1$PTBANK, Entertainment=dat.t1$PTENTMNT, Grocery=dat.t1$PTGROCER, Health=dat.t1$PTHEALTH, Retail=dat.t1$PTRETAIL, Service=dat.t1$PTSERVIC)
all_vars<-data.frame(Metro=dat.t1$METRO3, Cars=dat.t1$CARS, Trucks=dat.t1$TRUCKS, Public_Transport=dat.t1$PTDISPUB, Bus=dat.t1$PTDISBUS, Rail=dat.t1$PTDISRAIL, Shuttle=dat.t1$PTDISSHUT, Subway=dat.t1$PTDISSUB, Move_Public_Transport=dat.t1$WNTRAN, Move_Amenities=dat.t1$WNAMEN, Move_Job=dat.t1$WNJOB, 
                             Bank=dat.t1$PTBANK, Entertainment=dat.t1$PTENTMNT, Grocery=dat.t1$PTGROCER, Health=dat.t1$PTHEALTH, Retail=dat.t1$PTRETAIL, Service=dat.t1$PTSERVIC,
                             GetBus = dat.t1$PTGETBUS, GetRail= dat.t1$PTGETRAIL, GetShuttle = dat.t1$PTGETSHUT, GetSubway = dat.t1$PTGETSUB,
                     Freq_Bus = dat.t1$PTOFTBUS, Freq_Rail = dat.t1$PTOFTRAIL, Freq_Shut = dat.t1$PTOFTSHUT, Freq_Sub = dat.t1$PTOFTSUB, Freq_Work_School = dat.t1$PTWKSCHL)
skim(basic_model)

#OLS + Classification trees by subset of data
library(tree)
library(caret)
library(dplyr)
###########BASE model ############
#ols
mod1<-lm(formula= as.numeric(PTPUBTRN) ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISPUB + PTDISBUS + PTDISRAIL + PTDISSHUT + PTDISSUB, data = dat.t1)
summary(mod1)

#classification tree
base_mod<-data.frame(PTPUBTRN = dat.t1$PTPUBTRN, Metro = dat.t1$METRO3, Cars = dat.t1$CARS, Trucks = dat.t1$TRUCKS, Income = dat.t1$ZINC2, Bus = dat.t1$PTDISBUS, Rail = dat.t1$PTDISRAIL, Shuttle = dat.t1$PTDISSHUT, Subway= dat.t1$PTDISSUB)
#split the data into training and test set
training.sample <- base_mod$PTPUBTRN %>%
  createDataPartition(p = 0.8, list=FALSE)
train.data <- base_mod[training.sample, ]
test.data <-base_mod[-training.sample, ]
test.data <-data.frame(test.data)
y.test <-test.data$PTPUBTRN

#fit a classification tree
tree.ptpubtrn=tree(PTPUBTRN~., data = base_mod)
summary(tree.ptpubtrn)
par(mfrow=c(1,1))
plot(tree.ptpubtrn)
text(tree.ptpubtrn, pretty=0)
tree.ptpubtrn

tree.ptpubtrn.train=tree(PTPUBTRN~.,data = train.data)
tree.ptpubtrn.pred=predict(tree.ptpubtrn.train,test.data,type="class")
accuracy_Table_1base<-table(tree.ptpubtrn.pred, y.test)
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
prune.ptpubtrn = prune.misclass(tree.ptpubtrn.train, best=5)
par(mfrow=c(1,1))
plot(prune.ptpubtrn)
text(prune.ptpubtrn, pretty=0)
title(main = "Pruned Classification Tree")

#test pruned tree performance
tree.ptpubtrn.test=predict(prune.ptpubtrn, test.data, type = "class")
accuracy_Table_2base<-table(tree.ptpubtrn.test, y.test)

for (i in 1:1000){
  training.sample <- base_mod$PTPUBTRN %>%
    createDataPartition(p = 0.8, list=FALSE)
  train.data <- base_mod[training.sample, ]
  test.data <-base_mod[-training.sample, ]
  test.data <-data.frame(test.data)
  y.test <-test.data$PTPUBTRN
  
  tree.ptpubtrn.train=tree(PTPUBTRN~.,data = train.data)
  tree.ptpubtrn.pred=predict(tree.ptpubtrn.train,test.data,type="class")
  accuracy_Table_1base = accuracy_Table_1base + table(tree.ptpubtrn.pred, y.test)
  
  cv.ptpubtrn=cv.tree(tree.ptpubtrn.train,FUN=prune.misclass)
  prune.ptpubtrn = prune.misclass(tree.ptpubtrn.train, best=max(cv.ptpubtrn$size))
  
  tree.ptpubtrn.test=predict(prune.ptpubtrn, test.data, type = "class")
  accuracy_Table_2base = accuracy_Table_2base + table(tree.ptpubtrn.test, y.test)
}

##################BASE + neighborhood + attitudes###########################
mod2<-lm(formula= as.numeric(PTPUBTRN) ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB  + WNAMEN + WNJOB + WNTRAN, data = dat.t1)
summary(mod2)
neighborhood_attitude_mod<-data.frame(PTPUBTRN = dat.t1$PTPUBTRN, Metro = dat.t1$METRO3, Cars = dat.t1$CARS, Trucks = dat.t1$TRUCKS, Income = dat.t1$ZINC2,Bus= dat.t1$PTDISBUS, Rail = dat.t1$PTDISRAIL, Shuttle = dat.t1$PTDISSHUT, Subway = dat.t1$PTDISSUB, Move_Amenities = dat.t1$WNAMEN, Move_Job = dat.t1$WNJOB, Move_Public_Transportation = dat.t1$WNTRAN)

#split the data into training and test set
training.sample <- neighborhood_attitude_mod$PTPUBTRN %>%
  createDataPartition(p = 0.8, list=FALSE)
train.data <- neighborhood_attitude_mod[training.sample, ]
test.data <-neighborhood_attitude_mod[-training.sample, ]
test.data <-data.frame(test.data)
y.test <-test.data$PTPUBTRN

#fit a classification tree
tree.ptpubtrn=tree(PTPUBTRN~., data = neighborhood_attitude_mod)
summary(tree.ptpubtrn)
par(mfrow=c(1,1))
plot(tree.ptpubtrn)
text(tree.ptpubtrn, pretty=0)
tree.ptpubtrn

tree.ptpubtrn.train=tree(PTPUBTRN~.,data = train.data)
tree.ptpubtrn.pred=predict(tree.ptpubtrn.train,test.data,type="class")
accuracy_Table_1neigh<-table(tree.ptpubtrn.pred, y.test)

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
prune.ptpubtrn = prune.misclass(tree.ptpubtrn.train, best=5)
par(mfrow=c(1,1))
plot(prune.ptpubtrn)
text(prune.ptpubtrn, pretty=0)
title(main = "Pruned Classification Tree")

#test pruned tree performance
tree.ptpubtrn.test=predict(prune.ptpubtrn, test.data, type = "class")
accuracy_Table_2neigh<-table(tree.ptpubtrn.test, y.test)

for (i in 1:1000){
  training.sample <- neighborhood_attitude_mod$PTPUBTRN %>%
    createDataPartition(p = 0.8, list=FALSE)
  train.data <- neighborhood_attitude_mod[training.sample, ]
  test.data <-neighborhood_attitude_mod[-training.sample, ]
  test.data <-data.frame(test.data)
  y.test <-test.data$PTPUBTRN
  
  tree.ptpubtrn.train=tree(PTPUBTRN~.,data = train.data)
  tree.ptpubtrn.pred=predict(tree.ptpubtrn.train,test.data,type="class")
  accuracy_Table_1neigh = accuracy_Table_1neigh + table(tree.ptpubtrn.pred, y.test)
  
  cv.ptpubtrn=cv.tree(tree.ptpubtrn.train,FUN=prune.misclass)
  prune.ptpubtrn = prune.misclass(tree.ptpubtrn.train, best=max(cv.ptpubtrn$size))
  
  tree.ptpubtrn.test=predict(prune.ptpubtrn, test.data, type = "class")
  accuracy_Table_2neigh = accuracy_Table_2neigh + table(tree.ptpubtrn.test, y.test)
}

###########BASE + neighborhood + attitudes + access proxies#############
mod3<-lm(formula= as.numeric(PTPUBTRN) ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB  + WNAMEN + WNJOB + WNTRAN + PTBANK + PTENTMNT + PTGROCER + PTHEALTH + PTRETAIL + PTSERVIC , data = dat.t1)
summary(mod3)
access_mod<-data.frame(PTPUBTRN = dat.t1$PTPUBTRN, Metro = dat.t1$METRO3, Cars = dat.t1$CARS, Trucks = dat.t1$TRUCKS, Income = dat.t1$ZINC2, Bus = dat.t1$PTDISBUS, Rail = dat.t1$PTDISRAIL, Shuttle = dat.t1$PTDISSHUT, Subway = dat.t1$PTDISSUB, Move_Amenities = dat.t1$WNAMEN, Move_Job = dat.t1$WNJOB, Move_Public_Transportation = dat.t1$WNTRAN, Bank = dat.t1$PTBANK, Entertainment = dat.t1$PTENTMNT, Grocery = dat.t1$PTGROCER, Health = dat.t1$PTHEALTH, Retail = dat.t1$PTRETAIL, Service = dat.t1$PTSERVIC)

#split the data into training and test set
training.sample <- access_mod$PTPUBTRN %>%
  createDataPartition(p = 0.8, list=FALSE)
train.data <- access_mod[training.sample, ]
test.data <-access_mod[-training.sample, ]
test.data <-data.frame(test.data)
y.test <-test.data$PTPUBTRN

#fit a classification tree
tree.ptpubtrn=tree(PTPUBTRN~., data = access_mod)
summary(tree.ptpubtrn)
par(mfrow=c(1,1))
plot(tree.ptpubtrn)
text(tree.ptpubtrn, pretty=0)
tree.ptpubtrn

tree.ptpubtrn.train=tree(PTPUBTRN~.,data = train.data)
tree.ptpubtrn.pred=predict(tree.ptpubtrn.train,test.data,type="class")
accuracy_Table_1access<-table(tree.ptpubtrn.pred, y.test)
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
prune.ptpubtrn = prune.misclass(tree.ptpubtrn.train, best=5)
par(mfrow=c(1,1))
plot(prune.ptpubtrn)
text(prune.ptpubtrn, pretty=0)
title(main = "Pruned Classification Tree")

#test pruned tree performance
tree.ptpubtrn.test=predict(prune.ptpubtrn, test.data, type = "class")
accuracy_Table_2access<-table(tree.ptpubtrn.test, y.test)

for (i in 1:1000){
  training.sample <- access_mod$PTPUBTRN %>%
    createDataPartition(p = 0.8, list=FALSE)
  train.data <- access_mod[training.sample, ]
  test.data <-access_mod[-training.sample, ]
  test.data <-data.frame(test.data)
  y.test <-test.data$PTPUBTRN
  
  tree.ptpubtrn.train=tree(PTPUBTRN~.,data = train.data)
  tree.ptpubtrn.pred=predict(tree.ptpubtrn.train,test.data,type="class")
  accuracy_Table_1access = accuracy_Table_1access + table(tree.ptpubtrn.pred, y.test)
  
  cv.ptpubtrn=cv.tree(tree.ptpubtrn.train,FUN=prune.misclass)
  prune.ptpubtrn = prune.misclass(tree.ptpubtrn.train, best=max(cv.ptpubtrn$size))
  
  tree.ptpubtrn.test=predict(prune.ptpubtrn, test.data, type = "class")
  accuracy_Table_2access = accuracy_Table_2access + table(tree.ptpubtrn.test, y.test)
}

########################BASE + neighborhood + attitudes + access proxies + get vars#############################
mod4<-lm(formula= as.numeric(PTPUBTRN) ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB  + WNAMEN + WNJOB + WNTRAN + PTBANK + PTENTMNT + PTGROCER + PTHEALTH + PTRETAIL + PTSERVIC + PTGETBUS + PTGETRAIL + PTGETSHUT + PTGETSUB , data = dat.t1)
summary(mod4)
get_mod<-data.frame(PTPUBTRN = dat.t1$PTPUBTRN, Metro = dat.t1$METRO3, Cars = dat.t1$CARS, Trucks = dat.t1$TRUCKS, Income = dat.t1$ZINC2, Bus = dat.t1$PTDISBUS, Rail = dat.t1$PTDISRAIL, Shuttle = dat.t1$PTDISSHUT, Subway = dat.t1$PTDISSUB, Move_Amenities=dat.t1$WNAMEN, Move_Job=dat.t1$WNJOB, Move_Public_Transportation=dat.t1$WNTRAN, Bank = dat.t1$PTBANK, Entertainment = dat.t1$PTENTMNT, Grocery = dat.t1$PTGROCER, Health = dat.t1$PTHEALTH, Retail = dat.t1$PTRETAIL, Service = dat.t1$PTSERVIC,GetBus= dat.t1$PTGETBUS, GetRail = dat.t1$PTGETRAIL, GetShuttle = dat.t1$PTGETSHUT, GetSubway = dat.t1$PTGETSUB)

#split the data into training and test set
training.sample <- get_mod$PTPUBTRN %>%
  createDataPartition(p = 0.8, list=FALSE)
train.data <- get_mod[training.sample, ]
test.data <-get_mod[-training.sample, ]
test.data <-data.frame(test.data)
y.test <-test.data$PTPUBTRN

#fit a classification tree
tree.ptpubtrn=tree(PTPUBTRN~., data = get_mod)
summary(tree.ptpubtrn)
par(mfrow=c(1,1))
plot(tree.ptpubtrn)
text(tree.ptpubtrn, pretty=0)
tree.ptpubtrn

tree.ptpubtrn.train=tree(PTPUBTRN~.,data = train.data)
tree.ptpubtrn.pred=predict(tree.ptpubtrn.train,test.data,type="class")
accuracy_Table_1get<-table(tree.ptpubtrn.pred, y.test)

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
prune.ptpubtrn = prune.misclass(tree.ptpubtrn.train, best=5)
par(mfrow=c(1,1))
plot(prune.ptpubtrn)
text(prune.ptpubtrn, pretty=0)
title(main = "Pruned Classification Tree")

#test pruned tree performance
tree.ptpubtrn.test=predict(prune.ptpubtrn, test.data, type = "class")
accuracy_Table_2get<-table(tree.ptpubtrn.test, y.test)

for (i in 1:1000){
  training.sample <- get_mod$PTPUBTRN %>%
    createDataPartition(p = 0.8, list=FALSE)
  train.data <- get_mod[training.sample, ]
  test.data <-get_mod[-training.sample, ]
  test.data <-data.frame(test.data)
  y.test <-test.data$PTPUBTRN
  
  tree.ptpubtrn.train=tree(PTPUBTRN~.,data = train.data)
  tree.ptpubtrn.pred=predict(tree.ptpubtrn.train,test.data,type="class")
  accuracy_Table_1get = accuracy_Table_1get + table(tree.ptpubtrn.pred, y.test)
  
  cv.ptpubtrn=cv.tree(tree.ptpubtrn.train,FUN=prune.misclass)
  prune.ptpubtrn = prune.misclass(tree.ptpubtrn.train, best=max(cv.ptpubtrn$size))
  
  tree.ptpubtrn.test=predict(prune.ptpubtrn, test.data, type = "class")
  accuracy_Table_2get = accuracy_Table_2get + table(tree.ptpubtrn.test, y.test)
}

##############################BASE + neighborhood + attitudes + access proxies + get vars + frequency##########################
mod5<-lm(formula= as.numeric(PTPUBTRN) ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB  + WNAMEN + WNJOB + WNTRAN + PTBANK + PTENTMNT + PTGROCER + PTHEALTH + PTRETAIL + PTSERVIC + PTGETBUS + PTGETRAIL + PTGETSHUT + PTGETSUB + PTOFTSUB + PTOFTSHUT + PTOFTRAIL + PTOFTBUS + PTWKSCHL, data = dat.t1)
summary(mod5)
all_mod<-data.frame(PTPUBTRN = dat.t1$PTPUBTRN, Metro = dat.t1$METRO3, Cars = dat.t1$CARS, Trucks = dat.t1$TRUCKS, Income = dat.t1$ZINC2, Bus = dat.t1$PTDISBUS, Rail = dat.t1$PTDISRAIL, Shuttle = dat.t1$PTDISSHUT, Subway = dat.t1$PTDISSUB, Move_Amenities = dat.t1$WNAMEN, Move_Job = dat.t1$WNJOB, Move_Public_Transport = dat.t1$WNTRAN, Bank = dat.t1$PTBANK, Entertainment = dat.t1$PTENTMNT, Grocery = dat.t1$PTGROCER, Health = dat.t1$PTHEALTH, Retail = dat.t1$PTRETAIL, Service = dat.t1$PTSERVIC, GetBus = dat.t1$PTGETBUS, GetRail = dat.t1$PTGETRAIL, GetShuttle = dat.t1$PTGETSHUT, GetSubway = dat.t1$PTGETSUB, Freq_Sub = dat.t1$PTOFTSUB, Freq_Shut = dat.t1$PTOFTSHUT, Freq_Rail = dat.t1$PTOFTRAIL, Freq_Bus=dat.t1$PTOFTBUS, Freq_Work_School=dat.t1$PTWKSCHL)

#split the data into training and test set
training.sample <- all_mod$PTPUBTRN %>%
  createDataPartition(p = 0.8, list=FALSE)
train.data <- all_mod[training.sample, ]
test.data <-all_mod[-training.sample, ]
test.data <-data.frame(test.data)
y.test <-test.data$PTPUBTRN

#fit a classification tree
tree.ptpubtrn=tree(PTPUBTRN~., data = all_mod)
summary(tree.ptpubtrn)
par(mfrow=c(1,1))
plot(tree.ptpubtrn)
text(tree.ptpubtrn, pretty=0)
tree.ptpubtrn

tree.ptpubtrn.train=tree(PTPUBTRN~.,data = train.data)
tree.ptpubtrn.pred=predict(tree.ptpubtrn.train,test.data,type="class")
accuracy_Table_1all<-table(tree.ptpubtrn.pred, y.test)

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
prune.ptpubtrn = prune.misclass(tree.ptpubtrn.train, best=3)
par(mfrow=c(1,1))
plot(prune.ptpubtrn)
text(prune.ptpubtrn, pretty=0)
title(main = "Pruned Classification Tree")

#test pruned tree performance
tree.ptpubtrn.test=predict(prune.ptpubtrn, test.data, type = "class")
accuracy_Table_2all<-table(tree.ptpubtrn.test, y.test)

for (i in 1:1000){
  training.sample <- all_mod$PTPUBTRN %>%
    createDataPartition(p = 0.8, list=FALSE)
  train.data <- all_mod[training.sample, ]
  test.data <-all_mod[-training.sample, ]
  test.data <-data.frame(test.data)
  y.test <-test.data$PTPUBTRN
  
  tree.ptpubtrn.train=tree(PTPUBTRN~.,data = train.data)
  tree.ptpubtrn.pred=predict(tree.ptpubtrn.train,test.data,type="class")
  accuracy_Table_1all = accuracy_Table_1all + table(tree.ptpubtrn.pred, y.test)
  
  cv.ptpubtrn=cv.tree(tree.ptpubtrn.train,FUN=prune.misclass)
  prune.ptpubtrn = prune.misclass(tree.ptpubtrn.train, best=max(cv.ptpubtrn$size))
  
  tree.ptpubtrn.test=predict(prune.ptpubtrn, test.data, type = "class")
  accuracy_Table_2all = accuracy_Table_2all + table(tree.ptpubtrn.test, y.test)
}


################BASE + neighborhood + attitudes + access proxies + get vars + frequency#####################
mod51<-lm(formula= as.numeric(PTPUBTRN) ~ METRO3 + CARS + TRUCKS + ZINC2 + WNAMEN + WNJOB + WNTRAN + PTBANK + PTENTMNT + PTGROCER + PTHEALTH + PTRETAIL + PTSERVIC + PTOFTSUB + PTOFTSHUT + PTOFTRAIL + PTOFTBUS + PTWKSCHL, data = dat.t1)
summary(mod51)

no_dist_mod<-data.frame(PTPUBTRN = dat.t1$PTPUBTRN, Metro = dat.t1$METRO3, Cars = dat.t1$CARS, Trucks = dat.t1$TRUCKS, Income = dat.t1$ZINC2, Move_Amenities = dat.t1$WNAMEN, Move_Job = dat.t1$WNJOB, Move_Public_Transport = dat.t1$WNTRAN, Bank = dat.t1$PTBANK, Entertainment = dat.t1$PTENTMNT, Grocery = dat.t1$PTGROCER, Health = dat.t1$PTHEALTH, Retail = dat.t1$PTRETAIL, Service = dat.t1$PTSERVIC)
#split the data into training and test set
training.sample <- no_dist_mod$PTPUBTRN %>%
  createDataPartition(p = 0.8, list=FALSE)
train.data <- no_dist_mod[training.sample, ]
test.data <-no_dist_mod[-training.sample, ]
test.data <-data.frame(test.data)
y.test <-test.data$PTPUBTRN

#fit a classification tree
tree.ptpubtrn=tree(PTPUBTRN~., data = no_dist_mod)
summary(tree.ptpubtrn)
par(mfrow=c(1,1))
plot(tree.ptpubtrn)
text(tree.ptpubtrn, pretty=0)
tree.ptpubtrn

tree.ptpubtrn.train=tree(PTPUBTRN~.,data = train.data)
tree.ptpubtrn.pred=predict(tree.ptpubtrn.train,test.data,type="class")
accuracy_Table_1nodist<-table(tree.ptpubtrn.pred, y.test)

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
accuracy_Table_2nodist<-table(tree.ptpubtrn.test, y.test)

for (i in 1:1000){
  training.sample <- no_dist_mod$PTPUBTRN %>%
    createDataPartition(p = 0.8, list=FALSE)
  train.data <- no_dist_mod[training.sample, ]
  test.data <-no_dist_mod[-training.sample, ]
  test.data <-data.frame(test.data)
  y.test <-test.data$PTPUBTRN
  
  tree.ptpubtrn.train=tree(PTPUBTRN~.,data = train.data)
  tree.ptpubtrn.pred=predict(tree.ptpubtrn.train,test.data,type="class")
  accuracy_Table_1nodist = accuracy_Table_1nodist + table(tree.ptpubtrn.pred, y.test)
  
  cv.ptpubtrn=cv.tree(tree.ptpubtrn.train,FUN=prune.misclass)
  prune.ptpubtrn = prune.misclass(tree.ptpubtrn.train, best=max(cv.ptpubtrn$size))
  
  tree.ptpubtrn.test=predict(prune.ptpubtrn, test.data, type = "class")
  accuracy_Table_2nodist = accuracy_Table_2nodist + table(tree.ptpubtrn.test, y.test)
}


###############Produce Output##################
#produce regression output
library(stargazer)
stargazer(mod1, mod2, mod3, mod4, mod5, mod6, type = "text",
          dep.var.labels = c("Public Transport Use", "Public Transport Use"),
          out = "regmodels1.txt")
stargazer(mod1, mod2, mod3, mod4, mod5, mod6, type = "html",
          dep.var.labels = c("Public Transport Use", "Public Transport Use"),
          out = "regmodels1.htm")
stargazer(mod1, mod2, mod3, mod31, type = "text",
          dep.var.labels = c("Public Transport Use"),
          out = "regmodelsFINAL.txt")

#CLASSIFICATION TREE
library(tree)
#split the data into training and test set
training.sample <- reg.dat$PTPUBTRN %>%
  createDataPartition(p = 0.8, list=FALSE)
train.data <- reg.dat[training.sample, ]
test.data <-reg.dat[-training.sample, ]
test.data <-data.frame(test.data)
y.test <-test.data$PTPUBTRN

#fit a classification tree
tree.ptpubtrn=tree(PTPUBTRN~.-Public_Transport , data = reg.dat)
summary(tree.ptpubtrn)
par(mfrow=c(1,1))
plot(tree.ptpubtrn)
text(tree.ptpubtrn, pretty=0)
tree.ptpubtrn

tree.ptpubtrn.train=tree(PTPUBTRN~.-Public_Transport ,data = train.data)
tree.ptpubtrn.pred=predict(tree.ptpubtrn.train,test.data,type="class")
accuracy_Table11<-table(tree.ptpubtrn.pred, y.test)
(1316+4551)/(1310+4551+54)
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
prune.ptpubtrn = prune.misclass(tree.ptpubtrn.train, best=5)
par(mfrow=c(1,1))
plot(prune.ptpubtrn)
text(prune.ptpubtrn, pretty=0)
title(main = "Pruned Classification Tree")

#test pruned tree performance
tree.ptpubtrn.test=predict(prune.ptpubtrn, test.data, type = "class")
accuracy_Table21<-table(tree.ptpubtrn.test, y.test)
(1313+4551)/(1310+4551+54)

reg.dat<-data.frame(sum_stats_gen_pt, PTPUBTRN=dat.t1$PTPUBTRN)

for (i in 1:1000){
  training.sample <- reg.dat$PTPUBTRN %>%
    createDataPartition(p = 0.8, list=FALSE)
  train.data <- reg.dat[training.sample, ]
  test.data <-reg.dat[-training.sample, ]
  test.data <-data.frame(test.data)
  y.test <-test.data$PTPUBTRN
  
  tree.ptpubtrn.train=tree(PTPUBTRN~.-Public_Transport ,data = train.data)
  tree.ptpubtrn.pred=predict(tree.ptpubtrn.train,test.data,type="class")
  accuracy_Table1 = accuracy_Table1 + table(tree.ptpubtrn.pred, y.test)
  
  cv.ptpubtrn=cv.tree(tree.ptpubtrn.train,FUN=prune.misclass)
  prune.ptpubtrn = prune.misclass(tree.ptpubtrn.train, best=max(cv.ptpubtrn$size))
  
  tree.ptpubtrn.test=predict(prune.ptpubtrn, test.data, type = "class")
  accuracy_Table2 = accuracy_Table2 + table(tree.ptpubtrn.test, y.test)
}

run1<-(1314962+4555551)/(1314962+4555551+50402)
run2<-(2628523+9106551)/(2628523+9106551+100841)
run3<-(3942115+13657551)/(3942115+13657551+	151249)
run4<-(5255607+18208551)/(201757+5255607+18208551)


reg.dat.1<-data.frame(sum_stats_gen_pt_1, PTPUBTRN=dat.t1$PTPUBTRN)

for (i in 1:1000){
  training.sample <- reg.dat.1$PTPUBTRN %>%
    createDataPartition(p = 0.8, list=FALSE)
  train.data <- reg.dat.1[training.sample, ]
  test.data <-reg.dat.1[-training.sample, ]
  test.data <-data.frame(test.data)
  y.test <-test.data$PTPUBTRN
  
  tree.ptpubtrn.train=tree(PTPUBTRN~.-Public_Transport ,data = train.data)
  tree.ptpubtrn.pred=predict(tree.ptpubtrn.train,test.data,type="class")
  accuracy_Table11 = accuracy_Table11 + table(tree.ptpubtrn.pred, y.test)
  
  cv.ptpubtrn=cv.tree(tree.ptpubtrn.train,FUN=prune.misclass)
  prune.ptpubtrn = prune.misclass(tree.ptpubtrn.train, best=max(cv.ptpubtrn$size))
  
  tree.ptpubtrn.test=predict(prune.ptpubtrn, test.data, type = "class")
  accuracy_Table21 = accuracy_Table21 + table(tree.ptpubtrn.test, y.test)
}