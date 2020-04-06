#Alexa Canaan
#Metrics/Thesis Code

#read AHS data
dat.n<-read.csv("~/Desktop/ahs2013n.csv")

#test cleaning the data
dat.nt<-dat.n[1:100, ]

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
dat.nt.omit<-na.omit(dat.nt)
dat.n.omit<-na.omit(dat.n)

#remove NAs
dat.n2<- na.omit(dat.n.omit)
dat.n2<-subset(dat.n2, dat.n2$PTPUBTRN==1 | dat.n2$PTPUBTRN==0)
dat.n2<-subset(dat.n2, dat.n2$PTDISPUB==1 | dat.n2$PTDISPUB==2 |dat.n2$PTDISPUB==3 |dat.n2$PTDISPUB==4)
dat.n2<-subset(dat.n2, dat.n2$WNTRAN==1 | dat.n2$WNTRAN==0)

#remove variables that have less than 2 factors
for (i in names(dat.n2)) {
  if (nlevels(dat.n2[[i]]) < 2 & is.factor(dat.n2[[i]])==TRUE) {
    dat.n2[[i]]<-NULL
  }
}
dat.n<-dat.n2

#SUMMARY STATISTICS FOR POPULATION
library(skimr)
library(dplyr)
sum_stats_y<-filter(dat.n, PTPUBTRN==1)
sum_stats_y_pop<-data.frame(Sex=sum_stats_y$HHSEX, Age=sum_stats_y$HHAGE, Marriage=sum_stats_y$HHMAR, Race=sum_stats_y$HHRACE, Grad=sum_stats_y$HHGRAD, Income=sum_stats_y$ZINC2)
sum_stats_gen_pop<-data.frame(Sex=dat.n$HHSEX, Age=dat.n$HHAGE, Marriage=dat.n$HHMAR, Race=dat.n$HHRACE, Grad=dat.n$HHGRAD, Income=dat.n$ZINC2)
skim(sum_stats_y_pop)
skim(sum_stats_gen_pop)
#SUMMARY STATISTICS FOR PUBLIC TRANSPORTATION
sum_stats_y_pt<-data.frame(Metro=sum_stats_y$METRO3, Cars=sum_stats_y$CARS, Trucks=sum_stats_y$TRUCKS, Bus=sum_stats_y$PTDISBUS, Public_Transportation=sum_stats_y$PTDISPUB, Rail=sum_stats_y$PTDISRAIL, Shuttle=sum_stats_y$PTDISSHUT, Subway=sum_stats_y$PTDISSUB, Move_Public_Transport=sum_stats_y$WNTRAN, Move_Amenities=sum_stats_y$WNAMEN, Move_Job=sum_stats_y$WNJOB)
sum_stats_gen_pt<-data.frame(Metro=dat.n$METRO3, Cars=dat.n$CARS, Trucks=dat.n$TRUCKS, Bus=dat.n$PTDISBUS, Public_Transportation=dat.n$PTDISPUB, Rail=dat.n$PTDISRAIL, Shuttle=dat.n$PTDISSHUT, Subway=dat.n$PTDISSUB, Move_Public_Transport=dat.n$WNTRAN, Move_Amenities=dat.n$WNAMEN, Move_Job=dat.n$WNJOB)
skim(sum_stats_y_pt)
skim(sum_stats_gen_pt)

#REGRESSION ANALYSIS

#BASE model
#ols
mod1<-lm(formula= as.numeric(PTPUBTRN) ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB, data = dat.n)
summary(mod1)

#BASE + neighborhood amenities
#ols
mod3<-lm(formula= as.numeric(PTPUBTRN) ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB  + WNAMEN + WNJOB + WNTRAN, data = dat.n)
summary(mod3)

#BASE model
#logit 
dat.n$PTPUBTRN<-factor(dat.n$PTPUBTRN, labels = c(1, 0))
mod2<-glm(formula= PTPUBTRN ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB, data = dat.n, family = "binomial"(link = "logit"))
summary(mod2)

#BASE + neighborhood amenities
#logit
mod4<-glm(formula= PTPUBTRN ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB + WNAMEN + WNJOB + WNTRAN, data = dat.n, family = "binomial"(link = "logit"))
summary(mod4)

#produce regression output
library(stargazer)
stargazer(mod1, mod3, mod2, mod4, type = "text",
          dep.var.labels = c("Public Transport Use", "Public Transport Use"),
          out = "regmodels.txt")
stargazer(mod1, mod3, mod2, mod4, type = "html",
          dep.var.labels = c("Public Transport Use", "Public Transport Use"),
          out = "regmodels.htm")

#visuals
#household uses public transportation
library(ggplot2)
ggdat<-data.frame(Use=dat.n$PTPUBTRN)
ggdat<-na.omit(ggdat)
ggdat$Use<-factor(ggdat$Use, levels = c(1,0), labels = c("Yes","No"))
ggplot(data=ggdat,aes(x=Use, fill=Use))+
  geom_bar()+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Indicator of Public Transportation Use")+
  ylab("Count")+
  ggtitle("National Indicator of Public Transportation Use")

#metro
#MSA - metropolitan statistical area
ggdat<-data.frame(Metro=dat.n$METRO3)
ggdat<-na.omit(ggdat)
ggdat$Metro<-factor(ggdat$Metro, levels = c(1,2,3), labels = c("Central City", "In MSA - Urban", "In MSA - Rural", "Outside MSA - Urban", "Outside MSA - Rural"))
ggplot(data=ggdat,aes(x=Metro, fill=Metro))+
  geom_bar()+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Distance to Metro Area")+
  ylab("Count")+
  ggtitle("Metro Status")

#cars
ggdat<-data.frame(Cars=dat.n$CARS)
ggdat<-na.omit(ggdat)
ggdat$Cars<-factor(ggdat$Cars, levels = c(0,1,2,3,4,5), labels = c(0,1,2,3,4,"5+"))
ggplot(data=ggdat,aes(x=Cars, fill=Cars))+
  geom_bar()+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Number of Cars")+
  ylab("Count")+
  ggtitle("Number of Cars per Household")

#trucks
ggdat<-data.frame(Trucks=dat.n$TRUCKS)
ggdat<-na.omit(ggdat)
ggdat$Trucks<-factor(ggdat$Trucks, levels = c(0,1,2,3,4,5), labels = c(0,1,2,3,4,"5+"))
ggplot(data=ggdat,aes(x=Trucks, fill=Trucks))+
  geom_bar()+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Number of Trucks")+
  ylab("Count")+
  ggtitle("Number of Trucks per Household")

#household income
ggdat<-data.frame(Income=dat.n$ZINC2)
ggdat<-na.omit(ggdat)
ggplot(data=ggdat,aes(x=Income))+
  geom_histogram(color="black", fill="blue")+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Household Income")+
  ylab("Count")+
  ggtitle("National Household Income Demographic")+
  theme(legend.title = element_blank())

#general distance
ggdat<-data.frame(Distance=dat.n$PTDISPUB)
ggdat<-na.omit(ggdat)
ggdat$Distance<-factor(ggdat$Distance, levels = c(1,2,3,4), labels = c("< 1/4 Mile", "1/4 - 1/2 Mile", "1/2 - 1 Mile", "1+ Miles"))
ggplot(data=ggdat,aes(x=Distance, fill=Distance))+
  geom_bar()+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Distance from Public Transportation")+
  ylab("Count")+
  ggtitle("General Distance from Public Transportation")

#bus distance 
ggdat<-data.frame(Distance=dat.n$PTDISBUS)
ggdat<-na.omit(ggdat)
ggdat$Distance<-factor(ggdat$Distance, levels = c(1,2,3,4), labels = c("< 1/4 Mile", "1/4 - 1/2 Mile", "1/2 - 1 Mile", "1+ Miles"))
ggplot(data=ggdat,aes(x=Distance, fill=Distance))+
  geom_bar()+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Distance from Bus Stop")+
  ylab("Count")+
  ggtitle("Distance from Bus Stop")

#rail distance 
ggdat<-data.frame(Distance=dat.n$PTDISRAIL)
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
ggdat<-data.frame(Distance=dat.n$PTDISSHUT)
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
ggdat<-data.frame(Distance=dat.n$PTDISSUB)
ggdat<-na.omit(ggdat)
ggdat$Distance<-factor(ggdat$Distance, levels = c(1,2,3,4), labels = c("< 1/4 Mile", "1/4 - 1/2 Mile", "1/2 - 1 Mile", "1+ Miles"))
ggplot(data=ggdat,aes(x=Distance, fill=Distance))+
  geom_bar()+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Distance from Subway Stop")+
  ylab("Count")+
  ggtitle("Distance from Subway Stop")

#wnamen
ggdat<-data.frame(Amenities=dat.n$WNAMEN)
ggdat<-na.omit(ggdat)
ggdat$Amenities<-factor(ggdat$Amenities, levels = c(1,0), labels = c("Yes", "No"))
ggplot(data=ggdat,aes(x=Amenities, fill=Amenities))+
  geom_bar()+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Moved to be Closer to Amenities")+
  ylab("Count")+
  ggtitle("Moved to be Closer to Amenities")

#wnjob
ggdat<-data.frame(Work=dat.n$WNJOB)
ggdat<-na.omit(ggdat)
ggdat$Work<-factor(ggdat$Work, levels = c(1,0), labels = c("Yes", "No"))
ggplot(data=ggdat,aes(x=Work, fill=Work))+
  geom_bar()+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Moved to be Closer to Work")+
  ylab("Count")+
  ggtitle("Moved to be Closer to Work")

#wntrain
ggdat<-data.frame(Transport=dat.n$WNTRAN)
ggdat<-na.omit(ggdat)
ggdat$Transport<-factor(ggdat$Transport, levels = c(1,0), labels = c("Yes", "No"))
ggplot(data=ggdat,aes(x=Transport, fill=Transport))+
  geom_bar()+
  theme_bw()+
  geom_hline(yintercept=0)+
  xlab("Moved to be Closer to Transportation")+
  ylab("Count")+
  ggtitle("Moved to be Closer to Transportation")

#LASSO - load the library
library(glmnet)
library(tidyverse)
library(caret)
library(Matrix)
library(foreach)

#split the data into training and test set
set.seed(13)
training.sample <- dat.n2$PTPUBTRN %>%
  createDataPartition(p = 0.8, list=FALSE)
train.data <- dat.n2[training.sample, ]
test.data <-dat.n2[-training.sample, ]

#dummy code categorical predictor variables
x <- model.matrix(as.numeric(PTPUBTRN)~.- CONTROL, train.data)[,-1]
y <- as.numeric(train.data$PTPUBTRN)
# Find the best lambda using cross-validation
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(cv.lasso)
#fit the model on the training data
model<- glmnet(x, y, family = "binomial", alpha = 1, 
               lambda = cv.lasso$lambda.min, maxit = 1000000)
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

### refit to get p-values
train <- dplyr::select(train.data, CONTROL, label, vars_kept)
test <- dplyr::select(test.data, CONTROL, label, vars_kept)

# fit model
mod3 <- glm(label ~ ., data = train[,-1], family="binomial")
summary(mod3)

#RIDGE REGRESSION

library(tidyverse)
library(broom)
library(glmnet)
library(ridge)
library(car)
#predict with just linear regression
lmMod<- lm(PTPUBTRN~., data = train.data)
summary(lmMod)

#predict on test data
predictedlm<- predict(lmMod, test.data)
#combine actual and predicted
compare <- cbind(actual=test.data$PTPUBTRN, predictedlm)
#calculate accuracy
mean(apply(compare,1,min)/apply(compare,1,max))

#ridge regression model
linRidgeMod <- linearRidge(PTPUBTRN ~ ., data = test.data)
#predict on test data
predictedRidgeMod <- predict(linRidgeMod, test.data)
#compare
compare <- cbind(actual=test.data$PTPUBTRN, predictedRidgeMod)
#calculate accuracy
mean(apply(compare,1,min)/apply(compare,1,max))


#RANDOM FOREST
library(randomForest)
rf <- randomForest(as.numeric(PTPUBTRN) ~. - CONTROL, data=train.data)
