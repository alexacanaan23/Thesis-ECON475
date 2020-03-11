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
#as.character(dat.n$METRO3)
#as.numeric(dat.n$METRO3)
dat.n$METRO3 <- factor(dat.n$METRO3, labels = c("1", "2", "3", "4", "5"))

#cars
class(dat.n$CARS)
typeof(dat.n$CARS)
attributes(dat.n$CARS)
head(dat.n$CARS)
summary(dat.n$CARS)
#as.character(dat.n$CARS)
#as.numeric(dat.n$CARS)
dat.n$CARS <- factor(dat.n$CARS, labels = c(NA, "0", "1", "2", "3", "4", "5"))

#trucks
class(dat.n$TRUCKS)
typeof(dat.n$TRUCKS)
attributes(dat.n$TRUCKS)
head(dat.n$TRUCKS)
summary(dat.n$TRUCKS)
#as.character(dat.n$TRUCKS)
#as.numeric(dat.n$TRUCKS)
dat.n$TRUCKS <- factor(dat.n$TRUCKS, labels = c(NA, "0", "1", "2", "3", "4", "5"))

#Zinc2
class(dat.n$ZINC2)
typeof(dat.n$ZINC2)
attributes(dat.nt$ZINC2)
head(dat.n$ZINC2)
summary(dat.n$ZINC2)
#as.character(dat.n$ZINC2)
#as.numeric(dat.n$ZINC2)

#ptdisbus
class(dat.n$PTDISBUS)
typeof(dat.n$PTDISBUS)
attributes(dat.n$PTDISBUS)
head(dat.n$PTDISBUS)
summary(dat.n$PTDISBUS)
#as.character(dat.n$PTDISBUS)
#as.numeric(dat.n$pt)
dat.n$PTDISBUS <- factor(dat.n$PTDISBUS, labels = c(NA, NA, NA, "1", "2", "3", "4"))

#ptdispub
class(dat.n$PTDISPUB)
typeof(dat.n$PTDISPUB)
attributes(dat.n$PTDISPUB)
head(dat.n$PTDISPUB)
summary(dat.n$PTDISPUB)
#as.character(dat.n$PTDISPUB)
#as.numeric(dat.n$PTDISPUB)
dat.n$PTDISPUB <- factor(dat.n$PTDISPUB, labels = c(NA, NA, NA, "1", "2", "3", "4"))

#ptdisrail
class(dat.n$PTDISRAIL)
typeof(dat.n$PTDISRAIL)
attributes(dat.n$PTDISRAIL)
head(dat.n$PTDISRAIL)
summary(dat.n$PTDISRAIL)
#as.character(dat.n$PTDISRAIL)
#as.numeric(dat.n$PTDISRAIL)
dat.n$PTDISRAIL <- factor(dat.n$PTDISRAIL, labels = c(NA, NA, "1", "2", "3", "4"))

#ptdisshut
class(dat.n$PTDISSHUT)
typeof(dat.n$PTDISSHUT)
attributes(dat.n$PTDISSHUT)
head(dat.n$PTDISSHUT)
summary(dat.n$PTDISSHUT)
#as.character(dat.n$PTDISSHUT)
#as.numeric(dat.n$PTDISSHUT)
dat.n$PTDISSHUT <- factor(dat.n$PTDISSHUT, labels = c(NA, NA, NA, "1", "2", "3", "4"))

#ptdissub
class(dat.n$PTDISSUB)
typeof(dat.n$PTDISSUB)
attributes(dat.n$PTDISSUB)
head(dat.n$PTDISSUB)
summary(dat.n$PTDISSUB)
#as.character(dat.n$PTDISSUB)
#as.numeric(dat.n$PTDISSUB)
dat.n$PTDISSUB <- factor(dat.n$PTDISSUB, labels = c(NA, NA, NA, "1", "2", "3", "4"))

#DATA CLEANING - MODEL LEVEL 2
#wntran
class(dat.n$WNTRAN)
typeof(dat.n$WNTRAN)
attributes(dat.n$WNTRAN)
head(dat.n$WNTRAN)
summary(dat.n$WNTRAN)
#as.character(dat.n$WNTRAN)
#as.numeric(dat.n$WNTRAN)
dat.n$WNTRAN <- factor(dat.n$WNTRAN, labels = c(NA, NA, NA, NA, "1", "0"))

#wnamen
class(dat.n$WNAMEN)
typeof(dat.n$WNAMEN)
attributes(dat.n$WNAMEN)
head(dat.n$WNAMEN)
summary(dat.n$WNAMEN)
#as.character(dat.n$WNTRAN)
#as.numeric(dat.n$WNTRAN)
dat.n$WNAMEN <- factor(dat.n$WNAMEN, labels = c(NA, NA, NA, NA, "1", "0"))

#wnjob
class(dat.n$WNJOB)
typeof(dat.n$WNJOB)
attributes(dat.n$WNJOB)
head(dat.n$WNJOB)
summary(dat.n$WNJOB)
#as.character(dat.n$WNTRAN)
#as.numeric(dat.n$WNTRAN)
dat.n$WNJOB <- factor(dat.n$WNJOB, labels = c(NA, NA, NA, NA, "1", "0"))

#remove NA's
dat.nt.omit<-na.omit(dat.nt)
dat.n.omit<-na.omit(dat.n)

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
#dat.logit<-data.frame(dat.n$PTPUBTRN, dat.n$METRO3, dat.n$CARS, dat.n$TRUCKS, dat.n$ZINC2, dat.n$PTDISBUS, dat.n$PTDISPUB, dat.n$PTDISRAIL, dat.n$PTDISSHUT, dat.n$PTDISSUB, dat.n$WNAMEN, dat.n$WNJOB, dat.n$WNTRAN)
dat.n$PTPUBTRN<-factor(dat.n$PTPUBTRN, labels = c(1, 0))
mod2<-glm(formula= PTPUBTRN ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB, data = dat.n, family = "binomial"(link = "logit"))
summary(mod2)

#BASE + neighborhood amenities
#logit
mod4<-glm(formula= PTPUBTRN ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB + WNAMEN + WNJOB + WNTRAN, data = dat.n, family = "binomial"(link = "logit"))
summary(mod4)

#produce regression output
#install.packages("stargazer")
library(stargazer)
stargazer(mod1, mod3, mod2, mod4, type = "text",
          dep.var.labels = c("Public Transport Use", "Public Transport Use"),
          out = "regmodels.txt")
stargazer(mod1, mod3, mod2, mod4, type = "html",
          dep.var.labels = c("Public Transport Use", "Public Transport Use"),
          out = "regmodels.htm")

#visuals
#household uses public transportation
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
ggdat$Metro<-factor(ggdat$Metro, levels = c(1,2,3,4,5), labels = c("Central City", "In MSA - Urban", "In MSA - Rural", "Outside MSA - Urban", "Outside MSA - Rural"))
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

#remove NAs
dat.n2<- na.omit(dat.n.omit)

#remove variables that have less than 2 factors
for (i in names(dat.n2)) {
  if (nlevels(dat.n2[[i]]) < 2 & is.factor(dat.n2[[i]])==TRUE) {
    dat.n2[[i]]<-NULL
  }
}

dat.n2<-na.omit(dat.n2)

grid = 10^seq(10, -2, length = 100)

#split the data into training and test set
set.seed(69)
training.sample <- dat.n2$PTPUBTRN %>%
  createDataPartition(p = 0.8, list=FALSE)
train.data <- dat.n2[training.sample, ]
train.data <- na.omit(train.data)
test.data <-dat.n2[-training.sample, ]
test.data <- na.omit(test.data)

#dummy code categorical predictor variables
x <- model.matrix(PTPUBTRN~., train.data)[,-1]
y <- ifelse(train.data$PTPUBTRN == "pos", 1, 0)

glmnet(x, y, family = "binomial", alpha = 1, lambda = NULL)

# Find the best lambda using cross-validation
set.seed(123) 
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(cv.lasso)
# Fit the final model on the training data
model <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)
# Display regression coefficients
coef(model)
# Make predictions on the test data
x.test <- model.matrix(diabetes ~., test.data)[,-1]
probabilities <- model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
# Model accuracy
observed.classes <- test.data$diabetes
mean(predicted.classes == observed.classes)


for (i in names(dat.nt.omit)) {
  if (nlevels(dat.nt.omit[[i]]) < 2 & is.factor(dat.nt.omit[[i]])==TRUE) {
    dat.nt.omit[[i]]<-NULL
  }
}

dat.nt.omit<-na.omit(dat.nt.omit)

#inspect the data
sample_n(dat.nt.omit, 3)

#split the data into training and test set
set.seed(69)
training.sample <- dat.nt.omit$PTPUBTRN %>%
  createDataPartition(p = 0.8, list=FALSE)
train.data <- dat.nt.omit[training.sample, ]
test.data <-dat.nt.omit[-training.sample, ]

#dummy code categorical predictor variables
x <- model.matrix(PTPUBTRN~., train.data)[,-1]

#RIDGE REGRESSION
library(tidyverse)
library(broom)
library(glmnet)
#install.packages("ridge")
library(ridge)

