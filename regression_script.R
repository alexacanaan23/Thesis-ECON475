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
#grocery
class(dat.n$GROCERY)
typeof(dat.n$GROCERY)
attributes(dat.n$GROCERY)
head(dat.n$GROCERY)
summary(dat.n$GROCERY)
#as.character(dat.n$GROCERY)
#as.numeric(dat.n$GROCERY)
dat.n$GROCERY <- factor(dat.n$GROCERY, labels = c(NA, NA, NA, "1", "2", "3"))

#drugstore
class(dat.n$DRUGSTORE)
typeof(dat.n$DRUGSTORE)
attributes(dat.n$DRUGSTORE)
head(dat.n$DRUGSTORE)
summary(dat.n$DRUGSTORE)
#as.character(dat.n$GROCERY)
#as.numeric(dat.n$GROCERY)
dat.n$DRUGSTORE <- factor(dat.n$DRUGSTORE, labels = c(NA, NA, NA, "1", "0"))

#wmclos - OPTIONAL?
class(dat.n$WMCLOS)
typeof(dat.n$WMCLOS)
attributes(dat.n$WMCLOS)
head(dat.n$WMCLOS)
summary(dat.n$WMCLOS)
#as.character(dat.n$WMCLOS)
#as.numeric(dat.n$WMCLOS)
dat.n$WMCLOS <- factor(dat.n$WMCLOS, labels = c(NA, NA, NA, NA, "1", "2"))

#wntran
class(dat.n$WNTRAN)
typeof(dat.n$WNTRAN)
attributes(dat.n$WNTRAN)
head(dat.n$WNTRAN)
summary(dat.n$WNTRAN)
#as.character(dat.n$WNTRAN)
#as.numeric(dat.n$WNTRAN)
dat.n$WNTRAN <- factor(dat.n$WNTRAN, labels = c(NA, NA, NA, NA, "1", "2"))

#wnamen
class(dat.n$WNAMEN)
typeof(dat.n$WNAMEN)
attributes(dat.n$WNAMEN)
head(dat.n$WNAMEN)
summary(dat.n$WNAMEN)
#as.character(dat.n$WNTRAN)
#as.numeric(dat.n$WNTRAN)
dat.n$WNAMEN <- factor(dat.n$WNAMEN, labels = c(NA, NA, NA, NA, "1", "2"))

#wnjob
class(dat.n$WNJOB)
typeof(dat.n$WNJOB)
attributes(dat.n$WNJOB)
head(dat.n$WNJOB)
summary(dat.n$WNJOB)
#as.character(dat.n$WNTRAN)
#as.numeric(dat.n$WNTRAN)
dat.n$WNJOB <- factor(dat.n$WNJOB, labels = c(NA, NA, NA, NA, "1", "2"))

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

dat.logit<-data.frame(dat.n$PTPUBTRN, dat.n$METRO3, dat.n$CARS, dat.n$TRUCKS, dat.n$ZINC2, dat.n$PTDISBUS, dat.n$PTDISPUB, dat.n$PTDISRAIL, dat.n$PTDISSHUT, dat.n$PTDISSUB, dat.n$DRUGSTORE, dat.n$GROCERY, dat.n$WNAMEN, dat.n$WNJOB, dat.n$WNTRAN)
# for (i in names(dat.n.omit)) {
#   if (is.factor(dat.n.omit[[i]]==TRUE)){
#     if (nlevels(dat.n.omit[[i]]) < 2) {
#       dat.n.omit[[i]]<-NULL
#     }
#   }
# }
dat.n$PTPUBTRN<-factor(dat.n$PTPUBTRN, labels = c(1, 0))
# dat.n.omit$PTPUBTRN<-factor(dat.n.omit$PTPUBTRN, labels = c(1, 0))
mod2<-glm(formula= PTPUBTRN ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB, data = dat.n, family = "binomial"(link = "logit"))
summary(mod2)

#BASE + neighborhood amenities
#logit
mod4<-glm(formula= PTPUBTRN ~ METRO3 + CARS + TRUCKS + ZINC2 + PTDISBUS + PTDISPUB + PTDISRAIL + PTDISSHUT + PTDISSUB + WNAMEN + WNJOB + WNTRAN, data = dat.n, family = "binomial"(link = "logit"))
summary(mod4)

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

#inspect the data
sample_n(dat.n2, 3)

#split the data into training and test set
set.seed(69)
training.sample <- dat.n2$PTPUBTRN %>%
  createDataPartition(p = 0.8, list=FALSE)
train.data <- dat.n2[training.sample, ]
test.data <-dat.n2[-training.sample, ]

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

#RIDGE REGRESSION
