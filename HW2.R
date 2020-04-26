data <- read.csv("NJHealth1000.csv", head=T, sep=",")
summary(data)

#basic commands to get overview of the dataset
str(data) #summarize dataset (look at this along with codebook)
summary(data) # summary of all the variables
names(data)  # what variables are included  (look at this along with codebook)

#declaring new variables
data$agesq = data$age * data$age
data$sex.f = factor(data$sex)

#turning off scientific notation
options(scipen  = 999)

#1st regression
reg<-lm(weight ~ agesq + sex.f + middleincome + highincome + married + height, data=data)
summary(reg)

library(zoo)
library(lmtest)
library(sandwich)

# redo the second regression but with heteroskedasticty robust standard errors
coeftest(reg, vcov=vcovHC(reg))
summary(reg)

#adding education and weight
reg2<-lm(weight ~ agesq + sex.f + middleincome + highincome + married + height + educ + race, data=data)
coeftest(reg2, vcov=vcovHC(reg2))
summary(reg2)

#without height
reg3<-lm(weight ~ agesq + sex.f + middleincome + highincome + married + educ + race, data=data)
coeftest(reg3, vcov=vcovHC(reg3))
summary(reg3)

#3e
reg3e<-lm(weight ~ agesq + sex.f + lowincome + middleincome + married + height, data=data)
summary(reg3e)
coeftest(reg3e, vcov=vcovHC(reg3e))

