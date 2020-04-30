# Sujit Molleti and Amith Nitin

data <- read.csv("NJHealth1000.csv", head=T, sep=",")
summary(data)

#basic commands to get overview of the dataset
str(data) #summarize dataset (look at this along with codebook)
summary(data) # summary of all the variables
names(data)  # what variables are included  (look at this along with codebook)

#1st regression
#declaring new variables
data$agesq = data$age * data$age
data$sex.f = factor(data$sex)

#turning off scientific notation
options(scipen  = 999)


reg<-lm(weight ~ age + agesq + sex.f + middleincome + highincome + married + height, data=data)
summary(reg)

library(zoo)
library(lmtest)
library(sandwich)

# redo the first regression but with heteroskedasticty robust standard errors
coeftest(reg, vcov=vcovHC(reg))
summary(reg)

#2nd Regression
#adding education and race

data$race.c = factor(data$race)
data$educ.c = factor(data$educ)

reg2<-lm(weight ~ age + agesq + sex.f + middleincome + highincome + married + height + educ.c + race.c, data=data)
coeftest(reg2, vcov=vcovHC(reg2))
summary(reg2)

#3rd Regression
#without height
reg3<-lm(weight ~ age + agesq + sex.f + middleincome + highincome + married + educ.c + race.c, data=data)
coeftest(reg3, vcov=vcovHC(reg3))
summary(reg3)

#3e
reg3e<-lm(weight ~ age + agesq + sex.f + lowincome + middleincome + married + height, data=data)
summary(reg3e)
coeftest(reg3e, vcov=vcovHC(reg3e))


### Practicing Joint Hypothesis Tests in R ###
# load built-in data mtcars

#run a regression of mpg on wt (weight), hp (horsepower), and cyl (# cylinders)
model<-lm(weight ~ age + agesq + sex.f + middleincome + highincome + married + height + educ.c + race.c, data=data)

#model<-lm(mpg~wt+hp+cyl, data=mtcars)

#now we want to test some joint hypotheses. Need package "car" 
# in order to use linearHypothesis command
#install.packages("car")
library(car)

#Joint Hypothesis Tests
linearHypothesis(model, c("race.c2=0", "race.c3=0", "race.c4=0", "race.c5=0"))
linearHypothesis(model, c("educ.c2=0", "educ.c3=0", "educ.c4=0", "educ.c5=0", "educ.c6=0"))

# test joint hypothesis that coef on hp = coef on wt
#linearHypothesis(model,c("middleincome=highincome"))

