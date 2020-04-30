#' First set working directory to whatever folder
#' your data is in (this should preferably be like an "R Tutorial"
#' folder and not just "Downloads" but whatever works for you)
#' You can do this by going to "Session->Set Working Directory->Choose Directory
#' Or if you know the path just set it below like so:
setwd("~/Dropbox/Teaching/Rutgers/Econometrics Rutgers/R Tutorial 2")

# now let's load the data
# I am calling my data object "cpsdata" for today
# You can call it whatever you want, but make it useful
cpsdata<-read.csv('cps2012.csv', head=T, sep=",")

#### Some commands to explore the data ####
str(cpsdata) #summarize dataset (look at this along with codebook)
summary(cpsdata) # summary of all the variables
names(cpsdata)  # what variables are included  (look at this along with codebook)
levels(cpsdata$nchlt5) # what values does "nchlt5" take on  (look at this along with codebook)
levels(cpsdata$sex) # what values does the variable "sex" take on?
#### some summary statistics ####
table(cpsdata$nchlt5)  #frequency of each category of nchlt5
# what if you want proportion in each category?
# first define a new object that is the table of the variable you are interested in
nchild<-table(cpsdata$nchlt5)
prop.table(nchild)  # then this gives you the proportion in each category

# what about for gender?
pmale<-table(cpsdata$sex)
prop.table(pmale)

#### 'cleaning'/editing the data ####
#' Age is a "factor" or "categorical" variable in the dataset
#' but we want to transfrom age into numeric
cpsdata$agenum=as.numeric(cpsdata$age) #replace factor version of age with numeric

#' missing values of incwage are coded as 999998 or 999999
#' this will really throw off our regressions and sum stats
#' see for example:
mean(cpsdata$incwage) #says mean is $2,379,894!!!

# remember we can always create new variables like:
cpsdata$agesquared=cpsdata$agenum * cpsdata$agenum

mean(cpsdata$agesquared)
#' instead, for now, create new data object, cpsdata2 which gets rid of those values
#' with missing data 
cpsdata2<-subset(cpsdata,incwage<999998)

# this data also includes values of age that are really low
# and really high, we want to do some analysis on wages, so let's subset
# this data again to only people of "working age"
# and people who report any wages
min(cpsdata$agenum) #min value of age
max(cpsdata$agenum) #max value of age

cpsdata3<-subset(cpsdata2,incwage>0 & agenum>18 & agenum<65) #subset to working age people who report any wages


#### more summary stats #####
mean(cpsdata3$agenum)
mean(cpsdata3$incwage) # mean now makes more sense!
median(cpsdata3$incwage)
sd(cpsdata3$incwage)


#### Now let's run some regressions ####

# regress wages on age, in the subsetted data
# reg1 is just the name for our regression object, you can call it anything
reg1<-lm(incwage ~ agenum, data=cpsdata3)
summary(reg1) # shows you the result of the regression

# now let's add gender
# note that R is pretty smart about figuring out mutually exclusive categorical variables
# see what happens when you put in sex?
reg2<-lm(incwage ~ agenum + sex, data=cpsdata3)
summary(reg2)

#the above regressions reported regular standard errors that assume homokedasticity
#not usually a great assumption, for example let's create a scatter plot of 
# age and wages:
require(scales)

install.packages("ggplot2")  #install a new package ggplot
library("ggplot2") #load the ggplot2 package
ggplot(cpsdata3,(aes(x=agenum,y=incwage)))+ geom_point()  #simple scatter plot

ggplot(cpsdata3,(aes(x=agenum,y=incwage)))+ geom_point() +  #simple scatter plot
  scale_y_continuous(labels=comma)+                 # dont use sci notation for wage
  scale_x_continuous(breaks=seq(20,100,10))    #make axis easier to read

# so we want instead to use heteroskedasticity robust standard errors
# first you need to install 2 new packages: sandwich and lmtest
# once you install the packages once you don't need to install them again
# though you do need "library()" commands each time
install.packages("sandwich")
install.packages("lmtest")
library(zoo)
library(lmtest)
library(sandwich)

# redo the second regression but with heteroskedasticty robust standard errors
coeftest(reg2, vcov=vcovHC(reg2))
summary(reg2)
# now let's add health
# note that R is pretty smart about figuring out mutually exclusive categorical variables
# see what happens when you put in health?
reg3<-lm(incwage ~ agenum + sex + health, data=cpsdata3)
summary(reg3)
coeftest(reg3, vcov=vcovHC(reg3)) # get heteroskedasticty robust SEs
# only issue is you need to look at codebook to figure out which is the omitted category!


### Practicing Joint Hypothesis Tests in R ###
# load built-in data mtcars
data("mtcars")
head(mtcars)  #allows you to see the first 10 observations

#run a regression of mpg on wt (weight), hp (horsepower), and cyl (# cylinders)
model<-lm(mpg~wt+hp+cyl, data=mtcars)

#now we want to test some joint hypotheses. Need package "car" 
# in order to use linearHypothesis command
install.packages("car")
library(car)

# test joint hypothesis that coef on weight AND coef on cyl both =0
linearHypothesis(model, c("wt=0", "cyl=0"))

# test joint hypothesis that coef on hp = coef on wt
linearHypothesis(model,c("hp=wt"))


