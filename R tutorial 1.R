# You first need to set the working directory to the *folder* where your
# data is (wherever you put "ChickWeight.csv).  
# Go to Session->Set Working Directory->Choose Directory and Navigate there

# create on object called data that has the data from the .csv file
data<-read.csv('ChickWeight.csv', head=T, sep=",") #
#head=T means headers=True, that is the data has a header row with variable names
# sep says the data uses , to separate 

dim(data) #how many observations and varaibles does the data have?
names(data)  #tells the names of the variables in the dataset
summary(data) #gives you a summary of the variables in the dataset
mean(data$weight)  #tells you the mean of weight
mean(data$weight[data$feed=="casein"]) #mean of weight for Chicks who ate Casein
mean(data$weight[data$feed=="meatmeal"]) #mean of weight for Chicks who ate meatmeal
# == above is a rational operate meaning equal to
# you can also use <, >, <=, >=, or != (not equals)

mean(data$weight[data$weight>300]) #for fun: mean of weight only for chicks who weigh more than 300grams

sd(data$weight[data$feed=="casein"]) #standard dev of weight for Chicks who ate casein
sd(data$weight[data$feed=="meatmeal"]) #standard dev of weight for Chicks who ate meatmeal

#Standard Error
sd(data$weight[data$feed=="casein"])/sqrt(length(which(data$feed=="casein"))) #std error is just standard dev/sqrt(N)
#length(which(data$feed=="casein")) is the number of obs for which feed is casein
sd(data$weight[data$feed=="meatmeal"])/sqrt(length(which(data$feed=="meatmeal"))) #std error of weight for chicks who ate meatmeal

#Create a new data object, data2, which is the same as data but eliminates observations with feed type unknown
data2 <- data[data$feed!="unknown",] #Don't forget using ',' to separate rows and columns

t.test(data2$weight~data2$feed, paired=F, var.eq=T)  
# tests difference in means of weight by values of feed
# must use data2 because feed can only have two levels for the t-test
# tests Ho: means are equal when assuming equal standard deviation
# tells you p-value of mean difference, as well as 95% CI for mean difference

