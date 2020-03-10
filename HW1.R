data<-read.csv('birthweight.csv', head=T, sep=",") 
#summary(data)
data2 <- data[data$smoke!=9,]
