data<-read.csv('birthweight.csv', head=T, sep=",") 
#summary(data)
data2 <- data[data$smoke!=9,]

smokerMean <- mean(data2$bwt[data2$smoke == 1])
smokerMean
non_smokerMean <- mean(data2$bwt[data2$smoke == 0])
non_smokerMean

difference <- mean(data2$bwt[data2$smoke == 0]) - mean(data2$bwt[data2$smoke == 1])
difference

smokerSE <- sd(data2$bwt[data2$smoke==0])/sqrt(length(which(data2$smoke==0)))
smokerSE
non_smokerSE <- sd(data2$bwt[data2$smoke==1])/sqrt(length(which(data2$smoke==1)))
non_smokerSE

smokerMean + 1.96*smokerSE
smokerMean - 1.96*smokerSE
#(112.3026, 115.0686)

non_smokerMean + 1.96*non_smokerSE
non_smokerMean - 1.96*non_smokerSE
#(121.4745, 125.0951)

diffSE <- sqrt(smokerSE + non_smokerSE)
diffSE

(non_smokerMean - smokerMean) + 1.96*diffSE
(non_smokerMean - smokerMean) - 1.96*diffSE

t.test(data2$bwt~data2$smoke, paired=F, var.eq=T)  
