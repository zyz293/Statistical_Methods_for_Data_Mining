library(leaps)
setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/assignment/5')
data4 = read.csv('HW5_data.csv', header = T)
newdata = sapply(data4[,-1], function(x)(x-mean(x))/sd(x))
data = data.frame(newdata)

lm0 = lm(marathon~1, data=data)  #  Q1_a
lm7 = lm(marathon~., data=data)
step(lm0, scope=list(lower=formula(lm0), upper=formula(lm7)), direction='both', trace=1)

best = leaps(data[,1:7], data[,8], method='Cp', nbest=2, names=names(data[,1:7]))  #  Q1_b
data.frame(size=best$size, Cp=best$Cp, best$which)

