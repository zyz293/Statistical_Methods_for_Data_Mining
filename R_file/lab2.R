setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/lab/2')
train = read.csv('Lab2Training.csv', header = T)
test = read.csv('Lab2Test.csv', header = T)
pairs(train$y~., train) #Q1

lm1 = lm(y~., data=train)  #Q2
summary(lm1)
yhtrain = predict(lm1, train)   # when using "lm" command can code "data=..."
yhtest = predict(lm1, test)    # but when using "predict" command do not code "data=..."
plot(yhtrain, train$y, xlab='yhat', ylab='y')
points(yhtest, test$y, pch=3, col="red")

plim = predict(lm1, test, interval="prediction", level=0.95)  #Q3
prederror = test$y - yhtest
matplot(c(1:136), plim[,-1], lty=c(1,2,2),col=c("black","red","red"), type='l')
points(c(1:136), yhtest)

inf1 = influence(lm1)  #Q4
round(data.frame(inf1$hat, inf1$coefficients),2)
cooks.distance(lm1)

stdres = rstandard(lm1) #Q5
plot(yhtrain, stdres)

lm2 = lm(y~x1+x2, data=train)  #Q6 
summary(lm2)
yhtrain2 = predict(lm2, train)
yhtest2 = predict(lm2, test)
plot(yhtrain2, train$y, xlab='yhat', ylab='y')
points(yhtest2, test$y, pch=3, col="red")
plim2 = predict(lm2, test, interval="prediction", level=0.95)  
prederror2 = test$y - yhtest2
matplot(c(1:136), plim2[,-1], lty=c(1,2,2),col=c("black","red","red"), type='l')
points(c(1:136), yhtest2)
inf2 = influence(lm2) 
round(data.frame(inf2$hat, inf2$coefficients),2)
cooks.distance(lm2)
stdres2 = rstandard(lm2)
plot(yhtrain2, stdres2)

anova(lm2, lm1)
