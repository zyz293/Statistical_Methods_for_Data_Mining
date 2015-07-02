library(rgl)
setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/lab/4')
data1 = read.csv('Lab4_data1.csv', header = T)
data2 = read.csv('Lab4_data2.csv', header = T)
data2=data2[,1:3]
glm1 = glm(y~., family=binomial(link='logit'), data=data1)
summary(glm1)
x1 = seq(55, 80, length.out=30)
x2 = seq(80, 240, length.out=30)
f = function(x1, x2) exp(-33.36743+0.05688*x1+0.21042*x2) / (1 + exp(-33.36743+0.05688*x1+0.21042*x2) )
z = outer(x1, x2, f)
persp3d(x1, x2, z, front='line', back='line')
rgl.snapshot('fig1')
a2 = seq(80, 240, by=1)
a1 = 586.63 - 3.7*a2
plot(a2, a1, type='l', xlim=c(80,240), ylim=c(55,80))

glm2 = glm(y~., family=binomial(link='logit'), data=data1[-39,])
summary(glm2)
x1 = seq(55, 80, length.out=30)
x2 = seq(80, 240, length.out=30)
f = function(x1, x2) exp(-595.738+3.716*x1+2.535*x2) / (1 + exp(-595.738+3.716*x1+2.535*x2) )
z = outer(x1, x2, f)
persp3d(x1, x2, z, front='line', back='line')
rgl.snapshot('fig1')
x2 = seq(80, 240, by=1)
x1 = 160.32 - 0.68*a2
plot(x2, x1, type='l', xlim=c(80,240), ylim=c(55,80))


glm7 <- glm(y ~ ., family = binomial(link="logit"), data = data2)
yhat<-predict(glm7,type="response")
# data2$y<-jitter(data2$y,factor=.2); 
plot(yhat,jitter(data2$y, factor=0.2),xlab="Fitted Prob",ylab="Response")
identify(yhat,data2$y)
