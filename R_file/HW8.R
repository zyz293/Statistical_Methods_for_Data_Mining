library(tree)
setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/assignment/8')
data = read.csv('HW8_data.csv', header = T)
control = tree.control(nobs=nrow(data),mincut=2,minsize=4,mindev=0.002)
data.tr = tree(y~., data, control=control)
data.tr
summary(data.tr)
plot(data.tr, type='u')
text(data.tr, digits=3)
data.tr1 = prune.tree(data.tr)
plot(data.tr1)
plot(cv.tree(data.tr, ,prune.tree))
data.tr1 = prune.tree(data.tr, best=8)
data.tr1
plot(data.tr1, type='u')
text(data.tr1, digits=3)

library(nnet)
library(caret)
setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/assignment/8')
data = read.csv('HW8_data.csv', header = T)
k = ncol(data)
data1 = data
data1[2:k] = sapply(data1[2:k], function(x) (x-mean(x))/sd(x))
# data1[1] = (data1[1]-min(data1[1]))/(max(data1[1])-min(data1[1]))
fitcontrol = trainControl(method='repeatedcv', repeats=20)
Grid = expand.grid(.decay=seq(0.1,1.5,0.1), .size=seq(5,15,5))
fit = train(y~., data=data1, method='nnet', trControl=fitcontrol, tuneGrid=Grid, linout=T, maxit=1000, trace=F, skip=F)
fit
m1 = mean(data[,2]); m2 = mean(data[,3]); m3 = mean(data[,4])
s1 = sd(data[,2]); s2 = sd(data[,3]); s3 = sd(data[,4])
a1 = (3-m1)/s1; a1
a2 = (28-m2)/s2; a2
a3 = (1-m3)/s3; a3
newdata = data.frame(x1=a1, x2=a2, x3=a3)
fit1 = nnet(y~., data=data1, linout=T, decay=1.4, size=5, trance=F, maxit=1000, skip=F)
ypred = predict(fit1, newdata)
ypred
# max = max(data[,1]); min = min(data[,1])
# yhat = ypred*(max-min)+min
# yhat
