library(MASS)
setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/assignment/4')
data1 = read.csv('HW4_data1.csv', header = T)
data2 = read.csv('HW4_data2.csv', header = T)
data4 = read.csv('HW4_data4.csv', header = T)

lm1 = lm(y~x1+x2+x5, data = data1)
summary(lm1)
lm2 = lm(y~x2+x5, data = data1)
summary(lm2)
est = rstandard(lm2)
plot(data1$x1[data1$x2==1], est[data1$x2==1], xlab='x1', ylab='standardized residual')
points(data1$x1[data1$x2==0], est[data1$x2==0], pch=3)
legend(75,1.7,c('male','female'), pch=c(1,3))
plot(data1$x5, est)

lm3 = lm(PIQ~., data = data2)
beta1_star = lm3$coef[2]*7.25/22.6
beta1_star
beta2_star = lm3$coef[3]*3.99/22.6
beta2_star
beta3_star = lm3$coef[4]*23.48/22.6
beta3_star
data2$PIQ = (data2$PIQ - mean(data2$PIQ))/sd(data2$PIQ)
data2$MRI = (data2$MRI - mean(data2$MRI))/sd(data2$MRI)
data2$Height = (data2$Height - mean(data2$Height))/sd(data2$Height)
data2$Weight = (data2$Weight - mean(data2$Weight))/sd(data2$Weight)
lm4 = lm(PIQ~., data = data2)
lm4$coef
summary(lm4)

data2 = read.csv('HW4_data2.csv', header = T)
R = cor(data2[2:4]); round(R,3)
lm5 = lm(MRI~Height+Weight, data=data2)
summary(lm5)
lm6 = lm(Height~MRI+Weight, data=data2)
summary(lm6)
lm7 = lm(Weight~Height+MRI, data=data2)
summary(lm7)
VIF = diag(solve(R))
VIF
#data4$marathon = (data4$marathon - mean(data4$marathon))/sd(data4$marathon)
#data4$100m = (data4$100m - mean(data4$100m))/sd(data4$100m)
#data4$200m = (data4$200m - mean(data4$200m))/sd(data4$200m)
#data4$400m = (data4$400m - mean(data4$400m))/sd(data4$400m)
#data4$800m = (data4$800m - mean(data4$800m))/sd(data4$800m)
#data4$1500m = (data4$1500m - mean(data4$1500m))/sd(data4$1500m)
#data4$5000m = (data4$5000m - mean(data4$5000m))/sd(data4$5000m)
#data4$10000m = (data4$10000m - mean(data4$10000m))/sd(data4$10000m)
newdata = sapply(data4[,-1], function(x)(x-mean(x))/sd(x))
newdata = data.frame(newdata)
lm8 = lm.ridge(marathon~. -1, data=newdata, lambda=5)
lm8$coef
lm9 = lm(marathon~., data=newdata)
summary(lm9)

CVInd = function(n, K){
  m = floor(n/K)
  r = n-m*K
  I = sample(n,n)
  Ind = list()
  length(Ind) = K
  for (k in 1:K){
    if (k <= r) kpart = ((m+1)*(k-1)+1):((m+1)*k)
    else kpart = ((m+1)*r+m*(k-r-1)+1):((m+1)*r+m*(k-r))
    Ind[[k]] = I[kpart]
  }
  Ind
}
lambda = vector(mode='numeric', length=30)
for (i in 1:30) lambda[31-i] = 64/1.5^(i-1)
outridge = lm.ridge(marathon~.-1, data = newdata, lambda=lambda)
plot(outridge)
select(outridge)

#y = newdata[[8]]
#yhat = y
lambda = seq(0.5,10,0.5)
CVSSE = c(0)
for (i in 1:length(lambda)) {
  lambda = lambda[i]
  SSE1 = c(0)
  for (j in 1:30) {
    n=55;K=10
    Ind = CVInd(n=n,K=K)
    SSE = c(0)
    for (k in 1:K){
      out = lm.ridge(marathon~.-1, data=newdata[-Ind[[k]],], lambda=lambda)
      data = as.matrix(cbind(newdata[Ind[[k]],1:7]))
      yhat = data %*% as.matrix(out$coef)
      SSE[k] = sum((as.vector(t(yhat))-newdata[Ind[[k]],]$marathon)^2)
    }
    SSE1[j] = sum(SSE) 
  }
  CVSSE[i] = sum(SSE)/30
}
CVSSE
