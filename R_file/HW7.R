setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/assignment/7')
data = read.csv('HW7_data.csv', header = T)
data=data[,1:2]
yprime = 1 / data$y
xprime = 1 / data$x
lm = lm(yprime~xprime, data=data)
summary(lm)
r0 = 1 / lm$coef[1]
r0
r1 = r0 * lm$coef[2]
r1

setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/assignment/7')
data = read.csv('HW7_data.csv', header = T)
data=data[,1:2]
fn = function(p) {yhat=p[1]*data$x/(p[2]+data$x); sum((data$y-yhat)^2)}
out=nlm(fn, p=c(29.62201, 13.44881), hessian=TRUE)
gamma = out$estimate
gamma

library(boot)
setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/assignment/7')
data = read.csv('HW7_data.csv', header = T)
data=data[,1:2]
fit = function(Z, i, theta0) {
  Zboot = Z[i,]
  y = Zboot[[1]]; x = Zboot[[2]]
  fn =function(p) {yhat=p[1]*x/(p[2]+x); sum((y-yhat)^2)}
  out = nlm(fn, p=theta0)
  theta = out$estimate}
databoot = boot(data, fit, R=50000, theta0=c(29.62201, 13.44881))
CovTheta = cov(databoot$t)
SE = sqrt(diag(CovTheta))
databoot
CovTheta
SE
plot(databoot, index=1)
boot.ci(databoot, conf=c(0.9,0.95,0.99), index=1, type=c('norm','basic'))
plot(databoot, index=2)
boot.ci(databoot, conf=c(0.9,0.95,0.99), index=2, type=c('norm','basic'))
