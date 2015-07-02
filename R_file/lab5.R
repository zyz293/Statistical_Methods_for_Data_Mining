setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/lab/5')
origin = read.csv('Lab5_data.csv', header = T)
origin = origin[1:126,2:5]
f = sum(origin$A[origin$t<0.3&origin$D>0.6])/sum(origin$A)   #  Q1
f
R = mean(origin$R[origin$t<0.3&origin$D>0.6])
R

library(boot)
setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/lab/5') 
origin = read.csv('Lab5_data.csv', header = T)
origin = origin[1:126,2:5]
f = function(d,i) {
  d2 = d[i,]
  return(sum(d2$A[d2$t<0.3&d2$D>0.6])/sum(d2$A))
}
databoot1 = boot(origin, f, R=100)
# databoot1$t # the f of every resampling
boot.ci(databoot1, conf=0.95, index=1, type=c('norm','basic'))
plot(databoot1, index=1)

R = function(d,i) {
  d2 = d[i,]
  return(mean(d2$R[d2$t<0.3&d2$D>0.6]))
}
databoot2 = boot(origin, R, R=100)
# databoot2$t # the R of every resampling
boot.ci(databoot2, conf=0.95, index=1, type=c('norm','basic'))
plot(databoot2, index=1)
# http://statistics.ats.ucla.edu/stat/r/faq/boot.htm
# the website of how to use boot