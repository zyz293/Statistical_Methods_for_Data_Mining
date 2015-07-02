############Lab5#############

setwd("~/Documents/IEMS 304/Lab5")
library(gdata)

############Pro1#############


f=sum(data$A_sorted)/sum(data$A);f
miu=sum(data$R_sorted)/sum(data$Ind);miu


############Pro2#############

data = read.xls("Lab5_Data.xls", sheet = 1); data[1:10,]
library(boot) #need to load the boot package 
MLC<-data 
MLCfit<-function(Z,i) {Zboot<-Z[i,];A<-Zboot[[2]];
                       Ind<-Zboot[[6]];As<-Zboot[[7]];Rs<-Zboot[[9]];
                       f=sum(As)/sum(A)
                       miu=sum(Rs)/sum(Ind)
esti=c(f,miu)} 
MLCboot<-boot(MLC, MLCfit, R=100)
MLCboot$t

############Pro3#############
CovTheta<-cov(MLCboot$t);CovTheta
SE<-sqrt(diag(CovTheta));SE

fbar=sum(MLCboot$t[,1])/100;fbar
mbar=sum(MLCboot$t[,2])/100;mbar

############Pro4#############
hist(MLCboot$t[,1],xlab="f",main="Histogram of f") #index=i calculates results for ith parameter 
hist(MLCboot$t[,2],xlab="R",main="Histogram of R") 

############Pro5 & 6#############
boot.ci(MLCboot,conf=.95,index=1,type=c("norm","basic"))
boot.ci(MLCboot,conf=.95,index=2,type=c("norm","basic"))
quantile(MLCboot$t[,1],0.025)
quantile(MLCboot$t[,1],0.975)
quantile(MLCboot$t[,2],0.025)
quantile(MLCboot$t[,2],0.975)

