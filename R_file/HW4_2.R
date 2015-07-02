library(MASS)
setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/assignment/4')
data4 = read.csv('HW4_data4.csv', header = T)
newdata = sapply(data4[,-1], function(x)(x-mean(x))/sd(x))
X1 = data.frame(newdata)
CVInd <- function(n,K) {  #n is sample size; K is number of parts; returns K-length list of indices for each part
  m<-floor(n/K)  #approximate size of each part
  r<-n-m*K  
  I<-sample(n,n)  #random reordering of the indices
  Ind<-list()  #will be list of indices for all K parts
  length(Ind)<-K
  for (k in 1:K) {
    if (k <= r) kpart <- ((m+1)*(k-1)+1):((m+1)*k)  
    else kpart<-((m+1)*r+m*(k-r-1)+1):((m+1)*r+m*(k-r))
    Ind[[k]] <- I[kpart]  #indices for kth part of data
  }
  Ind
}
lam = seq(0.5,10,0.5)
CVSSE = c(0)
for (i in 1:20){
  lambda = lam[i]
  SSE = c(0)
  for (j in 1:100){
    n=55;K=10
    Ind<-CVInd(n=n,K=K)
    y<-X1[[8]]
    yhat<-y
    for (k in 1:K) {
      out<-lm.ridge(marathon~. -1,data=X1[-Ind[[k]],], lambda=lambda)
      yhat[Ind[[k]]]<-as.matrix(X1[Ind[[k]],1:7])%*%out$coef 
    } 
    SSE[j] = sum((y-yhat)^2)  #CV SSE
  }
  CVSSE[i] = sum(SSE)/100
}
CVSSE

