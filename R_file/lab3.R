setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/lab/3')
LAB3 = read.csv("Lab3.csv")
CVInd = function(n,K) {  #n is sample size; K is number of parts; returns K-length list of indices for each part
  m = floor(n/K)  #approximate size of each part
  r = n-m*K  
  I = sample(n,n)  #random reordering of the indices
  Ind = list()  #will be list of indices for all K parts
  length(Ind) = K
  for (k in 1:K) {
    if (k <= r) kpart = ((m+1)*(k-1)+1):((m+1)*k)  
    else kpart = ((m+1)*r+m*(k-r-1)+1):((m+1)*r+m*(k-r))
    Ind[[k]] = I[kpart]  #indices for kth part of data
  }
  return (Ind)
}
n=79;K=10
Ind = CVInd(n=n,K=K)
y = LAB3[[1]]
yhat = y
for (k in 1:K) {
  out = lm(y~.,data=LAB3[-Ind[[k]],])
  yhat[Ind[[k]]] = cbind(1,as.matrix(LAB3[Ind[[k]], 2:9]))%*% as.matrix(out$coef)
} 
CVSSE1 = sum((y-yhat)^2)  #CV SSE
#now compare to a different shrinkage parameter
yhat = y
for (k in 1:K) {
  out = lm(y~x1+x2,data=LAB3[-Ind[[k]],])
  yhat[Ind[[k]]] = cbind(1,as.matrix(LAB3[Ind[[k]], 2:3]))%*% as.matrix(out$coef) 
} 
CVSSE2 = sum((y-yhat)^2)  #CV SSE
c(CVSSE1,CVSSE2)
