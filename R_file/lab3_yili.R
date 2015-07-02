setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/lab/3')
lab3<-read.csv("Lab3.csv")   
lab3_2<-cbind(lab3$y,lab3$x1,lab3$x2)
lab3_2<-data.frame(lab3_2)
x<-1:79

SSE2<-rep(0,20)
SSE8<-rep(0,20)
for (j in 1:20)
{
  list<-sample(x,79)
  for (i in 1:10)
  {     if (i==1)
  {train=lab3[list[9:79],]
   test=lab3[list[1:8],]    
   
   train2=lab3_2[list[9:79],]
   test2=lab3_2[list[1:8],]
  }
  else if (i==10)
  {train=lab3[list[1:72],]
   test=lab3[list[73:79],]      
   
   train2=lab3_2[list[1:72],]
   test2=lab3_2[list[73:79],]
  }
  else
  {train=rbind(lab3[list[1:(8*(i-1))],],lab3[list[(8*i+1):79],])
   test=lab3[list[(8*(i-1)+1):(8*i)],]    
   
   train2=rbind(lab3_2[list[1:(8*(i-1))],],lab3_2[list[(8*i+1):79],])
   test2=lab3_2[list[(8*(i-1)+1):(8*i)],]
  }
  lm8<-lm(y~.,data=train)      
  ypredict8<-predict(lm8,newdata=test)     
  e8=sum((test$y-ypredict8)^2)      
  SSE8[j]=SSE8[j]+e8
  
  lm2<-lm(X1~.,data=train2)
  ypredict2<-predict(lm2,newdata=test2)
  e2=sum((test2$X1-ypredict2)^2)
  SSE2[j]=SSE2[j]+e2
  }
}
SSE8
SSE2
