setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/assignment/3')
data2 = read.csv('HW3_data2.csv', header = T)
data3 = read.csv('HW3_data3.csv', header = T)
data4 = read.csv('HW3_data4.csv', header = T)

plot(data2$Gravity, data2$Moisture) #Q2
lm1 = lm(Strength~., data=data2)
inf1 = influence(lm1)
inf1$hat
X = cbind(1, as.matrix(data2[,1:2]))
H = X %*% solve(t(X) %*% X) %*% t(X)
H
diag(H)
cooks.distance(lm1)
newdata = data2[-4,]
rlm1 = lm(Strength~., data = newdata)
summary(lm1)
summary(rlm1)
inf1$coefficients

lm2 = lm(y~., data=data3) #Q3
yhat = predict(lm2, data=data3)
e = data3$y - yhat
lm3 = lm(y~x1+x2+I(x1^2)+I(x2^2)+I(x1*x2), data=data3)
yhat2 = predict(lm3, data=data3)
e2 = data3$y - yhat2 
estar = rstandard(lm3)
plot(data3$x1, e)
plot(data3$x2, e)
plot(data3$x1, e2)
plot(data3$x2, e2)
plot(yhat2, estar)

Ldata4 = log(data4)  #Q4
names(Ldata4) = c("LMercury", "LAlkalin", "Lcalcium", "LpH")
lm4 = lm(LMercury~LAlkalin+Lcalcium, data=Ldata4)
yhat4 = predict(lm4, data=Ldata4)
e4 = Ldata4$LMercury - yhat4
plot(Ldata4$LAlkalin, e4)
plot(Ldata4$Lcalcium, e4)
plot(yhat4, e4)
plot(data4$pH, e4)

lm5 = lm(LMercury~LAlkalin+Lcalcium+LpH, data=Ldata4) #Q5
summary(lm5)
confint(lm5, level=0.95)
anova(lm4, lm5)
