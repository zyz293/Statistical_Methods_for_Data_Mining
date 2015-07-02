setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/assignment/2')
data = read.csv('HW2_data1.csv', header = T)
Y = matrix(c(data$y), ncol = 1) # Q1
X = data[,1:3]
X[,1] = 1
X = matrix(unlist(X), nrow = 8, ncol = 3)
XX = crossprod(X,X)
XXi = solve(XX)
XY = crossprod(X,Y)
beta = XXi %*% t(X) %*% Y
e = Y - X %*% beta

beta2 = matrix(c(5.62, 0.82, -1.327), ncol = 1) # Q2
x = matrix(c(1, 1, 2), nrow = 1)
fitvalue = x %*% beta2


setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/assignment/2')
data = read.csv('HW2_data3.csv', header = T)  #Q3
data$Year = data$Year-1920
fitm = lm(data$Male~data$Year+I(data$Year^2),data)
summary(fitm)
fitf = lm(data$Female~data$Year+I(data$Year^2),data)
summary(fitf)

setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/assignment/2')
data = read.csv('HW2_data4.csv', header = T)  #Q4
fit = lm(data$y~data$x1+data$x2,data)
summary(fit)
confint(fit, level=0.95)

fit2 = lm(data$y~data$x1+data$x2+I(data$x1^2)+I(data$x2^2)+I(data$x1*data$x2),data) #Q5
summary(fit2)
anova(fit2)
fitreduce = lm(data$y~I(data$x1^2)+I(data$x2^2)+I(data$x1*data$x2),data)
summary(fitreduce)
anova(fitreduce)
anova(fitreduce, fit2)
qf(0.95,3,34)

setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/assignment/2')
data = read.csv('HW2_data6.csv', header = T)  #Q6
fit = lm(data$y~., data)
summary(fit)
data = data[,1:12]
fit = lm(data$y~., data)
summary(fit)
data = data[,1:11]
fit = lm(data$y~., data)
summary(fit)
data = data[,1:10]
fit = lm(data$y~., data)
summary(fit)
