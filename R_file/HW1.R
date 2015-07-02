setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/assignment/1')
data = read.csv("HW1_mileage_data.csv")
plot(data$x1,data$y)  #Q2
plot(data$x2,data$y)
plot(data$x3,data$y)
plot(data$x4,data$y)
plot(data$x5,data$y)
plot(data$x6,data$y)
plot(data$x7,data$y)
plot(data$x8,data$y)
plot(data$x9,data$y)
plot(data$x10,data$y)
plot(data$x11,data$y)

boxplot(data$y ~ data$x11)  #Q3

summary(data$y[data$x11==1])  #Q4
summary(data$y[data$x11==0])
tapply(data$y,data$x11,sd)
tapply(data$y,data$x11,quantile)
t.test(data$y[data$x11==1])
t.test(data$y[data$x11==0])
hist(data$y[data$x11==1])
hist(data$y[data$x11==0])

t.test(c(data$y[data$x11==0]),c(data$y[data$x11==1]))#Q5

pairs(data$y~.,data)  # Q6

plot(data$x1, data$y)  #Q7
abline(lm(data$y~data$x1,data=data),col="red")
fit = lm(y~x1,data=data)
summary(fit)
anova(fit)

