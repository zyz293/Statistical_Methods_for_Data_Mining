setwd('C:/Users/DaTui/Desktop')
data = read.csv('ENB2012_data.csv', header = T)
data = data[1:36, 1:9]
lm = lm(Y1~., data=data)
summary(lm)
