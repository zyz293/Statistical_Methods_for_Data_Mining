setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/R_Example_Data')

plot(GAS$Weight, GAS$Mpg)
abline(lm(Mpg~Weight, data=GAS), col="red") #plot of simple lin. regression of Mpg on Weight
lines(lowess(GAS$Weight, GAS$Mpg), col="blue") #scatterplot smoother 
