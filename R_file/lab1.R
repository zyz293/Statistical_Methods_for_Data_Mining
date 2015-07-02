setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/lab')
data = read.csv("employee.csv")
average_age = mean(data$ageyrs)
f_age = sum(data$ageyrs[data$female==1])
f_count = length(data$ageyrs[data$female==1])
f_average_age = f_age / f_count
m_age = sum(data$ageyrs[data$female==0])
m_count = length(data$ageyrs[data$female==0])
m_average_age = m_age / m_count
quantile(data$ageyrs)

expyrs = data$expyrs
expyrs_sd = sd(expyrs)

data["SalaryX"] = NA
salary = data$salary
SalaryX = floor(salary/10000)*10000
crosstab = table(data$trainlev,data$SalaryX)
round(prop.table(crosstab),4)
round(prop.table(crosstab,1),4)
round(prop.table(crosstab,2),4)

bin = cut(data$salary,breaks = seq(20000,70000,by=10000))
binn = table(data$trainlev,bin)
round(prop.table(binn),4)
round(prop.table(binn,1),4)
round(prop.table(binn,2),4)
