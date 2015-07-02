setwd('C:/Users/DaTui/Desktop/winter_quarter/IEME304/lab/3')
data = read.csv("Lab3.csv")
list = list()  #Q1
index = sample(79, 79)
for (i in 1:9) {
  list[[i]] = index[(8*i-7):(8*i)]
}
list[[10]] = index[73:79]
SSEF = c(0)
for (j in 1:10) {  # 8-predcitors model
  lmf = lm(y~., data=data[-list[[j]], ])
  #yhatf1 = predict(lmf, data[list[[j]], 2:9])  # they are all the same command
  #yhatf2 = predict(lmf, data[list[[j]], ])  # do not code "data = ..." when using predict command
  newdata = as.matrix(cbind(1, data[list[[j]], 2:9]))
  yhatf = newdata %*% as.matrix(lmf$coef)
  SSEF[j] = sum((as.vector(t(yhatf)) - data[list[[j]], ]$y)^2)
}
SSEF
SSER= c(0)
for (j in 1:10) {  # 2-predcitors model
  lmr = lm(y~x1+x2, data=data[-list[[j]], ])
  newdata = as.matrix(cbind(1, data[list[[j]], 2:3]))
  yhatr = newdata %*% as.matrix(lmr$coef)
  #yhatr = predict(lmr, data=data[list[[j]], ])
  SSER[j] = sum((as.vector(t(yhatr)) - data[list[[j]], ]$y)^2)
}
SSER

SSEF_CV = sum(SSEF) #Q2
SSEF_CV
SSER_CV = sum(SSER)
SSER_CV

SSEF_CV = c(0)  #Q3
for (k in 1:20) {
  listF = list()
  indexF = sample(79, 79)
  SSEF = c(0)
  for (i in 1:9) {
    listF[[i]] = indexF[(8*i-7):(8*i)]
  }
  listF[[10]] = indexF[73:79]
  for (j in 1:10) {  # 8-predcitors model
    lmf = lm(y~., data=data[-listF[[j]], ])
    newdata = as.matrix(cbind(1, data[listF[[j]], 2:9]))
    yhatf = newdata %*% as.matrix(lmf$coef)
    SSEF[j] = sum((as.vector(t(yhatf)) - data[listF[[j]], ]$y)^2)
  }
  SSEF_CV[k] = sum(SSEF) 
}
SSEF_CV

SSER_CV = c(0)
for (k in 1:20) {
  listR = list()
  indexR = sample(79, 79)
  SSER = c(0)
  for (i in 1:9) {
    listR[[i]] = indexR[(8*i-7):(8*i)]
  }
  listR[[10]] = indexR[73:79]
  for (j in 1:10) {  # 2-predcitors model
    lmr = lm(y~x1+x2, data=data[-listR[[j]], ])
    newdata = as.matrix(cbind(1, data[listR[[j]], 2:3]))
    yhatr = newdata %*% as.matrix(lmr$coef)
    SSER[j] = sum((as.vector(t(yhatr)) - data[listR[[j]], ]$y)^2)
  }
  SSER_CV[k] = sum(SSER) 
}
SSER_CV

