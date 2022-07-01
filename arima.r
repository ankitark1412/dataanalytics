setwd("~/Downloads")
getwd()

data = read.csv("data.csv", header = T)
#data

north = data[data$Region == "NORTH", ]
y <- north[length(north)]
north <- north[-c(1:3, length(north))]
names(north) <- NULL

library(remotes)
install_github("cran/seasonal")
install_github("cran/tseries")
install_github("cran/forecast")

library(forecast)

north <- data.frame(t(north))

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('Prediction')
#ypred

for(x in 1:length(north)){
  nm <- auto.arima(north[, x:x], approximation = FALSE, allowdrift = TRUE, allowmean = TRUE)  
  f <- forecast(nm, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$Prediction)/y$May.21))
#for the north region, the MAPE comes out to be 10.36916 when auto ARIMA is applied row wise

east = data[data$Region == "EAST", ]
y <- east[length(east)]
east <- east[-c(1:3, length(east))]
names(east) <- NULL

east <- data.frame(t(east))

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('Prediction')

for(x in 1:length(east)){
  em <- auto.arima(east[, x:x], approximation = FALSE, allowdrift = TRUE, allowmean = TRUE)  
  f <- forecast(em, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$Prediction)/y$May.21))
#for the east region, the MAPE comes out to be 6.759858 when auto ARIMA is applied row wise

south = data[data$Region == "SOUTH", ]
y <- south[length(south)]
south <- south[-c(1:3, length(south))]
names(south) <- NULL

south <- data.frame(t(south))

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('Prediction')

for(x in 1:length(south)){
  sm <- auto.arima(south[, x:x], approximation = FALSE, allowdrift = TRUE, allowmean = TRUE)  
  f <- forecast(sm, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$Prediction)/y$May.21))
#for the south region, the MAPE comes out to be 6.596561 when auto ARIMA is applied row wise

west = data[data$Region == "WEST", ]
y <- west[length(west)]
west <- west[-c(1:3, length(west))]
names(west) <- NULL

west <- data.frame(t(west))

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('Prediction')

for(x in 1:length(west)){
  wm <- auto.arima(west[, x:x], approximation = FALSE, allowdrift = TRUE, allowmean = TRUE)  
  f <- forecast(wm, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$Prediction)/y$May.21))
#for the west region, the MAPE comes out to be 1.401087 when auto ARIMA is applied row wise

#to find the values of p,d,q manually and apply ARIMA
east <- data.frame(t(east))
north <- data.frame(t(north))
west <- data.frame(t(west))
south <- data.frame(t(south))

sumN <- colSums(north)/length(north)
sumE <- colSums(east)/length(east)
sumW <- colSums(west)/length(west)
sumS <- colSums(south)/length(south)

#NORTH region
north = data[data$Region == "NORTH", ]
y <- north[length(north)]
north <- north[-c(1:3, length(north))]
names(north) <- NULL
plot.new()
lines(1:37, sumN, type="l")
library(tseries)
adf.test(sumN)
#data:  sumN
#Dickey-Fuller = -3.4667, Lag order = 3, p-value = 0.06304
#alternative hypothesis: stationary

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumN, order=c(2, 1, 1))
checkresiduals(fit1)
for (i in 1:nrow(north)){
  n.test <- Arima(t(north[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 5.638783 for the north region with ARIMA(2, 1, 1)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumN, order=c(2, 1, 2))
checkresiduals(fit1)
for (i in 1:nrow(north)){
  n.test <- Arima(t(north[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#6.155996

Acf(sumN)
Pacf(sumN)
#2 major peaks 

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumN, order=c(1, 0, 1))
sumN
checkresiduals(fit1)
for (i in 1:nrow(north)){
  n.test <- Arima(t(north[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 130.8975 for the north region with ARIMA(1, 0, 1)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumN, order=c(2, 0, 1))
sumN
checkresiduals(fit1)
for (i in 1:nrow(north)){
  n.test <- Arima(t(north[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 122.9146 for the north region with ARIMA(2, 0, 1)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumN, order=c(1, 0, 2))
sumN
checkresiduals(fit1)
for (i in 1:nrow(north)){
  n.test <- Arima(t(north[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 126.5409 for the north region with ARIMA(1, 0, 2)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumN, order=c(1, 0, 3))
sumN
checkresiduals(fit1)
for (i in 1:nrow(north)){
  n.test <- Arima(t(north[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 107.2903 for the north region with ARIMA(1, 0, 3)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumN, order=c(1, 0, 4))
sumN
checkresiduals(fit1)
for (i in 1:nrow(north)){
  n.test <- Arima(t(north[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 714.2025 for the north region with ARIMA(1, 0, 4)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumN, order=c(2, 0, 3))
sumN
checkresiduals(fit1)
for (i in 1:nrow(north)){
  n.test <- Arima(t(north[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 109.9741 for the north region with ARIMA(2, 0, 3)

#auto arima with average
ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('Prediction')
#ypred

for(x in 1:nrow(north)){
  nm <- auto.arima(t(north[x, ]), approximation = FALSE, allowdrift = TRUE, allowmean = TRUE)  
  f <- forecast(nm, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$Prediction)/y$May.21))
#MAPE for auto arima is 10.36916

east = data[data$Region == "EAST", ]
y <- east[length(east)]
east <- east[-c(1:3, length(east))]
names(east) <- NULL
plot.new()
lines(1:37, sumE, type="l")
library(tseries)
adf.test(sumE)
#stationary

east
Acf(sumE)
Pacf(sumE)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumE, order=c(3, 0, 1))
checkresiduals(fit1)
for (i in 1:nrow(east)){
  n.test <- Arima(t(east[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 65.10842 for the east region with ARIMA(3, 0, 1)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumE, order=c(1, 0, 1))
checkresiduals(fit1)
for (i in 1:nrow(east)){
  n.test <- Arima(t(east[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 87.48919 for the east region with ARIMA(1, 0, 1)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumE, order=c(2, 0, 1))
checkresiduals(fit1)
for (i in 1:nrow(east)){
  n.test <- Arima(t(east[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 72.03942 for the east region with ARIMA(2, 0, 1)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumE, order=c(1, 0, 2))
checkresiduals(fit1)
for (i in 1:nrow(east)){
  n.test <- Arima(t(east[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 81.28945 for the east region with ARIMA(1, 0, 2)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumE, order=c(1, 0, 3))
checkresiduals(fit1)
for (i in 1:nrow(east)){
  n.test <- Arima(t(east[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 62.36335 for the east region with ARIMA(1, 0, 3)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumE, order=c(1, 1, 1))
checkresiduals(fit1)
for (i in 1:nrow(east)){
  n.test <- Arima(t(east[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 5.699523 for the east region with ARIMA(1, 1, 1)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumE, order=c(1, 2, 1))
checkresiduals(fit1)
for (i in 1:nrow(east)){
  n.test <- Arima(t(east[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 6.404774 for the east region with ARIMA(1, 2, 1)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumE, order=c(1, 3, 1))
checkresiduals(fit1)
for (i in 1:nrow(east)){
  n.test <- Arima(t(east[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 7.264471 for the east region with ARIMA(1, 3, 1)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumE, order=c(2, 1, 1))
checkresiduals(fit1)
for (i in 1:nrow(east)){
  n.test <- Arima(t(east[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 6.713674 for the east region with ARIMA(2, 1, 1)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumE, order=c(1, 1, 2))
checkresiduals(fit1)
for (i in 1:nrow(east)){
  n.test <- Arima(t(east[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 5.82058 for the east region with ARIMA(1, 1, 2)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumE, order=c(2, 1, 2))
checkresiduals(fit1)
for (i in 1:nrow(east)){
  n.test <- Arima(t(east[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 5.612667 for the east region with ARIMA(2, 1, 2)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumE, order=c(2, 2, 2))
checkresiduals(fit1)
for (i in 1:nrow(east)){
  n.test <- Arima(t(east[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 6.519982 for the north region with ARIMA(2, 2, 2)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumE, order=c(3, 2, 1))
checkresiduals(fit1)
for (i in 1:nrow(east)){
  n.test <- Arima(t(east[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 6.508848 for the north region with ARIMA(3, 2, 1)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumE, order=c(3, 2, 3))
checkresiduals(fit1)
for (i in 1:nrow(east)){
  n.test <- Arima(t(east[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 6.836041 for the north region with ARIMA(3, 2, 3)

#trying auto arima for the average of east

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('Prediction')
#ypred

for(x in 1:nrow(east)){
  nm <- auto.arima(t(east[x, ]), approximation = FALSE, allowdrift = TRUE, allowmean = TRUE)  
  f <- forecast(nm, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$Prediction)/y$May.21))
#MAPE for auto arima is 6.759858

#for the south region
south = data[data$Region == "SOUTH", ]
y <- south[length(south)]
south <- south[-c(1:3, length(south))]
names(south) <- NULL
plot.new()
lines(1:37, sumS, type="l")
library(tseries)
adf.test(sumS)

Acf(sumS)
Pacf(sumS)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('Prediction')
#ypred

for(x in 1:nrow(south)){
  nm <- auto.arima(t(south[x, ]), approximation = FALSE, allowdrift = TRUE, allowmean = TRUE)  
  f <- forecast(nm, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$Prediction)/y$May.21))
#auto arima MAPE 6.596561

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumS, order=c(3, 2, 1))
checkresiduals(fit1)
for (i in 1:nrow(south)){
  n.test <- Arima(t(south[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE 5.03103

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumS, order=c(1, 1, 1))
checkresiduals(fit1)
for (i in 1:nrow(south)){
  n.test <- Arima(t(south[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE 5.664939

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumS, order=c(1, 2, 1))
checkresiduals(fit1)
for (i in 1:nrow(south)){
  n.test <- Arima(t(south[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE 5.179874

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumS, order=c(1, 3, 1))
checkresiduals(fit1)
for (i in 1:nrow(south)){
  n.test <- Arima(t(south[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE 4.910218

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumS, order=c(2, 3, 1))
checkresiduals(fit1)
for (i in 1:nrow(south)){
  n.test <- Arima(t(south[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE 6.504571

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumS, order=c(1, 2, 2))
checkresiduals(fit1)
for (i in 1:nrow(south)){
  n.test <- Arima(t(south[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE 5.317467

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumS, order=c(1, 1, 1))
checkresiduals(fit1)
for (i in 1:nrow(south)){
  n.test <- Arima(t(south[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE 5.644939

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumS, order=c(2, 3, 2))
checkresiduals(fit1)
for (i in 1:nrow(south)){
  n.test <- Arima(t(south[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE 5.341541

#west region
west = data[data$Region == "WEST", ]
y <- west[length(west)]
west <- west[-c(1:3, length(west))]
names(west) <- NULL
plot.new()
lines(1:37, sumS, type="l")
library(tseries)
adf.test(sumW)

Acf(sumW)
Pacf(sumW)

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumW, order=c(1, 1, 1))
checkresiduals(fit1)
for (i in 1:nrow(west)){
  n.test <- Arima(t(west[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE 1.364116

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumW, order=c(1, 2, 1))
checkresiduals(fit1)
for (i in 1:nrow(west)){
  n.test <- Arima(t(west[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE 1.34347

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumW, order=c(1, 3, 1))
checkresiduals(fit1)
for (i in 1:nrow(west)){
  n.test <- Arima(t(west[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE 1.49283

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumW, order=c(2, 1, 1))
checkresiduals(fit1)
for (i in 1:nrow(west)){
  n.test <- Arima(t(west[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE 3.429056

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumW, order=c(1, 1, 2))
checkresiduals(fit1)
for (i in 1:nrow(west)){
  n.test <- Arima(t(west[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE 1.502744

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumW, order=c(1, 2, 2))
checkresiduals(fit1)
for (i in 1:nrow(west)){
  n.test <- Arima(t(west[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE 1.341391

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
fit1 <- Arima(sumW, order=c(1, 3, 2))
checkresiduals(fit1)
for (i in 1:nrow(west)){
  n.test <- Arima(t(west[i,]), model=fit1)
  f <- forecast(n.test, h = 1)
  ypred[nrow(ypred)+1,] <- (round(f[["mean"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE 1.326514