setwd("~/Downloads")
getwd()

data = read.csv("data.csv", header = T)
y <- data[length(data)]
data <- data[-c(1:3, length(data))]
names(data) <- NULL
data

require(smooth)
library(forecast)    

ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
for(x in 1:nrow(data)){
  f <- sma(t(data[x, ]), n=3, silent=FALSE)
  yp <- forecast(f, 1)
  ypred[nrow(ypred)+1,] <- (round(yp[["forecast"]][1]))
}

mean(abs((y$May.21-ypred$P)/y$May.21))
#MAPE is 5.948601

library(forecast)

ya = data.frame(matrix(ncol=1, nrow=0))
colnames(ya) <- c('A')
yactual = data.frame(matrix(ncol=1, nrow=0))
colnames(yactual) <- c('A')
ypred <- data.frame(matrix(ncol=1, nrow=0))
colnames(ypred) <- c('P')
re <- data.frame(matrix(ncol=1, nrow=0))
colnames(re) <- c('S')

for(x in 1:nrow(data)){
  if(length(unique(t(data[x, ]))) == 1)
    ya[nrow(ya)+1,] <- x
  else{
    yo  = auto.arima(t(data[x, ]), approximation = FALSE, allowdrift = TRUE, allowmean = TRUE)
    arma = yo$arma
    s <- arima(t(data[x, ]), order = c(arma[1], arma[6], arma[2]), seasonal = list(order=c(arma[3], arma[7], arma[4]), period=arma[5]), method="ML")
    f <- forecast(s, h = 1)
    ypred[nrow(ypred)+1,] <- (round((f[["mean"]][1])))
    yactual[nrow(yactual)+1,] <- y[x, ]
  }
  re[nrow(re)+1,] <- x
}

mean(abs((yactual$A-ypred$P)/yactual$A))
#MAPE is 6.070073

#all the rows which have equal values till April 2021
# ya
# A
# 1   172
# 2   176
# 3   403
# 4   410
# 5   419
# 6   535
# 7   570
# 8   665 
# 9   668 - May is different
# 10  686 - May is different
# 11  704
# 12  707
# 13  708
# 14  977
# 15 1016 - May is different