library(quantmod)
getSymbols('GDPC1',src='FRED')
Yl <- GDPC1
Yl.ts <- ts(Y, frequency = 4, start=c(1947, 01))
Y.ts <- diff(log(Yl.ts))*100
Y.ts <- window(Y.ts, start=c(1948, 01))

getSymbols('UNRATE',src='FRED')
u <- UNRATE
um.ts <- ts(u, frequency =12, start=c(1948, 01, 01))
u.ts <- aggregate(um.ts, nf = 4, FUN = mean)

Yhat <- as.vector(Y.ts)
u <- as.vector(u.ts)
macro <- data.frame(Yhat, u)
head(macro)

par(mar = c(5, 5, 4, 2))
plot(Yhat, u, main=expression("Red line: " * u == beta[0] + beta[1] * hat(Y)),
     sub="Data: FRED",
     ylab=expression(u *" (%)"), xlab=expression(hat(Y) *" (%)") )
abline(lm(u ~ Yhat), col="red")
mtext(bquote(beta[1]==.(coefficients(lm(u ~ Yhat))[2])))

plot(okun$x,okun$y, sub="Source: FRED. Calculations: JH", mtext(bquote(R^2 == .(cor(okun$x, okun$y)^2) ~ " Fitted line: " ~ u == .(reg$coefficients[1])~ .(reg$coefficients[2]) * hat(Y) )), main = expression(paste("US unemployment rate ", u, " vs. real GDP growth ", hat(Y), ": 1948q1-2018q3")), ylab=expression(u), xlab = expression(hat(Y)))
lines(okun$x, reg$fit, lty=2)

coefficients(lm(u ~ Yhat))
plot(Yhat, u, pch=16, 
     sub="Source: FRED, 1948Q1-2020Q1. Calculations: Julio Huato", 
     mtext(expression(paste("Fitted line: ", u==5.82 - 0.12 ~ hat(Y)))), 
     main = expression(paste("US unemployment rate ", u, " vs. real GDP growth ", hat(Y))), ylab=expression(u), xlab = expression(hat(Y)))
abline(lm(u ~ Yhat), lty=1, col="red")
