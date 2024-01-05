library(quantmod)
library(TTR)
getSymbols("GOOG", from = "2020-5-7", to = "2021-5-7", 
           src = "yahoo")
View(GOOG)

goog.close = Cl(GOOG)
plot(goog.close)


# SIMPLE MOVING AVERAGE
sma20 = SMA(goog.close, 20)
sma20
sma50 = SMA(goog.close, 50)
sma50

# Plot SMA
lines(sma20, lty = 1, col = "blue", lwd = 2)
lines(sma50, lty = 1, col = "red", lwd = 2)
legend("bottomright", legend = c("GOOG", "SMA20", "SMA50"),
       lty = 1, col = c("black", "blue", "red"),
       bty = "n", cex = 0.8)

# WEIGHTED MOVING AVERAGE
wma20 = WMA (goog.close, 20, wts = 1:20)
wma20
wma50 = WMA (goog.close, 50, wts = 1:50)

# Plot WMA
par(mfrow = c(1,2))
plot(goog.close)
lines(wma20, lty = 1, col = "blue", lwd = 2)
lines(wma50, lty = 1, col = "red", lwd = 2)

# Plot dengan chartSeries (AUTO)
chartSeries(GOOG, type = "candlestick", theme = chartTheme(theme = "white", up.col='blue',dn.col='white'), 
            TA = "addBBands()")
chartSeries(GOOG, TA="addBBands(n=20)")

