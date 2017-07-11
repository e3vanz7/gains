# Gains

gains <- read.csv("gainz.csv")

library(ggplot2)
gainsplot <- ggplot(gains, aes(x = Days.Elapsed, y = Weight))
gainsplot <- gainsplot + geom_line(colour = "red", size = 1) + geom_smooth(method = "auto", se = F, colour = "green") + geom_smooth(method = "lm", se = F)
gainsplot <- gainsplot + geom_hline(yintercept = min(gains$Weight)) + geom_hline(yintercept = max(gains$Weight)) + geom_hline(yintercept = mean(gains$Weight))
gainsplot <- gainsplot + xlab("Days Elapsed Since May 15th, 2015") + ylab("Weight (lbs)")
gainsplot <- gainsplot + ggtitle("Change of Weight Since May 15th, 2015")
gainsplot <- gainsplot + scale_x_continuous(breaks = round(seq(min(gains$Days.Elapsed), max(gains$Days.Elapsed), by = 10),1))

gainsplot


# Messing with detrending/differencing
gains.ts <- ts(gains$Weight)
plot(gains.ts)
acf(gains.ts)
diff.gains.ts <- ts(diff(gains.ts))
acf(diff.gains.ts, main = "ACF of Differenced Gains")
pacf(diff.gains.ts, main = "PACF of Differenced Gains")
plot(diff.gains.ts, main = "Differenced Gains") # what my weight looks like with the trend removed. interesting to see the fluctuations
abline(h = mean(diff.gains.ts))
# fitting a linear trend
gainsfit <- lm(gains$Weight ~ gains$Days.Elapsed)
summary(gainsfit)
qqnorm(gainsfit$residuals)
qqline(gainsfit$residuals)
acf(gainsfit$residuals)
plot(gainsfit$fitted.values, gainsfit$residuals, main = "Residuals vs Fitted Values")

# prediction (based on differenced data): 
# s = 7 b/c it's weekly
# d = 1, d = 1
# PACF lags at 2,3,5
# ACF lags at 1


sarima.for(gains.ts, nahead = 10, 3,1,0,3,1,0,7) # needs work - this is really hard because it was really poorly taught

#######################################
# Some basic stats
totalGain <- max(gains$Weight) - min(gains$Weight)
totalDays <- max(gains$Days.Elapsed)
meanPoundsPerDay <- totalGain / totalDays # gained on average 0.05601751 lbs per day


# Forecasting using the "forecast" library
library(forecast)
testfit <- auto.arima(gains.ts)
plot(forecast(testfit, h = 50), shadecols="oldstyle", xlab = "Weighing Days since May 15th, 2015", ylab = "Weight (lbs)", main = "Time Series of Weight with ARIMA Forecast")

# h var is num periods. default is 2 * frequency of ts if frequency is > 1, or 10 otherwise (freq for gains is 1 so default is indeed 10)
# This is cool. Definitely going to play around with this a lot more
# want to try forecasting with non-ts data in order to capture the days in which I didn't way myself







