# Question 1 ----------------------------------------------------------------------

library(quantmod)
getSymbols(Symbols = "JPM", src = "yahoo", from = "2010-01-01", to = "2015-12-31")

getSymbols(Symbols = "C", src = "google", from = "2010-01-01", to = "2015-12-31")

JPM.daily.return <- periodReturn(JPM, period = 'daily')
JPM.weekly.return <- periodReturn(JPM, period = 'weekly')
JPM.monthly.return <- periodReturn(JPM, period = 'monthly')
JPM.log.daily.return <- periodReturn(JPM, type = 'log', period = 'daily')
JPM.log.weekly.return <- periodReturn(JPM, type = 'log', period = 'weekly')
JPM.log.monthly.return <- periodReturn(JPM, type = 'log', period = 'monthly')

C.daily.return <- periodReturn(C, period = 'daily')
C.weekly.return <- periodReturn(C, period = 'weekly')
C.monthly.return <- periodReturn(C, period = 'monthly')
C.log.daily.return <- periodReturn(C, type = 'log', period = 'daily')
C.log.weekly.return <- periodReturn(C, type = 'log', period = 'weekly')
C.log.monthly.return <- periodReturn(C, type = 'log', period = 'monthly')
# All returns

chartSeries(JPM, TA = NULL)
addSMA()
addBBands()

# The two stock technical index used here are Simple Moving Average and Bollinger Bands 

# Simple Moving Average is the unweighted mean of the previous n days' closing price
#  used to identify a current trend

# The Simple Moving Average in day M is
# SMA = (P_M+P_{M-1}+P_{M-2}+...+P_{M-(n-1)})/n

# Bollinger Bands and the related indicators %b and bandwidth can be used to 
#  measure the "highness" or "lowness" of the price relative to previous trades. 

# Bollinger Bands consist of:
# an N-period moving average (MA)
# an upper band at K times an N-period standard deviation above the moving average (MA + Kσ)
# a lower band at K times an N-period standard deviation below the moving average (MA − Kσ)



# Question 2 ----------------------------------------------------------------------

library(fUnitRoots)
library(tseries)

# -----Stationary Test-----
adfTest(JPM.daily.return)

Title:
    Augmented Dickey-Fuller Test

Test Results:
    PARAMETER:
    Lag Order: 1
STATISTIC:
    Dickey-Fuller: -27.8664
P VALUE:
    0.01 

Description:
    Sat Oct 15 09:57:03 2016
# The P-value is 0.01, which is significant 
#  and the null hypothesis that at least one unit root is present would be rejected
#  which means the time series is stationary

adfTest(JPM.weekly.return)
adfTest(JPM.monthly.return)
adfTest(JPM.log.daily.return)
adfTest(JPM.log.weekly.return)
adfTest(JPM.log.monthly.return)
# In above 5 stationary tests
# The P-value is 0.01, which is significant
# The null hypothesis would be rejected
# These time series are stationary

adfTest(C.daily.return)
adfTest(C.weekly.return)
adfTest(C.monthly.return)
adfTest(C.log.daily.return)
adfTest(C.log.weekly.return)
adfTest(C.log.monthly.return)
# In above 6 stationary tests
# The P-value is 0.01, which is significant
# The null hypothesis would be rejected
# These time series are stationary

# -----Normality Test-----
jarque.bera.test(JPM.daily.return)

Jarque Bera Test

data:  JPM.daily.return
X-squared = 599.7, df = 2, p-value < 2.2e-16
# The p-value < 2.2e-16, which is significant
#  and the null hypothesis that the skewness being zero and the excess kurtosis being zero
#  would be rejected
# So we take alternative hypothesis that at least one condition is not satisfied
# The time series is not normal

jarque.bera.test(JPM.weekly.return)
# p-value = 4.266e-05, which is significant
# The null hypothesis should be rejected which means
#  the time series is not normal

jarque.bera.test(JPM.monthly.return)
# p-value = 0.06451 > 0.05, which is not significant
# We fail to reject the null hypothesis which means
#  the time series is normal

jarque.bera.test(JPM.log.daily.return)
jarque.bera.test(JPM.log.weekly.return)
jarque.bera.test(JPM.log.monthly.return)
# In above 3 normal test
# p-value < 0.05, which is significant
# The null hypothesis should be rejected which means
#  these time series are not normal

jarque.bera.test(C.daily.return)
jarque.bera.test(C.weekly.return)
jarque.bera.test(C.log.daily.return)
jarque.bera.test(C.log.weekly.return)
# In above 4 normal test
# p-value < 0.05, which is significant
# The null hypothesis should be rejected which means
#  these time series are not normal

jarque.bera.test(C.monthly.return)
# p-value = 0.9396
jarque.bera.test(C.log.monthly.return)
# p-value = 0.4392
# In above 2 normal test
# p-value > 0.05, which is not significant
# We fail to reject the null hypothesis which means
#  these time series are normal

#-----Skewness and Kurtosis-----
skewness(JPM.daily.return)
#[1] -0.04904404
kurtosis(JPM.daily.return)
#[1] 3.077726
# It is left-skewed with heavy tails

skewness(JPM.weekly.return)
#[1] -0.1696576
kurtosis(JPM.weekly.return)
#[1] 1.16775

skewness(JPM.monthly.return)
#[1] -0.5193808
kurtosis(JPM.monthly.return)
#[1] 0.7319893
# It is left-skewed with heavy tails

skewness(JPM.log.daily.return)
#[1] -0.1824038
kurtosis(JPM.log.daily.return)
#[1] 3.180239

skewness(JPM.log.weekly.return)
#[1] -0.3355528
kurtosis(JPM.log.weekly.return)
#[1] 1.235605

skewness(JPM.log.monthly.return)
#[1] -0.8210948
kurtosis(JPM.log.monthly.return)
#[1] 1.415136
# These dataset are all left-skewed with heavy tails


acf(JPM.daily.return)
auto.correlation <- acf(JPM.daily.return)$acf
auto.correlation[1:11]
# Auto-correlation for JPM daily return up to lag 10



# Question 3 ----------------------------------------------------------------------

dow30names <- c("MMM", "AXP", "AAPL", "BA", "CAT", "CVX", "CSCO", "KO", "DIS", "DD", 
               "XOM", "GE", "GS", "HD", "IBM", "INTC", "JNJ", "JPM", "MCD", "MRK", 
               "MSFT", "NKE","PFE", "PG", "TRV", "UTX","UNH", "VZ", "V", "WMT")
#https://finance.yahoo.com/quote/%5EDJI/components?p=%5EDJI

getstockreturn <- function(stock.name, start.date, end.date, prd) {
    stock <- getSymbols(stock.name, from = start.date, to = end.date, auto.assign = F)
    prdrt <- periodReturn(stock, period = prd)
    return(prdrt)
}

dowtable <- data.frame("Ticker" = NA, "Start date" = NA, "End date" = NA, 
                       "Mean for daily return" = NA, "Mean for weekly return" = NA)
n <- length(dow30names)

for (i in 1:length(dow30names)) {
    sd1 <- "2010-01-01"
    ed1 <- "2010-02-01"
    sd2 <- "2016-01-01"
    ed2 <- "2016-03-07"
    s1 <- getstockreturn(dow30names[i], sd1, ed1, "daily")
    s2 <- getstockreturn(dow30names[i], sd1, ed1, "weekly")
    m1 <- mean(s1)
    m2 <- mean(s2)
    newrow <- c(dow30names[i], sd1, ed1, m1, m2)
    dowtable <- rbind(dowtable, newrow)
    
    s1 <- getstockreturn(dow30names[i], sd2, ed2, "daily")
    s2 <- getstockreturn(dow30names[i], sd2, ed2, "weekly")
    m1 <- mean(s1)
    m2 <- mean(s2)
    newrow <- c(dow30names[i], sd2, ed2, m1, m2)
    dowtable <- rbind(dowtable, newrow)
}

dowtable <- dowtable[-1, ]
print(dowtable, row.names = F)

# Question 4 ----------------------------------------------------------------------

library("lubridate")

aplo <- read.csv("AAPL.O.csv")
aapl <- subset.data.frame(aplo, select = c(Date.L., Time.L., Price, Volume))

aapl$Date.L. <- ymd(aapl$Date.L.)
aapl$Time.L. <- hms(aapl$Time.L.)
Trading.Time <- aapl$Date.L. + aapl$Time.L.
aapl <- data.frame(Trading.Time, aapl)
#Add trading time

Time.Diff <- difftime(aapl$Trading.Time[-1], 
                      aapl$Trading.Time[-length(aapl$Trading.Time)], units = "secs")
Time.Diff <- as.numeric(Time.Diff)
aapl$Time.Diff <- c(Time.Diff, NA)

hist(Time.Diff, nclass = 100)

jarque.bera.test(Time.Diff)
# p-value < 2.2e-16 which is significant
#  and the null hypothesis that the skewness being zero and the excess kurtosis being zero
#  would be rejected
# So we take alternative hypothesis that at least one condition is not satisfied
# The time difference is not normal

timecut <- as.POSIXct(c("2005-10-27 09:30:00", "2005-10-27 11:30:00", 
                          "2005-10-27 14:00:00", "2005-10-27 16:00:00"), tz = "gmt")
aapl$Time.Section <- cut(aapl$Trading.Time, timecut, 
                         labels = c("9:30-11:30", "11:30-14:00", "14:00-16:00"))
# Split data into three section

Tradeprice1 <- aapl$Price[aapl$Time.Section == "9:30-11:30"]
Tradeprice2 <- aapl$Price[aapl$Time.Section == "11:30-14:00"]
Tradeprice3 <- aapl$Price[aapl$Time.Section == "14:00-16:00"]
Tradeprice1 <- as.numeric(na.omit(Tradeprice1))
Tradeprice2 <- as.numeric(na.omit(Tradeprice2))
Tradeprice3 <- as.numeric(na.omit(Tradeprice3))

jarque.bera.test(Tradeprice1)
jarque.bera.test(Tradeprice2)
jarque.bera.test(Tradeprice3)
# In above 3 normal test
# p-value < 0.05, which is significant
# The null hypothesis should be rejected which means
#  these dataset are not normal

TV1 <- aapl$Volume[aapl$Time.Section == "9:30-11:30"]
TV1 <- as.numeric(na.omit(TV1))
hist(TV1, nclass = 50, freq = F)
TV2 <- aapl$Volume[aapl$Time.Section == "11:30-14:00"]
TV2 <- as.numeric(na.omit(TV2))
hist(TV2, nclass = 50, freq = F)
TV3 <- aapl$Volume[aapl$Time.Section == "14:00-16:00"]
TV3 <- as.numeric(na.omit(TV3))
hist(TV3, nclass = 50, freq = F)
# Histogram for trade volume

hist(TV1[TV1<1000], freq = F)
hist(TV1[TV1<1000], freq = F)
hist(TV1[TV1<1000], freq = F)
# Histogram for trade volume less than 1000

aaplvl <- subset.data.frame(aapl, select =  c(Volume, Time.Section))
aaplvl <- na.omit(aaplvl)
VO1 <- as.numeric(aaplvl$Volume[aaplvl$Time.Section == "9:30-11:30"])
VO2 <- as.numeric(aaplvl$Volume[aaplvl$Time.Section == "11:30-14:00"])
VO3 <- as.numeric(aaplvl$Volume[aaplvl$Time.Section == "14:00-16:00"])
# Omit null volume

prop_nt <- function(x) {
    n <- length(x)
    c <- 0
    for (i in 1:n) {
        if (x[i] %% 100 == 0) c <- c + 1 
    }
    ans <- c/n
    return(ans)
}
Proportion1 <- prop_nt(VO1)
# 0.93860609 in 9:30-11:30
Proportion2 <- prop_nt(VO2)
# 0.93080065 in 11:30-14:00
Proportion3 <- prop_nt(VO3)
# 0.92787228 in 14:00-16:00
# Proportion of normal trade in different time section
# ------------------------------------------------------------------------
# Liting Hu