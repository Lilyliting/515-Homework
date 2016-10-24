#Question 1:  Send a monopoly to jail

Double.Count <- 0
Toss.Count <- 0
n <- 10000
Dicepoints <- seq(1:6)
Results <- NULL

for (i in 1:n) {
    Double.Count <- 0
    Toss.Count <- 0
    while (Double.Count < 3) {
        A <- sample(Dicepoints, 2, replace = T)
        if (A[1] == A[2]) Double.Count <- Double.Count + 1
        else Double.Count <- 0
        Toss.Count <- Toss.Count + 1
        }
    Results <- c(Results, Toss.Count)
}
print(mean(Results))
#About 257 tosses are needed on average to get three consecutive doubles.

#-------------------------------------------------
#Question 2: Checking Distributions

#Normal distribution
x <- rnorm(n = 10000, mean = 0, sd = 1)
hist(x, freq = FALSE, nclass = 40)
xfit <- seq(from = min(x), to = max(x), by = 0.1)
lines(xfit, dnorm(xfit, mean = 0, sd = 1), col = "blue")
lines(xfit, dnorm(xfit, mean = mean(x)), col = "red")
#There is a slight difference between these two lines 
# due to the difference between the sample mean and population mean.
#As sample size growing larger, the variance of the sample mean is getting smaller (variance/N)
# so that these two lines almost coincide as experiment times are large enough.


#Log-normal distribution
y <- exp(x)
hist(y, freq = FALSE, nclass = 80)

y2 <- rlnorm(10000)
hist(y2, freq = FALSE, nclass = 80)
yfit <- seq(from = min(y2), to = max(y2), by = 0.1)
lines(yfit, dlnorm(yfit), col = "red")
                 
#Gamma distribution
g <- rgamma(10000, shape = 2)
hist(g, freq = FALSE, nclass = 80)
gfit <- seq(from = min(g), to = max(g), by = 0.1)
lines(gfit, dgamma(gfit, shape = 2), col = "red")

#-------------------------------------------------
#Question 3: Returns

library(zoo)
library(xts)
library(TTR)
library(quantmod)
setwd("/Users/apple/Desktop/515/R files for 515")
pepci <- read.csv("pepci.csv")

all(pepci$X == pepci$date) #TRUE
#These two columns are identical, so we just use one of them.
pepci$X <- NULL
pepci$date <- as.POSIXlt(pepci$date)
colnames(pepci) <- c("Date", "Close")
pepci <- xts(pepci, order.by = pepci$Date, format = "%Y%m%d")
pepci$Close <- as.numeric(pepci$Close)
periodicity(pepci)
#Daily periodicity from 1995-01-03 to 2015-12-31 

wk <- to.weekly(pepci, OHLC = FALSE)
qt <- to.quarterly(pepci, OHLC = FALSE)
pepci.3y.daily <- last(pepci, "3 years")
pepci.5y.weekly <- last(wk, "5 years")
pepci.10y.weekly <- last(wk, "10 years")
pepci.20y.quarterly <- last(qt, "20 years")
#Recent prices

slreturns <- function(x) {
    mtx <- as.matrix(x)
    cprice <- mtx[, 2]
    cprice <- as.numeric(cprice)
    sr <- diff(cprice)/cprice[-length(cprice)]
    lr <- diff(log(cprice))
    olsr <- diff(cprice, lag = 2)/cprice[1 : (length(cprice) - 2)]
    ollr <- diff(log(cprice), lag = 2)
    hist(sr, nclass = 80, freq = FALSE, xlab = "Simple Return", main = "Histogram of Simple Return")
    #Draw histograms of non-overlapping simple return
    xs <- seq(from = min(sr), to = max(sr), by = 0.001)
    lines(xs, dnorm(xs, mean = mean(sr), sd = sqrt(var(sr))), col = "blue")
    #Draw normal distribution density line (mean and standard deviation same as simple return) 
    return(data.frame("Date" = mtx[1 : (nrow(mtx) - 1), 1], "Simple Return" = sr, "Log Return" = lr, 
                      "Overlapping Simple Return" = c(olsr, NaN), 
                      "Overlapping Log Return" = c(ollr, NaN)))
}

p3dr <- slreturns(pepci.3y.daily)
p5wr <- slreturns(pepci.5y.weekly)
p10wr <- slreturns(pepci.10y.weekly)
p20qr <- slreturns(pepci.20y.quarterly)
#Simple returns and log returns for non-overlapping and overlapping types.


#-------------------------------------------------
#Question 4: Intro. to HFT

library("lubridate")

aplo <- read.csv("AAPL.O.csv")
aapl <- subset.data.frame(aplo, select = c(Date.L., Time.L., Type, Price, Volume, 
                                           Bid.Price, Bid.Size, Ask.Price, Ask.Size))

aapl$Date.L. <- ymd(aapl$Date.L.)
aapl$Time.L. <- hms(aapl$Time.L.)
Trading.Time <- aapl$Date.L. + aapl$Time.L.
aapl <- data.frame(Trading.Time, aapl)
#Add trading time

AAPL.Quote <- subset(aapl, Type == "Quote")
AAPL.Trade <- subset(aapl, Type != "Quote")

Time.Diff<- difftime(aapl$Trading.Time[-1], 
                     aapl$Trading.Time[-length(aapl$Trading.Time)], units = "mins")
Time.Diff<- as.numeric(Time.Diff)

Spread <- aapl$Bid.Price - aapl$Ask.Price
Spread <- na.omit(Spread)
hist(Spread, xlim=c(min(Spread), max(Spread)), breaks = seq(min(Spread) : max(Spread), by = .01))

plot(AAPL.Trade$Trading.Time, AAPL.Trade$Volume, type = "l", xlab = "Time", ylab = "Volumn")

#-------------------------------------------------
#Liting Hu 9/24/2016