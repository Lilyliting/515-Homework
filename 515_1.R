#--------------------------------------------------
#Question 1

setwd("/Users/apple/Desktop/515/R files for 515")
library('fBasics')
library('quantmod')

?basicStats
#This command will open the help window introducing Basic Time Series Statistics
#It returns a data frame with the following entries and row names: 
#nobs, NAs, Minimum, Maximum , 1. Quartile, 3. Quartile, Mean, Median, Sum,
#SE Mean, LCL Mean, UCL Mean, Variance, Stdev, Skewness, Kurtosis.

xx <- basicStats(rnorm(100), ci = 0.95)

??basicStats
#Nothing in help window but a suggestion help page: fBasis:basicStats

#--------------------------------------------------
#Question 2

#Create a series
v1 <- seq(-10, 10, by=0.1)
print(v1)

#Create a STRING vector
x <- c("0", "1", "2")
v2 <- rep(x, times = 3)

#Convert to numeric
v2num <- as.numeric(v2)

#Convert to logical
v2NA <- as.logical(v2) #The result is all NA because the vector type is character
v2logical <- as.logical(v2num) #F T T F T T F T T

#Create a matrix
m2 <- matrix(v2num, nrow = 3, byrow = T)

#inverse matrix
solve(m2) #system is exactly singular: U[1,1] = 0
m2[1,1] <- 1
m2[2,2] <- 5
invm2=solve(m2)
print(invm2)

#Create a list
myFirstList <- list(char = v2, integer = v2num, NAs = v2NA, bool = v2logical, mat=m2)

#Two methods to subset the 4th element
myFirstList$bool
myFirstList[4]

#--------------------------------------------------
#Question 3
#Loops

#The script below is efficient only when n is small. (n < 16)
#k <- 1
#n <- 10
#c <- 0
#while(c < n - 1)
#{
#  c <- 0
#  k <- k+1 
#  for (i in n:2)
#  {
#    if (k%%i == 0)
#      c <- c+1
#    else
#      break
#  }
#}
#print(k)


#To calculate the lease common multiple of 1-20
n <- 20
a <- 1
b <- 2
for (i in 3:n)
{
    c <- min(a,b)
    while(a%%c!=0 || b%%c!=0)
    {
        c <- c - 1
    }
    lcmab <- a * b / c
    a=i
    b=lcmab
}
print(b)
#The result is 232792560.

#--------------------------------------------------
#Question 4

C2008 <- read.csv("C.csv")

#Create a subtable
Subtable <- data.frame(Open = C2008$Open, High = C2008$High, Low = C2008$Low, Close = C2008$Close)

#Means by column
meanbycol <- apply(Subtable, 2, mean)

#Means by row
meanbyrow <- apply(Subtable, 1, mean)
meanbyrow <- matrix(meanbyrow, nrow = 3, byrow = T)

#--------------------------------------------------
#Liting Hu 9/9/2016