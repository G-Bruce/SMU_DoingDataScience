# Unit 11: Modeling Financial Data
# Uncomment and install packages if you don't have it
#install.packages("tseries")

# Description: Please fill out that this is for educational purposes and talk about what it's for!

library(tseries)

## S&P 500 (^GSPC)
###    SNP - SNP Real Time Price. Currency in USD

# TODO: Download the data of SP500 '^gspc'.
SNPdata <- get.hist.quote('^gspc',quote="Close")

# TODO: Calculate the log returns, which is the subtraction of log(lag(SNPdata)) and log(SNPdata)
SNPret <- log(lag(SNPdata))-log(SNPdata)

# TODO: Calculate volatility measure that is to multiply sd(SNPret),sqrt(250), 100
# THE 250 COMES FROM THE FACT THERE ARE ROUGHLY 250 TRADING DAYS IN A YEAR
# MULTIPLYING BY 100 MAKES IT A PERCENTAGE

SNPvol <- sd(SNPret)*sqrt(250)*100

# d IS THE WEIGHT.  IF d = 10 THEN BY THE FORMULA IN THE FUNCTION (1 - 1/d) = 0.9
## Define getVol function for volatility

getVol <- function(d, logrets) {
	var = 0
	lam = 0
	varlist <- c()

	for (r in logrets) {
		lam = lam*(1 - 1/d) + 1
	  var = (1 - 1/lam)*var + (1/lam)*r^2
		varlist <- c(varlist, var)
	}

	sqrt(varlist)
}

# Calculate volatility over entire length of series for various three different decay factors: 10 30. 100

# TODO: call getVol function with the parameters: 10,SNPret
volest <- getVol(10,SNPret)

# TODO: call getVol function with the parameters: 30,SNPret
volest2 <- getVol(30,SNPret)

# TODO: call getVol function with the parameters: 100,SNPret
volest3 <- getVol(100,SNPret)

# Plot the results, overlaying the volatility curves on the data, just as was done in the S&P example.
plot(volest,type="l")
# TODO: Add connected line segments for volest2 with the parameters: type="l",col="red"
lines(volest2, type="l", col="red")
# TODO: Add connected line segments for volest3 with the parameters: type="l",col="blue"
lines(volest3, type="l", col="blue")
