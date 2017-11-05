# UNIT 9 - Preparing Data for Analysis

#REFERENCES ######################################################################################
# https://cran.r-project.org/web/views/WebTechnologies.html
# http://code.markedmondson.me/googleAnalyticsR/
# https://cran.r-project.org/web/packages/googleAnalyticsR/README.html
# https://cran.r-project.org/web/packages/googleAnalyticsR/index.html
# 
# 

install.packages("WDI")
library(RJSONIO)
library(WDI)

WDIsearch("drought") 

# "Droughts, floods, extreme temperatures (% of population, average 1990-2009)"
# indicator "EN.CLC.MDAT.ZS"

drought <- WDI(indicator="EN.CLC.MDAT.ZS")

str(drought)

## How many variables in the data set?
# 4
## How many cases?
# 1,848
## Download data using another indicator
#
## You can use a different search term if you like
#
## Add the code to your makefile 