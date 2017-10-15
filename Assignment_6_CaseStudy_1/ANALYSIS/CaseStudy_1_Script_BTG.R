#library(UScensus2010)
# UScensus2010: https://cran.r-project.org/web/packages/UScensus2010/UScensus2010.pdf
#detach("package:UScensus2010", unload=TRUE)


# https://www.computerworld.com/article/3120415/data-analytics/how-to-download-new-census-data-with-r.html

#library(acs)
# https://cran.r-project.org/web/packages/acs/acs.pdf
#rm(kansas09)
#detach("package:acs", unload=TRUE)

# SETUP ######################################################################################################
# DEFINE WORKING DIRECTORY
path<-"/Users/michaerl/Documents/SMU_DoingDataScience/Assignment_6_CaseStudy_1/DATA"
setwd(path)

# READ DATA ##################################################################################################
#READ BREWERIES DATA
Breweries<-read.csv("Breweries.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE, col.names =c("Brewery_id","Name","City","State"))
Breweries$Name<-as.character(Breweries$Name)


#READ BEER DATA
Beers<-read.csv("Beers.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
Beers$Style<-as.factor(Beers$Style)
typesOfBeers <- unique(Beers$Style, incomparables = FALSE)
typesOfBeers

#find how many beers there are in the df
nrow(Beers)
#gets a list the different types of beers
typesOfBeers <- unique(Beers$Style, incomparables = FALSE)
#finds how many different types of beers there are
length(typesOfBeers)
head(typesOfBeers,10)
typesOfBeers[-79]   ## the 79th "type" of beer was empty

# Q1: How many breweries are present in each state? ###########################################################
## LOAD magrittr PACKAGE TO ACCESS THE PIPE "%>%" OPERATOR, WHICH ALLOWS FOR VALUES TO FORWARD
## INTO AN EXPRESSION OR FUNCTIONAL CALL
## LOAD THE dplyr PACKAGE TO ACCESS THE "group_by"
library(magrittr)
library(dplyr)
Breweries_Per_State<-Breweries %>% group_by(State) %>% summarise(Count_By_State = length(State))
Breweries_Per_State
# SORT DATAFRAME BY BREWERY_COUNT
Breweries_Per_State_Sort_ASND <- Breweries_Per_State[order(Breweries_Per_State$Count_By_State),] 
Breweries_Per_State_Sort_DSND <- Breweries_Per_State[order(-Breweries_Per_State$Count_By_State),] 

# Q2: MERGE DATA #############################################################################################
# MERGE BREWERIES AND BEER                       
# Q2 Merge beer data with the breweries data.... 
#    b) Print the first 6 observations and the last six observations to check the merged file
Breweries_Beers<- merge(Breweries, Beers, by=c("Brewery_id"), all = TRUE)

# RENAME COLUMNS
colnames(Breweries_Beers) <- c("Brewery_ID","Brewery","City", "State", "Beer_Name", "Beer_ID", "ABV", "IBU", "Beer_Style", "Ounces")
head(Breweries_Beers, n=6)
tail(Breweries_Beers, n=6)
#https://stats.idre.ucla.edu/r/modules/subsetting-data/
#(Breweries_Beers_Top_Bottom<-Breweries_Beers[1:6,])

#Find just the beers from Colorado
col <- Breweries_Beers[ grep("CO",Breweries_Beers$State), ]
#Group, and then add up all the beers found in each city in Colorado
BeersPerCity <- col %>% group_by(City) %>% summarise(BeersInCity = length(City))
#Sort the data
sortCity <- BeersPerCity[ order(BeersPerCity$BeersInCity,decreasing=TRUE), ]
head(sortCity)

rm(Breweries_Beers_Top_Bottom)

# Q3: NA's ###################################################################################################
# Report the number of NA's in each column.
#missing_data<-sum(is.na(Breweries_Beers$ABV))
missing_data <-sapply(Breweries_Beers, function(y) sum(length(which(is.na(y)))))
missing_data <- data.frame(missing_data)
missing_data
#<<<<<<< HEAD

#=======
                      
### Mike's version
                      
##emptyObservations <- c( sum(is.na(Breweries_Beers$Brewery_id)),     # Sums NAs for Brewery IDs
  ##                    sum(is.na(Breweries_Beers$City)),           # Sums NAs for Cities
  ##                    sum(is.na(Breweries_Beers$State)),          # Sums NAs for States
  ##                    sum(is.na(Breweries_Beers$`Beer Name`)),    # Sums NAs for Beer Names
  ##                    sum(is.na(Breweries_Beers$Beer_ID)),        # Sums NAs for Beer IDs
  ##                    sum(is.na(Breweries_Beers$ABV)),            # Sums NAs for ABV
  ##                    sum(is.na(Breweries_Beers$IBU))             # Sums NAs for IBU
##)
                      
#>>>>>>> b4aae26fac2eb0b61de3637528126bece9f0e7a2
# Q4 MEDIAN ALCHOL CONTENT AND BITTERNESS ####################################################################
# a) Compute the median alcohol content and international bitterness unit for each state. 
# b) Plot a bar chart to compare.
Median_ABV_Per_State<-Breweries_Beers %>% group_by(State) %>% summarise(Median_ABV_By_State = median(ABV))
Median_ABV_Per_State

Median_ABV_Per_State_CompleteCases<-na.omit(Median_ABV_Per_State)
Median_ABV_Per_State_CompleteCases
Mean_ABV_Per_State<-Breweries_Beers %>% group_by(State) %>% summarise(Median_ABV_By_State = mean(ABV))
Mean_ABV_Per_State

#EXPILICTLY SEARCH WITH THE SPACE WITHIN THE SEARCH STRING
MN<-subset(Breweries_Beers, select = c(State, ABV), subset=(State==" MN"))
MN
#REMOVE THE SPACE FROM THE ENTIRE COLUMN, THEN SEARCH
Breweries_Beers$State <- trimws(Breweries_Beers$State)
MN<-subset(Breweries_Beers, select = c(State, ABV), subset=(State=="MN"))
MN
#rm(MN)
AK <- Breweries_Beers[ which(Breweries_Beers$State =='AK'), c("State","ABV")]
str(Breweries_Beers)
Breweries_Beers<-as.character(Breweries_Beers$State)

AK<-Breweries_Beers[,c(4,7)]
AK<-Breweries_Beers[,c("State","ABV")]
str(AK)

AK$State <- as.character(AK$State)
AK <- AK[ which(AK$State ==' AK'), ]
rm(AK_1)

# Q5: BY STATE: MAX ALCOHOLIC BEER, BITTER BEER ############################################################## 
# a) Which state has the maximum alcoholic (ABV) beer? 
# b) Which state has the most bitter (IBU) beer?

sortedByABV <- Breweries_Beers[ order(Breweries_Beers$ABV, decreasing=TRUE), ]    # Sorts the DF by ABV
sortedByABV[1,"State"]   #Colorado
sortedByABV[1,c(2,3,4,5,7)]
sortedByIBU <- Breweries_Beers[ order(Breweries_Beers$IBU, decreasing=TRUE), ]    # Sorts the DF by IBU
sortedByIBU[1,"State"]   #Oregon
                      
                      
# Q6: SUMMARY STATS FOR ABV ##################################################################################
#  Summary statistics for the ABV variable.
summary(Breweries_Beers$ABV)   # Min 0.001   1Q 0.05   Median 0.056   3Q 0.067   Max 0.128    62 NA
                      

# Q7: CORRELATION: BITTERNESS AND ALCOHOLIC CONTENT ##########################################################
# a) Is there an apparent relationship between the bitterness of the beer and its alcoholic content? 
Cor_Bitt_IBU<-cor(Breweries_Beers$IBU, Breweries_Beers$ABV, use = "pairwise.complete.obs", method="pearson")
# b) Draw a scatter plot. You are welcome to use the ggplot2 library for graphs. 
plot(Breweries_Beers$IBU,Breweries_Beers$ABV, ylim = c(.025, .125))
# PLOT REGRESSION LINE OF A SCATTER PLOT
library(faraway)
plot(IBU~ABV, data=Breweries_Beers, xlim = c(.025, .125))
m<-lm(IBU ~ ABV, data=Breweries_Beers)
abline(m)
# Please ignore missing values in your analysis. 
# Make your best judgment of a relationship and EXPLAIN your answer.





#determine if data is normally distributed in R
#https://stats.stackexchange.com/questions/3136/how-to-perform-a-test-using-r-to-see-if-data-follows-normal-distribution
# VIEW THE DISTRIBUTIONS OF BITTERNESS AND ALCOHOL
plot(density(Breweries_Beers$IBU, na.rm = TRUE), main = "Bitterness Density Distribution")
plot(density(Breweries_Beers$ABV, na.rm = TRUE), main = "Alcohol Density Distribution", xlim = c(.02, .11))

## Perform the Shapiro-Wilk test for normality
shapiro.test(Breweries_Beers$IBU); shapiro.test(Breweries_Beers$ABV)

## Plot using a qqplot
qqnorm(Breweries_Beers$IBU);qqline(Breweries_Beers$IBU, col = 2)
qqnorm(Breweries_Beers$ABV);qqline(Breweries_Beers$ABV, col = 2)

hist(Breweries_Beers$IBU, 90, col="black")
hist(Breweries_Beers$ABV, 90, col="black")

# Pg 215 Testing Correlation Significance
# NORMAL DISTRIBUTION
cor_test_Bitt_IBU<-cor.test(Breweries_Beers$IBU, Breweries_Beers$ABV)
cor_test_Bitt_IBU
# NON-NORMAL
cor_test_Bitt_IBU<-cor.test(Breweries_Beers$IBU, Breweries_Beers$ABV, method = "Spearman")
cor_test_Bitt_IBU
# P-VALUE OF SIGNIFICANCE p<0.05 INDICATES THAT THE CORRELATION IS LIKELY SIGIFICANT

# 


#http://www.dummies.com/programming/r/how-to-test-data-normality-in-a-formal-way-in-r/


# CORRELATION/COVARIANCE
#http://www.statmethods.net/stats/correlations.html
# Complete Cases

#IS THERE A CORRELATION BETWEEN BEER AND ABV --- 4.3% ... NO
Corr_Beer_ID_ABV<-cor(Breweries_Beers$Beer_ID, Breweries_Beers$ABV, use = "pairwise.complete.obs", method="pearson")

#IS THERE A CORRELATION BETWEEN BEER AND CITY
# CREATE UNIQUE CITY, STATE
unique_city<-data.frame(seq(1,length(unique(Breweries$City))), unique(Breweries$City))
colnames(unique_city)<- c("city_id", "city")


# INSERT NEW COLUMN 
unique_city$city_id<-

Corr_Beer_ID_City<-cor(Breweries_Beers$City , Breweries_Beers$Beer_ID , use = "pairwise.complete.obs", method="pearson")


#library(Hmisc)
#detach("package:Hmisc", unload=TRUE)
#Corr_Beer_ID_ABV<-rcorr(Breweries_Beers, type ="pearson")
