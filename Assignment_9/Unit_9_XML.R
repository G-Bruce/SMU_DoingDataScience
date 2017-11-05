# UNIT 9 - XML

library(XML)

# W3SCHOOL ##############################################################################################
fileUrl<-"http://www.w3schools.com/xml/simple.xml"

download.file(fileUrl, destfile = "food.xml")

doc = xmlTreeParse(file = "food.xml", useInternalNodes = TRUE)

rootnode<-xmlRoot(doc)

rootnode

rootnode[[1]]
# <food>
#   <name>Belgian Waffles</name>
#   <price>$5.95</price>
#   <description>Two of our famous Belgian Waffles with plenty of real maple syrup</description>
#   <calories>650</calories>
#   </food>

rootnode[[1]][[1]] 
# <name>Belgian Waffles</name>
rootnode[[1]][[2]] 
# <price>$5.95</price> 
rootnode[[1]][[3]] 
# <description>Two of our famous Belgian Waffles with plenty of real maple syrup</description> 
rootnode[[1]][[4]] 
# <calories>650</calories>
rootnode[[1]][[1]] 
# <name>Belgian Waffles</name> 

xmlSApply(rootnode,xmlValue)

names(rootnode)
# food   food   food   food   food 
# "food" "food" "food" "food" "food"

xpathSApply(rootnode,"//name",xmlValue)
# [1] "Belgian Waffles"             "Strawberry Belgian Waffles"  "Berry-Berry Belgian Waffles"
# [4] "French Toast"                "Homestyle Breakfast" 

xpathSApply(rootnode,"//price",xmlValue)
# [1] "$5.95" "$7.95" "$8.95" "$4.50" "$6.95"

xpathSApply(rootnode,"//calories",xmlValue)
# [1] "650" "900" "900" "600" "950"

xpathSApply(rootnode,"//description",xmlValue)
# [1] "Two of our famous Belgian Waffles with plenty of real maple syrup"                  
# [2] "Light Belgian waffles covered with strawberries and whipped cream"                  
# [3] "Light Belgian waffles covered with an assortment of fresh berries and whipped cream"
# [4] "Thick slices made from our homemade sourdough bread"                                
# [5] "Two eggs, bacon or sausage, toast, and our ever-popular hash browns"  

# ESPN ####################################################################################################

fileUrl<-"http://espn.go.com/nfl/team/stats/_/name/dal"

download.file(fileUrl, destfile = "dal_2.xml")

doc = xmlTreeParse(file = "dal_2.xml", useInternalNodes = TRUE)
doc = xmlTreeParse(fileUrl, useInternal = TRUE)

rootnode<-xmlRoot(doc)

rootnode

rootnode[[1]]

# 9.5 JSON ###############################################################################################
# http://www.gastonsanchez.com/work/webdata
# https://github.com/gastonstat/tutorial-R-web-data/blob/master/05-json-data/05-json-data.pdf
library(RJSONIO)
library(jsonlite)
library(magrittr)
library(tidyverse)

sw_data = rbind(
  c("Anakin", "male", "Tatooine", "41.9BBY",  "yes"),  
  c("Amidala", "female", "Naboo", "46BBY", "no"),
  c("Luke", "male", "Tatooine", "19BBY", "yes"),
  c("Leia", "female", "Alderaan", "19BBY", "no"),
  c("Obi-Wan",  "male", "Stewjon", "57BBY", "yes"),
  c("Han", "male", "Corellia", "29BBY", "no"),
  c("Palpatine", "male", "Naboo", "82BBY", "no"),
  c("R2-D2", "unknown", "Naboo", "33BBY", "no"))

sw_data

str(sw_data)

swdf<-data.frame((sw_data))

names(swdf) = c("Names", "Gender", "Homeworld", "Born", "Jedi")

str(swdf)

# JASON SYNTAX
sw_json<-toJSON(swdf)
# WHAT CLASS
class(sw_json)
# VIEW OBJECT
sw_json
# DISPLAY JSON FORMAT
cat(sw_json)
# CONRT JSON STRIN TO R LIST
sw_R = fromJSON(sw_json)
str(sw_R)
sw_R

# 9.6 Getting the Skill List and Links to Job Postings ###################################################
# CyberCoders

library(XML)
library(RCurl)

StopWords=readLines(file="Stoplist.txt")
asWords=function(txt, stopWords=StopWords, stem=FALSE)
{
  words=unlist(strsplit(txt, '[[:space:]]!.,;#:()/"]+'))
  words=words[words != ""]
  if(stem && require(Rlibstemmer))
    words=wordStem(words)
  i=tolower(words) %in% tolower(stopWords)
  words[!i]
}

removeStopWords= function(x, stopWords= StopWords){
  if(is.character(x))
    setdiff(x,stopWords)
  else if(is.list(x))
    lapply(x, removeStopWords, stopWords)
  else 
    x
}

cy.getFreeFormWords = function(doc, stopWords= StopWords){
  nodes=getNodeSet(doc, "//div[@class='job-details']/div[@data-section]")
  if(length(nodes)==0)
    nodes = getNodeSet(doc, "//div[@class='job-details']/[div[@data-section]//p")
}

# 9.7 Cleaning and Visualizing the Data ####################################################################