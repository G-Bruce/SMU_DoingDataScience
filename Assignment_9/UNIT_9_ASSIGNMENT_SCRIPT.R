# # UNIT 9 ASSIGNMENT
# MSDS 6306:  Doing Data Science – Preparing Data 

#  use View Source to narrow down good nodes to use
 
# PACAGES:  rcurl, rvest, dplyr, tidyr, ggplot2, reshape2, or stringr
library(dplyr)
library(tidyr)
library(reshape2)

# REMOVE ALL ENVIRONMENT VARIABLES
rm(list = ls())

# REVEST: Easily Harvest (Scrape) Web Pages
library(rvest)
# 1a) ################################################################################################

# URL OF INTEREST
url <- "http://www.imdb.com/title/tt1201607/fullcredits?ref_=tt_ql_1"

# CONSUME WEBPAGE
webpage <- read_html(url)

# IDENTIFY NODES
nodes_table <- html_nodes(webpage, 'table')

# Parse an html table into a data frame
nodes <- html_table(nodes_table)

cast <- nodes[[3]]

#str(cast)

# 1b) ################################################################################################

# ACCESS THE LIST OF ACTORS
# RENAME COLUMNS
# cast %>% select(X2,X4) %>% rename(Actor=X2,Charater = X4) -> cast
# 
# # FILTER OUT EMPTY ROWS
# cast %>% filter(., Actor != '') ->x
# 
# # FILTER OUT THE PHRASE: 'Rest of cast listed alphabetically:'
# x %>% filter(., Actor != 'Rest of cast listed alphabetically:') -> x
# 
# # COMPLETE CASES
# #cast[complete.cases(cast[ , 1:2]),] -> x DOESNT WORK
# # na.omit(cast) -> x
# cast[1,2]
# 
# x[x==""] <- NA

#nodes[3] # X1, X2, X3  X2 CONTAINS THE NAME 1-148

#cast <- case %>% select(X2) %>%  filter(., between(row_number(), 2, n())) %>% filter(., row_number() != 92)
library(stringr)
# cast %>% filter(str_detect(X2, ".arwick"))
# 1c) ################################################################################################

cast <- cast %>% select(X2,X4) %>%  
  select(X2,X4) %>% 
  rename(Actor=X2,Character = X4) %>%
  filter(., Actor != '') %>%
  filter(., Actor != 'Rest of cast listed alphabetically:')
  
  
 # filter(., between(row_number(), 2, n())) %>% filter(., row_number() != 92)

# 1d) ################################################################################################
#test_name_1 <- "Helena Bonham Carter"
#test_name_2 <- "Helena Carter"
library(stringr)

#stringr::str_count(test_name_1, " ")  # "Helena Bonham Carter"  [1] 2

#stringr::str_locate(test_name_1, " ")

#FullName <- c("Jimmy John Cephus", "Frank Chester", "Hank Chester", "Brody Buck Clyde", "Merle Rufus Roscoe Jed Quaid")

df <- data.frame(FullName)

stringr::str_count(df$FullName, " ")

stringr::word(df$FullName, 1)
stringr::word(df$FullName, 2)

split_firstname = function(full_name){
  x <- stringr::str_count(full_name, " ")
  return(x)
}

split_firstname(df$FullName)
split_firstname(cast$Actor)


split_firstname = function(full_name){
  x <- stringr::str_count(full_name, " ")
  if(x==1){
    return(stringr::word(full_name,1))
  }else if(x==2){
    return(paste(stringr::word(full_name,1), stringr::word(full_name,2), sep = " "))
  }else if(x==4){
    return(paste(stringr::word(full_name,1), stringr::word(full_name,2), stringr::word(full_name,3), stringr::word(full_name,4), sep = " "))
  }
}

split_firstname = function(full_name){
  x <- stringr::str_count(full_name, " ")
  ifelse(x==1, return(stringr::word(full_name,1)), 
         ifelse(x==2, return(paste(stringr::word(full_name,1), stringr::word(full_name,2), sep = " ")), 
                ifelse(x==4, return(paste(stringr::word(full_name,1), stringr::word(full_name,2), stringr::word(full_name,3), stringr::word(full_name,4), sep = " ")))))
} 
library(dplyr)

split_firstname <- function(full_name){
  x <- stringr::str_count(full_name, " ")
  full_name <- trimws(full_name, which = "both")
  case_when(
    x == 1 ~ stringr::word(full_name, 1),
    x == 2 ~ paste(stringr::word(full_name,1), stringr::word(full_name,2), sep = " "),
    x == 4 ~ paste(stringr::word(full_name,1), stringr::word(full_name,2), stringr::word(full_name,3), stringr::word(full_name,4), sep = " ")
  )
}

#split_firstname(df$FullName)

#cast <- cast %>% rowwise() %>%  mutate(Last_Name = stringr::word(FullName, -1))

#df$first <- sapply(df$FullName, split_firstname)

#split_firstname(cast$Actor)

cast$FirstName <- sapply(cast$Actor, split_firstname) # WORKS
# cast <- cast %>%  mutate(Surname = stringr::word(.$Actor, -1)) # WORKS

cast_2 <- cast %>%  mutate(Surname = stringr::word(.$Actor, -1)) %>% select(FirstName, Surname, Character)

cast_bu <- cast
cast <- cast_bu

#cast <- cast %>% rowwise() %>%  mutate(FirstName = split_firstname(FullName), LastName = stringr::word(FullName, -1)) # DOES NOT WORK
#cast <- cast %>% rowwise() %>%  mutate(LastName = stringr::word(FullName, -1)) # DOES NOT WORK

cast_bu <- cast

split_firstname = function(full_name){
  x <- stringr::str_count(full_name, " ")
  if(x == 1){
    #x <- word(full_name,1) 
    return(1)
  }else if (x == 2){
    #x <- paste(word(full_name,1), word(full_name,2), sep = " ")
    return(2)
  }else if (x == 4){
    #x <- paste(word(full_name,1), word(full_name,2), word(full_name,3), word(full_name,4), sep = " ")
    return(4)
  }
}

split_firstname = function(full_name){
  x <- stringr::str_count(full_name, " ")
  if(x == 1){
    x <- word(full_name,1) 
   return(x)
  }else if (x == 2){
    x <- paste(word(full_name,1), word(full_name,2), sep = " ")
    return(x)
  }else if (x == 4){
    x <- paste(word(full_name,1), word(full_name,2), word(full_name,3), word(full_name,4), sep = " ")
  }
}

split_firstname = function(full_name){
  x <- stringr::str_count(full_name, " ")
  if(x == 1){
    a <- stringr::word(full_name,1) 
    return(a);
    
  }else if (x == 2){
    b <- stringr::word(full_name,1)
    c <- stringr::word(full_name,2)
    a <- paste0(b,c,sep= " ")
    return(a);

  }else if (x == 4){
    b <- stringr::word(full_name,1)
    c <- stringr::word(full_name,2)
    d <- stringr::word(full_name,3)
    e <- stringr::word(full_name,4)
    a <- paste0(b,c,d,e,sep= " ");
    return(a)

  }
}

split_firstname = function(full_name){
  x <- stringr::str_count(full_name, " ")
  case_when(
    x == 0 ~ 0
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 3
  )
}

#str_locate(cast$Actor, "[$ ]")

#str_length(cast$Actor[[1]]) # [1] 13

cast %>% mutate(FirstName = split_firstname(.$Actor)) -> cast

split_firstname(cast$Actor)

stringr::str_count(cast$Actor, " ")




# http://www.fabianheld.com/stringr/
# https://www.regular-expressions.info/reference.html
# https://regexr.com/
# http://stringr.tidyverse.org/articles/regular-expressions.html
# http://r4ds.had.co.nz/strings.html

# https://rdrr.io/cran/dplyr/man/slice.html
# LIST OF LISTS http://r4ds.had.co.nz/lists.html

str(nodes)
str(nodes[3])

nodes[3][1:148]

x <- (nodes = unlist(nodes[3]))
str(x)

x <- unlist(nodes, recursive = TRUE, use.names = TRUE)
str(x)

# SHOW ALL THE NAMES (COLUMNS) OF AN OBJECT
names(x)

# ACCESS' OBJECT NAME AND VALUE
x["X22"]  # X22 "Ralph Fiennes"

# ACCESS' ONLY THE VALUE OF OBJECT  
x[["X22"]]    # [1] "Ralph Fiennes"
x[["X2148"]]  # [1] "Thomas Williamson"

# CONVERT X TO DATADRAME
f_df <- data.frame(x)

str(f_df)


f_df %>% filter(length(x) > 0) -> f_df_f

x22-x2148
f_df[149:308,]

f_df


# CONVERT TABLE TO DATA FRAME
nodes_df <- data.frame(nodes = unlist(nodes[3]))
unlist(x, recursive = TRUE, use.names = TRUE)

x <- unlist(nodes, recursive = TRUE, use.names = TRUE)


f_df <- data.frame(x)

f_df <- df[c(162:308),1]

 rm(nodes_df)
 
 # Q2a) ####################################################
 url <- "http://www.espn.com/nba/team/stats/_/name/sa/san-antonio-spurs"
 
 # Q2b) ####################################################
 # CONSUME WEBPAGE
 webpage <- read_html(url)
 
 # IDENTIFY NODES
 nodes_table <- html_nodes(webpage, 'table')
 
 # Parse an html table into a data frame
 nodes <- html_table(nodes_table)
 
 shoot_stats <- unlist(nodes)
 
 shoot_stats <- data.frame(shoot_stats)
 
 #x_df_cols <- as.data.frame(split(x_df, 1:16))
 shoot_stats <- matrix(shoot_stats[,1], nrow=16, ncol=30)
 
 #shoot_stats <- nodes[[2]]
 
 shoot_stats <- data.frame(shoot_stats)
 str(shoot_stats)
#shoot_stats <- url %>% rvest::read_html(.) %>% rvest::html_nodes(., 'table') %>% rvest::
#   rvest::html_table(.) %>%  unlist(.) %>% data.frame(.) %>% 
#   matrix(.[,1], nrow=16, ncol=30) %>% data.frame(.)
 
 # Q2c) ####################################################
 #https://idc9.github.io/stor390/notes/dplyr/dplyr.html#renaming_variables
 #
 shoot_stats <- shoot_stats %>%  .[which(.$X1 !='GAME STATISTICS'), ]  %>%  
   .[which(.$X1 !='PLAYER'), ] %>%  .[which(.$X1 !='Totals'), ] %>% select(-16) %>%
   rename(., PLAYER = X1, FGM = X2, FGA = X3, 'FG%' = X4, '3PM' = X5, '3PA' = X6, 
          '3P%' = X7, FTM = X8, FTA = X9, 'FT%' = X10, '2PM' = X11, '2PA' = X12, '2P%' = X13, 
          PPS = X14, 'AFG%' = X15, 'FGM' = X17, 'FGA' = X18, 'FG%' = X19, '3PM' = X20, 
          '3PA' = X21, '3P%' = X22,  'FTM' = X23, 'FTA' = X24, 'FT%' = X25, '2PM' = X26, 
          '2PA' = X27, '2P%' = X28, 'PPS' = X29,'AFG%' = X30)
   
 #shoot_stats_col_names <-  shoot_stats[2,]
 
 #shoot_stats <- shoot_stats[c(-1,-2),]

 #shoot_stats_col_names
library(dplyr)
 #rename(data frame, var1_new_name = var1_old_name, var2_new_name = var2_old_name,
# shoot_stats <- dplyr::rename(shoot_stats, PLAYER = X1, FGM = X2, FGA = X3, 
#'FG%' = X4, '3PM' = X5, '3PA' = X6, '3P%' = X7, FTM = X8, FTA = X9, 'FT%' = X10, 
#'2PM' = X11,  '2PA' = X12, '2P%' = X13, PPS = X14, 'AFG%' = X15)
 
 # Q2c2 ########################################################################
 # shoot_stats$FirstName <- sapply(shoot_stats$PLAYER, split_firstname)
 # 
 # #sapply(shoot_stats, paste(stringr::word(shoot_stats$PLAYER,1), stringr::word(shoot_stats$PLAYER,2), sep = " "))
 # 
 # stringr::str_count(shoot_stats$PLAYER, " ")
 # stringr::word(shoot_stats$PLAYER,1)
 # paste(stringr::word(shoot_stats$PLAYER,1), stringr::word(shoot_stats$PLAYER,2), sep = " ")

shoot_stats_name <- function(player){
   x <- paste(stringr::word(shoot_stats$PLAYER,1), stringr::word(shoot_stats$PLAYER,2), sep = " ")
   return(sub(",","",x))
 }
 
shoot_stats$Name <- sapply(shoot_stats$PLAYER, shoot_stats_name)
 
#shoot_stats$Name <- sub(",","",shoot_stats$Name)

shoot_stats_position <- function(player){
  return(stringr::word(shoot_stats$PLAYER,3))
}

shoot_stats$Position <- sapply(shoot_stats$PLAYER, shoot_stats_position)

 shoot_stats_name <- unlist(shoot_stats$Name)
 shoot_stats_name <-  shoot_stats_name %>% data.frame(shoot_stats_name) %>% select(Name = 1)
 
 shoot_stats_position <- unlist(shoot_stats$Position)
 shoot_stats_position <-  shoot_stats_position %>% data.frame(shoot_stats_position) %>% select(Position = 1)
 
 shoot_stats_2 <- cbind(shoot_stats_name,shoot_stats_position)
 
 rm(shoot_stats_position)
 rm(shoot_stats_name)
 
 shoot_stats_3 <- data.frame(shoot_stats[,4])
 
 shoot_stats_4 <- cbind(shoot_stats_2,shoot_stats_3)
 
 rm(shoot_stats_2)
 rm(shoot_stats_3)
 
 shoot_stats <- shoot_stats_4 %>% rename(FG_Percent = shoot_stats...4.)
 str(shoot_stats)
 
 rm(shoot_stats_4)
 
 shoot_stats$FG_Percent <- as.double(as.character(shoot_stats$FG_Percent))
 str(shoot_stats)
 
# shoot_stats <- matrix(shoot_stats[,1], nrow=14, ncol=31)


# Q2c3 ########################################################################
# shoot_stats_bu <- shoot_stats
# shoot_stats <- shoot_stats_bu
str(shoot_stats)
#shoot_stats <- shoot_stats %>% select('Name', 'Position', 'FGM', 'FGA', 'FG%', '3PM', '3PA', '3P%','FTM', 
#'FTA', 'FT%', '2PM', '2PA', '2P%', 'PPS', 'AFG%')

#C('FGM', 'FGA', 'FG%', '3PM', '3PA', '3P%','FTM', 'FTA', 'FT%', '2PM', '2PA', '2P%', 'PPS', 'AFG%')
# https://garrettgman.github.io/tidying/
#https://sesync-ci.github.io/data-manipulation-in-R-lesson/2016/07/26/

# shoot_stats_col_type <- function(col){
#   shoot_stats$col <- as.double(shoot_stats$col)
# }
# 
# shoot_stats_col_type <- function(df, col){
#   col <-enquo(col)
#   print(col)
#   
#   df %>% as.double(!!col) -> df
# }
str(shoot_stats)
# shoot_stats$FGM <- as.double(as.character(shoot_stats$FGM))
# shoot_stats$FGA <- as.double(as.character(shoot_stats$FGA))
# shoot_stats$'FG%' <- as.double(as.character(shoot_stats$'FG%'))
# shoot_stats$'3PM' <- as.double(as.character(shoot_stats$'3PM'))
# shoot_stats$'3PA' <- as.double(as.character(shoot_stats$'3PA'))
# shoot_stats$'3P%' <- as.double(as.character(shoot_stats$'3P%'))
# shoot_stats$'FTM' <- as.double(as.character(shoot_stats$'FTM'))
# shoot_stats$'FTA' <- as.double(as.character(shoot_stats$'FTA'))
# shoot_stats$'FT%' <- as.double(as.character(shoot_stats$'FT%'))
# shoot_stats$'2PM' <- as.double(as.character(shoot_stats$'2PM'))
# shoot_stats$'2PA' <- as.double(as.character(shoot_stats$'2PA'))
# shoot_stats$'2P%' <- as.double(as.character(shoot_stats$'2P%'))
# shoot_stats$'PPS' <- as.double(as.character(shoot_stats$'PPS'))
# shoot_stats$'AFG%' <- as.double(as.character(shoot_stats$'AFG%'))
# shoot_stats$'FGM' <- as.double(as.character(shoot_stats$'FGM'))
# shoot_stats$'FGA' <- as.double(as.character(shoot_stats$'FGA'))
# shoot_stats$'FG%' <- as.double(as.character(shoot_stats$'FG%'))
# shoot_stats$'3PM' <- as.double(as.character(shoot_stats$'3PM'))
# shoot_stats$'3PA' <- as.double(as.character(shoot_stats$'3PA'))
# shoot_stats$'3P%' <- as.double(as.character(shoot_stats$'3P%'))
# shoot_stats$'FTM' <- as.double(as.character(shoot_stats$'FTM'))
# shoot_stats$'FTA' <- as.double(as.character(shoot_stats$'FTA'))
# shoot_stats$'FT%' <- as.double(as.character(shoot_stats$'FT%'))
# shoot_stats$'2PM' <- as.double(as.character(shoot_stats$'2PM'))
# shoot_stats$'2PA' <- as.double(as.character(shoot_stats$'2PA'))
# shoot_stats$'2P%' <- as.double(as.character(shoot_stats$'2P%'))
# shoot_stats$'PPS' <- as.double(as.character(shoot_stats$'PPS'))
# shoot_stats$'AFG%' <- as.double(as.character(shoot_stats$'AFG%'))

shoot_stats <- shoot_stats %>% select('Name', 'Position', 'FG%')


library(ggplot2)
str(shoot_stats)
#rownames(shoot_stats) <- 1:nrow(shoot_stats)

# shoot_stats <- shoot_stats_bu
# 
# shoot_stats <- unlist(shoot_stats)
# geom_jitter(aes(FG_Percent, Name)) +

ggplot(shoot_stats) +
  geom_point(aes(FG_Percent, Name, color=Position)) +
  ggtitle("ESPN NBA Data: San Antonio Spurs") +
  labs(x = "Field Goals Percentage Per Game")

ggplot(shoot_stats) +
  geom_point(aes(FG_Percent, Name)) +
  facet_wrap(~Position) +
  ggtitle("ESPN NBA Data: San Antonio Spurs") +
  labs(x = "Field Goals Percentage Per Game")

 