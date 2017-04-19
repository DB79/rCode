setwd("D:/yr4/Big_Data/Project/ipl/Analysis")


#install.packages("ggrepel")
#install.packages("readr")
#install.packages("dplyr")
#install.packages("sqldf")

library(sqldf)
library(ggplot2) 
library(readr)
library(dplyr)
library(ggrepel)

deliveries <- read_csv('deliveries.csv')

matches_per_player <-deliveries %>% 
  group_by(batsman) %>% 
  summarise(total_matches = n_distinct(match_id)) 


#   total boundaries per batsmen 
Boundaries_Per_Batsman  <- group_by(deliveries, batsman) %>% 
  summarise(boundaries = sum(ifelse(batsman_runs==4 | batsman_runs==6, 1, 0)))%>% 
  arrange(desc(boundaries))

# only batsmen who have scored more than 50 boundaries. 

Boundaries_Per_Batsman <- merge(Boundaries_Per_Batsman, matches_per_player, by.x = "batsman", by.y = "batsman")

Boundaries_Per_Batsman_Per_Match <- sqldf('select batsman, (boundaries/total_matches) as "Boundaries_Per_Match" from Boundaries_Per_Batsman group by batsman')


boxplot(Boundaries_Per_Batsman_Per_Match$Boundaries_Per_Match ,main="Boxplot of Average Boundaries Per Match Per Player", ylab="Average Boundaries")



