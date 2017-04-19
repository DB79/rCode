
setwd("D:/yr4/Big_Data/Project/ipl/Analysis")


#install.packages("ggrepel")
#install.packages("readr")
#install.packages("dplyr")
#install.packages("sqldf")
#install.packages("ggplot2") 

library(sqldf)
library(ggplot2) 
library(readr)
library(dplyr)
library(ggrepel)


deliveries <- read_csv('deliveries.csv');

#total matches per player
matches_per_player <-deliveries %>% 
  group_by(batsman) %>% 
  summarise(total_matches = n_distinct(match_id)) 


# get runs, balls and strike rate for all batsmen
batsman <- sqldf('SELECT batsman, SUM(batsman_runs) AS "runs", Count(match_id) AS "balls"
                 FROM deliveries GROUP BY batsman')

batsman_overall_stats <- merge(batsman, matches_per_player, by.x = "batsman", by.y = "batsman")

batsman_stats_per_match <- sqldf('select batsman,
                                         (runs/total_matches) as "Runs_Per_Match",
                                         (balls/total_matches) as "Balls_Faced_Per_Match"
                                         from batsman_overall_stats group by batsman')

sr = sqldf('select batsman, ((cast(Runs_Per_Match as float)/Balls_Faced_Per_Match )*100) as "strike_rate" from batsman_stats_per_match group by batsman')

batsman_stats_per_match <- merge(batsman_stats_per_match, sr, by.x = "batsman", by.y = "batsman")


# scatter plot Runs scored vs balls faced
plot(x	=	batsman$balls,	y	=	batsman$runs, main	= "Scatter Plot of Runs Scored vs Balls Faced", xlab	=	"Balls Faced", ylab	=	"Runs Scored")


# remove the batsmen to allow for boxplot creation
stats <- sqldf('select Runs_Per_Match, Balls_Faced_Per_Match, strike_rate from batsman_stats_per_match')

# boxplot of average runs scored per player per match
boxplot(stats$Runs_Per_Match ,main="Boxplot of Average Runs Per Match Per Player", ylab="Average Runs")

#boxplot of average number of balls faced per match
boxplot(stats$Balls_Faced_Per_Match ,main="Boxplot of Balls Faced Per Match Per Player", ylab="Average Number of Balls Faced")

#boxplot of average strike rate per match
boxplot(stats$strike_rate ,main="Boxplot of Average Strike Rate Per Match Per Player", ylab="Average Strike Rate")

