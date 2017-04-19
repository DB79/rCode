setwd("E:/yr4/Big_Data/Project/ipl/\Analysis")

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

deliveries <- read_csv('deliveries.csv')

# total_matches_per_player
matches_per_player <-deliveries %>% 
  group_by(bowler) %>% 
  summarise(total_matches = n_distinct(match_id)) 


#dot balls, total balls, runs conceded
bowler_stats <- group_by(deliveries, bowler)%>%summarise(dot_balls = sum(ifelse(batsman_runs ==0 &
                                                                                  extra_runs == 0 , 1, 0 ))
                                                         , total_balls = sum(ifelse(wide_runs == 1 &
                                                                                      noball_runs == 1 
                                                                                    , 0, 1 ))
                                                         , total_runs = sum(batsman_runs+extra_runs))

# add number of games played per player
bowler_stats <- merge(bowler_stats, matches_per_player, by.x = "bowler", by.y = "bowler")


# total wickets per player
total_wickets_per_player <- group_by(deliveries, bowler)%>%
  summarise(wickets = sum(ifelse(dismissal_kind 
  %in% c("caught", "bowled", "lbw", "caught and bowled"), 1,0)))

bowler_stats <- merge(bowler_stats, total_wickets_per_player, by.x = "bowler", by.y = "bowler")

# bowler name to allow for creation of boxplots
bowler_overall_stats <- sqldf('select (dot_balls) as "Dots" ,(total_balls) as "Deliveries", (total_runs) as "Runs_Conceded", (wickets), (total_matches) from bowler_stats')

bowler_stats_Per_match <- sqldf('select (total_balls/total_matches) as "Deliveries_Per_Match",
                                        (total_runs/total_matches) as "Runs_Conceded_Per_Match",
                                        (dot_balls/total_matches) as "Dots_Per_Match",
                                        (wickets/total_matches) as "Wickets_Per_Match" from bowler_stats')

# boxplot of Deliveries per player per match
boxplot(bowler_stats_Per_match$Deliveries_Per_Match, main="Boxplot of Deliveries  per player per match", ylab="Number of Deliveries")

# boxplot of Run Conceeded per player per match
boxplot(bowler_stats_Per_match$Runs_Conceded_Per_Match, main="Boxplot	of Runs Conceded  per player per match", ylab="Number of Runs Conceded")

# boxplot of Dot Balls per player per match
boxplot(bowler_stats_Per_match$Dots_Per_Match, main="Boxplot of Dot Balls  per player per match", ylab="Number of Dot Balls")

# boxplot of Wicets per player per match
boxplot(bowler_stats_Per_match$Wickets_Per_Match, main="Boxplot of Wickets per player per match", ylab="Number of Wickets")



