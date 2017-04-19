###############################################################################################
# COMMON SETUP
################################################################################################

setwd("D:/yr4/Big_Data/Project/ipl/Analysis")

#install.packages("readr")
#install.packages("sqldf")
#install.packages("dplyr")

library(sqldf)
library(readr)
library(dplyr)
library(ggplot2)


matches <- read.csv('matches.csv')
deliveries <- read.csv('deliveries.csv')

# get total matches where team is team1 and team is team2
total_team1_matches_per_team <- sqldf('select team1 as "team", count(id) as "matches" from matches group by team1')
total_team2_matches_per_team <- sqldf('select team2 as "team", count(id) as "matches" from matches group by team2')

# combine both sets above th get total numbuer of games per team

total_matches_per_team <- sqldf('
                                select team,sum(matches) total_matches
                                from
                                (
                                select team,matches
                                from total_team1_matches_per_team
                                union all
                                select team,matches
                                from total_team2_matches_per_team
                                ) t
                                group by team')

################################################################################################
# EnD COMMON SETUP
################################################################################################


################################################################################################
# TEAM STATS - WINNING %
################################################################################################

#  get each team that has played a game
df_teams<-(as.data.frame(unique(matches$team1)))

# column name = team
colnames(df_teams)[1]<-"team"

# total wins for each team
df_wins<-matches %>% 
  filter(result == "normal" | result == "tie") %>% 
  group_by(winner) %>% 
  summarise(total_win = n_distinct(id)) 

# merge two data-frames
df_teams<-merge(df_teams, df_wins, by.x = "team", by.y = "winner")



# combine data frames, no. wins, total matches
df_teams<-merge(df_teams, total_matches_per_team, by.x = "team", by.y = "team")


# get win % based on total wins and total matches
df_teams<-df_teams %>% 
  mutate(winning_perc = (total_win/total_matches_per_team$total_matches)*100)


################################################################################################
# END TEAM STATS - WINNING %
################################################################################################


################################################################################################
# Team Batting Performance - Run Scored
################################################################################################

Runs_Scored_Per_Team <- sqldf('select sum(total_runs) AS "total_runs", batting_team from deliveries group by batting_team order by 1 DESC')

Runs_Scored_Per_Team <- merge(Runs_Scored_Per_Team, total_matches_per_team, by.x = "batting_team", by.y = "team")

Runs_Scored_Per_Match_Per_Team <- sqldf('select batting_team, (cast(total_runs as float)/total_matches) as "Runs_Per_Match" from Runs_Scored_Per_Team group by batting_team')

Runs_Scored_Per_Team <- merge(Runs_Scored_Per_Team, Runs_Scored_Per_Match_Per_Team, by.x = "batting_team", by.y = "batting_team")


################################################################################################
# End Team Batting Performance - Run Scored
################################################################################################


################################################################################################
# Team Bowling Performance - Runs Conceded 
################################################################################################


Runs_Conceded_Per_Team <- sqldf('select sum(total_runs) AS "total_runs", bowling_team from deliveries group by bowling_team order by 1 DESC')

Runs_Conceded_Per_Team <- merge(Runs_Conceded_Per_Team, total_matches_per_team, by.x = "bowling_team", by.y = "team")

Runs_Conceded_Per_Match_Per_Team <- sqldf('select bowling_team, (cast(total_runs as float)/total_matches) as "Runs_Per_Match" from Runs_Conceded_Per_Team group by bowling_team')

Runs_Conceded_Per_Team <- merge(Runs_Conceded_Per_Team, Runs_Conceded_Per_Match_Per_Team, by.x = "bowling_team", by.y = "bowling_team")


################################################################################################
# End Team Bowling Performance - Runs Conceded 
################################################################################################


################################################################################################
# Team Bowling Performance - Wickets Taken
################################################################################################

# get total number of wickets per team
wickets <- sqldf('select bowling_team, count(*) as "Total_Wickets_Taken" from deliveries where dismissal_kind != "" group by bowling_team')

wickets <- merge(wickets, total_matches_per_team, by.x = "bowling_team", by.y = "team")

#calculate wickets per match and add new column to wickets dataset
wickets_per_match <- sqldf('select bowling_team ,(cast(Total_Wickets_Taken as float)/total_matches) as "Wickets_Per_Match" from wickets group by bowling_team')

wickets <- merge(wickets, wickets_per_match, by.x = "bowling_team", by.y = "bowling_team")

################################################################################################
# End Team Bowling Performance - Wickets Taken
################################################################################################


################################################################################################
# Correlations
################################################################################################

# correlation between winning % and wickets taken per match
cor(df_teams$winning_perc,wickets$Wickets_Per_Match)

# correlation between winning % and runs scroed per match
cor(df_teams$winning_perc, Runs_Scored_Per_Team$Runs_Per_Match)

# correlation between winning % and run conceded per match
cor(df_teams$winning_perc, Runs_Conceded_Per_Team$Runs_Per_Match)

# correlation between runs scored and runs conceded
cor(Runs_Scored_Per_Team$Runs_Per_Match, Runs_Conceded_Per_Team$Runs_Per_Match)

# correlation between wickets conceded and runs conceded
cor(wickets$Wickets_Per_Match, Runs_Conceded_Per_Team$Runs_Per_Match)

# correlation between wickets conceded and scored conceded
cor(wickets$Wickets_Per_Match, Runs_Scored_Per_Team$Runs_Per_Match)


################################################################################################
# End Correlations
################################################################################################

