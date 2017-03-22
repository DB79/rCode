setwd("D:/yr4/Big_Data/Project/ipl")

#install and import libraries
install.packages("ggrepel")
install.packages("readr")
install.packages("dplyr")
install.packages("sqldf")

library(sqldf)
library(ggplot2)
library(readr) 
library(dplyr)
library(ggrepel)

#read all matches from csv
allMatches = read_csv("matches.csv")

#remove any matches that produced no result or were decided by DL, this will have impact on the performance of teams
completedMatches <- sqldf('SELECT * FROM allMatches where dl_applied == 0 AND result != "no result"')


########################################################################################################################
## Analysis of completed matches ##

deliveries <-read.csv("deliveries.csv")
matches <-read.csv("matches.csv")
completedMatches <- sqldf('SELECT * FROM allMatches where dl_applied == 0 AND result != "no result"')


combinedData<-merge(deliveries, completedMatches, by.x = "match_id", by.y = "id")

#  get each team that has played a game
df_teams<-(as.data.frame(unique(combinedData$team1)))

# column name = team
colnames(df_teams)[1]<-"team"

# total wins for each team
df_wins<-combinedData %>% 
  filter(result == "normal" | result == "tie") %>% 
  group_by(winner) %>% 
  summarise(total_win = n_distinct(match_id)) 

# merge two data-frames
df_teams<-merge(df_teams, df_wins, by.x = "team", by.y = "winner")

# get total number of matches for each team
df_total_matches<-combinedData %>% 
  filter(result=="normal" | result=="tie") %>% 
  group_by(batting_team) %>% 
  summarise(total_matches = n_distinct(match_id))

# combine data frames, no. wins, total matches
df_teams<-merge(df_teams, df_total_matches, by.x = "team", by.y = "batting_team")

# get win % based on total wins and total matches
df_teams<-df_teams %>% 
  mutate(winning_perc = (total_win/total_matches)*100)


# plot win datas
df_teams %>% 
  ggplot(aes(x= reorder(team, -winning_perc), y= winning_perc))+
  geom_bar(aes(fill = team), stat = "identity")+
  labs(list(title = "Teams Performance", x = "Team", y = "Winning Percentage"))+
  scale_fill_grey()+
  theme(axis.text.x=element_text(angle=75, hjust=1), plot.title = element_text(size = 8, face = "bold"),text = element_text(size=8))

