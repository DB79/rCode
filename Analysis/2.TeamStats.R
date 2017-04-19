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

# get total matches where team is team1 and team is team2
total_team1_matches_per_team <- sqldf('select team1 as "team", count(id) as "matches" from matches group by team1')
total_team2_matches_per_team <- sqldf('select team2 as "team", count(id) as "matches" from matches group by team2')

# combine both sets above th get total numbuer of games per team

total_matches_per_team <- sqldf('
  select team,sum(matches) total
  from
  (
    select team,matches
    from total_team1_matches_per_team
    union all
    select team,matches
    from total_team2_matches_per_team
  ) t
  group by team')


# combine data frames, no. wins, total matches
df_teams<-merge(df_teams, total_matches_per_team, by.x = "team", by.y = "team")


# get win % based on total wins and total matches
df_teams<-df_teams %>% 
  mutate(winning_perc = (total_win/total_matches_per_team$total)*100)


ggplot(df_teams, aes(x= reorder(team, winning_perc), y= winning_perc))+
  geom_bar(fill = 'grey', stat = "identity")+
  theme_classic()+
  ggtitle('Winning Percent per Team')+
  labs(x = "Team", y = "Winning Percentage")+
  scale_fill_grey()+
  coord_flip()+
  geom_text(aes(label = winning_perc, y = winning_perc),
            size = 3,  position = position_dodge(0.9), vjust = 0)
  

