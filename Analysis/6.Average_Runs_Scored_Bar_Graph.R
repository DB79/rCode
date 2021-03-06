
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
matches <- read_csv('matches.csv')

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

# total runs per team

Runs_Scored_Per_Team <- sqldf('select sum(total_runs) AS "total_runs", batting_team from deliveries group by batting_team order by 1 DESC')

Runs_Scored_Per_Team <- merge(Runs_Scored_Per_Team, total_matches_per_team, by.x = "batting_team", by.y = "team")

Runs_Scored_Per_Match_Per_Team <- sqldf('select batting_team, (cast(total_runs as float)/total_matches) as "Runs_Per_Match" from Runs_Scored_Per_Team group by batting_team')

Runs_Scored_Per_Team <- merge(Runs_Scored_Per_Team, Runs_Scored_Per_Match_Per_Team, by.x = "batting_team", by.y = "batting_team")


##########################################################################################
# Scatter Plot of Runs Scored Vs Winning %
##########################################################################################

png(filename="Images/Scatter_Plot_Winning_Percentage_Vs_Runs_Scored.png")

plot(df_teams$winning_perc,
     Runs_Scored_Per_Team$Runs_Per_Match,
     xlab = "Winning Percentage",
     ylab = "Avaerage Runs Scored",
     main = "Scatter Plor of Average Runs Scored Vs Winning %")

dev.off()

##########################################################################################


ggplot(Runs_Scored_Per_Team, aes(x= reorder(batting_team, Runs_Per_Match), y= Runs_Per_Match))+
  geom_bar(fill = 'grey', stat = "identity")+
  theme_classic()+
  ggtitle('Average Runs Scored By Team Per Game')+
  coord_flip()+ 
  labs(x='Batting Team', y='Average Runs Scored')+
  geom_text(aes(label = Runs_Per_Match, y = Runs_Per_Match),
           size = 3,  position = position_dodge(0.9), vjust = 0)
