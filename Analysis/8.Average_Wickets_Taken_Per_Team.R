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
matches <- read_csv('matches.csv')

# get total number of wickets per team
wickets <- sqldf('select bowling_team, count(*) as "Total_Wickets_Taken" from deliveries where dismissal_kind != "" group by bowling_team')

#calculate total matches played per team and add to wickets 
total_team1_matches_per_team <- sqldf('select team1 as "team", count(id) as "matches" from matches group by team1')
total_team2_matches_per_team <- sqldf('select team2 as "team", count(id) as "matches" from matches group by team2')
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

wickets <- merge(wickets, total_matches_per_team, by.x = "bowling_team", by.y = "team")

#calculate wickets per match and add new column to wickets dataset
wickets_per_match <- sqldf('select bowling_team ,(cast(Total_Wickets_Taken as float)/total_matches) as "Wickets_Per_Match" from wickets group by bowling_team')

wickets <- merge(wickets, wickets_per_match, by.x = "bowling_team", by.y = "bowling_team")

##########################################################################################
# Scatter Plot of Runs Scored Vs Winning %
##########################################################################################

png(filename="Images/Scatter_Plot_Winning_Percentage_Vs_Wickets_Taken.png")

plot(df_teams$winning_perc,
     wickets$Wickets_Per_Match,
     xlab = "Winning Percentage",
     ylab = "Avaerage Wickets Taken",
     main = "Scatter Plor of Average Wickets Taken Vs Winning %")

dev.off()

cor(df_teams$winning_perc, wickets$Wickets_Per_Match)
##########################################################################################






ggplot(wickets,aes(x= reorder(bowling_team, Wickets_Per_Match), y= Wickets_Per_Match))+
  geom_bar(fill = 'grey', stat = "identity")+
  theme_classic()+
  ggtitle('Average Wickets Taken By Team Per Game')+
  coord_flip()+ 
  labs(x='Bowling Team', y='Average Wickets Taken')+
  geom_text(aes(label = Wickets_Per_Match, y = Wickets_Per_Match),
            size = 3,  position = position_dodge(0.9), vjust = 0)

