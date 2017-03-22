#Bowlers

setwd("E:/yr4/Big_Data/Project/ipl")

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


#dot balls, total balls
bowler1 <- group_by(deliveries, bowler)%>%summarise(dot_balls = sum(ifelse(batsman_runs ==0 &
                                                                             extra_runs == 0 , 1, 0 ))
                                                    , total_balls = sum(ifelse(wide_runs == 1 &
                                                                                 noball_runs == 1 
                                                                               , 0, 1 ))
                                                    , total_runs = sum(batsman_runs+extra_runs))

# bowler name to allow for creation of boxplots
bowlerStats <- sqldf('select dot_balls,total_balls, total_runs from bowler1')

boxplot(bowlerStats)

#wickets
bowler2 <- group_by(deliveries, bowler)%>%summarise(count = sum(ifelse(dismissal_kind 
                                                                       %in% c("caught", "bowled"
                                                                              , "lbw", "caught and bowled"), 1,0)))

bowler3 <- group_by(deliveries, fielder)%>%summarise(count = sum(ifelse(dismissal_kind 
                                                                        %in% c("run out", "stumped")
                                                                        & bowler != fielder, 1,0)))

#Avg runs given
bowler4 <- group_by(deliveries, bowler, match_id) %>% summarise(total_runs = sum(batsman_runs+extra_runs))%>%
  group_by(bowler)%>%summarise(avg_runs = mean(total_runs))

names(bowler3)[1] <- "bowler"

bowler <- left_join(bowler1, bowler2, by = "bowler") 
bowler <- left_join(bowler, bowler3, by = "bowler")%>%
  mutate(wickets = count.x + ifelse(is.na(count.y), 0, count.y))
bowler <- left_join(bowler, bowler4, by = "bowler")%>%
  mutate(sr = total_balls/wickets,
         economy = total_runs/(total_balls/6))%>%
  select(bowler, dot_balls, wickets, avg_runs, sr, economy)
