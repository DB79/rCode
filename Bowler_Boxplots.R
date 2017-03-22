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