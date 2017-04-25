
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
matches_per_player <- sqldf('select batsman, 
                                    count(DISTINCT(match_id)) as "total_matches" 
                                    from deliveries group by batsman')


# get runs, balls and strike rate for all batsmen
batsman <- sqldf('SELECT batsman, SUM(batsman_runs) AS "runs", Count(match_id) AS "balls"
                 FROM deliveries GROUP BY batsman')

batsman_overall_stats <- merge(batsman, matches_per_player, by.x = "batsman", by.y = "batsman")


Boundaries_Per_Batsman  <- group_by(deliveries, batsman) %>% 
  summarise(boundaries = sum(ifelse(batsman_runs==4 | batsman_runs==6, 1, 0)))%>% 
  arrange(desc(boundaries))

batsman_overall_stats <- merge(batsman_overall_stats, Boundaries_Per_Batsman, by.x = "batsman", by.y = "batsman")


batsman_stats_per_match <- sqldf('select batsman,
                                         (runs/total_matches) as "Runs_Per_Match",
                                         (balls/total_matches) as "Balls_Faced_Per_Match",
                                         (boundaries/total_matches) as "Boundaries_Per_Match"
                                         from batsman_overall_stats group by batsman')

sr = sqldf('select batsman, ((cast(Runs_Per_Match as float)/Balls_Faced_Per_Match )*100) as "strike_rate" from batsman_stats_per_match group by batsman')

batsman_stats_per_match <- merge(batsman_stats_per_match, sr, by.x = "batsman", by.y = "batsman")


###########################################################################################################
# Boxplot of Average Number of Balls Faced
###########################################################################################################


png(filename="Images/Boxplot_Average_Number_Of_Balls_Faced.png")

boxplot(batsman_stats_per_match$Balls_Faced_Per_Match ,main="Boxplot of Balls Faced Per Match Per Player", ylab="Average Number of Balls Faced")
dev.off()

summary(batsman_stats_per_match$Balls_Faced_Per_Match)

###########################################################################################################
# Scatter Plot of Runs Scored vs Balls Faced
###########################################################################################################

png(filename="Images/Scatter_Plot_Runs_Scored_Vs_Balls_Faced.png")

plot(batsman_stats_per_match$Balls_Faced_Per_Match,
     batsman_stats_per_match$Runs_Per_Match,
     main = "Scatter Plot of Runs Scored vs Balls Faced",
     xlab = "Average Balls Faced",
     ylab = "Average Runs Scored")
dev.off()

cor(batsman_stats_per_match$Runs_Per_Match, batsman_stats_per_match$Balls_Faced_Per_Match)

###########################################################################################################
# Scatter Plot of Strike Rate vs Balls Faced
###########################################################################################################

png(filename="Images/Scatter_Plot_Strike_Vs_Balls_Faced.png")

plot(batsman_stats_per_match$Balls_Faced_Per_Match,
     batsman_stats_per_match$strike_rate,
     main = "Scatter Plot of Strike Rate vs Balls Faced",
     xlab = "Average Balls Faced",
     ylab = "Average Strike Rate")
dev.off()

cor(batsman_stats_per_match$Balls_Faced_Per_Match, batsman_stats_per_match$strike_rate)

###########################################################################################################
# Scatter Plot of No. of Boundaries vs Balls Faced
###########################################################################################################


png(filename="Images/Scatter_Plot_Boundaries_Vs_Balls_Faced.png")

plot(batsman_stats_per_match$Balls_Faced_Per_Match,
     batsman_stats_per_match$Boundaries_Per_Match,
     main = "Scatter Plot of Boundaries Scored vs Balls Faced",
     xlab = "Average Balls Faced",
     ylab = "Average Boundaries")
dev.off()

cor(batsman_stats_per_match$Balls_Faced_Per_Match, batsman_stats_per_match$Boundaries_Per_Match)

###########################################################################################################
# Scatter Plot of No. of Boundaries vs Strike Rate
###########################################################################################################


png(filename="Images/Scatter_Plot_Boundaries_Vs_Strike_Rate.png")

plot(batsman_stats_per_match$Boundaries_Per_Match,
     batsman_stats_per_match$strike_rate,
     main = "Scatter Plot of Strike Rate vs Boundaries",
     xlab = "Average Boundaries",
     ylab = "Average Strike Rate")
dev.off()

cor(batsman_stats_per_match$Balls_Faced_Per_Match, batsman_stats_per_match$Boundaries_Per_Match)






#boxplot(Boundaries_Per_Batsman_Per_Match$Boundaries_Per_Match ,main="Boxplot of Average Boundaries Per Match Per Player", ylab="Average Boundaries")






#boxplot of average number of balls faced per match
#boxplot(batsman_stats_per_match$Balls_Faced_Per_Match ,main="Boxplot of Balls Faced Per Match Per Player", ylab="Average Number of Balls Faced")

#boxplot of average strike rate per match
#boxplot(batsman_stats_per_match$strike_rate ,main="Boxplot of Average Strike Rate Per Match Per Player", ylab="Average Strike Rate")

