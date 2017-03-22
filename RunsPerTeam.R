
setwd("D:/yr4/Big_Data/Project/ipl")


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


# total runs per team
RunsPerTean <- sqldf('select sum(total_runs) AS "total_runs", batting_team from deliveries group by batting_team order by 1 DESC')





pp <- ggplot(RunsPerTean, aes(x = RunsPerTean$batting_team, y = RunsPerTean$total_runs,
                    fill = RunsPerTean$batting_team)) + 
  geom_bar(width = 1, stat = "identity", position = "dodge") +
  scale_fill_grey()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  geom_text(aes(label = RunsPerTean$total_runs, y = RunsPerTean$total_runs),
            size = 3,  position = position_dodge(0.9), vjust = 0)
plot(pp)







