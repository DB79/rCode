
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

# info per player per innings. 
batsman <- sqldf('SELECT match_id, batsman, SUM(batsman_runs) AS "runs", Count(match_id) AS "balls", cast(SUM(batsman_runs) as float) / Count(match_id) AS "sr"
                 FROM deliveries GROUP BY match_id, batsman')


# overall stats per batsman
batsman1 <- group_by(batsman, batsman) %>% summarise(median_runs = median(runs)
                                                     , median_balls = median(balls)
                                                     , median_sr = median(sr)
                                                     , sum_run = sum(runs)
                                                     , avg_run = mean(runs)
                                                     , sum_balls = sum(balls)) %>% 
  mutate(strike_rate = sum_run/sum_balls)%>%
  arrange(desc(median_runs))

