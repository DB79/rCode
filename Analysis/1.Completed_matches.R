setwd("D:/yr4/Big_Data/Project/ipl/InitialDatasets")

#install.packages("readr")
#install.packages("sqldf")

library(sqldf)
library(readr)

#read all matches from csv
allMatches = read_csv("matches.csv")

#remove any matches that produced no result or were decided by DL, this will have impact on the performance of teams
completedMatches <- sqldf('SELECT * FROM allMatches where dl_applied == 0 AND result != "no result"')

#write completed matches to matches.csv in Analysis folder
write.csv(completedMatches,'../Analysis/matches.csv')

#update deliveries in Analysis folder to only include completed matches
deliveries <-read.csv("deliveries.csv")
deliveriesFromCompletedMatches <- sqldf('select * from deliveries where match_id in (select id from completedMatches)')
write.csv(deliveriesFromCompletedMatches,'../Analysis/deliveries.csv')

