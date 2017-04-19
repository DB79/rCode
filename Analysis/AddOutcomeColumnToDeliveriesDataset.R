
setwd("D:/yr4/Big_Data/Project/ipl")

deliveries <- read.csv("deliveries.csv")


##ad new outcomes field, possible outcomes 0,1,2,3,4,5,6,7,W
deliveries$outcome[deliveries$total_runs == 0] <- 0
deliveries$outcome[deliveries$total_runs == 1] <- 1
deliveries$outcome[deliveries$total_runs == 2] <- 2
deliveries$outcome[deliveries$total_runs == 3] <- 3
deliveries$outcome[deliveries$total_runs == 4] <- 4
deliveries$outcome[deliveries$total_runs == 5] <- 5
deliveries$outcome[deliveries$total_runs == 6] <- 6
deliveries$outcome[deliveries$total_runs == 7] <- 7
deliveries$outcome[deliveries$player_dismissed != ""] <- 'W'









