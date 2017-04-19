library("arules");
library("arulesViz");

patterns = random.patterns(nItems = 1000);
summary(patterns);
trans = random.transactions(nItems = 1000, nTrans = 1000, method =
                              "agrawal", patterns = patterns);
image(trans);

setwd("D:/yr4/Big_Data/Project/ipl")

deliveries.csv<- read.csv("deliveries.csv")

deliveries$outcome <- 0


deliveries$outcome[deliveries$player_dismissed != ""] <- 'W'



