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

#   total boundaries per batsmen 
BoundariesPerBatsman  <- group_by(deliveries, batsman) %>% 
  summarise(boundaries = sum(ifelse(batsman_runs==4 | batsman_runs==6, 1, 0)))%>% 
  arrange(desc(boundaries))


hist(BoundariesPerBatsman$boundaries,	main	=	"Histogram of Boundaries per player", xlab	=	"No. of Boundaries")


# only batsmen who have scored more than 50 boundaries. 
BatsmenOver50Boundaries <- sqldf('select batsman, boundaries from BoundariesPerBatsman where boundaries >= 50 group by batsman')

hist(BatsmenOver50Boundaries$boundaries,	main	=	"Histogram of Boundaries per player", xlab	=	"No. of Boundaries")





