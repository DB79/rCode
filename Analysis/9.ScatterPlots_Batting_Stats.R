
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

deliveries <- read_csv('deliveries.csv')

# get runs, balls and strike rate for all batsmen
batsman <- sqldf('SELECT match_id, batsman, SUM(batsman_runs) AS "runs", Count(match_id) AS "balls", cast(SUM(batsman_runs) as float) / Count(match_id) AS "sr"
                 FROM deliveries GROUP BY match_id, batsman')

# correlation of runs scored and balls faced
cor(batsman$runs,batsman$balls)


# scatter plot Runs scored vs balls faced
plot(x	=	batsman$balls,	y	=	batsman$runs, main	= "Scatter Plot of Runs Scored vs Balls Faced", xlab	=	"Balls Faced", ylab	=	"Runs Scored")

# scatter plot of innings scores per match
plot(x	=	batsman$match_id,	y	=	batsman$runs, main	= "Scatter Plot of Runs Scored Per Match by Each Batsman", xlab	=	"Match ID", ylab	=	"Runs Scored")

    