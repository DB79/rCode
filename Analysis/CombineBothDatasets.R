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

matches <- read_csv("matches.csv");

combineddata <- merge(x=deliveries, y=matches, by.x='match_id', by.y='id')