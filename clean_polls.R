#######################################################################################
#### This cleans the 2016 polling data from FiveThirtyEight
#### http://projects.fivethirtyeight.com/general-model/president_general_polls_2016.csv
#### Author: Kevin Soo
#######################################################################################

# load libraries
library(tidyverse)

# load data
polls2016 <- read.csv("president_general_polls_2016.csv")

# set types
polls2016$forecastdate <- as.Date(polls2016$forecastdate, format="%m/%d/%y")
polls2016$startdate <- as.Date(polls2016$startdate, format="%m/%d/%y")
polls2016$enddate <- as.Date(polls2016$enddate, format="%m/%d/%y")
polls2016$createddate <- as.Date(polls2016$createddate, format="%m/%d/%y")

# get relevant data (only trump and clinton)
id <- 1:nrow(polls2016)
polls2016 <- polls2016 %>% filter(type=="polls-only") %>% 
    select(state:rawpoll_trump, adjpoll_clinton, adjpoll_trump, poll_id:question_id)
polls2016 <- data.frame(id, polls2016)

# long format
polls2016 <- polls2016 %>% gather(poll, level, rawpoll_clinton:adjpoll_trump) %>% arrange(id, poll)
polls2016$candidate <- NA
polls2016$type <- NA
for (i in 1:nrow(polls2016)) { 
    tmp <- str_split(polls2016$poll[i], "_")[[1]]
    polls2016$candidate[i] <- tmp[2]
    polls2016$type[i] <- tmp[1]
}

# final checks
polls <- polls2016 %>% select(id:poll_wt, type, candidate, level)
polls$type <- ifelse(polls$type=="adjpoll", "Adjusted", "Raw")
polls$candidate <- ifelse(polls$candidate=="clinton", "Clinton", "Trump")
polls$type <- as.factor(polls$type)
polls$candidate <- as.factor(polls$candidate)
colnames(polls) <- c("ID", "State", "Start", "End", "Pollster", "Grade", "SampleSize", "Population", "PollWeight", "Type", "Candidate", "Level")

# save data frame
save(polls, file="polls.Rda")
