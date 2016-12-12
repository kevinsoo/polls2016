###########################################################
#### This compares polling data with final election results
#### Author: Kevin Soo
#### Run after clean_polls.R and scrape_results.R
###########################################################

# load libraries
library(tidyverse)
library(stringr)
library(rvest)
theme_set(theme_bw())

# load data
load(file="election.Rda")
load(file="polls.Rda")

# make sure state names are the same
polls$State <- ifelse(polls$State=="District of Columbia", "Washington DC", as.character(polls$State))
polls$State <- as.factor(polls$State)

# filter out unique congressional districts
polls <- polls %>% 
    filter(!State %in% c("Nebraska CD-1", "Nebraska CD-2", "Nebraska CD-3", "Maine CD-1", "Maine CD-2"))

# reorder states
polls$State <- as.character(polls$State)
election$State <- as.character(election$State)
polls <- polls %>% arrange(State)
election <- election %>% arrange(State)
polls$State <- as.factor(polls$State)
election$State <- as.factor(election$State)

# match election results to poll by state
polls$Votes <- NA
polls$Election <- NA
for (i in 1:nrow(polls)) {
    polls$Votes[i] <- filter(election,
                             State==as.character(polls$State[i]), 
                             Candidate==as.character(polls$Candidate[i]))$Votes
    polls$Election[i] <- filter(election, 
                                State==as.character(polls$State[i]), 
                                Candidate==as.character(polls$Candidate[i]))$Level
}

# calculate error, difference is actual-polls, so we see how much actual results are different from the projections
polls$Error <- polls$Election - polls$Level

# bind winner
polls$Winner <- NA
for (i in 1:nrow(polls)) {
    result <- filter(election, State==polls$State[i]) %>% arrange(-Level)
    polls$Winner[i] <- as.character(result$Candidate[1])
}
polls$Winner <- as.factor(polls$Winner)

# save data
save(polls, file="polls.Rda")
