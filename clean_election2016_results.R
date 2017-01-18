##############################################################
#### This cleans final 2016 election data from David Wasserman
#### Author: Kevin Soo
##############################################################

# load libraries
library(tidyverse)

# load data
results2016 <- read.csv("results_by_state_2016.csv")

# compute vote-share
Clinton <- (results2016$Clinton/results2016$Total)*100
Trump <- (results2016$Trump/results2016$Total)*100
Others <- (results2016$Others/results2016$Total)*100
State <- results2016$State
voteShare <- data.frame(State, Clinton, Trump, Others)

# long format for results
election <- results2016 %>% gather(Candidate, Votes, Clinton:Others) %>% arrange(State, Candidate)
Level <- voteShare %>% gather(Candidate, Level, Clinton:Others) %>% arrange(State, Candidate) %>% select(Level)
election <- data.frame(election, Level)

# save data
save(election, file="election.Rda")
