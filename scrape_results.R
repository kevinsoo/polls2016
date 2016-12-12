###############################################################
#### This scrapes and cleans 2016 election results from bbc.com
#### Author: Kevin Soo
###############################################################

# load libraries
library(tidyverse)
library(stringr)
library(rvest)

# get names of states
states <- read_html("http://www.bbc.com/news/world/us_and_canada/states") %>%
    html_nodes(".gel-long-primer-bold") %>% html_text()

# get links of all pages
links <- read_html("http://www.bbc.com/news/world/us_and_canada/states") %>%
    html_nodes(".gel-long-primer-bold") %>% html_attr('href')
links <- paste("http://www.bbc.com", links, sep="")

# directory of states
bbc <- data.frame(states, links)

# get data from each page
for (i in 1:nrow(bbc)) {
    stateResult <- read_html(as.character(bbc$links[i])) %>% 
        html_nodes('.us2016-popular-vote__votes , .us2016-popular-vote__candidate-name') %>%
        html_text()
    
    # get data from page into right format
    stateResult <- matrix(stateResult, ncol=2, nrow=3, byrow=T)
    colnames(stateResult) <- c("Candidate", "vote")
    stateResult <- data.frame(stateResult)
    stateResult$Votes <- NA
    
    # clean 'votes'
    for (j in 1:nrow(stateResult)) {
        stateResult$Votes[j] <- as.numeric(gsub(",", "", str_extract_all(stateResult$vote[j],"\\(?[0-9,.]+\\)?")[[1]]))
    }
    
    # state results
    State <- rep(bbc$states[i], nrow(stateResult))
    stateResult <- data.frame(State, select(stateResult, -vote))
    
    # save results
    if (i==1) { election <- stateResult }
    else { election <- rbind(election, stateResult) }
}

# compute national popular vote
us <- election %>% group_by(Candidate) %>% summarise(Votes=sum(Votes))
State <- rep("U.S.", nrow(us))
us <- data.frame(State, us)
election <- rbind(us, election)

# compute vote total for each state
totals <- election %>% group_by(State) %>% summarise(Total=sum(Votes))

# compute vote share
election$Level <- NA
for (i in 1:nrow(election)) {
    stateTotal <- totals %>% filter(State==election$State[i]) %>% select(Total)
    election$Level[i] <- (election$Votes[i]/stateTotal)*100
}

# final checks
election$Level <- as.numeric(election$Level)

# save data
save(election, file="election.Rda")
