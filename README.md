# polls2016
#### *Author: Kevin Soo*

Code for analyzing and visualizing polling and election data for the 2016 presidential election.

The repository contains the following:
* `president_general_polls_2016.csv` contains results from polls leading up to the election, taken from [FiveThirtyEight](http://projects.fivethirtyeight.com/2016-election-forecast/)
* `clean_polls.R` cleans this data
* `scrape_results.R` scrapes the final election results from the [BBC](http://www.bbc.co.uk/news/election/us2016/results) website
* `compare_polls_election.R` combines the polling data and election results

All these are used in `PollingError.Rmd` to produce a blog post. More to come!
