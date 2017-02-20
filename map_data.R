##########################################
#### This cleans county-level mapping data
#### Author: Kevin Soo
##########################################

# libraries
library(tidyverse)
library(maps)
library(geosphere)
library(RColorBrewer)
library(scales)

# get fips codes
fips <- county.fips
fips$state <- NA
fips$county <- NA
for (i in 1:nrow(fips)) {
    fips$state[i] <- unlist(strsplit(as.character(fips$polyname[i]), "[,]"))[1]
    fips$county[i] <- unlist(strsplit(as.character(fips$polyname[i]), "[,]"))[2]
}
fips$county <- gsub(" ","", fips$county)
fips$county <- gsub("[.]","", fips$county)
fips <- select(fips, -polyname)
colnames(fips) <- c("CountyFIPS", "state", "county")

# weird counties
fips <- fips %>% filter(CountyFIPS!=12091, CountyFIPS!=22099, CountyFIPS!=37053,
                        CountyFIPS!=48167, CountyFIPS!=51001, CountyFIPS!=53053, CountyFIPS!=53055)
fips <- rbind(fips, c(12091, "florida", "okaloosa"))
fips <- rbind(fips, c(22099, "louisiana", "stmartin"))
fips <- rbind(fips, c(37053, "north carolina", "currituck"))
fips <- rbind(fips, c(48167, "texas", "galveston"))
fips <- rbind(fips, c(51001, "virginia", "accomack"))
fips <- rbind(fips, c(53053, "washington", "pierce"))
fips <- rbind(fips, c(53055, "washington", "sanjuan"))

# get county map data
counties <- map_data("county")
colnames(counties) <- c("Long", "Lat", "Group", "Order", "state", "county")
counties$county <- gsub(" ","", counties$county)
counties$county <- gsub("[.]","", counties$county)
counties$CountyFIPS <- NA

# merge data
for (i in 1:nrow(counties)) {
    state.tmp <- counties$state[i]
    county.tmp <- counties$county[i]
    counties$CountyFIPS[i] <- filter(fips, state==state.tmp, county==county.tmp)$CountyFIPS
}

# yellowstone national park is its own county for some reason
counties <- counties %>% filter(county!="yellowstonenational")

# read spreadsheet of county-level results
df <- read_csv("results_by_county-nations_2008-2016.csv") %>% filter(StateCode!="AK", StateCode!="HI")
df$County <- as.factor(df$County)
df$StateCode <- as.factor(df$StateCode)
df$State <- as.factor(df$State)
df$NationCode <- as.factor(df$NationCode)
df$Nation <- as.factor(df$Nation)

# calculate percentages by county
df$pDem_2008 <- 100*df$Dem_2008/df$Total_2008
df$pGOP_2008 <- 100*df$GOP_2008/df$Total_2008
df$diff_2008 <- 100*(df$Dem_2008-df$GOP_2008)/df$Total_2008
df$pDem_2012 <- 100*df$Dem_2012/df$Total_2012
df$pGOP_2012 <- 100*df$GOP_2012/df$Total_2012
df$diff_2012 <- 100*(df$Dem_2012-df$GOP_2012)/df$Total_2012
df$pDem_2016 <- 100*df$Dem_2016/df$Total_2016
df$pGOP_2016 <- 100*df$GOP_2016/df$Total_2016
df$diff_2016 <- 100*(df$Dem_2016-df$GOP_2016)/df$Total_2016

# calculate changes from 2012 to 2016
df$change_2016 <- df$diff_2016-df$diff_2012

# get state- and nation-level difference
states <- df %>% group_by(State) %>% 
    summarise(Dem=sum(Dem_2016), GOP=sum(GOP_2016), Total=sum(Total_2016), Diff=100*(Dem-GOP)/Total)
nations <- df %>% group_by(Nation) %>%
    summarise(Dem=sum(Dem_2016), GOP=sum(GOP_2016), Total=sum(Total_2016), Diff=100*(Dem-GOP)/Total)
df$diffState_2016 <- NA
df$diffNation_2016 <- NA
for (i in 1:nrow(df)) {
    df$diffState_2016[i] <- filter(states, State==df$State[i])$Diff
    df$diffNation_2016[i] <- filter(nations, Nation==df$Nation[i])$Diff
}

# save df for analyses - this is the key data frame, use for stats
county2016 <- df
save(county2016, file="county2016.Rda")

# merge with county data - this is the data for mapping
map2016 <- merge(counties, df, by="CountyFIPS") %>% select(-state, -county)
map2016$CountyFIPS <- as.factor(map2016$CountyFIPS)
map2016$Winner_2016 <- as.factor(map2016$Winner_2016)
save(map2016, file="map2016.Rda")

########## get outline for maps
load("map2016.Rda")

# get outline for American Nations
load("americanNations.Rda")

# get outlines of country/states for plotting
country <- map_data("usa") # us border
states <- map_data("state") # state borders

############ create plots
ggplot(map2016, aes(Long, Lat)) +
    geom_polygon(aes(group=Group, fill=diff_2016), colour="white", size=.1) + 
    geom_polygon(data=country, aes(long, lat, group=group), fill=NA, colour = "black", size=.3) + 
    # geom_polygon(data=states, aes(long, lat, group=group), fill=NA, colour = "black", size=.3) + 
    # geom_polygon(data=filter(map2016, Nation %in% c("Federal Entity", "Yankeedom-Midlands", "New France-Deep South")), aes(group=Group), fill=NA, colour="black", size=.3) + 
    geom_path(data=americanNations, aes(group=NationGroup), colour="black", size=.3) + 
    coord_map("bonne", param=45) +
    scale_fill_gradient2(name="Margin of\nvictory (%)", high=c("#3399FF"), mid="white", low=c("#FF3333")) +
    theme_void() +
    labs(title = "Difference in vote-share by county",
         subtitle = "Borders of American Nations outlined")

# ggplot(map2016, aes(Long, Lat)) +
#     geom_polygon(aes(group=Group, fill=Nation), colour = "white", size=.1) + 
#     coord_map("bonne", param=45) +
#     theme_void()

head(county2016)
ggplot(county2016) +
    geom_point(aes(x=Total_2016, y=diff_2016, color=Winner_2016)) +
    facet_wrap(~ State, ncol = 10) +
    scale_color_manual(values=c("#3399FF", "#FF3333")) +
    theme_minimal()

############## regression models
library(lme4)
library(lmerTest)
library(rstanarm)
options(mc.cores = parallel::detectCores())

model <- lm(data=county2016)


ggplot(map2016, aes(Long, Lat)) +
    geom_polygon(aes(group=Group, fill=diff_2016), colour="white", size=.1) + 
    geom_polygon(data=filter(map2016, Nation %in% c("Federal Entity", "Yankeedom-Midlands", "New France-Deep South")), aes(group=Group), fill=NA, colour="black", size=.3) + 
    geom_path(data=americanNations, aes(group=NationGroup), colour= "black", size=.5) + 
    geom_polygon(data=country, aes(long, lat, group=group), fill=NA, colour = "black", size=.5) +
    coord_map("bonne", param=45) +
    scale_fill_gradient2(name="Margin of\nvictory (%)", high=c("#3399FF"), mid="white", low=c("#FF3333")) +
    theme_void() +
    labs(title = "Borders of American Nations shown")



ggplot(map2016, aes(Long, Lat)) +
    geom_polygon(aes(group=Group, fill=diff_2016), colour="white", size=.1) + 
    geom_polygon(data=filter(map2016, Nation %in% c("Federal Entity", "Yankeedom-Midlands", "New France-Deep South")), aes(group=Group), fill=NA, colour="black", size=.3) + 
    coord_map("bonne", param=45) +
    scale_fill_gradient2(name="Margin of\nvictory (%)", high=c("#3399FF"), mid="white", low=c("#FF3333")) +
    theme_void() + geom_path(data=americanNations, aes(group=NationGroup), colour= "black", size=.5) +
    geom_polygon(data=country, aes(long, lat, group=group), fill=NA, colour = "black", size=.5)
