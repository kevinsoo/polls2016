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

########## get outline for nations

# get outlines of country/states for plotting
country <- map_data("usa") # us border
states <- map_data("state") # state borders

# nation borders - locate duplicate points in different nations
nations <- map2016 %>% group_by(Long, Lat) %>% summarise(N=n()) %>% arrange(-N) %>% filter(N>0)
map2016$nationBorder <- 0
map2016$countryBorder <- 0
for (i in 1:nrow(map2016)) {
    # identify points of intersection
    if ((map2016$Long[i] %in% nations$Long)&&(map2016$Lat[i] %in% nations$Lat)) {
        tmp <- filter(map2016, Long==Long[i], Lat==Lat[i])
        # identify if those intersection points are across nations
        if (length(unique(tmp$Nation))>1) {
            map2016$nationBorder[i] <- 1
        }
    }
    # identify points from country border
    if ((map2016$Long[i] %in% country$long)&&(map2016$Lat[i] %in% country$lat)) {
        map2016$countryBorder[i] <- 1
    }
    print(i/nrow(map2016))
}

# save points in map that are nation borders
nations <- map2016 %>% filter(nationBorder==1) %>% 
    select(Long, Lat, County, StateCode, State, NationCode, Nation, Group, Order)
save(nations, file="nations.Rda")
write.csv(nations, file="nations.csv")











############ create plots
ggplot(map2016, aes(Long, Lat)) +
    geom_polygon(aes(group=Group, fill=diff_2016), color="white", size=.1) + 
    # geom_polygon(data=states, aes(long, lat, group=group), fill=NA, colour = "black", size=.2) + 
    geom_polygon(data=country, aes(long, lat, group=group), fill=NA, colour = "black", size=.2) + 
    geom_path(data=nations, aes(Long, Lat, group=Group), colour = "black", size=.2) + 
    coord_map("bonne", param=45) +
    scale_fill_gradient2(high=c("#3399FF"), mid="white", low=c("#FF3333")) +
    theme_void()

nations <- nations %>% arrange(Nation, Group, Order)
nations %>% group_by(Long, Lat) %>% summarise(N=n()) %>% arrange(-N)






ggplot(filter(map2016, Nation %in% c("Deep South", "New France", "New France-Deep South")), aes(Long, Lat)) +
    geom_polygon(aes(group=Group, fill=Nation, alpha=diff_2016), colour = "white", size=.1) + 
    coord_map("bonne", param=45) +
    theme_void()

    ggplot(map2016, aes(Long, Lat)) +
    geom_polygon(aes(group=Group, fill=Nation), colour = "white", size=.1) + 
    coord_map("bonne", param=45) +
    theme_void()




ggplot(map2016, aes(Long, Lat)) +
    geom_polygon(aes(group=Group, fill=diff_2016), colour=NA, size=.2) + 
    # geom_polygon(data=nations, aes(group=Nation), colour="black", size=2) + 
    geom_path(data=leftcoast, aes(), colour="black", size=.2) + 
    coord_map("bonne", param=45) +
    scale_fill_gradient2(high=c("#3399FF"), mid="white", low=c("#FF3333")) +
    theme_void()
