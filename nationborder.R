ml <- nations %>% filter(NationCode=="ML") %>% arrange(Long)
head(lc)

test <- dist(select(ml, Long, Lat), method = "euclidean", upper = FALSE, p = 2)
test <- as.matrix(test)
ml$n <- sort.int(test[1,], index.return=TRUE)$ix


ml <- ml %>% arrange(n)

ggplot(filter(map2016, NationCode=="ML"), aes(Long, Lat)) +
    geom_polygon(aes(group=Group), fill="grey", colour=NA, size=.2) + 
    # geom_polygon(data=nations, aes(group=Nation), colour="black", size=2) + 
    geom_path(data=filter(ml),colour="black", size=.2) +
    coord_map("bonne", param=45) +
    scale_fill_gradient2(high=c("#3399FF"), mid="white", low=c("#FF3333")) +
    theme_void()

write.csv(ml, file="ml.csv")
sc <- read.csv("sc.csv") %>% arrange(n)


sc <- nations %>% filter(NationCode=="SC") %>% arrange(-Long)
write.csv(sc, file="sc.csv")
lc$n <- sort.int(test[1,], index.return=TRUE)$ix
