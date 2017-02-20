# load libraries
library(shiny)
library(ggplot2)
library(mapproj)
library(maps)

# load maps
load("map2016.Rda") # counties
load("americanNations.Rda") # american nations
load("country.Rda") # us border
load("states.Rda") # state borders

shinyServer(function(input, output) {
    
    output$electionMap <- renderPlot({
        
        # color results on base map
        if(input$unit=="Counties") {
            p0 <- ggplot(map2016, aes(Long, Lat)) +
                geom_polygon(aes(group=Group, fill=diff_2016), colour="white", size=.1) + 
                coord_map("bonne", param=45) +
                scale_fill_gradient2(name="Margin of\nvictory (%)", high=c("#3399FF"), mid="white", low=c("#FF3333"), limits=c(-92, 92)) +
                theme_void()
        }
        else if(input$unit=="States") {
            p0 <- ggplot(map2016, aes(Long, Lat)) +
                geom_polygon(aes(group=Group, fill=diffState_2016), colour=NA, size=0) + 
                coord_map("bonne", param=45) +
                scale_fill_gradient2(name="Margin of\nvictory (%)", high=c("#3399FF"), mid="white", low=c("#FF3333"), limits=c(-92, 92)) +
                theme_void()
        }
        else if(input$unit=="American Nations") {
            p0 <- ggplot(map2016, aes(Long, Lat)) +
                geom_polygon(aes(group=Group, fill=diffNation_2016), colour=NA, size=0) + 
                coord_map("bonne", param=45) +
                scale_fill_gradient2(name="Margin of\nvictory (%)", high=c("#3399FF"), mid="white", low=c("#FF3333"), limits=c(-92, 92)) +
                theme_void()
        }
        
        # plot borders
        if(input$outlines=="States") { 
            p1 <- p0 + geom_polygon(data=states, aes(long, lat, group=group), fill=NA, colour = "black", size=.5)
        }
        else if(input$outlines=="American Nations") { 
            p1 <- p0 + 
                geom_path(data=americanNations, aes(group=NationGroup), colour= "black", size=.5) +
                geom_polygon(data=country, aes(long, lat, group=group), fill=NA, colour = "black", size=.5)
        }
        # else if(input$outlines=="Both") { 
        #     p1 <- p0 + 
        #         geom_path(data=americanNations, aes(group=NationGroup), colour= "black", size=.7) + 
        #         geom_polygon(data=country, aes(long, lat, group=group), fill=NA, colour = "black", size=.5) +
        #         geom_polygon(data=states, aes(long, lat, group=group), fill=NA, colour = "black", linetype="dotted", size=.4)
        # }
        else if(input$outlines=="None") { p1 <- p0 + geom_polygon(data=country, aes(long, lat, group=group), fill=NA, colour = "black", size=.5) }
        p1
    })
})