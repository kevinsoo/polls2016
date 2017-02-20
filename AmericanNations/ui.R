# load libraries
library(shiny)
library(ggplot2)

shinyUI(fluidPage(
    # input
    fluidRow(
        column(4, selectInput("unit", "Show results:",
                              choices = c("Counties", "States", "American Nations"))),
        column(4, selectInput("outlines", "Show borders:",
                              choices = c("States", "American Nations", "None")))
    ),
    
    # plot
    mainPanel(plotOutput("electionMap"))
))