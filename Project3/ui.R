## Phillip Rodriguez-Lebron
## ST 558
## ui for Project 3
## 7-17-2019

library(shiny)
library(tidyverse)
library(DT)

nba2 <- read_csv("NBA.csv")
nba2$Season <- as.factor(nba2$Season)
nba2$Conference <- as.factor(nba2$Conference)
nba2$Team <- as.factor(nba2$Team)

ui <- fluidPage(
  
  
    titlePanel("ST 558 Project 3"),
    
    sidebarLayout(
        
        sidebarPanel(
            
            conditionalPanel(condition = "input.tabs == '1'", h3("Welcome to My NBA App!")),
            
            conditionalPanel(condition = "input.tabs == '2'",
                             selectInput("season", "Season", choices = levels(nba2$Season)),
                             selectInput("conference", "Conference", choices = c("Both", "East", "West")),
                             downloadButton("download1", "Download Data")),
            
            conditionalPanel(condition = "input.tabs == '3'",
                             uiOutput("dataTitle"),
                             conditionalPanel("input.tabs2 == '1'",
                                              selectInput("var", "Variable", 
                                                          choices = names(nba2[sapply(nba2, is.numeric)])),
                                              sliderInput("bins", "Number of Bins", min = 4, 
                                                          max = 30, value = 13),
                                              downloadButton("download2", "Save Histogram")),
                             conditionalPanel("input.tabs2 == '2'",
                                              selectInput("var1", "Variable 1:",
                                                          choices = names(nba2[sapply(nba2, is.numeric)])),
                                              selectInput("var2", "Variable 2:",
                                                          choices = names(nba2[sapply(nba2, is.numeric)])),
                                              br(),
                                              tags$style("#clickText {font-size:14px;"),
                                              div(style = "text-align:center;"),
                                              textOutput("clickText"),
                                              br(),
                                              br(),
                                              downloadButton("download3", "Save Plot")
                                              )
                             ),
            
            conditionalPanel(condition = "input.tabs == '4'",
                             selectInput("team", "Team", choices = levels(nba2$Team)),
                             downloadButton("download4", "Download Team Data")),
            conditionalPanel(condition = "input.tabs == '6'",
                             selectInput("model", "Choose Modeling Option", choices = c("kNN", "Boosted Forest")),
                             conditionalPanel(condition = "input.model == 'kNN'",
                                              selectInput("numVar", "Number of Variables", 
                                                          choices = seq(4, length(names(nba[sapply(nba, is.numeric)]))))),
                             conditionalPanel(condition = "input.model == 'Boosted Forest'",
                                              selectInput("numVar", "Number of Variables",
                                                          choices = seq(4, length(names(nba[sapply(nba, is.numeric)])))),
                                              selectInput("numTrees", "Number of Trees", choices = c(1,2,3)))
                             )
            ),
        
        mainPanel(
            tabsetPanel(type = "tabs", id = "tabs",
                        tabPanel("About", value = "1", h2("About the App"), p("Some more text")),
                        tabPanel("Scroll through Data", value = "2", dataTableOutput("table1"),
                                 id = "tabselected"),
                        tabPanel("Data Exploration", value = "3",
                                 tabsetPanel(type = "tabs", id = "tabs2",
                                             tabPanel("Histogram", plotOutput("hist"), value = '1'),
                                             tabPanel("Plot", plotOutput("plot", click = "plot_click"), 
                                                      tags$style("#plotInfo {font-size:20px;"),
                                                        div(style = "text-align:center;"),
                                                          textOutput("plotInfo"), value = '2'))
                                 ),
                        tabPanel("Team Data", value = "4", dataTableOutput("table2"),
                                 id = "tabselected"),
                        tabPanel("PCA", value = "5"),
                        tabPanel("Modeling", value = "6"))
        )
    )
)