## Phillip Rodriguez-Lebron
## ST 558
## ui for Project 3
## 7-17-2019

library(shiny)

nba <- read_csv("C:/Users/Phillip/Desktop/ST 558/Projects/ST 558 Project 3/Project3/2018-2019 NBA.csv")

ui <- fluidPage(
  
  
    titlePanel("ST 558 Project 3"),
    
    sidebarLayout(
        
        sidebarPanel(
            
            conditionalPanel(condition = "input.tabs == '1'", h3("Welcome to My NBA App!")),
            
            conditionalPanel(condition = "input.tabs == '2'",
                             downloadButton("download1", "Download Full Data")),
            
            conditionalPanel(condition = "input.tabs == '3'",
                             conditionalPanel("input.tabs2 == '1'",
                                              selectInput("var", "Variable", 
                                                          choices = names(nba[sapply(nba, is.numeric)])),
                                              sliderInput("bins", "Number of Bins", min = 4, 
                                                          max = 30, value = 13),
                                              downloadButton("download2", "Save Histogram"))),
            
            conditionalPanel(condition = "input.tabs == '4'",
                             selectInput("team", "Team", choices = nba$Team),
                             downloadButton("download3", "Download Team Data"))
            
        ),
        
        mainPanel(
            tabsetPanel(type = "tabs", id = "tabs",
                        tabPanel("About", value = "1", h2("About the App"), p("Some more text")),
                        tabPanel("Scroll through Data", value = "2", dataTableOutput("table1"),
                                 id = "tabselected"),
                        tabPanel("Data Exploration", value = "3",
                                 tabsetPanel(type = "tabs", id = "tabs2",
                                             tabPanel("Histogram", plotOutput("hist"), value = '1')
                                            )),
                        tabPanel("Team Data", value = "4", dataTableOutput("table2"),
                                 id = "tabselected"),
                        tabPanel("PCA", value = "5"),
                        tabPanel("Modeling", value = "6"))
        )
    )
)