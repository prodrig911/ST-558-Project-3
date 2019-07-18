## Phillip Rodriguez-Lebron
## ST 558
## ui for Project 3
## 7-17-2019

library(shiny)

ui <- fluidPage(
    
    titlePanel("ST 558 Project 3"),
    
    sidebarLayout(
        
        sidebarPanel(),
        
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("About", h2("About the App"), p("Some more text")),
                        tabPanel("Data Exploration"),
                        tabPanel("PCA"),
                        tabPanel("Modeling"),
                        tabPanel("Scroll through Data"))
        )
    )
)