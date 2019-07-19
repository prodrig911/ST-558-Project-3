## Phillip Rodriguez-Lebron
## ST 558
## server for Project 3
## 7-17-2019

library(shiny)
library(tidyverse)
library(DT)

server <- function(input, output) {
    
    bbStats <- read_csv("C:/Users/Phillip/Desktop/ST 558/2018-2019 NBA.csv")
    nba <- bbStats[-31,]
    playoff <- data.frame(Playoff = ifelse(substring(nba$Team, 
                                                     nchar(nba$Team)) == "*", "Y", "N"))
    
    conference <- matrix(c("E", "W", "W", "E", "W", "W", "W", "E", "W",
                         "E", "W", "E", "W", "E", "E", "W", "W", "W", 
                         "E", "W", "W", "E", "W", "E", "E", "E", "E", 
                         "E", "E", "W"), nrow = 30, ncol = 1)
    
    colnames(conference) <- "Conference"
    nba <- tbl_df(cbind(nba, playoff, conference))
    nba$Team <- str_replace_all(nba$Team, "[[*]]", "")
    
    # nba$Team <- as.factor(nba$Team)
    nba$Playoff <- as.factor(nba$Playoff)
    nba$Conference <- as.factor(nba$Conference)
    
    getData <- reactive({
        
        team <- nba %>% 
                select(Team,`FG%`,`3P`,`3P%`,`2P`,`2P%`,FT,`FT%`,ORB,
                       DRB,AST,STL,BLK,TOV,PTS) %>% 
                filter(Team == input$team)
    })
    
    output$table1 <- renderDataTable({
        
        nba %>% select(Team,`FG%`,`3P`,`3P%`,`2P`,`2P%`,FT,`FT%`,ORB,
                       DRB,AST,STL,BLK,TOV,PTS)
    })
    
    output$table2 <- renderDataTable({
        
        getData()
    })
    
    output$hist <- renderPlot({
        
        hist(nba$PTS, main = "Histogram of Points", xlab = "Points",
             col = "cornflowerblue")
        
    })
    
    output$download1 <- downloadHandler(
        
        filename = function(){
            
            paste("2018-2019 NBA Statistics", ".csv", sep = "")
        },
        
        content = function(file){
            
            write.csv(nba, file)
        }
        
    )
    
    output$download2 <- downloadHandler(
        
        
        filename = function(){
            
            paste("histogram", ".png", sep = "")
        },
        
        content = function(file){
            
            png(file)
            dev.off()
        }
        
    )
    
    
    output$download3 <- downloadHandler(
            
        filename = function(){

            paste(input$team, ".csv", sep = "")
        },

        content = function(file){

            write.csv(getData(), file)
        }

    )
    
    
    
}