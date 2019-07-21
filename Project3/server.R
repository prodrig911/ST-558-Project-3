## Phillip Rodriguez-Lebron
## ST 558
## server for Project 3
## 7-17-2019

library(shiny)
library(tidyverse)
library(DT)
library(caret)
library(class)

# nba <- read_csv("C:/Users/Phillip/Desktop/NBA/2018-2019 NBA.csv")
nba2 <- read_csv("NBA.csv")

server <- function(input, output) {
    
    getData1 <- reactive({
      
      if (input$conference == "Both"){
        teams <- nba2 %>% filter(Season == input$season) %>% select(Team:PTS)
      } else{
      teams <- nba2 %>% filter(Season == input$season, Conference == input$conference) %>% select(Team:PTS)
      }
    })
    
    getData2 <- reactive({
      
        team <- nba2 %>% filter(Team == input$team) %>% select(Team:PTS, Season)
    })
    
    output$table1 <- renderDataTable({
      
        getData1()
    })
    
    output$table2 <- renderDataTable({
        
        getData2()
    })
    
    output$hist <- renderPlot({
        
        a <- input$var
        b <- pull(getData1(),a)
        bins <- seq(min(b),max(b),length.out = input$bins + 1)
        
        if (input$conference == "Both"){
          
          hist(b, main = paste0("Histogram of ", a, " for ", input$season, " NBA Season"), 
               xlab = a, col = "cornflowerblue", breaks = bins)
        } else {
            hist(b, main = paste0("Histogram of ", a, " for ", input$season, " NBA Season ", 
                                  input$conference, "ern Conference"), xlab = a, col = "cornflowerblue", breaks = bins)
        }
    })
    
    p <- reactive({
        
        a <- input$var
        b <- pull(getData1(),a)
        bins <- seq(min(b),max(b),length.out = input$bins + 1)
        
        if (input$conference == "Both"){
          
          hist(b, main = paste0("Histogram of ", a, " for ", input$season, " NBA Season"), 
               xlab = a, col = "cornflowerblue", breaks = bins)
        } else {
          hist(b, main = paste0("Histogram of ", a, " for ", input$season, " NBA Season ", 
                                input$conference, "ern Conference"), xlab = a, col = "cornflowerblue", breaks = bins)
        }
    })
    
    output$download1 <- downloadHandler(
        
        filename = function(){
            
            if (input$conference == "Both"){
            
              paste(input$season, " NBA Statistics", ".csv", sep = "")
            } else {
              
                paste(input$season, " ", input$conference, "ern Conference NBA Statistics", ".csv", sep = "")
            }
        },
        
        content = function(file){
            
            write.csv(getData1(), file)
        }
        
    )
    
    output$download2 <- downloadHandler(
        
        
        filename = function(){
                    
          if (input$conference == "Both"){
            paste("Histogram of ", input$var, " for ", input$season, " NBA Season", ".png", sep = "")
          } else{
            paste("Histogram of ", input$var, " for ", input$season, " NBA Season ", input$conference,
                  "ern Conference", ".png", sep = "") 
          }
        }
          ,
        
        content = function(file){
          
            png(file)
            p()
            dev.off()
        }
        
    )
    
    
    output$download4 <- downloadHandler(
            
        filename = function(){

            paste(input$team, " 2015-2016 to 2018-2019 NBA Statistics", ".csv", sep = "")
        },

        content = function(file){

            write.csv(getData2(), file)
        }

    )
    
    output$plot <- renderPlot({
     
     ggplot(getData1(), aes_string(as.name(input$var1) , as.name(input$var2))) + geom_smooth() + geom_point(size = 3.5)
      
    })
    
    output$plotInfo <- renderText({
      
      if(is.null(input$plot_click)) return("")
      paste0("Coordinates for Plot: x = ", round(input$plot_click$x, 2), ", y = ", round(input$plot_click$y, 2))
    })
    
    output$clickText <- renderText({
      
      "Click Anywhere on the Plot for X and Y Coordinates"
    })
    
    output$dataTitle <- renderUI({
      
      if (input$conference == "Both"){
        text <- paste0(input$season, " NBA Season Statistics", sep = "")
        h3(text)
      } else{
        text <- paste0(input$season, " NBA Season ", input$conference, "ern Conference Statistics", sep = "")
        h3(text)
      }
    })
    
    output$download3 <- downloadHandler(
      
      
      filename = function(){
        
        paste("Variable Plot", ".png", sep = "")
      },
      
      content = function(file){
        
        ggsave(file, width = 16, height = 9, dpi = 100)
      }
      
    )
    
    
    
}