## Phillip Rodriguez-Lebron
## ST 558
## server for Project 3
## 7-17-2019

library(shiny)
library(tidyverse)
library(DT)
library(caret)

# nba <- read_csv("C:/Users/Phillip/Desktop/NBA/2018-2019 NBA.csv")
nba2 <- read_csv("NBA.csv")
nba2$Playoff <- as.factor(nba2$Playoff)

server <- function(input, output) {
    
    getData1 <- reactive({
      
      if (input$conference == "Both"){
        teams <- nba2 %>% filter(Season == input$season) %>% select(Team:PTS)
      } else{
      teams <- nba2 %>% filter(Season == input$season, Conference == input$conference) %>% select(Team:PTS)
      }
    })
    
    getData2 <- reactive({
      
        team <- nba2 %>% filter(Team == input$team) %>% select(Season, Team, !!!input$varStats2)
    })
    
    getData3 <- reactive({
      
      if (input$conference == "Both"){
        teams <- nba2 %>% filter(Season == input$season) %>% select(Team, !!!input$varStats)
      } else{
        teams <- nba2 %>% filter(Season == input$season, Conference == input$conference) %>% select(Team, !!!input$varStats)
      }
    })
    
    
    
    output$table1 <- renderDataTable({
      
        getData3()
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
            
            write.csv(getData3(), file)
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
     
     ggplot(getData1(), aes_string(as.name(input$var1) , as.name(input$var2))) + 
        geom_smooth() + geom_point(size = 3.5) + ggtitle(paste("Plot of ", input$var1, " by ", input$var2)) + 
        theme(plot.title = element_text(hjust = 0.5))
      
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
        
        paste("Plot of ", input$var1, " by ", input$var2, ".png", sep = "")
      },
      
      content = function(file){
        
        ggsave(file, width = 16, height = 9, dpi = 100)
      }
      
    )
    
    output$kNNdata <- renderPrint({
      
      # if (input$checkbox1){
      #   
      #   set.seed(2)
      #   
      #   dataSet <- select(nba2, !!!input$kNN_var)
      #   dataSet <- cbind(dataSet, Playoff = nba2$Playoff)
      #   
      #   train <- dataSet %>% sample_frac(0.80)
      #   test <- anti_join(dataSet, train)
      # 
      #   trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
      # 
      #   knn_fit <- train(Playoff ~ ., data = train, method = "knn",
      #                  trControl = trctrl, preProcess = c("center", "scale"), tuneGrid = data.frame(k = 2:30))
      #   knn_fit
      # 
      #   test_pred <- predict(knn_fit, newdata = test)
      # 
      #   confusionMatrix(test_pred, test$Playoff)
      # }
      missclass()
    })
    
    missclass <- reactive({

      if (input$checkbox1){

        set.seed(2)

        dataSet <- select(nba2, !!!input$kNN_var)
        dataSet <- cbind(dataSet, Playoff = nba2$Playoff)

        train <- dataSet %>% sample_frac(0.80)
        test <- anti_join(dataSet, train)

        trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

        knn_fit <- train(Playoff ~ ., data = train, method = "knn",
                         trControl = trctrl, preProcess = c("center", "scale"), tuneGrid = data.frame(k = 2:30))
        knn_fit

        test_pred <- predict(knn_fit, newdata = test)

        confusionMatrix(test_pred, test$Playoff)
      }
    })
      
      output$kNN_title <- renderText({
        paste("K Nearest Neighbors to Predict Playoff Status")  
      })
      
      output$kNN_warning <- renderText({
        paste("Test may take a few moments")
      })
      
      output$ensemble_warning <- renderText({
        paste("Test may take a few moments")
      })
      
      output$accuracy <- renderText({
        a <- missclass()
        b <- (1 - a$overall[1])
        
        if (input$checkbox1){
          paste("Misclassification rate is ", round(b,4)) 
        }
      })
      
      output$ensembleTitle <- renderText({
        paste("Ensemble Learning to Predict Playoff Status")
      })
      
      output$ensembleData <- renderPrint({
        
       # if (input$learningMethod == "Random Forests"){
       # 
       #    if (input$checkbox2){
       # 
       #      set.seed(2)
       # 
       #      dataSet <- select(nba2, !!!input$ensemble_var)
       #      dataSet <- cbind(dataSet, Playoff = nba2$Playoff)
       # 
       #      train <- dataSet %>% sample_frac(0.80)
       #      test <- anti_join(dataSet, train)
       # 
       #      trctrl <- trainControl(method = "repeatedcv", number = 10)
       # 
       #      rf_fit <- train(Playoff ~ ., data = train, method = "rf",
       #                       trControl = trctrl, preProcess = c("center", "scale"))
       # 
       #      test_pred <- predict(rf_fit, newdata = test)
       # 
       #      confusionMatrix(test_pred, test$Playoff)
       #    }
       #  } else {
       # 
       #    if (input$checkbox2){
       # 
       #      set.seed(2)
       # 
       #      dataSet <- select(nba2, !!!input$ensemble_var)
       #      dataSet <- cbind(dataSet, Playoff = nba2$Playoff)
       # 
       #      train <- dataSet %>% sample_frac(0.80)
       #      test <- anti_join(dataSet, train)
       # 
       #      trctrl <- trainControl(method = "repeatedcv", number = 10)
       # 
       #      boost_fit <- train(Playoff ~ ., data = train, method = "gbm",
       #                       trControl = trctrl, preProcess = c("center", "scale"), verbose = FALSE)
       # 
       #      test_pred <- predict(boost_fit, newdata = test)
       # 
       #      confusionMatrix(test_pred, test$Playoff)
       #    }
       #  }
        missclass2()
      })
      
      missclass2 <- reactive({

        if (input$learningMethod == "Random Forests"){

          if (input$checkbox2){

            set.seed(2)

            dataSet <- select(nba2, !!!input$ensemble_var)
            dataSet <- cbind(dataSet, Playoff = nba2$Playoff)

            train <- dataSet %>% sample_frac(0.80)
            test <- anti_join(dataSet, train)

            trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

            rf_fit <- train(Playoff ~ ., data = train, method = "rf",
                            trControl = trctrl, preProcess = c("center", "scale"))

            test_pred <- predict(rf_fit, newdata = test)

            confusionMatrix(test_pred, test$Playoff)
          }
        } else {

          if (input$checkbox2){

            set.seed(2)

            dataSet <- select(nba2, !!!input$ensemble_var)
            dataSet <- cbind(dataSet, Playoff = nba2$Playoff)

            train <- dataSet %>% sample_frac(0.80)
            test <- anti_join(dataSet, train)

            trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

            boost_fit <- train(Playoff ~ ., data = train, method = "gbm",
                               trControl = trctrl, preProcess = c("center", "scale"), verbose = FALSE)

            test_pred <- predict(boost_fit, newdata = test)

            confusionMatrix(test_pred, test$Playoff)
          }
        }

      })
      
      output$accuracy2 <- renderText({
        
        if (input$learningMethod == "Random Forests"){

          if (input$checkbox2){

            a <- missclass2()
            b <- (1 - a$overall[1])
            paste("Misclassification rate is ", round(b,4))

          }
        } else {

          if (input$checkbox2){

            a <- missclass2()
            b <- (1 - a$overall[1])
            paste("Misclassification rate is ", round(b,4))
          }
        }
        
        number_summary <- reactive({
          summary(select(nba2, input$var))
        })
        
        output$numSummary <- renderPrint({
          number_summary()
        })

      })
      
      
    
    
}