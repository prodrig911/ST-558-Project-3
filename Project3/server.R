## Phillip Rodriguez-Lebron
## ST 558
## server for Project 3
## 7-17-2019

library(shiny)
library(tidyverse)
library(DT)
library(caret)
library(GGally)

# nba <- read_csv("C:/Users/Phillip/Desktop/NBA/2018-2019 NBA.csv")
nba2 <- read_csv("NBA.csv")
nba2$Playoff <- as.factor(nba2$Playoff)

server <- function(input, output, session) {
  
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
    
    # a <- input$var
    # b <- pull(getData1(),a)
    # bins <- seq(min(b),max(b),length.out = input$bins + 1)
    # 
    # if (input$conference == "Both"){
    #   
    #   hist(b, main = paste0("Histogram of ", a, " for ", input$season, " NBA Season"), 
    #        xlab = a, col = "cornflowerblue", breaks = bins)
    # } else {
    #   hist(b, main = paste0("Histogram of ", a, " for ", input$season, " NBA Season ", 
    #                         input$conference, "ern Conference"), xlab = a, col = "cornflowerblue", breaks = bins)
    # }
    p()
  })
  
  p <- function(){
    
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
  }
  
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
  })
    
  number_summary <- reactive({
    summary(select(nba2, input$var))
  })
    
  output$numSummary <- renderPrint({
    number_summary()
  })
    
  pairsData <- function(){
    sub <- select(nba2, !!!input$pcaVars)
    pairs(sub, cex = 0.75)
  }
    
  output$pairsWarning <- renderText({
    if (length(select(nba2, !!!input$pcaVars)) <= 1){
      "Choose at least two variables!"
    }
  })
  
  # output$downloadWarning1 <- renderUI({
  #   
  #   if (length(select(nba2, !!!input$pcaVars)) <= 1){
  #     if (input$download5){
  #       text <- paste("Must choose at least two variables to download!")
  #       h4(text)
  #     } 
  #   }
  # })
  
  output$screeWarning <- renderText({
    if (length(select(nba2, !!!input$pcaVars)) <= 1){
      "Choose at least two variables from the comparison tab!"
    }
  })
  
  output$biWarning <- renderText({
    if (length(select(nba2, !!!input$pcaVars)) <= 1){
      "Choose at least two variables from the comparison tab!"
    }
  })
  
  output$pairsPlot <- renderPlot({
      
    if (length(select(nba2, !!!input$pcaVars)) >= 2){
        
        pairsData()
    }
    
  })
  
  pc <- reactive({
      
    sub <- select(nba2, !!!input$pcaVars)
    PCs <- prcomp(sub, center = TRUE, scale = TRUE)
    PCs
  })
    
  output$pcInfo <- renderPrint({
      
    if (length(select(nba2, !!!input$pcaVars)) >= 2){
        pc()
    }
  })
    
  plotScree1 <- function(){
      
    PCs <- pc()
      
    plot(PCs$sdev^2/sum(PCs$sdev^2), xlab = "Principal Component", 
           ylab = "Proportion of Variance Explained", ylim = c(0,1), type = "b")
  }
  
  
  plotScree2 <- function(){
    
    PCs <- pc()
    
    plot(cumsum(PCs$sdev^2/sum(PCs$sdev^2)), xlab = "Principal Component",
         ylab = "Cum. Prop of Variance Explained", ylim = c(0,1), type = "b")
  }
  
  output$scree1 <- renderPlot({
    if (length(select(nba2, !!!input$pcaVars)) >= 2){
      plotScree1()
    }
  })
  
  output$scree2 <- renderPlot({
    if (length(select(nba2, !!!input$pcaVars)) >= 2){
      plotScree2()
    }
  })
  
  subData <- function(){
    select(nba2, !!!input$pcaVars, Season, Playoff, Conference, Team)
  }
  
  plotBiplot <- function(){
    
    sub <- subData()
    PCs <- pc()
    
    biplot(PCs, xlabs = rep("+", nrow(subData())), c(input$numPCA1, input$numPCA2))

    if (input$pcaCheck2){
    
      if (input$biCat == "Team"){
      
        biplot(PCs, xlabs = sub$Team, c(input$numPCA1, input$numPCA2))
      
      } 
    
      else if (input$biCat == "Playoff"){
      
        biplot(PCs, xlabs = sub$Playoff, c(input$numPCA1, input$numPCA2))
      
      }
    
      else if (input$biCat == "Season"){
      
        biplot(PCs, xlabs = sub$Season, c(input$numPCA1, input$numPCA2))
      
      }
    
      else{
      
        biplot(PCs, xlabs = sub$Conference, c(input$numPCA1, input$numPCA2))
      }
    }
  
  }
  
  output$biplot <- renderPlot({
    
    if (length(select(nba2, !!!input$pcaVars)) >= 2){
      plotBiplot()
    }
  })
  
  lengthPCA <- function(){
    length(select(nba2,!!!input$pcaVars))
  }
  
  observe(updateNumericInput(session, "numPCA1", min = 1, max = lengthPCA(), value = 1))
  observe(updateNumericInput(session, "numPCA2", min = 1, max = lengthPCA(), value = 2))
  
  output$download5 <- downloadHandler(
    
    # if (length(select(nba2, !!!input$pcaVars)) >= 2)
    
      filename = function(){
      
        paste("Pairs Plot", ".png", sep = "")
      
      }
    ,
    
      content = function(file){

        png(file, width = 600, height = 500)
        pairsData()
        dev.off()
      }
    
  )
  
  output$download6 <- downloadHandler(
    
    filename = function(){
      
      paste("Proportion of Variance Plot", ".png", sep = "")
      
    }
    ,
    
    content = function(file){
      
      png(file, width = 600, height = 500)
      plotScree1()
      dev.off()
    }
    
  )
  
  output$download7 <- downloadHandler(
    
    filename = function(){
      
      paste("Cum. Prop of Variance Plot", ".png", sep = "")
      
    }
    ,
    
    content = function(file){
      
      png(file, width = 600, height = 500)
      plotScree2()
      dev.off()
    }
    
  )
  
  output$download8 <- downloadHandler(
    
    filename = function(){
      
      paste("Bi-plot of PCA ", input$numPCA1, " and PCA ", input$numPCA2, ".png", sep = "")
      
    }
    ,
    
    content = function(file){
      
      png(file, width = 600, height = 500)
      plotBiplot()
      dev.off()
    }
    
  )
  
  url <- a("basketball-reference.com", href = "https://www.basketball-reference.com/")
  
  output$link <- renderUI({
    tagList("", url)
  })
  
  output$appText1 <- renderText({
    
    paste("This app uses NBA statistics curated from the website basketball-reference.com. 
          The statistics are from all 30 teams and include statistics from the 2015-2016, 
          2016-2017, 2017-2018, and 2018-2019 NBA seasons. The statistics include the following: 
          team name, field goals (FG), field goal attempts (FGA), field goal percentage (FG%), 
          three pointers (3P), three point attempts (3PA), three point percentage (3P%), 
          two pointers (2P), two point attempts (2PA), two point percentage (2P%), free throws (FT), 
          free throw attempts (FTA), free throw percentage (FT%), offensive rebounds (ORB), 
          defensive rebounds (DRB), total rebounds (TRB), assists (AST), blocks (BLK), 
          turnovers (TOV), personal fouls (PF), points (PTS), playoff status (Playoff), 
          conference (Conference). The link to the website is below.")
  })
  
  output$appText2 <- renderText({
    
    paste("The app allows users to choose a season and then choose which statistics 
          to view from all 30 teams or by conference. Users can also view the statistics 
          of a specific team. Users can then view a histogram and summary statistics of a 
          variable and view a plot that shows the relationship between two variables. The app 
          allows the user to perform a PCA analysis that includes pairs plots of chosen variables, 
          PCA information, and bi-plots. Finally, users can build models to predict the playoff 
          status of NBA teams using k nearest neighbors, random forests, and boosted forests methods. ")
    
  })
  
  output$modelText1 <- renderText({
    
    paste("The goal of using k Nearest Neighbors for classification is to predict class membership, 
          in this case whether the team makes the playoffs or does not make the playoffs, based on a 
          combination of variables. The general idea is to use the closest k observations from a training set 
          to predict the class in a training set. Euclidean distance between predictors is used to decide what 
          is closest. The proportion of k closest values that make the playoffs (yes) and the proportion of 
          k closest values that do not make the playoffs (no) is as follows: ")
  })
  
  output$modelText2 <- renderText({
    
    paste("Random Forests create multiple trees from bootstrap samples and averages the results. The process does 
          not use all of the predictors but uses random subsets of predictors for each bootstrap sample. For 
          the purpose of predicting playoff status, or classification, the size of the subsets are determined by: ")
    
  })
  
  output$modelText3 <- renderText({
    
    paste("In this method, the trees are grown sequentially one tree at a time. Each subsequent tree helps to reduce the 
          errors produced by the previously trained tree. As each tree is grown, the predictions are updated. ")
    
  })
  
  output$varPairs <- renderPlot({
    
    if (length(select(nba2, !!!input$varInfo)) >= 2){
      
      ggpairs(select(nba2, !!!input$varInfo), aes(color = nba2$Playoff, alpha = 0.4))
      
    }
    
  })
  
  output$download9 <- downloadHandler(
    
    
    filename = function(){
      
      if (input$conference == "Both"){
        
        paste("Pairs Plot of ", input$season, " NBA Statistics", ".png", sep = "")
      } else {
        
        paste("Pairs Plot of ", input$season, " ", input$conference, "ern Conference NBA Statistics", ".png", sep = "")
      }
      
    },
    
    content = function(file){
      
      ggsave(file, width = 16, height = 9, dpi = 100)
    }
    
  )
  
  
}