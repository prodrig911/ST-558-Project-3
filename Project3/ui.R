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
nba2$Playoff <- as.factor(nba2$Playoff)

nba3 <- nba2 %>% select(FG:PTS)

ui <- fluidPage(
  
  
  titlePanel("ST 558 Project 3"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      conditionalPanel(condition = "input.tabs == '1'", h3("Welcome to My NBA App!")),
      
      conditionalPanel(condition = "input.tabs == '2'",
                       selectInput("season", "Season", choices = levels(nba2$Season)),
                       selectInput("conference", "Conference", choices = c("Both", "East", "West")),
                       varSelectInput("varStats", "Choose Variables to View:", select(nba2,FG:Conference), multiple = TRUE),
                       downloadButton("download1", "Download Data")),
      
      conditionalPanel(condition = "input.tabs == '3'",
                       uiOutput("dataTitle"),
                       conditionalPanel("input.tabs2 == '1'",
                                        selectInput("var", "Variable", 
                                                    choices = names(nba2[sapply(nba2, is.numeric)])),
                                        sliderInput("bins", "Number of Bins", min = 4, 
                                                    max = 30, value = 13),
                                        downloadButton("download2", "Save Histogram"),
                                        br(),
                                        br(),
                                        verbatimTextOutput("numSummary")),
                       conditionalPanel("input.tabs2 == '2'",
                                        selectInput("var1", "X Variable:",
                                                    choices = names(nba2[sapply(nba2, is.numeric)])),
                                        selectInput("var2", "Y Variable:",
                                                    choices = names(nba2[sapply(nba2, is.numeric)])),
                                        br(),
                                        tags$style("#clickText {font-size:14px;"),
                                        div(style = "text-align:center;"),
                                        textOutput("clickText"),
                                        br(),
                                        br(),
                                        downloadButton("download3", "Save Plot")),
                       conditionalPanel("input.tabs2 == '3'",
                                        varSelectInput("varInfo", "Choose variables to view pairs info", 
                                                       select(nba2,FG:PTS),
                                                       multiple = TRUE),
                                        downloadButton("download9", "Save Pairs Plot"))),
      
      conditionalPanel(condition = "input.tabs == '4'",
                       selectInput("team", "Team", choices = levels(nba2$Team)),
                       varSelectInput("varStats2", "Choose Variables to View",  select(nba2,FG:Conference), multiple = TRUE),
                       downloadButton("download4", "Download Team Data")),
      
      conditionalPanel(condition = "input.tabs == '5'",
                       conditionalPanel("input.tabs4 == '1'",
                                        varSelectInput("pcaVars", "Choose At Least Two Variables for Comparison", 
                                                       select(nba2,FG:PTS), multiple = TRUE),
                                        downloadButton("download5", "Download Pairs Plot")),
                                        # uiOutput("downloadWarning1")),
                       conditionalPanel("input.tabs4 == '2'",
                                        verbatimTextOutput("pcInfo"),
                                        br(),
                                        downloadButton("download6", "Download Prop. of Variance Plot"),
                                        br(),
                                        br(),
                                        downloadButton("download7", "Download Cum. Pop of Variance Plot")),
                       conditionalPanel("input.tabs4 == '3'",
                                        numericInput("numPCA1", "Choose PCA Number for x",
                                                     min = 0, max = 0, value = 0),
                                        numericInput("numPCA2" ,"Choose PCA Number for y",
                                                     min = 0, max = 0, value = 0),
                                        selectInput("biCat", "Choose Categorical Varible for Analysis",
                                                    choices = names(nba2[sapply(nba2, is.factor)])),
                                        checkboxInput("pcaCheck2", "Check Box for Categorical Variable Analysis"),
                                        downloadButton("download8", "Download Bi-plot"))),
      
      conditionalPanel(condition = "input.tabs == '6'",
                       conditionalPanel("input.tabs3 == '1'",
                                        textOutput("kNN_title"),
                                        br(),
                                        varSelectInput("kNN_var", "Select Predictor Variables", nba3, multiple = TRUE),
                                        numericInput("kFolds", "Choose # of Folds/Resampling Iterations", 
                                                     min = 5, max = 10, value = 5),
                                        numericInput("kRepeats", "Choose # of complete sets of folds", 
                                                     min = 3, max = 5, value = 3),
                                        checkboxInput("checkbox1", "Check box to run kNN"),
                                        textOutput("kNN_warning"),
                                        br(),
                                        br(),
                                        textOutput("accuracy")),
                       conditionalPanel("input.tabs3 == '2'",
                                        textOutput("ensembleTitle"),
                                        br(),
                                        varSelectInput("ensemble_var", "Select Variables", nba3, multiple = TRUE),
                                        numericInput("eFolds", "Choose # of Folds/Resampling Iterations",
                                                     min = 5, max = 10, value = 5),
                                        numericInput("eRepeats", "Choose # of complete sets of folds",
                                                     min = 3, max = 5, value = 3),
                                        selectInput("learningMethod", "Ensemble Learning Method", 
                                                    choices = c("Random Forests", "Boosted")),
                                        checkboxInput("checkbox2", "Check box to run chosen method"),
                                        textOutput("ensemble_warning"),
                                        textOutput("accuracy2")))),
  mainPanel(
      tabsetPanel(type = "tabs", id = "tabs",
                  tabPanel("About", value = "1",
                           tabsetPanel(type = "tabs", id = "tabs5",
                                       tabPanel(h5("About the App"), 
                                                br(),
                                                br(),
                                                tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
                                                textOutput("appText1"),
                                                br(),
                                                uiOutput("link"),
                                                br(),
                                                tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))), 
                                                textOutput("appText2")),
                                       tabPanel(h5("About the Prediction Models"),
                                                h4("kNN"),
                                                tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
                                                textOutput("modelText1"),
                                                withMathJax(div("$$P(yes|x_1,x_2,..), P(no|x_1,x_2,..)$$")),
                                                h4("Random Forests"),
                                                tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
                                                textOutput("modelText2"),
                                                div("$$m=\\sqrt{p}$$"),
                                                h4("Boosted"),
                                                textOutput("modelText3")))),
                  
                  tabPanel("Scroll through Data", value = "2", dataTableOutput("table1"),
                           id = "tabselected"),
                  
                  tabPanel("Data Exploration", value = "3",
                           tabsetPanel(type = "tabs", id = "tabs2",
                                       tabPanel("Histogram", plotOutput("hist"), value = '1'),
                                       tabPanel("Plot", plotOutput("plot", click = "plot_click"), 
                                                tags$style("#plotInfo {font-size:20px;"),
                                                div(style = "text-align:center;"),
                                                textOutput("plotInfo"), value = '2'),
                                       tabPanel("Pairs Info", plotOutput("varPairs"), value = '3'))),
                  
                  tabPanel("Team Data", value = "4", dataTableOutput("table2"),
                           id = "tabselected"),
                  
                  tabPanel("PCA Analysis", value = "5",
                           tabsetPanel(type = "tabs", id = "tabs4",
                                       tabPanel("Linear Combination Comparison",
                                                textOutput("pairsWarning"),
                                                plotOutput("pairsPlot"),
                                                value = '1'),
                                       tabPanel("PC Info", 
                                                textOutput("screeWarning"),
                                                fluidRow(splitLayout(cellWidths = c("50%","50%"), 
                                                                     plotOutput("scree1"),
                                                                     plotOutput("scree2"))), value = '2'),
                                       tabPanel("Biplot", textOutput("biWarning"),
                                                plotOutput("biplot"), value = '3'))),
                  
                  tabPanel("Modeling", value = "6",
                           tabsetPanel(type = "tabs", id = "tabs3",
                                       tabPanel("kNN", verbatimTextOutput("kNNdata"), value = '1'),
                                       tabPanel("Ensemble Learning", verbatimTextOutput("ensembleData"), value = '2')))
      )
    )
  )
)


