## Junjie Wu  
## cn_preliminary_analysis Shiny App

rm(list = ls())

#setwd('/Users/junjiewu/OneDrive - McGill University/GLIS_Research/GLIS_research_project/script/cn_preliminary_analysis') 

library(rsconnect)
library(data.table)
library(shiny)
# library(rpart)
# library(rpart.plot)
# library(plotly)
# library(arsenal)
# library(ggplot2)
# library(rattle)
# library(RColorBrewer)
#library(statsr)
library(tidyverse)
# library(arules)
# library(arulesViz)
# library(OneR)
# library(rCBA)


# Use a fluid Bootstrap layout
ui <-
  navbarPage(    
    
    # Give the page a title
    titlePanel("Data Mining_CN Survey"),
    
    tabPanel('By Gender - Cue',
             # Generate a row with a sidebar
             fluidRow(
               column(4,
                      selectInput("gender_cue_sup", "Support threshold:",
                                  choices=c("0.1",
                                            "0.2",
                                            "0.3",
                                            "0.4",
                                            "0.5",
                                            "0.5")),
               ),
               column(4,
                      selectInput("gender_cue_conf", "Confidence threshold:",
                                  choices=c("0.1",
                                            "0.2",
                                            "0.3",
                                            "0.4",
                                            "0.5")),
               ),
               column(4,
                 selectInput("gender_cue", "Gender:", 
                             choices=c("Male",
                                       "Female")),
               ),
               
               #display association rules result
               mainPanel(width = 10,
                         DT::dataTableOutput("h1_gender_cue")
               )
             )
    ),
    
    tabPanel('By Gender - all question',
             fluidRow(
               column(4,
                      selectInput("gender_all_sup", "Support threshold:",
                                  choices=c("0.1",
                                            "0.2",
                                            "0.3",
                                            "0.4",
                                            "0.5",
                                            "0.5")),
               ),
               column(4,
                      selectInput("gender_all_conf", "Confidence threshold:",
                                  choices=c("0.1",
                                            "0.2",
                                            "0.3",
                                            "0.4",
                                            "0.5")),
               ),
               column(4,
                      selectInput("gender_all", "Gender:", 
                                  choices=c("Male",
                                            "Female")),
               ),
               
               #display association rules result
               mainPanel(width = 10,
                         DT::dataTableOutput("h1_gender_all")
               )
             )
    ),
    
    tabPanel('By Innovativeness - Cue',
             # Generate a row with a sidebar
             
             # Create a new Row in the UI for selectInputs
             fluidRow(
               column(4,
                      selectInput("innov_cue_sup", "Support threshold:",
                                  choices=c("0.1",
                                            "0.2",
                                            "0.3",
                                            "0.4",
                                            "0.5",
                                            "0.5")),
               ),
               column(4,
                      selectInput("innov_cue_conf", "Confidence threshold:",
                                  choices=c("0.1",
                                            "0.2",
                                            "0.3",
                                            "0.4",
                                            "0.5")),
               ),
               column(4,
                      selectInput("innov_cue", "Innovativeness:", 
                                  choices=c("low",
                                            "high")),
               ),
               #display association rules result
               mainPanel(width = 10,
                         DT::dataTableOutput("h1_innov_cue")
               )
               
             )
    ),
    
    
    tabPanel('By Innovativeness - all question',
             # Generate a row with a sidebar
             fluidRow(
               column(4,
                      selectInput("innov_all_sup", "Support threshold:",
                                  choices=c("0.1",
                                            "0.2",
                                            "0.3",
                                            "0.4",
                                            "0.5",
                                            "0.5")),
               ),
               column(4,
                      selectInput("innov_all_conf", "Confidence threshold:",
                                  choices=c("0.1",
                                            "0.2",
                                            "0.3",
                                            "0.4",
                                            "0.5")),
               ),
               column(4,
                 selectInput("innov_all", "Innovativeness:", 
                             choices=c("low",
                                       "high")),
               ),
               
               #display association rules result
               mainPanel(width = 10,
                         DT::dataTableOutput("h1_innov_all")
               )
             )
    )
    
  )



# Define server logic required to draw a histogram
server <- function(input, output) {


  # cues to innov
  output$h1_innov_cue <- DT::renderDataTable(DT::datatable({
    
    # read preprocess rules
    dt_fp <- fread('rules_innov_cue.csv')[,-1]
    
    #fillter by selected threshold and label
    dt_fp <- dt_fp[ support >= as.numeric(input$innov_cue_sup)]
    dt_fp <- dt_fp[ confidence >= as.numeric(input$innov_cue_conf)]
    
    rules <- dt_fp[grep(input$innov_cue, dt_fp$rhs),]
    rules
  }))
  
  
  # all question to innov
  output$h1_innov_all <- DT::renderDataTable(DT::datatable({
    
    # read preprocess rules
    dt_fp <- fread('rules_innov_all.csv')[,-1]
    
    #fillter by selected threshold and label
    dt_fp <- dt_fp[ support >= as.numeric(input$innov_all_sup)]
    dt_fp <- dt_fp[ confidence >= as.numeric(input$innov_all_conf)]
    
    rules <- dt_fp[grep(input$innov_all, dt_fp$rhs),]
    rules
  }))
  
  # cues to gender
  output$h1_gender_cue <- DT::renderDataTable(DT::datatable({
    
    # read preprocess rules
    dt_fp <- fread('rules_gender_cue.csv')[,-1]
    
    #fillter by selected threshold and label
    dt_fp <- dt_fp[ support >= as.numeric(input$gender_cue_sup)]
    dt_fp <- dt_fp[ confidence >= as.numeric(input$gender_cue_conf)]
    
    rules <- dt_fp[grep(input$gender_cue, dt_fp$rhs),]
    rules
  }))

  # all questions to gender
  output$h1_gender_all <- DT::renderDataTable(DT::datatable({
    
    # read preprocess rules
    dt_fp <- fread('rules_gender_all.csv')[,-1]

    #fillter by selected threshold and label
    dt_fp <- dt_fp[ support >= as.numeric(input$gender_all_sup)]
    dt_fp <- dt_fp[ confidence >= as.numeric(input$gender_all_conf)]
    
    rules <- dt_fp[grep(input$gender_all, dt_fp$rhs),]
    rules
  }))
  
 }

# Run the application 
shinyApp(ui = ui, server = server)

