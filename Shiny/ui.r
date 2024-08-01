## ui.R  ##
library(shinyjs)
library(shiny)
library(latex2exp)
library(markdown)
library(magrittr)
library(shinydashboard)
library(lubridate)

shinyUI(fluidPage(
  
  titlePanel("MSC: Estimated Waiting Time"),
  
    mainPanel(
      br(),
      strong(textOutput("text1")),
      ("Select below to see the estimated waiting times of a particular day/week."),
      br(), 
     
     ######### Needs to be updated for next semester ###########
      selectInput("Week", label = "Choose Week of the Semester:",
                  choices = c("1","2","3","4","5","6","7","8","9",
                              "10","11","12"), selected = ifelse((Sys.time() %>% week())>36, 
                                                                 (Sys.time() %>% week())-36,0)),
      
      selectInput("Day", label = "Choose Day of the Semester:",
                  choices = c("Monday", "Tuesday", "Wednesday",
                              "Thursday", "Friday"), selected = (Sys.time() %>% weekdays())),
     br(),
     strong(textOutput("text2")),
     br(),
     plotOutput(outputId = "hist", height = "300px", width = "600px")

  )
))

