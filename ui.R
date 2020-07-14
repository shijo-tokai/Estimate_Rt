library(shiny)
library(lubridate)
library(data.table)
library(dplyr)
library(EpiEstim)
library(tidyr)
library(readr)
library(stringr)
library(tidyverse)
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Estimated Rt"),
    
    uiOutput("pref"), 
        mainPanel(
            plotOutput("Plot")
        )
    )
)


