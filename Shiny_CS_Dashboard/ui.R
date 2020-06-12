## ui.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(odbc)
library(dbplyr)
library(DT)
library(DBI)
library(lubridate)
library(plotly)
library(scales)

ui <- dashboardPage(
    
    # Dashboard Header
    dashboardHeader(title = "CS Dashboard"),
    
    # Dashboard Sidebar
    dashboardSidebar(
        
        # CS Name Input
        selectInput("cs_name", "CS Rep:",
             c("Ryan Johnson" = "0050L000009iXh7QAE",
               "Josiah Parry" = "0050L000008zeFcQAI",
               "Elena Ruiz" = "0050L000009fFfTQAU",
               "Katie Masiello" = "0050L000009O9wqQAC",
               "Colin Cohan" = "0050L000009iGP9QAM",
               "Shannon Hagerty" = "0050L000009fI76QAE",
               "Brian Law" = "0050L000009iGPEQA2"))),

    # Dashboard Body
    dashboardBody(
        fluidRow(
            box(plotlyOutput("cs_opp_plot"), width = "100%"),
            box(DTOutput("cs_date_table"), width = "100%")
        )
    )
)