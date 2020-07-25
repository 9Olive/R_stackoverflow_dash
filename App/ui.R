library(tidyverse)
library(shiny)
library(shinydashboard)
  
intro_body <- read_lines(file = paste0(getwd(), '/introduction_body.html')) %>% paste0(collapse = '')


ui <- dashboardPage(
  dashboardHeader(),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = 'about', icon = icon("dashboard")),
      menuItem("Data Exploration", tabName = 'datexp', icon = icon("wpexplorer"))
    )
  ),
  
  # Body of the dashboard where the tabs will be placed
  dashboardBody(
    tabItems(
      
      tabItem(
        tabName = 'about',
        fluidRow(
          HTML(intro_body)
        )
      ),
      
      tabItem(
        tabName = 'datexp',
        fluidPage(
          # Defining tabs
          tabsetPanel(
            tabPanel(
              title = "Exploratory Data Analysis",
              fluidRow(
                box(
                  DT::dataTableOutput('ans_dt_id')),
                
                box(
                  title = "Questions, Id = Conditional#",
                  htmlOutput('que_txt_id')),
                
                box(
                  title = "Answer, Id = Condition#",
                  htmlOutput('ans_txt_id'))
                )
              ), 
            
            tabPanel(
              title = "Latent Dirichlet Allocation",
              textOutput('test')) 
            )
          )
        )
      
      )
    )
  )
