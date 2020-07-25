library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
  
intro_body   <- read_lines(file = paste0(getwd(), '/introduction_body.html')) %>% paste0(collapse = '')


ui <- dashboardPage(
  dashboardHeader(),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = 'about', icon = icon("dashboard")),
      menuItem(
        "Data Exploration", 
        tabName = 'datexp', 
        icon = icon("wpexplorer")
      ),
      menuItem(
        "Plot Parameters",
        tabName = 'dataexp_pars',
        menuSubItem(
          selectInput(
            'q_or_a_id',
            'Q or A?',
            choices = c('Questions', 'Answers'),
            selected = 'Questions'
            )
        ),
        menuSubItem(
          radioButtons(
            'q_in_id',
            'Y-Axis:',
            choices = c("Total Inquiries or Replies",  "Mean Score", "Median Score",  "Max Score")
            ),
          )
        )
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
                  title = "Over the Years: ",
                  plotlyOutput(
                    outputId = 'ts_id'
                    )
                  ),
                box(
                  title = "Observations: ",
                  htmlOutput(
                    'plot_obs_id'
                  )
                )
              ),
                            
              fluidRow(
                box(
                  DT::dataTableOutput('ans_dt_id')),
                
                box(
                  title = "Question: ",
                  htmlOutput('que_txt_id')),
                
                box(
                  title = "Answer: ",
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
