library(shiny)
library(dplyr)
library(ggplot2)

source(file = paste0('dataexp_r_con.R'))
dataexp_pobs <- read_lines(file = paste0(getwd(), '/ts_obs.html')) %>% paste0(collapse = '')
dataexp_lda_intro <- read_lines(file = paste0(getwd(), '/lda_intro.html')) %>% paste0(collapse = '')
tidy_text_ex <- read_csv(file = 'lda_tidy_ex.csv')

server <- function(input, output, session) {

# Question and Answer preview ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.  
  
  output$ans_dt_id <- DT::renderDataTable(DT::datatable(ans_dt, selection = 'single'), 
                                          options = list(pageLength = 5),
                                          initComplete = DT::JS('function(setting, json) { alert("done"); }'))
  # output$ans_dt_id <- renderPlot(hist(rnorm(1000)))
  output$ans_txt_id <- renderUI({
    s = input$ans_dt_id_rows_selected
    if (length(s)) {
      HTML(ans_txt[s])
    } else {
      HTML("<p> Select row from Preview Table to view corresponding Q&A. </p>")
    }
    })
  
  output$que_txt_id <- renderUI({
    s = input$ans_dt_id_rows_selected
    if (length(s)) {
      HTML(tit_txt[s])
    } else {
      HTML("<p> Select row from Preview Table to view corresponding Q&A. </p>")
    }
  })
  
# Data Exploration Plotly ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.  
  output$ts_id <- renderPlotly({
    
    if (input$q_or_a_id == 'Questions') {
      plot_data <- que_ts
    } else {
      plot_data <- ans_ts
    }
    
    plot_ly(x = plot_data[['posted']], 
            y = plot_data[[dict$bknd[which(dict$user == input$q_in_id)]]]) %>%
      add_lines() %>%
      layout(xaxis = list(title = "Date Posted"),
             yaxis = list(title = input$q_in_id),
             title = paste0('Looking at: ', input$q_or_a_id))
    })
  
  output$plot_obs_id <- renderUI(
    HTML(dataexp_pobs)
  )

# Latent Dirichlet Allocation ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.  

  output$lda_intro_id <- renderUI(
    HTML(dataexp_lda_intro)
  )
  
  output$tdy_text_id <- DT::renderDataTable(tidy_text_ex)
  
  output$que_cls_dt_id <- DT::renderDataTable(DT::datatable(que_cls_dt, selection = 'single'), 
                                          options = list(pageLength = 5),
                                          initComplete = DT::JS('function(setting, json) { alert("done"); }'))
  # output$ans_dt_id <- renderPlot(hist(rnorm(1000)))
  output$ans_cls_txt_id <- renderUI({
    z = input$que_cls_dt_id_rows_selected
    if (length(z)) {
      HTML(ans_cls_txt[z])
    } else {
      HTML("<p> Select row from Preview Table to view corresponding Q&A. </p>")
    }
  })
  
  output$que_cls_txt_id <- renderUI({
    z = input$que_cls_dt_id_rows_selected
    if (length(z)) {
      HTML(que_cls_txt[z])
    } else {
      HTML("<p> Select row from Preview Table to view corresponding Q&A. </p>")
    }
  })
    
}

