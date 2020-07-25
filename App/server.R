library(shiny)
library(dplyr)
library(ggplot2)

source(file = paste0('dataexp_r_con.R'))

server <- function(input, output, session) {
  output$ans_dt_id <- DT::renderDataTable(DT::datatable(ans_dt, selection = 'single'), 
                                          options = list(pageLength = 5),
                                          initComplete = DT::JS('function(setting, json) { alert("done"); }'))
  # output$ans_dt_id <- renderPlot(hist(rnorm(1000)))
  output$ans_txt_id <- renderUI({
    s = input$ans_dt_id_rows_selected
    if (length(s)) {
      HTML(ans_txt$Body[s, drop = F])
    } else {
      HTML("<p> Select row from Preview Table to view corresponding Q&A. </p>")
    }
    })
  output$que_txt_id <- renderUI(HTML(ans_txt$Body[2]))
  observe({updateTextInput(session, 'test', print(input$ans_dt_id_rows_selected))}) 
}

