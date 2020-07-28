library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

source(file = paste0('dataexp_r_con.R'))
source(file = paste0('models_r_con.R'))
source(file = 'question_classifier.R')

load(file = '../Modeling/models_tfidf_plots.RData')

dataexp_pobs <- read_lines(file = paste0(getwd(), '/ts_obs.html')) %>% paste0(collapse = '')
dataexp_lda_intro <- read_lines(file = paste0(getwd(), '/lda_intro.html')) %>% paste0(collapse = '')
tidy_text_ex <- read_csv(file = 'lda_tidy_ex.csv')
models_ft_eng <- read_lines(file = paste0(getwd(), '/models_data_prep_intro.html')) %>% paste0(collapse = '')
tags_fns_html <- read_lines(file = paste0(getwd(), '/models_tag_blurb.html')) %>% paste0(collapse = '')
knn_text_html <- read_lines(file = paste0(getwd(), '/kNN_intro.html')) %>% paste0(collapse = '')
bst_text_html <- read_lines(file = paste0(getwd(), '/models_boost_blurb.html')) %>% paste0(collapse = '')

server <- function(input, output, session) {

#  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.  
#  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`. 
# Question and Answer preview ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.  
#  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.  
#  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`. 
  
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
  
#  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.  
#  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`. 
# Data Exploration Plotly ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.
#  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.  
#  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.   
  
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
#  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.  
#  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`. 
# Latent Dirichlet Allocation ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.  
#  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.  
#  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`. 
  output$lda_intro_id <- renderUI(
    HTML(dataexp_lda_intro)
  )
  
  output$tdy_text_id <- DT::renderDataTable(tidy_text_ex)
  
  output$que_cls_dt_id <- DT::renderDataTable(DT::datatable(que_cls_dt, selection = 'single'), 
                                          options = list(pageLength = 5),
                                          initComplete = DT::JS('function(setting, json) { alert("done"); }'))
  # output$ans_dt_id <- renderPlot(hist(rnorm(1000)))
  output$ans_cls_txt_id <- renderUI({
    
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

#  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.    
#  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.    
# Models - Data Prep Tab  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.  
#  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.  
#  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.  
  
  output$models_data_prep_html_id <- renderUI(
    HTML(models_ft_eng)
  )
  
  # Step 1. tf_idf plot
  output$models_tf_rank_plot_id <- renderPlotly({
    
    if (input$models_tf_plot_ctrl_id == '2') {
      if (input$models_tf_plot_fitted_id) {
        
        
        g2_lm <- g2 + geom_abline(intercept = coef(lm.plot2)[1],
                                  slope = coef(lm.plot2)[2],
                                  color = 'gray50',
                                  linetype = 2,
                                  size = as.numeric(input$models_tf_plot_fitted_size_id),
                                  alpha = as.numeric(input$models_tf_plot_fitted_alpha_id))
        
        ggplotly(g2_lm) 
        
      } else {
        
        ggplotly(g2)
        
      }
      
    } else if (input$models_tf_plot_ctrl_id == '3') {
      if (input$models_tf_plot_fitted_id) {
        
        g3_lm <- g3 + geom_abline(intercept = coef(lm.plot3)[1], 
                                  slope = coef(lm.plot3)[2], 
                                  color = 'gray50', 
                                  linetype = 2,
                                  size = as.numeric(input$models_tf_plot_fitted_size_id),
                                  alpha = as.numeric(input$models_tf_plot_fitted_alpha_id))
        
        ggplotly(g3_lm)
        
      } else {
        
        ggplotly(g3)
        
      }
    } else {
      
      if (input$models_tf_plot_fitted_id) {
        
        g4_lm <- g4 + geom_abline(intercept = coef(lm.plot4)[1], 
                                  slope = coef(lm.plot4)[2], 
                                  color = 'gray50', 
                                  linetype = 2,
                                  size = as.numeric(input$models_tf_plot_fitted_size_id),
                                  alpha = as.numeric(input$models_tf_plot_fitted_alpha_id))
        
        ggplotly(g4_lm)
        
      } else {
        
        ggplotly(g4)
        
        }
      }
    
  })
  
  # Step 2. tf_idf top terms
  output$models_top_terms_tbl_id <- DT::renderDataTable({
    if (input$models_tf_terms_ctrl_id == '2') {
      DT::datatable(tf_idf_terms2, filter = 'top')
    } else if (input$models_tf_terms_ctrl_id == '3') {
      DT::datatable(tf_idf_terms3, filter = 'top')
    } else {
      DT::datatable(tf_idf_terms4, filter = 'top')
    }
    
    })
  
  # Step 3. tags
  
  output$models_tags_blurb_id <- renderUI(
    HTML(
      tags_fns_html
      )
    )
  
  output$models_tags_prev_tbl_id <- DT::renderDataTable({
    if (input$models_tags_prev_tbl_chk_id) {
      
      DT::datatable(nTags %>%
                      group_by(Tag) %>%
                      count(Tag, sort = T),
                    filter = 'top'
                    )
      
    } else {
      DT::datatable(nTags, filter = 'top')
    }
  })
  
  # Data Modeling section
  # Knn
  
  output$models_knn_text_id <- renderUI(
    HTML(
      knn_text_html
    )
  )
  
  output$knn_plot_id <- renderPlot({
    if (input$knn_toggl_id == "CV Plot") {
      if (input$models_knn_topic_id == '2') {
        if (input$knn_plot_chkbx_id) {

          gknn <- knn_fit_k2 %>%
            ggplot() +
            geom_boxplot(aes(x = Neighbors, y = `Misclassification Rate`, fill = Neighbors), alpha = 0.7, show.legend = F) +
            scale_y_continuous(labels = scales::percent_format(accuracy = .1), breaks = scales::pretty_breaks(7)) +
            geom_smooth(aes(x = as.numeric(Neighbors), y = `Misclassification Rate`), se = F, alpha = 0.75, linetype = 'dotdash')

          gknn

          } else {

            gknn <- knn_fit_k2 %>%
              ggplot() +
              geom_boxplot(aes(x = Neighbors, y = `Misclassification Rate`, fill = Neighbors), alpha = 0.7, show.legend = F) +
              scale_y_continuous(labels = scales::percent_format(accuracy = .1), breaks = scales::pretty_breaks(7))

            gknn

          }
        } else if (input$models_knn_topic_id == '3') {

          if (input$knn_plot_chkbx_id) {

            gknn <- knn_fit_k3 %>%
              ggplot() +
              geom_boxplot(aes(x = Neighbors, y = `Misclassification Rate`, fill = Neighbors), alpha = 0.7, show.legend = F) +
              scale_y_continuous(labels = scales::percent_format(accuracy = .1), breaks = scales::pretty_breaks(7)) +
              geom_smooth(aes(x = as.numeric(Neighbors), y = `Misclassification Rate`), se = F, alpha = 0.75, linetype = 'dotdash')

            gknn

            } else {

              gknn <- knn_fit_k3 %>%
                ggplot() +
                geom_boxplot(aes(x = Neighbors, y = `Misclassification Rate`, fill = Neighbors), alpha = 0.7, show.legend = F) +
                scale_y_continuous(labels = scales::percent_format(accuracy = .1), breaks = scales::pretty_breaks(7))

              gknn

              }

        } else {

          if (input$knn_plot_chkbx_id) {

            gknn <- knn_fit_k4 %>%
              ggplot() +
              geom_boxplot(aes(x = Neighbors, y = `Misclassification Rate`, fill = Neighbors), alpha = 0.7, show.legend = F) +
              scale_y_continuous(labels = scales::percent_format(accuracy = .1), breaks = scales::pretty_breaks(7)) +
              geom_smooth(aes(x = as.numeric(Neighbors), y = `Misclassification Rate`), se = F, alpha = 0.75, linetype = 'dotdash')

            gknn

            } else {

              gknn <- knn_fit_k4 %>%
                ggplot() +
                geom_boxplot(aes(x = Neighbors, y = `Misclassification Rate`, fill = Neighbors), alpha = 0.7, show.legend = F) +
                scale_y_continuous(labels = scales::percent_format(accuracy = .1), breaks = scales::pretty_breaks(7))

              gknn

            }
          }
      } else {
        NULL
        }
    })

  output$knn_dt_id <- DT::renderDataTable({
    if (input$knn_toggl_id == "Table") {
      if (input$models_knn_topic_id == '2') {
        if (input$knn_tbl_chkbx_id) {

          knn_fit_dt <- knn_fit_k2 %>%
            group_by(Neighbors) %>%
            summarise(`Mean Error Rate` = mean(`Misclassification Rate`),
                      'SD Error Rate' = sd(`Misclassification Rate`)) %>%
            arrange(`Mean Error Rate`)

          DT::datatable(knn_fit_dt)

        } else {

          DT::datatable(arrange(knn_fit_k2, -`Misclassification Rate`))

        }
      } else if (input$models_knn_topic_id == '3') {

        if (input$knn_tbl_chkbx_id) {

          knn_fit_dt <- knn_fit_k3 %>%
            group_by(Neighbors) %>%
            summarise(`Mean Error Rate` = mean(`Misclassification Rate`),
                      'SD Error Rate' = sd(`Misclassification Rate`)) %>%
            arrange(`Mean Error Rate`)

          DT::datatable(knn_fit_dt)

        } else {

          DT::datatable(arrange(knn_fit_k3, -`Misclassification Rate`))

        }

      } else {

        if (input$knn_tbl_chkbx_id) {

          knn_fit_dt <- knn_fit_k4 %>%
            group_by(Neighbors) %>%
            summarise(`Mean Error Rate` = mean(`Misclassification Rate`),
                      'SD Error Rate' = sd(`Misclassification Rate`)) %>%
            arrange(`Mean Error Rate`)

          DT::datatable(knn_fit_dt)

        } else {

          DT::datatable(knn_fit_k4)

        }
      }
    } else {
      NULL
    }
  })
    
  output$model_bst_disc_html_id <- renderUI(
    HTML(bst_text_html)
  )

  output$model_bst_tbl_id <- DT::renderDataTable({
    if (input$models_bst_topic_id == '2') {
      DT::datatable(bst_fit_k2)
    } else if (input$models_bst_topic_id == '3') {
      DT::datatable(bst_fit_k3)
    } else {
      DT::datatable(bst_fit_k4)
    }
  })

  output$model_bst_mscls_tbl_id <- DT::renderDataTable(DT::datatable(bst_mscls))
  
#  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.  
#  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.
# User Submit Questions  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`. 
#  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.  
#  ~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.~`.  
  
  output$que_pred_res_id <- renderText({
    
    observe({input$que_button_id})
    
    user_text <- isolate({input$que_man_submit_id})

    if (input$que_man_tags_chk_id){
      user_tag  <- isolate({input$que_man_tags_id})
    } else {
      user_tag  <- isolate({''})
    }
    
    isolate({k_user <- input$que_man_topic_id})
    
    cat <- user_pred(user_text, user_tag, k_user)
    cat
   
  })
  
  output$que_dt_id <- DT::renderDataTable({
    DT::datatable(que_cls_dt, selection = 'single')
    })
  
  output$que_pred_txt_id <- renderUI({
    u = input$que_dt_id_rows_selected
    if (input$que_button_id > 0 & length(u)) {
      HTML(ans_cls_txt[u])
    } else {
      HTML("<p> Submit a question. </p>")
    }
  })
  
  output$ans_pred_txt_id <- renderUI({
    u = input$que_dt_id_rows_selected
    if (input$que_button_id > 0 & length(u)) {
      HTML(que_cls_txt[u])
    } else {
      HTML("<p>Submit a question. </p>")
    }
  })
  
  datasetdl <- reactive({que_cls_dt})
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("SimilarQuestions.csv")
    },
    content = function(file) {
      write.csv(datasetdl(), file, row.names = FALSE)
    }
  )
  
  
}

