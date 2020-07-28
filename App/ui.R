library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
  
intro_body   <- read_lines(file = paste0(getwd(), '/introduction_body.html')) %>% paste0(collapse = '')


ui <- dashboardPage(
  dashboardHeader(),
  
  dashboardSidebar(
    sidebarMenu(
      # About project side bar UI
      menuItem("About", tabName = 'about', icon = icon("dashboard")),
      
      # Data exploration sidebar UI
      menuItem(
        "Data Exploration", 
        tabName = 'datexp', 
        icon = icon("wpexplorer")
      ),
      
      # Plot parameters sidebar UI
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
            )
          )
        ),
      
      menuItem(
        "Modeling Our Classifications",
        tabName = 'models',
        icon = icon('filter')

      ),
      
      menuItem(
        "Submit a Question",
        tabName = "user_int",
        icon = icon("angle-double-right")
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
          # EDA tab
          tabsetPanel(
            tabPanel(
              title = "Exploratory Data Analysis",
              
              
              fluidRow(
                box(
                  width = 9,
                  title = "Over the Years: ",
                  plotlyOutput(
                    outputId = 'ts_id'
                    )
                  ),
                box(
                  width = 3,
                  title = "Observations: ",
                  htmlOutput(
                    'plot_obs_id'
                  )
                )
              ),
                            
              fluidRow(
                box(
                  width = 12,
                  title = 'Q&A Data Preview:',
                  DT::dataTableOutput('ans_dt_id'))
              ),
              fluidRow(
                box(
                  title = "Question: ",
                  htmlOutput('que_txt_id')),
                
                box(
                  title = "Answer: ",
                  htmlOutput('ans_txt_id'))
                )

              ), 
            
            # LDA tab
            tabPanel(
              title = "Latent Dirichlet Allocation (Unsupervised Learning)",
              
              # LDA introduction
              fluidRow(
               box(
                 width = 7,
                 htmlOutput('lda_intro_id')
                 ),
               box(
                 width = 5,
                 withMathJax(),
                 helpText('$$\\begin{aligned} p(\\theta, z | w, \\alpha, \\beta) = \\frac{p(\\theta, z, w | \alpha, \beta)}{p(w|\\alpha, \\beta)} \\\\ \\theta \\text{ is a Dirichlet Random Varaible with parameter } \\alpha \\\\ z \\text{ denotes topic variable vector} \\\\ w \\text{ denotes a word vector of } 1,...,V \\text{ words.}  \\\\ \\beta \\text{ is a } k \\times V \\text{ matrix that denotes the probability of the Vth word occuring in the zth topic.} \\\\ k \\text{ denotes the dimenosionality of the topics, and as such the dimension of } z \\\\ \\end{aligned}$$')
                     )
                       
                ),
              
              # Example of LDA Data Prep
              fluidRow(
                box(
                  title = 'Step 1. Observe Raw Answer Text Data',
                  width = 3,
                  "<p>I've only come across both R and the Dirichlet distribution in passing, so I hope I'm not too much off the mark.</p>\n\n<p><a href=\"https://stat.ethz.ch/pipermail/r-help/2006-September/113258.html\" rel=\"nofollow noreferrer\">This mailing list message</a> seems to answer your question:</p>\n\n<blockquote>\n  <p>Scrolling through the results of\n  RSiteSearch(\"dirichlet\") suggests some useful tools\n  in the VGAM package.  The gtools package and\n  MCMC packages also have ddirichlet() functions\n  that you could use to construct a (negative log) likelihood\n  function and optimize with optim/nlmin/etc.</p>\n</blockquote>\n\n<p>The deal, DPpackage and mix packages also may or may not provide what you need.</p>\n\n<p>Then again, these are all still CRAN packages, so I'm not sure if you already found these and found them unsuitable.</p>\n\n<p>As for searching for R, <a href=\"http://www.r-project.org/\" rel=\"nofollow noreferrer\">the R project site</a> itself already provides a few links on <a href=\"http://www.r-project.org/search.html\" rel=\"nofollow noreferrer\">its search page</a>.</p>\n"
                ),
                
                box(
                  title = 'Step 2. Clean Raw Answer Text Data',
                  width = 2,
                  "I've come across R and Dirichlet distribution in passing  I hope I'm not much mark.            Scrolling results   RSiteSearch \"dirichlet\"  suggests useful tools   in VGAM package.  The gtools package and   MCMC packages also ddirichlet   functions   use construct  negative log  likelihood   function and optimize with optim/nlmin/etc.      The deal  DPpackage and mix packages also may or may not provide need.    Then  all still CRAN packages  I'm not sure if already found and found unsuitable.    As for searching for R"
                ),
                
                box(
                  title = 'Step 3. Tokenize Cleaned Text Data per Answer',
                  width = 7,
                  DT::dataTableOutput('tdy_text_id')
                ),
                
                box(
                  title = 'Step 4. Generate Document Term Matrix',
                  "A document term matrix (DTM) is an w by z sparse matrix. Where w are the unique words in the tidied tibble and z are the unique documents, or in this case the question Id. Each w by z cell represents the presences of a w in document z. Additionally, there is a dimension v that contains information about the frequency of w in document z. This DTM is fed to the LDA function from the topicmodels package. The algorithm is ran 3 differnt times for k = {2, 3, 4}."
                ),
                
                box(
                  title = 'Step 5. Bind LDA Results to Questions',
                  "The results from the LDA include the probability that a word belongs to each of the identified k categories and the probability that the questionId's belong to each of the identified k categories. The latter is most interesting for modeling question data. The unsupervised learning task was to the Answers text data. The classification with the highest probability for each document (or question ID) is retained and joined to the Question text data. The results are shared below. "
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  title = 'LDA Classifications: What do you detect?',
                  footer = 'Do you agree with the classifications for k = 2, k =3, or k = 4?',
                  DT::dataTableOutput('que_cls_dt_id'))
              ),
              
              fluidRow(
                box(
                  title = "Question: ",
                  htmlOutput('que_cls_txt_id')),
                
                box(
                  title = "Answer: ",
                  htmlOutput('ans_cls_txt_id'))
              )
              
              ) 
            )
          )
        ),
      
      # Modeling Tab
      tabItem(
        
        tabName = 'models',
        
        fluidPage(
          
          #Defining tabs
          tabsetPanel(
            
            # Data Prep
            tabPanel(
              title = 'Data Prep for Modeling',
              fluidRow(
                box(
                  title = 'Going from Latent Dirichlet Allocation to Measuring Parameters',
                  width = 8,
                  "This modeling section builds upon the work done by the Latent Dirichlet Allocation algorithm. Now that there are classifications to predict, the next step is to generate parameters that can be modeled to predict the classifications. The data that will be used to generate parameters are the questions and the associated tags supplied by the stackoverflow user. The ultimate goal is to identify a model that is able to predict the classifcation of a question based solely on that atributes of that question. In this tab, I've documented the steps of producing each parameter."
                ),
                box(
                  title = ,
                  width = 4,
                  htmlOutput('models_data_prep_html_id')
                )
              ),
              
              # Step 1 of the data prepTF-IDF 
              fluidRow(
                box(
                  title = "Step 1. tf-idf",
                  width = 2,
                  "To asses the discriminating power of terms in the questions, the tf-idf stastitic is leveraed. The tf-idf is the product to two measurements about a document, and is commonly used for text mining and natural language processing. The two measures are Term Frequency (tf) and Inverse Document Frequency (idf). tf is how frequent a word occurs in the classification category (or document), and idf of a term is the natural log of the quotient between the number of total categories (k from the LDA algorithm) and the number of categories containing the term. Together, they describe the unique association of a term to a category. In the next step, I ranked the terms using this statistic and kept only the top 100 terms from each category. In the plot to right, we can see how the term frequency is logarithmically related to the rank."),
                
                box(
                  width = 7,
                  title = 'Rank vs Term Frequency',
                  plotlyOutput(
                    outputId = 'models_tf_rank_plot_id'
                  )
                ),
                
                box(
                  title = 'tf-idf Plot Parameters',
                  width = 3,
                  radioButtons(
                    'models_tf_plot_ctrl_id',
                    'LDA, k = ',
                    choices = c('2', '3', '4'),
                    selected = '2'
                  ),
                  checkboxInput(
                    'models_tf_plot_fitted_id',
                    'Fit relationship between Rank and Term Frequency?',
                    value = FALSE
                  ),
                  
                  
                  conditionalPanel(
                    condition = "input.models_tf_plot_fitted_id",
                    h5('The fitted line demonstrates the relationship lograithmic inverse logarithm relationship between term frequency and rank.'),
                    sliderInput(
                      'models_tf_plot_fitted_size_id', 
                      'Size of fitted line: ',
                      min = 0.5,
                      max = 3,
                      value = 1,
                      step = 0.25
                    ),
                    
                    sliderInput(
                      'models_tf_plot_fitted_alpha_id',
                      'Opacity of fitted line: ',
                      min = 0.1,
                      max = 1,
                      value = 1, 
                      step = 0.05
                    )
                  )
                  
                )
              ),
              
              # Step 2 of the data prep
              fluidRow(
                box(
                  title = "Step 2. Keeping Top Terms",
                  width = 2,
                  "This step is quite simple as I am just keeping the top 100 terms per category sorted by the tf-idf statistic. To the left are the top 100 terms per category. Use the table controls to view the different categories.", br(), "At this point, I have a bag of words to check against. With the bag of words I can ", em('count'), ' the number to terms from a question that are in the top words for cateogry ', em('k = i'), '. This is accomplished using a custom function that factors in the LDA category to search of the presence of a word. In practice, I would need to look at each bag of words for each category. For now, this is metric generation to asses and model the classifications.'
                ),
                
                box(
                  width = 7, 
                  title = "Top Terms Table: ",
                  DT::dataTableOutput('models_top_terms_tbl_id')
                ),
                
                box(
                  title = "Top Terms Table Controls",
                  width = 3,
                  radioButtons(
                    'models_tf_terms_ctrl_id',
                    'LDA, k = ',
                    choices = c('2', '3', '4'),
                    selected = '2'
                  )
                )
              ),
              
              # Step 3 of the data prep
              fluidRow(
                box(
                  title = 'Step 3. Repeat process for Question Tags',
                  width = 2,
                  htmlOutput('models_tags_blurb_id')
                ), 
                
                box(
                  width = 7,
                  title = 'Tag Prevalence',
                  DT::dataTableOutput('models_tags_prev_tbl_id')
                ),
                
                box(
                  width = 3,
                  title = 'Tag Table Parameters: ',
                  checkboxInput(
                    'models_tags_prev_tbl_chk_id',
                    'Count Tags? ',
                    value = FALSE
                  )
                )
              )
            ),
            
            # Data models discussion tab
            tabPanel(
              title = "Data Modeling",
              fluidRow(
                box(
                  title = 'k-NN Modeling Section',
                  width = 5,
                  htmlOutput(
                    'models_knn_text_id'
                    )
                  ),
                
                box(
                  title = "kNN Summary", 
                  width = 5,
                  DT::dataTableOutput(
                    'knn_dt_id'
                    ),
                  plotOutput(
                    'knn_plot_id'
                    )
                  ),
                
                box(
                  title = "Toggle kNN Summary Views",
                  width = 2,
                  radioButtons(
                    'models_knn_topic_id',
                    'LDA, k = ',
                    choices = c('2', '3', '4'),
                    selected = '2'
                  ),
                  selectInput(
                    'knn_toggl_id',
                    'Select summary output ',
                    choices = c('Table', 'CV Plot')
                  ),
                  conditionalPanel(
                    condition = "input.knn_toggl_id == 'Table'",
                    checkboxInput(
                      'knn_tbl_chkbx_id',
                      'Summarize table view?',
                      value = FALSE
                    )
                  ),
                  conditionalPanel(
                    condition = "input.knn_toggl_id == 'CV Plot'",
                    checkboxInput(
                      'knn_plot_chkbx_id',
                      'Plot trend line?',
                      value = FALSE
                      )
                    )
                  )
                ),
                fluidRow(
                box(
                  title = 'Boosted Tree',
                  width = 3,
                  htmlOutput('model_bst_disc_html_id')
                  ),
                
                box(
                  title = 'Boosted Tree Misclassification Rates',
                  width = 4,
                  DT::dataTableOutput(
                    'model_bst_mscls_tbl_id'
                  )
                ),
                
                box(
                  title = 'Boosted Tree Variable Importance table',
                  width = 4,
                  DT::dataTableOutput(
                    'model_bst_tbl_id'
                  )
                ),

                box(
                  title = 'LDA k Toggle',
                  width = 1,
                  radioButtons(
                    'models_bst_topic_id',
                    'LDA, k = ',
                    choices = c('2', '3', '4'),
                    selected = '2'
                    )
                  )
                )
              )
            )
          )
        ),
      
      # User tab
      tabItem(
        tabName = 'user_int',
        
        fluidPage(
          fluidRow(
            box(
              width = 12,
              title = 'Submit a question!',
              "Try submitting your own question either by entering in the text box below or by sumbitting a link to a question already on stackoverflow. For best results, make sure your question is R related. Once your question is submitted set the parameters and hit ", tags$b("Classify"), " to run the boosted tree model. It might take a few minutes."
            )
          ),
          
          # Submit question phase
          fluidRow(
            box(
              width = 2,
              title = 'Toggle: Question or Link',
              selectInput(
                'que_tgl_input_id',
                'Select: ',
                choices = c('Manual input')#, 'Link')
              )
            ),
            box(
              width = 3,
              title = 'Submit Question: ',
              #conditionalPanel(
                #condition = "input.que_tgl_input_id == 'Manual input'",
                textInput(
                  'que_man_submit_id',
                  'Enter question: ',
                  value = ' '
                ),
              br(),
              br(),
              'When submitting your question you are allowed to it. What topics would you list in your tags to ensure other users find your questions? Ensure you tags are separated by spaces.',
              checkboxInput(
                'que_man_tags_chk_id',
                'Submit tags?'
              ),
              
              conditionalPanel(
                condition = 'input.que_man_tags_chk_id',
                textInput(
                  'que_man_tags_id',
                  'List tags: ',
                  value = ' '
                )
              )
              
              #),
              # conditionalPanel(
              #   condition = "input.que_tgl_input_id == 'Link'",
              #   textInput(
              #     'que_link_submit_id',
              #     'Copy in link',
              #     value = ' '
              #   )
              # )
            ),
            
            box(
              width = 2,
              title = '',
              radioButtons(
                'que_man_topic_id',
                'Select LDA, k = ',
                choices = c('2', '3', '4'),
                selected = '2'
              ),
              actionButton(
                'que_button_id',
                label = 'Submit Question'
              )
            ),
            box(
              width = 1,
              textOutput(
                'que_pred_res_id'
              )
            )
            
          ),
          
          fluidRow(
            box(
              width = 9,
              title = 'Similar Questions:',
              DT::dataTableOutput('que_dt_id')),
            box(
              title = 'Download similar questions: ',
              width = 3,
              downloadButton("downloadData", "Download")
            )
          ),
          fluidRow(
            box(
              title = "Question: ",
              htmlOutput('que_pred_txt_id')),
            
            box(
              title = "Answer: ",
              htmlOutput('ans_pred_txt_id'))
          )
        )
      )
      
      )
    )
  )
