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
            ),
          )
        )#,
      
      # menuItem(
      #   
      # )
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
                  title = 'Q&A Data Preview:',
                  DT::dataTableOutput('ans_dt_id')),
                
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
              title = "Latent Dirichlet Allocation",
              
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
                  title = 'LDA Classifications: What do you detect?',
                  footer = 'Do you agree with the classifications for k = 2, k =3, or k = 4?',
                  DT::dataTableOutput('que_cls_dt_id')),
                
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
        )
      
      )
    )
  )
