library(gbm)
library(tidyverse)

load("classifier_mdl.RData")

class2words <- read_csv('../Modeling/class2words.csv', col_types = 'ncnddd')
class3words <- read_csv('../Modeling/class3words.csv', col_types = 'ncnddd')
class4words <- read_csv('../Modeling/class4words.csv', col_types = 'ncnddd')

rfunc <- read_csv('https://raw.githubusercontent.com/v-kozhevnikov/GitHub_R_commands/master/data/top_2000_functions.csv')
func <- unique(rfunc$content)
pkg <- unique(rfunc$library) 

count_k_topics <- function(user_text, k) {
  
  if (as.numeric(k) == 2) {
    
    t1w <- class2words %>%
      filter(topic == 1) %>%
      pull(word)
    
    t2w <- class2words %>%
      filter(topic == 2) %>%
      pull(word)
    
    t1w_vote <- str_count(user_text, pattern = paste0('(?<=[:space:])', t1w, '(?=[:space:])', collapse = '|'))
    
    t2w_vote <- str_count(user_text, pattern = paste0('(?<=[:space:])', t2w, '(?=[:space:])', collapse = '|'))
    
    max(t1w_vote, t2w_vote)
    
  } else if (as.numeric(k) == 3) {
    
    t1w <- class3words %>%
      filter(topic == 1) %>%
      pull(word)
    
    t2w <- class3words %>%
      filter(topic == 2) %>%
      pull(word)
    
    t3w <- class3words %>%
      filter(topic == 3) %>%
      pull(word)
    
    t1w_vote <- str_count(user_text, pattern = paste0('(?<=[:space:])', t1w, '(?=[:space:])', collapse = '|'))
    
    t2w_vote <- str_count(user_text, pattern = paste0('(?<=[:space:])', t2w, '(?=[:space:])', collapse = '|'))
    
    t3w_vote <- str_count(user_text, pattern = paste0('(?<=[:space:])', t3w, '(?=[:space:])', collapse = '|'))
    
    max(t1w_vote, t2w_vote, t3w_vote)
    
  } else {
    
    t1w <- class3words %>%
      filter(topic == 1) %>%
      pull(word)
    
    t2w <- class3words %>%
      filter(topic == 2) %>%
      pull(word)
    
    t3w <- class3words %>%
      filter(topic == 3) %>%
      pull(word)
    
    t4w <- class3words %>%
      filter(topic == 4) %>%
      pull(word)
    
    t1w_vote <- str_count(user_text, pattern = paste0('(?<=[:space:])', t1w, '(?=[:space:])', collapse = '|'))
    
    t2w_vote <- str_count(user_text, pattern = paste0('(?<=[:space:])', t2w, '(?=[:space:])', collapse = '|'))
    
    t3w_vote <- str_count(user_text, pattern = paste0('(?<=[:space:])', t3w, '(?=[:space:])', collapse = '|'))    
    
    t4w_vote <- str_count(user_text, pattern = paste0('(?<=[:space:])', t4w, '(?=[:space:])', collapse = '|'))      
    
    } 
  }

count_pkgs <- function(user_text) {
  regex_filter <- paste0('(?<=[:space:])', pkg, '(?=[:space:]|\\()', collapse = '|')
  
  return(str_count(user_text, pattern = regex_filter))
}

count_func <- function(user_text) {
  regex_filter <- paste0('(?<=[:space:])', func, '(?=[:space:]|\\()', collapse = '|')
  
  return(str_count(user_text, pattern = regex_filter))
}

count_func2 <- function(user_tags) {
  x <- rfunc %>%
    filter(library %in% unlist(str_split(user_tags, ' '))) %>%
    nrow()
  return(x)
}

rfunc_pkg <- rfunc %>%
  distinct(library)

count_pkg2 <- function(user_tags) {
  x <- rfunc_pkg %>%
    filter(library %in% unlist(str_split(user_tags, ' '))) %>%
    nrow()
  return(x)
}

user_pred <- function(user_text, user_tags = '', k) {
  
  if (user_tags != '') {
    ptbl <- data.frame("kn_class_n_words" = count_k_topics(user_text, k),
                     "n_func" = count_func(user_text),
                     "n_pkg" = count_pkgs(user_text),
                     "n_tags" = length(unlist(str_split(user_tags, ' '))),
                     "n_tag_func" = count_func2(user_tags),
                     "n_tag_pkg" = count_pkg2(user_tags)
                     )
  } else { 
    
    ptbl <- data.frame("kn_class_n_words" = count_k_topics(user_text, k),
                       "n_func" = count_func(user_text),
                       "n_pkg" = count_pkgs(user_text),
                       "n_tags" = 0,
                       "n_tag_func" = 0,
                       "n_tag_pkg" = 0
                       )
    }
  
  colnames(ptbl)[1] <- paste0('k', k, '_class_n_words')
  
  
  if (as.numeric(k) == 2) {
    
    preds <- predict(boostFit_k2, 
                     ptbl,
                     n.trees = best_iter_k2, 
                     type = 'response')
    
  } else if (as.numeric(k) == 3) {

    preds <- predict(boostFit_k3, 
                     ptbl,
                     n.trees = best_iter_k3, 
                     type = 'response')    
    
  } else {
    
    preds <- predict(boostFit_k4, 
                     ptbl,
                     n.trees = best_iter_k4, 
                     type = 'response')
    
  }
  
  return(which.max(preds))
}
