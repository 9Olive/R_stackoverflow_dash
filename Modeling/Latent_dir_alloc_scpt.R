# Generating Unsupervised Learning Classifications:
# Method: Latent Dirichlet Allocation

start.time <- Sys.time()

print('Step 1. Loading packages:')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(NLP)
library(scales)
library(tidytext)
library(tm)
library(httr)
library(rvest)
library(topicmodels)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

print('Elapsed time: ')
Sys.time() - start.time
print("Step 2. Loading data:")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
answers <- read_csv('C:\\Users\\JDOli\\Documents\\Projects\\Career\\Masters in Statistics\\ST558\\Project 3\\Raw_data/Answers.csv')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

print('\n Running time: ')
Sys.time() - start.time
print('\n Step 3. Setting up regular expressions for cleaning: ')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ans <- ( accepted <- answers %>% 
           filter(IsAcceptedAnswer == 'TRUE')) %>%
  pull(Body)

## Pull down HTML tags
session <- html_session('https://www.w3schools.com/TAGs/')
html_table <- html_table(session)
html_tags <- tibble(tags = unlist(html_table))
html_tags <- html_tags %>%
  filter(str_detect(tags, pattern = '^<')) %>%
  rename(open_tags = tags) %>%
  mutate(close_tags = paste0(str_sub(open_tags, end = 1L), '/', str_sub(open_tags, start = 2L)))

html_tag_vctr <- c(html_tags$open_tags, html_tags$close_tags, use.names = F)

# Define Regular Expression
html_regex <- paste0('\\n|<img src=.+>|<a href=.+>|\\(|\\)|\\{|\\}|,|',
                     paste0(html_tag_vctr, collapse = '|'), collapse = '')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

print('\n Running time: ')
Sys.time() - start.time
print('\n Step 4. Cleaning up text data: ')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Clean text data up
ans_clean <- str_replace_all(ans, pattern = html_regex, ' ') %>% 
  str_trim()
# Set up data to be used
ans_clean_tdy <- tibble(questionId = accepted$ParentId, 
                        body = ans_clean) %>%
  unnest_tokens(word, body) %>%
  count(word, questionId)

# ans_sw <- ans_clean_tdy %>%
#   filter(word %in% stopwords()) %>%
#   arrange(desc(n)) %>%
#   distinct(word) %>%
#   pull(word)

# Define stopwords that are actually useful in coding context
keep_sw <- c('from', 'if', 'all', 'by', 'i', 'in', 'and', 'not', 
             'for', 'when', 'with', 'between', 'which', 'or', 
             'any', 'where', 'while', 'then')

# Actual stopwords
actual_sw <- c(stopwords()[-which(stopwords() %in% keep_sw)], 'x', 'lt')

# Filter out acutal stopwords
# ans_clean_tdy %>%
#   filter(!word %in% actual_sw) %>%
#   arrange(desc(n)) %>%
#   distinct(word, .keep_all = T)

# Last bit of cleaning
ans_clean_tdy <- ans_clean_tdy %>%
  filter(!word %in% actual_sw) %>%
  filter(str_detect(word, pattern = '^[:digit:]+', negate = T)) %>%
  filter(str_detect(word, pattern = '_+', negate = T))

# Generate document term matrix
(ans_dtm <- ans_clean_tdy %>%
    cast_dtm(questionId, word, n))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

print('\n Running time: ')
Sys.time() - start.time
print('\n Step 5. Beginning Latent Dirichlet Allocation: ')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print('\n LDA, k = 2')
# Run LDA. 
ans_LDA2 <- LDA(ans_dtm, k = 2, control = list(seed = 49))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print('\n Running time: ')
Sys.time() - start.time
print('\n LDA, k = 3')
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ans_LDA3 <- LDA(ans_dtm, k = 3, control = list(seed = 49))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

print('\n Running time: ')
Sys.time() - start.time
print('\n LDA, k = 4')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ans_LDA4 <- LDA(ans_dtm, k = 4, control = list(seed = 49))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

print('\n Running time: ')
Sys.time() - start.time
print('\n LDA, k = 5')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ans_LDA5 <- LDA(ans_dtm, k = 5, control = list(seed = 49))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

print('\n Running time: ')
Sys.time() - start.time
print('\n LDA, k = 6')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ans_LDA6 <- LDA(ans_dtm, k = 6, control = list(seed = 49))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

print('\n Running time: ')
Sys.time() - start.time
print('\n Cleaning & Saving outputs: ')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

que <- read_csv('C:\\Users\\JDOli\\Documents\\Projects\\Career\\Masters in Statistics\\ST558\\Project 3\\Raw_data/Questions.csv')

# Define data manipulation function
que_classifier <- function(lda_res, n = 5) {
  cluster <- tidy(lda_res, matrix = 'gamma') %>%
    group_by(document) %>%
    top_n(1, gamma) %>%
    ungroup() %>%
    right_join(mutate(que, Id = as.character(Id)), by = c('document' = 'Id')) %>%
    arrange(desc(gamma), topic) 
}

solved_que_class <- inner_join(que_classifier(ans_LDA2, n = 2), 
                               que_classifier(ans_LDA3, n = 3), 
                               by = c("document", "OwnerUserId", "CreationDate", "Score", "Title", "Body"),
                               suffix = c('.k2', '')) %>%
  inner_join(que_classifier(ans_LDA4, n = 4), 
             by = c("document", "OwnerUserId", "CreationDate", "Score", "Title", "Body"),
             suffix = c('.k3', '.k4')) # %>%
  # inner_join(que_classifier(ans_LDA5, n = 5), 
  #            by = c("document", "OwnerUserId", "CreationDate", "Score", "Title", "Body"),
  #            suffix = c('.k4', '')) %>% 
  # inner_join(que_classifier(ans_LDA6, n = 6), 
  #            by = c("document", "OwnerUserId", "CreationDate", "Score", "Title", "Body"),
  #            suffix = c('.k5', '.k6'))


# Write the output to .csv
write_csv(solved_que_class, path = 'C:\\Users\\JDOli\\Documents\\Projects\\Career\\Masters in Statistics\\ST558\\Project 3\\Prelim_Results/class_questions.csv')

rm(answers, keep_sw, ans_clean_tdy, html_regex, html_tag_vctr, html_tags, html_table, ans)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

print('\n Total Elapsed Time: ')
Sys.time() - start.time

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~