### LDA Results Summarization for Dashboard
library(tidyverse)

class_questions <- read_csv("~/Projects/Career/Masters in Statistics/ST558/Prelim_Results/class_questions.csv")
Answers <- read_csv("~/Projects/Career/Masters in Statistics/ST558/Raw_data/Answers.csv")

class_questions %>%# head()
  arrange(desc(gamma.k4), desc(gamma.k3), desc(gamma.k2)) %>%
  select(QuestionId = document, 
         Title, 
         Topic_k2 = topic.k2,
         Topic_k3 = topic.k3,
         Topic_k4 = topic.k4,
         Body) %>%
  left_join((Answers %>% filter(IsAcceptedAnswer == 'TRUE') %>% select(ParentId, Body)), 
            by = c('QuestionId' = 'ParentId'), 
            suffix = c('_Que', '_Ans')) %>%
  head(200) %>%
  write_csv('~/Projects/Career/Masters in Statistics/ST558/Prelim_Results/lda_class_que.csv')