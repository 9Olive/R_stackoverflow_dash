library(tidyverse)

answers_pre <- read_csv('../../Prelim_Results/QandA.csv', n_max = 100)
ans_ts <- read_csv('../../Prelim_Results/Ans_Sum_TS.csv')
que_ts <- read_csv('../../Prelim_Results/Que_Sum_TS.csv')
theme_set(theme_bw())

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data table output:

ans_dt <- answers_pre %>%
  filter(IsAcceptedAnswer == 'TRUE') %>%
  select(ParentId, `Answer Date` = CreationDate_ans, `Question Score` = Score_ans, `Answer Score` = Score_que, IsAcceptedAnswer, Title) 
# head(20) %>%
  

ans_txt <- answers_pre %>%
  pull(Body_ans)

tit_txt <- answers_pre %>%
  pull(Body_que)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For plot output

dict <- data.frame(user = c("Total Inquiries or Replies",  "Mean Score", "Median Score",  "Max Score"),
                   bknd = c("Total_Ans", "Mean_Score", "Med_Score", "Max_Score"))

# b4 <- answers_pre$Body_ans[1]
# aft <- str_replace_all(b4, pattern = html_regex, ' ') %>%
#   str_replace_all(pattern = sw_regex, ' ') %>%
#   str_replace_all(pattern = sw_regex, ' ') %>%
#   str_replace_all(pattern = sw_regex, ' ') %>%
#   str_trim()
# 
# sw_regex <- paste0('[:space:]',paste0(actual_sw, collapse = '[:space:]|[:space:]'), '[:space:]', collapse = '') #%>% 
#   # str_pad(width = str_length(.)+1, side = 'left') %>% str_pad(width = str_length(.)+1, side = 'right')
# 
# actual_sw
# 
# tibby <- tibble(questionId = answers_pre$ParentId[1],
#        body = aft) %>%
#   unnest_tokens(word, body) %>%
#   count(word, questionId) %>%
#   arrange(desc(n))
# 
# write_csv(tibby, path = paste0(getwd(), '/lda_tidy_ex.csv'))
