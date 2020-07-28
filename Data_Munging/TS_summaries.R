library(tidyverse)
library(lubridate)

ans <- read_csv('../../Raw_data/Answers.csv')
que <- read_csv('../../Raw_data/Questions.csv')

ans_sum_ts <- ans %>%
  select(CreationDate, Score) %>%
  mutate(posted = as_date(CreationDate),
         posted = round_date(posted, unit = 'month')) %>%
  group_by(posted) %>%
  summarise(Total_Ans = n(),
            Mean_Score = mean(Score, na.rm = T),
            Med_Score = median(Score, na.rm = T),
            Max_Score = max(Score, na.rm = T)) 

que_sum_ts <- que %>%
  select(CreationDate, Score) %>%
  mutate(posted = as_date(CreationDate),
         posted = round_date(posted, unit = 'month')) %>%
  group_by(posted) %>%
  summarise(Total_Ans = n(),
            Mean_Score = mean(Score, na.rm = T),
            Med_Score = median(Score, na.rm = T),
            Max_Score = max(Score, na.rm = T))

write_csv(ans_sum_ts, path = '../../Prelim_Results/Ans_Sum_TS.csv')
write_csv(que_sum_ts, path = '../../Prelim_Results/Que_Sum_TS.csv')
