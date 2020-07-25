library(tidyverse)

answers_pre <- read_csv('../Prelim_Results/ggAns.csv', n_max = 100)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data table output:

ans_dt <- answers_pre %>%
  filter(IsAcceptedAnswer == 'TRUE') %>%
  select(Id, OwnerUserId, CreationDate, ParentId, Score, IsAcceptedAnswer) 
# head(20) %>%
  

ans_txt <- answers_pre %>%
  filter(IsAcceptedAnswer == 'TRUE') %>%
  select(Id, Body)

