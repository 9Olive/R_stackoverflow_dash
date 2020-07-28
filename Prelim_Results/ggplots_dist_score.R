
```{r}
ans_top_brkdwn %>%
  filter(Score <= quantile(Score, 0.99)) %>%
  ggplot(aes(x = Score)) +
  geom_histogram(aes(fill = IsAcceptedAnswer), color = 'black') +
  facet_grid(IsAcceptedAnswer~., labeller = 
               labeller(
                 IsAcceptedAnswer = c('FALSE' = 'Not Accepted Answers', 'TRUE' = 'Accepted Answers')
               )
  ) +
  scale_x_continuous(breaks = pretty_breaks(20)) +
  scale_y_continuous(breaks = pretty_breaks(7),
                     label = number_format(big.mark = ',')) +
  labs(title = "Histogram of Top Scores of Accepted and Non-Accepted Answers",
       caption = "Only the top score pulled from each 'Solved' question's accepted and non-accepted answers.") +
  theme(legend.position = 'None', text = element_text(size = 16))
```

Maybe the user wants to see them overlaid?
  
  ```{r}
ans_top_brkdwn %>%
  filter(Score <= quantile(Score, 0.99)) %>%
  mutate(IsAcceptedAnswer = ifelse(IsAcceptedAnswer == TRUE, 'Yes', 'No'),
         IsAcceptedAnswer = factor(IsAcceptedAnswer, levels = c('Yes', 'No'))) %>%
  ggplot(aes(x = Score)) +
  geom_histogram(aes(fill = IsAcceptedAnswer), color = 'black',
                 position = 'identity') +
  scale_x_continuous(breaks = pretty_breaks(20)) +
  scale_y_continuous(breaks = pretty_breaks(7),
                     label = number_format(big.mark = ',')) +
  labs(title = "Histogram of Top Scores of Accepted and Non-Accepted Answers",
       caption = "Only the top score pulled from each 'Solved' question's accepted and non-accepted answers.",
       fill = 'Is Answer Accepted') +
  theme(text = element_text(size = 16)) +
  scale_fill_manual(values = c('#00BFC4', '#F8766D'))
```

What are the top scoring questions?
  
  Sorting by `score`

```{r}
que %>%
  arrange(desc(Score)) %>%
  select(CreationDate, Title, Score) %>%
  head(500) %>% 
  DT::datatable()
```

- Most of the top questions are from 2009 - 2011. 


