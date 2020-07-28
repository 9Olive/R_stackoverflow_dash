library(tidyverse)

# Backend code for generating plots!

tf_idf_plotk2 <- read_csv('../Prelim_Results/models_tf_k2.csv', col_types = c('dfi'))
tf_idf_plotk3 <- read_csv('../Prelim_Results/models_tf_k3.csv', col_types = c('dfi'))
tf_idf_plotk4 <- read_csv('../Prelim_Results/models_tf_k4.csv', col_types = c('dfi'))

lm.plot2 <- lm(log10(`tf`) ~ log10(rank), data = tf_idf_plotk2 %>% filter(between(rank, quantile(rank, 0.2), quantile(rank, 0.8))))

g2 <- tf_idf_plotk2 %>%
  ggplot(aes(rank, `tf`, color = topic)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = F) +
  scale_x_log10(labels = scales::number_format(big.mark = ',')) +
  scale_y_log10(labels = scales::number_format(big.mark = ',')) +
  labs(x = 'log(Term Rank)', y = 'log(Term Frequency)') 

lm.plot3 <- lm(log10(`tf`) ~ log10(rank), data = tf_idf_plotk3 %>% filter(between(rank, quantile(rank, 0.2), quantile(rank, 0.8))))

g3 <- tf_idf_plotk3 %>%
  ggplot(aes(rank, `tf`, color = topic)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = F) +
  scale_x_log10(labels = scales::number_format(big.mark = ',')) +
  scale_y_log10(labels = scales::number_format(big.mark = ','))  +
  labs(x = 'log(Term Rank)', y = 'log(Term Frequency)') 

lm.plot4 <- lm(log10(`tf`) ~ log10(rank), data = tf_idf_plotk4 %>% filter(between(rank, quantile(rank, 0.2), quantile(rank, 0.8))))

g4 <- tf_idf_plotk4 %>%
  ggplot(aes(rank, `tf`, color = topic)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = F) +
  scale_x_log10(labels = scales::number_format(big.mark = ',')) +
  scale_y_log10(labels = scales::number_format(big.mark = ',')) +
  labs(x = 'log(Term Rank)', y = 'log(Term Frequency)')
