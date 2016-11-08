library(ggplot2)
library(dplyr)

df = read.csv('polls.csv')

pred_df = df %>%
  group_by(state) %>%
  mutate(count = n()) %>%
  filter(count > 5) %>%
  summarize(prediction=median(dem_adv))

states = as.character(pred_df$state)
states = states[order(pred_df$prediction)]

pred_df$state = factor(pred_df$state,
                       levels=states)

ggplot(pred_df, aes(x=state, y=prediction)) +
  geom_hline(aes(yintercept=0), linetype='dashed') +
  geom_point() +
  theme_bw()
