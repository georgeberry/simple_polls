library(ggplot2)
library(dplyr)

df = read.csv('polls.csv')

state_df = df[df$state != 'US',]
national_df = df[df$state == 'US',]

state_predictions = state_df %>%
  group_by(state) %>%
  summarize(median=median(dem_adv))

national_prediction = national_df %>%
  group_by(state) %>%
  summarize(median=median(dem_adv))

agg_df = rbind(
  as.data.frame(state_predictions),
  as.data.frame(national_prediction))

agg_df$state = factor(
  agg_df$state,
  levels=agg_df$state[order(agg_df$median)])

ggplot(agg_df, aes(x=state, y=median)) +
  geom_hline(aes(yintercept=0), linetype='dashed') +
  geom_point() +
  theme_bw()
