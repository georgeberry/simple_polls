library(ggplot2)
library(dplyr)

TODAY = 310
LOESS_VALS = c(0.7, 0.75, 0.8, 0.85)

df = read.csv('polls.csv')

national_df = df[df$state == 'US',]
state_df = df[df$state != 'US',]
state_df = state_df[state_df$state != 'WY',]

#### lowess regression ########################################################
mod = loess(dem_adv ~ doy, national_df, span=0.75, control = loess.control(surface = "direct"))
predict(mod, national_df$doy)
predict(mod, matrix(TODAY))

p1 = ggplot(national_df, aes(x=doy, y=dem_adv)) + stat_smooth(span=.65) + geom_point() +
  labs(x='Day of year', y='Democratic advantage') +
  geom_hline(aes(yintercept=0), linetype='dashed')+
  theme_bw()
p2 = ggplot(national_df, aes(x=doy, y=dem_adv)) + stat_smooth(span=.85) + geom_point() +
  labs(x='Day of year', y='Democratic advantage') +
  geom_hline(aes(yintercept=0), linetype='dashed') +
  theme_bw()
multiplot(p1, p2, cols=2)


#### state aggregation ########################################################

adjust = function(row, loess_mod) {
  dem_adv = as.numeric(row[['dem_adv']])
  poll_day = as.numeric(row[['doy']])
  # how much have polls changed since the poll day
  poll_movement = predict(loess_mod, TODAY) - predict(loess_mod, poll_day)
  return(dem_adv + poll_movement)
}

raw_median = state_df %>%
  group_by(state) %>%
  summarize(median = median(dem_adv),
            smoothing_val = 'None')

#### loop #####################################################################

aggregation_df = data.frame(state = c(), adjusted_dem_adv = c(), smoothing_val = c())

for (val in LOESS_VALS) {
  mod = loess(dem_adv ~ doy, national_df, span=val, control = loess.control(surface = "direct"))
  adjusted_dem_adv = apply(state_df, 1, adjust, loess_mod=mod)
  adjusted_df = data.frame(
    state = state_df$state,
    adjusted_dem_adv = adjusted_dem_adv,
    smoothing_val = val
  )
  aggregation_df = rbind(aggregation_df, adjusted_df)
}


#### aggregate  ###############################################################

agg_df = aggregation_df %>%
  group_by(smoothing_val, state) %>%
  summarize(median=median(adjusted_dem_adv))

agg_df$smoothing_val = as.character(agg_df$smoothing_val)

agg_df = rbind(as.data.frame(agg_df), as.data.frame(raw_median))

agg_df$smoothing_val = factor(agg_df$smoothing_val, levels=c('None', 0.85, 0.8, 0.75, 0.7))

p3 = ggplot(agg_df, aes(x = factor(state), y = median, color=factor(smoothing_val))) +
  geom_point() +
  labs(x='State', y='Democratic Advantage') +
  geom_hline(aes(yintercept=0), linetype='dashed') +
  scale_color_discrete(name='Smoothing value') +
  theme_bw() +
  geom_point()
