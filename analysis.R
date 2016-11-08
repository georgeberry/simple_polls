library(ggplot2)
library(dplyr)

#### ggplot multiplot function ################################################

# this function is from
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)
# included here for completeness

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }
 if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#### constants and data loading ###############################################

TODAY = 311
LOESS_VALS = c(0.7, 0.75, 0.8, 0.85)

df = read.csv('polls.csv')
df = df[df$doy <= TODAY,] # remove a dozen or so polls from last year

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
agg_df$state = factor(agg_df$state,
                      levels=c('GA', 'AZ', 'IA', 'OH', 'NC', 'NV', 'FL', 'WI', 'PA', 'NH', 'CO', 'VA'))

p3 = ggplot(agg_df, aes(x = factor(state), y = median, color=factor(smoothing_val))) +
  geom_point() +
  labs(x='State', y='Democratic Advantage') +
  geom_hline(aes(yintercept=0), linetype='dashed') +
  scale_color_discrete(name='Smoothing value') +
  theme_bw() +
  geom_point()
