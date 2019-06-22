library(ggplot2)
library(dplyr)
library(tidyverse)

#####################################
# Distribution of effect of winning #
#####################################

prepped_data <- read.csv("/Users/${user}/workspace/ws_data.csv")
prepped_data <- mutate(prepped_data, year_of_ws_increase = (home_attendance/prior_3year_attendance)-1)
prepped_data <- mutate(prepped_data, year_post_ws_increase = (next_year_attendance/prior_3year_attendance)-1)

# Calculate the distribution of win increases
dens <- density(prepped_data$year_post_ws_increase)
df <- data.frame(x=dens$x, y=dens$y)
probs <- c(0, 0.25, 0.5, 0.75, 1)
quantiles <- quantile(prepped_data$year_post_ws_increase, prob=probs)
df$quant <- factor(findInterval(df$x,quantiles))

# Plot distribution
ggplot() +
  geom_histogram(mapping = aes(x = prepped_data$year_post_ws_increase)) +
  geom_line(mapping = aes(x = df$x, df$y)) +
  geom_ribbon(aes(x = df$x, ymin=0, ymax=df$y, fill=df$quant)) +
  scale_x_continuous(breaks=quantiles,
                     labels=c(paste0(round(quantiles[[1]],2)*100,"%"),
                              paste0(round(quantiles[[2]],2)*100,"%"),
                              paste0(round(quantiles[[3]],2)*100,"%"),
                              paste0(round(quantiles[[4]],2)*100,"%"),
                              paste0(round(quantiles[[5]],2)*100,"%")
                              )) +
  scale_fill_brewer(guide="none") +
  geom_histogram(fill="white") +
  xlab("Increase in attendance, vs. 3-year prior avg.") +
  ylab("Team wins") +
  labs(title = "Winning the World Series increases home attendance the next season",
       subtitle = "Increase in avg. home attendance, over 3-year baseline",
       caption = "Source: Lahman’s Baseball Database")



####################################
# Timeseries leading up to the win #
####################################

ts_data <- tbl_df(read.csv("/Users/${user}/workspace/ws_ts.csv"))

# Get average team attendance in the WS period for each win:
win_avg_attnd <- ts_data %>% filter(t_to_ws < 0) %>% group_by(year_of_win) %>%
                             summarize(avg_attendance = mean(attendance) / mean(home_games))
ts_data <- ts_data %>% inner_join(x=ts_data, y=win_avg_attnd, by="year_of_win", suffix=c(".ts", ".compd_avg"))

ts_data %>% group_by(t_to_ws) %>%
  summarize(avg_attendance_index = (mean(attendance) / mean(home_games)) / mean(avg_attendance))
    #   t_to_ws avg_attendance_index
    #     <int>                <dbl>
    # 1      -3                0.983
    # 2      -2                0.995
    # 3      -1                1.05
    # 4       0                1.17
    # 5       1                1.21
ts_plot <- mutate(ts_data, avg_attendance_index = (attendance / home_games)/avg_attendance)


ggplot(data=ts_plot, aes(x=t_to_ws, y=avg_attendance_index, ymin=0, ymax=2.5)) +
  geom_point() +
  geom_smooth(method="loess", span = 0.8) +
  xlab("Years before/after World Series win") +
  ylab("Average home attendance (indexed)") +
  labs(title = "The effect of winning the World Series on home game attendance",
       subtitle = "Avg attendance for teams w/o a win in prior 3 years",
       caption = "Source: Lahman’s Baseball Database")


# TODO: incorporate size of historic baseball parks as % of capacity.
# library(rvest)
# bbParkUrl <- "https://en.wikipedia.org/wiki/List_of_current_Major_League_Baseball_stadiums"

# divs <- bbParkUrl %>%
#         read_html %>%
#         html_nodes("table")
# html_table(divs[2])
