rm(list = ls())
library(tidyverse)
library(ggplot2)

df_trends <- read.csv("~/Desktop/info201/assignment-3---incarceration-YanRemes/incarceration_trends.csv")
df_jurisdiction <- read.csv("~/Desktop/info201/assignment-3---incarceration-YanRemes/incarceration_trends_jail_jurisdiction.csv")

# What is the average value of my variable across all the counties (in the current year)?
recent_jail_pop <- filter(
  df_jurisdiction,
  year == max(year)
)
recent_jail_pop <- na.omit(recent_jail_pop)

recent_avg_black_jail_pop <- mean(recent_jail_pop$black_jail_pop)

recent_avg_white_jail_pop <- mean(recent_jail_pop$white_jail_pop)

# Where is my variable the highest / lowest?
county_with_highest_black_pop_15to64 <- df_trends[which.max(df_trends$black_pop_15to64), 'county_name']
county_with_highest_white_pop_15to64 <- df_trends[which.min(df_trends$white_pop_15to64), 'county_name']

# How much has my variable change over the last N years?
date_2000 <- filter(df_trends, year == 2000, county_name == "New York County")
date_2018 <- filter(df_trends, year == 2018, county_name == "New York County")
change_In_black_pop <- date_2018$black_pop_15to64 - date_2000$black_pop_15to64

# Trends over time chart
highest_in_each_state <- 
  df_trends %>%
  group_by(state) %>%
  filter(year == max(year)) %>%
  filter(black_pop_15to64 == max(black_pop_15to64)) %>%
  pull(state)

keep_5_State <- highest_in_each_state[1:5]
states <- df_trends %>%
  filter(state %in% keep_5_State) %>%
  select(year, state, black_pop_15to64) %>%
  na.omit() %>%
  group_by(year, state) %>%
  summarise(black_pop_15to64 = sum(black_pop_15to64))

trends_over_time <- ggplot(states, aes(x = year, y = black_pop_15to64, color = state)) +
  geom_line() + ylab("Black population from 15 to 64") +
  ggtitle("Black population over time")

# Variable comparison chart
keep_5_State <- highest_in_each_state[1:5]
state <- df_trends %>%
  filter(state %in% keep_5_State) %>%
  select(year, state, black_pop_15to64, white_pop_15to64) %>%
  na.omit() %>%
  group_by(year, state) %>%
  summarise(black_pop_15to64 = sum(black_pop_15to64), white_pop_15to64 = sum(white_pop_15to64))

variable_comparison <- ggplot(state) +
  geom_line(aes(x = year, y = black_pop_15to64, color = "black_pop_15to64")) + 
  geom_line(aes(x = year, y = white_pop_15to64, color = "white_pop_15to64")) +
  facet_grid(. ~ state) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_y_continuous(labels = function(y) format(y, scientific = FALSE)) +
  ylab("White vs black popluation from 15 to 64") +
  scale_colour_discrete("race", labels = c('black population from 15 to 64', 'white population from 15 to 64')) +
  ggtitle("White vs black popluation over time")

# Map
df_trends <- df_trends %>%
  mutate(county_name = str_remove_all(county_name, " County"))
df_trends$county_name <- tolower(df_trends$county_name)
df_trends <- df_trends[, c('county_name', 'black_jail_pop')]

map_data <- map_data("county")
colnames(map_data)[6] <- "county_name"
map_data <- left_join(map_data, df_trends, by = "county_name")
map_data <- map_data %>% filter(!is.na(map_data$black_jail_pop))

map <- ggplot(map_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = black_jail_pop), color = "gray", size = 0.1) +
  scale_fill_continuous(limits = c(0, max(map_data$black_jail_pop)), na.value = "white", low="yellow", high="red") +
  guides(col=guide_legend("Black jail population")) + 
  ggtitle("Black jail population in the United States")

map <- map +
  theme(
    axis.line = element_blank(), # remove axis lines
    axis.text = element_blank(), # remove axis labels
    axis.ticks = element_blank(), # remove axis ticks
    axis.title = element_blank(), # remove axis titles
    plot.background = element_blank(), # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank() # remove border around plot
  )
