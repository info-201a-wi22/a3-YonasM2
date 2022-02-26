library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)
install.packages("patchwork")
library(patchwork)
install.packages("ggmap")
library(ggmap)
Data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
View(Data)

#1 What year had the highest black jail population 
highest_black_year <- filter(Data, black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>% pull(year)

Data_trends_map <- filter(Data,
                          county_name == "Los Angeles County")
View(Data_trends_map)

Trend_over_time_chart_black <- ggplot(Data_trends_map, aes(x = year, y = black_jail_pop)) +
  geom_line(col='black')+
  labs(title = "Black Jail population in Los Angeles County", x = "Year",
       y = "Black Population")
plot(Trend_over_time_chart_black)


Variable_comparsion_black_white_NY <- ggplot(Data_trends_map, aes(x = black_pop_15to64 , y=black_jail_pop)) +
  geom_point(aes(col = county_name)) + 
  geom_smooth() +
  labs(title = "Comparing the case number of black and white people in the Jail",
       x = "The white people population in jail", y="The black people population in jail")

Variable_comparsion_black_white_NY
