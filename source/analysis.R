library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)
library(patchwork)
library(ggmap)
incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
View(incarceration_data)

#1 What year had the highest black jail population 
highest_black_year <- filter(incarceration_data, black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>% pull(year)

highest_year <- filter(incarceration_data, total_jail_pop == max(total_jail_pop,na.rm = TRUE)) %>% pull(year)

highest_white_year <- filter(incarceration_data, white_jail_pop == max(white_jail_pop, na.rm = TRUE)) %>% pull(year)

#2 What County had the highest/lowest black male prison population 
county_max_black_male <- filter(incarceration_data, black_male_prison_pop == max(black_male_prison_pop, na.rm = TRUE))%>% pull(county_name)
county_min_black_male <- filter(incarceration_data, black_male_prison_pop == min(black_male_prison_pop, na.rm = TRUE))%>% pull(county_name)

#3 What is the ratio of the general black population to the imprisoned black population during the highest jail year in the county with the highest black male prison population 
ratio_black_prison  <- filter(incarceration_data,year == highest_year & county_name == county_max_black_male) %>% summarize(ratio = black_jail_pop / black_pop_15to64) %>% pull(ratio)

#4 Average amount of black people in prison across all the counties in the highest jail year
average_black <- filter(incarceration_data, year == highest_year)
average_black <- mean(incarceration_data$black_male_prison_pop, na.rm = TRUE)

#5 The ratio of blacks to whites in jail in most recent year in Los Angles County
ratio_black_white <- filter(incarceration_data, year == max(year) & county_name == "Los Angeles County") %>% summarize(ratio = black_jail_pop / white_jail_pop) %>% pull(ratio)


#Chart 1 - Comparing the mean black and white jail populations in new york County 
sum_pop_race <- incarceration_data %>% filter(county_name == "New York County") %>% group_by(year) %>%
  summarize(mean_black = mean(black_jail_pop, na.rm = TRUE),
            mean_white = mean(white_jail_pop, na.rm = TRUE)) %>%
  gather(race, population, -year)
chart_one <- ggplot(data = sum_pop_race) +
  geom_line(mapping = aes(x = year, y = population, color = race)) + labs(title = "Black and White Jail Population in New York County", x = "Year",
                                                                          y = "Population")
plot(chart_one)

#Chart 2 - Comparing black population in New York County Versus Los Angeles County
chart_two <- ggplot(filter(incarceration_data, county_name == "Los Angeles County" | county_name == "New York County"), aes(x = year, y = black_jail_pop)) +
  geom_line(aes(color = county_name))+
  labs(title = "Black Jail Population in New York County versus Los Angeles County", x = "Year",
       y = "Black Population")
plot(chart_two)

#Map - Trend map of black people in jail in the year 1993

Map_trend <- data.frame(
    filter(incarceration_data, year == highest_black_year) %>%
    select(county_name, total_jail_pop, black_jail_pop, white_jail_pop) %>%
    mutate(county_name = tolower(county_name)) %>%
    mutate(county_name = word(county_name, 1, -2))
)
colnames(Map_trend)[which(names(Map_trend) == "county_name")] <- "subregion"

county_shape <- map_data("county")

county_shape <- left_join(county_shape, Map_trend, by="subregion")

county_shape_data <- county_shape %>%
  filter(!is.na(county_shape$black_jail_pop))

county_map <- ggplot(county_shape_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = black_jail_pop), color="white")+
  scale_fill_gradient(name = "Number of Black People", low = "blue", high = "red") + 
  labs(title = "Trend map of black people in jail in 1993") +
  coord_map()

plot(county_map)

