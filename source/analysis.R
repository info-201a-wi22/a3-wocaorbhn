library(ggplot2)
library(dplyr)
library(maps)
library(usmap)

incarceration_data <- read.csv("../incarceration_trends.csv")



#Which year has the most jailed person in Del Norte County in CA
most_jailed_year <- incarceration_data %>%
  filter(county_name == "Del Norte County", state == "CA") %>%
  filter(total_jail_pop == max(total_jail_pop))

#what is the average jailed populations in 2005?
ave_jailed_2005 <- incarceration_data %>%
  filter(year == 2005) %>%
  summarize(mean(total_jail_pop, na.rm = TRUE))

#What is the total jailed populations in 2005 in Del Norte County in CA
total_jailed_Del <- most_jailed_year$total_jail_pop

#What is the proportion of black people jailed in 2005 in Del Norte County in CA
prop_black_person <- most_jailed_year$black_jail_pop/total_jailed_Del

#What is the proportion of white people jailed in 2005 in Del Norte County in CA
prop_white_person <- most_jailed_year$white_jail_pop/total_jailed_Del

#What is the rate of change in black people during 2000 to 2010 in Del Norte
#County in CA
black_2000 <- incarceration_data %>%
  filter(year == 2000, county_name == "Del Norte County", state == "CA") %>%
  pull(black_jail_pop)
black_2010 <- incarceration_data %>%
  filter(year ==2010, county_name == "Del Norte County", state == "CA") %>%
  pull(black_jail_pop)
roc_black_Del <- (black_2010-black_2000)/black_2000
#What is the rate of change in white people during 2000 to 2010 in Del Norte
#County in CA
white_2000 <- incarceration_data %>%
  filter(year == 2000, county_name == "Del Norte County", state == "CA") %>%
  pull(white_jail_pop)
white_2010 <- incarceration_data %>%
  filter(year ==2010, county_name == "Del Norte County", state == "CA") %>%
  pull(white_jail_pop)
roc_white_Del <- (white_2010-white_2000)/white_2000

#trends over time chart 1  (jailed black person in Del Norte County(CA) over
#2000-2010)
chart_data <- incarceration_data %>%
  filter(year >= 2000, year <=2010, 
         county_name == "Del Norte County", state == "CA")
trends_over_time_chart <- blackwhite_2000_2010 <- ggplot(chart_data) +
  geom_line(aes(x = year, y = black_jail_pop,
            color = "black jailed person")) +
  geom_line(aes(x = year, y = total_jail_pop,
            color = "total jailed person")) +
  geom_line(aes(x = year, y = white_jail_pop,
            color = "white jailed person")) +
  labs(
    title = "Black and White people jailed in Del Norte County over 2000-2010",
    x="Year",
    y="Population of jailed person"
  ) +
scale_x_continuous(breaks = seq(2000,2010,1)) +
scale_color_manual(name="Legend", values = c("black jailed person" = "red",
                                             "total jailed person" = "blue",
                                             "white jailed person" = "green"))
blackwhite_2000_2010

#two variable chart
chart_two_data <- incarceration_data %>%
  filter(year == 2018, total_pop <= 100000, total_jail_pop <= 400)

two_variable_chart <- scatter<-ggplot(chart_two_data) +
  geom_point(aes(x = total_pop, y = total_jail_pop), 
             position=position_dodge(width=0.5)) +
  labs(title="The Correlation Between Total population and Total Jail Population",
       x="Total Population",
       y="Total Jailed Population") +
  scale_x_continuous(breaks=seq(0, 100000, 10000))+
  scale_y_continuous(breaks = seq(0, 400, 50))

scatter

   
              
#map
data_2000 <- incarceration_data %>%
  filter(year == 2000)

Map <- plot_usmap(data = data_2000, values = "total_jail_pop") +
  scale_fill_continuous(name = "Jailed Population",
                        label = scales::comma) +
  theme(legend.position = "right") +
  labs(title = "United State of American",
       subtitle = "Total jail populations by county in 2000") +
  theme(legend.position = "right")
