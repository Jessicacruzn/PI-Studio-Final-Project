# import libraries
library(tidyverse)
library(textclean)
library(quanteda)
library(quanteda.textplots)

# import dataset
setwd("~/PI Studio/PI-Studio-Final-Project/Dataset")
data <- read_csv('cleaned_dataframe.csv')

# get reviews per restaurants
reviews_per_restaurant <- data %>%
  group_by(business_id) %>%
  count() %>%
  arrange(desc(n))

# reviews count
business_per_review_amount <- reviews_per_restaurant %>%
  rename(Review_Amount = n) %>%
  group_by(Review_Amount) %>%
  count() %>%
  arrange(desc(n)) %>%
  rename(Business_Amount = n)

rpb <- ggplot(business_per_review_amount) +
  aes(x = Review_Amount, y = Business_Amount) +
  geom_line(size = 0.5, colour = "#112446") +
  labs(
    x = "Amount of Reviews",
    y = "Amount of Restaurants",
    title = "Amount of Reviews That Restaurants Have",
    caption = "Most restaurants have only a few reviews"
  ) +
  theme_minimal()

ggsave('ReviewPerRest.jpeg', rpb)
