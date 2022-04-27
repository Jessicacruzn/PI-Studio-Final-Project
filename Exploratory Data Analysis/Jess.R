# import libraries
library(tidyverse)
library(textclean)
library(quanteda)
library(quanteda.textplots)

# import dataset
setwd("~/PI Studio/PI-Studio-Final-Project/Dataset")
data <- read_csv('Yelp Dataset Shortened.csv')

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


# network of categories
corp <- corpus(data, text_field = 'categories')

dfm <- corp %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_remove(c("Food", "Restaurant")) %>%
  dfm()

fcm <- fcm(dfm)

top <- names(topfeatures(fcm, 10))

topfcm <- fcm_select(fcm, pattern = top)

textplot_network(fcm, min_freq = 0.1, edge_alpha = 0.8, edge_size = 2)

top
