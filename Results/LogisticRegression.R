library(mlr)
library(xgboost)
library(modelStudio)
library(DALEX)
library(tidyverse)
library(tidyr)

rm(list = ls())

# read data
setwd("~/PI Studio/PI-Studio-Final-Project/Dataset")
df <- read_csv('df_median.csv')
data <- select(df, -c('useful','latitude', 'longitude', 'business_id', 'comp_score')) %>%
  rename(free_parking = `Free Parking`)

# summarize variables
summary(data)

# normal model
model_classification <- glm(as.factor(is_open)~., data = data, family = "binomial")
table <- tidy(model_classification)
regression_table <- summary(model_classification)
write.table(regression_table, file = "regression.txt", sep = ",", quote = FALSE, row.names = F)

explainer <- explain(model_classification,
                       data = data,
                       y = data$is_open)

importance <- variable_importance(explainer, type = "raw", loss_function = 'rsme') %>%
  filter(permutation == 10)
importance$score <- 1 - importance$dropout_loss

# feature importance via normal model  
importance_graph <- importance %>%
  filter(!(variable %in% c("_baseline_", "_full_model_", "is_open"))) %>%
  ggplot(aes(x = reorder(variable, score), weight = score)) +
  geom_bar() + 
  labs(
    x = "Variable",
    y = " ",
    title = "Feature Importance by Variable"
  ) +
  coord_flip() +
  theme_minimal()

ggsave('importance_graph.jpeg', importance_graph)

yelp_stars <- data %>%
  ggplot(aes(x = stars_y, y = factor(is_open))) +
  geom_boxplot() +
  labs(
    x = "Yelp Stars",
    y = " ",
    title = "Distribution of Yelp Stars against DV"
  ) +
  theme_minimal()

ggsave('Yelp Stars.jpeg', yelp_stars)

price_range <- data %>%
  ggplot(aes(x = RestaurantsPriceRange, y = factor(is_open))) +
  geom_boxplot() +
  labs(
    x = "Price Range",
    y = " ",
    title = "Distribution of Price Range against DV"
  ) +
  theme_minimal()

ggsave('Price Range.jpeg', price_range)

sentiment_score <- data %>%
  ggplot(aes(x = compound, y = factor(is_open))) +
  geom_boxplot() +
  labs(
    x = "Sentiment Score",
    y = " ",
    title = "Distribution of Sentiment Score against DV"
  ) +
  theme_minimal()

ggsave('Sentiment Score.jpeg', sentiment_score)
  
  
# model for is_open == 0
model_classification_0 <- glm(as.factor(is_open == 0)~., data = data, family = "binomial")

explainer_0 <- explain(model_classification_0,
                       data = data,
                       y = data$is_open == 0,
                       label = 'Closed')

importance_0 <- variable_importance(explainer_0, type = "raw") %>%
  filter(permutation == 10)

model_classification_1 <- glm(as.factor(is_open == 1)~., data = data, family = "binomial")

# model for is_open == 1
explainer_1 <- explain(model_classification_1,
                     data = data,
                     y = data$is_open == 1,
                     label = 'Open')

importance_1 <- variable_importance(explainer_1, type = "raw") %>%
  filter(permutation == 10)

# graph for 0 vs 1 feature importance
feature_importance_by_status <- rbind(importance_0, importance_1) %>%
  filter(!(variable %in% c("_baseline_", "_full_model_", "is_open"))) %>%
  ggplot() +
  aes(x = reorder(variable, dropout_loss), fill = label, weight = dropout_loss) +
  geom_bar(position = "dodge") +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Variable",
    y = " ",
    title = "Feature Importance by Variable",
    fill = "Status"
  ) +
  coord_flip() +
  theme_minimal()

ggsave('fimp_by_status.jpeg', feature_importance_by_status)


# model studio visualization
new_observations <- data[2:3,]
rownames(new_observations) <- c("Restaurant 1","Restaurant 2")

modelStudio(explainer, new_observations)