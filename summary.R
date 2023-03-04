library(tidyverse)

summary_df <- read.csv("c:/users/where/documents/publisher_df.csv", stringsAsFactors = FALSE)

# Find the top 5 publishers by total checkouts
top_5_publishers <- summary_df %>% 
  group_by(Publisher) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  head(5)

# most checked out item (1-3 are WiFi and headphones)

top_item <- summary_df %>% 
  group_by(Title) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(desc(TotalCheckouts)) %>% 
  slice(4)

top_item_title <- top_item %>%
  pull(Title)

top_item_checkouts <- top_item %>%
  pull(TotalCheckouts)

# Find the most checked out item for each publisher in the top 5
top_5_items <- summary_df %>% 
  filter(Publisher %in% top_5_publishers$Publisher) %>% 
  group_by(Publisher, Title) %>% 
  summarise(TotalCheckouts = sum(Checkouts)) %>% 
  arrange(Publisher, desc(TotalCheckouts)) %>% 
  group_by(Publisher) %>% 
  slice(1)
