library(tidyverse)
library(ggplot2)
library(plotly)

# read in the data
pub2_df <- read.csv("c:/users/where/documents/publisher_df.csv", stringsAsFactors = FALSE)
pub2_df <- pub2_df %>% select(Publisher, Checkouts, CheckoutDate)

pub2_df$CheckoutDate <- as.Date(pub2_df$CheckoutDate, format = "%Y-%m-%d")

# find the top 5 publishers by overall checkouts
top_5_publishers <- pub2_df %>% 
  group_by(Publisher) %>%
  summarise(TotalCheckouts = sum(Checkouts)) %>%
  arrange(desc(TotalCheckouts)) %>%
  head(5)

top_publishers_df <- pub2_df %>%
  filter(Publisher %in% top_5_publishers$Publisher)

# compute the sum of monthly checkouts for each publisher
top_publishers_df <- top_publishers_df %>%
  group_by(Publisher, CheckoutDate) %>%
  summarise(Checkouts = sum(Checkouts))

# create plot
ggplotly2 <- ggplotly(ggplot(top_publishers_df,
  aes(x = CheckoutDate,
      y = Checkouts,
      color = Publisher)) + 
  geom_line() + 
  scale_color_manual(values = c("blue", "red", "green", "orange", "purple")) + 
  labs(x = "Checkout Month", y = "Checkouts",
       title = "Monthly Checkouts for top 5 Publishers by Checkouts",
       subtitle = "Data Source: SPL Materials checked out more than 5 times from 2017-2023") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  scale_y_continuous(breaks = seq(0, max(top_publishers_df$Checkouts), by = 4000)))

# plot
ggplotly2