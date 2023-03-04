library(tidyverse)
library(ggplot2)
library(plotly)
library(viridis)

pub3_df <- read.csv("c:/users/where/documents/publisher_df.csv", stringsAsFactors = FALSE)

pub3_df <- pub3_df %>% 
  select(Publisher, Checkouts, MaterialType, Title, CheckoutDate)

pub3_df$CheckoutDate <- as.Date(pub3_df$CheckoutDate, format = "%Y-%m-%d")

top_5_publishers <- pub3_df %>% 
  group_by(Publisher) %>%
  summarise(TotalCheckouts = sum(Checkouts)) %>%
  arrange(desc(TotalCheckouts)) %>%
  head(5)

pub3_df <- pub3_df %>%
  filter(Publisher %in% top_5_publishers$Publisher)

# Rename all rows in MaterialType column that are "ER, SOUNDDISC" to "SOUNDDISC"
pub3_df$MaterialType <- gsub("ER, SOUNDDISC", "SOUNDDISC", pub3_df$MaterialType)

# Rename "AUDIOBOOK" to "Audiobook", "BOOK" to "Book", "EBOOK" to "eBook", "SOUNDDISC" to "CD", and "REGPRINT" to "MAGAZINE"
pub3_df$MaterialType <- gsub("AUDIOBOOK", "Audiobook", pub3_df$MaterialType)
pub3_df$MaterialType <- gsub("BOOK", "Book", pub3_df$MaterialType)
pub3_df$MaterialType <- gsub("EBOOK", "eBook", pub3_df$MaterialType)
pub3_df$MaterialType <- gsub("SOUNDDISC", "CD", pub3_df$MaterialType)
pub3_df$MaterialType <- gsub("SOUNDREC", "CD", pub3_df$MaterialType)
pub3_df$MaterialType <- gsub("REGPRINT", "Magazine", pub3_df$MaterialType)

ggplotly3 <- ggplot(data = pub3_df) +
  geom_point(
    mapping = aes(x = CheckoutDate, y = Checkouts, color = MaterialType),
    position = "jitter"
  ) +
  facet_wrap(~Publisher) +
  labs(
    title = "Checkouts by Material Type and Year",
    x = "Checkout Year",
    y = "Checkouts",
    color = "Material Type",
    labels = c("Audiobook", "Book", "eBook", "CD", "DVD")
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  scale_y_continuous(breaks = seq(0, max(pub3_df$Checkouts), by = 1000)) +
  scale_color_viridis(discrete = T)

ggplotly3 <- ggplotly3 %>%
  ggplotly()

ggplotly3
