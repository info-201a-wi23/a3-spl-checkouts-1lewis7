library(tidyverse)
library(ggplot2)
library(plotly)

# Read data
publisher_df <- read.csv("c:/users/where/documents/publisher_df.csv", stringsAsFactors = FALSE)

material_type_df <- publisher_df %>%
  select(MaterialType, Checkouts, CheckoutDate)

# find the top 5 material types by overall checkouts
top_5_material_types <- material_type_df %>% 
  group_by(MaterialType) %>%
  summarise(TotalCheckouts = sum(Checkouts)) %>%
  arrange(desc(TotalCheckouts)) %>%
  head(5)

material_type_df <- material_type_df %>%
  filter(MaterialType %in% top_5_material_types$MaterialType)

# Compute average checkouts 
material_type_df <- material_type_df %>%
  group_by(MaterialType, CheckoutDate) %>%
  summarise(Checkouts = mean(Checkouts))

material_type_df <- material_type_df %>%
  rename(AverageCheckouts = Checkouts)
material_type_df$MaterialType <- factor(material_type_df$MaterialType,
  levels = c("BOOK", "EBOOK", "AUDIOBOOK", "SOUNDDISC", "VIDEODISC"),
  labels = c("Books", "eBooks", "Audiobooks", "CDs", "DVDs"))

# Convert CheckoutDate to date format

material_type_df$CheckoutDate <- as.Date(material_type_df$CheckoutDate, format = "%Y-%m-%d")

# Plot the data
gg_material_type <- ggplot(material_type_df, aes(x = CheckoutDate, y = AverageCheckouts, color = MaterialType)) +
  geom_line() + 
  labs(x = "Checkout Year",
    y = "Average Checkouts",
    title = "Checkouts by Material Type Over Time",
    subtitle = "Audiobook and eBook checkouts spiked during 2020 while Books, CD, and DVD checkouts saw plummeting checkout averages",
    caption = "Data source: SPL Checkout Data") + 
  scale_x_date(date_breaks = "1 year",
    date_labels = "%Y") +
  scale_color_manual(values = c("blue", "red", "green", "orange", "purple"),
  labels = c("Audiobook", "Book", "eBook", "CD", "DVD") +
  theme(plot.caption = element_text(hjust = 0.5, size = 10),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#d3d3d3", size = 0.5)))
  
ggplotly1 <- ggplotly(gg_material_type)

# Show the plot
ggplotly1