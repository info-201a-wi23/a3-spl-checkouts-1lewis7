library(tidyverse)

spl_df <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# remove ISBN, CheckoutType, UsageClass, and Subjects columns then remove rows with NA or empty values
publisher_df <- spl_df %>% select(-ISBN, -CheckoutType, -UsageClass, -Subjects) %>% 
  na.omit() %>% filter(Publisher != "")


# trimws publisher names and material types and remove trailing commas
publisher_df$Publisher <- trimws(publisher_df$Publisher)
publisher_df$MaterialType <- trimws(publisher_df$MaterialType)

publisher_df <- publisher_df %>%
  mutate(CheckoutDate = as.Date(paste(CheckoutYear, CheckoutMonth, "01", sep = "-")))

publisher_df <- publisher_df %>%
  select(-CheckoutYear, -CheckoutMonth)

# rename rows with "Random House," and "Random House" to "Random House, Inc."
publisher_df$Publisher <- gsub("Random House,", "Random House, Inc.", publisher_df$Publisher)
publisher_df$Publisher <- gsub("Viking, an imprint of Penguin Random House LLC,", "Random House, Inc.", publisher_df$Publisher)
publisher_df$Publisher <- gsub("Random House Group Limited", "Random House, Inc.", publisher_df$Publisher)
publisher_df$Publisher <- gsub("Random House, Inc. Inc.", "Random House, Inc.", publisher_df$Publisher)

# rename rows with "Hachette Audio,", "Hachette Audio" and "Hachette Book Group" to "Hachette Digital, Inc."
publisher_df$Publisher <- gsub("Hachette Audio,", "Hachette Digital, Inc.", publisher_df$Publisher)
publisher_df$Publisher <- gsub("Hachette Audio", "Hachette Digital, Inc.", publisher_df$Publisher)
publisher_df$Publisher <- gsub("Hachette Books,", "Hachette Digital, Inc.", publisher_df$Publisher)
publisher_df$Publisher <- gsub("Hachette Digital, Inc.", "Hachette", publisher_df$Publisher)

# rename rows with "Harper, an imprint of HarperCollinsPublishers," and "HarperCollins," to "HarperCollins Publishers Inc."
publisher_df$Publisher <- gsub("Harper, an imprint of HarperCollinsPublishers,", "HarperCollins Publishers Inc.", publisher_df$Publisher)
publisher_df$Publisher <- gsub("HarperCollins,", "HarperCollins Publishers Inc.", publisher_df$Publisher)

# rename rows with "Penguin Books," and "Penguin Press," to "Penguin Group (USA), Inc."
publisher_df$Publisher <- gsub("Penguin Books,", "Penguin Group (USA), Inc.", publisher_df$Publisher)
publisher_df$Publisher <- gsub("ePenguin", "Penguin Group (USA), Inc.", publisher_df$Publisher)
publisher_df$Publisher <- gsub("Penguin Press,", "Penguin Group (USA), Inc.", publisher_df$Publisher)
publisher_df$Publisher <- gsub("Penguin Young Readers, an imprint of Penguin Group (USA) LLC,", "Penguin Group (USA), Inc.", publisher_df$Publisher)
publisher_df$Publisher <- gsub("Penguin Group (USA) Inc.", "Penguin Group (USA), Inc.", publisher_df$Publisher)

publisher_df <- publisher_df %>% select(-PublicationYear, Creator)

# remove any before 2018
publisher_df <- publisher_df %>% filter(CheckoutDate >= "2018-01-01")

# write to csv
write.csv(publisher_df, "publisher_df.csv", row.names = FALSE)
