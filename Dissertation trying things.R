######## SENTIMENT ANALYSIS OF HEADLINES

##### NLP ON EXISTING DATAFRAME

library(rvest)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(dplyr)
library(tm)
library(SnowballC)
library(textclean)
library(tidytext)
library(readxl)
library(ggplot2)
library(reshape2)

# Read the dataset
headlines_df <- read_csv("Fake.csv")

# Convert text to lowercase
headlines_df$title <- tolower(headlines_df$title)

# Tokenize the headlines
tidy_headlines <- headlines_df %>%
  unnest_tokens(word, title)  # Use the correct column name 'title'

# Remove stop words
tidy_headlines <- tidy_headlines %>%
  anti_join(stop_words)

# Remove blank spaces and single-character tokens
tidy_headlines <- tidy_headlines %>%
  filter(nchar(word) > 1)  # Only keep words with more than one character

# Remove any unwanted characters (e.g., punctuation, special characters)
tidy_headlines$word <- gsub("[[:punct:]]", "", tidy_headlines$word)

# Calculate word frequency
word_freq <- tidy_headlines %>%
  count(word, sort = TRUE)

# View the top 10 frequent words
head(word_freq, 10)

## Emotion detection in the headlines
# Get the NRC emotion lexicon

# Load the data from the xlsx file
nrc_lexicon <- read_excel("~/Downloads/NRC-Emotion-Lexicon-v0.92-In105Languages-Nov2017Translations.xlsx")

# Select the relevant columns
nrc_cleaned <- nrc_lexicon %>%
  select(Word = `English (en)...1`, 
         Positive, Negative, Anger, Anticipation, 
         Disgust, Fear, Joy, Sadness, Surprise, Trust)

# View the cleaned dataset
head(nrc_cleaned)

nrc_cleaned <- nrc_cleaned %>%
  rename(word = Word)

# Perform the inner join and calculate the sum of emotion scores for each headline
headlines_emotions <- tidy_headlines %>%
  inner_join(nrc_cleaned, by = "word") %>%
  group_by(id = row_number()) %>%
  summarize(across(c(Positive:Trust), sum, na.rm = TRUE))  # Aggregate emotion scores per headline

# Merge the aggregated emotion scores back with the headlines dataframe
headlines_emotions_df <- headlines_df %>%
  mutate(id = row_number()) %>%
  left_join(headlines_emotions, by = "id")

# View the result to ensure proper merging
head(headlines_emotions_df)

headlines_emotions_df %>%
  summarize(across(Positive:Trust, mean, na.rm = TRUE))

########### VISUALISATION

#Basic bar chart visualisation

headlines_emotions_df %>%
  summarize(across(Positive:Trust, sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "Emotion", values_to = "Count") %>%
  ggplot(aes(x = reorder(Emotion, -Count), y = Count, fill = Emotion)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Overall Emotional Distribution in Headlines",
       x = "Emotion",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

heatmap_data <- as.matrix(headlines_emotions_df %>% select(Positive:Trust))
heatmap(heatmap_data, scale = "column", col = heat.colors(256))


##### SCRAPING HEADLINES FROM WEBSITE

library(RSelenium)
library(rvest)

# URL of the page
url <- "https://thebridge.in/"

# Read the webpage
webpage <- read_html(url)

# Extract headlines from h3 and h4 tags with the appropriate classes
headlines_h3 <- webpage %>% html_nodes("h3.bd-heading-text") %>% html_text()
headlines_h4 <- webpage %>% html_nodes("h4.bd-heading-text") %>% html_text()

# Combine both sets of headlines
all_headlines <- c(headlines_h3, headlines_h4)

# Create a dataframe of headlines
headlines_df <- data.frame(Headline = all_headlines)

# View the dataframe
print(headlines_df)

# Start the Selenium server
rD <- rsDriver(browser = "chrome", port = 4444L)  # Port corrected to integer with 'L'
remote_driver <- rD$client

# Base URL for scraping
base_url <- "https://thebridge.in/latest"
all_headlines <- c()  # To store all headlines

# Function to scrape headlines from a page
scrape_page <- function(url) {
  # Navigate to the page
  remote_driver$navigate(url)
  Sys.sleep(3)  # Wait for the page to load
  
  # Extract the page source
  page_source <- remote_driver$getPageSource()[[1]]
  
  # Use rvest to parse the page source and extract headlines
  page_html <- read_html(page_source)
  
  # Scrape both h3 and h4 headlines
  h3_headlines <- page_html %>%
    html_nodes("h3.bd-heading-text") %>%
    html_text(trim = TRUE)
  
  h4_headlines <- page_html %>%
    html_nodes("h4.bd-heading-text") %>%
    html_text(trim = TRUE)
  
  # Combine both h3 and h4 headlines into one vector
  all_headlines <- c(h3_headlines, h4_headlines)
  
  return(all_headlines)
}

# Loop through the first 5 pages (change the range if needed)
for (i in 0:4) {
  # Create the URL for each page (first page URL is different)
  page_url <- ifelse(i == 0, base_url, paste0(base_url, "/", i + 1))
  
  # Scrape headlines from the current page
  headlines <- scrape_page(page_url)
  
  # Append the headlines to the list
  all_headlines <- c(all_headlines, headlines)
  
  cat("Scraped page", page_url, "with", length(headlines), "headlines.\n")
}

# Convert to a data frame (optional)
headlines_df <- data.frame(headline = all_headlines)

# Print first few headlines
head(headlines_df)

# Test with a simple page (like the homepage)
cat("Navigating to URL:", "https://thebridge.in", "\n")
remote_driver$navigate("https://thebridge.in")
Sys.sleep(5)  # Wait to see if it loads

#NOTE: UNSUCCESSFUL


