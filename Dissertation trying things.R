##### Scraping all headlines from a page

library(rvest)
library(dplyr)

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

```


##### Scraping all headlines from a website

# Load necessary libraries
library(RSelenium)
library(rvest)
library(dplyr)

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

##### Trying NLP on an article database




