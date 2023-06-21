################################################################################
# Load Libraries
################################################################################

library(tidyverse)
library(lubridate)
library(rvest)
library(httr)

################################################################################
# Define the URL of the coaches page
################################################################################
url <- "https://www.pro-football-reference.com/coaches/"

################################################################################
# Send an HTTP GET request to fetch the HTML content
################################################################################
response <- httr::GET(url)

################################################################################
# Parse the HTML content
################################################################################
html <- read_html(response)

################################################################################
# Extract the links to coach pages
################################################################################
coach_links <- html %>%
  html_nodes("table#coaches tbody tr a") %>%
  html_attr("href")

################################################################################
# Generate the URLs of coach pages
################################################################################
coach_urls <- paste0("https://www.pro-football-reference.com", coach_links)


# Loop over the coach URLs to fetch and process the HTML content of each coach page
for (coach_url in coach_urls) {
  coach_response <- GET(coach_url)
  coach_html <- read_html(coach_response)
  
  # Define the expected table names
  expected_table_names <- c("Coaching Results", "Team's Ranks", "Full Coaching History")
  
  table_node <- coach_html %>%
    html_nodes(xpath = paste0("//table[preceding-sibling::h2[text()='", table_name, "']]"))
  
  # Extract the tables from the coach HTML page
  tables <- coach_html %>%
    html_nodes("table")
  
  # Loop over the tables and process them as desired
  for (i in seq_along(tables)) {
    table_data <- tables[[i]] %>%
      html_table(fill = TRUE)
    
    # Process the table data as desired
    # (e.g., perform analysis, save to a file, etc.)
    # Example: Print the table data
    cat("Table", i, "\n")
    print(table_data)
    cat("\n")
  }
}


# Define the URL of the coach's page
url <- "https://www.pro-football-reference.com/coaches/BeliBi0.htm#coaching_ranks"

# Fetch the HTML content of the page
response <- GET(url)
html_content <- content(response, "text")

# Extract the tables using regular expressions or other parsing methods
# Adjust the pattern and extraction logic based on the specific web page structure

# Example: Extract all tables using regular expression pattern matching
table_pattern <- "<table[^>]*>.*?</table>"
tables <- regmatches(html_content, gregexpr(table_pattern, html_content, ignore.case = TRUE))[[1]]

# Process the extracted tables as desired
# Example: Print the tables
for (i in seq_along(tables)) {
  table_html <- read_html(tables[[i]])
  table_df <- table_html %>%
    html_table(fill = TRUE) %>% 
    data.frame()
  
  
  
}


# Loop over the tables and process them as desired
for (i in seq_along(tables)) {
  table_data <- tables[[i]] %>% html_table(fill = TRUE)
  
  # Convert the HTML content into a data frame
  table_df <- XML::readHTMLTable(table_data[[1]], stringsAsFactors = FALSE)
  
  # Print the resulting data frame
  print(table_df)
}

