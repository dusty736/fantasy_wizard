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
coach_results_list <- list()
coach_team_ranks_list <- list()
coach_full_team_hist <- list()
for (i in seq_along(coach_urls[1:5])) {
  coach_url <- coach_urls[i]
  # Fetch the HTML content of the page
  response <- GET(coach_url)
  html_content <- content(response, "text")
  
  # Get HTML
  table_pattern <- "<table[^>]*>.*?</table>"
  tables <- regmatches(html_content, gregexpr(table_pattern, html_content, ignore.case = TRUE))[[1]]
  
  for (j in seq_along(tables)) {
    table_html <- read_html(tables[[j]])
    table_df <- table_html %>%
      html_table(fill = TRUE) %>% 
      data.frame()
  }
}
