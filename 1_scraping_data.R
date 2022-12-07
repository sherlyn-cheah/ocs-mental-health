## Section 1: Scraping data

# to easily load and save data
library(here)
# to scrape web pages
library(rvest)

# webpage location of data
url <- "https://www.samhsa.gov/data/sites/default/files/cbhsq-reports/NSDUHDetailedTabs2018R2/NSDUHDetTabsSect11pe2018.htm"

# Table 11.1A
# XPath of Table 11.1A: /html/body/div[4]/div[1]/table
table11.1a <- url %>%
  read_html() %>%
  html_nodes(xpath= '/html/body/div[4]/div[1]/table') %>%
  html_table()
# output is a list, so need to extract element out of list using code below
table11.1a <- table11.1a[[1]]

# to scrap multiple tables w/o c+p code repeatedly, write a function "scraper" with XPATH input
scraper <- function(XPATH){
  url <- "https://www.samhsa.gov/data/sites/default/files/cbhsq-reports/NSDUHDetailedTabs2018R2/NSDUHDetTabsSect11pe2018.htm"
  
  table <- url %>%
    read_html() %>%
    html_nodes(xpath = XPATH) %>%
    html_table()
  output <- table[[1]]
  output
}

# continue scraping using "scraper" function
# Table 11.1B
table11.1b <- scraper("/html/body/div[4]/div[2]/table")

# Table 11.2A
table11.2a <- scraper("/html/body/div[4]/div[3]/table")

# Table 11.2B
table11.2b <- scraper("/html/body/div[4]/div[4]/table")

# Table 11.3A
table11.3a <- scraper("/html/body/div[4]/div[5]/table")

# Table 11.3B
table11.3b <- scraper("/html/body/div[4]/div[6]/table")

# Table 11.4A
table11.4a <- scraper("/html/body/div[4]/div[7]/table")

# Table 11.4B
table11.4b <- scraper("/html/body/div[4]/div[8]/table")

#save imported data into "/data/imported"
save(table11.1a, table11.1b, table11.2a, table11.2b, table11.3a, table11.3b,
     table11.4a, table11.4b, file = here("data", "imported", "imported_data.rda"))
