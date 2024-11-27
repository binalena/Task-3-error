# Task-3-error #

## FINAL PROJECT ##

install.packages("httr")
install.packages("rvest")

library(httr)
library(rvest)


# Task 1 #

get_wiki_covid19_page <- function() {
  
  wiki_base_url <- "https://en.wikipedia.org/w/index.php"
  
  wiki_url_parameter <- list(title = "Template:COVID-19_testing_by_country")

  response <- GET(wiki_base_url, query = wiki_url_parameter)
  
  return(response)
  
}

get_wiki_covid19_page

page_response <- get_wiki_covid19_page()
print(page_response)


# Task 2 #

root_node <- read_html(page_response)

table_node <- html_nodes(root_node, "table")
table_node

covid_table <- html_table(table_node[[2]], fill = TRUE)
data_frame <- as.data.frame(covid_table)
print(data_frame)

summary(data_frame)

preprocess_covid_data_frame <- function(data_frame) {
  
  shape <- dim(data_frame)
  
  data_frame<-data_frame[!(data_frame$Country.or.region=="World"),]
  data_frame <- data_frame[1:172, ]
  
  data_frame["Ref."] <- NULL
  data_frame["Units.b."] <- NULL
  
  names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
  
  data_frame$country <- as.factor(data_frame$country)
  data_frame$date <- as.factor(data_frame$date)
  data_frame$tested <- as.numeric(gsub(",","",data_frame$tested))
  data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
  data_frame$'confirmed.tested.ratio' <- as.numeric(gsub(",","",data_frame$confirmed.tested.ratio))
  data_frame$'tested.population.ratio' <- as.numeric(gsub(",","",data_frame$tested.population.ratio))
  data_frame$'confirmed.population.ratio' <- as.numeric(gsub(",","",data_frame$confirmed.population.ratio))
  
  return(data_frame)
}
print(data_frame)

preprocess_covid_data_frame(data_frame)

# Following the above code, I am receiving NAs for all the values of the data_frame dataframe. Please tell me what I need to do to resolve this so I can move on to the next tasks.
