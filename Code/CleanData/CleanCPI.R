library(httr)
library(jsonlite)
library(quantmod)
library(tidyverse)
library(lubridate)

source("Code/CleanData/Clean_DMARCData.R")

# Getting Outside Data from FRED--------

# Define the base URL and parameters
API_KEY <- "5ea7b4d12383c5ac8db1e64d97e0929f"  # Replace with your API key

# Get CPI sub-indexes
# I used Chatgpt to help me get this
# I wasn't sure how to get multiple series
# This function allows me to get various subseries from the CPI data
get_cpi_subseries <- function(series_id, name_label) {
  response <- GET("https://api.stlouisfed.org/fred/series/observations", query = list(
    series_id = series_id,
    api_key = API_KEY,
    file_type = "json"
  ))
  
  json_data <- fromJSON(content(response, as = "text"))
  
  as_tibble(json_data$observations) %>%
    filter(value != ".") %>%
    mutate(
      date = as.Date(date),
      value = as.numeric(value),
      category = name_label
    )
}

# Cleaning and Formatting the data --------

cpi_food <- get_cpi_subseries("CPIUFDSL", "Food")
cpi_meat <- get_cpi_subseries("CUSR0000SAF112", "Meat")
cpi_veg <- get_cpi_subseries("CUSR0000SAF113", "FruitsVeggies")
cpi_bakery <- get_cpi_subseries("CUSR0000SAF111", "CerealBakery")
cpi_dairy <- get_cpi_subseries("CUSR0000SEFJ", "Dairy")

# Combine
cpi_food_all <- bind_rows(cpi_food, cpi_meat, cpi_veg, cpi_bakery, cpi_dairy)

# Pivot CPI data wider: one row per month, columns for each food category
# I used ChatGPT here so that I could create columns for each food category
cpi_wide <- cpi_food_all %>%
  select(date, category, value) %>%
  pivot_wider(names_from = category, values_from = value) %>%
  rename(floor_month = date)  # rename to match monthly_counts

# Merge with visit counts and enhance with features
monthly_and_cpi <- monthly_counts %>%
  left_join(cpi_wide, by = "floor_month") %>%
  arrange(floor_month) %>%
  mutate(
    month = month(floor_month, label = TRUE),
    year = year(floor_month),
    # Lagged CPI values (e.g., 1-month lag)
    # I used ChatGPT to create these lag variables
    # The lag can explore the possibility of delayed effects of CPI changes 
    Food_lag1 = lag(Food, 1),
    Meat_lag1 = lag(Meat, 1),
    FruitsVeggies_lag1 = lag(FruitsVeggies, 1),
    CerealBakery_lag1 = lag(CerealBakery, 1),
    Dairy_lag1 = lag(Dairy, 1),
    high_inflation = if_else(Food > quantile(Food, 0.75, na.rm = TRUE), 1, 0)
  ) %>%
  filter(year(floor_month) > 2018) %>% # Filter out 2018 since it was the first year collecting data
  drop_na()  # drop any rows with NA after lagging


