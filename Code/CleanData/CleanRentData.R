library(httr)
library(jsonlite)
library(quantmod)
library(tidyverse)
library(lubridate)

source("Code/CleanData/Clean_DMARCData.R")

# Getting outside data ----
# I used ChatGPT to help me get this data
# I also used it to help format this data correctly

zori_county <- read_csv("DataRaw/County_zori_uc_sfrcondomfr_sm_sa_month.csv")

# Filter for Polk County, IA
zori_polk <- zori_county %>%
  filter(RegionName == "Polk County", StateName == "IA")

# Convert wide to long format
zori_polk_long <- zori_polk %>%
  pivot_longer(
    cols = matches("^\\d{4}-\\d{2}-\\d{2}"), 
    names_to = "raw_date",
    values_to = "zori"
  ) %>%
  mutate(
    floor_month = floor_date(as.Date(raw_date), unit = "month"),  # align to 1st of month
    zori = as.numeric(zori)
  ) %>%
  select(floor_month, zori)

# Joining the data with monthly_counts
zori_data <- monthly_counts %>%
  left_join(zori_polk_long, by = "floor_month")