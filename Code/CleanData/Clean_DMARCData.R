library(tidyverse)
library(lubridate)
library(haven)
library(scales)

all <- read.csv("DataRaw/DMARC Data 2018-2024 copy.csv")

# Every row of data is a person visit combination
# Every row in a month is the amount of people served
# AFN is household ID
# Individual id is just individual id 

# served date is year - month - day
# Annual income is household income
# Federal Poverty Level 

# dim(all)

# little bit of data cleaning
all$servedDate <- as.Date(all$servedDate)

all <- all %>% 
  mutate(
    servedDate = ymd(servedDate),
    dob = ymd(dob)
  )

# Create a visit level data set

visit <- all %>% 
  group_by(houseHoldIdAfn, servedDate) %>% 
  # in summarise, you neeed to really think about how to characterise a visit
  summarise(
    n_household = first(householdMembers),
    annualIncome = first(annualIncome),
    fedPovertyLevel = first(fedPovertyLevel),
    gender = first(gender),
    race = first(race),
    education = first(education),
    location = first(location),
    foodstamps = first(foodstamps)
  ) %>% 
  mutate(
    served_year = year(servedDate),
    served_month = month(servedDate),
    served_day_of_month = mday(servedDate),
    floor_month = floor_date(servedDate, unit = "month")
  )

# head(visit)

monthly_counts <- visit %>% 
  group_by(floor_month) %>% 
  summarise(
    num_VISITS = n(),                      # number of household visits
    num_PEOPLE_SERVED = sum(n_household, na.rm = TRUE)  # number of people across all households
  )

# look up tidycensus package

# Household Level Dataset

household_data <- all %>%
  group_by(houseHoldIdAfn) %>%
  summarise(
    # Visit date stats
    first_visit = min(servedDate, na.rm = TRUE),
    last_visit = max(servedDate, na.rm = TRUE),
    total_visits = n_distinct(servedDate),
    
    # Duration and frequency
    duration_days = as.numeric(difftime(last_visit, first_visit, units = "days")),
    avg_days_between_visits = if_else(total_visits > 1,
                                      duration_days / (total_visits - 1),
                                      NA_real_),
    
    # Household size
    avg_household_size = mean(householdMembers, na.rm = TRUE),
    max_household_size = max(householdMembers, na.rm = TRUE),
    total_people_served = sum(householdMembers, na.rm = TRUE),
    
    # Income and poverty
    avg_annual_income = mean(annualIncome, na.rm = TRUE),
    poverty_level = mean(fedPovertyLevel, na.rm = TRUE),
    deep_poverty = if_else(poverty_level < 50, 1, 0),
    low_income = if_else(avg_annual_income < 20000, 1, 0),
    
    # SNAP/foodstamps usage
    snap_consistency = n_distinct(foodstamps),
    snap_ever = max(foodstamps, na.rm = TRUE),
    
    # Common metadata
    first_year = year(first_visit)
  ) %>%
  mutate(
    returning = if_else(total_visits > 1, 1, 0)
  ) %>%
  ungroup()

# Top location per Household
top_locations <- all %>%
  group_by(houseHoldIdAfn, location) %>%
  summarise(visits = n(), .groups = "drop") %>%
  arrange(houseHoldIdAfn, desc(visits)) %>%
  group_by(houseHoldIdAfn) %>%
  slice(1) %>%
  ungroup() %>%
  select(houseHoldIdAfn, top_location = location)

# Join into household
household_data <- household_data %>%
  left_join(top_locations, by = "houseHoldIdAfn")

print(head(household_data, n = 5))



