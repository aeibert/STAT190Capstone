all <- read_excel("CleanData/visitor_data.xlsx")

visit <- all %>%
  group_by(afn) %>%
  filter(served_date == min(served_date)) %>% # Keep only the earliest served_date per afn
  summarise(
    served_date = first(served_date),
    n_household = n(),
    zip = first(zip),
    annual_income = first(annual_income),
    fed_poverty_level = first(fed_poverty_level),
    gender = first(gender),
    race = first(race),
    ethnicity = first(ethnicity),
    education = first(education),
    family_type = first(family_type),
    location = first(location),
    homeless = first(homeless)
  ) %>%
  mutate(
    served_year = year(served_date),
    served_month = month(served_date),
    served_day_of_month = mday(served_date),
    round_month = round_date(served_date, "month")
  )

#Aggregate Visitors by ZIP

visitors_by_zip <- visit %>% 
  group_by(zip) %>% 
  summarise(Total_visitors = n())


