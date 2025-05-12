# ========================================================
# Initial Setup
# ========================================================
rm(list = ls())

library(tidyverse)
library(lubridate)
library(readxl)

# ========================================================
# Read and Combine All Excel Sheets
# ========================================================
file_directory <- "DataRaw/RD_Yearly_Data/"

excel_files <- list.files(file_directory, pattern = "\\.xlsx$", full.names = TRUE)

all_data <- bind_rows(
  lapply(excel_files, function(file) {
    sheets <- excel_sheets(file)
    bind_rows(
      lapply(sheets, function(sheet) {
        read_excel(file, sheet = sheet) %>%
          mutate(SheetName = sheet, FileName = basename(file))
      })
    )
  })
)

# ========================================================
# Zipcode Lookup Table
# ========================================================
zipcode_lookup <- tribble(
  ~Location, ~Zipcode,
  "Brick Street Market & Cafe", "50263",  # Waukee, IA
  "Brick Street Market & Cafe", "50263",
  "Fleur", "50315",
  "Euclid Ave", "50313",
  "Grand Ave", "50312",
  "Johnston", "50131",
  "Meredith Dr", "50310",
  "North Ankeny Blvd", "50023",
  "Oralabor Rd", "50021",
  "University Ave", "50311",
  "Gateway Market", "50309",
  "Ankeny North", "50023",
  "Mills Civic", "50266",
  "Park Avenue", "50321",
  "Prairie Trail", "50023",
  "Urbandale", "50322",
  "Valley West", "50266",
  "West Lakes", "50266",
  "Windsor Heights", "50324",
  "Beaver", "50310",
  "Ingersoll", "50312",
  "Drugstore University", "50311",
  "Hy-Vee Drug #7082", "50311",
  "Southridge", "50315",
  "St. James Lutheran Church", "50131",
  "Unknown Location", NA
)

# ========================================================
# Data Cleaning and Transformation
# ========================================================
cleanRedBarrel <- all_data %>%
  filter(!is.na(LOCATION), !is.na(DATE), !is.na(VALUE), !is.na(ITEMS)) %>%
  select(LOCATION, ITEMS, DATE, VALUE) %>%
  mutate(
    DATE = as.Date(as.numeric(DATE), origin = "1899-12-30"),
    LOCATION = gsub(" \\(.*\\)", "", LOCATION),
    LOCATION = ifelse(LOCATION == "Location Unknown", "Unknown Location", LOCATION),
    Store = trimws(sub(" - .*", "", LOCATION)),
    Location = trimws(sub(".*- ", "", LOCATION)),
    `Barrel TYPE` = case_when(
      str_detect(ITEMS, "food items") ~ "Food",
      str_detect(ITEMS, "Hunger Sack") ~ "Hunger Sack",
      str_detect(ITEMS, "produce") ~ "Produce",
      str_detect(ITEMS, "diapers") ~ "Diapers",
      str_detect(ITEMS, "personal care|personal hygiene") ~ "Personal Care",
      str_detect(ITEMS, "refrigerated items") ~ "Refrigerated",
      TRUE ~ "Other"
    ),
    ITEMS = as.numeric(str_extract(ITEMS, "\\d+"))
  ) %>%
  left_join(zipcode_lookup, by = "Location") %>%
  select(-LOCATION)

# Save cleaned data
write.csv(cleanRedBarrel, "CleanData/RD_Yearly_Data.csv", row.names = FALSE)

# ========================================================
# Aggregated Data by Location and Year
# ========================================================
foodByZipData <- cleanRedBarrel %>%
  mutate(Year = format(DATE, "%Y")) %>%
  group_by(Location, Zipcode, Year) %>%
  summarise(
    Total_ITEMS = sum(ITEMS, na.rm = TRUE),
    Total_VALUE = sum(VALUE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Location, Zipcode) %>%
  summarise(
    Total_ITEMS = sum(Total_ITEMS),
    Total_VALUE = sum(Total_VALUE),
    TotalRB = n_distinct(Year),
    .groups = "drop"
  )

# ========================================================
# Aggregated Data by Store and Year
# ========================================================
foodByStoreType <- cleanRedBarrel %>%
  mutate(Year = format(DATE, "%Y")) %>%
  group_by(Store, Year) %>%
  summarise(
    Total_ITEMS = sum(ITEMS, na.rm = TRUE),
    Total_VALUE = sum(VALUE, na.rm = TRUE),
    .groups = "drop"
  )

# ========================================================
# Donations by Store and Barrel TYPE
# ========================================================
donations_by_store <- cleanRedBarrel %>%
  mutate(Year = format(DATE, "%Y")) %>%
  group_by(Store, Year, `Barrel TYPE`) %>%
  summarise(
    Total_ITEMS = sum(ITEMS, na.rm = TRUE),
    .groups = "drop"
  )