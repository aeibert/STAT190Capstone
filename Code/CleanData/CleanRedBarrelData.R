library(tidyverse)
library(lubridate)
library(dplyr)
library(readxl)

# Define the directory containing your Excel files
file_directory <- "DataRaw/RD_Yearly_Data/"

# List all Excel files in the directory
excel_files <- list.files(file_directory, pattern = "\\.xlsx$", full.names = TRUE)

# Read and combine all sheets from all Excel files
all_data  <- bind_rows(
  lapply(excel_files, function(file) {
    sheets <- excel_sheets(file) # Get sheet names for each file
    bind_rows(
      lapply(sheets, function(sheet) {
        read_excel(file, sheet = sheet) %>%
          mutate(SheetName = sheet, FileName = basename(file)) # Add sheet and file name
      })
    )
  })
)



# Create a lookup table for Location to Zipcode mapping
zipcode_lookup <- tribble(
  ~Location, ~Zipcode,
  "Brick Street Market & Cafe", "50263",  # Waukee, IA
  "Fleur", "50315",  # Fleur Drive, Des Moines, IA
  "Euclid Ave", "50313",  # Euclid Avenue, Des Moines, IA
  "Grand Ave", "50312",  # Grand Avenue, Des Moines, IA
  "Johnston", "50131",  # Johnston, IA
  "Meredith Dr", "50310",  # Meredith Drive, Des Moines, IA
  "North Ankeny Blvd", "50023",  # North Ankeny Blvd, Ankeny, IA
  "Oralabor Rd", "50021",  # Oralabor Road, Ankeny, IA
  "University Ave", "50311",  # University Avenue, Des Moines, IA
  "Gateway Market", "50309",  # Gateway Market, Downtown Des Moines, IA
  "Ankeny North", "50023",  # Ankeny, IA
  "Grand Ave", "50312",  # Grand Avenue, Des Moines, IA (Hy-Vee location)
  "Mills Civic", "50266",  # Mills Civic Parkway, West Des Moines, IA
  "Park Avenue", "50321",  # Park Avenue, Des Moines, IA
  "Prairie Trail", "50023",  # Prairie Trail, Ankeny, IA
  "Urbandale", "50322",  # Urbandale, IA
  "Valley West", "50266",  # Valley West, West Des Moines, IA
  "West Lakes", "50266",  # West Lakes, West Des Moines, IA
  "Windsor Heights", "50324",  # Windsor Heights, IA
  "Beaver", "50310",  # Beaver Avenue, Des Moines, IA
  "Ingersoll", "50312",  # Ingersoll Avenue, Des Moines, IA
  "Drugstore University", "50311",
  "Hy-Vee Drug #7082", "50311",
  "Southridge", "50315",
  "St. James Lutheran Church", "50131",
  "Unknown Location", NA  # Keep NA for unknown locations
)

# ========================================================
# Clean the LOCATION column by separating Store and Location
cleanRedBarrel <- all_data %>%
  filter(!is.na(LOCATION) & !is.na(DATE) & !is.na(VALUE) & !is.na(ITEMS)) %>%  # Remove rows with NA values
  select(LOCATION, ITEMS, DATE, VALUE) %>%  # Keep only required columns
  mutate(
    DATE = as.Date(as.numeric(DATE), origin = "1899-12-30"),  # Convert Excel date format
    LOCATION = gsub(" \\(.*\\)", "", LOCATION),  # Remove text inside parentheses
    LOCATION = ifelse(LOCATION == "Location Unknown", "Unknown Location", LOCATION),  # Standardize unknown locations
    Store = trimws(sub(" - .*", "", LOCATION)),  # Extract text before '-'
    Location = trimws(sub(".*- ", "", LOCATION)),  # Extract text after '-'
    `Barrel TYPE` = case_when(
      str_detect(ITEMS, "food items|Hunger Sack|Produce|refrigerated items") ~ "Food",
      str_detect(ITEMS, "personal care|personal hygiene|diapers") ~ "Personal Care",
      TRUE ~ "Other"
    ),
    ITEMS = as.numeric(str_extract(ITEMS, "\\d+"))  # Extract numeric values from ITEMS
  ) %>%
  left_join(zipcode_lookup, by = "Location") %>%  # Add Zipcode based on Location
  select(-LOCATION)  # Remove LOCATION column

write.csv(cleanRedBarrel, file = "CleanData/RD_Yearly_Data.csv", row.names = FALSE)
# ========================================================
# Aggregate data by Store and Location
foodByZipData <- cleanRedBarrel %>%
  group_by(Location, Zipcode) %>%
  summarise(
    Total_ITEMS = sum(ITEMS, na.rm = TRUE),
    Total_VALUE = sum(VALUE, na.rm = TRUE)
  )

# ========================================================
# Aggregate data by Store type while keeping year information
foodByStoreType <- cleanRedBarrel %>%
  mutate(Year = format(DATE, "%Y")) %>%  # Extract only the year
  group_by(Store, Year) %>%
  summarise(
    Total_ITEMS = sum(ITEMS, na.rm = TRUE),
    Total_VALUE = sum(VALUE, na.rm = TRUE)
  )

# ========================================================
# Summarize donations by Store and Barrel TYPE
donations_by_store <- cleanRedBarrel %>%
  mutate(Year = format(DATE, "%Y")) %>% 
  group_by(Store, Year, `Barrel TYPE`) %>%
  summarise(Total_ITEMS = sum(ITEMS, na.rm = TRUE)) %>%
  ungroup()

# Agreggate donations by ZIP
donations_by_zip <- cleanRedBarrel %>%
  group_by(Zipcode) %>%
  summarise(Total_Donations = sum(ITEMS, na.rm = TRUE))











































