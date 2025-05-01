# Clear workspace
rm(list = ls())
source("Code/CleanData/CleanRedBarrelData.R") # Load cleaned Red Barrel data
source("Code/CleanData/VisitData.R")

# Load libraries
library(sf)
library(tigris)
library(ggplot2)
library(dplyr)
library(stringr)
library(viridis)
library(shiny)
library(RColorBrewer)

# =============================
# Set options for tigris
options(tigris_use_cache = TRUE)

# Get ZIP code boundaries for Iowa
iowa_zips <- zctas(state = "IA", year = 2010)

# Join iowa_zips with foodByZipData on Zipcode and ZCTA5CE10
iowa_zips <- iowa_zips %>%
  left_join(foodByZipData, by = c("ZCTA5CE10" = "Zipcode")) %>%
  mutate(Total_ITEMS = ifelse(is.na(Total_ITEMS), 0, Total_ITEMS)) %>%  # Replace NA with 0 if needed
  mutate(Total_ITEMS = ifelse(Total_ITEMS == 0, NA, Total_ITEMS))
  
# Now, plot the data
ggplot(data = iowa_zips) +
  geom_sf(aes(fill = Total_ITEMS), color = "black") +
  scale_fill_gradient(
    low = "lightblue", high = "darkblue",  
    na.value = "white",  # Make 0 areas white by converting them to NA
    name = "Total Food Items"
  ) +
  labs(
    title = "Red Barrel Collections by ZIP Code",
    subtitle = "Total items collected per ZIP code area"
  ) +
  theme_minimal()

# =============================


# Remove "Unknown" from donations
filter_unknown <- donations_by_store %>%
  filter(!Store %in% c("Unknown Location"))
  
# Plot Type of Item from Each Store

# Create the stacked bar chart grouped by year
ggplot(filter_unknown, aes(x = Store, y = Total_ITEMS, fill = `Barrel TYPE`)) +
  geom_bar(stat = "identity") +  # Stacked bars
  facet_wrap(~ Year) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Donations by Store and Category (Grouped by Year)",
       x = "Store",
       y = "Total Items Donated",
       fill = "Donation Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 75, hjust =1)) +
  coord_flip() # Flip axis for readability
 

# =============================


# removing stores we don't get a lot of
# Remove filtered by year
filtered_stores <- donations_by_store %>%
  filter(!Store %in% c("Unknown Location", "Brick Street Market & Cafe", "Cash Saver",
                       "Hy-Vee Drug #7082", "St. James Lutheran Church"))

ggplot(filtered_stores, aes(x = Store, y = Total_ITEMS, fill = `Barrel TYPE`)) +
  geom_bar(stat = "identity") +  # Stacked bars
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Donations by High Result Stores and Category (2022-2024)",
       x = "Store",
       y = "Total Items Donated",
       fill = "Donation Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust =1)) +
  coord_flip() # Flip axis for readability

# Sample data
donation_data <- cleanRedBarrel %>%
  group_by(Store) %>%
  summarise(Total_ITEMS = sum(ITEMS))



















