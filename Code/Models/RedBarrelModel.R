# =============================
# SETUP
# =============================

# Clear workspace and load data
rm(list = ls())
source("Code/CleanRedBarrelData.R")  # Load cleaned Red Barrel data

# Load libraries
library(sf)
library(tigris)
library(ggplot2)
library(dplyr)
library(stringr)
library(viridis)

# Set tigris options
options(tigris_use_cache = TRUE)

# =============================
# ZIP CODE MAPS: All Iowa ZIP Codes
# =============================

# Get Iowa ZIP code boundaries
iowa_zips <- zctas(state = "IA", year = 2010) %>%
  left_join(foodByZipData, by = c("ZCTA5CE10" = "Zipcode")) %>%
  mutate(
    Total_ITEMS = ifelse(is.na(Total_ITEMS), 0, Total_ITEMS),
    Total_ITEMS = ifelse(Total_ITEMS == 0, NA, Total_ITEMS)
  )

# Plot: Total items collected per ZIP code (statewide)
ggplot(data = iowa_zips) +
  geom_sf(aes(fill = Total_ITEMS / TotalRB), color = "black") +
  scale_fill_gradient(
    low = "lightblue", high = "darkblue", na.value = "white",
    name = "Total Food Items"
  ) +
  labs(
    title = "Red Barrel Collections by ZIP Code",
    subtitle = "Total items collected per ZIP code area from 2022–2024"
  ) +
  theme_minimal()

# =============================
# ZIP CODE MAPS: Labeled Map (non-zero ZIP codes only)
# =============================

# Filter and compute centroids
iowa_zips <- iowa_zips %>%
  filter(!is.na(Total_ITEMS)) %>%
  mutate(centroid = st_centroid(geometry))

# Plot with ZIP code labels
ggplot(data = iowa_zips) +
  geom_sf(aes(fill = Total_ITEMS / TotalRB), color = "black") +
  geom_sf_text(aes(geometry = centroid, label = ZCTA5CE10), size = 3, color = "black") +
  scale_fill_gradient(
    low = "lightblue", high = "darkblue", na.value = "white",
    name = "Total Food Items"
  ) +
  labs(
    title = "Red Barrel Collections by ZIP Code",
    subtitle = "Total items collected per ZIP code area from 2022–2024",
    caption = "Source: DMARC"
  ) +
  theme_minimal()

# =============================
# ZIP CODE MAPS: Allowlist ZIP Codes Only
# =============================

# Define allowlist ZIPs
allowlist <- c("50021", "50023", "50111", "50131", "50263", "50265", "50266",
               "50309", "50310", "50311", "50312", "50313", "50314", "50315",
               "50316", "50317", "50320", "50321", "50322", "50324", "50325",
               "50323")

# Filter and compute centroids
iowa_zips <- zctas(state = "IA", year = 2010) %>%
  left_join(foodByZipData, by = c("ZCTA5CE10" = "Zipcode")) %>%
  mutate(
    Total_ITEMS = ifelse(is.na(Total_ITEMS), 0, Total_ITEMS),
    Total_ITEMS = ifelse(Total_ITEMS == 0, NA, Total_ITEMS)
  ) %>%
  filter(ZCTA5CE10 %in% allowlist) %>%
  mutate(centroid = st_centroid(geometry))

# Plot allowlist ZIPs
ggplot(data = iowa_zips) +
  geom_sf(aes(fill = Total_ITEMS / TotalRB), color = "black") +
  scale_fill_gradient(
    low = "lightblue", high = "darkblue", na.value = "white",
    name = "Average Items Donated"
  ) +
  labs(
    title = "Red Barrel Collections by ZIP Code per Barrel",
    subtitle = "Average items from 2022–2024",
    caption = "Source: DMARC"
  ) +
  theme_minimal()

# =============================
# STORE ANALYSIS: Total Items Over Time by Store
# =============================

# Filter unknown stores
filtered_data <- foodByStoreType %>% 
  filter(!Store %in% c("Unknown Location", "Brick Street Market & Cafe"))

# Line plot by store over time
ggplot(filtered_data, aes(x = Year, y = Total_ITEMS, color = Store, group = Store)) +
  geom_line(size = 1) + 
  geom_point(size = 2) + 
  scale_color_viridis_d(option = "D") +
  labs(
    title = "Total Items Sold by Store Over the Years",
    x = "Year", y = "Total Items", color = "Store"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =============================
# STORE ANALYSIS: Donations by Category and Store
# =============================

# Remove unknown store data
filtered_stores <- donations_by_store %>%
  filter(!Store %in% c("Unknown Location"))

# Stacked bar chart (flipped)
ggplot(filtered_stores, aes(x = Store, y = Total_ITEMS, fill = `Barrel TYPE`)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Year) +
  scale_fill_viridis_d(option = "D") +
  labs(
    title = "Donations by Store and Category (Grouped by Year)",
    x = "Store", y = "Total Items Donated", fill = "Donation Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  coord_flip()

# Stacked bar chart (non-flipped)
ggplot(filtered_stores, aes(x = Store, y = Total_ITEMS, fill = `Barrel TYPE`)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Year) +
  scale_fill_viridis_d(option = "D") +
  labs(
    title = "Donations by Store and Category (Grouped by Year)",
    x = "Store", y = "Total Items Donated", fill = "Donation Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Total stacked donations (no faceting)
ggplot(donations_by_store, aes(x = Store, y = Total_ITEMS, fill = `Barrel TYPE`)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "D") +
  labs(
    title = "Donations by Store and Category",
    x = "Store", y = "Total Items Donated", fill = "Donation Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))