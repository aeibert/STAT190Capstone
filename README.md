# STAT 190 Capstone Project

Title: Modeling Visitors for DMARC

Authors: Amelia Eibert, Jimmy Fiasche, Sam Trujillo

Date: May 13th, 2025

## Introduction

For our data analytics capstone, we worked with the Des Moines Area Relgious Council, which is a network of food pantries across the greater Des Moines area. We were provided with a visit dataset which included anonymized data from their intake system. One row for each individual in a household who visited a pantry that month from 2018-2024. We were also provided information about their Red Barrel Program, where they are able to receive donations through these barrels that are located in stores such as Hyvee, Fareway, or Cashsaver.

Our group examined multiple angles such as economic influences, forecasting new visitors, and evaluating the Red Barrel donation program.

## Research Questions
- How many new visitors may they see?
- How do economic factors affect visits?
- Does Red Barrel Work?
- Where can they add more Red Barrels?

## Data Sources

Visit Data:
- Annual Income, federal poverty level, income source, SNAP Household
- DOB, gender, race, ethnicity, family type
- Housing, Housing type, zip
- Location visited, date of visit

Red Barrel Data: 
- Store Name, Location
- Items Donated
- Value of donations

FRED CPI Data: 
- Meat, Poultry, Fish, Eggs
- Fruits, Veggies
- Cereal, Bakery Items
- Dairy

Zillow ZORI Data:
- Rent trends for Polk County, IA

## Modeling Tools
- R: `tidyverse`, `caret`, `randomForest`, `pROC`, `lubridate`, `glmnet`
- External APIS: FRED (CPI), Zillow (ZORI)
- Visualization: ggplot2 time series, scatter plots, feature importance charts, maps
  
## Data Processing
- Aggregated visits by month and household.
- Cleaned and merged economic indicators with visitata.
- Built separate datasets for:
  - Monthly Visit Counts
  - Household-level stats
  - Red Barrel Donations

## Analysis Highlights
### Visitor Forecasting

### CPI and Rent Modeling

### Red Barrel Analysis

### Other Insights

## Key Findings


