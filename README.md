# STAT 190 Capstone Project

Title: Understanding and Predicting Food Pantry Usage

Authors: Amelia Eibert, Jimmy Fiasche, Sam Trujillo

Date: May 13th, 2025

## Introduction

For our Data Analytics Capstone project, we partnered with the Des Moines Area Religious Council (DMARC), a network of food pantries serving the greater Des Moines area. We worked with a comprehensive dataset of anonymized pantry visits from 2018 to 2024, where each row represents an individual household member who received assistance during a given month. Additionally, we analyzed data from DMARC’s Red Barrel Program, which collects donations at local grocery stores like Hy-Vee, Fareway, and Cash Saver.

Our group explored this data from several perspectives including the impact of economic conditions on visit volume, forecasting patterns among new visitors, and assessing the role of Red Barrel donations in meeting community needs.

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
- Cleaned and merged economic indicators with visit data.
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

## Visuals

### 1. Actual vs Predicted Household Visits

Shows the performance of our visit prediction model using Random Forest regression.

### 2. Actual vs Predicted Total Individual Visits

Demonstrates prediction accuracy at the individual level across months.

### 3. Food Prices (CPI) vs Pantry Visits

Highlights the positive correlation between rising food prices and increased pantry demand.

### 4. DMARC Visits and Food CPI Over Time

Tracks visit volume alongside food price inflation.

### 5. Rent (ZORI) vs Pantry Visits

Suggests upward rent pressure may influence visit volume.

### 6. Donations by Store and Category (Red Barrel Program)

Illustrates how much each store contributed by donation type and year.

### 7. ROC Curve: New Visitor Classification 

Compares model performance in forecasting new pantry visitors.






