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
- External APIs: FRED (CPI), Zillow (ZORI)
- Visualization: `ggplot2` time series, scatter plots, feature importance charts, maps

## Data Processing
- Aggregated visits by month and household.
- Cleaned and merged economic indicators with visit data.
- Built separate datasets for:
  - Monthly Visit Counts
  - Household-level stats
  - Red Barrel Donations

### Red Barrel Data Processing

- Cleaned and combined multiple Red Barrel Excel files containing store-level donation records (2022–2024).
- Standardized location names and matched stores to ZIP codes using a custom lookup table.
- Extracted and categorized donation types such as Food, Hunger Sack, Produce, Diapers, Personal Care, and Refrigerated items.
- Created aggregated summaries by ZIP code, store, and donation category using the `CleanRedBarrellData.R` script.
- Resulted in cleaned donation data stored in `CleanData/RD_Yearly_Data.csv`.

## Analysis Highlights

### Visitor Forecasting

- Modeled future visit volumes using household-level data and time series regressions.
- Predicted visits from new households using Random Forest and logistic regression.

### CPI and Rent Modeling

- Merged food price (CPI) and rent (ZORI) data with visit records.
- Identified strong associations between inflationary pressure and visit increases.

### Red Barrel Analysis

- Built interactive and static visualizations of food donation patterns across ZIP codes and stores (`RedBarrellModel.R`).
- Mapped total donations by ZIP and highlighted key locations for expansion.
- Analyzed donation volumes by store and barrel type to evaluate contribution patterns over time.

### SNAP Participation Modeling

- Used ZIP-level demographic and economic features to predict SNAP participation rates (`SNAPModel.R`).
- Created a logistic regression model to flag under-participating areas based on structural features.
- Visualized feature correlations, model performance (ROC/AUC), and ZIP code-level risk categories.

### Other Insights

- Identified seasonal trends in donations and visits.
- Proposed a framework to match donation supply with visit demand by location.

## Key Findings

- Economic stressors (rent and food costs) significantly impact pantry usage.
- Red Barrel donations are concentrated in a few ZIP codes; opportunity exists for broader coverage.
- SNAP participation is predictable using ZIP-level socioeconomic data, helping DMARC identify underserved areas.

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

### 8. ZIP Code Donation Map (Red Barrel)

Visualizes food donations per ZIP code and identifies gaps in store participation.

### 9. SNAP Participation Predictions by ZIP Code

Maps predicted SNAP participation levels across the Des Moines area and highlights underserved regions.
