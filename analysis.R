library(tidyverse)
library(ipumsr)

# read in data
raw <- read_ipums_micro("data/USA Data 00006.xml", data_file = "data/USA Data 00006.dat.gz")

# examine data structure
glimpse(raw)
head(raw)

# check available years
table(raw$YEAR)

# examine Hispanic/Latino variables
table(raw$HISPAN, useNA = "always")
if("HISPRULE" %in% names(raw)) table(raw$HISPRULE, useNA = "always")

# examine other key variables
if("BPL" %in% names(raw)) table(raw$BPL, useNA = "always") # birthplace
if("CITIZEN" %in% names(raw)) table(raw$CITIZEN, useNA = "always") # citizenship
if("REGION" %in% names(raw)) table(raw$REGION, useNA = "always") # region
if("OCC" %in% names(raw)) head(table(raw$OCC, useNA = "always")) # occupation
if("CLASSWKR" %in% names(raw)) table(raw$CLASSWKR, useNA = "always") # class of worker
if("FARM" %in% names(raw)) table(raw$FARM, useNA = "always") # farm status
if("OWNERSHP" %in% names(raw)) table(raw$OWNERSHP, useNA = "always") # homeownership
if("VALUEH" %in% names(raw)) summary(raw$VALUEH) # home value
if("HHINCOME" %in% names(raw)) summary(raw$HHINCOME) # household income
if("REALPROP" %in% names(raw)) summary(raw$REALPROP) # real estate property

# Define Latino population based on HISPAN and HISPRULE variables
data <- raw %>%
  mutate(
    # Create Latino identifier
    latino = case_when(
      HISPAN > 0 & HISPAN < 900 ~ TRUE,
      "HISPRULE" %in% names(.) & HISPRULE > 0 & HISPRULE < 900 ~ TRUE,
      TRUE ~ FALSE
    ),
    # Clean up year variable
    census_year = YEAR,
    # Weight variable for population estimates
    weight = if("PERWT" %in% names(.)) PERWT else if("HHWT" %in% names(.)) HHWT else 1
  )

# 1. Total Latino Population by Year
latino_pop_by_year <- data %>%
  group_by(census_year) %>%
  summarise(
    total_population = sum(weight),
    latino_population = sum(weight[latino == TRUE]),
    .groups = "drop"
  ) %>%
  mutate(
    latino_percentage = round((latino_population / total_population) * 100, 2)
  )

print("Latino Population by Year:")
print(latino_pop_by_year)

# 2. Share Foreign Born among Latinos
if("BPL" %in% names(data)) {
  foreign_born_latinos <- data %>%
    filter(latino == TRUE) %>%
    mutate(
      foreign_born = case_when(
        BPL >= 100 & BPL <= 120 ~ FALSE,  # US states
        BPL > 120 ~ TRUE,                 # Foreign countries
        TRUE ~ NA
      )
    ) %>%
    filter(!is.na(foreign_born)) %>%
    group_by(census_year) %>%
    summarise(
      total_latinos = sum(weight),
      foreign_born_latinos = sum(weight[foreign_born == TRUE]),
      .groups = "drop"
    ) %>%
    mutate(
      pct_foreign_born = round((foreign_born_latinos / total_latinos) * 100, 2)
    )
  
  print("Foreign Born Latinos by Year:")
  print(foreign_born_latinos)
}

# 3. Citizenship Status among Latinos
if("CITIZEN" %in% names(data)) {
  citizenship_latinos <- data %>%
    filter(latino == TRUE) %>%
    filter(!is.na(CITIZEN) & CITIZEN != 0) %>%
    mutate(
      not_citizen = case_when(
        CITIZEN == 1 ~ FALSE,  # Born in US
        CITIZEN == 2 ~ FALSE,  # Naturalized citizen
        CITIZEN == 3 ~ TRUE,   # Not a citizen
        CITIZEN == 4 ~ TRUE,   # Not a citizen, but has received first papers
        TRUE ~ NA
      ),
      first_papers = case_when(
        CITIZEN == 4 ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    filter(!is.na(not_citizen)) %>%
    group_by(census_year) %>%
    summarise(
      total_latinos = sum(weight),
      not_citizens = sum(weight[not_citizen == TRUE]),
      first_papers_only = sum(weight[first_papers == TRUE]),
      .groups = "drop"
    ) %>%
    mutate(
      pct_not_citizen = round((not_citizens / total_latinos) * 100, 2),
      pct_first_papers = round((first_papers_only / total_latinos) * 100, 2)
    )
  
  print("Citizenship Status among Latinos:")
  print(citizenship_latinos)
}

# 4. Latino Subgroup Analysis (by birthplace and HISPAN codes)
latino_subgroups <- data %>%
  filter(latino == TRUE) %>%
  mutate(
    latino_origin = case_when(
      HISPAN == 100 ~ "Mexican",
      HISPAN == 200 ~ "Puerto Rican", 
      HISPAN == 300 ~ "Cuban",
      HISPAN == 401 ~ "Dominican",
      HISPAN == 410 ~ "Central American",
      HISPAN == 420 ~ "South American",
      HISPAN >= 400 & HISPAN < 500 ~ "Other Central/South American",
      HISPAN >= 500 ~ "Other Hispanic",
      TRUE ~ "Hispanic/Latino (unspecified)"
    )
  ) %>%
  group_by(census_year, latino_origin) %>%
  summarise(
    count = sum(weight),
    .groups = "drop"
  ) %>%
  group_by(census_year) %>%
  mutate(
    total_latinos_year = sum(count),
    percentage = round((count / total_latinos_year) * 100, 2)
  )

print("Latino Subgroups by Year:")
print(latino_subgroups)

# 5. Latino Population by Region
if("REGION" %in% names(data)) {
  latino_by_region <- data %>%
    filter(latino == TRUE) %>%
    mutate(
      region_name = case_when(
        REGION == 11 ~ "New England",
        REGION == 12 ~ "Middle Atlantic", 
        REGION == 21 ~ "East North Central",
        REGION == 22 ~ "West North Central",
        REGION == 31 ~ "South Atlantic",
        REGION == 32 ~ "East South Central",
        REGION == 33 ~ "West South Central",
        REGION == 41 ~ "Mountain",
        REGION == 42 ~ "Pacific",
        TRUE ~ as.character(REGION)
      )
    ) %>%
    group_by(census_year, region_name) %>%
    summarise(
      latino_count = sum(weight),
      .groups = "drop"
    ) %>%
    group_by(census_year) %>%
    mutate(
      total_latinos_year = sum(latino_count),
      percentage = round((latino_count / total_latinos_year) * 100, 2)
    )
  
  print("Latino Population by Region:")
  print(latino_by_region)
}

# 6. Top 3 Occupations among Latinos
if("OCC" %in% names(data)) {
  top_occupations <- data %>%
    filter(latino == TRUE & !is.na(OCC) & OCC != 0) %>%
    group_by(census_year, OCC) %>%
    summarise(
      count = sum(weight),
      .groups = "drop"
    ) %>%
    group_by(census_year) %>%
    slice_max(count, n = 3) %>%
    arrange(census_year, desc(count)) %>%
    mutate(rank = row_number())
  
  print("Top 3 Occupations among Latinos by Year:")
  print(top_occupations)
}

# 7. Business Ownership among Latinos (Self-employed)
if("CLASSWKR" %in% names(data)) {
  business_ownership <- data %>%
    filter(latino == TRUE & !is.na(CLASSWKR) & CLASSWKR != 0) %>%
    mutate(
      self_employed = case_when(
        CLASSWKR == 1 ~ TRUE,   # Self-employed
        CLASSWKR == 2 ~ FALSE,  # Works for wages
        TRUE ~ NA
      )
    ) %>%
    filter(!is.na(self_employed)) %>%
    group_by(census_year) %>%
    summarise(
      total_latinos = sum(weight),
      self_employed_latinos = sum(weight[self_employed == TRUE]),
      .groups = "drop"
    ) %>%
    mutate(
      pct_self_employed = round((self_employed_latinos / total_latinos) * 100, 2)
    )
  
  print("Business Ownership (Self-employed) among Latinos:")
  print(business_ownership)
}

# 8. Farm Status among Latino Households
if("FARM" %in% names(data)) {
  farm_status <- data %>%
    filter(latino == TRUE & !is.na(FARM)) %>%
    mutate(
      has_farm = case_when(
        FARM == 1 ~ FALSE,  # No farm
        FARM == 2 ~ TRUE,   # Farm
        TRUE ~ NA
      )
    ) %>%
    filter(!is.na(has_farm)) %>%
    group_by(census_year) %>%
    summarise(
      total_latino_households = sum(weight),
      latino_farm_households = sum(weight[has_farm == TRUE]),
      .groups = "drop"
    ) %>%
    mutate(
      pct_farm_households = round((latino_farm_households / total_latino_households) * 100, 2)
    )
  
  print("Farm Status among Latino Households:")
  print(farm_status)
}

# 9. Homeownership among Latinos
if("OWNERSHP" %in% names(data)) {
  homeownership <- data %>%
    filter(latino == TRUE & !is.na(OWNERSHP) & OWNERSHP != 0) %>%
    mutate(
      owns_home = case_when(
        OWNERSHP == 1 ~ TRUE,   # Owned or being bought (loan)
        OWNERSHP == 2 ~ FALSE,  # Rents
        TRUE ~ NA
      )
    ) %>%
    filter(!is.na(owns_home)) %>%
    group_by(census_year) %>%
    summarise(
      total_latino_households = sum(weight),
      latino_homeowners = sum(weight[owns_home == TRUE]),
      .groups = "drop"
    ) %>%
    mutate(
      pct_homeowners = round((latino_homeowners / total_latino_households) * 100, 2)
    )
  
  print("Homeownership among Latino Households:")
  print(homeownership)
}

# 10. Median Home Value among Latino Homeowners
if("VALUEH" %in% names(data)) {
  home_values <- data %>%
    filter(latino == TRUE & !is.na(VALUEH) & VALUEH > 0 & VALUEH < 9999999) %>%
    group_by(census_year) %>%
    summarise(
      median_home_value = median(rep(VALUEH, weight)),
      .groups = "drop"
    )
  
  print("Median Home Value among Latino Homeowners:")
  print(home_values)
}

# 11. Median Household Income among Latinos
if("HHINCOME" %in% names(data)) {
  household_income <- data %>%
    filter(latino == TRUE & !is.na(HHINCOME) & HHINCOME > 0 & HHINCOME < 9999999) %>%
    group_by(census_year) %>%
    summarise(
      median_hh_income = median(rep(HHINCOME, weight)),
      .groups = "drop"
    )
  
  print("Median Household Income among Latinos:")
  print(household_income)
}

# 12. Real Estate Value (1850 only)
if("REALPROP" %in% names(data)) {
  real_estate_1850 <- data %>%
    filter(latino == TRUE & census_year == 1850 & !is.na(REALPROP) & REALPROP > 0) %>%
    summarise(
      median_real_estate = median(rep(REALPROP, weight)),
      .groups = "drop"
    )
  
  print("Real Estate Value among Latinos in 1850:")
  print(real_estate_1850)
}

# Create output data frame in template format
# Initialize the output dataframe with row labels and years
template_years <- c(1850, 1880, 1910, 1920, 1940, 1960, 1980, 2000, 2005, 2010, 2015, 2020, 2022)

# Create base data frame
output_df <- data.frame(
  Metric = c(
    "Total Population (HISPAN + HISPRULE)",
    "Share Foreign Born (Birthplace) % Born outside US", 
    "Citizenship Status (Citizen; 1870, 1910 and onwards) % Not a citizen",
    "% Not a citizen, but has received first papers (1900 - 1940)",
    "Latino subgroup counts (Birthplace, HISPAN) Percentage",
    "Latino Population across Regions",
    "Top 3 occupations (OCC; none for 1860)",
    "Business Owner (1910 - 2023) CLASSWKR % that are Self-employed",
    "Farm Status (household) FARM % that have a farm", 
    "Homeownership (1900 onwards) OWNERSHP % that Owned or being bought (loan)",
    "Homevalue (1930, 40, 1960 onwards) VALUEH Median Home Value",
    "Household Income (1980 onwards) HHINCOME Median Household Income",
    "Real Estate Value (1850) (REALPROP)"
  ),
  stringsAsFactors = FALSE
)

# Add year columns with empty values initially
for(year in template_years) {
  output_df[[as.character(year)]] <- ""
}

# Fill in the data where available
available_years <- unique(data$census_year)

# 1. Total Latino Population
for(year in available_years) {
  if(year %in% template_years) {
    year_data <- latino_pop_by_year[latino_pop_by_year$census_year == year, ]
    if(nrow(year_data) > 0) {
      output_df[1, as.character(year)] <- formatC(year_data$latino_population, format = "f", digits = 0, big.mark = ",")
    }
  }
}

# 2. Foreign Born Percentage
if(exists("foreign_born_latinos")) {
  for(year in available_years) {
    if(year %in% template_years) {
      year_data <- foreign_born_latinos[foreign_born_latinos$census_year == year, ]
      if(nrow(year_data) > 0) {
        output_df[2, as.character(year)] <- paste0(year_data$pct_foreign_born, "%")
      }
    }
  }
}

# 3. Citizenship Status
if(exists("citizenship_latinos")) {
  for(year in available_years) {
    if(year %in% template_years) {
      year_data <- citizenship_latinos[citizenship_latinos$census_year == year, ]
      if(nrow(year_data) > 0) {
        output_df[3, as.character(year)] <- paste0(year_data$pct_not_citizen, "%")
        output_df[4, as.character(year)] <- paste0(year_data$pct_first_papers, "%")
      }
    }
  }
}

# 5. Business Ownership
if(exists("business_ownership")) {
  for(year in available_years) {
    if(year %in% template_years) {
      year_data <- business_ownership[business_ownership$census_year == year, ]
      if(nrow(year_data) > 0) {
        output_df[8, as.character(year)] <- paste0(year_data$pct_self_employed, "%")
      }
    }
  }
}

# 6. Farm Status  
if(exists("farm_status")) {
  for(year in available_years) {
    if(year %in% template_years) {
      year_data <- farm_status[farm_status$census_year == year, ]
      if(nrow(year_data) > 0) {
        output_df[9, as.character(year)] <- paste0(year_data$pct_farm_households, "%")
      }
    }
  }
}

# 7. Homeownership
if(exists("homeownership")) {
  for(year in available_years) {
    if(year %in% template_years) {
      year_data <- homeownership[homeownership$census_year == year, ]
      if(nrow(year_data) > 0) {
        output_df[10, as.character(year)] <- paste0(year_data$pct_homeowners, "%")
      }
    }
  }
}

# 8. Home Values
if(exists("home_values")) {
  for(year in available_years) {
    if(year %in% template_years) {
      year_data <- home_values[home_values$census_year == year, ]
      if(nrow(year_data) > 0) {
        output_df[11, as.character(year)] <- paste0("$", formatC(year_data$median_home_value, format = "f", digits = 0, big.mark = ","))
      }
    }
  }
}

# 9. Household Income
if(exists("household_income")) {
  for(year in available_years) {
    if(year %in% template_years) {
      year_data <- household_income[household_income$census_year == year, ]
      if(nrow(year_data) > 0) {
        output_df[12, as.character(year)] <- paste0("$", formatC(year_data$median_hh_income, format = "f", digits = 0, big.mark = ","))
      }
    }
  }
}

# 10. Real Estate 1850
if(exists("real_estate_1850") && nrow(real_estate_1850) > 0) {
  output_df[13, "1850"] <- paste0("$", formatC(real_estate_1850$median_real_estate, format = "f", digits = 0, big.mark = ","))
}

# Write output to CSV
write.csv(output_df, "latino_wealth_analysis_output.csv", row.names = FALSE)

print("Analysis complete! Output saved to latino_wealth_analysis_output.csv")
print("Preview of output:")
print(output_df)
