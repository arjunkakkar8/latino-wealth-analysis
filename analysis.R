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
table(raw$HISPRULE, useNA = "always")

# examine other key variables
table(raw$BPL, useNA = "always") # birthplace
table(raw$CITIZEN, useNA = "always") # citizenship
table(raw$REGION, useNA = "always") # region
# occupation
table(raw$CLASSWKR, useNA = "always") # class of worker
table(raw$FARM, useNA = "always") # farm status
table(raw$OWNERSHP, useNA = "always") # homeownership

summary(raw$VALUEH) # home value
summary(raw$HHINCOME) # household income
summary(raw$REALPROP) # real estate property

# Define Latino population based on HISPAN and HISPRULE variables
data <- raw %>%
  mutate(
    # Create Latino identifier based on codebook: HISPAN codes 1-4 are Hispanic, 0=Not Hispanic
    latino = case_when(
      HISPAN >= 1 & HISPAN <= 4 ~ TRUE,
      .default = FALSE
    ),
    latino_origin = case_when(
      HISPAND > 0 & HISPAND < 200 ~ "Mexican",
      HISPAND == 200 ~ "Puerto Rican",
      HISPAND == 300 ~ "Cuban",
      HISPAND == 460 ~ "Dominican",
      HISPAND >= 401 & HISPAND <= 417 ~ "Central American",
      HISPAND >= 420 & HISPAND <= 431 ~ "South American",
      HISPAND %in% c(450, 470, 480, 498) ~ "Other Latino",
      .default = "Not Latino"
    ),
  )

summarization_data <- rbind(
  data %>% filter(latino == TRUE),
  data %>%
    filter(latino == TRUE) %>%
    mutate(latino_origin = "Total")
)

# 1. Total Latino Population by Year (Person-level analysis)
latino_pop_by_year <- summarization_data %>%
  group_by(YEAR, group = latino_origin) %>%
  summarise(
    value = sum(PERWT),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = group, values_from = value)

# 2. Share Foreign Born among Latinos (Person-level analysis)
foreign_born_latinos <- summarization_data %>%
  mutate(
    # Based on codebook: BPL codes 001-120 are US states/territories, >120 are foreign countries
    foreign_born = case_when(
      BPL >= 1 & BPL <= 120 ~ FALSE, # US states and territories
      BPL > 120 ~ TRUE, # Foreign countries
      .default = NA
    )
  ) %>%
  filter(!is.na(foreign_born)) %>%
  group_by(YEAR, group = latino_origin) %>%
  summarise(
    value = sum(PERWT),
    filtered_value = sum(ifelse(foreign_born == TRUE, PERWT, 0)),
    .groups = "drop"
  ) %>%
  mutate(
    percent_filtered = 100 * filtered_value / value
  ) %>%
  select(-value, -filtered_value) %>%
  pivot_wider(names_from = group, values_from = percent_filtered)

# 3. Citizenship Status among Latinos (Person-level analysis)
citizenship_latinos <- data %>%
  filter(latino == TRUE) %>%
  filter(!is.na(CITIZEN) & CITIZEN != 0) %>%
  mutate(
    # Based on codebook: 1=Born abroad of American parents, 2=Naturalized, 3=Not citizen, 4=First papers
    citizen = case_when(
      CITIZEN == 1 ~ TRUE, # Born abroad of American parents (citizen)
      CITIZEN == 2 ~ TRUE, # Naturalized citizen
      CITIZEN == 3 ~ FALSE, # Not a citizen
      CITIZEN == 4 ~ FALSE, # Not a citizen, but has received first papers
      .default = NA
    ),
    not_citizen = case_when(
      CITIZEN == 1 ~ FALSE, # Born abroad of American parents (citizen)
      CITIZEN == 2 ~ FALSE, # Naturalized citizen
      CITIZEN == 3 ~ TRUE, # Not a citizen
      CITIZEN == 4 ~ TRUE, # Not a citizen, but has received first papers
      .default = NA
    ),
    first_papers = case_when(
      CITIZEN == 4 ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(!is.na(citizen)) %>%
  group_by(YEAR) %>%
  summarise(
    total_latinos = sum(PERWT),
    citizens = sum(ifelse(citizen == TRUE, PERWT, 0)),
    not_citizens = sum(ifelse(not_citizen == TRUE, PERWT, 0)),
    first_papers_only = sum(ifelse(first_papers == TRUE, PERWT, 0)),
    .groups = "drop"
  ) %>%
  mutate(
    pct_citizen = 100 * citizens / total_latinos,
    pct_not_citizen = 100 * not_citizens / total_latinos,
    pct_first_papers = 100 * first_papers_only / total_latinos
  )

# 4. Latino Subgroup Analysis (Person-level analysis)
latino_subgroups <- data %>%
  filter(latino == TRUE) %>%
  mutate(
    # Based on codebook: HISPAN codes 1=Mexican, 2=Puerto Rican, 3=Cuban, 4=Other
    latino_origin = case_when(
      HISPAN == 1 ~ "Mexican",
      HISPAN == 2 ~ "Puerto Rican",
      HISPAN == 3 ~ "Cuban",
      HISPAN == 4 ~ "Other Hispanic",
      HISPRULE == 1 ~ "Hispanic by birthplace",
      HISPRULE == 2 ~ "Hispanic by parent birthplace",
      HISPRULE == 3 ~ "Hispanic by grandparent birthplace",
      HISPRULE %in% 4:5 ~ "Hispanic by family relationship",
      HISPRULE %in% 6:8 ~ "Hispanic by surname",
      TRUE ~ "Hispanic/Latino (unspecified)"
    )
  ) %>%
  group_by(YEAR, latino_origin) %>%
  summarise(
    count = sum(PERWT),
    .groups = "drop"
  ) %>%
  group_by(YEAR) %>%
  mutate(
    total_latinos_year = sum(count),
    percentage = round((count / total_latinos_year) * 100, 2)
  )

print("Latino Subgroups by Year:")
print(latino_subgroups)

# 5. Latino Population by Region (Person-level analysis)
if ("REGION" %in% names(data)) {
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
    group_by(YEAR, region_name) %>%
    summarise(
      latino_count = sum(PERWT),
      .groups = "drop"
    ) %>%
    group_by(YEAR) %>%
    mutate(
      total_latinos_year = sum(latino_count),
      percentage = round((latino_count / total_latinos_year) * 100, 2)
    )

  print("Latino Population by Region:")
  print(latino_by_region)
}

# 6. Top 3 Occupations among Latinos (Person-level analysis)
if ("OCC" %in% names(data)) {
  top_occupations <- data %>%
    filter(latino == TRUE & !is.na(OCC) & OCC != 0) %>%
    group_by(YEAR, OCC) %>%
    summarise(
      count = sum(PERWT),
      .groups = "drop"
    ) %>%
    group_by(YEAR) %>%
    slice_max(count, n = 3) %>%
    arrange(YEAR, desc(count)) %>%
    mutate(rank = row_number())

  print("Top 3 Occupations among Latinos by Year:")
  print(top_occupations)
}

# 7. Business Ownership among Latinos (Person-level analysis)
if ("CLASSWKR" %in% names(data)) {
  business_ownership <- data %>%
    filter(latino == TRUE & !is.na(CLASSWKR) & CLASSWKR != 0) %>%
    mutate(
      # Based on codebook: 1=Self-employed, 2=Works for wages
      self_employed = case_when(
        CLASSWKR == 1 ~ TRUE, # Self-employed
        CLASSWKR == 2 ~ FALSE, # Works for wages
        TRUE ~ NA
      )
    ) %>%
    filter(!is.na(self_employed)) %>%
    group_by(YEAR) %>%
    summarise(
      total_latinos = sum(PERWT),
      self_employed_latinos = sum(PERWT[self_employed == TRUE]),
      .groups = "drop"
    ) %>%
    mutate(
      pct_self_employed = round((self_employed_latinos / total_latinos) * 100, 2)
    )

  print("Business Ownership (Self-employed) among Latinos:")
  print(business_ownership)
}

# 8. Farm Status among Latino Households (Household-level analysis)
if ("FARM" %in% names(data)) {
  farm_status <- data %>%
    filter(latino == TRUE & !is.na(FARM) & PERNUM == 1) %>% # Use only householder records
    mutate(
      # Based on codebook: 1=Non-Farm, 2=Farm
      has_farm = case_when(
        FARM == 1 ~ FALSE, # Non-farm
        FARM == 2 ~ TRUE, # Farm
        TRUE ~ NA
      )
    ) %>%
    filter(!is.na(has_farm)) %>%
    group_by(YEAR) %>%
    summarise(
      total_latino_households = sum(HHWT),
      latino_farm_households = sum(HHWT[has_farm == TRUE]),
      .groups = "drop"
    ) %>%
    mutate(
      pct_farm_households = round((latino_farm_households / total_latino_households) * 100, 2)
    )

  print("Farm Status among Latino Households:")
  print(farm_status)
}

# 9. Homeownership among Latino Households (Household-level analysis)
if ("OWNERSHP" %in% names(data)) {
  homeownership <- data %>%
    filter(latino == TRUE & !is.na(OWNERSHP) & OWNERSHP != 0 & PERNUM == 1) %>% # Use only householder records
    mutate(
      # Based on codebook: 1=Owned or being bought (loan), 2=Rented
      owns_home = case_when(
        OWNERSHP == 1 ~ TRUE, # Owned or being bought (loan)
        OWNERSHP == 2 ~ FALSE, # Rented
        TRUE ~ NA
      )
    ) %>%
    filter(!is.na(owns_home)) %>%
    group_by(YEAR) %>%
    summarise(
      total_latino_households = sum(HHWT),
      latino_homeowners = sum(HHWT[owns_home == TRUE]),
      .groups = "drop"
    ) %>%
    mutate(
      pct_homeowners = round((latino_homeowners / total_latino_households) * 100, 2)
    )

  print("Homeownership among Latino Households:")
  print(homeownership)
}

# 10. Median Home Value among Latino Homeowners (Household-level analysis)
if ("VALUEH" %in% names(data)) {
  home_values <- data %>%
    filter(latino == TRUE & !is.na(VALUEH) & VALUEH > 0 & VALUEH < 9999999 & PERNUM == 1) %>% # Use only householder records
    group_by(YEAR) %>%
    summarise(
      median_home_value = median(rep(VALUEH, HHWT)),
      .groups = "drop"
    )

  print("Median Home Value among Latino Homeowners:")
  print(home_values)
}

# 11. Median Household Income among Latino Households (Household-level analysis)
if ("HHINCOME" %in% names(data)) {
  household_income <- data %>%
    filter(latino == TRUE & !is.na(HHINCOME) & HHINCOME > 0 & HHINCOME < 9999999 & PERNUM == 1) %>% # Use only householder records
    group_by(YEAR) %>%
    summarise(
      median_hh_income = median(rep(HHINCOME, HHWT)),
      .groups = "drop"
    )

  print("Median Household Income among Latino Households:")
  print(household_income)
}

# 12. Real Estate Value (1850 only) (Person-level analysis)
if ("REALPROP" %in% names(data)) {
  real_estate_1850 <- data %>%
    filter(latino == TRUE & YEAR == 1850 & !is.na(REALPROP) & REALPROP > 0) %>%
    summarise(
      median_real_estate = median(rep(REALPROP, PERWT)),
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
for (year in template_years) {
  output_df[[as.character(year)]] <- ""
}

# Fill in the data where available
available_years <- unique(data$YEAR)

# 1. Total Latino Population
for (year in available_years) {
  if (year %in% template_years) {
    year_data <- latino_pop_by_year[latino_pop_by_year$YEAR == year, ]
    if (nrow(year_data) > 0) {
      output_df[1, as.character(year)] <- formatC(year_data$latino_population, format = "f", digits = 0, big.mark = ",")
    }
  }
}

# 2. Foreign Born Percentage
if (exists("foreign_born_latinos")) {
  for (year in available_years) {
    if (year %in% template_years) {
      year_data <- foreign_born_latinos[foreign_born_latinos$YEAR == year, ]
      if (nrow(year_data) > 0) {
        output_df[2, as.character(year)] <- paste0(year_data$pct_foreign_born, "%")
      }
    }
  }
}

# 3. Citizenship Status
if (exists("citizenship_latinos")) {
  for (year in available_years) {
    if (year %in% template_years) {
      year_data <- citizenship_latinos[citizenship_latinos$YEAR == year, ]
      if (nrow(year_data) > 0) {
        output_df[3, as.character(year)] <- paste0(year_data$pct_not_citizen, "%")
        output_df[4, as.character(year)] <- paste0(year_data$pct_first_papers, "%")
      }
    }
  }
}

# 5. Business Ownership
if (exists("business_ownership")) {
  for (year in available_years) {
    if (year %in% template_years) {
      year_data <- business_ownership[business_ownership$YEAR == year, ]
      if (nrow(year_data) > 0) {
        output_df[8, as.character(year)] <- paste0(year_data$pct_self_employed, "%")
      }
    }
  }
}

# 6. Farm Status
if (exists("farm_status")) {
  for (year in available_years) {
    if (year %in% template_years) {
      year_data <- farm_status[farm_status$YEAR == year, ]
      if (nrow(year_data) > 0) {
        output_df[9, as.character(year)] <- paste0(year_data$pct_farm_households, "%")
      }
    }
  }
}

# 7. Homeownership
if (exists("homeownership")) {
  for (year in available_years) {
    if (year %in% template_years) {
      year_data <- homeownership[homeownership$YEAR == year, ]
      if (nrow(year_data) > 0) {
        output_df[10, as.character(year)] <- paste0(year_data$pct_homeowners, "%")
      }
    }
  }
}

# 8. Home Values
if (exists("home_values")) {
  for (year in available_years) {
    if (year %in% template_years) {
      year_data <- home_values[home_values$YEAR == year, ]
      if (nrow(year_data) > 0) {
        output_df[11, as.character(year)] <- paste0("$", formatC(year_data$median_home_value, format = "f", digits = 0, big.mark = ","))
      }
    }
  }
}

# 9. Household Income
if (exists("household_income")) {
  for (year in available_years) {
    if (year %in% template_years) {
      year_data <- household_income[household_income$YEAR == year, ]
      if (nrow(year_data) > 0) {
        output_df[12, as.character(year)] <- paste0("$", formatC(year_data$median_hh_income, format = "f", digits = 0, big.mark = ","))
      }
    }
  }
}

# 10. Real Estate 1850
if (exists("real_estate_1850") && nrow(real_estate_1850) > 0) {
  output_df[13, "1850"] <- paste0("$", formatC(real_estate_1850$median_real_estate, format = "f", digits = 0, big.mark = ","))
}

# Write output to CSV
write.csv(output_df, "latino_wealth_analysis_output.csv", row.names = FALSE)

print("Analysis complete! Output saved to latino_wealth_analysis_output.csv")
print("Preview of output:")
print(output_df)
