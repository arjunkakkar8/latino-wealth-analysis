library(tidyverse)
library(spatstat)
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

household_summarization_data <- summarization_data %>%
  filter(PERNUM == 1)

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
      BPL > 120 ~ TRUE, # Foreign countrie,
    filtered_value = sum(ifelse(foreign_born == TRUE, PERWT, 0)),
    .groups = "drop"
  ) %>%
  mutate(
    percent_filtered = 100 * filtered_value / value
  ) %>%
  select(-value, -filtered_value) %>%
  pivot_wider(names_from = group, values_from = percent_filtered)

# 3. Citizenship Status among Latinos (Person-level analysis)
citizenship_latinos <- summarization_data %>%
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
  group_by(YEAR, group = latino_origin) %>%
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
  ) %>%
  select(YEAR, group, pct_citizen, pct_not_citizen, pct_first_papers) %>%
  pivot_longer(cols = c(pct_citizen, pct_not_citizen, pct_first_papers), 
               names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = group, values_from = value)

# 5. Latino Population by Region (Person-level analysis)
latino_by_region <- summarization_data %>%
  mutate(
    region_name = case_when(
      REGION >= 11 & REGION <= 13 ~ "Northeast",
      REGION >= 21 & REGION <= 23 ~ "Midwest",
      REGION >= 31 & REGION <= 34 ~ "South",
      REGION >= 41 & REGION <= 43 ~ "West",
      .default = NA
    )
  ) %>%
  filter(!is.na(region_name)) %>%
  group_by(YEAR, group = latino_origin, region_name) %>%
  summarise(
    latino_count = sum(ifelse(!is.na(REGION), PERWT, 0)),
    .groups = "drop"
  ) %>%
  group_by(YEAR, group) %>%
  mutate(
    total_latinos_year = sum(latino_count),
    percentage = ifelse(total_latinos_year > 0, round((latino_count / total_latinos_year) * 100, 2), NA)
  ) %>%
  ungroup() %>%
  select(YEAR, group, region_name, percentage) %>%
  pivot_wider(names_from = group, values_from = percentage)

# 6. Top 3 Occupations among Latinos (Person-level analysis)
top_occupations <- summarization_data %>%
  filter(!is.na(OCC) & OCC != 0 & OCC != 999) %>%
  group_by(YEAR, group = latino_origin, OCC) %>%
  summarise(
    count = sum(PERWT),
    .groups = "drop"
  ) %>%
  filter(count > 0) %>%
  group_by(YEAR, group) %>%
  slice_max(count, n = 3, with_ties = FALSE) %>%
  arrange(YEAR, group, desc(count)) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  pivot_wider(names_from = group, values_from = c(OCC, count), names_sep = "_")

# 7. Business Ownership among Latinos (Person-level analysis)
business_ownership <- summarization_data %>%
  filter(!is.na(CLASSWKR) & CLASSWKR != 0 ) %>%
  mutate(
    # Based on codebook: 1=Self-employed, 2=Works for wages
    self_employed = case_when(
      CLASSWKR == 1 ~ TRUE, # Self-employed
      CLASSWKR == 2 ~ FALSE, # Works for wages
      .default = NA
    )
  ) %>%
  filter(!is.na(self_employed)) %>%
  group_by(YEAR, group = latino_origin) %>%
  summarise(
    total_latinos = sum(PERWT),
    self_employed_latinos = sum(ifelse(self_employed == TRUE, PERWT, 0)),
    .groups = "drop"
  ) %>%
  mutate(
    pct_self_employed = 100 * self_employed_latinos / total_latinos
  ) %>%
  select(YEAR, group, pct_self_employed) %>%
  pivot_wider(names_from = group, values_from = pct_self_employed)

# 8. Farm Status among Latino Households (Household-level analysis)
farm_status <- household_summarization_data %>%
  mutate(
    # Based on codebook: 1=Non-Farm, 2=Farm
    has_farm = case_when(
      FARM == 1 ~ FALSE, # Non-farm
      FARM == 2 ~ TRUE, # Farm
      .default = NA
    )
  ) %>%
  filter(!is.na(has_farm)) %>%
  group_by(YEAR, group = latino_origin) %>%
  summarise(
    total_latino_households = sum(HHWT),
    latino_farm_households = sum(ifelse(has_farm == TRUE, HHWT, 0)),
    .groups = "drop"
  ) %>%
  mutate(
    pct_farm_households = 100 * latino_farm_households / total_latino_households
  ) %>%
  select(YEAR, group, pct_farm_households) %>%
  pivot_wider(names_from = group, values_from = pct_farm_households)

# 9. Homeownership among Latino Households (Household-level analysis)
homeownership <- household_summarization_data %>%
  mutate(
    # Based on codebook: 1=Owned or being bought (loan), 2=Rented
    owns_home = case_when(
      OWNERSHP == 1 ~ TRUE, # Owned or being bought (loan)
      OWNERSHP == 2 ~ FALSE, # Rented
      .default = NA
    )
  ) %>%
  filter(!is.na(owns_home)) %>%
  group_by(YEAR, group = latino_origin) %>%
  summarise(
    total_latino_households = sum(HHWT),
    latino_homeowners = sum(ifelse(owns_home == TRUE, HHWT, 0)),
    .groups = "drop"
  ) %>%
  mutate(
    pct_homeowners = 100 * latino_homeowners / total_latino_households
  ) %>%
  select(YEAR, group, pct_homeowners) %>%
  pivot_wider(names_from = group, values_from = pct_homeowners)

# 10. Median Home Value among Latino Homeowners (Household-level analysis)
home_values <- household_summarization_data %>%
  filter(!is.na(VALUEH) & VALUEH > 0 & VALUEH < 9999998) %>%
  group_by(YEAR, group = latino_origin) %>%
  summarise(
    median_home_value = weighted.median(VALUEH, HHWT),
    .groups = "drop"
  ) %>%
  select(YEAR, group, median_home_value) %>%
  pivot_wider(names_from = group, values_from = median_home_value)

# 11. Median Household Income among Latino Households (Household-level analysis)
household_income <- household_summarization_data %>%
  filter(!is.na(HHINCOME) & HHINCOME < 9999999) %>%
  group_by(YEAR, group = latino_origin) %>%
  summarise(
    median_hh_income = weighted.median(HHINCOME, HHWT),
    .groups = "drop"
  ) %>%
  select(YEAR, group, median_hh_income) %>%
  pivot_wider(names_from = group, values_from = median_hh_income)

# 12. Real Estate Value (1850 only) (Person-level analysis)
real_estate_1850 <- summarization_data %>%
  filter(!is.na(REALPROP) & REALPROP > 0 & REALPROP < 999998) %>% 
  group_by(YEAR, group = latino_origin) %>%
  summarise(
    median_real_estate = weighted.median(REALPROP, PERWT),
    .groups = "drop"
  ) %>%
  select(YEAR, group, median_real_estate) %>%
  pivot_wider(names_from = group, values_from = median_real_estate)
