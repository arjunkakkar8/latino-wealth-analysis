library(tidyverse)
library(spatstat)
library(ipumsr)
library(rlang)
library(arrow)

# Wrapper function to calculate estimates with standard errors using replicate weights
calculate_with_se <- function(data, computation_fn, weight_type = "person") {
  # Determine which replicate weights to use
  if (weight_type == "person") {
    main_weight <- "PERWT"
    rep_weights <- paste0("REPWTP", 1:80)
  } else if (weight_type == "household") {
    main_weight <- "HHWT"
    rep_weights <- paste0("REPWT", 1:80)
  } else {
    stop("weight_type must be 'person' or 'household'")
  }

  # Calculate main estimate using primary weight
  data_with_weight <- data %>%
    mutate(weight = !!sym(main_weight))

  main_estimate <- computation_fn(data_with_weight)

  # Calculate replicate estimates
  rep_estimates <- map_dfr(rep_weights, function(rep_wt) {
    data_with_rep_weight <- data %>%
      mutate(weight = !!sym(rep_wt))

    result <- computation_fn(data_with_rep_weight)
    return(result)
  })

  # Calculate standard errors
  # Assume first column is labels, rest are values needing variance calculations
  label_col <- names(main_estimate)[1]
  value_cols <- names(main_estimate)[-1]

  # Calculate variance for each cell (row-column combination)
  se_results <- rep_estimates %>%
    group_by(!!sym(label_col)) %>%
    summarise(
      across(all_of(value_cols),
        ~ sqrt(sum((. - mean(., na.rm = TRUE))^2, na.rm = TRUE) / 79), # 79 = 80-1 degrees of freedom
        .names = "{.col}_se"
      ),
      .groups = "drop"
    )

  # Join main estimates with standard errors
  result <- main_estimate %>%
    left_join(se_results, by = label_col)

  return(result)
}

# read in data
raw <- read_csv_arrow("data/usa_00011.csv")

# examine data structure
glimpse(raw)
head(raw)

# check available years
table(raw$YEAR)

# examine Hispanic/Latino variables
table(raw$HISPAN, useNA = "always")

# examine other key variables
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
data <- raw |>
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
    region_name = case_when(
      REGION >= 11 & REGION <= 13 ~ "Northeast",
      REGION >= 21 & REGION <= 23 ~ "Midwest",
      REGION >= 31 & REGION <= 34 ~ "South",
      REGION >= 41 & REGION <= 43 ~ "West",
      .default = NA
    )
  )

summarization_data <- rbind(
  data %>% filter(latino == TRUE),
  data %>%
    filter(latino == TRUE) %>%
    mutate(latino_origin = "Total", region_name = "Total")
)

household_summarization_data <- summarization_data %>%
  filter(PERNUM == 1)

# 1. Total Latino Population by Year (Person-level analysis)
latino_pop_by_year <- calculate_with_se(
  data = summarization_data,
  computation_fn = function(data) {
    data %>%
      group_by(YEAR, group = latino_origin) %>%
      summarise(
        value = sum(weight),
        .groups = "drop"
      ) %>%
      pivot_wider(names_from = group, values_from = value)
  },
  weight_type = "person"
)

write_csv(latino_pop_by_year, "output/population.csv")


# 2. Share Foreign Born among Latinos (Person-level analysis)
foreign_born_latinos_origin <- calculate_with_se(
  data = summarization_data %>%
    mutate(
      # Based on codebook: BPL codes 001-120 are US states/territories, >120 are foreign countries
      foreign_born = case_when(
        BPL >= 1 & BPL <= 120 ~ FALSE, # US states and territories
        BPL > 120 ~ TRUE, # Foreign countries
        .default = NA
      )
    ) %>%
    filter(!is.na(foreign_born)),
  computation_fn = function(data) {
    data %>%
      group_by(YEAR, group = latino_origin) %>%
      summarise(
        value = sum(weight),
        filtered_value = sum(ifelse(foreign_born == TRUE, weight, 0)),
        .groups = "drop"
      ) %>%
      mutate(
        percent_filtered = 100 * filtered_value / value
      ) %>%
      select(-value, -filtered_value) %>%
      pivot_wider(names_from = group, values_from = percent_filtered)
  },
  weight_type = "person"
)

write_csv(foreign_born_latinos_origin, "output/foreign_born_origin.csv")

foreign_born_latinos_region <- calculate_with_se(
  data = summarization_data %>%
    mutate(
      # Based on codebook: BPL codes 001-120 are US states/territories, >120 are foreign countries
      foreign_born = case_when(
        BPL >= 1 & BPL <= 120 ~ FALSE, # US states and territories
        BPL > 120 ~ TRUE, # Foreign countries
        .default = NA
      )
    ) %>%
    filter(!is.na(foreign_born) & !is.na(region_name)),
  computation_fn = function(data) {
    data %>%
      group_by(YEAR, group = region_name) %>%
      summarise(
        value = sum(weight),
        filtered_value = sum(ifelse(foreign_born == TRUE, weight, 0)),
        .groups = "drop"
      ) %>%
      mutate(
        percent_filtered = 100 * filtered_value / value
      ) %>%
      select(-value, -filtered_value) %>%
      pivot_wider(names_from = group, values_from = percent_filtered)
  },
  weight_type = "person"
)

write_csv(foreign_born_latinos_region, "output/foreign_born_region.csv")


# 3. Citizenship Status among Latinos (Person-level analysis)
citizenship_latinos_origin <- calculate_with_se(
  data = summarization_data %>%
    filter(!is.na(CITIZEN) & CITIZEN != 0) %>%
    mutate(
      # Based on codebook: 1=Born abroad of American parents, 2=Naturalized, 3=Not citizen, 4=First papers
      citizen = case_when(
        CITIZEN == 1 ~ TRUE, # Born abroad of American parents (citizen)
        CITIZEN == 2 ~ TRUE, # Naturalized citizen
        CITIZEN == 3 ~ FALSE, # Not a citizen
        CITIZEN == 4 ~ TRUE, # Not a citizen, but has received first papers
        .default = NA
      )
    ) %>%
    filter(!is.na(citizen)),
  computation_fn = function(data) {
    data %>%
      group_by(YEAR, group = latino_origin) %>%
      summarise(
        total_latinos = sum(weight),
        citizens = sum(ifelse(citizen == TRUE, weight, 0)),
        .groups = "drop"
      ) %>%
      mutate(
        pct_citizen = 100 * citizens / total_latinos
      ) %>%
      select(YEAR, group, pct_citizen) %>%
      pivot_wider(names_from = group, values_from = pct_citizen)
  },
  weight_type = "person"
)

write_csv(citizenship_latinos_origin, "output/citizenship_origin.csv")


citizenship_latinos_region <- calculate_with_se(
  data = summarization_data %>%
    filter(!is.na(CITIZEN) & CITIZEN != 0) %>%
    mutate(
      # Based on codebook: 1=Born abroad of American parents, 2=Naturalized, 3=Not citizen, 4=First papers
      citizen = case_when(
        CITIZEN == 1 ~ TRUE, # Born abroad of American parents (citizen)
        CITIZEN == 2 ~ TRUE, # Naturalized citizen
        CITIZEN == 3 ~ FALSE, # Not a citizen
        CITIZEN == 4 ~ TRUE, # Not a citizen, but has received first papers
        .default = NA
      ),
      first_papers = case_when(
        CITIZEN == 4 ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    filter(!is.na(citizen) & !is.na(region_name)),
  computation_fn = function(data) {
    data %>%
      group_by(YEAR, group = region_name) %>%
      summarise(
        total_latinos = sum(weight),
        citizens = sum(ifelse(citizen == TRUE, weight, 0)),
        first_papers_only = sum(ifelse(first_papers == TRUE, weight, 0)),
        .groups = "drop"
      ) %>%
      mutate(
        pct_citizen = 100 * citizens / total_latinos
      ) %>%
      select(YEAR, group, pct_citizen) %>%
      pivot_wider(names_from = group, values_from = pct_citizen)
  },
  weight_type = "person"
)

write_csv(citizenship_latinos_region, "output/citizenship_region.csv")

# 5. Latino Population by Region (Person-level analysis)
latino_by_region <- calculate_with_se(
  data = summarization_data %>%
    filter(!is.na(region_name)),
  computation_fn = function(data) {
    data %>%
      group_by(YEAR, group = latino_origin, region_name) %>%
      summarise(
        latino_count = sum(ifelse(!is.na(REGION), weight, 0)),
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
  },
  weight_type = "person"
)

write_csv(latino_by_region, "output/population_by_region.csv")


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

write_csv(top_occupations, "output/top_occupations.csv")

education_origin <- calculate_with_se(
  data = summarization_data %>%
    filter(EDUCD >= 2 & EDUCD <= 116),
  computation_fn = function(data) {
    data %>%
      group_by(YEAR, group = latino_origin) %>%
      summarise(
        total_latinos = sum(weight),
        bachelors_above = sum(ifelse(EDUCD > 100, weight, 0)),
        .groups = "drop"
      ) %>%
      mutate(
        pct = 100 * bachelors_above / total_latinos
      ) %>%
      select(YEAR, group, pct) %>%
      pivot_wider(names_from = group, values_from = pct)
  },
  weight_type = "person"
)

write_csv(education_origin, "output/education_origin.csv")

education_region <- calculate_with_se(
  data = summarization_data %>%
    filter(EDUCD >= 2 & EDUCD <= 116 & !is.na(region_name)),
  computation_fn = function(data) {
    data %>%
      group_by(YEAR, group = region_name) %>%
      summarise(
        total_latinos = sum(weight),
        bachelors_above = sum(ifelse(EDUCD > 100, weight, 0)),
        .groups = "drop"
      ) %>%
      mutate(
        pct = 100 * bachelors_above / total_latinos
      ) %>%
      select(YEAR, group, pct) %>%
      pivot_wider(names_from = group, values_from = pct)
  },
  weight_type = "person"
)

write_csv(education_region, "output/education_region.csv")


# 7. Business Ownership among Latinos (Person-level analysis)
business_ownership <- calculate_with_se(
  data = summarization_data %>%
    filter(!is.na(CLASSWKR) & CLASSWKR != 0) %>%
    mutate(
      # Based on codebook: 1=Self-employed, 2=Works for wages
      self_employed = case_when(
        CLASSWKR == 1 ~ TRUE, # Self-employed
        CLASSWKR == 2 ~ FALSE, # Works for wages
        .default = NA
      )
    ) %>%
    filter(!is.na(self_employed)),
  computation_fn = function(data) {
    data %>%
      group_by(YEAR, group = latino_origin) %>%
      summarise(
        total_latinos = sum(weight),
        self_employed_latinos = sum(ifelse(self_employed == TRUE, weight, 0)),
        .groups = "drop"
      ) %>%
      mutate(
        pct_self_employed = 100 * self_employed_latinos / total_latinos
      ) %>%
      select(YEAR, group, pct_self_employed) %>%
      pivot_wider(names_from = group, values_from = pct_self_employed)
  },
  weight_type = "person"
)

write_csv(business_ownership, "output/business_ownership.csv")


# 8. Farm Status among Latino Households (Household-level analysis)
farm_status <- calculate_with_se(
  data = household_summarization_data %>%
    mutate(
      # Based on codebook: 1=Non-Farm, 2=Farm
      has_farm = case_when(
        FARM == 1 ~ FALSE, # Non-farm
        FARM == 2 ~ TRUE, # Farm
        .default = NA
      )
    ) %>%
    filter(!is.na(has_farm)),
  computation_fn = function(data) {
    data %>%
      group_by(YEAR, group = latino_origin) %>%
      summarise(
        total_latino_households = sum(weight),
        latino_farm_households = sum(ifelse(has_farm == TRUE, weight, 0)),
        .groups = "drop"
      ) %>%
      mutate(
        pct_farm_households = 100 * latino_farm_households / total_latino_households
      ) %>%
      select(YEAR, group, pct_farm_households) %>%
      pivot_wider(names_from = group, values_from = pct_farm_households)
  },
  weight_type = "household"
)

write_csv(farm_status, "output/farm_status.csv")


# 9. Homeownership among Latino Households (Household-level analysis)
homeownership_origin <- calculate_with_se(
  data = household_summarization_data %>%
    mutate(
      # Based on codebook: 1=Owned or being bought (loan), 2=Rented
      owns_home = case_when(
        OWNERSHP == 1 ~ TRUE, # Owned or being bought (loan)
        OWNERSHP == 2 ~ FALSE, # Rented
        .default = NA
      )
    ) %>%
    filter(!is.na(owns_home)),
  computation_fn = function(data) {
    data %>%
      group_by(YEAR, group = latino_origin) %>%
      summarise(
        total_latino_households = sum(weight),
        latino_homeowners = sum(ifelse(owns_home == TRUE, weight, 0)),
        .groups = "drop"
      ) %>%
      mutate(
        pct_homeowners = 100 * latino_homeowners / total_latino_households
      ) %>%
      select(YEAR, group, pct_homeowners) %>%
      pivot_wider(names_from = group, values_from = pct_homeowners)
  },
  weight_type = "household"
)

write_csv(homeownership_origin, "output/homeownership_origin.csv")


homeownership_region <- calculate_with_se(
  data = household_summarization_data %>%
    mutate(
      # Based on codebook: 1=Owned or being bought (loan), 2=Rented
      owns_home = case_when(
        OWNERSHP == 1 ~ TRUE, # Owned or being bought (loan)
        OWNERSHP == 2 ~ FALSE, # Rented
        .default = NA
      )
    ) %>%
    filter(!is.na(owns_home), !is.na(region_name)),
  computation_fn = function(data) {
    data %>%
      group_by(YEAR, group = region_name) %>%
      summarise(
        total_latino_households = sum(weight),
        latino_homeowners = sum(ifelse(owns_home == TRUE, weight, 0)),
        .groups = "drop"
      ) %>%
      mutate(
        pct_homeowners = 100 * latino_homeowners / total_latino_households
      ) %>%
      select(YEAR, group, pct_homeowners) %>%
      pivot_wider(names_from = group, values_from = pct_homeowners)
  },
  weight_type = "household"
)

write_csv(homeownership_region, "output/homeownership_region.csv")


# 10. Median Home Value among Latino Homeowners (Household-level analysis)
home_values_origin <- calculate_with_se(
  data = household_summarization_data %>%
    filter(!is.na(VALUEH) & VALUEH > 0 & VALUEH < 9999998),
  computation_fn = function(data) {
    data %>%
      filter(!is.na(weight)) %>%
      group_by(YEAR, group = latino_origin) %>%
      summarise(
        median_home_value = weighted.median(VALUEH, ifelse(is.na(weight) | weight < 0, 0, weight)),
        .groups = "drop"
      ) %>%
      select(YEAR, group, median_home_value) %>%
      pivot_wider(names_from = group, values_from = median_home_value)
  },
  weight_type = "household"
)

write_csv(home_values_origin, "output/home_values_origin.csv")

home_values_region <- calculate_with_se(
  data = household_summarization_data %>%
    filter(!is.na(VALUEH) & VALUEH > 0 & VALUEH < 9999998 & !is.na(region_name)),
  computation_fn = function(data) {
    data %>%
      filter(!is.na(weight)) %>%
      group_by(YEAR, group = region_name) %>%
      summarise(
        median_home_value = weighted.median(VALUEH, ifelse(is.na(weight) | weight < 0, 0, weight)),
        .groups = "drop"
      ) %>%
      select(YEAR, group, median_home_value) %>%
      pivot_wider(names_from = group, values_from = median_home_value)
  },
  weight_type = "household"
)

write_csv(home_values_region, "output/home_values_region.csv")

# 11. Median Household Income among Latino Households (Household-level analysis)
household_income_origin <- calculate_with_se(
  data = household_summarization_data %>%
    filter(!is.na(HHINCOME) & HHINCOME < 9999999),
  computation_fn = function(data) {
    data %>%
      filter(!is.na(weight)) %>%
      group_by(YEAR, group = latino_origin) %>%
      summarise(
        median_hh_income = weighted.median(HHINCOME, ifelse(is.na(weight) | weight < 0, 0, weight)),
        .groups = "drop"
      ) %>%
      select(YEAR, group, median_hh_income) %>%
      pivot_wider(names_from = group, values_from = median_hh_income)
  },
  weight_type = "household"
)

write_csv(household_income_origin, "output/household_income_origin.csv")

household_income_region <- calculate_with_se(
  data = household_summarization_data %>%
    filter(!is.na(HHINCOME) & HHINCOME < 9999999 & !is.na(region_name)),
  computation_fn = function(data) {
    data %>%
      filter(!is.na(weight)) %>%
      group_by(YEAR, group = region_name) %>%
      summarise(
        median_hh_income = weighted.median(HHINCOME, ifelse(is.na(weight) | weight < 0, 0, weight)),
        .groups = "drop"
      ) %>%
      select(YEAR, group, median_hh_income) %>%
      pivot_wider(names_from = group, values_from = median_hh_income)
  },
  weight_type = "household"
)

write_csv(household_income_region, "output/household_income_region.csv")


# 12. Real Estate Value (1850 only) (Person-level analysis)
real_estate_1850 <- household_summarization_data %>%
  filter(!is.na(REALPROP) & REALPROP > 0 & REALPROP < 999998) %>%
  group_by(YEAR, group = latino_origin) %>%
  summarise(
    median_real_estate = weighted.median(REALPROP, HHWT),
    .groups = "drop"
  ) %>%
  select(YEAR, group, median_real_estate) %>%
  pivot_wider(names_from = group, values_from = median_real_estate)

write_csv(real_estate_1850, "output/real_estate.csv")
