library(tidyverse)
library(spatstat)
library(rlang)
library(arrow)

mem.maxVSize(50000)

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
  main_estimate <- data %>%
    mutate(weight = !!sym(main_weight)) %>%
    computation_fn()

  # Calculate replicate estimates
  rep_estimates <- map_dfr(rep_weights, function(rep_wt) {
    if (rep_wt %in% names(data)) {
      data %>%
        mutate(weight = !!sym(rep_wt)) %>%
        computation_fn() %>%
        return()
    } else {
      return(main_estimate)
    }
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
fragment <- "all"
raw <- open_dataset("data/chunks")

# Tranform original csv.gz file into chunks
# raw <- read_csv_arrow("data/usa_00015.csv.gz")
# raw %>%
#   group_by(YEAR) %>%
#   write_dataset("data/chunks")

# examine data structure
# glimpse(raw)
# head(raw)
#
# # check available years
# table(raw$YEAR)
#
# # examine Hispanic/Latino variables
# table(raw$HISPAN, useNA = "always")
#
# # examine other key variables
# table(raw$CITIZEN, useNA = "always") # citizenship
# table(raw$REGION, useNA = "always") # region
# # occupation
# table(raw$CLASSWKR, useNA = "always") # class of worker
# table(raw$FARM, useNA = "always") # farm status
# table(raw$OWNERSHP, useNA = "always") # homeownership
#
# summary(raw$VALUEH) # home value
# summary(raw$HHINCOME) # household income
# summary(raw$REALPROP) # real estate property

# Define Latino population based on HISPAN and HISPRULE variables
data <- raw |>
  select(-SAMPLE, -SERIAL, -CBSERIAL, -CLUSTER, -STRATA, -GQ, -REALPROP_CPIU_2010) |>
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
      .default = NA_character_
    ),
    race = case_when(
      latino == TRUE ~ NA_character_,
      RACE == 1 ~ "White",
      RACE == 2 ~ "Black",
      .default = NA_character_
    ),
  ) %>%
  compute()

summarization_data <- rbind(
  data %>% filter(latino == TRUE) %>% collect(),
  data %>%
    filter(latino == TRUE) %>%
    mutate(latino_origin = "Total", region_name = "Total") %>%
    collect()
)

race_summarization_data <- data %>%
  filter(!is.na(race)) %>%
  collect()

household_summarization_data <- summarization_data %>%
  filter(PERNUM == 1)

################################################################################
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

write_csv(latino_pop_by_year, paste0("output/population_", fragment, ".csv"))


################################################################################
foreign_born <- function(data, groupVariable) {
  return(calculate_with_se(
    data %>%
      mutate(
        # Based on codebook: BPL codes 001-120 are US states/territories, >120 are foreign countries
        foreign_born = case_when(
          BPL >= 1 & BPL <= 120 ~ FALSE, # US states and territories
          BPL > 120 & BPL < 997 ~ TRUE, # Foreign countries
          .default = NA
        )
      ) %>%
      filter(!is.na(foreign_born)),
    function(data) {
      data %>%
        group_by(YEAR, group = !!sym(groupVariable)) %>%
        summarise(
          value = sum(weight),
          filtered_value = sum(ifelse(foreign_born == TRUE, weight, 0)),
          .groups = "drop"
        ) %>%
        mutate(
          # percent_filtered = 100 * filtered_value / value
          percent_filtered = filtered_value
        ) %>%
        select(-value, -filtered_value) %>%
        pivot_wider(names_from = group, values_from = percent_filtered)
    },
    weight_type = "person"
  ))
}

foreign_born_latinos_origin <- foreign_born(
  summarization_data,
  "latino_origin"
)

write_csv(foreign_born_latinos_origin, paste0("output/foreign_born_origin_", fragment, ".csv"))

foreign_born_latinos_region <- foreign_born(
  summarization_data %>%
    filter(!is.na(region_name)),
  "region_name"
)

write_csv(foreign_born_latinos_region, paste0("output/foreign_born_latinos_region_", fragment, ".csv"))

foreign_born_mexican_region <- foreign_born(
  summarization_data %>%
    filter(latino_origin == "Mexican" & !is.na(region_name)),
  "region_name"
)

write_csv(foreign_born_mexican_region, paste0("output/foreign_born_mexican_region_", fragment, ".csv"))

foreign_born_race <- foreign_born(
  race_summarization_data,
  "race"
)

write_csv(foreign_born_race, paste0("output/foreign_born_race_", fragment, ".csv"))



################################################################################
citizenship <- function(data, groupVariable) {
  return(calculate_with_se(
    data %>%
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
    function(data) {
      data %>%
        group_by(YEAR, group = !!sym(groupVariable)) %>%
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
  ))
}

citizenship_latinos_origin <- citizenship(
  summarization_data,
  "latino_origin"
)

write_csv(citizenship_latinos_origin, paste0("output/citizenship_origin_", fragment, ".csv"))

citizenship_latinos_region <- citizenship(
  summarization_data %>%
    filter(!is.na(region_name)),
  "region_name"
)

write_csv(citizenship_latinos_region, paste0("output/citizenship_region_", fragment, ".csv"))

citizenship_mexican_region <- citizenship(
  summarization_data %>%
    filter(latino_origin == "Mexican" & !is.na(region_name)),
  "region_name"
)

write_csv(citizenship_mexican_region, paste0("output/citizenship_mexican_region_", fragment, ".csv"))

citizenship_race <- citizenship(
  race_summarization_data,
  "race"
)

write_csv(citizenship_race, paste0("output/citizenship_race_", fragment, ".csv"))

################################################################################
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

write_csv(latino_by_region, paste0("output/population_by_region_", fragment, ".csv"))


################################################################################
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

write_csv(top_occupations, paste0("output/top_occupations_", fragment, ".csv"))


################################################################################
education <- function(data, groupVariable) {
  return(calculate_with_se(
    data %>%
      filter(EDUCD >= 2 & EDUCD <= 116 & AGE >= 25),
    function(data) {
      data %>%
        group_by(YEAR, group = !!sym(groupVariable)) %>%
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
  ))
}

education_origin <- education(
  summarization_data,
  "latino_origin"
)

write_csv(education_origin, paste0("output/education_origin_", fragment, ".csv"))

education_region <- education(
  summarization_data %>%
    filter(!is.na(region_name)),
  "region_name"
)

write_csv(education_region, paste0("output/education_region_", fragment, ".csv"))

education_mexican_region <- education(
  summarization_data %>%
    filter(latino_origin == "Mexican" & !is.na(region_name)),
  "region_name"
)

write_csv(education_mexican_region, paste0("output/education_mexican_region_", fragment, ".csv"))

education_race <- education(
  race_summarization_data,
  "race"
)

write_csv(education_race, paste0("output/education_race_", fragment, ".csv"))


################################################################################
business_ownership_func <- function(data, groupVariable) {
  return(calculate_with_se(
    data %>%
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
    function(data) {
      data %>%
        group_by(YEAR, group = !!sym(groupVariable)) %>%
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
  ))
}

business_ownership <- business_ownership_func(
  summarization_data,
  "latino_origin"
)

write_csv(business_ownership, paste0("output/business_ownership_", fragment, ".csv"))

business_ownership_latinos_region <- business_ownership_func(
  summarization_data %>%
    filter(!is.na(region_name)),
  "region_name"
)

write_csv(business_ownership_latinos_region, paste0("output/business_ownership_latinos_region_", fragment, ".csv"))

business_ownership_mexican_region <- business_ownership_func(
  summarization_data %>%
    filter(latino_origin == "Mexican" & !is.na(region_name)),
  "region_name"
)

write_csv(business_ownership_mexican_region, paste0("output/business_ownership_mexican_region_", fragment, ".csv"))

business_ownership_race <- business_ownership_func(
  race_summarization_data,
  "race"
)

write_csv(business_ownership_race, paste0("output/business_ownership_race_", fragment, ".csv"))


################################################################################
farm_status_func <- function(data, groupVariable) {
  return(calculate_with_se(
    data %>%
      mutate(
        # Based on codebook: 1=Non-Farm, 2=Farm
        has_farm = case_when(
          FARM == 1 ~ FALSE, # Non-farm
          FARM == 2 ~ TRUE, # Farm
          .default = NA
        )
      ) %>%
      filter(!is.na(has_farm)),
    function(data) {
      data %>%
        group_by(YEAR, group = !!sym(groupVariable)) %>%
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
  ))
}

farm_status <- farm_status_func(
  household_summarization_data,
  "latino_origin"
)

write_csv(farm_status, paste0("output/farm_status_", fragment, ".csv"))


################################################################################
homeownership_func <- function(data, groupVariable) {
  return(calculate_with_se(
    data %>%
      mutate(
        # Based on codebook: 1=Owned or being bought (loan), 2=Rented
        owns_home = case_when(
          OWNERSHP == 1 ~ TRUE, # Owned or being bought (loan)
          OWNERSHP == 2 ~ FALSE, # Rented
          .default = NA
        )
      ) %>%
      filter(!is.na(owns_home)),
    function(data) {
      data %>%
        group_by(YEAR, group = !!sym(groupVariable)) %>%
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
  ))
}

homeownership_origin <- homeownership_func(
  household_summarization_data,
  "latino_origin"
)

write_csv(homeownership_origin, paste0("output/homeownership_origin_", fragment, ".csv"))

homeownership_region <- homeownership_func(
  household_summarization_data %>%
    filter(!is.na(region_name)),
  "region_name"
)

write_csv(homeownership_region, paste0("output/homeownership_region_", fragment, ".csv"))

homeownership_mexican_region <- homeownership_func(
  household_summarization_data %>%
    filter(latino_origin == "Mexican" & !is.na(region_name)),
  "region_name"
)

write_csv(homeownership_mexican_region, paste0("output/homeownership_mexican_region_", fragment, ".csv"))

homeownership_race <- homeownership_func(
  race_summarization_data %>%
    filter(PERNUM == 1),
  "race"
)

write_csv(homeownership_race, paste0("output/homeownership_race_", fragment, ".csv"))


################################################################################
home_values_func <- function(data, groupVariable) {
  return(calculate_with_se(
    data %>%
      filter(!is.na(VALUEH) & VALUEH > 0 & VALUEH < 9999998),
    function(data) {
      data %>%
        filter(!is.na(weight)) %>%
        group_by(YEAR, group = !!sym(groupVariable)) %>%
        summarise(
          median_home_value = weighted.median(VALUEH, ifelse(is.na(weight) | weight < 0, 0, weight)),
          .groups = "drop"
        ) %>%
        select(YEAR, group, median_home_value) %>%
        pivot_wider(names_from = group, values_from = median_home_value)
    },
    weight_type = "household"
  ))
}

home_values_origin <- home_values_func(
  household_summarization_data,
  "latino_origin"
)

write_csv(home_values_origin, paste0("output/home_values_origin_", fragment, ".csv"))

home_values_region <- home_values_func(
  household_summarization_data %>%
    filter(!is.na(region_name)),
  "region_name"
)

write_csv(home_values_region, paste0("output/home_values_region_", fragment, ".csv"))

home_values_mexican_region <- home_values_func(
  household_summarization_data %>%
    filter(latino_origin == "Mexican" & !is.na(region_name)),
  "region_name"
)

write_csv(home_values_mexican_region, paste0("output/home_values_mexican_region_", fragment, ".csv"))

home_values_race <- home_values_func(
  race_summarization_data %>%
    filter(PERNUM == 1),
  "race"
)

write_csv(home_values_race, paste0("output/home_values_race_", fragment, ".csv"))


################################################################################
household_income_func <- function(data, groupVariable) {
  return(calculate_with_se(
    data %>%
      filter(!is.na(HHINCOME) & HHINCOME < 9999999),
    function(data) {
      data %>%
        filter(!is.na(weight)) %>%
        group_by(YEAR, group = !!sym(groupVariable)) %>%
        summarise(
          median_hh_income = weighted.median(HHINCOME, ifelse(is.na(weight) | weight < 0, 0, weight)),
          .groups = "drop"
        ) %>%
        select(YEAR, group, median_hh_income) %>%
        pivot_wider(names_from = group, values_from = median_hh_income)
    },
    weight_type = "household"
  ))
}

household_income_origin <- household_income_func(
  household_summarization_data,
  "latino_origin"
)

write_csv(household_income_origin, paste0("output/household_income_origin_", fragment, ".csv"))

household_income_region <- household_income_func(
  household_summarization_data %>%
    filter(!is.na(region_name)),
  "region_name"
)

write_csv(household_income_region, paste0("output/household_income_region_", fragment, ".csv"))

household_income_mexican_region <- household_income_func(
  household_summarization_data %>%
    filter(latino_origin == "Mexican" & !is.na(region_name)),
  "region_name"
)

write_csv(household_income_mexican_region, paste0("output/household_income_mexican_region_", fragment, ".csv"))

household_income_race <- household_income_func(
  race_summarization_data %>%
    filter(PERNUM == 1),
  "race"
)

write_csv(household_income_race, paste0("output/household_income_race_", fragment, ".csv"))


################################################################################
real_estate_1850 <- household_summarization_data %>%
  filter(!is.na(REALPROP) & REALPROP > 0 & REALPROP < 999998) %>%
  group_by(YEAR, group = latino_origin) %>%
  summarise(
    median_real_estate = weighted.median(REALPROP, HHWT),
    .groups = "drop"
  ) %>%
  select(YEAR, group, median_real_estate) %>%
  pivot_wider(names_from = group, values_from = median_real_estate)

write_csv(real_estate_1850, paste0("output/real_estate_", fragment, ".csv"))
