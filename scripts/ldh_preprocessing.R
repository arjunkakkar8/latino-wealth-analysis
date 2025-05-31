setwd("/Users/Jie Zong/Box/Latino Policy and Politics Institute/Research/Current Projects/latino data hub/Data Processing")

library(htmltools)
library(shiny)
library(DT)
library(ipumsr)
library(tidyverse)
library(tictoc)
library(multidplyr)
library(data.table)
source("append_county_codes.R")

# Use for styling file based on tidyverse style guide
# Keeps the file manageable and readable
library(styler)
styler:::style_active_file()

# Read IPUMS data files
ddi <- read_ipums_ddi("data/usa_00171.xml")
raw <- read_ipums_micro(ddi)

# Show variable descriptions and levels in an interactive
# document
ipums_view(ddi)

# Create a cluster of processes, parallel::detectCores() - 2
# is usually a good upper bound
cluster <- new_cluster(parallel::detectCores())
cluster_library(cluster, c("dplyr", "ipumsr"))

# Create the processed data file including only the final
# output rows
tic()
processed <- raw %>%
  # filter(STATEFIP == 6) %>%
  # sample_n(1000000) %>%
  # slice(1:10000) %>%
  append_county_codes() %>%
  partition(cluster) %>%
  mutate(
    .before = 1,
    gq = case_when(
      GQ %in% c(1, 2, 5) ~ 0,
      TRUE ~ 1
    ),
    age_group = case_when(
      AGE <= 17 ~ 1,
      AGE <= 64 ~ 2,
      TRUE ~ 3
    ),
    married = ifelse(
      AGE >= 15,
      case_when(
        MARST == 1 |
          MARST == 2 ~ 2,
        MARST == 6 ~ 1,
        TRUE ~ 3
      ),
      NA
    ),
    household_size = ifelse(PERNUM == 1 & gq == 0,
      NUMPREC,
      NA
    ),
    race_recoded = case_when(
      HISPAN != 0 ~ 1,
      RACE == 1 ~ 2,
      (RACE == 4 | RACE == 5 | RACE == 6) ~ 3,
      RACE == 2 ~ 4,
      RACE == 3 ~ 5,
      RACE == 7 ~ 6,
      (RACE == 8 | RACE == 9) ~ 7
    ),
    latino_race = ifelse(
      HISPAN > 0,
      case_when(
        RACBLK == 2 ~ 1,
        RACE == 3 & RACBLK == 1 ~ 2,
        RACE >= 4 & RACE <= 6 & RACBLK == 1 ~ 3,
        RACE == 1 & RACBLK == 1 ~ 4,
        RACE == 7 & RACBLK == 1 ~ 5,
        RACE >= 8 & RACBLK == 1 ~ 6
      ),
      NA
    ),
    latino_origin = case_when(
      HISPAND > 0 & HISPAND < 200 ~ 1,
      HISPAND == 200 ~ 2,
      HISPAND == 300 ~ 3,
      HISPAND == 460 ~ 4,
      HISPAND >= 401 & HISPAND <= 417 ~ 5,
      HISPAND >= 420 & HISPAND <= 431 ~ 6,
      HISPAND == 450 | HISPAND == 498 ~ 7
    ),
    latino_origin_detailed = case_when(
      HISPAND > 0 & HISPAND < 200 ~ 1,
      HISPAND == 200 ~ 2,
      HISPAND == 300 ~ 3,
      HISPAND == 460 ~ 4,
      HISPAND == 411 ~ 5,
      HISPAND == 412 ~ 6,
      HISPAND == 413 ~ 7,
      HISPAND == 414 ~ 8,
      HISPAND == 415 ~ 9,
      HISPAND == 416 ~ 10,
      HISPAND == 417 ~ 11,
      HISPAND == 420 ~ 12,
      HISPAND == 421 ~ 13,
      HISPAND == 422 ~ 14,
      HISPAND == 423 ~ 15,
      HISPAND == 424 ~ 16,
      HISPAND == 425 ~ 17,
      HISPAND == 426 ~ 18,
      HISPAND == 427 ~ 19,
      HISPAND == 428 ~ 20,
      HISPAND == 431 ~ 21,
      HISPAND == 450 | HISPAND == 498 ~ 22
    ),
    veteran = ifelse(
      AGE >= 17,
      case_when(
        VETSTAT == 2 ~ 1,
        VETSTAT == 1 ~ 0
      ),
      NA
    ),
    citizenship = case_when(
      CITIZEN == 0 | CITIZEN == 1 ~ 1,
      CITIZEN == 2 ~ 2,
      CITIZEN == 3 ~ 3
    ),
    yrsusa2 = ifelse(
      (CITIZEN == 2 | CITIZEN == 3) &
        (YRSUSA2 != 0 | YRSUSA2 != 9),
      YRSUSA2,
      NA
    ),
    yrsusa1 = ifelse(
      (CITIZEN == 2 | CITIZEN == 3),
      YRSUSA1,
      NA
    ),
    family_structure = ifelse(
      AGE < 18,
      case_when(
        (MOMLOC != 0 & POPLOC != 0) | (MOMLOC != 0 & MOMLOC2 != 0) | (POPLOC != 0 & POPLOC2 != 0) ~ 1,
        (MOMLOC != 0 & POPLOC == 0 & MOMLOC2 == 0) | (POPLOC != 0 & MOMLOC == 0 & POPLOC2 == 0) ~ 2,
        MOMLOC == 0 & POPLOC == 0 ~ 3
      ),
      NA
    ),
    three_gen = ifelse(
      AGE < 18,
      case_when(
        MULTGEN == 3 ~ 1,
        MULTGEN == 1 | MULTGEN == 2 ~ 0
      ),
      NA
    ),
    live_grandkid = ifelse(
      AGE >= 30 &
        GQTYPE != 1,
      case_when(
        GCHOUSE == 2 ~ 1,
        GCHOUSE == 1 ~ 0
      ),
      NA
    ),
    responsible_grandkid = ifelse(
      AGE >= 30 &
        GQTYPE != 1 &
        GCHOUSE == 2,
      case_when(
        GCRESPON == 2 ~ 1,
        GCRESPON == 1 ~ 0
      ),
      NA
    ),
    kids_gq = ifelse(
      AGE < 18,
      case_when(
        gq == 1 ~ 1,
        TRUE ~ 0
      ),
      NA
    ),
    preschool = ifelse(
      AGE >= 3 & AGE <= 4,
      case_when(
        SCHOOL == 1 ~ 3,
        SCHLTYPE == 2 ~ 1,
        SCHLTYPE == 3 ~ 2
      ),
      NA
    ),
    not_enrolled_es = ifelse(
      AGE >= 5 & AGE <= 12,
      case_when(
        SCHOOL == 1 ~ 1,
        SCHOOL == 2 ~ 0
      ),
      NA
    ),
    not_enrolled_hs = ifelse(
      AGE >= 13 & AGE <= 17,
      case_when(
        SCHOOL == 1 ~ 1,
        SCHOOL == 2 ~ 0
      ),
      NA
    ),
    disconnect_youth = ifelse(
      AGE >= 16 & AGE <= 24,
      case_when(
        SCHOOL == 1 & (EMPSTAT == 2 | EMPSTAT == 3) ~ 1,
        TRUE ~ 0
      ),
      NA
    ),
    eduatt = ifelse(
      AGE >= 25,
      case_when(
        EDUCD > 1 & EDUCD <= 61 ~ 1,
        EDUCD == 63 | EDUCD == 64 ~ 2,
        EDUCD >= 65 & EDUCD <= 81 ~ 3,
        EDUCD == 101 ~ 4,
        EDUCD >= 114 & EDUCD <= 116 ~ 5
      ),
      NA
    ),
    eduatt_condensed = ifelse(
      AGE >= 25,
      case_when(
        EDUCD > 1 & EDUCD <= 61 ~ 1,
        EDUCD == 63 | EDUCD == 64 ~ 2,
        EDUCD >= 65 & EDUCD <= 81 ~ 3,
        EDUCD == 101 ~ 3,
        EDUCD >= 114 & EDUCD <= 116 ~ 3
      ),
      NA
    ),
    lep = ifelse(
      AGE >= 5,
      case_when(
        SPEAKENG == 3 ~ 1,
        SPEAKENG == 4 ~ 2,
        SPEAKENG %in% c(1, 5, 6) ~ 3
      ),
      NA
    ),
    spanish = ifelse(
      AGE >= 5,
      case_when(
        LANGUAGE == 12 ~ 1,
        TRUE ~ 0
      ),
      NA
    ),
    spanish_lep = ifelse(
      AGE >= 5 &
        LANGUAGE == 12,
      case_when(
        SPEAKENG == 4 ~ 1,
        SPEAKENG %in% c(1, 5, 6) ~ 2
      ),
      NA
    ),
    otherlang = ifelse(
      AGE >= 5,
      case_when(
        LANGUAGE != 12 & LANGUAGE != 1 ~ 1,
        TRUE ~ 0
      ),
      NA
    ),
    otherlang_lep = ifelse(
      AGE >= 5 &
        LANGUAGE != 12 & LANGUAGE != 1,
      case_when(
        SPEAKENG == 4 ~ 1,
        SPEAKENG %in% c(1, 5, 6) ~ 2
      ),
      NA
    ),
    lingisol = ifelse(
      gq == 0,
      case_when(
        LINGISOL == 2 ~ 1,
        LINGISOL == 1 ~ 0
      ),
      NA
    ),
    labforce = ifelse(
      AGE >= 16 &
        EMPSTATD != 14 &
        EMPSTATD != 15,
      case_when(
        EMPSTATD %in% c(10, 12, 20) ~ 1,
        EMPSTATD == 30 ~ 0
      ),
      NA
    ),
    employ = ifelse(
      AGE >= 16 &
        EMPSTATD %in% c(10, 12, 20),
      case_when(
        EMPSTATD %in% c(10, 12) ~ 1,
        EMPSTATD == 20 ~ 2
      ),
      NA
    ),
    employ_filter = ifelse(
      AGE >= 16 &
        EMPSTATD %in% c(10, 12, 20, 30),
      case_when(
        EMPSTATD %in% c(10, 12) ~ 1,
        EMPSTATD == 20 ~ 2,
        EMPSTATD == 30 ~ 3
      ),
      NA
    ),
    selfemp = ifelse(
      AGE >= 16 &
        EMPSTATD %in% c(10, 12),
      case_when(
        CLASSWKR == 1 ~ 1,
        CLASSWKR == 2 ~ 0
      ),
      NA
    ),
    occ_group = ifelse(
      AGE >= 16 &
        EMPSTATD %in% c(10, 12),
      case_when(
        OCC >= 10 & OCC <= 960 ~ 1,
        OCC <= 1980 ~ 2,
        OCC <= 2920 ~ 3,
        OCC <= 3550 ~ 4,
        OCC <= 4655 ~ 5,
        OCC <= 4965 ~ 6,
        OCC <= 5940 ~ 7,
        OCC <= 6130 ~ 8,
        OCC <= 6950 ~ 9,
        OCC <= 7640 ~ 10,
        OCC <= 8990 ~ 11,
        OCC <= 9760 ~ 12
      ),
      NA
    ),
    occ_group_condensed = ifelse(
      AGE >= 16 &
        EMPSTATD %in% c(10, 12),
      case_when(
        OCC >= 10 & OCC <= 3550 ~ 1,
        OCC >= 3601 & OCC <= 4655 ~ 2,
        OCC >= 4700 & OCC <= 5920 ~ 3,
        OCC >= 6005 & OCC <= 7640 ~ 4,
        OCC >= 7700 & OCC <= 9760 ~ 5
      ),
      NA
    ),
    ind_group = ifelse(
      AGE >= 16 &
        EMPSTATD %in% c(10, 12),
      case_when(
        IND >= 170 & IND <= 490 ~ 1,
        IND <= 690 ~ 6,
        IND == 770 ~ 2,
        IND <= 3990 ~ 3,
        IND <= 4590 ~ 4,
        IND <= 5790 ~ 5,
        IND <= 6390 ~ 6,
        IND <= 6780 ~ 7,
        IND <= 7190 ~ 8,
        IND <= 7790 ~ 9,
        IND <= 8470 ~ 10,
        IND <= 8690 ~ 11,
        IND <= 9290 ~ 12,
        IND <= 9590 ~ 13
      ),
      NA
    ),
    wage = ifelse(
      AGE >= 16 &
        INCWAGE > 0 & INCWAGE < 999998 &
        WKSWORK1 > 0 & UHRSWORK > 0,
      round(INCWAGE / (WKSWORK1 * UHRSWORK)),
      NA
    ),
    wage_15 = ifelse(
      AGE >= 16 &
        INCWAGE > 0 & INCWAGE < 999998 &
        WKSWORK1 > 0 & UHRSWORK > 0,
      case_when(
        wage >= 15 ~ 1,
        TRUE ~ 0
      ),
      NA
    ),
    hhincome = ifelse(
      PERNUM == 1 &
        gq == 0 &
        HHINCOME < 9999999,
      HHINCOME,
      NA
    ),
    poverty = ifelse(
      gq == 0 &
        POVERTY > 0,
      case_when(
        POVERTY < 100 ~ 1,
        POVERTY >= 100 & POVERTY < 200 ~ 2,
        TRUE ~ 3
      ),
      NA
    ),
    poverty_child = ifelse(
      gq == 0 &
        POVERTY > 0 &
        AGE < 18,
      case_when(
        POVERTY < 100 ~ 1,
        POVERTY >= 100 & POVERTY < 200 ~ 2,
        TRUE ~ 3
      ),
      NA
    ),
    poverty_employed = ifelse(
      gq == 0 &
        POVERTY > 0 &
        AGE >= 16 &
        (EMPSTATD == 10 | EMPSTATD == 12),
      case_when(
        POVERTY < 100 ~ 1,
        TRUE ~ 0
      ),
      NA
    ),
    free_lunch = ifelse(
      gq == 0 &
        POVERTY > 0 &
        AGE < 18,
      case_when(
        POVERTY <= 185 ~ 1,
        TRUE ~ 0
      ),
      NA
    ),
    foodstamp = ifelse(
      gq == 0,
      case_when(
        FOODSTMP == 2 ~ 1,
        FOODSTMP == 1 ~ 0
      ),
      NA
    ),
    foodstamp_child = ifelse(
      gq == 0 &
        AGE < 18,
      case_when(
        FOODSTMP == 2 ~ 1,
        FOODSTMP == 1 ~ 0
      ),
      NA
    ),
    cash = ifelse(
      AGE >= 15 &
        INCWELFR < 99999,
      case_when(
        INCWELFR > 0 ~ 1,
        INCWELFR == 0 ~ 0
      ),
      NA
    ),
    ownership = ifelse(
      gq == 0,
      case_when(
        OWNERSHP == 1 ~ 1,
        OWNERSHP == 2 ~ 0
      ),
      NA
    ),
    ownership_child = ifelse(
      gq == 0 &
        AGE < 18,
      case_when(
        OWNERSHP == 1 ~ 1,
        OWNERSHP == 2 ~ 0
      ),
      NA
    ),
    hvalue = ifelse(
      PERNUM == 1 &
        OWNERSHP == 1 &
        gq == 0 &
        VALUEH < 9999998,
      VALUEH,
      NA
    ),
    owner_hcost = ifelse(
      OWNERSHP == 1 &
        gq == 0 &
        OWNCOST < 99999 &
        HHINCOME < 9999999,
      case_when(
        OWNCOST / (HHINCOME / 12) >= .3 & OWNCOST / (HHINCOME / 12) < .5 ~ 1,
        OWNCOST / (HHINCOME / 12) >= .5 ~ 2,
        TRUE ~ 3
      ),
      NA
    ),
    owner_hcost_child = ifelse(
      OWNERSHP == 1 &
        gq == 0 &
        OWNCOST < 99999 &
        HHINCOME < 9999999 &
        AGE < 18,
      case_when(
        OWNCOST / (HHINCOME / 12) >= .3 & OWNCOST / (HHINCOME / 12) < .5 ~ 1,
        OWNCOST / (HHINCOME / 12) >= .5 ~ 2,
        TRUE ~ 3
      ),
      NA
    ),
    renter_hcost = ifelse(
      OWNERSHP == 2 &
        gq == 0 &
        RENTGRS > 0 &
        HHINCOME < 9999999,
      case_when(
        RENTGRS / (HHINCOME / 12) >= .3 & RENTGRS / (HHINCOME / 12) < .5 ~ 1,
        RENTGRS / (HHINCOME / 12) >= .5 ~ 2,
        TRUE ~ 3
      ),
      NA
    ),
    renter_hcost_child = ifelse(
      OWNERSHP == 2 &
        gq == 0 &
        RENTGRS > 0 &
        HHINCOME < 9999999 &
        AGE < 18,
      case_when(
        RENTGRS / (HHINCOME / 12) >= .3 & RENTGRS / (HHINCOME / 12) < .5 ~ 1,
        RENTGRS / (HHINCOME / 12) >= .5 ~ 2,
        TRUE ~ 3
      ),
      NA
    ),
    overcrowd = ifelse(
      gq == 0,
      case_when(
        NUMPREC / ROOMS > 1 ~ 1,
        TRUE ~ 0
      ),
      NA
    ),
    overcrowd_child = ifelse(
      gq == 0 &
        AGE < 18,
      case_when(
        NUMPREC / ROOMS > 1 ~ 1,
        TRUE ~ 0
      ),
      NA
    ),
    internet = ifelse(
      gq == 0,
      case_when(
        CINETHH == 3 ~ 1,
        CINETHH == 1 | CINETHH == 2 ~ 0
      ),
      NA
    ),
    broadband = ifelse(
      gq == 0,
      case_when(
        CIHISPEED == 20 ~ 1,
        CIHISPEED == 10 ~ 0
      ),
      NA
    ),
    laptop = ifelse(
      gq == 0,
      case_when(
        CILAPTOP == 2 ~ 1,
        CILAPTOP == 1 ~ 0
      ),
      NA
    ),
    smartphone = ifelse(
      gq == 0,
      case_when(
        CISMRTPHN == 2 ~ 1,
        CISMRTPHN == 1 ~ 0
      ),
      NA
    ),
    tablet = ifelse(
      gq == 0,
      case_when(
        CITABLET == 2 ~ 1,
        CITABLET == 1 ~ 0
      ),
      NA
    ),
    device = ifelse(
      gq == 0,
      case_when(
        CILAPTOP == 2 & CISMRTPHN == 2 & CITABLET == 2 ~ 1,
        TRUE ~ 0
      ),
      NA
    ),
    vehicle = ifelse(
      gq == 0,
      case_when(
        VEHICLES == 9 ~ 1,
        VEHICLES > 0 ~ 0
      ),
      NA
    ),
    wfh = ifelse(
      AGE >= 16 &
        gq == 0 &
        WRKLSTWK == 2,
      case_when(
        TRANWORK == 80 ~ 1,
        TRUE ~ 0
      ),
      NA
    ),
    commute_time = ifelse(
      AGE >= 16 &
        gq == 0 &
        WRKLSTWK == 2 &
        TRANWORK > 0 & TRANWORK < 80,
      TRANTIME,
      NA
    ),
    public_commute = ifelse(
      AGE >= 16 &
        gq == 0 &
        WRKLSTWK == 2 &
        TRANWORK > 0 & TRANWORK < 80,
      case_when(
        TRANWORK %in% c(31, 34, 36, 37, 39) ~ 1,
        TRUE ~ 0
      ),
      NA
    ),
    coverage_type = case_when(
      HCOVANY == 1 ~ 1,
      HINSCARE == 2 ~ 2,
      HINSCAID == 2 ~ 3,
      HINSVA == 2 ~ 4,
      HINSEMP == 2 ~ 5,
      HINSPUR == 2 ~ 6,
      HINSPUR == 1 ~ 7
    ),
    medicaid_child = ifelse(
      AGE < 18,
      case_when(
        HCOVANY == 2 & HINSCAID == 2 & HINSCARE == 1 ~ 1,
        TRUE ~ 0
      ),
      NA
    ),
    uninsured_child = ifelse(
      AGE < 18,
      case_when(
        HCOVANY == 1 ~ 1,
        TRUE ~ 0
      ),
      NA
    ),
    eligible_voters = case_when(
      CITIZEN >= 0 & CITIZEN <= 2 & AGE >= 18 ~ 1,
      CITIZEN > 2 | AGE < 18 ~ 0
    ),
    hhincome_filter = ifelse(
      gq == 0,
      case_when(
        HHINCOME < 50000 ~ 1,
        HHINCOME < 80000 ~ 2,
        HHINCOME < 9999999 ~ 3
      ),
      NA
    ),
    hhincome_range = ifelse(
      gq == 0 & HHINCOME < 9999999,
      HHINCOME,
      NA
    ),
    poverty_filter = ifelse(
      gq == 0 &
        POVERTY > 0,
      case_when(
        POVERTY <= 138 ~ 1,
        TRUE ~ 0
      ),
      NA
    )
  ) %>%
  select(1:67, YEAR, STATEFIP, SEX, AGE, HHWT, PERWT, county_id, hhincome_filter, poverty_filter, uninsured_child, eligible_voters, hhincome_range, PUMA) %>%
  collect() %>%
  # Convert categorical data to factors for easy summarization and tabulation
  mutate(across(
    !c(
      AGE,
      HHWT,
      PERWT,
      yrsusa1,
      hhincome,
      hhincome_range,
      hvalue,
      commute_time,
      wage
    ),
    as.factor
  )) %>%
  mutate(hhincome = as.integer(hhincome)) %>%
  mutate(hhincome_range = as.integer(hhincome_range))

toc()

# Save the processed data to csv for use by Forio
fwrite(processed, file = "data/processed_full.csv", row.names = FALSE, na = "", quote = FALSE)

fwrite(processed, file = "data/processed_sample.csv", row.names = FALSE, na = "", quote = FALSE)

# Take a look at the processed data summary to ensure
# computed variables look reasonable
processed %>% summary()

# The cross_tabulate function is a helper to identify
# missing criterion in the creation of the processed
# variables.
cross_tabulate <- function(df, in_var, cross_vars) {
  tables <- lapply(cross_vars, function(cross_var) {
    df %>%
      group_by(across(all_of(c(in_var, cross_var)))) %>%
      summarize(count = n()) %>%
      pivot_wider(names_from = 2, values_from = count)
  })

  names(tables) <- cross_vars
  return(tables)
}

# Example usage:
cross_tabulate(processed, "married", c("age_group", "gq"))
cross_tabulate(processed, "cash_child", c("age_group", "gq"))
