library(tidyverse)
library(tidycensus)
library(dplyr)
library(purrr)
library(tidyr)

# Load data
# properties <- read.csv("data/property_assessments_2021.csv")
# addresses <- read.csv("data/sam_addresses.csv", stringsAsFactors = FALSE) %>% mutate(PARCEL = as.numeric(PARCEL))
# dp04 <- read.csv("data/dp042020.csv")
# 
# 
# geo_props <- left_join(properties, addresses, by = c("PID" = "PARCEL"))
# 
# by_yr_const <- properties %>% group_by(YR_BUILT)
# 
# summarise(properties)
# 
# census_api_key("509520e4a2870f2d97c9a9825581a7a923efbb9e", install = TRUE)
# 
# race_vars2000 = c(
#   White = "P003003",
#   Black = "P003004",
#   Native = "P003005",
#   Asian = "P003006",
#   HIPI = "P003007",
#   Hispanic = "P004002",
#   Mixed = "P003009",
#   Other = "P003008"
# )
# 
# race_vars2010 = c(
#   White = "P003002",
#   Black = "P003003",
#   Native = "P003004",
#   Asian = "P003005",
#   HIPI = "P003006",
#   Hispanic = "P004003",
#   Mixed = "P003008",
#   Other = "P003007"
# )

race_vars_acs = c(
  White = "B02001_002",
  Black = "B02001_003",
  Native = "B02001_004",
  Asian = "B02001_005",
  HIPI = "B02001_006",
  Hispanic = "B03002_012",
  Mixed = "B02001_008",
  Other = "B02001_007"
)

housing_vars_acs = c(
  TotalOccupied = "DP04_0044",
  OwnerOccupied = "DP04_0045",
  RenterOccupied = "DP04_0046",
  PercentOwner = "DP04_0045P",
  PercentRenter = "DP04_0046P"
)

cHousing <- get_decennial(
  geography = "block",
  state = "MA",
  county = "Suffolk",
  year = 2000,
  variables = ""
)

c2000 <- get_decennial(
  geography = "block",
  state = "MA",
  county = "Suffolk",
  year = 2000,
  variables = race_vars2000
)

c2010 <- get_decennial(
  geography = "block",
  state = "MA",
  county = "Suffolk",
  year = 2010,
  variables = race_vars2010
)

c2020 <- get_decennial(
  geography = "block",
  state = "MA",
  county = "Suffolk",
  year = 2020,
  variables = race_vars
)

acs2010 <- get_acs(
  geography = "tract",
  state = "MA",
  county = "Suffolk",
  year = 2010,
  variables = housing_vars_acs
)

pr2010 <- get_acs(
  geography = "tract",
  state = "MA",
  county = "Suffolk",
  year = 2010,
  variables = c(PercentRenterOccupied = "DP04_0046P")
)

race2010 <- get_acs(
  geography = "tract",
  state = "MA",
  county = "Suffolk",
  year = 2010,
  variables = race_vars_acs
)

years <- 2010:2019
names(years) <- years


race_by_year <- map_dfr(years, ~{
  get_acs(
    geography = "tract",
    variables = race_vars_acs,
    state = "MA",
    county = "Suffolk",
    summary_var = "B02001_001",
    survey = "acs5",
    year = .x
  )
}, .id = "year")

pivoted_race <- race_by_year %>%
  group_by(NAME, year) %>%
  pivot_wider(id_cols = NAME,
              names_from = year,
              values_from = estimate)
