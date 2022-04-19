library(tidyverse)
library(tidycensus)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

# Load data
census_api_key("509520e4a2870f2d97c9a9825581a7a923efbb9e", install = TRUE)

properties <- read.csv("data/property_assessments_2021.csv")
addresses <- read.csv("data/sam_addresses.csv", stringsAsFactors = FALSE)
dp04 <- read.csv("data/dp042020.csv")


geo_props <- left_join(properties, addresses, by = c("PID" = "PARCEL"))

by_yr_const <- properties %>% group_by(YR_BUILT)

summarise(properties)

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
  White = "B03002_003",
  Black = "B03002_004",
  Native = "B03002_005",
  Asian = "B03002_006",
  HIPI = "B03002_007",
  Hispanic = "B03002_012",
  Mixed = "B03002_009",
  Other = "B03002_008"
)

housing_vars_acs = c(
  TotalOccupied = "DP04_0044",
  OwnerOccupied = "DP04_0045",
  RenterOccupied = "DP04_0046",
  PercentOwner = "DP04_0045P",
  PercentRenter = "DP04_0046P"
)

acs2010 <- get_acs(
  geography = "tract",
  state = "MA",
  county = "Suffolk",
  year = 2010,
  variables = housing_vars_acs
)

acs2015 <- get_acs(
  geography = "tract",
  state = "MA",
  county = "Suffolk",
  year = 2015,
  variables = housing_vars_acs
)



race2010 <- get_acs(
  geography = "tract",
  state = "MA",
  county = "Suffolk",
  year = 2010,
  variables = race_vars_acs
)

# Function to calculate entropy index of segregation
# https://www.dartmouth.edu/~segregation/IndicesofSegregation.pdf
entropy_index <- function(race_counts) {
  total_pop <- sum(race_counts)
  entropy <- 0
  for(i in length(race_counts)) {
    prop <- race_counts[i]/total_pop
    entropy <- entropy -(prop) * log(prop)
  }
  return(entropy)
}

# Define range of years to pull data
years <- 2010:2019
names(years) <- years

# Code structure taken from link below
# https://walker-data.com/census-r/wrangling-census-data-with-tidyverse-tools.html#preparing-time-series-acs-estimates
# Get racial estimates for each census tract in Suffolk County
race_by_year <- map_dfr(years, ~{
  get_acs(
    geography = "tract",
    variables = race_vars_acs,
    state = "MA",
    county = "Suffolk",
    summary_var = "B03002_001",
    survey = "acs5",
    year = .x
  )
}, .id = "year")

# Calculate time series of race counts
pivoted_race <- race_by_year %>%
  group_by(NAME, year) %>%
  pivot_wider(id_cols = NAME,
              names_from = year,
              names_prefix = "y",
              values_from = estimate)

# Calculate time series of entropy index 
pivoted_entropy <- race_by_year %>%
  group_by(NAME, year) %>%
  pivot_wider(id_cols = NAME,
              names_from = year,
              names_prefix = "y",
              values_from = estimate,
              values_fn = entropy_index)

# Iterate over every year for every tract
percent_rental_by_year <- map_dfr(years, ~{
  get_acs(
    geography = "tract",
    variables = "DP04_0046P",
    state = "MA",
    county = "Suffolk",
    survey = "acs5",
    year = .x
  )
}, .id = "year")

# Get estimated percent of occupied units occupied by renters
pivoted_rental <- percent_rental_by_year %>%
  group_by(NAME, year) %>%
  pivot_wider(id_cols = NAME,
              names_from = year,
              names_prefix = "y",
              values_from = estimate) %>%
  # Recalculate percentages from 2015-2019 to account for shift
  # in question codes
  mutate_at(vars(y2015, y2016, y2017, y2018, y2019), 
            function(x) {return(100 - x)})

longer_rental <- pivoted_rental %>% 
  pivot_longer(!NAME, names_to = "year", values_to = "PercentRenterOccupied")

stacked_series <- ggplot(longer_rental, aes(x = year, y = PercentRenterOccupied, group = NAME)) + 
  geom_point() +
  geom_line()

filtered <- filter(longer_rental, NAME == "Census Tract 1, Suffolk County, Massachusetts")
time_series <- ggplot(filtered, aes(x = year, y = PercentRenterOccupied)) +
  geom_point() +
  geom_line(group = 1) +
  ylim(0, 100) +
  xlab("year") +
  ylab("Percent renter owned")
