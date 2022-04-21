library(tidyverse)
library(tidycensus)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

# Load data
census_api_key("509520e4a2870f2d97c9a9825581a7a923efbb9e", install = TRUE)
 
# properties <- read.csv("data/property_assessments_2021.csv")
# addresses <- read.csv("data/sam_addresses.csv", stringsAsFactors = FALSE)
# 
# 
# geo_props <- left_join(properties, addresses, by = c("PID" = "PARCEL"))
# 
# by_yr_const <- properties %>% group_by(YR_BUILT)
# 
# summarise(properties)

race_vars_acs = c(
  White = "DP05_0077",
  Black = "DP05_0078",
  Native = "DP05_0079",
  Asian = "DP05_0080",
  HIPI = "DP05_0081",
  Hispanic = "DP05_0071",
  MixedNonHispanic = "DP05_0083",
  Other = "DP05_0082"
)

housing_vars_acs = c(
  TotalOccupied = "DP04_0044",
  OwnerOccupied = "DP04_0045",
  RenterOccupied = "DP04_0046",
  PercentOwner = "DP04_0045P",
  PercentRenter = "DP04_0046P"
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

# Convert back into long format
longer_entropy <- pivoted_entropy %>%
  pivot_longer(!NAME, names_to = "year", values_to = "Entropy")

# Get changes in entropy index from previous year
entropy_changes <- longer_entropy %>%
  group_by(NAME) %>%
  mutate(EntropyChange = (Entropy - lag(Entropy)))

# Compare 2010 to 2019 for longer term effects
entropy_changes_longer <- longer_entropy %>%
  filter(year == "y2010" | year == "y2019") %>%
  group_by(NAME) %>%
  mutate(EntropyChange = (Entropy - lag(Entropy)))

# Get estimated percent of occupied units occupied by renters for each census tract in Suffolk County
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

# Convert back into long format
longer_rental <- pivoted_rental %>% 
  pivot_longer(!NAME, names_to = "year", values_to = "PercentRenterOccupied")

# Get changes in rental percentages from previous year
rental_changes <- longer_rental %>%
  group_by(NAME) %>%
  mutate(RentalChange = (PercentRenterOccupied - lag(PercentRenterOccupied)))

# Compare 2010 to 2019 for longer term effects
rental_changes_longer <- longer_rental %>%
  filter(year == "y2010" | year == "y2019") %>%
  group_by(NAME) %>%
  mutate(RentalChange = (PercentRenterOccupied - lag(PercentRenterOccupied)))

# Join both tables
joined <- left_join(entropy_changes, rental_changes, by = c("NAME", "year"))
joined_longer <- left_join(entropy_changes_longer, rental_changes_longer, by = c("NAME", "year"))


# Exploratory visualizations
scatterplot <- ggplot(joined, aes(x = RentalChange, y = EntropyChange)) +
  geom_point() +
  xlim(-15, 15) +
  ylim(-0.26, 0.25)

scatterplot2 <- ggplot(joined_longer, aes(x = RentalChange, y = EntropyChange)) +
  geom_point() +
  xlim(-15, 15) +
  ylim(-0.26, 0.25) 
