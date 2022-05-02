library(tidyverse)
library(tidycensus)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

# Load data
census_api_key("509520e4a2870f2d97c9a9825581a7a923efbb9e", install = TRUE)

# Question codes for ACS race counts
race_vars_acs = c(
  White = "DP05_0072",
  Black = "DP05_0073",
  Native = "DP05_0074",
  Asian = "DP05_0075",
  HIPI = "DP05_0076",
  Hispanic = "DP05_0070",
  MixedNonHispanic = "DP05_0078",
  Other = "DP05_0077"
)

# Question codes for ACS housing unit occupancy characteristics
housing_vars_acs = c(
  TotalOccupied = "DP04_0044",
  OwnerOccupied = "DP04_0045",
  RenterOccupied = "DP04_0046",
  PercentOwner = "DP04_0045P",
  PercentRenter = "DP04_0046P"
)

# Question codes for ACS income bins in order of below $10k,
# $10k-$15k, $15k-$25k, $25k-$35k, $35k-$50k, $50k-$75k, $75k-$100k,
# $100k-$150k, $150k-$200k, and more than $200k
income_vars_acs = c(
  bin1 = "DP03_0052",
  bin2 = "DP03_0053",
  bin3 = "DP03_0054",
  bin4 = "DP03_0055",
  bin5 = "DP03_0056",
  bin6 = "DP03_0057",
  bin7 = "DP03_0058",
  bin8 = "DP03_0059",
  bin9 = "DP03_0060",
  bin10 = "DP03_0061"
)

# Function to calculate entropy index of segregation
# https://www.dartmouth.edu/~segregation/IndicesofSegregation.pdf
# Note that max value of index is ln(k) where k = number of racial groups
entropy_index <- function(counts) {
  total_pop <- sum(counts)
  entropy <- 0
  for(i in length(counts)) {
    prop <- counts[i]/total_pop
    entropy <- entropy -(prop) * log(prop)
  }
  return(entropy)
}

# Define range of years to pull data
years <- 2010:2019
names(years) <- years

# Code structure taken from link below
# https://walker-data.com/census-r/wrangling-census-data-with-tidyverse-tools.html#preparing-time-series-acs-estimates

# RACE
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
pivoted_race_entropy <- race_by_year %>%
  group_by(NAME, year) %>%
  pivot_wider(id_cols = NAME,
              names_from = year,
              names_prefix = "y",
              values_from = estimate,
              values_fn = entropy_index)

# Convert back into long format
longer_race_entropy <- pivoted_race_entropy %>%
  pivot_longer(!NAME, names_to = "year", values_to = "RaceEntropy")

# Get changes in entropy index from previous year
race_entropy_changes <- longer_race_entropy %>%
  group_by(NAME) %>%
  mutate(RaceEntropyChange = (RaceEntropy - lag(RaceEntropy)))

# INCOME
# Get income estimates for each census tract in Suffolk County
income_by_year <- map_dfr(years, ~{
  get_acs(
    geography = "tract",
    variables = income_vars_acs,
    state = "MA",
    county = "Suffolk",
    summary_var = "DP03_0062",
    survey = "acs5",
    year = .x
  )
}, .id = "year")

# Calculate time series of income bins
pivoted_income <- income_by_year %>%
  group_by(NAME, year) %>%
  pivot_wider(id_cols = NAME,
              names_from = year,
              names_prefix = "y",
              values_from = estimate)

# Calculate time series of entropy index for income 
pivoted_income_entropy <- income_by_year %>%
  group_by(NAME, year) %>%
  pivot_wider(id_cols = NAME,
              names_from = year,
              names_prefix = "y",
              values_from = estimate,
              values_fn = entropy_index)

# Convert back into long format
longer_income_entropy <- pivoted_income_entropy %>%
  pivot_longer(!NAME, names_to = "year", values_to = "IncomeEntropy")

# Get changes in entropy index from previous year
income_entropy_changes <- longer_income_entropy %>%
  group_by(NAME) %>%
  mutate(IncomeEntropyChange = (IncomeEntropy - lag(IncomeEntropy)))

# HOUSING
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

# Join all tables
joined <- left_join(race_entropy_changes, rental_changes, by = c("NAME", "year")) %>%
  left_join(income_entropy_changes, by = c("NAME", "year"))

# Linear models
race_model <- lm(RaceEntropyChange ~ RentalChange, joined)
summary(race_model)

income_model <- lm(IncomeEntropyChange ~ RentalChange, joined)
summary(income_model)

# Exploratory visualizations
race_plot <- ggplot(joined, aes(x = RentalChange, y = RaceEntropyChange)) +
  geom_point() +
  xlim(-15, 15) +
  ylim(-0.26, 0.25)

ggsave("race_plot.pdf", width = 8, height = 6)

income_plot <- ggplot(joined, aes(x = RentalChange, y = IncomeEntropyChange)) +
  geom_point() +
  xlim(-15, 15) +
  ylim(-0.2, 0.2)

ggsave("income_plot.pdf", width = 8, height = 6)
