# Libraries

library(tidyverse)
library(readxl)
library(here)
library(readabs)

# Word Cloud
raw_names <- read_excel(paste0(here(),"/src/data/raw/Annual fund-level superannuation statistics June 2025.xlsx"),
                        sheet = "Table 2a", skip = 4) %>% mutate(name = `Fund name`) %>% select(name) %>% filter(!is.na(name))

word_cloud <- raw_names %>%
  mutate(
    # Remove parentheses
    name = str_remove_all(name, "[()]"),
    
    # Split lowercase-to-uppercase boundaries
    name = str_replace_all(name, "([a-z])([A-Z])", "\\1 \\2"),
    
    # Split "Super" unless it starts "Superannuation"
    name = str_replace_all(
      name,
      regex("super(?!annuation)", ignore_case = TRUE),
      " Super "
    ),
    
    # Standardise terms
    name = str_replace_all(
      name,
      regex("\\bsuperannuation\\b", ignore_case = TRUE),
      "Superannuation"
    ),
    name = str_replace_all(
      name,
      regex("\\bconstruction\\b", ignore_case = TRUE),
      "Construction"
    ),
    name = str_replace_all(
      name,
      regex("\\bbuilding\\b", ignore_case = TRUE),
      "Building"
    ),
    
    # Clean up extra spaces
    name = str_squish(name)
  ) %>%
  separate_longer_delim(name, delim = " ") %>%
  filter(name != "") %>%
  count(name, sort = TRUE)
write_csv(word_cloud, file = paste0(here(),"/src/data/clean/word_cloud.csv"))

# Balance by State and Gender
raw_statebalances <- read_excel(paste0(here(),"/src/data/raw/ts24snapshot05indexofchartdata.xlsx"),
                        sheet = "13", skip = 39) %>%
  select(
    `Income year`,
    `State/Territory`,
    contains("Average"),
    contains("Number") 
  ) %>%
  rename(`Male Average account balance` = `Average account balance $...4`,
         `Female Average account balance` = `Average account balance $...7`,
         `Average account balance` = `Average account balance $...10`,
         `Males` = `Number...3`,
         `Females` = `Number...6`,
         `Total` = `Number...9`) %>%
  filter(`Income year` != "2013–14") %>%
  mutate(across(
    c(`Male Average account balance`,
      `Female Average account balance`,
      `Average account balance`,
      Males, Females, Total),
    as.numeric
  )) %>%
  mutate(`Total Balances` = `Average account balance`*`Total`,
         `Male Balances` = `Male Average account balance`*`Males`,
         `Female Balances` = `Female Average account balance`*`Females`,
         `Male Proportion of Balances` = `Male Balances`/`Total Balances`,
         `Female Proportion of Balances` = `Female Balances`/`Total Balances`) %>%
  mutate(`Income year` = str_extract(`Income year`, "^\\d{4}"),
         `State/Territory` = if_else(
           `State/Territory` == "Total",
           "AUS",
           `State/Territory`
         ))

state_coords <- tribble(
  ~`State/Territory`, ~Longitude, ~Latitude,
  "NSW", 151.2, -33.8,
  "VIC", 145.0, -37.8,
  "QLD", 153.0, -27.5,
  "WA", 117.9, -31.9,
  "SA", 138.6, -34.9,
  "TAS", 147.3, -42.0,
  "ACT", 149.1, -35.3,
  "NT", 130.8, -12.5,
  "Unknown or overseas", 157, -38,
  "AUS", 157, -17
)

statebalances_map <- raw_statebalances %>%
  mutate(
    `Income year` = str_replace_all(`Income year`, "-", "\u2013")
  ) %>%
  filter(
    `Income year` >= "2014"
  ) %>%
  left_join(state_coords, by = "State/Territory")

map_data <- statebalances_map %>%
  select(
    `Income year`,
    `State/Territory`,
    Longitude,
    Latitude,
    `Average account balance`,
    `Male Proportion of Balances`,
    `Female Proportion of Balances`
  ) %>%
  pivot_longer(
    cols = c(
      `Male Proportion of Balances`,
      `Female Proportion of Balances`
    ),
    names_to = "Gender",
    values_to = "Proportion"
  ) %>%
  mutate(
    Gender = recode(
      Gender,
      `Male Proportion of Balances` = "Male",
      `Female Proportion of Balances` = "Female"
    )
  ) %>%
  filter(!is.na(Proportion))

write_csv(map_data, here("src/data/clean", "state_balances_map.csv"))

# Balance by Age and Gender
raw_demobalances <- read_excel(paste0(here(),"/src/data/raw/ts24snapshot05indexofchartdata.xlsx"),
                                sheet = "12", skip = 45) %>%
  select(
    `Income year`,
    `Age`,
    contains("Average"),
    contains("Number") 
  ) %>%
  rename(`Male Average account balance` = `Average account balance $...4`,
         `Female Average account balance` = `Average account balance $...7`,
         `Average account balance` = `Average account balance $...10`,
         `Males` = `Number...3`,
         `Females` = `Number...6`,
         `Total` = `Number...9`) %>%
  filter(`Income year` != "2013–14") %>%
  mutate(across(
    c(`Male Average account balance`,
      `Female Average account balance`,
      `Average account balance`,
      Males, Females, Total),
    as.numeric
  )) %>%
  mutate(`Income year` = str_extract(`Income year`, "^\\d{4}")) %>%
  filter(Age != "Total") %>% filter(Age != "unknown")
demobalances <- raw_demobalances %>%
  pivot_longer(
    cols = c(
      `Male Average account balance`,
      `Female Average account balance`,
      `Average account balance`
    ),
    names_to = "Gender",
    values_to = "Average account balance"
  ) %>%
  mutate(
    Gender = recode(
      Gender,
      `Male Average account balance` = "Male",
      `Female Average account balance` = "Female",
      `Average account balance` = "Total"
    )
  )
write_csv(demobalances, here("src/data/clean", "demoblances.csv"))

# Number of Super Accounts
account_distribution <- tibble(
  `Number of accounts` = c(
    "1 account",
    "2 accounts",
    "3 accounts",
    "4 or more accounts"
  ),
  Accounts = c(1, 2, 3, 4),
  Proportion = c(0.80, 0.16, 0.03, 0.01)
)
write_csv(account_distribution, here("src/data/clean", "account_distribution.csv"))

# Census data
raw_census10 <- read_csv(paste0(here(),"/src/data/raw/Retirement.csv"), skip = 8) %>%
  filter(!is.na(Total)) %>%
  rename(SA1 = `AGE10P Age in Ten Year Groups`) %>%
  mutate(
    over_60 = rowSums(
      pick(`60-69 years`, `70-79 years`,
           `80-89 years`, `90-99 years`,
           `100 years and over`),
      na.rm = TRUE
    )
  ) %>% select(c(SA1, over_60, Total))
retirees <- raw_census10 %>%
  mutate(Percentage = round((over_60/Total), digits = 2),
         Retirees = round((over_60/100), digits = 0)) %>%
  filter(over_60 > 99, SA1 != "Total") %>%
  select(c(SA1, Percentage, Retirees))
write_csv(retirees, here("src/data/clean", "retirees.csv"))

# Census SA3 Data
raw_censusage <- read_csv(
  here("src/data/raw/sa3.csv"),skip = 9,col_types = cols(.default = col_character())
) %>%
  rename(SA3 = `AGEP Age`) %>%
  filter(SA3 != "Total") %>%
  mutate(
    across(
      `0`:`115`,
      ~ parse_number(.x, na = c("", " na", "n/a", "N/A", "null"))
    )
  ) %>%
  mutate(
    total_population = rowSums(across(`0`:`115`),na.rm = TRUE
    ),
    Average_Age = if_else(total_population > 0,
      rowSums(across(`0`:`115`,~ .x * as.numeric(cur_column())),na.rm = TRUE) / total_population,NA_real_
    )
  ) %>%
  filter(!is.na(Average_Age)) %>%
  select(SA3, Average_Age)

write_csv(
  raw_censusage,
  here("src/data/clean/sa3_age.csv")
)
library(sf)
library(dplyr)
library(readr)

# Read SA3 geographic boundaries
sa3_map <- st_read(
  "src/data/boundaries/SA3_2021_AUST_GDA2020.shp",
  quiet = TRUE
)

# Read cleaned data
sa3_data <- read_csv("src/data/clean/sa3_age.csv") %>%
  mutate(SA3 = as.character(SA3))

# Join using the matching SA3 code fields.
# Replace these names with the actual column names.
sa3_map <- sa3_map %>%
  left_join(
    sa3_data,
    by = c("SA3_CODE21" = "SA3")
  )
names(sa3_map)
names(sa3_data)
# Simplify geometry to reduce browser rendering time
sa3_map <- sa3_map %>%
  st_make_valid() %>%
  st_transform(3577) %>%
  st_simplify(dTolerance = 100, preserveTopology = TRUE) %>%
  st_transform(4326)

# Export the simplified map
st_write(
  sa3_map,
  "src/data/clean/sa3_choropleth.geojson",
  delete_dsn = TRUE,
  quiet = TRUE
)


# Benefit Reasons
raw_benefitpayments <- read_excel(paste0(here(),"/src/data/raw/Annual fund-level superannuation statistics June 2025.xlsx"),
     sheet = "Table 4", skip = 4) %>%
  select(c(`Fund name`,`Total lump sum benefit payments`,`Total number of member accounts from which a lump sum benefit payment was made`,`Retirement...32`, `Attaining age 65...33`, `Attaining preservation age...34`, 
           `Death...35`, `Termination...36`, `Termination with less than $200...37`, 
           `Unclaimed money and lost member...38`, `Lost member who is found...39`, 
           `Non categorised conditions of release...40`, `Terminal medical condition...41`, 
           `Permanent incapacity...42`, `Temporary incapacity...43`, `Compassionate grounds...44`, 
           `Severe financial hardship...45`, `Excess contributions tax...46`, 
           `Release of unrestricted non-preserved benefits...47`)) %>%
           rename(
             fund = `Fund name`,
             total = `Total lump sum benefit payments`,
             members = `Total number of member accounts from which a lump sum benefit payment was made`,
             retirement = `Retirement...32`,
             age_65 = `Attaining age 65...33`,
             preservation = `Attaining preservation age...34`,
             Death = `Death...35`,
             Termination = `Termination...36`,
             `Termination with less than $200` = `Termination with less than $200...37`,
             `Unclaimed money and lost member` = `Unclaimed money and lost member...38`,
             `Lost member who is found` = `Lost member who is found...39`,
             `Non categorised conditions of release` = `Non categorised conditions of release...40`,
             `Terminal medical condition` = `Terminal medical condition...41`,
             `Permanent incapacity` = `Permanent incapacity...42`,
             `Temporary incapacity` = `Temporary incapacity...43`,
             `Compassionate grounds` = `Compassionate grounds...44`,
             `Severe financial hardship` = `Severe financial hardship...45`,
             `Excess contributions tax` = `Excess contributions tax...46`,
             `Release of unrestricted non-preserved benefits` = `Release of unrestricted non-preserved benefits...47`
           ) %>% 
  # Turning to numeric removing *
  mutate(
    across(
      c(
        total,
        members,
        retirement,
        age_65,
        preservation,
        Death,
        Termination,
        `Termination with less than $200`,
        `Unclaimed money and lost member`,
        `Lost member who is found`,
        `Non categorised conditions of release`,
        `Terminal medical condition`,
        `Permanent incapacity`,
        `Temporary incapacity`,
        `Compassionate grounds`,
        `Severe financial hardship`,
        `Excess contributions tax`,
        `Release of unrestricted non-preserved benefits`
      ),
      ~ as.numeric(na_if(as.character(.x), "*"))
    )
  )
payments <- raw_benefitpayments %>% filter(!is.na(total)) %>% filter(!is.na(retirement)) %>%
  mutate(
    other = rowSums(
      across(
        c(
          Termination,
          `Termination with less than $200`,
          `Unclaimed money and lost member`,
          `Lost member who is found`,
          `Non categorised conditions of release`,
          `Terminal medical condition`,
          `Permanent incapacity`,
          `Temporary incapacity`,
          `Compassionate grounds`,
          `Severe financial hardship`,
          `Excess contributions tax`,
          `Release of unrestricted non-preserved benefits`
        )
      ),
      na.rm = TRUE
    ),
      missing_per = 1 -
        (coalesce(retirement, 0) + coalesce(age_65, 0) + coalesce(preservation, 0) +coalesce(other, 0))
  ) %>%
  select(
    fund,
    total,
    members,
    retirement,
    age_65,
    preservation,
    other,
    missing_per
  )
ratio <- sum(payments$age_65, na.rm = TRUE) /
  sum(payments$preservation, na.rm = TRUE)

benefitpayments <- payments %>%
  mutate(
    age65_missing = is.na(age_65),
    preservation_missing = is.na(preservation),
    age_65 = case_when(
      age65_missing & !preservation_missing ~ missing_per,
      age65_missing & preservation_missing ~
        missing_per * ratio / (1 + ratio),
      TRUE ~ age_65
    ),
    preservation = case_when(
      preservation_missing & !age65_missing ~ missing_per,
      preservation_missing & age65_missing ~ missing_per / (1 + ratio),
      TRUE ~ preservation
    ),
    average_payments = (total/members)*1000
  ) %>%
  select(-age65_missing, -preservation_missing, -missing_per)


write_csv(benefitpayments, here("src/data/clean", "benefit_payments.csv"))

# Bump chart members rank
raw_memberships <- read_excel(
  paste0(
    here(),
    "/src/data/raw/Annual fund-level superannuation statistics back series June 2004 to June 2025.xlsx"
  ),
  sheet = "Table 10",
  skip = 5
) %>%
  rename(
    Year = Period,
    Fund = `Fund name`,
    Benefits = `Members' benefits at beginning of period`,
    Members = `Number of member accounts at beginning of period`
  ) %>%
  select(Year, Fund, ABN, Benefits, Members) %>%
  filter(!is.na(Year)) %>%
  filter(Members != "*") %>%
  mutate(
    Fund = str_squish(str_replace_all(Fund, "\u00A0", " ")),
    ABN = as.character(ABN),
    Year = as.integer(substr(as.character(Year), 1, 4)),
    Benefits = as.numeric(Benefits),
    Members = as.numeric(Members)
  )

fund_rankings <- bind_rows(
  raw_memberships %>%
    filter(!is.na(Benefits)) %>%
    group_by(Year) %>%
    mutate(
      Rank = min_rank(desc(Benefits))
    ) %>%
    ungroup() %>%
    filter(Rank <= 5) %>%
    mutate(Ranking_type = "Assets"),
  
  raw_memberships %>%
    filter(!is.na(Members)) %>%
    group_by(Year) %>%
    mutate(
      Rank = min_rank(desc(Members))
    ) %>%
    ungroup() %>%
    filter(Rank <= 5) %>%
    mutate(Ranking_type = "Member accounts")
) %>%
  arrange(Ranking_type, Year, Rank) %>%
  select(Year, Ranking_type, Fund, ABN, Members, Benefits, Rank)
write_csv(fund_rankings, here("src/data/clean", "fund_rankings.csv"))

read_csv("src/data/clean/fund_rankings.csv") %>%
  filter(
    Year >= 2015,
    Year <= 2025,
    Ranking_type == "Assets"
  ) %>%
  distinct(Fund) %>%
  arrange(Fund)

# Gender Population Pyramid

# Age Profiles over time?
top_bracket_value <- 1500000
bracket_info <- tibble::tibble(
  `Members' benefit bracket` = c(
    "< $1,000",
    "$1,000 to $5,999",
    "$6,000 to $9,999",
    "$10,000 to $14,999",
    "$15,000 to $24,999",
    "$25,000 to $39,999",
    "$40,000 to $59,999",
    "$60,000 to $99,999",
    "$100,000 to $199,999",
    "$200,000 to $499,999",
    "$500,000 to $999,999",
    "$1,000,000 +"
  ),
  lower = c(
    0, 1000, 6000, 10000, 15000, 25000,
    40000, 60000, 100000, 200000, 500000, 1000000
  ),
  upper = c(
    999, 5999, 9999, 14999, 24999, 39999,
    59999, 99999, 199999, 499999, 999999, NA
  ),
  bracket_order = 1:12
) %>%
  mutate(
    midpoint = case_when(
      is.na(upper) ~ top_bracket_value,
      TRUE ~ (lower + upper) / 2
    )
  )
age_order <- c(
  "<25",
  "25 to 29",
  "30 to 34",
  "35 to 39",
  "40 to 44",
  "45 to 49",
  "50 to 54",
  "55 to 59",
  "60 to 64",
  "65 to 69",
  "70 to 74",
  "75 to 84",
  "85+"
)
estimate_median_by_age <- function(file_path, persons_col, date) {
  
  raw_age_balances <- read_excel(
    file_path,
    sheet = "Table 8",
    skip = 6
  ) %>%
    rename(Persons = all_of(persons_col)) %>%
    filter(
      !is.na(Persons),
      !`Age bracket` %in%
        c("Age information not available", "Total"),
      !`Members' benefit bracket` %in% "Total"
    ) %>%
    select(
      `Age bracket`,
      `Members' benefit bracket`,
      Persons
    )
  
  balance_data <- raw_age_balances %>%
    mutate(
      `Age bracket` = str_squish(
        str_replace_all(`Age bracket`, "\u00A0", " ")
      ),
      `Members' benefit bracket` = str_squish(
        str_replace_all(
          `Members' benefit bracket`, "\u00A0", " "
        )
      ),
      `Age bracket` = factor(
        `Age bracket`,
        levels = age_order,
        ordered = TRUE
      )
    ) %>%
    left_join(
      bracket_info,
      by = "Members' benefit bracket"
    )
  
  median_by_age <- balance_data %>%
    group_by(`Age bracket`) %>%
    arrange(bracket_order, .by_group = TRUE) %>%
    mutate(
      Total_persons = sum(Persons, na.rm = TRUE),
      Cumulative_before = lag(
        cumsum(Persons), default = 0
      ),
      Cumulative_after = cumsum(Persons)
    ) %>%
    filter(
      Total_persons > 0,
      Cumulative_before < Total_persons / 2,
      Cumulative_after >= Total_persons / 2
    ) %>%
    mutate(
      Estimated_median = if_else(
        !is.na(upper) & Persons > 0,
        lower +
          ((Total_persons / 2 - Cumulative_before) /
             Persons) * (upper - lower + 1),
        NA_real_
      )
    ) %>%
    ungroup() %>%
    select(`Age bracket`, Estimated_median)
  
  median_by_age %>%
    mutate(Date = as.Date(date)) %>%
    select(Date, `Age bracket`, Estimated_median)
}

file_path <- paste0(
  here(),
  "/src/data/raw/Quarterly Superannuation Industry Publication - June 2026.xlsx"
)

median_profiles <- bind_rows(
  estimate_median_by_age(
    file_path,
    persons_col = "...7",
    date = "2016-06-30"
  ),
  estimate_median_by_age(
    file_path,
    persons_col = "...15",
    date = "2021-06-30"
  ),
  estimate_median_by_age(
    file_path,
    persons_col = "...23",
    date = "2026-06-30"
  )
)%>%
  mutate(Date = lubridate::year(Date))
write_csv(median_profiles, here("src/data/clean", "age_profiles.csv"))



# Gender population pyramid

raw_gender <- read_excel(
  here(
    "data", "raw",
    "Quarterly Superannuation Industry Publication - June 2026.xlsx"
  ),
  sheet = "Table 7",
  col_names = FALSE
) %>%
  mutate(excel_row = row_number()) %>%
  select(
    `Age bracket` = 1,
    Gender = 2,
    value = 23,
    excel_row
  )

measure_rows <- tribble(
  ~Measure,           ~start_row, ~end_row,
  "Member accounts ('000)",  8L,         59L,
  "Member benefits ($ million)",  72L,        123L,
  "Average balance ($)",  135L,       186L
)

gender <- raw_gender %>%
  mutate(
    measure = case_when(
      between(excel_row, 8L, 59L)    ~ "Member accounts ('000)",
      between(excel_row, 72L, 123L)  ~ "Member benefits ($ million)",
      between(excel_row, 135L, 186L) ~ "Average balance ($)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(measure)) %>%
  mutate(
    value = str_squish(as.character(value)),
    value = str_remove_all(value, ","),
    value = na_if(value, "-"),
    value = na_if(value, ""),
    value = as.numeric(value)
  ) %>%
  filter(Gender != "Other") %>% filter(Gender != "Total")
write_csv(gender, here("src/data/clean", "gender.csv"))

# Joining SA1 Dots and data

library(sf)
library(dplyr)
library(readr)
library(purrr)

# 1. Read the retirees data
retirees <- read_csv(
  "data/clean/retirees.csv",
  col_types = cols(SA1 = col_character())
)

# 2. Read the SA1 boundaries
sa1 <- st_read(
  "src/data/boundaries/SA1_2021_AUST_GDA2020.shp",
  quiet = TRUE
)

# Check the actual SA1 code column in the shapefile
names(sa1)

# IMPORTANT:
# Replace SA1_CODE21 below with the actual column name
# returned by names(sa1), if it differs.
sa1 <- sa1 %>%
  mutate(SA1 = as.character(SA1_CODE21))


# 3. Join retiree counts to the boundaries
sa1_retirees <- sa1 %>%
  inner_join(retirees, by = "SA1") %>%
  filter(!is.na(Retirees), Retirees > 0)

# 4. Remove missing or empty geometries
sa1_retirees <- sa1_retirees %>%
  filter(!st_is_empty(geometry), !is.na(st_dimension(geometry)))

# Check how many observations were removed
cat("SA1 areas remaining:", nrow(sa1_retirees), "\n")

# 5. Project geometries
sa1_retirees <- st_transform(sa1_retirees, 3577)

# 6. Generate points inside each SA1
set.seed(123)

dot_points <- purrr::map2(
  st_geometry(sa1_retirees),
  seq_len(nrow(sa1_retirees)),
  function(polygon, i) {
    n <- as.integer(sa1_retirees$Retirees[i])
    
    if (is.na(n) || n <= 0) {
      return(st_sfc(crs = st_crs(sa1_retirees)))
    }
    
    points <- sf::st_sample(
      polygon,
      size = n,
      type = "random"
    )
    
    points
  }
)

# 7. Keep the SA1 classification associated with every dot
dot_data <- purrr::map2(
  dot_points,
  seq_along(dot_points),
  function(points, i) {
    if (length(points) == 0) {
      return(NULL)
    }
    
    coords <- st_coordinates(points)
    
    tibble(
      longitude = coords[, 1],
      latitude = coords[, 2],
      SA1 = sa1_retirees$SA1[i],
      Percentage = sa1_retirees$Percentage[i]
    )
  }
)

retiree_dots <- bind_rows(dot_data)

# 8. Convert coordinates to longitude and latitude
# Points are still in the projected CRS, EPSG:3577
dots_sf <- st_as_sf(
  retiree_dots,
  coords = c("longitude", "latitude"),
  crs = 3577,
  remove = FALSE
)

dots_sf <- st_transform(dots_sf, 4326)
coords <- st_coordinates(dots_sf)

retiree_dots <- retiree_dots %>%
  mutate(
    longitude = coords[, 1],
    latitude = coords[, 2],
    age_group = if_else(
      Percentage > 0.7,
      "Over 50%",
      "50% or less",
      missing = "Unknown"
    )
  )

# 9. Export for Vega-Lite
write_csv(
  retiree_dots,
  "src/data/clean/retiree_dots.csv"
)

cat("Dots generated:", nrow(retiree_dots), "\n")
cat("Expected dots:", sum(sa1_retirees$Retirees, na.rm = TRUE), "\n")