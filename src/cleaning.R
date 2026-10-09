# Libraries

library(tidyverse)
library(readxl)
library(here)
library(readabs)

# Word Cloud
raw_names <- read_excel(paste0(here(),"/data/raw/Annual fund-level superannuation statistics June 2025.xlsx"),
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
write_csv(word_cloud, file = paste0(here(),"/data/clean/word_cloud.csv"))

# Balance by State and Gender
raw_statebalances <- read_excel(paste0(here(),"/data/raw/ts24snapshot05indexofchartdata.xlsx"),
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

write_csv(map_data, here("data/clean", "state_balances_map.csv"))

# Balance by Age and Gender
raw_demobalances <- read_excel(paste0(here(),"/data/raw/ts24snapshot05indexofchartdata.xlsx"),
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
write_csv(demobalances, here("data/clean", "demoblances.csv"))

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
write_csv(account_distribution, here("data/clean", "account_distribution.csv"))

# Census data
raw_census10 <- read_csv(paste0(here(),"/data/raw/Retirement.csv"), skip = 8) %>%
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
write_csv(retirees, here("data/clean", "retirees.csv"))

# Census SA2 Data
raw_censusage <- read_csv(paste0(here(),"/data/raw/Age Range.csv"), skip = 8) %>%
  mutate(
    `Average Age` = rowSums(
      across(`0`:`115`, ~ .x * as.numeric(cur_column())),
      na.rm = TRUE
    ) / rowSums(across(`0`:`115`), na.rm = TRUE)
  ) %>% filter(!is.na(`Average Age`)) %>% rename(SA2 = `AGEP Age`) %>%
  select(c(SA2, `Average Age`))

# 
  
