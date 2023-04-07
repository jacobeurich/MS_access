# 00-load-datasets.R 
# load datasets being used in MS_access

library(here) 
source(here("MS_access", "00-load-libraries.R"))

# Metadata for all question IDs and descriptions:
var_labels <- read_csv(here("Data", "2021-08-30_hies_question-id-to-label-key.csv"))

# HIES household and individual-level tidy data
# Create 'respondent_id' and 'island_village' variables
hies_tidy <- read_csv(here("Data", "2021-08-30_hies_tidy_individual-level.csv")) %>% 
  mutate(respondent_id = str_c(interview__key, hm_basic__id, sep = "-")) %>% 
  mutate(island_village = snakecase::to_snake_case(str_c(island,village, sep = "_")))

hies_tidy_house <- read_csv(here("Data", "2021-08-30_hies_tidy_household-level.csv"))

# Other HIES special-format data that doesn't pivot tidy with the rest of the dataset
hies_expenditure <- read_csv(here("Data", "2021-08-30_hies_expenditures-standard-units.csv"))
hies_income <- read_csv(here("Data", "2021-08-30_hies_income-standard-units.csv"))
root_recall <- read_csv(here("Data", "2021-08-30_special-roster-hies_root-crop-details.csv"))


# HIES LONG: contains info not in other 'tidy' tables.
hies_long <- read_csv(here("Data","2021-08-30_hies_long_qs-with-unique-ids.csv"))

# Food recall table
# For food recall questions, query the expenditures dataset
# Unlike hies_tidy, expenditures data (including food recall questions) are not tagged by question ID
# (WF: Household level?)
food_recall <- hies_expenditure %>%
  filter(section == "21_foodrecall")

# VRS tidy
vrs_tidy <- read_csv(here("Data", "vrs_tidy.csv"))

# VRS metadata
vrs_labels <- read_csv(here("Data", "vrs_question-id-to-label-key.csv"))

# VRS roster: shared catch / food
vrs_share <- read_csv(here("Data", "vrs_share_roster.csv"))
vrs_share_labels <- read_csv(here("Data", "vrs_share_roster_labels.csv")) 

