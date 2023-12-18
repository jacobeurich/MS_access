# 01-format-tables.R -----------------------------#
# Load datasets and format tables 
# used in Access MS
# W. Friedman
# March 2022
# ------------------------------------------------#

# Uncomment if running separate from RMD
# Load libraries
#library(here) 
#source(here("MS_access", "00-load-libraries.R"))

# Load CNH Kiribati datasets
#source(here("MS_access", "00-load-datasets.R"))

# Format tables ----
#  - Use 'hh' and 'vrs' from here

# HEIS (#dim: 5002x1560)
#  - IF DESIRED: filter hh and vrs for NSF / CBFM sites only HERE (add line below):
#  - filter(island_village %in% c(nsf_villages, cbfm_villages))

hh <- hies_tidy

# VRS
vrs <- vrs_tidy %>% 
  separate(vrs_village, into = c('village_id', 'village'), sep = " - ") %>%
  mutate(island_village = snakecase::to_snake_case(str_c(vrs_island,village, sep = "_")))

# Linking table respondent__id - interview__key - island_village - island
id_tbl <- hh %>% select(interview__key, respondent_id, hm_basic__id, island, island_village, sex, age) %>% 
  distinct(.keep_all = T) %>% 
  mutate(age_category = case_when(is.na(age) ~ NA_character_, 
                                  age < 15 ~ "child", 
                                  age >= 15 ~ "adult"))

# Linking table interview__key - island village - island
hh_tbl <- hh %>% select(interview__key, island, island_village) %>% 
  distinct(.keep_all = T)


# Village location - from VRS
# VRS has most complete location information
# Some villages have more than 1 entry; just take the first location
# beru_taboiaki is missing. Set to: lat: -1.368039, lon: 176.013625 (web search)
# south_tarawa_antebuka is missing. location estimated @Antebuka Store (LMTA); lat: 1.341231; lon: 173.026247
# beru_aoniman is missing.lat: -1.309231, lon: 175.98555 
village_loc <- vrs %>% 
  select(island_village, starts_with("villageGPS")) %>% 
  rename(lat = villageGPS__Latitude, lon = villageGPS__Longitude) %>% 
  mutate(lat = if_else(island_village == "beru_taboiaki", -1.368039, lat), 
         lon = if_else(island_village == "beru_taboiaki", 176.013625, lon)) %>% 
  arrange(villageGPS__Accuracy) %>% 
  distinct(island_village, .keep_all = T) %>% 
  select(island_village, lon, lat) %>% 
  drop_na(island_village) %>% 
  bind_rows(tibble(island_village = "south_tarawa_antebuka", 
                   lon = 173.026247, lat = 1.341231)) %>% 
  bind_rows(tibble(island_village = "beru_aoniman", 
                   lon = 175.98555, lat = -1.309231))

# export for team
# village_loc %>% write_csv(here("Data", "village_locations.csv"))

# Village info
# Create 'island_village' variable for joins
# Join with (more complete) village location

village_info <- read_csv(here("Data_shared", "village_metadata.csv")) %>% 
  mutate(island_village = snakecase::to_snake_case(str_c(island,village, sep = "_"))) %>% 
  select(-c(lat, lon)) %>% 
  left_join(village_loc, by = "island_village") %>% 
  mutate(region = if_else(island %in% c("Teeraina","Tabuaeran","Kiritimati"), "line_islands", "gilbert_islands"))

island_info <- village_info %>% 
  group_by(island) %>% 
  summarise(n_villages = n(), 
            mean_lat = mean(lat), 
            region = unique(region))


# Linking table (hh-vrs): island, island_village
village_tbl <- hh %>% select(island, island_village) %>% 
  distinct(.keep_all =T) %>% 
  left_join(island_info) %>% 
  arrange(region, -mean_lat, island_village) %>% 
  mutate(vlg_order = seq(1,111)) %>% 
  mutate(region = recode(region, 
                         "gilbert_islands" = "Gilbert Islands",
                         "line_islands" = "Line Islands")) %>% 
  select(region, island, island_village, vlg_order)

village_tbl

# Linking table: VRS interview key, island, island_village
# NOTE: 1 village (from Beru; interview__key == "43-87-07-62") 
# doesn't have a name - dropped.
vrs_tbl <- vrs %>% 
  select(interview__key, island_village) %>% 
  left_join(village_tbl) %>% 
  drop_na(island_village)

# NSF & CBFM VILLAGES ----
# CBFM sites as of April 2019: Buariki (N Tarawa), Tabonibara (N Tarawa), Kuma (Butaritari), Tanimaiaki (Butaritari), Bikaati/Notururu (Butaritari). Last one doesn't exist in hies.

nsf_villages <- village_info %>% 
  filter(nsf_site == "yes") %>% 
  pull(island_village)

cbfm_villages <- c("north_tarawa_buariki", "north_tarawa_tabonibara", 
                   "butaritari_kuma", "butaritari_tanimainiku") %>% sort()



# AME table
# From Coates et al 2017; Table 2; Average energy (calories) AME weight by age, sex
# Calculated from:
# 1 World Health Organization, Food and Agriculture Organization of the United
# Nations, University UN. Human Energy Requirements. Food and Nutrition Technical
# Report Series, 2004.
# 2 World Health Organization, Food and Agriculture Organization, University UN.
# Protein and Amino Acid Requirements in Human Nutrition. WHO Technical Report Series
# 935, 2007.
# 3 World Health Organization, Food and Agriculture Organization. Vitamin and Mineral
# Requirements in Human Nutrition, 2nd Edition. Report of a joint FAO/WHO expert
# consultation, Bangkok, Thailand, 21–30 September 1998, 2004

ame_table <- read_csv(here("MS_access","inputs","coates_2017_table2_ame.csv")) %>% 
  clean_names() %>%
  remove_empty() %>% 
  mutate(age_sex = str_c(age_category, sex, sep = "_"))

# note: these age categories are based on AME stats; other age cat's are based on working age
hh_ame <- hh %>% 
  select(interview__key, respondent_id, sex, age) %>%
  mutate(age_category = case_when(age < 1 ~ "infant", 
                                  age < 5 ~ "child", 
                                  age < 18 ~ "youth",
                                  age <= 65 ~ "adult", 
                                  age > 65 ~ "ederly")) %>% 
  mutate(age_sex = str_c(age_category, tolower(sex), sep = "_")) %>% 
  left_join(ame_table[c("age_sex","ame_energy")]) %>% 
  mutate(ame_energy = as.numeric(ame_energy)) %>% 
  group_by(interview__key) %>% 
  summarise(ame_sum = sum(ame_energy), 
            hh_size = n())

# Legal working age in Kiribati: 15-64 (ILL; international labor org)
hh_ageclass <- id_tbl %>% 
  group_by(interview__key, age_category) %>% 
  summarise(n_agecat = n()) %>% 
  pivot_wider(names_from = age_category, values_from = n_agecat) %>% 
  rename(n_adults = adult, n_children = child) %>% 
  mutate(across(c(n_adults, n_children), ~replace_na(., 0))) %>% 
  mutate(hh_size = n_adults+n_children)
  

# Color Palette mapped to island
n_islands = length(unique(village_tbl$island))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))
island_palette = getPalette(n_islands)

# Color Palette for categorical vars (5 categories)
cat_palette = rev(hcl.colors(5, palette = "Zissou 1"))
cat_palette2 = c("cornflowerblue","grey70","purple")

