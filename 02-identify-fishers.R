# 02-identify-fishers.R --------------------------#
# Identify fishers and fishing households
# W. Friedman
# March 2022
# ------------------------------------------------#

# Uncomment if running separate from RMD
# Load libraries
#library(here) 
#source(here("MS_access", "00-load-libraries.R"))

# Load CNH Kiribati datasets
#source(here("MS_access", "00-load-datasets.R"))
#source(here("MS_access", "01-format-tables.R")) 

## Identify Fishers

# Professional fisher:
#   P801. Which of the following best describes what %hmName% is MAINLY doing at present? == "Working in fishing or gleaning seafood"  (n = 46; all villages)
# 
# Casual fishers ('went fishing')
# P901. In the last 7 days, did %hmName% engage in fishing, hunting, or seafood collection? == YES (n = 1298)
# - Not worried about hunting; re: Jacob, people dont hunt on land.
# P902. In the last 7 days, how many hours did %hmName% spend in total on fishing, hunting, or seafood collection during the last week?
# 
# Fishing household: 
# - Contains professional fisher, OR
# - Contains casual fisher, OR
# - Consumed seafood from 'home production' in last 7 days
# - ADD? has any fishing hours? -  check if this is already in here.
# 
# H1503a. In the last 7 days, did any member of this household consume any of the following? (any yes = ate fish/inverts/shark- fresh, canned, preserved)
# H1503l. Was any of the %h1503b% %h1503c% of %rostertitle% consumed, came from HOME PRODUCTION?
  
# Info ---- 
# This is the number of RESPONDENTS who had fished in the last 7 days (n=1289)
hies_tidy %>% filter(p901 == "Yes") %>% 
tabyl(island) %>% adorn_totals()

# fweights are calculated at the household level
# this estimates the NUMBER OF FISHERS in an region
# and ends with the same # of fishers / island as Jacob has from Mike (filter to NSF sites)
hies_tidy %>% filter(p901 == "Yes") %>% 
  filter(island_village %in% c(nsf_villages, cbfm_villages)) %>% 
  group_by(island) %>% 
  summarise(sum_fweight = sum(fweight))


# Casual / professional fishers ----

# 'Fishers' (casual or professional) = 1310 (in hies); 476 (in NSF / CBFM)

fishers <- hies_tidy %>% 
  select(interview__key, respondent_id, fweight, island, village, island_village,rururb, sex, age, p801, p802, p901, p902, contains("p903")) %>%
  mutate(fisher_professional = if_else(p801 == "Working in fishing or gleaning seafood", 1, 0)) %>% 
  # Remove any respondents who said "hunting with gun"
  mutate(fish_method_sum = 
           rowSums(across(matches("p903__1|p903__2|p903__3|p903__4|p903__5|p903__6|p903__7|p903__9|p903__10")), na.rm=T), 
         fish_method_yn = case_when(fish_method_sum >= 1 ~ 1, 
                                    TRUE ~ 0)) %>%
  mutate(fisher_last7 = case_when((p901 == "Yes" & fish_method_yn == 1) ~ 1,
                                   p901 == "No" ~ 0, 
                                   TRUE ~ NA_real_)) %>% 
  mutate(fisher_category = case_when(fisher_professional == 1 ~ "professional", 
                                     fisher_last7 == 1 ~ "casual",
                                     fisher_last7 == 0 ~ "didnt_fish",
                                     TRUE ~ NA_character_))

# 46 'professional', 1264 'casual': all of hies
# (22 'professional', 454 'casual': NSF & CBFM)
fishers %>% 
  tabyl(island,fisher_category) %>% 
  adorn_totals()

# Fishers by island (*un-weighted)
fishers %>% 
  tabyl(island, fisher_category) %>% 
  pivot_longer(-island, names_to = "fisher_category", "values_to" = "n_respondents") %>% 
  ggplot(aes(x = island, y = n_respondents, fill = fisher_category)) + 
  geom_bar(stat = "identity", position = "dodge")+
  theme(axis.text.x = element_text(angle = 90))

fisher_tbl <- fishers %>% 
  select(interview__key, respondent_id, fisher_category)

fisher_tbl


# HOUSEHOLDS who ate fish in the last 7 days (h1503a), from home production (h1503l) ----

# The number of HOUSEHOLDS who ate fish in the last 7 days (h1503a), from home production (h1503l)
# Need to query 'food_recall' to get the Fish and seafood data for households.
# where food == fish and seafood" and type == "home production"
#' h1503' = h1503_query
#!NOTE! These are for all hies data; not only NSF/CBFM sites. Will filter on merge

h1503 <- food_recall %>%
  filter(coicop_class == "Fish and sea food")

h1503_wide <- h1503 %>% 
  filter(type == "Home production") %>% 
  mutate(coicop_clean = snakecase::to_snake_case(coicop),
         coicop_clean = recode(coicop_clean, 
                               "113023015" = "other_113023015",
                               "shellfish_clam_muscles_oystersâ" = "shellfish_clam_muscles_oysters")) %>% 
  select(interview__key,coicop_clean) %>% 
  arrange(coicop_clean, interview__key) %>% 
  pivot_wider(names_from = coicop_clean, names_glue = "h1503a_{coicop_clean}",
              values_from = coicop_clean, values_fn = unique) 

h1503_wide

# Create binary variable for 'any seafood' consumed
# across all; make binary; if(NA) -> 0; else 1; sum; 
# mutate(h1503a_any = if_else(sum_food >=1, 1, 0)

make_binary <- function(x){
  if(is.na(x)){
    x1 <- 0
  }else{x1 <- 1}
  return(x1)
}

# all households in the h1503 subset (ate seafood; home prod) have at least one seafood item listed
h1503_hh <- h1503_wide %>% 
  rowwise() %>% 
  mutate(across(contains("h1503a"), ~ make_binary(.x))) %>%
  mutate(h1503a_any = rowSums(across(contains("h1503a")), na.rm=T),
         h1503_ate_homeprod_seafood = if_else(h1503a_any >= 1, 1, 0)) %>% 
  select(interview__key, h1503_ate_homeprod_seafood)

h1503_hh





## Fishing household ----
#fishing_hh == 1 IF: 
# - HH contains: any casual / professional fishers (see), OR
# - HH contains: any hh with members who ate seafood in the last 7 days (h1503a), from home production (h1503l)
# - n = 2973/5002 respondents, from 486 fishing households (828 total hh's in hies_gov)
# Combine all 3 fisher categories into 1 table

fishers_hh <- fisher_tbl %>% 
  drop_na(fisher_category) %>% 
  distinct(interview__key, fisher_category) %>% 
  rename(fisher_category_hh = fisher_category)
  
# check for hh's with two categories (none)
# fishers_hh %>% get_dupes()

fisher_hh_tbl <- hies_tidy %>% 
  select(interview__key, respondent_id) %>% 
  left_join(fishers_hh, by = c("interview__key")) %>% 
  left_join(h1503_hh, by = c("interview__key")) %>% 
  mutate(fisher_hh = if_else((!is.na(fisher_category_hh) | !is.na(h1503_ate_homeprod_seafood)), 1, 0)) %>% 
  distinct(interview__key, fisher_hh)
  
fisher_hh_tbl

# Keep only the fisher tables from this script
rm(list = c("fishers","h1503","h1503_wide","make_binary", "h1503_hh", "fishers_hh"))
