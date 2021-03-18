# Libraries
library(readr)
library(dplyr)
library(rgdal)
library(tidyr)

# Set working directory
setwd("~/Desktop/beat-analysis-sdpd")

###################################################################
######################## IMPORT STOPS #############################
###################################################################

# Import stop data
library(readr)
stops <- read_csv("ripa_stops_datasd.csv", 
                  col_types = cols(highway_exit = col_character(), 
                                   stop_id = col_character(),
                                   time_stop = col_character(),
                                   land_mark = col_character(), officer_assignment_key = col_character(), 
                                   school_name = col_character()))

# Create column for eventual merge
stops$id <- paste0(stops$stop_id, "_", stops$pid)

# Rearrange
library(dplyr)
stops <- stops %>%
  select(stop_id, pid, id, everything())

# How many unique stop_individuals are there?
n_distinct(stops$id)
# 427,350
## Means there are duplicates in the data

# Find dupes
dups <- table(stops$id) %>% as.data.frame()
dups <- dups %>% filter(Freq >1)
# There are 15 stops_people (30 rows) that appear to have all of the same info
## Except for age. The majority were entered by officers with 1 years experience.

# Remove duplicates from df
## New row count == 427335
stops = subset(stops, !(id %in% dups$Var1))

# Check unique stop_individuals again
n_distinct(stops$id)
# 427,335 which matches row count

# Simplify column names
names(stops)
names(stops) <- c("stop_id", "pid", "id", "ori", "agency", "exp_years", 
                  "date", "time", "dur", "is_serv", "assign_key", 
                  "assign_words", "inters", "block", "ldmk", 
                  "street", "hw_exit", "is_school", "school_name", 
                  "city", "beat", "beat_name", "is_student", "lim_eng", 
                  "age", "gender_words", "is_gendnc", "gender_code", 
                  "gendnc_code", "lgbt")

# Remove dups
remove(dups)

###################################################################
######################## IMPORT RACE ##############################
###################################################################

# Import ethnicity data
race <- read_csv("ripa_race_datasd.csv", 
                 col_types = cols(stop_id = col_character()))

# Create column for merge
race$id <- paste0(race$stop_id, "_", race$pid)

# Rearrange
race <- race %>%
  select(id, everything())

# Remove original stop_id and pid columns to avoid dups in merge
race <- race %>% 
  select(-stop_id, -pid)

# Check for dups / why row count doesn't == stops df
n_distinct(race$id)
# 427,350
## This means duplicate rows

# Multiple races can be assigned to one person and they're all in multiple rows
## Aggregate rows based on the id and collapse bases into one cell
race = aggregate(race~id, data = race, paste, collapse="|")
## New row count == 427,350

###################################################################
###################### IMPORT DISABILITY ##########################
###################################################################

# Import disability data
dis <- read_csv("ripa_disability_datasd.csv", 
                col_types = cols(stop_id = col_character()))

# Create column for merge
dis$id <- paste0(dis$stop_id, "_", dis$pid)

# Rearrange
dis <- dis %>%
  select(id, everything())

# Remove original stop_id and pid columns to avoid dups in merge
dis <- dis %>% 
  select(-stop_id, -pid)

# Check for dups / why row count doesn't == stops df
n_distinct(dis$id)
# 427,350
## This means duplicate rows

# There can be multiple disabilities listed for each person
## Aggregate rows based on the id and collapse bases into one cell
dis <- aggregate(disability~id, data = dis, paste, collapse="|")
## Row count == 427,350

###################################################################
######################## IMPORT REASON ############################
###################################################################

# Import stop reasons data
reason <- read_csv("ripa_stop_reason_datasd.csv", 
                   col_types = cols(reason_for_stopcode = col_character(), 
                                    stop_id = col_character()))

# Create column for merge
reason$id <- paste0(reason$stop_id, "_", reason$pid)

# Rearrange
reason <- reason %>%
  select(id, everything())

# Remove original stop_id and pid columns to avoid dups in merge
reason <- reason %>% 
  select(-stop_id, -pid)

# Check for dups / why row count doesn't == stops df
n_distinct(reason$id)
# 427,350
## This means duplicate rows

# There are duplicates due to multiple reason_for_stop_detail entries/diffs
## Aggregate rows based on the id and collapse bases into one cell
reason2 <- reason %>% 
  group_by(id) %>%
  summarise(reason_for_stop_detail = paste(reason_for_stop_detail, collapse = "|"))

# Upon inspection, no other columns contain multiple (different) entries
## Keep only distinct rows for each id to merge with reason2
reason3 <- reason[!duplicated(reason$id),]

# Remove reason_for_stop_detail from reason3 for merge
reason3 <- reason3 %>% 
  select(-reason_for_stop_detail)

# Left_join reason2 and reason3
reason_final <- left_join(reason2, reason3, by = "id")

# Rearrange
reason_final <- reason_final %>%
  select(id, reason_for_stop, reason_for_stopcode, 
         reason_for_stop_code_text, reason_for_stop_detail, 
         reason_for_stop_explanation)

remove(reason, reason2, reason3)

# Simplify column names
names(reason_final)
names(reason_final) <- c("id", "reason_words", "reasonid", "reason_text", 
                         "reason_detail", "reason_exp")

###################################################################
###################### IMPORT SEARCH BASIS ########################
###################################################################

# Import search basis
search_basis <- read_csv("ripa_search_basis_datasd.csv", 
                         col_types = cols(stop_id = col_character()))

# Create column for merge
search_basis$id <- paste0(search_basis$stop_id, "_", search_basis$pid)

# Rearrange
search_basis <- search_basis %>%
  select(id, everything())

# Remove original stop_id and pid columns to avoid dups in merge
search_basis <- search_basis %>% 
  select(-stop_id, -pid)

# Check for dups / why row count doesn't == stops df
n_distinct(search_basis$id)
# 427,350
## This means duplicate rows

# There can be multiple search bases listed for each person
## Aggregate rows based on the id and collapse bases into one cell
search_basis2 <- search_basis %>% 
  group_by(id) %>%
  summarise(basis_for_search = paste(basis_for_search, collapse = "|"))

# Upon inspection, no other columns contain multiple (different) entries
## Keep only distinct rows for each id to merge with search_basis2
search_basis3 <- search_basis[!duplicated(search_basis$id),]

# Remove orig basis_for_search column
search_basis3 <- search_basis3 %>% 
  select(-basis_for_search)

# Left_join
search_basis_final <- left_join(search_basis2, search_basis3, by = "id")

remove(search_basis, search_basis2, search_basis3)

# Simplify column names
names(search_basis_final)
names(search_basis_final) <- c("id", "search_basis", "search_basis_exp")

###################################################################
###################### IMPORT SEIZE BASIS #########################
###################################################################

# Import seize basis
seize_basis <- read_csv("ripa_prop_seize_basis_datasd.csv", 
                        col_types = cols(stop_id = col_character()))

# Create column for merge
seize_basis$id <- paste0(seize_basis$stop_id, "_", seize_basis$pid)

# Rearrange
seize_basis <- seize_basis %>%
  select(id, everything())

# Remove original stop_id and pid columns to avoid dups in merge
seize_basis <- seize_basis %>% 
  select(-stop_id, -pid)

# Check for dups / why row count doesn't == stops df
n_distinct(seize_basis$id)
# 427,350
## This means duplicate rows

# There can be multiple seize bases listed for each person
## Aggregate rows based on the id and collapse bases into one cell
seize_basis <- seize_basis %>% 
  group_by(id) %>%
  summarise(basisforpropertyseizure = paste(basisforpropertyseizure, collapse = "|"))

# Simplify column names
names(seize_basis) <- c("id", "seiz_basis")

###################################################################
####################### IMPORT PROP TYPE ##########################
###################################################################

# Import property seized type
prop_type <- read_csv("ripa_prop_seize_type_datasd.csv", 
                      col_types = cols(stop_id = col_character()))

# Create column for merge
prop_type$id <- paste0(prop_type$stop_id, "_", prop_type$pid)

# Rearrange
prop_type <- prop_type %>%
  select(id, everything())

# Remove original stop_id and pid columns to avoid dups in merge
prop_type <- prop_type %>% 
  select(-stop_id, -pid)

# Check for dups / why row count doesn't == stops df
n_distinct(prop_type$id)
# 427,350
## This means duplicate rows

# There can be multiple property types listed for each person
## Aggregate rows based on the id and collapse bases into one cell
prop_type <- prop_type %>% 
  group_by(id) %>%
  summarise(type_of_property_seized = paste(type_of_property_seized, collapse = "|"))

# Simplify column names
names(prop_type) <- c("id", "prop_type")

###################################################################
####################### IMPORT CONTRABAND #########################
###################################################################

# Import contraband / evidence
cont <- read_csv("ripa_contraband_evid_datasd.csv", 
                 col_types = cols(stop_id = col_character()))

# Create column for merge
cont$id <- paste0(cont$stop_id, "_", cont$pid)

# Rearrange
cont <- cont %>%
  select(id, everything())

# Remove original stop_id and pid columns to avoid dups in merge
cont <- cont %>% 
  select(-stop_id, -pid)

# Check for dups / why row count doesn't == stops df
n_distinct(cont$id)
# 427,350
## This means duplicate rows

# There can be multiple contrabands listed for each person
## Aggregate rows based on the id and collapse bases into one cell
cont <- cont %>% 
  group_by(id) %>%
  summarise(contraband = paste(contraband, collapse = "|"))

# Simplify column names
names(cont) <- c("id", "cont")

###################################################################
######################### IMPORT ACTIONS ##########################
###################################################################

# Import actions taken
actions <- read_csv("ripa_actions_taken_datasd.csv", 
                    col_types = cols(consented = col_character(), 
                                     stop_id = col_character()))

# Create column for merge
actions$id <- paste0(actions$stop_id, "_", actions$pid)

# Rearrange
actions <- actions %>%
  select(id, everything())

# Remove original stop_id and pid columns to avoid dups in merge
actions <- actions %>% 
  select(-stop_id, -pid)

# Check for dups / why row count doesn't == stops df
n_distinct(actions$id)
# 427,350
## This means duplicate rows

# There can be multiple actions taken for each person
## Aggregate rows based on the id and collapse bases into one cell
actions2 <- actions %>% 
  group_by(id) %>%
  summarise(actions = paste(action, collapse = "|"))

# There's Y or N for consent of each action 
## Aggregate rows based on the id and collapse bases into one cell
actions3 <- actions %>%
  group_by(id) %>%
  summarise(consented = paste(consented, collapse = "|"))

# Left_join actions2 and actions3
actions_final <- left_join(actions2, actions3, by = "id")

remove(actions, actions2, actions3)

# Simplify column names
names(actions_final) <- c("id", "actions", "act_consent")

###################################################################
######################### IMPORT RESULTS ##########################
###################################################################

# Import actions taken
library(readr)
result <- read_csv("ripa_stop_result_datasd.csv", 
                   col_types = cols(pid = col_character(), 
                                    resultkey = col_character(), stop_id = col_character()))

# Create column for merge
result$id <- paste0(result$stop_id, "_", result$pid)

# Rearrange
result <- result %>%
  select(id, everything())

# Remove unnecessary columns
## Will insert result descriptions later
result <- result %>% 
  select(-stop_id, -pid, -result, -code, -resulttext)

# Check for dups / why row count doesn't == stops df
n_distinct(result$id)
# 427,350
## This means duplicate rows

# There can be multiple results for each person
## Extra duplicate rows remain from dup entries in removed columns
## Keep only distinct rows
result <- result[!duplicated(result),]

## Aggregate rows based on the id and collapse resultkeys into one cell
result <- result %>% 
  group_by(id) %>%
  summarise(results = paste(resultkey, collapse = "|"))

###################################################################
########################### MERGE #################################
###################################################################

# Merge
library(plyr)
master <- join_all(list(stops, race, dis, reason_final, search_basis_final, 
                        seize_basis, prop_type, cont, actions_final, result), 
                   by = "id", 
                   type = "left")
## Row count should == 427,335 (stops row count)
## 45 columns total

# Unload plyr, to avoid masking with dplyr
detach("package:plyr", unload=TRUE)

# Remove originals to clean environment
remove(actions_final, cont, dis, prop_type, 
       race, reason_final, search_basis_final, 
       seize_basis, stops, result)

###################################################################
########################### CLEAN #################################
###################################################################

# Remove leading and trailing whitespace
library(stringr)
master <- master %>% 
  mutate_if(is.character, str_trim)

# Remove all types of whitespace inside strings
master <- master %>% 
  mutate_if(is.character, str_squish)

# Remove floating commas
master$reason_exp <- gsub(" , ", ", ", master$reason_exp)
master$search_basis_exp <- gsub(" , ", ", ", master$search_basis_exp)

## Count number of characters in time string
library(stringi)
master$count <- nchar(stri_escape_unicode(master$time))

table(master$count)
#      8     19 
#   427312   23 

# Some times have incorrect dates attached
## gsub out the dates
master$time <- gsub("1900-01-01 ", "", master$time)
master$time <- gsub("1899-12-30 ", "", master$time)

# Create second time column that's in time format
library(chron)
master$time2 <- times(master$time)

# Unload chron, to avoid masking with lubridate
detach("package:chron", unload=TRUE)

# Remove count column
master <- master %>% 
  select(-count)

###################################################################
###################### IMPORT BEAT INFO ###########################
###################################################################

# Import beat shapefile
library(rgdal)
beats <- readOGR(dsn = "pd_beats_datasd", 
                   layer = "pd_beats_datasd")

#Change projection so CRS comes from the polygon objects
beats <- spTransform(beats, CRS("+proj=longlat +ellps=sphere +no_defs"),
                       use_ob_tran=TRUE)

# Import beat populations (2019 ACS Census data by beat from Campaign Zero)
# https://policescorecard.org/assets/san-diego/police-scorecard-san-diego-report.pdf
beat_pop <- read_csv("beat_demographics_2019_acs.csv")

# Clean column names
names(beat_pop) <- c("beat", "total_pop", "white_pop", "black_pop", 
                     "nam_pop", "asian_pop", "pi_pop", "hisp_pop")

###################################################################
############################# AGE #################################
###################################################################

# Create age categories
## Under 25
master$age_category <- ifelse(master$age < 25, "under_25",
                              ifelse(master$age > 24 & master$age < 35, "a25_34",
                                     ifelse(master$age > 34 & master$age < 45, "a35_44",
                                            ifelse(master$age > 44 & master$age < 55, "a45_54",
                                                   ifelse(master$age > 54 & master$age < 65, "a55_64",
                                                          ifelse(master$age > 64, "above_65", "CHECK"))))))

###################################################################
####################### CLEAN RACE ################################
###################################################################

# Create simplified column for race, written out
master <- master %>% 
  mutate(race_simp = str_replace_all(race, "White", "white") %>% 
           str_replace_all("Pacific Islander", "pi") %>% 
           str_replace_all("Native American", "nam") %>% 
           str_replace_all("Middle Eastern or South Asian", "me_sa") %>% 
           str_replace_all("Hispanic/Latino/a", "hisp") %>% 
           str_replace_all("Black/African American", "black") %>% 
           str_replace_all("Asian", "asian"))

# Create new race category
## Hispanic + other race == "hisp"
## More than one race (but not hisp) == "mixed"
master <- master %>% 
  mutate(race_condensed = case_when(str_detect(race_simp, "hisp") ~ "hisp", # if contains "hisp", add to hispanic category
                                    str_detect(race_simp, "\\|") ~ "mixed", # if contains "|", create mixed category
                                    TRUE ~ race_simp)) # if neither above is true, paste original from race_words

# Remove original race column
master <- master %>% 
  select(-race)

# Create race column descriptions for easy calculations
master$asian <- ifelse(grepl("asian", master$race_condensed), 1, 0)
master$black <- ifelse(grepl("black", master$race_condensed), 1, 0)
master$hisp <- ifelse(grepl("hisp", master$race_condensed), 1, 0)
master$me_sa <- ifelse(grepl("me_sa", master$race_condensed), 1, 0)
master$mixed <- ifelse(grepl("mixed", master$race_condensed), 1, 0)
master$nam <- ifelse(grepl("nam", master$race_condensed), 1, 0)
master$pi <- ifelse(grepl("pi", master$race_condensed), 1, 0)
master$white <- ifelse(grepl("white", master$race_condensed), 1, 0)

###################################################################
######################## CLEAN RESULTS ############################
###################################################################

# Create new column for results, written out
master <- master %>% 
  mutate(results_words = str_replace_all(results, "13", "res_schoolc") %>% 
           str_replace_all("12", "res_schoola") %>% 
           str_replace_all("11", "res_dhs") %>% 
           str_replace_all("10", "res_psych") %>% 
           str_replace_all("9", "res_parent") %>% 
           str_replace_all("8", "res_nctrans") %>% 
           str_replace_all("7", "res_interview") %>% 
           str_replace_all("6", "res_arrest_nowarr") %>% 
           str_replace_all("5", "res_arrest_warr") %>% 
           str_replace_all("4", "res_cite_rel") %>% 
           str_replace_all("3", "res_citation") %>% 
           str_replace_all("2", "res_warn") %>% 
           str_replace_all("1", "res_none"))

# Create category for no results
master$results_none <- ifelse(grepl("res_none", master$results_words) 
                              & !grepl("\\|", master$results_words), 1, 0)

# Create category for citations
master$results_cite <- ifelse(grepl("res_cite_rel", master$results_words), 1,
                              ifelse(grepl("res_citation", master$results_words), 1, 0))

# Create category for arrests
master$results_arrest <- ifelse(grepl("res_arrest_nowarr", master$results_words), 1,
                                ifelse(grepl("res_arrest_warr", master$results_words), 1, 0))

# Create final category for arrests, citations and none
master$results_category <- ifelse(master$results_none == 1, "NONE",
                                  ifelse(master$results_cite == 1, "CITE",
                                         ifelse(master$results_arrest == 1, "ARREST", "OTHER")))

###################################################################
###################### CLEAN STOP REASON ##########################
###################################################################

# Create new column for reason of stop simplified
## Only one reason is listed in this column, no multiple entries
master <- master %>% 
  mutate(reason_simp = str_replace_all(reason_words, "Determine whether the student violated school policy", "rs_school") %>% 
           str_replace_all("Possible conduct warranting discipline under Education Code sections 48900, 48900.2, 48900.3, 48900.4 and 48900.7", "rs_ed") %>%
           str_replace_all("Consensual Encounter resulting in a search", "rs_consent") %>%
           str_replace_all("Investigation to determine whether the person was truant", "rs_truant") %>%
           str_replace_all("Knowledge of outstanding arrest warrant/wanted person", "rs_warrant") %>%
           str_replace_all("Known to be on Parole / Probation / PRCS / Mandatory Supervision", "rs_parole") %>% 
           str_replace_all("Reasonable Suspicion", "rs_susp") %>% 
           str_replace_all("Traffic Violation", "rs_traff"))

# Create final "grouped" reason id column
master$reason_condensed <- ifelse(master$reason_simp == "rs_traff", "TRAFFIC",
                                  ifelse(master$reason_simp == "rs_susp", "SUSP",
                                         ifelse(master$reason_simp == "rs_school", "OTHER",
                                                ifelse(master$reason_simp == "rs_ed", "OTHER",
                                                       ifelse(master$reason_simp == "rs_consent", "OTHER",
                                                              ifelse(master$reason_simp == "rs_truant", "OTHER",
                                                                     ifelse(master$reason_simp == "rs_warrant", "OTHER",
                                                                            ifelse(master$reason_simp == "rs_parole", "OTHER", "CHECK"))))))))

###################################################################
######################### CLEAN ACTIONS ###########################
###################################################################

# Create new column for actions, simplified
master <- master %>% 
  mutate(act_simp = str_replace_all(actions, "None", "act_none") %>% 
           str_replace_all("Admission or written statement obtained from student", "act_student") %>%
           str_replace_all("Vehicle impounded", "act_vi") %>%
           str_replace_all("Property was seized", "act_prop_seiz") %>%
           str_replace_all("Search of property was conducted", "act_sch_prop") %>%
           str_replace_all("Asked for consent to search property", "act_req_sch_prop") %>% 
           str_replace_all("Search of person was conducted", "act_sch_pers") %>% 
           str_replace_all("Asked for consent to search person", "act_req_sch_pers") %>% 
           str_replace_all("Person photographed", "act_photo") %>%
           str_replace_all("Physical or Vehicle contact", "act_physical") %>%
           str_replace_all("Chemical spray used", "act_chem") %>%
           str_replace_all("Baton or other impact weapon used", "act_baton") %>%
           str_replace_all("Canine bit or held person", "act_k9_bit") %>%
           str_replace_all("Impact projectile discharged or used", "act_ip") %>%
           str_replace_all("Electronic control device used", "act_elect") %>%
           str_replace_all("Firearm discharged or used", "act_fad") %>%
           str_replace_all("Firearm pointed at person", "act_fp") %>%
           str_replace_all("Canine removed from vehicle or used to search", "act_k9_rem") %>%
           str_replace_all("Patrol car detention", "act_car_det") %>%
           str_replace_all("Handcuffed or flex cuffed", "act_hc") %>%
           str_replace_all("Curbside detention", "act_curb") %>%
           str_replace_all("Field sobriety test conducted", "act_sober") %>%
           str_replace_all("Person removed from vehicle by physical contact", "act_rem_cont") %>%
           str_replace_all("Person removed from vehicle by order", "act_rem_order"))

# Create separate column for action detained category
master$act_detained <- ifelse(grepl("act_car_det", master$act_simp), 1, 
                              ifelse(grepl("act_hc", master$act_simp), 1,
                                     ifelse(grepl("act_curb", master$act_simp), 1, 0)))

# Create separate column for action force category
master$act_force <- ifelse(grepl("act_chem", master$act_simp), 1, 
                           ifelse(grepl("act_baton", master$act_simp), 1,
                                  ifelse(grepl("act_k9_bit", master$act_simp), 1,
                                         ifelse(grepl("act_ip", master$act_simp), 1, 
                                                ifelse(grepl("act_elect", master$act_simp), 1,
                                                       ifelse(grepl("act_fad", master$act_simp), 1,
                                                              ifelse(grepl("act_fp", master$act_simp), 1,
                                                                     ifelse(grepl("act_rem_cont", master$act_simp), 1,
                                                                            ifelse(grepl("act_physical", master$act_simp), 1,0)))))))))

# Create separate column for action none category
master$act_none <- ifelse(grepl("act_none", master$act_simp), 1, 0)

# Create separate column for action other category
master$act_other <- ifelse(grepl("act_k9_rem", master$act_simp), 1, 
                           ifelse(grepl("act_stud", master$act_simp), 1,
                                  ifelse(grepl("act_photo", master$act_simp), 1,
                                         ifelse(grepl("act_sober", master$act_simp), 1,
                                                ifelse(grepl("act_rem_order", master$act_simp), 1, 0)))))

# Create separate column for action search requested category
master$act_req_search <- ifelse(grepl("act_req_sch_prop", master$act_simp), 1, 
                                ifelse(grepl("act_req_sch_pers", master$act_simp), 1, 0))

# Create separate column for action search conducted category
master$act_search <- ifelse(grepl("act_sch_prop", master$act_simp), 1, 
                            ifelse(grepl("act_sch_pers", master$act_simp), 1, 0))

# Create separate column for action seize category
master$act_seize <- ifelse(grepl("act_vi", master$act_simp), 1, 
                           ifelse(grepl("act_prop_seize", master$act_simp), 1, 0))

###################################################################
########################## ANALYSIS ###############################
###################################################################

# Calculate proportion of age categories for all people stopped
master %>% 
  count(age_category) %>% 
  arrange(desc(n))

# age_category      n
#       a25_34 134453
#       a35_44  94964
#       a45_54  74036
#     under_25  63255
#       a55_64  45822
#     above_65  14805

# Calculate percentages
master %>% 
  count(age_category) %>% 
  mutate((prop = n / sum(n))*100) %>% 
  arrange(desc(`(prop = n/sum(n)) * 100`))

# age_category  COUNT               PERCENT
#       a25_34 134453               31.463138
#       a35_44  94964               22.222378
#       a45_54  74036               17.325049
#     under_25  63255               14.802204
#       a55_64  45822               10.722735
#     above_65  14805                3.464495

###################################################################
# Calculate gender bd
gender <- master %>% 
  group_by(gender_words) %>% 
  count(race_condensed)

# Spread
library(tidyr)
gender <- gender %>%
  spread(key = gender_words, value = n, fill = 0)

# Calculate total percentages
gender %>%
  mutate(per_of_female = round((Female / sum(Female))*100,1),
         per_of_male = round((Male / sum(Male))*100,1)) %>% 
  arrange(desc(per_of_male)) %>% 
  select(-`Transgender man/boy`, -`Transgender woman/girl`)

# race_condensed Female   Male `<NA>` per_of_female per_of_male
# white           54444 125158     39          47.4        40.2
# hisp            30949  94282     39          26.9        30.3
# black           18697  65656     22          16.3        21.1
# asian            6947  13260      4           6           4.3
# me_sa            2310   8791      4           2           2.8
# pi                899   2388      4           0.8         0.8
# mixed             461    953      1           0.4         0.3
# nam               237    620      4           0.2         0.2
# TOTAL          114944 311108

# 26.97886643 were female
# 73.02113357 were male
## Out of just males and females

# Add percentages by race, as opposed to by the total
gender %>% 
  mutate(fem_per = round((Female / (Female + Male))*100,1),
         male_per = round((Male / (Female + Male))*100,1),
         total_race = Female + Male) %>%
  arrange(desc(male_per)) %>% 
  select(-`Transgender man/boy`, -`Transgender woman/girl`, -`<NA>`)

# race_condensed Female   Male fem_per male_per total_race
# me_sa            2310   8791    20.8     79.2      11101
# black           18697  65656    22.2     77.8      84353
# hisp            30949  94282    24.7     75.3     125231
# pi                899   2388    27.4     72.6       3287
# nam               237    620    27.7     72.3        857
# white           54444 125158    30.3     69.7     179602
# mixed             461    953    32.6     67.4       1414
# asian            6947  13260    34.4     65.6      20207

# Remove (optional)
remove(gender)

###################################################################
# Calculate age categories by race
age_race <- master %>% 
  group_by(age_category) %>% 
  count(race_condensed)

# Spread
age_race <- age_race %>%
  spread(key = age_category, value = n, fill = 0)

# Filter to blacks, whites and hispanics and asians
age_race2 <- age_race %>% 
  filter(race_condensed == "asian" |
           race_condensed == "black" |
           race_condensed == "hisp" |
           race_condensed == "white")

# Calculate percentages by race to get rates for younger stopped individuals
age_race2 %>% 
  mutate(under_25_per = round((under_25 / (under_25 + a25_34 + a35_44 + a45_54 + a55_64 + above_65))*100,1),
         a25_34_per = round((a25_34 / (under_25 + a25_34 + a35_44 + a45_54 + a55_64 + above_65))*100,1),
         total_race = under_25 + a25_34 + a35_44 + a45_54 + a55_64 + above_65) %>%
  arrange(desc(a25_34_per))

# race_condensed a25_34 a35_44 a45_54 a55_64 above_65 under_25 under_25_per a25_34_per total_race
# hisp            45064  28146  17442   7816     2044    25117         20         35.9     125629
# black           26263  18652  16138  10303     2425    10824         12.8       31        84605
# asian            6020   4170   3371   1893      819     3986         19.7       29.7      20259
# white           51490  40092  34450  24457     9075    20553         11.4       28.6     180117

# Remove (optional)
remove(age_race, age_race2)

###################################################################

# Calculate race by results_category
results_race <- master %>% 
  group_by(results_category) %>% 
  count(race_condensed)

# Spread
results_race <- results_race %>%
  spread(key = results_category, value = n, fill = 0)

# Calculate percentages by race to get rates
results_race %>% 
  mutate(arrest_per = round((ARREST / (ARREST + CITE + NONE + OTHER))*100,1),
         cite_per = round((CITE / (ARREST + CITE + NONE + OTHER))*100,1),
         none_per = round((NONE / (ARREST + CITE + NONE + OTHER))*100,1),
         other_per = round((OTHER / (ARREST + CITE + NONE + OTHER))*100,1),
         total_race = ARREST + CITE + NONE + OTHER) %>%
  arrange(desc(arrest_per))

# race_condensed ARREST  CITE  NONE OTHER arrest_per cite_per none_per other_per total_race
# nam               150   182   114   424       17.2     20.9     13.1      48.7        870
# black           12726 16940 13151 41788       15       20       15.5      49.4      84605
# pi                434   993   420  1465       13.1     30       12.7      44.2       3312
# hisp            16223 37752 18077 53577       12.9     30.1     14.4      42.6     125629
# white           22304 56951 22663 78199       12.4     31.6     12.6      43.4     180117
# mixed             150   437   184   652       10.5     30.7     12.9      45.8       1423
# asian            1499  8531  2392  7837        7.4     42.1     11.8      38.7      20259
# me_sa             652  5264  1280  3924        5.9     47.3     11.5      35.3      11120

# GLM TESTS
# If black / arrests
glmTest1 <- glm(results_arrest ~ black,
                data = master,
                family = "binomial")
summary(glmTest1)
exp(coef(glmTest1))

# (Intercept)       black 
# 0.1387514   1.2846299
## Someone is 1.3 times more likely to be arrested if they are Black (than any other race)

# If hisp / arrests
glmTest2 <- glm(results_arrest ~ hisp,
                data = master,
                family = "binomial")
summary(glmTest2)
exp(coef(glmTest2))

# (Intercept)        hisp 
# 0.1449595   1.0329280
## Someone is 1.03 times more likely to be arrested if they are Hispanic (than any other race)

# If asian / cited
glmTest3 <- glm(results_cite ~ asian,
                data = master,
                family = "binomial")
summary(glmTest3)
exp(coef(glmTest3))

# (Intercept)       asian 
# 0.4107299   1.7710043
## Someone is 1.8 times more likely to be arrested if they are Asian (than any other race)

# Remove (optional)
remove(results_race, glmTest1, glmTest2, glmTest3)

###################################################################
# Calculate disability percentages

## Create check for disability
master$is_dis <- ifelse(master$disability != "None", 1, 0)

master %>% 
  count(is_dis) %>% 
  mutate((prop = n / sum(n))*100) %>% 
  arrange(desc(`(prop = n/sum(n)) * 100`))

# is_dis  COUNT               PERCENT
#      0 407393               95.333404
#      1  19942                4.666596

###################################################################
# Calculate race by beats
beat_race <- master %>% 
  group_by(beat, race_condensed) %>% 
  count(race_condensed)

# Spread
beat_race <- beat_race %>%
  spread(key = race_condensed, value = n, fill = 0)

# Calculate percents among beats
beat_race <- beat_race %>% 
  mutate(asian_per = round((asian / (asian + black + hisp + me_sa + mixed + nam + pi + white))*100,1),
         black_per = round((black / (asian + black + hisp + me_sa + mixed + nam + pi + white))*100,1),
         hisp_per = round((hisp / (asian + black + hisp + me_sa + mixed + nam + pi + white))*100,1),
         me_sa_per = round((me_sa / (asian + black + hisp + me_sa + mixed + nam + pi + white))*100,1),
         mixed_per = round((mixed / (asian + black + hisp + me_sa + mixed + nam + pi + white))*100,1),
         nam_per = round((nam / (asian + black + hisp + me_sa + mixed + nam + pi + white))*100,1),
         pi_per = round((pi / (asian + black + hisp + me_sa + mixed + nam + pi + white))*100,1),
         white_per = round((white / (asian + black + hisp + me_sa + mixed + nam + pi + white))*100,1),
         total_stopped = asian + black + hisp + me_sa + mixed + nam + pi + white)

# Find largest stop rate in each beat
beat_race[, "max"] <- apply(beat_race[, 10:17], 1, max)

# Identify race with highest stop rate in each beat
beat_race$max_name <- ifelse(beat_race$max == beat_race$asian_per, "ASIAN",
                             ifelse(beat_race$max == beat_race$black_per, "BLACK",
                                    ifelse(beat_race$max == beat_race$hisp_per, "HISP",
                                           ifelse(beat_race$max == beat_race$me_sa_per, "ME_SA",
                                                  ifelse(beat_race$max == beat_race$mixed_per, "MIXED",
                                                         ifelse(beat_race$max == beat_race$nam_per, "NAM",
                                                                ifelse(beat_race$max == beat_race$pi_per, "PI",
                                                                       ifelse(beat_race$max == beat_race$white_per, "WHITE", "CHECK"))))))))

beat_race %>% 
  group_by(max_name) %>% 
  count(max_name) %>% 
  arrange(desc(n))

# max_name     n
# WHITE       75
# HISP        39
# BLACK       12

# Compare percent of White and Black individuals stopped by beat only
beat_race$bvw <- ifelse(beat_race$black_per > beat_race$white_per, "BLACK", "WHITE")

beat_race %>% 
  group_by(bvw) %>% 
  count(bvw)

# bvw       n
# BLACK    38
# WHITE    88

# Left join with beats spdf to get beat names
library(tigris)
beat_race <- geo_join(beats, beat_race, "beat", "beat")

# Export to csv for interactive plot
write.csv(beat_race, "beat-race-plot.csv")

###################################################################
# Calculate search rates of each race by beats

# Create df of just total stopped by beat
stopped <- master %>% 
  group_by(beat, race_condensed) %>% 
  count(race_condensed)

# Spread
stopped <- stopped %>%
  spread(key = race_condensed, value = n, fill = 0)

# Rename columns
names(stopped) <- c("beat", "asian_st", "black_st", "hisp_st",
                    "me_sa_st", "mixed_st", "nam_st", "pi_st",
                    "white_st")

# Change character string of NA in search_basis to real NA
master$search_basis[master$search_basis == "NA"] = NA

# Create is_searched column check
master$is_searched <- ifelse(!is.na(master$search_basis),1,0)

# Create df of just those who were searched
searches <- master %>% 
  filter(is_searched == 1)

# Calculate total searched by beat and race
beat_search_race <- searches %>% 
  group_by(beat, race_condensed) %>% 
  count(race_condensed)

# Spread
beat_search_race <- beat_search_race %>%
  spread(key = race_condensed, value = n, fill = 0)

# Left join beat_search_race with stopped
beat_search_race <- left_join(beat_search_race, stopped, by = "beat")

# Calculate search rate for each race by beat
beat_search_race <- beat_search_race %>% 
  mutate(asian_rate = round(asian / (asian_st)*100,1),
         black_rate = round(black / (black_st)*100,1),
         hisp_rate = round(hisp / (hisp_st)*100,1),
         me_sa_rate = round(me_sa / (me_sa_st)*100,1),
         mixed_rate = round(mixed / (mixed_st)*100,1),
         nam_rate = round(nam / (nam_st)*100,1),
         pi_rate = round(pi / (pi_st)*100,1),
         white_rate = round(white / (white_st)*100,1))

# Calculate which beats had blacks searched at higher rates per population than whites
beat_search_race$bvw <- ifelse(beat_search_race$white_rate > beat_search_race$black_rate, "WHITE", "BLACK")

# Count results
beat_search_race %>% 
  group_by(bvw) %>% 
  count(bvw)

# bvw       n
# BLACK    95
# WHITE    31

# Left join with beats spdf to get beat names
beat_search_race <- geo_join(beats, beat_search_race, "beat", "beat")

# Export to csv for interactive plot
write.csv(beat_search_race, "beat-search-race-plot.csv")

###################################################################
# Calculate percents of each race pop out of total population

# Create mixed race / other category column
beat_pop <- beat_pop %>% 
  mutate(mixed_pop = total_pop - (white_pop + black_pop + nam_pop + 
                                    asian_pop + pi_pop + hisp_pop))

# Calculate percentages for each race
beat_pop <- beat_pop %>% 
  mutate(white_per = round((white_pop / total_pop)*100,1),
         black_per = round((black_pop / total_pop)*100,1),
         nam_per = round((nam_pop / total_pop)*100,1),
         asian_per = round((asian_pop / total_pop)*100,1),
         pi_per = round((pi_pop / total_pop)*100,1),
         hisp_per = round((hisp_pop / total_pop)*100,1),
         mixed_per = round((mixed_pop / total_pop)*100,1))

# Find largest pop rate in each beat
beat_pop[, "max"] <- apply(beat_pop[, 10:16], 1, max)

# Identify race with highest portion of total population
beat_pop$max_name <- ifelse(beat_pop$max == beat_pop$asian_per, "ASIAN",
                             ifelse(beat_pop$max == beat_pop$black_per, "BLACK",
                                    ifelse(beat_pop$max == beat_pop$hisp_per, "HISP",
                                                  ifelse(beat_pop$max == beat_pop$mixed_per, "MIXED",
                                                         ifelse(beat_pop$max == beat_pop$nam_per, "NAM",
                                                                ifelse(beat_pop$max == beat_pop$pi_per, "PI",
                                                                       ifelse(beat_pop$max == beat_pop$white_per, "WHITE", "CHECK")))))))

beat_pop %>% 
  group_by(max_name) %>% 
  count(max_name) %>% 
  arrange(desc(n))

# max_name     n
# WHITE       74
# HISP        47
# ASIAN        6

###################################################################
# Calculate stop rates by population
stop_rates <- master %>% 
  group_by(beat, race_condensed) %>% 
  count(race_condensed)

# Spread
stop_rates <- stop_rates %>%
  spread(key = race_condensed, value = n, fill = 0)

pops <- beat_pop %>% 
  select(beat, total_pop, white_pop, black_pop, nam_pop, asian_pop, pi_pop, hisp_pop, mixed_pop)

# left join
stop_rates <- left_join(stop_rates, pops, by = "beat")
remove(pops)

# Calculate rate of people stopped per 100 people of population in beat
stop_rates <- stop_rates %>% 
  mutate(white_rate = round((white / white_pop)*100,1),
         black_rate = round((black / black_pop)*100,1),
         nam_rate = round((nam / nam_pop)*100,1),
         asian_rate = round((asian / asian_pop)*100,1),
         pi_rate = round((pi / pi_pop)*100,1),
         hisp_rate = round((hisp / hisp_pop)*100,1),
         mixed_rate = round((mixed / white_pop)*100,1))

# Bring in beat names
beat_names <- beats@data %>% 
  select(beat, name)

# Join
stop_rates <- left_join(stop_rates, beat_names, by = "beat")

# Remove (optional)
remove(beat_names)

# There are duplicate beats
## Remove them
stop_rates2 <- stop_rates[!duplicated(stop_rates$beat),]

# Calculate which beats had blacks stopped at higher rates per population than whites
stop_rates2$bvw <- ifelse(stop_rates2$white_rate > stop_rates2$black_rate, "WHITE", "BLACK")

# Count results
stop_rates2 %>% 
  group_by(bvw) %>% 
  count(bvw)

# bvw       n
# BLACK   115
# WHITE     9
# NA        2

write.csv(stop_rates, "stop_rates_by_pop.csv")
