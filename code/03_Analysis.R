########################################################################
######About#############################################################
########################################################################

#This script is used to: 
#Clean data from participant surveys
#Analyze data from participant surveys
#Data from participant survyes can be made available upon request 



#Load Libraries
library(tidyverse)
library(magrittr)
library(reshape2)
library(fifer)
library(FSA)
library(rcompanion)
library(ggstatsplot)
library(ggpubr)



set.seed(123)

#Load data
data = read_csv("./data_cleaned.csv") #Can be made available upon request



#################################################################################
##########################Clean Categorical Variables############################
#################################################################################

###########################AGE GROUP#############################################
summary(data$numeric_age)

data %<>% 
  dplyr::mutate(age_bin = cut(numeric_age, breaks=c(0, 17, 28, 38, 48, 58, 68, 78, 83))) %>% 
  mutate(age_bin = as.character(age_bin)) %>%
  mutate(age_bin = case_when(age_bin == "(0,17]" ~ "(0,5]", 
                             age_bin == "(17,28]" ~ "(18-28]", 
                             TRUE ~ age_bin)) 


###########################PETS##############################################


data = data %>% 
  mutate(pets = as.character(pets)) %>%
  mutate(pets = case_when(pets == "I do not live with any companion animals" ~ "No", 
                          pets == "My child does not live with any companion animals" ~ "No", 
                          TRUE ~ pets)) %>%
  dplyr::mutate(pets = stringr::str_replace(pets, " ", "")) %>% 
  dplyr::mutate(pets = stringr::str_replace(pets, "\t", "")) %>% 
  dplyr::mutate(pets = stringr::str_replace(pets, "\t", "")) %>% 
  mutate(pets = case_when(pets == "Cats" ~ "Cats Only", 
                          pets == "Dogs" ~ "Dogs Only", 
                          pets == "Cats,Dogs" ~ "Cats & Dogs", 
                          pets == "Birds" ~ "Birds", 
                          pets == "Cats,Dogs,Other" ~ "Cats & Dogs & Other", 
                          pets == "Cats,Dogs,Birds" ~ "Cats & Dogs & Birds", 
                          pets == "Cats,Dogs,Rodents/SmallMammals" ~ "Cats & Dogs & Other",
                          pets == "Cats,Dogs,Rodents/SmallMammals" ~ "Cats & Dogs & Other",
                          pets == "Cats,Reptiles" ~ "Cats & Other", 
                          pets == "Cats,Reptiles,Rodents/Small Mammals" ~ "Cats & Other", 
                          pets == "Cats,Rodents/SmallMammals" ~ "Cats & Other", 
                          pets == "Cats,Birds,Reptiles" ~ "Cats & Birds & Other",
                          pets == "Dogs,Birds" ~ "Dogs & Other", 
                          pets == "Dogs,Reptiles" ~ "Dogs & Other",
                          pets == "Dogs,Rodents/SmallMammals" ~"Dogs & Other",
                          pets == "Dogs,Other" ~ "Dogs & Other", 
                          pets == "Reptiles,Rodents/SmallMammals"~ "Other", 
                          pets == "Rodents/SmallMammals" ~ "Other", 
                          TRUE ~ pets))


data %<>% 
  dplyr::mutate(pets = as.character(pets)) %>%
  dplyr::mutate(pets_binary = case_when(pets == "No" ~ "No", 
                                        pets == NA ~ NA_character_, 
                                        TRUE ~ "Yes")) %>%
  mutate(cats =  case_when(grepl("Cats", pets) ~ "Yes",
                           TRUE ~ "No")) %>% 
  mutate(dogs =  case_when(grepl("Dogs", pets) ~ "Yes",
                           TRUE ~ "No")) %>%
  mutate(birds =  case_when(grepl("Birds", pets) ~ "Yes",
                            TRUE ~ "No")) %>%
  mutate(small_mammals =  case_when(grepl("Rodents", pets) ~ "Yes",
                                    TRUE ~ "No")) %>%
  mutate(reptiles =  case_when(grepl("Reptiles", pets) ~ "Yes",
                               TRUE ~ "No")) %>%
  mutate(pets_other =  case_when(grepl("Other", pets) ~ "Yes",
                                 TRUE ~ "No"))

###############################RACE & ETHNICITY########################################

data %<>% 
  mutate(race = case_when(race == "Mixed race: Asian & White" ~ "Asian", 
                          race == "Prefer not to respond" ~ NA_character_, 
                          TRUE ~ race)) %>% 
  mutate(ethnicity = case_when(ethnicity == "Prefer not to respond" ~ NA_character_, 
                               TRUE ~ ethnicity))


##############################GI CONDITIONS##############################################

data %<>% 
  mutate(acid_reflux =  case_when(grepl("Acid reflux", gi_symptoms_currently) ~ "Yes",
                                  TRUE ~ "No")) %>% 
  mutate(GERD =  case_when(grepl("GERD", gi_symptoms_currently) ~ "Yes",
                           TRUE ~ "No")) %>% 
  mutate(diverticulitis =  case_when(grepl("Diverticulitis", gi_symptoms_currently) ~ "Yes",
                                     TRUE ~ "No")) %>% 
  mutate(gallbladder_removed =  case_when(grepl("Gallbladder previously removed", gi_symptoms_currently) ~ "Yes",
                                          TRUE ~ "No")) %>% 
  mutate(IBS =  case_when(grepl("Inflammatory Bowel Disease", gi_symptoms_currently) ~ "Yes",
                          TRUE ~ "No")) %>% 
  mutate(other_gi_condition =  case_when(grepl("Other", gi_symptoms_currently) ~ "Yes",
                                         grepl("Gastritis", gi_symptoms_currently) ~ "Yes",
                                         grepl("intestinal dysmotility", gi_symptoms_currently) ~ "Yes",
                                         grepl("gastroparesis", gi_symptoms_currently) ~ "Yes",
                                         TRUE ~ "No")) %>% 
  mutate(gi_condition_binary =  case_when(gi_symptoms_currently == "No" ~ "No",
                                          gi_symptoms_currently == NA ~ NA_character_, 
                                          TRUE ~ "Yes"))

##############################GI Symptoms Past 48 Hours######################################

data %<>% 
  mutate(nausea_48hr =  case_when(grepl("Nausea", gi_symptoms_past_48_hours) ~ "Yes",
                                  TRUE ~ "No")) %>% 
  mutate(constipation_48hr =  case_when(grepl("Constipation", gi_symptoms_past_48_hours) ~ "Yes",
                                        TRUE ~ "No")) %>% 
  mutate(diarrhea_48hr =  case_when(grepl("Diarrhea", gi_symptoms_past_48_hours) ~ "Yes",
                                    TRUE ~ "No")) %>% 
  mutate(gi_symptoms_48hr_binary = case_when(gi_symptoms_past_48_hours == "I have not had any of the above symptoms" ~ "No",
                                             gi_symptoms_past_48_hours == NA ~ NA_character_, 
                                             TRUE ~ "Yes"))


################################GI Symptoms Past Month##########################################


data %<>% 
  mutate(nausea_month =  case_when(grepl("Nausea", symptoms_past_3_to_30_days) ~ "Yes",
                                   TRUE ~ "No")) %>% 
  mutate(constipation_month =  case_when(grepl("Constipation", symptoms_past_3_to_30_days) ~ "Yes",
                                         TRUE ~ "No")) %>% 
  mutate(diarrhea_month =  case_when(grepl("Diarrhea", symptoms_past_3_to_30_days) ~ "Yes",
                                     TRUE ~ "No")) %>% 
  mutate(gi_symptoms_month_binary = case_when(symptoms_past_3_to_30_days == "I have not had any of the above symptoms" ~ "No",
                                              symptoms_past_3_to_30_days == NA ~ NA_character_, 
                                              TRUE ~ "Yes"))

################################Livestock Exposure##########################################

data %<>% 
  mutate(exposure_livestock_binary = case_when(regular_livestock_exposure == "No" ~ "No",
                                               regular_livestock_exposure == NA ~ NA_character_,
                                               TRUE ~ "Yes")) %>%
  mutate(exposure_poultry =  case_when(grepl("Poultry", regular_livestock_exposure) ~ "Yes",
                                       TRUE ~ "No")) %>% 
  mutate(exposure_horses =  case_when(grepl("Horses", regular_livestock_exposure) ~ "Yes",
                                      TRUE ~ "No")) %>% 
  mutate(exposure_large_animals =  case_when(grepl("livestock, or other large animals", regular_livestock_exposure) ~ "Yes",
                                             TRUE ~ "No"))



#########################Environmental Risk Exposure###############################

data %<>% 
  mutate(environmental_exposure_binary = case_when(regular_exposure_to_env_risk == "I do not have regular exposure to any of these choices" ~ "No",
                                                   regular_exposure_to_env_risk == NA ~ NA_character_,
                                                   TRUE ~ "Yes")) %>%
  mutate(exposure_companion_animals =  case_when(grepl("Companion animals", regular_exposure_to_env_risk) ~ "Yes",
                                                 TRUE ~ "No")) %>% 
  mutate(exposure_animal_waste =  case_when(grepl("Animal waste", regular_exposure_to_env_risk) ~ "Yes",
                                            TRUE ~ "No")) %>% 
  mutate(exposure_human_waste =  case_when(grepl("Human waste", regular_exposure_to_env_risk) ~ "Yes",
                                           TRUE ~ "No")) %>% 
  mutate(exposure_children =  case_when(grepl("Childcare facilities", regular_exposure_to_env_risk) ~ "Yes",
                                        TRUE ~ "No")) %>% 
  mutate(exposure_k12 =  case_when(grepl("K-12 schools", regular_exposure_to_env_risk) ~ "Yes",
                                   TRUE ~ "No")) %>% 
  mutate(exposure_correctional_facilities =  case_when(grepl("Correctional facilities", regular_exposure_to_env_risk) ~ "Yes",
                                                       TRUE ~ "No")) %>% 
  mutate(exposure_raw_meat =  case_when(grepl("Raw meat/poultry", regular_exposure_to_env_risk) ~ "Yes",
                                        TRUE ~ "No")) %>% 
  mutate(exposure_pesticides =  case_when(grepl("Pesticides/Herbicides", regular_exposure_to_env_risk) ~ "Yes",
                                          TRUE ~ "No")) %>% 
  mutate(exposure_vet_facilities =  case_when(grepl("Veterinary facilities", regular_exposure_to_env_risk) ~ "Yes",
                                              TRUE ~ "No")) 


#########################Healthcare Exposure###############################

data %<>% 
  mutate(healthcare_exposure_binary = case_when(healthcare_exposure == "No, I do not have regular exposure to healthcare environments" ~ "No",
                                                healthcare_exposure == NA ~ NA_character_,
                                                TRUE ~ "Yes")) %>%
  mutate(exposure_doctor_office =  case_when(grepl("Doctor", healthcare_exposure) ~ "Yes",
                                             TRUE ~ "No")) %>%
  mutate(exposure_hospital =  case_when(grepl("Hospital", healthcare_exposure) ~ "Yes",
                                        TRUE ~ "No")) %>%
  mutate(exposure_longterm_care =  case_when(grepl("Long term", healthcare_exposure) ~ "Yes",
                                             TRUE ~ "No")) %>%
  mutate(exposure_assisted_living =  case_when(grepl("Nursing home", healthcare_exposure) ~ "Yes",
                                               grepl("Assisted living", healthcare_exposure) ~ "Yes",
                                               TRUE ~ "No"))


##################################Water Exposure#################################################

data %<>% 
  mutate(treated_water_exposure_week_binary = case_when(treated_recreational_water_exposure_past_week > 0 ~ "Yes",
                                                        treated_recreational_water_exposure_past_week == 0 ~ "No",
                                                        TRUE ~ NA_character_)) %>%
  mutate(untreated_water_exposure_week_binary = case_when(untreated_recreational_water_exposure_past_week > 0 ~ "Yes",
                                                          untreated_recreational_water_exposure_past_week == 0 ~ "No",
                                                          TRUE ~ NA_character_)) %>% 
  mutate(treated_water_exposure_month_binary = case_when(treated_recreational_water_exposure_past_month > 0  ~ "Yes",
                                                         treated_recreational_water_exposure_past_month == 0 ~ "No",
                                                         TRUE ~ NA_character_)) %>%
  mutate(untreated_water_exposure_month_binary = case_when(untreated_recreational_water_exposure_past_month > 0 ~ "Yes",
                                                           untreated_recreational_water_exposure_past_month ==  0 ~ "No",
                                                           TRUE ~ NA_character_)) %>%
  mutate(any_water_exposure_week_binary = case_when(treated_water_exposure_week_binary == "Yes" ~ "Yes",
                                                    treated_water_exposure_week_binary == "No" ~ "No",
                                                    untreated_water_exposure_week_binary == "Yes" ~ "Yes",
                                                    untreated_water_exposure_week_binary == "No" ~ "No",
                                                    TRUE ~ NA_character_)) %>%
  mutate(any_water_exposure_month_binary = case_when(treated_water_exposure_month_binary == "Yes" ~ "Yes",
                                                     treated_water_exposure_month_binary == "No" ~ "No",
                                                     untreated_water_exposure_month_binary == "Yes" ~ "Yes",
                                                     untreated_water_exposure_month_binary == "No" ~ "No",
                                                     TRUE ~ NA_character_)) 

##################################Diet###########################################################


data %<>% 
  mutate(eat_poultry = case_when(eat_poultry_past_week == 0 ~ "No",
                                 eat_poultry_past_week > 0 ~ "Yes",
                                 TRUE ~ NA_character_)) %>%
  mutate(eat_dairy = case_when(eat_dairy_past_week == 0 ~ "No",
                               eat_dairy_past_week > 0 ~ "Yes",
                               TRUE ~ NA_character_)) %>%
  mutate(eat_red_meat = case_when(eat_pork_or_beef_past_week == 0 ~ "No",
                                  eat_pork_or_beef_past_week > 0 ~ "Yes",
                                  TRUE ~ NA_character_)) %>%
  mutate(eat_fish = case_when(eat_fish_or_shellfish_past_week == 0 ~ "No",
                              eat_fish_or_shellfish_past_week > 0 ~ "Yes",
                              TRUE ~ NA_character_)) %>%
  mutate(eat_fruit_veg = case_when(eat_raw_fruits_or_vegetables_past_week == 0 ~ "No",
                                   eat_raw_fruits_or_vegetables_past_week > 0 ~ "Yes",
                                   TRUE ~ NA_character_))


###############################Food Poisoning#########################################

data %<>%
  mutate(food_poisoning_binary =  case_when(grepl("Yes", food_poisioning_past_month) ~ "Yes",
                                            food_poisioning_past_month == NA ~ NA_character_,
                                            TRUE ~ "No"))

####################################UTI#############################################

data %<>%
  mutate(uti_binary =  case_when(grepl("Yes", uti_past_month) ~ "Yes",
                                 uti_past_month == NA ~ NA_character_,
                                 TRUE ~ "No"))
#N = 8
####################################Antibiotics######################################

data %<>%
  mutate(antibiotics_week_binary = case_when(grepl("Yes", any_antibiotic_past_week) ~ "Yes",
                                             any_antibiotic_past_week == NA ~ NA_character_,
                                             TRUE ~ "No")) %>%
  mutate(antibiotics_month_binary =  case_when(grepl("Yes", any_antibiotics_past_month) ~ "Yes",
                                               any_antibiotics_past_month == NA ~ NA_character_,
                                               TRUE ~ "No")) %>% 
  mutate(antibiotics_year_binary = case_when(grepl("Yes", multiple_antibiotics_past_year) ~ "Yes",
                                             multiple_antibiotics_past_year == NA ~ NA_character_,
                                             TRUE ~ "No"))

########################################MEDS###########################################

data %<>%
  mutate(prescription_meds = case_when(regular_perscription_meds_past_month != "NA" ~ "Yes",
                                       TRUE ~ "No")) %>%
  mutate(non_prescription_meds =  case_when(regular_non_perscription_meds_past_month != "NA" ~ "Yes",
                                            TRUE ~ "No")) %>%
  mutate(supplements =  case_when(regular_vitamins_or_supps_past_month != "NA" ~ "Yes",
                                  TRUE ~ "No"))


####################Collection Date########################################

#Separate date into Week,Month,Quarter
data %<>% 
  separate(recorded_date, into = c("date", "time"), sep = " ") %>% 
  mutate(date = as.Date(date)) %>%
  mutate(week = lubridate::floor_date(date, 'week')) %>%
  mutate(month = lubridate::floor_date(date, 'month')) %>%
  mutate(quarter = lubridate::quarter(date, with_year = T)) 

##########################################################################
###########################ANALYSIS#######################################
##########################################################################

observed_values = 
  data %>% 
  mutate_if(is.character, factor) %>% 
  mutate_if(is.numeric, factor) %>%
  #filter(group == "adult") %>%
  dplyr::select("Target", 
                "detection",
                "group",
                "age_bin",
                "biological_sex", 
                "race",
                "ethnicity",
                "antibiotics_week_binary", 
                "antibiotics_month_binary", 
                "antibiotics_year_binary", 
                "prescription_meds", 
                "non_prescription_meds", 
                "supplements", 
                "gi_condition_binary", 
                "gi_symptoms_48hr_binary",
                "gi_symptoms_month_binary",
                "food_poisoning_binary",
                "uti_binary",
                "healthcare_exposure_binary",
                "environmental_exposure_binary",
                "regular_animal_exposure", 
                "pets_binary", 
                "exposure_livestock_binary",
                "treated_water_exposure_week_binary",
                "untreated_water_exposure_week_binary",
                "treated_water_exposure_month_binary",
                "untreated_water_exposure_month_binary",
                "eat_poultry", 
                "eat_dairy", 
                "eat_red_meat", 
                "eat_fish", 
                "eat_fruit_veg", 
                "is_your_child_currently", 
                "first_four_months",
                "cats", 
                "dogs", 
                "birds", 
                "small_mammals", 
                "reptiles", 
                "pets_other", 
                "food_poisioning_past_month", 
                "eat_pork_or_beef_past_week", 
                "week", 
                "month", 
                "quarter") %>%
  mutate(detection = as.numeric(detection)) %>%
  mutate(detection = case_when(detection == "1" ~ 0, 
                               detection == "2" ~ 1, 
                               TRUE ~ detection)) %>%
  melt(na.rm = FALSE, value.name = "level", id = c("Target", "detection")) %>% 
  dplyr::group_by(variable, level, Target) %>% 
  dplyr::summarise(n = n(), 
                   n.pos = sum(detection)) %>% 
  mutate(percent.pos = n.pos/n, 
         n.neg = n-n.pos, 
         percent.neg = n.neg/n) %>%
  pivot_wider(names_from = "Target", values_from = c("n", "n.pos", "percent.pos", "n.neg", "percent.neg")) %>% 
  mutate(crAssphage_Positive = paste0(n.pos_crassphage, " (", round(percent.pos_crassphage*100), "%", ")")) %>% 
  mutate(crAssphage_Negative = paste0(n.neg_crassphage, " (", round(percent.neg_crassphage*100), "%", ")")) %>% 
  mutate(HF183_Positive = paste0(n.pos_HF183, " (", round(percent.pos_HF183*100), "%", ")")) %>% 
  mutate(HF183_Negative = paste0(n.neg_HF183, " (", round(percent.neg_HF183*100), "%", ")")) %>% 
  mutate(PMMoV_Positive = paste0(n.pos_pmmov, " (", round(percent.pos_pmmov*100), "%", ")")) %>% 
  mutate(PMMoV_Negative = paste0(n.neg_pmmov, " (", round(percent.neg_pmmov*100), "%", ")")) 


total.n.pos = data %>% 
  mutate_if(is.character, factor) %>% 
  mutate_if(is.numeric, factor) %>%
  #filter(group == "adult") %>%
  dplyr::select("Target", 
                "detection",
                "group",
                "age_bin",
                "biological_sex", 
                "race",
                "ethnicity",
                "antibiotics_week_binary", 
                "antibiotics_month_binary", 
                "antibiotics_year_binary", 
                "prescription_meds", 
                "non_prescription_meds", 
                "supplements", 
                "gi_condition_binary", 
                "gi_symptoms_48hr_binary",
                "gi_symptoms_month_binary",
                "food_poisoning_binary",
                "uti_binary",
                "healthcare_exposure_binary",
                "environmental_exposure_binary",
                "regular_animal_exposure", 
                "pets_binary", 
                "exposure_livestock_binary",
                "treated_water_exposure_week_binary",
                "untreated_water_exposure_week_binary",
                "treated_water_exposure_month_binary",
                "untreated_water_exposure_month_binary",
                "eat_poultry", 
                "eat_dairy", 
                "eat_red_meat", 
                "eat_fish", 
                "eat_fruit_veg", 
                "is_your_child_currently", 
                "first_four_months",
                "cats", 
                "dogs", 
                "birds", 
                "small_mammals", 
                "reptiles", 
                "pets_other", 
                "food_poisioning_past_month", 
                "eat_pork_or_beef_past_week", 
                "week", 
                "month", 
                "quarter") %>%
  mutate(detection = as.numeric(detection)) %>%
  mutate(detection = case_when(detection == "1" ~ 0, 
                               detection == "2" ~ 1, 
                               TRUE ~ detection)) %>%
  melt(na.rm = FALSE, value.name = "level", id = c("Target", "detection")) %>% 
  dplyr::group_by(variable, Target) %>% 
  dplyr::summarise(n.total = n(), 
                   n.pos.total = sum(detection)) %>% 
  mutate(n.neg.total = n.total-n.pos.total)



expected_values = 
  data %>% 
  mutate_if(is.character, factor) %>% 
  mutate_if(is.numeric, factor) %>%
  #filter(group == "adult") %>%
  dplyr::select("Target", 
                "detection",
                "group",
                "age_bin",
                "biological_sex", 
                "race",
                "ethnicity",
                "antibiotics_week_binary", 
                "antibiotics_month_binary", 
                "antibiotics_year_binary", 
                "prescription_meds", 
                "non_prescription_meds", 
                "supplements", 
                "gi_condition_binary", 
                "gi_symptoms_48hr_binary",
                "gi_symptoms_month_binary",
                "food_poisoning_binary",
                "uti_binary",
                "healthcare_exposure_binary",
                "environmental_exposure_binary",
                "regular_animal_exposure", 
                "pets_binary", 
                "exposure_livestock_binary",
                "treated_water_exposure_week_binary",
                "untreated_water_exposure_week_binary",
                "treated_water_exposure_month_binary",
                "untreated_water_exposure_month_binary",
                "eat_poultry", 
                "eat_dairy", 
                "eat_red_meat", 
                "eat_fish", 
                "eat_fruit_veg", 
                "is_your_child_currently", 
                "first_four_months",
                "cats", 
                "dogs", 
                "birds", 
                "small_mammals", 
                "reptiles", 
                "pets_other", 
                "food_poisioning_past_month", 
                "eat_pork_or_beef_past_week", 
                "week", 
                "month", 
                "quarter") %>%
  mutate(detection = as.numeric(detection)) %>%
  mutate(detection = case_when(detection == "1" ~ 0, 
                               detection == "2" ~ 1, 
                               TRUE ~ detection)) %>%
  melt(na.rm = FALSE, value.name = "level", id = c("Target", "detection")) %>% 
  dplyr::group_by(variable, level, Target) %>% 
  dplyr::summarise(n = n(), 
                   n.pos = sum(detection)) %>% 
  mutate(n.neg = n-n.pos) %>% 
  left_join(total.n.pos) %>% 
  mutate(n.pos.expected = round(n*n.pos.total/n.total), 
         n.neg.expected = round(n*n.neg.total/n.total),
         p.pos.expected = round((n.pos.expected/n)*100), 
         p.neg.expected = round((n.neg.expected/n)*100)) %>% 
  dplyr::select(variable, level, Target, n.pos.expected, n.neg.expected, p.pos.expected, p.neg.expected) %>%
  pivot_wider(names_from = "Target", values_from = c("n.pos.expected", "n.neg.expected", "p.pos.expected", "p.neg.expected")) %>% 
  mutate(crAssphage_Positive = paste0(n.pos.expected_crassphage, " (", p.pos.expected_crassphage, "%", ")")) %>% 
  mutate(crAssphage_Negative = paste0(n.neg.expected_crassphage, " (", p.neg.expected_crassphage, "%", ")")) %>% 
  mutate(HF183_Positive = paste0(n.pos.expected_HF183, " (", p.pos.expected_HF183, "%", ")")) %>% 
  mutate(HF183_Negative = paste0(n.neg.expected_HF183, " (", p.neg.expected_HF183, "%", ")")) %>% 
  mutate(PMMoV_Positive = paste0(n.pos.expected_pmmov, " (", p.pos.expected_pmmov, "%", ")")) %>% 
  mutate(PMMoV_Negative = paste0(n.neg.expected_pmmov, " (", p.neg.expected_pmmov, "%", ")")) 



################################################################################
################################Chi-Square#####################################
################################################################################

variables = 
  c("group",
    "age_bin",
    "biological_sex", 
    "race",
    "ethnicity",
    "antibiotics_week_binary", 
    "antibiotics_month_binary", 
    "antibiotics_year_binary", 
    "prescription_meds", 
    "non_prescription_meds", 
    "supplements", 
    "gi_condition_binary", 
    "gi_symptoms_48hr_binary",
    "gi_symptoms_month_binary",
    "food_poisoning_binary",
    "uti_binary",
    "healthcare_exposure_binary",
    "environmental_exposure_binary",
    "regular_animal_exposure", 
    "pets_binary", 
    "exposure_livestock_binary",
    "treated_water_exposure_week_binary",
    "untreated_water_exposure_week_binary",
    "treated_water_exposure_month_binary",
    "untreated_water_exposure_month_binary",
    "eat_poultry", 
    "eat_dairy", 
    "eat_red_meat", 
    "eat_fish", 
    "eat_fruit_veg")



################################crAssphage#######################################

chisq.crassphage <- matrix(NA, nrow = length(variables), ncol = 4)
crassphage = data %>% filter(Target == "crassphage") %>% mutate_if(is.character, factor) 


for(i in 1:length(variables)){
  chisq.crassphage[i,1] = variables[i]
  chisq.crassphage[i,2] = chisq.test(crassphage[,"detection"], unlist(crassphage[,variables[i]]))$p.value
  chisq.crassphage[i,3] = chisq.test(crassphage[,"detection"], unlist(crassphage[,variables[i]]), simulate.p.value = TRUE, B = 10000)$p.value 
  chisq.crassphage[i,4] = fisher.test(unlist(crassphage[,"detection"]), unlist(crassphage[,variables[i]]), simulate.p.value=TRUE)$p.value 
}

chisq.crassphage = as.data.frame(chisq.crassphage)
names(chisq.crassphage) = c("Variable", "Chi.Square", "Chi.Square_Monti.Carlo", "Fisher_Monti.Carlo")

chisq.crassphage = chisq.crassphage %>% 
  mutate(Fisher_Monti.Carlo = as.numeric(Fisher_Monti.Carlo)) %>%
  mutate(bonferonni.adjust.pvalue = p.adjust(Fisher_Monti.Carlo,method="bonferroni")) %>% 
  mutate(fdr.adjust.pvalue = p.adjust(Fisher_Monti.Carlo,method="fdr")) %>% 
  mutate(p.value.reported = paste0(round(Fisher_Monti.Carlo,2), "/", round(fdr.adjust.pvalue,2)))


#Child's Diet
fisher.test(crassphage$is_your_child_currently, crassphage$detection, simulate.p.value=TRUE)
fisher.test(crassphage$first_four_months, crassphage$detection, simulate.p.value=TRUE)



#Significant: food poisoning, pets

#Food Poisoning
crassphage_food.poisoning = table(crassphage$food_poisioning_past_month, crassphage$detection)
crassphage_food.poisoning = chisq.post.hoc(crassphage_food.poisoning)

#Pets
crassphage_pets_binary = table(crassphage$pets_binary, crassphage$detection)
crassphage_pets_binary = chisq.post.hoc(crassphage_pets_binary)

crassphage_dogs = table(crassphage$dogs, crassphage$detection)
crassphage_dogs = chisq.post.hoc(crassphage_dogs)

crassphage_cats = table(crassphage$cats, crassphage$detection)
crassphage_cats = chisq.post.hoc(crassphage_cats)

crassphage_birds = table(crassphage$birds, crassphage$detection)
crassphage_birds = chisq.post.hoc(crassphage_birds)

crassphage_small_mammals = table(crassphage$small_mammals, crassphage$detection)
crassphage_small_mammals = chisq.post.hoc(crassphage_small_mammals)

crassphage_reptiles = table(crassphage$reptiles, crassphage$detection)
crassphage_reptiles = chisq.post.hoc(crassphage_reptiles)

crassphage_pets_other = table(crassphage$pets_other, crassphage$detection)
crassphage_pets_other = chisq.post.hoc(crassphage_pets_other)

crassphage_pets = table(crassphage$pets, crassphage$detection)
crassphage_pets = chisq.post.hoc(crassphage_pets)

crassphage_posthoc = 
  bind_rows(
    crassphage_food.poisoning, 
    crassphage_pets_binary, 
    crassphage_pets,
    crassphage_dogs, 
    crassphage_cats, 
    crassphage_birds, 
    crassphage_small_mammals, 
    crassphage_reptiles,
    crassphage_pets_other, 
    crassphage_pets
  )

crassphage_posthoc = crassphage_posthoc %>% 
  mutate(fdr.adjust.pvalue = p.adjust(raw.p,method="fdr")) %>% 
  mutate(p.value.reported = paste0(round(raw.p,2), "/", round(fdr.adjust.pvalue,2)))


################################HF183#######################################

chisq.HF183 <- matrix(NA, nrow = length(variables), ncol = 4)
HF183 = data %>% filter(Target == "HF183") %>% mutate_if(is.character, factor) 


for(i in 1:length(variables)){
  chisq.HF183[i,1] = variables[i]
  chisq.HF183[i,2] = chisq.test(HF183[,"detection"], unlist(HF183[,variables[i]]))$p.value
  chisq.HF183[i,3] = chisq.test(HF183[,"detection"], unlist(HF183[,variables[i]]), simulate.p.value = TRUE, B = 10000)$p.value 
  chisq.HF183[i,4] = fisher.test(unlist(HF183[,"detection"]), unlist(HF183[,variables[i]]), simulate.p.value=TRUE)$p.value 
}

chisq.HF183 = as.data.frame(chisq.HF183)
names(chisq.HF183) = c("Variable", "Chi.Square", "Chi.Square_Monti.Carlo", "Fisher_Monti.Carlo")

chisq.HF183 = chisq.HF183 %>% 
  mutate(Fisher_Monti.Carlo = as.numeric(Fisher_Monti.Carlo)) %>%
  mutate(bonferonni.adjust.pvalue = p.adjust(Fisher_Monti.Carlo,method="bonferroni")) %>% 
  mutate(fdr.adjust.pvalue = p.adjust(Fisher_Monti.Carlo,method="fdr")) %>% 
  mutate(p.value.reported = paste0(round(Fisher_Monti.Carlo,2), "/", round(fdr.adjust.pvalue,2)))


#Child's Diet
#fisher.test(HF183$is_your_child_currently, HF183$detection, simulate.p.value=TRUE)
#fisher.test(HF183$first_four_months, HF183$detection, simulate.p.value=TRUE)


#Significant: eat red meat, any_antibiotic_past_week, ethnicity

HF183_red_meat = chisq.post.hoc(table(HF183$eat_red_meat, HF183$detection))
HF183_red_meat_count = chisq.post.hoc(table(HF183$eat_pork_or_beef_past_week, HF183$detection))


HF183_posthoc = bind_rows(HF183_red_meat, 
                          HF183_red_meat_count)


################################PMMoV#######################################

chisq.pmmov <- matrix(NA, nrow = length(variables), ncol = 4)
pmmov = data %>% 
  filter(Target == "pmmov")  %>%
  mutate_if(is.character, factor)


for(i in 1:length(variables)){
  chisq.pmmov[i,1] = variables[i]
  chisq.pmmov[i,2] = chisq.test(pmmov[,"detection"], unlist(pmmov[,variables[i]]))$p.value
  chisq.pmmov[i,3] = chisq.test(pmmov[,"detection"], unlist(pmmov[,variables[i]]), simulate.p.value = TRUE, B = 10000)$p.value 
  chisq.pmmov[i,4] = fisher.test(unlist(pmmov[,"detection"]), unlist(pmmov[,variables[i]]), simulate.p.value=TRUE)$p.value 
}

chisq.pmmov = as.data.frame(chisq.pmmov)
names(chisq.pmmov) = c("Variable", "Chi.Square", "Chi.Square_Monti.Carlo", "Fisher_Monti.Carlo")

chisq.pmmov = chisq.pmmov %>% 
  mutate(bonferonni.adjust.pvalue = p.adjust(Fisher_Monti.Carlo,method="bonferroni")) %>% 
  mutate(Fisher_Monti.Carlo = as.numeric(Fisher_Monti.Carlo)) %>%
  mutate(fdr.adjust.pvalue = p.adjust(Fisher_Monti.Carlo,method="fdr")) %>% 
  mutate(p.value.reported = paste0(round(Fisher_Monti.Carlo,2), "/", round(fdr.adjust.pvalue,2)))

write_csv(chisq.pmmov, "./data/processed_data/chisq.pmmov.csv")


#Child's Diet
#fisher.test(pmmov$is_your_child_currently, pmmov$detection, simulate.p.value=TRUE)
#fisher.test(pmmov$first_four_months, pmmov$detection, simulate.p.value=TRUE)


#Significant: Untreated water exposure, possibly


###################################################################################################################################
################################################CONCENTRATION######################################################################
###################################################################################################################################

#crAssphage & HF183

#crAssphage: >6.5
#HF183 7.5

data %<>% 
  mutate(log10_copies = log10(copies_gram_stool_ave)) %>%
  mutate(shed.level = case_when(Target == "crassphage" & log10_copies > 6.5 ~ "High",
                                Target == "crassphage" & log10_copies < 0 ~ NA_character_,
                                Target == "HF183" & log10_copies > 7.5 ~ "High", 
                                Target == "HF183" & log10_copies < 0 ~ NA_character_, 
                                Target == "pmmov" ~ NA_character_, 
                                TRUE ~ "Low")) 


#Are high shedders of crAssphage also high shedders of HF183?

#Is the detection of crAssphage independant of the detection of HF183 & PMMoV?
high_shedders = data %>% 
  mutate(shed.level = factor(shed.level)) %>%
  dplyr::select(Sample, Target, shed.level) %>% 
  pivot_wider(names_from = "Target", values_from = "shed.level")

fisher.test(high_shedders$crassphage, high_shedders$HF183, simulate.p.value=TRUE)


#Contingency Table

level.observed = data %>% 
  mutate_if(is.character, factor) %>% 
  filter(Target != "pmmov") %>% 
  dplyr::select("Target", 
                "shed.level",
                "group",
                "age_bin",
                "biological_sex", 
                "race",
                "ethnicity",
                "antibiotics_week_binary", 
                "antibiotics_month_binary", 
                "antibiotics_year_binary", 
                "prescription_meds", 
                "non_prescription_meds", 
                "supplements", 
                "gi_condition_binary", 
                "gi_symptoms_48hr_binary",
                "gi_symptoms_month_binary",
                "food_poisoning_binary",
                "uti_binary",
                "healthcare_exposure_binary",
                "environmental_exposure_binary",
                "regular_animal_exposure", 
                "pets_binary", 
                "exposure_livestock_binary",
                "treated_water_exposure_week_binary",
                "untreated_water_exposure_week_binary",
                "treated_water_exposure_month_binary",
                "untreated_water_exposure_month_binary",
                "eat_poultry", 
                "eat_dairy", 
                "eat_red_meat", 
                "eat_fish", 
                "eat_fruit_veg", 
                "is_your_child_currently", 
                "first_four_months",
                "cats", 
                "dogs", 
                "birds", 
                "small_mammals", 
                "reptiles", 
                "pets_other", 
                "week", 
                "month", 
                "quarter") %>%
  melt(na.rm = FALSE, value.name = "level", id = c("Target", "shed.level")) %>% 
  dplyr::group_by(variable, level, Target, shed.level) %>% 
  dplyr::summarize(n = n()) %>% 
  pivot_wider(names_from = "shed.level", values_from = c("n")) %>%
  mutate(n.High.observed = replace_na(High, 0)) %>%
  mutate(n.Low.observed = replace_na(Low, 0)) %>% 
  mutate(p.High.observed = round((n.High.observed/(n.High.observed + n.Low.observed))*100), 
         p.Low.observed = round((n.Low.observed/(n.High.observed + n.Low.observed))*100)) %>% 
  mutate(High_Observed = paste0(n.High.observed, " (", p.High.observed, "%", ")")) %>% 
  mutate(Low_Observed = paste0(n.Low.observed, " (", p.Low.observed, "%", ")"))  


level.total = data %>% 
  mutate_if(is.character, factor) %>% 
  mutate_if(is.numeric, factor) %>%
  filter(Target != "pmmov") %>%
  dplyr::select("Target", 
                "shed.level",
                "group",
                "age_bin",
                "biological_sex", 
                "race",
                "ethnicity",
                "antibiotics_week_binary", 
                "antibiotics_month_binary", 
                "antibiotics_year_binary", 
                "prescription_meds", 
                "non_prescription_meds", 
                "supplements", 
                "gi_condition_binary", 
                "gi_symptoms_48hr_binary",
                "gi_symptoms_month_binary",
                "food_poisoning_binary",
                "uti_binary",
                "healthcare_exposure_binary",
                "environmental_exposure_binary",
                "regular_animal_exposure", 
                "pets_binary", 
                "exposure_livestock_binary",
                "treated_water_exposure_week_binary",
                "untreated_water_exposure_week_binary",
                "treated_water_exposure_month_binary",
                "untreated_water_exposure_month_binary",
                "eat_poultry", 
                "eat_dairy", 
                "eat_red_meat", 
                "eat_fish", 
                "eat_fruit_veg", 
                "is_your_child_currently", 
                "first_four_months",
                "cats", 
                "dogs", 
                "birds", 
                "small_mammals", 
                "reptiles", 
                "pets_other", 
                "food_poisioning_past_month", 
                "eat_pork_or_beef_past_week", 
                "week", 
                "month", 
                "quarter") %>%
  melt(na.rm = FALSE, value.name = "level", id = c("Target", "shed.level")) %>% 
  dplyr::group_by(variable, Target, shed.level) %>% 
  dplyr::summarise(n.total = n()) %>% 
  pivot_wider(names_from = "shed.level", values_from = c("n.total")) %>% 
  mutate(n.total = High + Low)

level.expected = 
  data %>% 
  mutate_if(is.character, factor) %>% 
  mutate_if(is.numeric, factor) %>%
  filter(Target != "pmmov") %>%
  dplyr::select("Target", 
                "shed.level",
                "group",
                "age_bin",
                "biological_sex", 
                "race",
                "ethnicity",
                "antibiotics_week_binary", 
                "antibiotics_month_binary", 
                "antibiotics_year_binary", 
                "prescription_meds", 
                "non_prescription_meds", 
                "supplements", 
                "gi_condition_binary", 
                "gi_symptoms_48hr_binary",
                "gi_symptoms_month_binary",
                "food_poisoning_binary",
                "uti_binary",
                "healthcare_exposure_binary",
                "environmental_exposure_binary",
                "regular_animal_exposure", 
                "pets_binary", 
                "exposure_livestock_binary",
                "treated_water_exposure_week_binary",
                "untreated_water_exposure_week_binary",
                "treated_water_exposure_month_binary",
                "untreated_water_exposure_month_binary",
                "eat_poultry", 
                "eat_dairy", 
                "eat_red_meat", 
                "eat_fish", 
                "eat_fruit_veg", 
                "is_your_child_currently", 
                "first_four_months",
                "cats", 
                "dogs", 
                "birds", 
                "small_mammals", 
                "reptiles", 
                "pets_other", 
                "food_poisioning_past_month", 
                "eat_pork_or_beef_past_week", 
                "week", 
                "month", 
                "quarter") %>%
  melt(na.rm = FALSE, value.name = "level", id = c("Target", "shed.level")) %>% 
  dplyr::group_by(variable, level, Target) %>% 
  filter(shed.level != "NA") %>%
  dplyr::summarise(n.group = n()) %>% 
  left_join(level.total)   %>% 
  mutate(n.High.expected = round(n.group*High/n.total),
         n.Low.expected = round(n.group*Low/n.total)) %>%
  mutate(p.High.expected = round((n.High.expected/n.group)*100), 
         p.Low.expected = round((n.Low.expected/n.group)*100)) %>%
  mutate(High_Expected = paste0(n.High.expected, " (", p.High.expected, "%", ")")) %>% 
  mutate(Low_Expected = paste0(n.Low.expected, " (", p.Low.expected, "%", ")"))





####Analysis
chisq.crassphage.level <- matrix(NA, nrow = length(variables), ncol = 4)
crassphage = data %>% filter(Target == "crassphage")  %>% mutate_if(is.character, factor) 


variables = c("group",
              "age_bin",
              "biological_sex", 
              "race",
              "ethnicity",
              "antibiotics_week_binary", 
              "antibiotics_month_binary", 
              "antibiotics_year_binary", 
              "prescription_meds", 
              "non_prescription_meds", 
              "supplements", 
              "gi_condition_binary", 
              "gi_symptoms_48hr_binary",
              "gi_symptoms_month_binary",
              #"food_poisoning_binary",
              "uti_binary",
              "healthcare_exposure_binary",
              "environmental_exposure_binary",
              "regular_animal_exposure", 
              "pets_binary", 
              "exposure_livestock_binary",
              "treated_water_exposure_week_binary",
              "untreated_water_exposure_week_binary",
              "treated_water_exposure_month_binary",
              "untreated_water_exposure_month_binary",
              "eat_poultry", 
              "eat_dairy", 
              "eat_red_meat", 
              "eat_fish", 
              "eat_fruit_veg")



for(i in 1:length(variables)){
  chisq.crassphage.level[i,1] = variables[i]
  chisq.crassphage.level[i,2] = chisq.test(crassphage[,"shed.level"], unlist(crassphage[,variables[i]]))$p.value
  chisq.crassphage.level[i,3] = chisq.test(crassphage[,"shed.level"], unlist(crassphage[,variables[i]]), simulate.p.value = TRUE, B = 10000)$p.value 
  chisq.crassphage.level[i,4] = fisher.test(unlist(crassphage[,"shed.level"]), unlist(crassphage[,variables[i]]), simulate.p.value=TRUE)$p.value 
}

chisq.crassphage.level = as.data.frame(chisq.crassphage.level)
names(chisq.crassphage.level) = c("Variable", "Chi.Square", "Chi.Square_Monti.Carlo", "Fisher_Monti.Carlo")

chisq.crassphage.level = chisq.crassphage.level %>% 
  mutate(Fisher_Monti.Carlo = as.numeric(Fisher_Monti.Carlo)) %>%
  mutate(bonferonni.adjust.pvalue = p.adjust(Fisher_Monti.Carlo,method="bonferroni")) %>% 
  mutate(fdr.adjust.pvalue = p.adjust(Fisher_Monti.Carlo,method="fdr")) %>% 
  mutate(p.value.reported = paste0(round(Fisher_Monti.Carlo,2), "/", round(fdr.adjust.pvalue,2)))


crassphage.level_cats = chisq.post.hoc(table(crassphage$shed.level, crassphage$cats))
crassphage.level_dogs = chisq.post.hoc(table(crassphage$shed.level, crassphage$dogs))
crassphage.level_birds = chisq.post.hoc(table(crassphage$shed.level, crassphage$birds))
crassphage.level_small.mammals = chisq.post.hoc(table(crassphage$shed.level, crassphage$small_mammals))
crassphage.level_reptiles = chisq.post.hoc(table(crassphage$shed.level, crassphage$reptiles))
crassphage.level_other = chisq.post.hoc(table(crassphage$shed.level, crassphage$pets_other))

crassphage.level.pets.posthoc = bind_rows(crassphage.level_cats, 
                                          crassphage.level_dogs, 
                                          crassphage.level_birds,
                                          crassphage.level_small.mammals, 
                                          crassphage.level_reptiles, 
                                          crassphage.level_other)


crassphage.level.pets.posthoc = crassphage.level.pets.posthoc %>% 
  mutate(raw.p = as.numeric(raw.p)) %>%
  mutate(fdr.adjust.pvalue = p.adjust(raw.p,method="fdr")) %>% 
  mutate(p.value.reported = paste0(round(raw.p,2), "/", round(fdr.adjust.pvalue,2)))


crassphage_child.current.diet = chisq.post.hoc(table(crassphage$is_your_child_currently, crassphage$shed.level))
crassphage_child.current.first.four = chisq.post.hoc(table(crassphage$first_four_months, crassphage$shed.level))


#############################HF183###################################################################


variables = c("group",
              "age_bin",
              "biological_sex", 
              "race",
              "ethnicity",
              "antibiotics_week_binary", 
              "antibiotics_month_binary", 
              "antibiotics_year_binary", 
              "prescription_meds", 
              "non_prescription_meds", 
              "supplements", 
              "gi_condition_binary", 
              "gi_symptoms_48hr_binary",
              "gi_symptoms_month_binary",
              "food_poisoning_binary",
              "uti_binary",
              "healthcare_exposure_binary",
              "environmental_exposure_binary",
              "regular_animal_exposure", 
              "pets_binary", 
              "exposure_livestock_binary",
              "treated_water_exposure_week_binary",
              "untreated_water_exposure_week_binary",
              "treated_water_exposure_month_binary",
              "untreated_water_exposure_month_binary",
              "eat_poultry", 
              "eat_dairy", 
              "eat_red_meat", 
              "eat_fish", 
              "eat_fruit_veg")

chisq.HF183.level <- matrix(NA, nrow = length(variables), ncol = 4)
HF183 = data %>% filter(Target == "HF183")  %>% mutate_if(is.character, factor) 

for(i in 1:length(variables)){
  chisq.HF183.level[i,1] = variables[i]
  chisq.HF183.level[i,2] = chisq.test(HF183[,"shed.level"], unlist(HF183[,variables[i]]))$p.value
  chisq.HF183.level[i,3] = chisq.test(HF183[,"shed.level"], unlist(HF183[,variables[i]]), simulate.p.value = TRUE, B = 10000)$p.value 
  chisq.HF183.level[i,4] = fisher.test(unlist(HF183[,"shed.level"]), unlist(HF183[,variables[i]]), simulate.p.value=TRUE)$p.value 
}

chisq.HF183.level = as.data.frame(chisq.HF183.level)
names(chisq.HF183.level) = c("Variable", "Chi.Square", "Chi.Square_Monti.Carlo", "Fisher_Monti.Carlo")


chisq.HF183.level = chisq.HF183.level %>% 
  mutate(Fisher_Monti.Carlo = as.numeric(Fisher_Monti.Carlo)) %>%
  mutate(bonferonni.adjust.pvalue = p.adjust(Fisher_Monti.Carlo,method="bonferroni")) %>% 
  mutate(fdr.adjust.pvalue = p.adjust(Fisher_Monti.Carlo,method="fdr")) %>% 
  mutate(p.value.reported = paste0(round(Fisher_Monti.Carlo,2), "/", round(fdr.adjust.pvalue,2)))


HF183_child.current.diet = chisq.post.hoc(table(HF183$is_your_child_currently, HF183$shed.level))
HF183_child.current.first.four = chisq.post.hoc(table(HF183$first_four_months, HF183$shed.level))


#############################HF183 LEVEL POSTHOC##########################
HF183 = data %>% filter(Target == "HF183") 
#Update variables 
HF183 %<>%
  mutate(untreated_recreational_water_exposure_past_week = as.factor(untreated_recreational_water_exposure_past_week)) %>%
  mutate(untreated_recreational_water_head_submerge = case_when(untreated_recreational_water_head_submerge == "My child was not exposed to untreated water sources" ~ NA_character_, 
                                                                untreated_recreational_water_head_submerge == "I was not exposed to untreated recreational water" ~ NA_character_,
                                                                TRUE ~ untreated_recreational_water_head_submerge)) %>% 
  mutate(untreated_recreational_water_sources = case_when(untreated_recreational_water_sources == "My child was not exposed to untreated water sources" ~ NA_character_, 
                                                          untreated_recreational_water_sources == "I was not exposed to untreated recreational water" ~ NA_character_,
                                                          TRUE ~ untreated_recreational_water_sources)) %>%
  mutate(untreated_surface_water_contact = case_when(untreated_surface_water_contact == "My child was not exposed to untreated recreational water" ~ NA_character_, 
                                                     untreated_surface_water_contact == "I was not exposed to untreated recreational water" ~ NA_character_,
                                                     TRUE ~ untreated_surface_water_contact))


untreated_recreational_water_sources = HF183 %>% 
  dplyr::select(shed.level, untreated_recreational_water_sources) %>% 
  #dplyr::mutate(untreated_recreational_water_sources = stringr::str_replace_all(untreated_recreational_water_sources, " ", "")) %>% 
  separate(untreated_recreational_water_sources, into = c("1", "2", "3"), sep = ",") %>%  
  melt(na.rm = FALSE, id = 'shed.level') 



untreated_surface_water_contact = HF183 %>% 
  dplyr::select(shed.level, untreated_surface_water_contact) %>% 
  dplyr::mutate(untreated_surface_water_contact = stringr::str_replace(untreated_surface_water_contact, " ", "")) %>% 
  dplyr::mutate(untreated_surface_water_contact = stringr::str_replace(untreated_surface_water_contact, " ", "")) %>% 
  dplyr::mutate(untreated_surface_water_contact = stringr::str_replace(untreated_surface_water_contact, " ", "")) %>% 
  dplyr::mutate(untreated_surface_water_contact = stringr::str_replace(untreated_surface_water_contact, " ", "")) %>% 
  dplyr::mutate(untreated_surface_water_contact = stringr::str_replace(untreated_surface_water_contact, "Boating,paddling", "Boating")) %>% 
  dplyr::mutate(untreated_surface_water_contact = stringr::str_replace(untreated_surface_water_contact, "Water-sports (Surfing, Waterskiing, etc.)", "WaterSports")) %>% 
  dplyr::mutate(untreated_surface_water_contact = stringr::str_replace(untreated_surface_water_contact, "Wading,Fishing", "Fishing")) %>% 
  separate(untreated_surface_water_contact, into = c("1", "2", "3", "4"), sep = ",") %>%  
  melt(na.rm = FALSE, id = 'shed.level') %>% 
  filter(value != "Waterskiing") %>% 
  filter(value != "etc.)")

#Save Values


#UNTREATED WATER EXPOSURE PAST WEEK
HF183_untreated.water.week = chisq.post.hoc(table(HF183$untreated_recreational_water_exposure_past_week, HF183$shed.level))
HF183_untreated_recreational_water_head_submerge = chisq.post.hoc(table(HF183$untreated_recreational_water_head_submerge, HF183$shed.level))
HF183_untreated_recreational_water_sources = chisq.post.hoc(table(untreated_recreational_water_sources$value, untreated_recreational_water_sources$shed.level))
HF183_untreated_surface_water_contact = chisq.post.hoc(table(untreated_surface_water_contact$value, untreated_surface_water_contact$shed.level))

HF183.level.untreated.water.week.posthoc = bind_rows(HF183_untreated.water.week, 
                                                     HF183_untreated_recreational_water_head_submerge, 
                                                     HF183_untreated_recreational_water_sources, 
                                                     HF183_untreated_surface_water_contact)


HF183.level.untreated.water.week.posthoc = HF183.level.untreated.water.week.posthoc %>% 
  mutate(raw.p = as.numeric(raw.p)) %>%
  mutate(fdr.adjust.pvalue = p.adjust(raw.p,method="fdr")) %>% 
  mutate(p.value.reported = paste0(round(raw.p,2), "/", round(fdr.adjust.pvalue,2)))


###########################################################################################################################
###################################################PMMOV###################################################################


#Summarize Mean & SD of PMMoV by Group


variables = c(#"group",
  "age_bin",
  "biological_sex", 
  "race",
  "ethnicity",
  "antibiotics_week_binary", 
  "antibiotics_month_binary", 
  "antibiotics_year_binary", 
  "prescription_meds", 
  "non_prescription_meds", 
  "supplements", 
  "gi_condition_binary", 
  "gi_symptoms_48hr_binary",
  "gi_symptoms_month_binary",
  "food_poisoning_binary",
  "uti_binary",
  "healthcare_exposure_binary",
  "environmental_exposure_binary",
  "regular_animal_exposure", 
  "pets_binary", 
  "exposure_livestock_binary",
  "treated_water_exposure_week_binary",
  "untreated_water_exposure_week_binary",
  "treated_water_exposure_month_binary",
  "untreated_water_exposure_month_binary",
  "eat_poultry", 
  "eat_dairy", 
  "eat_red_meat", 
  "eat_fish", 
  "eat_fruit_veg")

pmmov.concentration = data %>% 
  mutate_if(is.character, factor) %>% 
  filter(copies_gram_stool_ave > 1) %>%
  mutate(log10_copies = log10(copies_gram_stool_ave)) %>%
  filter(group == "adult") %>%
  dplyr::select("Target", 
                "log10_copies",
                "age_bin",
                "biological_sex", 
                "race",
                "ethnicity",
                "antibiotics_week_binary", 
                "antibiotics_month_binary", 
                "antibiotics_year_binary", 
                "prescription_meds", 
                "non_prescription_meds", 
                "supplements", 
                "gi_condition_binary", 
                "gi_symptoms_48hr_binary",
                "gi_symptoms_month_binary",
                "food_poisoning_binary",
                "uti_binary",
                "healthcare_exposure_binary",
                "environmental_exposure_binary",
                "regular_animal_exposure", 
                "pets_binary", 
                "exposure_livestock_binary",
                "treated_water_exposure_week_binary",
                "untreated_water_exposure_week_binary",
                "treated_water_exposure_month_binary",
                "untreated_water_exposure_month_binary",
                "eat_poultry", 
                "eat_dairy", 
                "eat_red_meat", 
                "eat_fish", 
                "eat_fruit_veg", 
                "is_your_child_currently", 
                "first_four_months", 
                "week", 
                "month", 
                "quarter") %>%
  melt(na.rm = FALSE, value.name = "level", id = c("Target", "log10_copies")) %>% 
  dplyr::group_by(variable, level, Target) %>% 
  dplyr::summarize( n = n(), 
                    med_copies = median(log10_copies),
                    mean_copies = mean(log10_copies), 
                    sd_copies = sd(log10_copies)) %>% 
  filter(Target == "pmmov") %>% 
  mutate(shed.level = paste0(round(mean_copies, 2), " (", round(sd_copies,2), ")"))


pmmov = data %>% 
  filter(Target == "pmmov") %>% 
  filter(copies_gram_stool_ave > 1) %>%
  mutate(log10_copies = log10(copies_gram_stool_ave)) %>% 
  filter(group == "adult")


#Test for normality 
shapiro.test(pmmov$log10_copies)
#p-value = 0.5138, compare means using aov 


#AOV

aov.pmmov <- matrix(NA, nrow = length(variables), ncol = 3)

for(i in 1:length(variables)){
  aov.pmmov[i,1] = variables[i]
  aov.pmmov[i,2] = kruskal.test(unlist(pmmov[,"log10_copies"]) ~ unlist(pmmov[,variables[i]]))$p.value
  aov.pmmov[i,3] = unlist(summary(aov(unlist(pmmov[,"log10_copies"]) ~ unlist(pmmov[,variables[i]]), na.action=na.omit)))["Pr(>F)1"]
}


aov.pmmov = as.data.frame(aov.pmmov)
names(aov.pmmov) = c("Variable", "kruskall.p.value", "aov.p.value")
aov.pmmov %<>% 
  mutate(aov.p.value = as.numeric(aov.p.value)) %>%
  mutate(fdr.aov.p.value = p.adjust(aov.p.value, method="fdr")) %>% 
  mutate(p.value.reported = paste0(round(aov.p.value,2),"/",round(fdr.aov.p.value,2)))



############################PMMoV Post-Hoc####################################

#Update variables 
pmmov %<>%
  mutate(treated_recreational_water_exposure_past_month = as.factor(treated_recreational_water_exposure_past_month)) %>%
  mutate(treated_recreational_water_head_submerge = case_when(treated_recreational_water_head_submerge == "My child was not exposed to treated recreational water" ~ NA_character_, 
                                                              treated_recreational_water_head_submerge == "I was not exposed to treated recreational water" ~ NA_character_,
                                                              TRUE ~ treated_recreational_water_head_submerge)) %>% 
  mutate(treated_recreational_water_sources = case_when(treated_recreational_water_sources == "My child was not exposed to treated recreational water" ~ NA_character_, 
                                                        treated_recreational_water_sources == "I was not exposed to treated recreational water" ~ NA_character_,
                                                        TRUE ~ treated_recreational_water_sources)) %>% 
  mutate(symptoms_past_3_to_30_days = case_when(symptoms_past_3_to_30_days == "My child has not had any of the above symptoms" ~ "No", 
                                                symptoms_past_3_to_30_days == "I have not had any of the above symptoms" ~ "No",  
                                                TRUE ~ symptoms_past_3_to_30_days)) %>% 
  mutate(regular_exposure_to_env_risk = case_when(regular_exposure_to_env_risk == "My child does not have regular exposure to any of these choices" ~ "No", 
                                                  regular_exposure_to_env_risk == "I do not have regular exposure to any of these choices" ~ "No",  
                                                  TRUE ~ regular_exposure_to_env_risk))


#Save Values
pmmov.post.hoc.concentration = pmmov  %>% 
  mutate_if(is.character, factor) %>% 
  filter(group == "adult") %>%
  dplyr::select("Target", 
                "log10_copies",
                "treated_water_exposure_month_binary",
                "treated_recreational_water_exposure_past_month",
                "treated_recreational_water_head_submerge",
                "treated_recreational_water_sources", 
                "symptoms_past_3_to_30_days", 
                "regular_exposure_to_env_risk") %>%
  melt(na.rm = FALSE, value.name = "level", id = c("Target", "log10_copies")) %>% 
  dplyr::group_by(variable, level, Target) %>% 
  dplyr::summarize(n = n(),
                   med_copies = median(log10_copies),
                   mean_copies = mean(log10_copies), 
                   sd_copies = sd(log10_copies)) %>% 
  filter(Target == "pmmov") %>% 
  mutate(shed.level = paste0(round(mean_copies, 2), " (", round(sd_copies,2), ")"))


#TREATED WATER EXPOSURE

treated_water_exposure_month_binary.aov <- aov(log10_copies~treated_water_exposure_month_binary, data=pmmov)
summary(treated_water_exposure_month_binary.aov)

treated_recreational_water_exposure_past_month.aov <- aov(log10_copies~treated_recreational_water_exposure_past_month, data=pmmov)
summary(treated_recreational_water_exposure_past_month.aov)

treated_recreational_water_head_submerge.aov <- aov(log10_copies~treated_recreational_water_head_submerge, data=pmmov)
summary(treated_recreational_water_head_submerge.aov)

treated_recreational_water_sources.aov <- aov(log10_copies~treated_recreational_water_sources, data=pmmov)
summary(treated_recreational_water_sources.aov)

#AGE BIN
age_bin.aov <- aov(log10_copies~age_bin, data=pmmov)
TukeyHSD(age_bin.aov)


#GI SYMPTOMS PAST MONTH
gi_month.aov <- aov(log10_copies~symptoms_past_3_to_30_days, data=pmmov)
summary(gi_month.aov)
TukeyHSD(gi_month.aov)



#ENVIRONMENTAL EXPOSURE 
environmental_exposure.aov <- aov(log10_copies~regular_exposure_to_env_risk, data=pmmov)
summary(environmental_exposure.aov)



####################################################################################
#################################CHILDREN########################################
####################################################################################


crassphage.child.detection = 
  data %>% 
  filter(group == "child") %>%
  filter(Target == "crassphage")%>% 
  ggbarstats(
    y = numeric_age, 
    x = detection, 
    xlab = "Numeric Age", 
    legend.title = "Detection"
  )


crassphage.child.titer = 
  data %>% 
  filter(group == "child") %>%
  filter(Target == "crassphage")%>% 
  ggbarstats(
    y = numeric_age, 
    x = shed.level, 
    xlab = "Numeric Age", 
    legend.title = "Titer"
  )

crassphage.child.plot = 
  ggarrange(
    crassphage.child.detection,
    crassphage.child.titer,
    ncol  = 1, 
    labels = c("A", "B"))


plot(crassphage.child.plot)


HF183.child.detection = 
  data %>% 
  filter(group == "child") %>%
  filter(Target == "HF183")%>% 
  ggbarstats(
    y = numeric_age, 
    x = detection, 
    xlab = "Numeric Age", 
    legend.title = "Detection"
  )


HF183.child.titer = 
  data %>% 
  filter(group == "child") %>%
  filter(Target == "HF183")%>% 
  ggbarstats(
    y = numeric_age, 
    x = shed.level, 
    xlab = "Numeric Age", 
    legend.title = "Shed Titer"
  )

HF183.child.plot = 
  ggarrange(
    HF183.child.detection,
    HF183.child.titer,
    ncol  = 1, 
    labels = c("A", "B"))


plot(HF183.child.plot)



pmmov.child.detection = 
  data %>% 
  filter(group == "child") %>%
  filter(Target == "pmmov")%>% 
  ggbarstats(
    y = numeric_age, 
    x = detection, 
    xlab = "Numeric Age", 
    legend.title = "Detection"
  )


pmmov.child.titer = 
  data %>% 
  filter(group == "child") %>%
  filter(Target == "pmmov")%>% 
  ggbetweenstats(
    x = numeric_age, 
    y = log10_copies
  )

pmmov.child.plot = 
  ggarrange(
    pmmov.child.detection,
    pmmov.child.titer,
    ncol  = 1, 
    labels = c("A", "B"))


plot(pmmov.child.plot)




#####################################################################
#########################SEASONALITY#################################
#####################################################################

data %<>% 
  separate(recorded_date, into = c("date", "time"), sep = " ") %>% 
  mutate(date = as.Date(date)) %>%
  mutate(week = lubridate::floor_date(date, 'week')) %>%
  mutate(month = lubridate::floor_date(date, 'month')) %>%
  mutate(quarter = lubridate::quarter(date, with_year = T)) 

data %<>%  
  mutate(log10_copies = log10(copies_gram_stool_ave)) %>%
  mutate(shed.level = case_when(Target == "crassphage" 
                                & log10_copies > 6.5 ~"High",
                                Target == "crassphage" 
                                & log10_copies < 0 ~ NA_character_,
                                Target == "HF183" 
                                & log10_copies > 7.5 ~ "High", 
                                Target == "HF183" 
                                & log10_copies < 0 ~ NA_character_, 
                                Target == "pmmov" ~ NA_character_, 
                                TRUE ~ "Low")) 


crassphage.seasonality = data %>% filter(Target == "crassphage")
HF183.seasonality = data %>% filter(Target == "HF183")
pmmov.seasonality = data %>% filter(Target == "pmmov")


#Detection by Week

crassphage.seasonality %>% 
  ggbarstats(
    y = week, 
    x = detection, 
    xlab = "Week", 
    title = "crAssphage"
  )

HF183.seasonality %>% 
  ggbarstats(
    y = week, 
    x = detection, 
    xlab = "Week", 
    title = "HF183"
  )

pmmov.seasonality %>% 
  ggbarstats(
    y = week, 
    x = detection, 
    xlab = "Week", 
    title = "PMMoV"
  )


# Detection by month
crassphage.seasonality %>% 
  ggbarstats(
    y = month, 
    x = detection, 
    xlab = "Month", 
    title = "crAssphage"
  )

HF183.seasonality %>% 
  ggbarstats(
    y = month, 
    x = detection, 
    xlab = "Month", 
    title = "HF183"
  )

pmmov.seasonality %>% 
  ggbarstats(
    y = month, 
    x = detection, 
    xlab = "Month", 
    title = "PMMoV"
  )

#Detection by quarter
crassphage.seasonality %>% 
  ggbarstats(
    y = quarter, 
    x = detection, 
    xlab = "Quarter", 
    title = "crAssphage"
  )

HF183.seasonality %>% 
  ggbarstats(
    y = quarter, 
    x = detection, 
    xlab = "Quarter", 
    ylab = "HF183"
  )

pmmov.seasonality %>% 
  ggbarstats(
    y = quarter, 
    x = detection, 
    xlab = "Quarter", 
    title= "PMMoV"
  )



#Titer by Week
crassphage.seasonality %>% 
  ggbarstats(
    y = week, 
    x = shed.level, 
    xlab = "Week", 
    title = "crAssphage"
  )

HF183.seasonality %>% 
  ggbarstats(
    y = week, 
    x = shed.level, 
    xlab = "Week", 
    ylab = "HF183"
  )

pmmov.seasonality %<>% 
  filter(copies_gram_stool_ave > 1)

pmmov.seasonality %>%
  ggbetweenstats(
    y = log10_copies, 
    x = week,
    xlab = "Week", 
    title= "PMMoV", 
  )


#Titer by month
crassphage.seasonality %>% 
  ggbarstats(
    y = month, 
    x = shed.level, 
    xlab = "Month", 
    title = "crAssphage"
  )

HF183.seasonality %>% 
  ggbarstats(
    y = month, 
    x = shed.level, 
    xlab = "Month", 
    ylab = "HF183"
  )

pmmov.seasonality %>%
  ggbetweenstats(
    y = log10_copies, 
    x = month,
    xlab = "Month", 
    title= "PMMoV", 
  )


#Titer by Quarter
crassphage.seasonality %>% 
  ggbarstats(
    y = quarter, 
    x = shed.level, 
    xlab = "Quarter", 
    title = "crAssphage"
  )

HF183.seasonality %>% 
  ggbarstats(
    y = quarter, 
    x = shed.level, 
    xlab = "Quarter", 
    ylab = "HF183"
  )

pmmov.seasonality %>%
  ggbetweenstats(
    y = log10_copies, 
    x = quarter,
    xlab = "Quarter", 
    title= "PMMoV", 
  )


#Is detection different by week?
fisher.test(crassphage.seasonality$week, crassphage.seasonality$detection, simulate.p.value = TRUE)
fisher.test(HF183.seasonality$week, HF183.seasonality$detection, simulate.p.value = TRUE)

#Is detection different by month?
fisher.test(crassphage.seasonality$month, crassphage.seasonality$detection, simulate.p.value = TRUE)
fisher.test(HF183.seasonality$month, HF183.seasonality$detection, simulate.p.value = TRUE)

chisq.post.hoc(table(crassphage.seasonality$month, crassphage.seasonality$detection))

#Is detection different by quarter?
fisher.test(crassphage.seasonality$quarter, crassphage.seasonality$detection, simulate.p.value = TRUE)
fisher.test(HF183.seasonality$quarter, HF183.seasonality$detection, simulate.p.value = TRUE)

#Is PMMoV normally distributed with equal variances?
bartlett.test(log10_copies ~ quarter, data = pmmov.seasonality)

#Differences by week?
fisher.test(crassphage.seasonality$week, crassphage.seasonality$shed.level, simulate.p.value = TRUE)
fisher.test(HF183.seasonality$week, HF183.seasonality$shed.level, simulate.p.value = TRUE)
summary(aov(data = pmmov.seasonality,log10_copies~week))

#Differences by month?
fisher.test(crassphage.seasonality$month, crassphage.seasonality$shed.level, simulate.p.value = TRUE)
fisher.test(HF183.seasonality$month, HF183.seasonality$shed.level, simulate.p.value = TRUE)
summary(aov(data = pmmov.seasonality,month~log10_copies))

#Differences by quarter? 
fisher.test(crassphage.seasonality$quarter, crassphage.seasonality$shed.level, simulate.p.value = TRUE)
fisher.test(HF183.seasonality$quarter, HF183.seasonality$shed.level, simulate.p.value = TRUE)
summary(aov(data = pmmov.seasonality,quarter~log10_copies))