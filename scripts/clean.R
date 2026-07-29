######################### LOAD PACKAGES
library(dplyr)    # data wrangling
library(stringr)  # string wrangling


######################### DATA
# qualtrics
qualtrics = read.csv('data/data-qualtrics-noPII.csv', stringsAsFactors = TRUE)

# formr
formr = read.csv('data/data-formr-noPII.csv', stringsAsFactors = TRUE)


##################################### QUALTRICS

######################### RENAME LONG VARIABLES
## survey timestamps
qualtrics = rename(qualtrics,
                   Survey_Completion_Time = duration_in_seconds,
                   Date = recorded_date
)

## demographics: age, race/ethnicity, gender, sexual orientation
qualtrics = rename(qualtrics,
                   Age = what_is_your_age,
                   MinorityEthnic_Race_Status = do_you_consider_yourself_to_be_part_of_a_minority_ethnic_or_racial_group_in_the_country_where_you_currently_live,
                   Gender = what_is_your_gender,
                   Sexual_Orientation = what_is_your_sexual_orientation_how_you_feel_even_if_you_are_not_sexually_active
)

## geography
qualtrics = rename(qualtrics,
                   Live_in_USA = do_you_live_in_the_united_states_of_america,
                   Location = where_do_you_live,
                   Household_Size = how_many_persons_are_part_of_your_household,
                   MultiGeneration_Household = do_you_live_in_a_household_with_more_than_two_generations_more_than_the_youngest_generation_and_their_parents,
                   Generations_per_Household = how_many_generations_live_with_you
)

## income
qualtrics = rename(qualtrics,
                   Income = what_is_your_yearly_income,
                   Income_Meets_Needs = does_your_household_income_meet_your_needs
)

## income and ses
qualtrics = rename(qualtrics,
                   US_Race = we_are_interested_in_the_interaction_of_income_and_race_in_the_us_on_covid_19_which_do_you_identify_as_please_check_all_that_apply,
                   Compare_Income_Race = how_does_your_household_income_compare_to_others_in_your_ethnicity_or_race_group_in_the_country_where_you_currently_live,
                   Compare_Income_GeneralPopulation = how_does_your_household_income_compare_to_the_general_population_regardless_of_race_or_ethnicity_group_in_the_country_where_you_currently_live
)

## resources
qualtrics = rename(qualtrics,
                   Have_Resources = explanation_of_household_resources_apart_from_income_your_household_may_have_other_resources_available_household_resources_are_the_things_needed_for_daily_life_like_food_toilet_paper_cleaning_supplies_shelter_that_are_acquired_by_means_other_than_income_such_as_government_assistance_unemployment_welfare_disability_other_or_social_community_assistance_e_g_food_banks_meals_delivered_to_home_through_charities_charitable_donations_do_you_have_other_household_resources_available,
                   Resources_Meets_Needs = do_the_household_resources_available_to_you_meet_your_needs
)

## resources and ses
qualtrics = rename(qualtrics,
                   Compare_Resources_Race = how_do_the_household_resources_available_to_you_compare_to_others_in_your_ethnicity_or_race_group_in_the_country_where_you_currently_live,
                   Compare_Resources_GeneralPopulation = how_do_the_household_resources_available_to_you_compare_to_the_general_population_regardless_of_race_or_ethnicity_group_in_the_country_where_you_currently_live
)

## environmental exposure
qualtrics = rename(qualtrics,
                   Exposure_Chemicals = in_your_occupation_or_home_environment_are_you_exposed_to_any_of_the_following_on_a_regular_basis_most_days_in_a_week_check_all_that_apply_chemicals_e_g_cleaning_solvents_sanitation_hair_salon_pesticides_chemical_plant_manufacturing_processing_treatment_facility,
                   Exposure_AirPollution = in_your_occupation_or_home_environment_are_you_exposed_to_any_of_the_following_on_a_regular_basis_most_days_in_a_week_check_all_that_apply_air_pollution_e_g_outdoor_work_in_heavy_traffic_like_sanitation_or_gardening_mining_manufacturing_heavy_traffic_fire_fighting,
                   Exposure_Tobacco = in_your_occupation_or_home_environment_are_you_exposed_to_any_of_the_following_on_a_regular_basis_most_days_in_a_week_check_all_that_apply_tobacco_use_e_g_working_in_a_place_where_smoking_is_allowed_and_common,
                   Exposure_PoorVentilation = in_your_occupation_or_home_environment_are_you_exposed_to_any_of_the_following_on_a_regular_basis_most_days_in_a_week_check_all_that_apply_poor_ventilation_e_g_closed_interior_environment_poor_air_circulation,
                   Exposure_NotApplicable = in_your_occupation_or_home_environment_are_you_exposed_to_any_of_the_following_on_a_regular_basis_most_days_in_a_week_check_all_that_apply_not_applicable
)

## ST Loss
qualtrics = rename(qualtrics,
                   Experienced_SmellTasteLoss = have_you_ever_experienced_loss_of_smell_or_taste_or_do_you_currently_have_an_impaired_sense_of_smell_or_taste,
                   Time_Since_SmellTasteLoss = how_long_ago_was_your_most_recent_smell_or_taste_loss,
                   SmellTasteLoss_Cause_General = what_is_the_cause_of_your_most_recent_smell_or_taste_loss,
                   SmellTasteLoss_Cause_VirusSpecific = what_viral_illness_caused_your_most_recent_smell_or_taste_loss
)

## Symptoms
qualtrics = rename(qualtrics,
                   Symptom_Fever = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_fever,
                   Symptom_DryCough = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_dry_cough,
                   Symptom_DryMouth = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_dry_mouth,
                   Symptom_CoughMucus = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_cough_with_mucus,
                   Symptom_DifficultyBreathing = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_difficulty_breathing_shortness_of_breat,
                   Symptom_ChestTightness = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_chest_tightness,
                   Symptom_RunnyNose = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_runny_nose,
                   Symptom_SoreThroat = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_sore_throat,
                   Symptom_ChangesFoodFlavor = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_changes_in_food_flavor,
                   Symptom_ChangesSmell = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_changes_in_smell,
                   Symptom_LostAppetite = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_loss_of_appetite,
                   Symptom_Headache = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_headache,
                   Symptom_Muscleache = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_muscle_aches,
                   Symptom_Fatigue = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_fatigue,
                   Symptom_Diarrhea = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_diarrhea,
                   Symptom_AbdominalPain = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_abdominal_pain,
                   Symptom_Nausea = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_nausea,
                   Symptom_OtherSymptoms = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_other_symptoms,
                   Symptom_NoSymptoms = have_you_had_any_of_the_following_symptoms_with_your_recent_viral_illness_select_all_that_apply_no_symptoms
)

## diagnosis
qualtrics = rename(qualtrics,
                   Diagnosis_Method = how_was_that_viral_illness_diagnosed,
                   Diagnosis_due_to_SmellTasteLoss = was_a_change_in_your_sense_of_smell_or_taste_a_factor_in_your_decision_to_seek_a_diagnosis_for_your_recent_viral_illness
)

## self-rated OA
qualtrics = rename(qualtrics,
                   OlfactoryAbility_BeforeSmellLoss = rate_from_0_10_your_ability_to_smell_before_your_most_recent_smell_or_taste_loss,
                   OlfactoryAbility_DuringSmellLoss = rate_from_0_10_your_ability_to_smell_during_your_most_recent_smell_or_taste_loss_when_your_loss_was_at_its_worst,
                   OlfactoryAbility_Current = rate_from_0_10_your_current_ability_to_smell
)

## odor importance
qualtrics = rename(qualtrics,
                   Odor_Importance = how_important_are_odors_to_you_in_your_everyday_life,
                   Notice_Other_House_Smells = when_you_visit_someone_else_s_house_do_you_notice_how_it_smells,
                   Odors_Revive_Memories_Emotions = do_odors_revive_memories_or_emotions_in_you,
                   Notice_Odors = do_you_notice_the_odors_good_or_bad_around_you
)

## health
qualtrics = rename(qualtrics,
                   Chronic_Illnesses = do_you_have_any_chronic_illnesses,
                   Cancer_Treatment = have_you_ever_received_cancer_treatment_chemotherapy_radiation_or_iodine_treatment,
                   Daily_Medications = do_you_use_any_daily_medications,
                   Oral_Condition_Lickert = how_would_you_describe_the_overall_condition_of_your_teeth_dentures_or_gums
)

## smoking
qualtrics = rename(qualtrics,
                   Smoke_Vape_Daily = do_you_smoke_or_vape_daily,
                   Smoke_Traditional = which_kind_of_products_do_you_smoke_or_vape_check_all_that_apply_traditional_tobacco_products_like_cigarettes_cigars_and_pipes,
                   Smoke_Vape = which_kind_of_products_do_you_smoke_or_vape_check_all_that_apply_e_cigarettes_and_other_vaping_devices
)



######################### RECODE VARS FOR DOWNWTREAM ANALYSIS
qualtrics$Gender = gsub("[[:punct:]]", " ", qualtrics$Gender)
qualtrics$Gender = as.factor(qualtrics$Gender)
qualtrics = qualtrics |> mutate(Gender=str_replace(Gender, "Cis woman  same as assigned at birth ", "CisWoman"))
qualtrics = qualtrics |> mutate(Gender=str_replace(Gender, "Cis man  same as assigned at birth ", "CisMan"))
qualtrics = qualtrics |> mutate(Gender=str_replace(Gender, "GenderQueer or Non binary  gender non conforming ", "NonBinary"))
qualtrics = qualtrics |> mutate(Gender=str_replace(Gender, "Trans man  different from that assigned at birth ", "TransMan"))
qualtrics = qualtrics |> mutate(Gender=str_replace(Gender, "Trans woman  different from that assigned at birth ", "TransWoman"))

# income
qualtrics$Income = gsub("[[:punct:]]", " ", qualtrics$Income)
qualtrics = qualtrics |> mutate(Income=str_replace(Income, "Less than  32  000", "Below32"))
qualtrics = qualtrics |> mutate(Income=str_replace(Income, " 32 000    41  000", "Upto41"))
qualtrics = qualtrics |> mutate(Income=str_replace(Income, "More than  41  000", "41K_Plus"))

# daily  medications
qualtrics = qualtrics |> mutate(Daily_Medications=str_replace(Daily_Medications, "2 or more", "2_Plus"))

# smoking
qualtrics = qualtrics |> mutate(Smoke_Traditional=str_replace(Smoke_Traditional, "Traditional tobacco products, like cigarettes, cigars and pipes", "Yes"))
qualtrics = qualtrics |> mutate(Smoke_Vape=str_replace(Smoke_Vape, "E-cigarettes and other vaping devices", "Yes"))

#chronic illnesses
qualtrics = qualtrics |> mutate(Chronic_Illnesses=str_replace(Chronic_Illnesses, "2 or more", "2_Plus"))

# Diagnosis Method
qualtrics$Diagnosis_Method = gsub("[[:punct:]]", " ", qualtrics$Diagnosis_Method)
qualtrics = qualtrics |> mutate(Diagnosis_Method=str_replace(Diagnosis_Method, "Lab test  swab or nose throat  saliva or blood test ", "Lab_Test"))
qualtrics = qualtrics |> mutate(Diagnosis_Method=str_replace(Diagnosis_Method, "Clinical  interview with medical expert ", "Clinical"))

# Symptoms
qualtrics = qualtrics |> mutate(Symptom_Fever=str_replace(Symptom_Fever,"Fever","1"))
qualtrics = qualtrics |> mutate(Symptom_DryCough=str_replace(Symptom_DryCough,"Dry cough","1"))
qualtrics = qualtrics |> mutate(Symptom_DryMouth=str_replace(Symptom_DryMouth,"Dry mouth","1"))
qualtrics = qualtrics |> mutate(Symptom_CoughMucus=str_replace(Symptom_CoughMucus,"Cough with mucus","1"))
qualtrics = qualtrics |> mutate(Symptom_DifficultyBreathing=str_replace(Symptom_DifficultyBreathing,"Difficulty breathing/shortness of breat","1"))
qualtrics = qualtrics |> mutate(Symptom_ChestTightness=str_replace(Symptom_ChestTightness,"Chest tightness","1"))
qualtrics = qualtrics |> mutate(Symptom_RunnyNose=str_replace(Symptom_RunnyNose,"Runny nose","1"))
qualtrics = qualtrics |> mutate(Symptom_SoreThroat=str_replace(Symptom_SoreThroat,"Sore throat","1"))
qualtrics = qualtrics |> mutate(Symptom_ChangesFoodFlavor=str_replace(Symptom_ChangesFoodFlavor,"Changes in food flavor","1"))
qualtrics = qualtrics |> mutate(Symptom_ChangesSmell=str_replace(Symptom_ChangesSmell,"Changes in smell","1"))
qualtrics = qualtrics |> mutate(Symptom_LostAppetite=str_replace(Symptom_LostAppetite,"Loss of appetite","1"))
qualtrics = qualtrics |> mutate(Symptom_Headache=str_replace(Symptom_Headache,"Headache","1"))
qualtrics = qualtrics |> mutate(Symptom_Muscleache=str_replace(Symptom_Muscleache,"Muscle aches","1"))
qualtrics = qualtrics |> mutate(Symptom_Fatigue=str_replace(Symptom_Fatigue,"Fatigue","1"))
qualtrics = qualtrics |> mutate(Symptom_Diarrhea=str_replace(Symptom_Diarrhea,"Diarrhea","1"))
qualtrics = qualtrics |> mutate(Symptom_AbdominalPain=str_replace(Symptom_AbdominalPain,"Abdominal pain","1"))
qualtrics = qualtrics |> mutate(Symptom_Nausea=str_replace(Symptom_Nausea,"Nausea","1"))
qualtrics = qualtrics |> mutate(Symptom_OtherSymptoms=str_replace(Symptom_OtherSymptoms,"Other symptoms","1"))
qualtrics = qualtrics |> mutate(Symptom_NoSymptoms=str_replace(Symptom_NoSymptoms, "No symptoms", "0"))

# SmellTasteLoss_Cause_General
qualtrics$SmellTasteLoss_Cause_General = gsub("[[:punct:]]", " ", qualtrics$SmellTasteLoss_Cause_General)
qualtrics = qualtrics |> mutate(SmellTasteLoss_Cause_General=str_replace(SmellTasteLoss_Cause_General, "Medication use or treatment for cancer", "Medication_or_Cancer"))
qualtrics = qualtrics |> mutate(SmellTasteLoss_Cause_General=str_replace(SmellTasteLoss_Cause_General, "Viral illness  like COVID 19  common cold  or flu  influenza ", "Viral"))
qualtrics = qualtrics |> mutate(SmellTasteLoss_Cause_General=str_replace(SmellTasteLoss_Cause_General, "I never had a sense of smell  congenital ", "Congenital"))
qualtrics = qualtrics |> mutate(SmellTasteLoss_Cause_General=str_replace(SmellTasteLoss_Cause_General, "Head injury or any other injury", "Injury"))
qualtrics = qualtrics |> mutate(SmellTasteLoss_Cause_General=str_replace(SmellTasteLoss_Cause_General, "Occupational causes  like exposure to chemicals at work ", "Occupational"))
qualtrics = qualtrics |> mutate(SmellTasteLoss_Cause_General=str_replace(SmellTasteLoss_Cause_General, "Bacterial illness  strep throat ", "Bacterial"))

# SmellTasteLoss_Cause_VirusSpecific
qualtrics$SmellTasteLoss_Cause_VirusSpecific = gsub("[[:punct:]]", " ", qualtrics$SmellTasteLoss_Cause_VirusSpecific)
qualtrics = qualtrics |> mutate(SmellTasteLoss_Cause_VirusSpecific=str_replace(SmellTasteLoss_Cause_VirusSpecific, "Flu  influenza ", "Flu"))
qualtrics = qualtrics |> mutate(SmellTasteLoss_Cause_VirusSpecific=str_replace(SmellTasteLoss_Cause_VirusSpecific, "COVID 19", "Covid"))
qualtrics = qualtrics |> mutate(SmellTasteLoss_Cause_VirusSpecific=str_replace(SmellTasteLoss_Cause_VirusSpecific, "Common cold  rhinoviruses  coronaviruses other than Covid  adenoviruses and enteroviruses ", "NonCovid Virus"))

# time since loss
qualtrics$Time_Since_SmellTasteLoss = gsub("[[:punct:]]", " ", qualtrics$Time_Since_SmellTasteLoss)
qualtrics = qualtrics |> mutate(Time_Since_SmellTasteLoss=str_replace(Time_Since_SmellTasteLoss, "Less than 3 months", "3 months or less"))
qualtrics = qualtrics |> mutate(Time_Since_SmellTasteLoss=str_replace(Time_Since_SmellTasteLoss, "3 months   2 years", "3 months to 2 years"))
qualtrics = qualtrics |> mutate(Time_Since_SmellTasteLoss=str_replace(Time_Since_SmellTasteLoss, "2 10 years", "2 to 10 years"))
qualtrics = qualtrics |> mutate(Time_Since_SmellTasteLoss=str_replace(Time_Since_SmellTasteLoss, "More than 10 years", "10 years or more"))

# Sexual orientation
qualtrics$Sexual_Orientation = gsub("[[:punct:]]", " ", qualtrics$Sexual_Orientation)
qualtrics = qualtrics |> mutate(Sexual_Orientation=str_replace(Sexual_Orientation, "Pansexual  attracted to anyone  not tied to binary gender ", "Pansexual"))
qualtrics = qualtrics |> mutate(Sexual_Orientation=str_replace(Sexual_Orientation, "Homosexual  attracted to same sex or gender ", "Homosexual"))
qualtrics = qualtrics |> mutate(Sexual_Orientation=str_replace(Sexual_Orientation, "Heterosexual  attracted to opposite sex or gender ", "Heterosexual"))
qualtrics = qualtrics |> mutate(Sexual_Orientation=str_replace(Sexual_Orientation, "Bisexual  attracted to men and women ", "Bisexual"))
qualtrics = qualtrics |> mutate(Sexual_Orientation=str_replace(Sexual_Orientation, "Asexual  not interested in sex not sexual ", "Asexual"))

# gender
qualtrics = qualtrics |> mutate(Gender=str_replace_all(Gender, "Trans man (different from that assigned at birth)", "Trans_Man"))
qualtrics = qualtrics |> mutate(Gender=str_replace_all(Gender, "GenderQueer or Non-binary (gender non-conforming)", "GenderQueer"))
qualtrics = qualtrics |> mutate(Gender=str_replace_all(Gender, "Cis woman (same as assigned at birth)", "Cis_Woman"))
qualtrics = qualtrics |> mutate(Gender=str_replace_all(Gender, "Cis man (same as assigned at birth)", "Cis_Man"))

# US Race
qualtrics$US_Race = gsub("[[:punct:]]", " ", qualtrics$US_Race)
qualtrics = qualtrics |> mutate(US_Race=str_replace(US_Race, "Black or African American", "Black"))

# Exposure
qualtrics$Exposure_Chemicals = gsub("[[:punct:]]", " ", qualtrics$Exposure_Chemicals)
qualtrics$Exposure_AirPollution = gsub("[[:punct:]]", " ", qualtrics$Exposure_AirPollution)
qualtrics$Exposure_Tobacco = gsub("[[:punct:]]", " ", qualtrics$Exposure_Tobacco)
qualtrics$Exposure_PoorVentilation = gsub("[[:punct:]]", " ", qualtrics$Exposure_PoorVentilation)

qualtrics = qualtrics |> mutate(Exposure_Chemicals=str_replace(Exposure_Chemicals,"Chemicals  e g   cleaning solvents sanitation  hair salon  pesticides  chemical plant  manufacturing  processing treatment facility ","1"))
qualtrics = qualtrics |> mutate(Exposure_AirPollution=str_replace(Exposure_AirPollution,"Air pollution  e g   outdoor work in heavy traffic like sanitation or gardening  mining  manufacturing  heavy traffic  fire fighting ","1"))
qualtrics = qualtrics |> mutate(Exposure_Tobacco=str_replace(Exposure_Tobacco,"Tobacco use  e g   working in a place where smoking is allowed and common ","1"))
qualtrics = qualtrics |> mutate(Exposure_PoorVentilation=str_replace(Exposure_PoorVentilation,"Poor ventilation  e g   closed interior environment  poor air circulation ","1"))

# Health
qualtrics = qualtrics |> mutate(Chronic_Illnesses=str_replace(Chronic_Illnesses,"None","0"))
qualtrics = qualtrics |> mutate(Chronic_Illnesses=str_replace(Chronic_Illnesses,"2_Plus","2"))
qualtrics = qualtrics |> mutate(Cancer_Treatment=str_replace(Cancer_Treatment,"Yes","1"))
qualtrics = qualtrics |> mutate(Cancer_Treatment=str_replace(Cancer_Treatment,"No","0"))
qualtrics = qualtrics |> mutate(Daily_Medications=str_replace(Daily_Medications,"No","0"))
qualtrics = qualtrics |> mutate(Daily_Medications=str_replace(Daily_Medications,"2_Plus","2"))
qualtrics = qualtrics |> mutate(Smoke_Traditional=str_replace(Smoke_Traditional,"Yes","1"))
qualtrics = qualtrics |> mutate(Smoke_Vape=str_replace(Smoke_Vape,"Yes","1"))

# Create oral Condition binary variable
qualtrics = qualtrics |> mutate(Oral_Condition_Lickert=str_replace_all(Oral_Condition_Lickert, " ", "_"))
qualtrics$Oral_Condition_Number = qualtrics$Oral_Condition_Lickert
#recode
qualtrics = qualtrics |> mutate(Oral_Condition_Number=str_replace(Oral_Condition_Number,"Excellent","2"))
qualtrics = qualtrics |> mutate(Oral_Condition_Number=str_replace(Oral_Condition_Number,"Very_good","2"))
qualtrics = qualtrics |> mutate(Oral_Condition_Number=str_replace(Oral_Condition_Number,"Good","1"))
qualtrics = qualtrics |> mutate(Oral_Condition_Number=str_replace(Oral_Condition_Number,"Fair","0"))
qualtrics = qualtrics |> mutate(Oral_Condition_Number=str_replace(Oral_Condition_Number,"Poor","0"))

# Location
qualtrics$Location = gsub("[[:punct:]]", " ", qualtrics$Location)
qualtrics = qualtrics |> mutate(Location=str_replace(Location, "Urban  towns  cities  suburbs ", "Urban"))
qualtrics = qualtrics |> mutate(Location=str_replace(Location, "Rural  agricultural  rural  outside towns  cities  suburbs ", "Rural"))


######################### DATA CLEANING
# docx: "long responses to short versions...presence/absence to 0/1...replace spaces with underscores"

# replace spaces with underscore in columns (universal across all listed columns)
space_to_underscore_cols = c(
  "Age", "Smoke_Vape_Daily", "Chronic_Illnesses", "Cancer_Treatment",
  "Notice_Odors", "Odors_Revive_Memories_Emotions", "Odor_Importance", "Notice_Other_House_Smells",
  "OlfactoryAbility_Current", "OlfactoryAbility_BeforeSmellLoss", "OlfactoryAbility_DuringSmellLoss",
  "Diagnosis_Method", "Diagnosis_due_to_SmellTasteLoss",
  "Symptom_Fever", "Symptom_DryCough", "Symptom_DryMouth", "Symptom_CoughMucus",
  "Symptom_DifficultyBreathing", "Symptom_ChestTightness", "Symptom_RunnyNose", "Symptom_SoreThroat",
  "Symptom_ChangesFoodFlavor", "Symptom_ChangesSmell", "Symptom_LostAppetite", "Symptom_Headache",
  "Symptom_Muscleache", "Symptom_Fatigue", "Symptom_Diarrhea", "Symptom_AbdominalPain",
  "Symptom_Nausea", "Symptom_OtherSymptoms", "Symptom_NoSymptoms",
  "SmellTasteLoss_Cause_General", "SmellTasteLoss_Cause_VirusSpecific", "Time_Since_SmellTasteLoss",
  "Sexual_Orientation", "Experienced_SmellTasteLoss", "MinorityEthnic_Race_Status", "US_Race",
  "Compare_Income_Race", "Income_Meets_Needs", "Compare_Income_GeneralPopulation",
  "Compare_Resources_Race", "Compare_Resources_GeneralPopulation", "Resources_Meets_Needs",
  "Exposure_Chemicals", "Exposure_AirPollution", "Exposure_Tobacco", "Exposure_PoorVentilation",
  "Exposure_NotApplicable", "Location", "Household_Size", "Generations_per_Household"
)
qualtrics = qualtrics |> mutate(across(all_of(space_to_underscore_cols), ~str_replace_all(.x, " ", "_")))

# recode responses: presence/absence to 0/1 -- replace NA with 0 in Symptoms (universal across all Symptom_ columns)
qualtrics = qualtrics |> mutate(across(starts_with("Symptom_"), ~replace(.x, is.na(.x), 0)))



######################### IMPLAUSIBLE RESPONSES FROM PRE-REG
# docx note: 32-based pairs (32 vs 40-41, 32 vs 37) were judged plausible on review (32 is a
# general/lifetime item, not COVID-specific) -- retained, no exclusion code needed.
# Only the pairs below were confirmed implausible and excluded.

qualtrics = qualtrics |> mutate(
  Olfactory_Scale_Changed = OlfactoryAbility_DuringSmellLoss != OlfactoryAbility_BeforeSmellLoss,
  Implausible_ChangesSmell_NoScale = Symptom_ChangesSmell == 1 & Olfactory_Scale_Changed == FALSE,
  Implausible_Dx_NoScale = Diagnosis_due_to_SmellTasteLoss == "Yes" & Olfactory_Scale_Changed == FALSE,
  Implausible_Dx_NoSymptom = Diagnosis_due_to_SmellTasteLoss == "Yes" & is.na(Symptom_ChangesSmell),
  Implausible_Generations = MultiGeneration_Household == "Yes" & Generations_per_Household < 3
)

# reported ST symptom (37) but no scale change (40-41)
table(qualtrics$Symptom_ChangesSmell, qualtrics$Olfactory_Scale_Changed)
# 5

# reported dx seeking due to ST (38) but no scale change (40-41)
table(qualtrics$Diagnosis_due_to_SmellTasteLoss, qualtrics$Olfactory_Scale_Changed)
# 12

# reported dx seeking due to ST (38) but no ST symptom (38,37)
table(qualtrics$Diagnosis_due_to_SmellTasteLoss, qualtrics$Symptom_ChangesSmell, useNA = "ifany")
# 30

# reported more than two generations in household (19) but gives a number lower than 3 (20)
table(qualtrics$MultiGeneration_Household, qualtrics$Generations_per_Household)

### check whether implausible no-scale-change cases still show long-haul/recovery direction
## long haul
table(qualtrics$Implausible_ChangesSmell_NoScale, qualtrics$OlfactoryAbility_Current < qualtrics$OlfactoryAbility_BeforeSmellLoss)
table(qualtrics$Implausible_Dx_NoScale, qualtrics$OlfactoryAbility_Current < qualtrics$OlfactoryAbility_BeforeSmellLoss)
# Implausible_ChangesSmell_NoScale, 1/5 shows current < before
# Implausible_Dx_NoScale, 1/12 shows current < before.

## recovery
table(qualtrics$Implausible_ChangesSmell_NoScale, qualtrics$OlfactoryAbility_Current < qualtrics$OlfactoryAbility_DuringSmellLoss)
table(qualtrics$Implausible_Dx_NoScale, qualtrics$OlfactoryAbility_Current < qualtrics$OlfactoryAbility_DuringSmellLoss)
# Implausible_ChangesSmell_NoScale, 1/5 shows current < before
# Implausible_Dx_NoScale, 1/12 shows current < before.

# docx: "exclude data for implausible pairs but retain data for questions that are valid"
qualtrics = qualtrics |> mutate(
  Symptom_ChangesSmell = if_else(Implausible_ChangesSmell_NoScale | Implausible_Dx_NoSymptom, NA, Symptom_ChangesSmell),
  OlfactoryAbility_BeforeSmellLoss = if_else(Implausible_ChangesSmell_NoScale | Implausible_Dx_NoScale, NA, OlfactoryAbility_BeforeSmellLoss),
  OlfactoryAbility_DuringSmellLoss = if_else(Implausible_ChangesSmell_NoScale | Implausible_Dx_NoScale, NA, OlfactoryAbility_DuringSmellLoss),
  Diagnosis_due_to_SmellTasteLoss = if_else(Implausible_Dx_NoScale | Implausible_Dx_NoSymptom, NA, Diagnosis_due_to_SmellTasteLoss),
  MultiGeneration_Household = if_else(Implausible_Generations, NA, MultiGeneration_Household),
  Generations_per_Household = if_else(Implausible_Generations, NA, Generations_per_Household)
)



######################### SURVEY RESPONSE ISSUES DEVIATING FROM PRE-REG

## -- Race --
# docx: MinorityEthnic_Race_Status -- 60 respondents answered "Unsure"
# Decision: retain only Yes/No responses in new variable MinorityEthnic_Race_Status_Binary
qualtrics = qualtrics |> mutate(MinorityEthnic_Race_Status_Binary = case_when(
  MinorityEthnic_Race_Status == "Yes" ~ "Yes",
  MinorityEthnic_Race_Status == "No" ~ "No"
))

## -- Household --
# docx: "2 nonsense responses to household size" -- Decision: remove values in cells
qualtrics = qualtrics |> mutate(Generations_per_Household=str_replace(Generations_per_Household, "One", "1"))
qualtrics = qualtrics |> mutate(Generations_per_Household=str_replace(Generations_per_Household, "Hfjdhds", ""))
qualtrics = qualtrics |> mutate(Generations_per_Household=str_replace(Generations_per_Household, "76", ""))

# docx: "Missing value imputation for multi-generational household" -- Decision: use household size for missing values
qualtrics = qualtrics |> mutate(MultiGeneration_Household=replace(MultiGeneration_Household, Household_Size==0, "No"))
qualtrics = qualtrics |> mutate(MultiGeneration_Household=replace(MultiGeneration_Household, Household_Size==1, "No"))
qualtrics = qualtrics |> mutate(MultiGeneration_Household=replace(MultiGeneration_Household, Household_Size==2, "No"))

# docx: "one person reporting symptoms but not selecting any specific symptoms" -- Decision: [blank in notes]
symptom_cols = c("Symptom_Fever","Symptom_DryCough","Symptom_DryMouth","Symptom_CoughMucus",
                 "Symptom_DifficultyBreathing","Symptom_ChestTightness","Symptom_RunnyNose",
                 "Symptom_SoreThroat","Symptom_ChangesFoodFlavor","Symptom_ChangesSmell",
                 "Symptom_LostAppetite","Symptom_Headache","Symptom_Muscleache","Symptom_Fatigue",
                 "Symptom_Diarrhea","Symptom_AbdominalPain","Symptom_Nausea","Symptom_OtherSymptoms")

qualtrics = qualtrics |>
  mutate(Has_Other_Symptom = rowSums(across(all_of(symptom_cols), ~ .x == "1"), na.rm = TRUE) > 0) |>
  mutate(Symptom_NoSymptoms = if_else(Symptom_NoSymptoms == "0" & Has_Other_Symptom, "", Symptom_NoSymptoms)) |>
  dplyr::select(-Has_Other_Symptom)




######################### REPLACE MISSING WITH NA
qualtrics[qualtrics==""]=NA



######################### KEEP ONLY VARS NEEDED FOR ANALYSIS
kept_vars = c(
  "Age", "MinorityEthnic_Race_Status", "MinorityEthnic_Race_Status_Binary", "Gender", "Sexual_Orientation",
  "Live_in_USA", "Location", "State", "Household_Size", "MultiGeneration_Household", "Generations_per_Household",
  "Income", "Income_Meets_Needs",
  "US_Race", "Compare_Income_Race", "Compare_Income_GeneralPopulation",
  "Have_Resources", "Resources_Meets_Needs",
  "Compare_Resources_Race", "Compare_Resources_GeneralPopulation",
  "Exposure_Chemicals", "Exposure_AirPollution", "Exposure_Tobacco", "Exposure_PoorVentilation", "Exposure_NotApplicable",
  "Experienced_SmellTasteLoss", "Time_Since_SmellTasteLoss", "SmellTasteLoss_Cause_General", "SmellTasteLoss_Cause_VirusSpecific",
  "Symptom_Fever", "Symptom_DryCough", "Symptom_DryMouth", "Symptom_CoughMucus",
  "Symptom_DifficultyBreathing", "Symptom_ChestTightness", "Symptom_RunnyNose", "Symptom_SoreThroat",
  "Symptom_ChangesFoodFlavor", "Symptom_ChangesSmell", "Symptom_LostAppetite", "Symptom_Headache",
  "Symptom_Muscleache", "Symptom_Fatigue", "Symptom_Diarrhea", "Symptom_AbdominalPain",
  "Symptom_Nausea", "Symptom_OtherSymptoms", "Symptom_NoSymptoms",
  "Diagnosis_Method", "Diagnosis_due_to_SmellTasteLoss",
  "OlfactoryAbility_BeforeSmellLoss", "OlfactoryAbility_DuringSmellLoss", "OlfactoryAbility_Current",
  "Odor_Importance", "Notice_Other_House_Smells", "Odors_Revive_Memories_Emotions", "Notice_Odors",
  "Chronic_Illnesses", "Cancer_Treatment", "Daily_Medications", "Oral_Condition_Lickert", "Oral_Condition_Number",
  "Smoke_Vape_Daily", "Smoke_Traditional", "Smoke_Vape"
)
qualtrics = qualtrics |> dplyr::select(all_of(kept_vars))



##################################### FORMR

######################### RENAME TO MATCH QUALTRICS
## demographics
formr = rename(formr,
               Age = age,
               US_Race = us_race,
               Gender = gender,
               Sexual_Orientation = sexual_orientation
)

## geography
formr = rename(formr,
               Live_in_USA = live_in_usa,
               Location = location,
               Household_Size = household_size,
               Generations_per_Household = generations_per_household
)

## income and ses
formr = rename(formr,
               Income = income,
               Income_Meets_Needs = income_meets_needs,
               Compare_Income_Race = compare_income_race,
               Compare_Income_GeneralPopulation = compare_income_general_population
)

## resources
formr = rename(formr,
               Have_Resources = have_resources,
               Resources_Meets_Needs = resources_meets_needs,
               Compare_Resources_Race = compare_resources_race,
               Compare_Resources_GeneralPopulation = compare_resources_general_population
)

## environmental exposure
formr = rename(formr,
               Exposure_Chemicals = exposure_chemicals,
               Exposure_AirPollution = exposure_air_pollution,
               Exposure_Tobacco = exposure_tobacco,
               Exposure_PoorVentilation = exposure_poor_ventilation,
               Exposure_NotApplicable = exposure_not_applicable
)

## ST loss
formr = rename(formr,
               Experienced_SmellTasteLoss = experienced_smell_taste_loss,
               Time_Since_SmellTasteLoss = time_since_smell_taste_loss,
               SmellTasteLoss_Cause_General = smell_taste_loss_cause_general,
               SmellTasteLoss_Cause_VirusSpecific = smell_taste_loss_cause_virus_specific
)

## symptoms -- virus_* is the generic-viral-illness block matching qualtrics; covid_* dropped below
formr = rename(formr,
               Symptom_Fever = virus_fever,
               Symptom_DryCough = virus_dry_cough,
               Symptom_DryMouth = virus_dry_mouth,
               Symptom_CoughMucus = virus_cough_mucus,
               Symptom_DifficultyBreathing = virus_difficulty_breathing,
               Symptom_ChestTightness = virus_chest_tightness,
               Symptom_RunnyNose = virus_runny_nose,
               Symptom_SoreThroat = virus_sore_throat,
               Symptom_ChangesFoodFlavor = virus_changes_food_flavor,
               Symptom_ChangesSmell = virus_changes_smell,
               Symptom_LostAppetite = virus_lost_appetite,
               Symptom_Headache = virus_headache,
               Symptom_Muscleache = virus_muscleache,
               Symptom_Fatigue = virus_fatigue,
               Symptom_Diarrhea = virus_diarrhea,
               Symptom_AbdominalPain = virus_abdominal_pain,
               Symptom_Nausea = virus_nausea,
               Symptom_OtherSymptoms = virus_other_symptoms,
               Symptom_NoSymptoms = virus_no_symptoms
)

## diagnosis -- diagnosis_method_virus is the generic-viral-illness match; diagnosis_method_covid dropped below
formr = rename(formr,
               Diagnosis_Method = diagnosis_method_virus,
               Diagnosis_due_to_SmellTasteLoss = diagnosis_due_to_smell_taste_loss
)

## self-rated OA
formr = rename(formr,
               OlfactoryAbility_BeforeSmellLoss = olfactory_ability_before_smell_loss,
               OlfactoryAbility_DuringSmellLoss = olfactory_ability_during_smell_loss,
               OlfactoryAbility_Current = olfactory_ability_current
)

## odor importance
formr = rename(formr,
               Odor_Importance = odor_importance,
               Notice_Other_House_Smells = notice_other_house_smells,
               Odors_Revive_Memories_Emotions = odors_revive_memories_emotions,
               Notice_Odors = notice_odors
)

## health
formr = rename(formr,
               Chronic_Illnesses = chronic_illnesses,
               Cancer_Treatment = cancer_treatment,
               Daily_Medications = daily_medications,
               Oral_Condition_Lickert = oral_condition_lickert,
               Oral_Condition_Number = oral_condition_number
)

## smoking
formr = rename(formr,
               Smoke_Vape_Daily = smoke_vape_daily,
               Smoke_Traditional = smoke_traditional,
               Smoke_Vape = smoke_vape
)

## race, already binary in formr
formr = rename(formr,
               MinorityEthnic_Race_Status = minority_ethnic_race_status,
               MinorityEthnic_Race_Status_Binary = minority_ethnic_race_status_binary
)



######################### DROP DUPLICATE COVID-SPECIFIC ITEMS
# formr asked symptoms/diagnosis-method/time-since twice: once generic (virus_*, kept and
# renamed above to match qualtrics) and once covid-specific (covid_*, a followup refinement
# qualtrics never asked). dropping the covid-specific duplicates here.
formr = formr |> dplyr::select(-matches("^covid_"), -diagnosis_method_covid, -time_since_covid)



######################### KEEP ONLY VARS NEEDED FOR ANALYSIS
formr_kept_vars = c(
  "Age", "MinorityEthnic_Race_Status", "MinorityEthnic_Race_Status_Binary", "Gender", "Sexual_Orientation",
  "Live_in_USA", "Location", "State", "Household_Size", "Generations_per_Household",
  "Income", "Income_Meets_Needs",
  "US_Race", "Compare_Income_Race", "Compare_Income_GeneralPopulation",
  "Have_Resources", "Resources_Meets_Needs",
  "Compare_Resources_Race", "Compare_Resources_GeneralPopulation",
  "Exposure_Chemicals", "Exposure_AirPollution", "Exposure_Tobacco", "Exposure_PoorVentilation", "Exposure_NotApplicable",
  "Experienced_SmellTasteLoss", "Time_Since_SmellTasteLoss", "SmellTasteLoss_Cause_General", "SmellTasteLoss_Cause_VirusSpecific",
  "Symptom_Fever", "Symptom_DryCough", "Symptom_DryMouth", "Symptom_CoughMucus",
  "Symptom_DifficultyBreathing", "Symptom_ChestTightness", "Symptom_RunnyNose", "Symptom_SoreThroat",
  "Symptom_ChangesFoodFlavor", "Symptom_ChangesSmell", "Symptom_LostAppetite", "Symptom_Headache",
  "Symptom_Muscleache", "Symptom_Fatigue", "Symptom_Diarrhea", "Symptom_AbdominalPain",
  "Symptom_Nausea", "Symptom_OtherSymptoms", "Symptom_NoSymptoms",
  "Diagnosis_Method", "Diagnosis_due_to_SmellTasteLoss",
  "OlfactoryAbility_BeforeSmellLoss", "OlfactoryAbility_DuringSmellLoss", "OlfactoryAbility_Current",
  "Odor_Importance", "Notice_Other_House_Smells", "Odors_Revive_Memories_Emotions", "Notice_Odors",
  "Chronic_Illnesses", "Cancer_Treatment", "Daily_Medications", "Oral_Condition_Lickert", "Oral_Condition_Number",
  "Smoke_Vape_Daily", "Smoke_Traditional", "Smoke_Vape"
)
formr = formr |> dplyr::select(all_of(formr_kept_vars))



######################### SAVE CLEAN DATA FOR DOWNSTREAM ANALYSIS
write.csv(qualtrics, "data/data-qualtrics-clean.csv", quote=FALSE, row.names = FALSE)

write.csv(formr, "data/data-formr-clean.csv", quote=FALSE, row.names = FALSE)



######################### TIDY
rm(list = ls())
gc()