######################### LOAD PACKAGES
library(dplyr)  # data wrangling



######################### NOTE
# See artifacts/data cleaning notes.docx
# contains the rationale for deviations from pre-reg



######################### DATA
qualtrics = read.csv('data/data-qualtrics-clean.csv', stringsAsFactors = TRUE)

formr = read.csv('data/data-formr-clean.csv', stringsAsFactors = TRUE)



######################### Index_SES
### qualtrics
# based on income bracket only
qualtrics = qualtrics |> mutate(Index_SES = as.factor(case_when(
  Income=="Below32" ~ "Below32",
  Income=="Upto41" ~ "Upto41")))

### formr
## based on income bracket only
# us only
formr = formr |> mutate(Index_SES_US = as.factor(case_when(
  Income=="Below32" ~ "Below32",
  Income=="Upto41" ~ "Upto41",
  Income=="41K_Plus" ~ "41K_Plus")))

# global limiting to income only
formr = formr |> mutate(Index_SES = as.factor(case_when(
  Compare_Income_GeneralPopulation %in% c("Much_less", "Moderately_less", "Slightly_less") ~ "less",
  Compare_Income_GeneralPopulation == "About_the_same" ~ "same",
  Compare_Income_GeneralPopulation %in% c("Slightly_more", "Moderately_more", "Much_more") ~ "more"
)))



######################### Index_Needs
## collapse to met/not-

#qualtrics
qualtrics = qualtrics |> mutate(Index_Needs = case_when(
  Income_Meets_Needs == "Yes" | Resources_Meets_Needs == "Yes" ~ 'Needs_Met', TRUE ~ 'Needs_Not_Met'
))

qualtrics = qualtrics |> mutate(Index_Needs_Number = as.factor(case_when(
  Index_Needs=="Needs_Met" ~ 1,
  Index_Needs=="Needs_Not_Met" ~ 0)))

#formr
formr = formr |> mutate(Index_Needs = case_when(
  Income_Meets_Needs == "Yes" | Resources_Meets_Needs == "Yes" ~ 'Needs_Met', TRUE ~ 'Needs_Not_Met'
))

formr = formr |> mutate(Index_Needs_Number = as.factor(case_when(
  Index_Needs=="Needs_Met" ~ 1,
  Index_Needs=="Needs_Not_Met" ~ 0)))



######################### Index Household
# qualtrics, imput missing info using multi-gen
qualtrics = qualtrics |> mutate(Index_Household = case_when(
  Location == "Urban" & MultiGeneration_Household == "No" ~ 'Urban_1to2_Generations',
  Location == "Rural" & MultiGeneration_Household == "No" ~ 'Rural_1to2_Generations',
  Location == "Urban" & Generations_per_Household %in% c(1, 2) ~ 'Urban_1to2_Generations',
  Location == "Urban" & Generations_per_Household > 2 ~ 'Urban_3Plus_Generations',
  Location == "Rural" & Generations_per_Household %in% c(1, 2) ~ 'Rural_1to2_Generations',
  Location == "Rural" & Generations_per_Household > 2 ~ 'Rural_3Plus_Generations'
))

#formr, only generations question present
formr = formr |> mutate(Index_Household = case_when(
  Location == "Urban" & Generations_per_Household %in% c(1, 2) ~ 'Urban_1to2_Generations',
  Location == "Urban" & Generations_per_Household > 2 ~ 'Urban_3Plus_Generations',
  Location == "Rural" & Generations_per_Household %in% c(1, 2) ~ 'Rural_1to2_Generations',
  Location == "Rural" & Generations_per_Household > 2 ~ 'Rural_3Plus_Generations'
))



######################### Index_Environment
# env cols
environment_cols = c("Exposure_Chemicals", "Exposure_AirPollution", "Exposure_Tobacco", "Exposure_PoorVentilation")

# qualtrics
qualtrics$Index_Environment = rowSums(qualtrics[ , environment_cols], na.rm=TRUE)

#formr
formr$Index_Environment = rowSums(formr[ , environment_cols], na.rm=TRUE)



######################### Index_ST_Loss_Cause
#qualtrics
qualtrics = qualtrics |> mutate(Index_ST_Loss_Cause = case_when(
  SmellTasteLoss_Cause_General == "Medication_or_Cancer"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Unknown"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Other"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Occupational"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Injury"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Congenital"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Bacterial"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid' & Diagnosis_Method == "Lab_Test" ~ 'Viral_Covid_Confirmed',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid' & Diagnosis_Method == "Clinical"   ~ 'Viral_Covid_Confirmed',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid'  ~ 'Viral_Covid_Unconfirmed',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Other'  ~ 'Viral_nonCovid',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'NonCovid_Virus'  ~ 'Viral_nonCovid',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Flu'  ~ 'Viral_nonCovid'
))

# formr
formr = formr |> mutate(Index_ST_Loss_Cause = case_when(
  SmellTasteLoss_Cause_General == "Medication_or_Cancer"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Unknown"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Other"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Occupational"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Injury"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Congenital"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Bacterial"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid' & Diagnosis_Method == "Lab_Test" ~ 'Viral_Covid_Confirmed',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid' & Diagnosis_Method == "Clinical"   ~ 'Viral_Covid_Confirmed',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid'  ~ 'Viral_Covid_Unconfirmed',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Other'  ~ 'Viral_nonCovid',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'NonCovid_Virus'  ~ 'Viral_nonCovid',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Flu'  ~ 'Viral_nonCovid'
))



######################### Index_Symptoms
# symptom cols
symptom_cols = c("Symptom_Fever","Symptom_DryCough","Symptom_DryMouth","Symptom_CoughMucus",
                 "Symptom_DifficultyBreathing","Symptom_ChestTightness","Symptom_RunnyNose",
                 "Symptom_SoreThroat","Symptom_ChangesFoodFlavor","Symptom_ChangesSmell",
                 "Symptom_LostAppetite","Symptom_Headache","Symptom_Muscleache","Symptom_Fatigue",
                 "Symptom_Diarrhea","Symptom_AbdominalPain","Symptom_Nausea","Symptom_OtherSymptoms")

# qualtrics
qualtrics$Index_Symptoms = rowSums(qualtrics[ , symptom_cols], na.rm=TRUE)

# formr
formr$Index_Symptoms = rowSums(formr[ , symptom_cols], na.rm=TRUE)



######################### Index_ST_Loss
# qualtrics
qualtrics = qualtrics |> mutate(Index_ST_Loss = case_when(
  Experienced_SmellTasteLoss == "No"  ~ 'No_ST_Loss',
  Experienced_SmellTasteLoss == "Yes"  ~ 'ST_Loss',
  Symptom_ChangesFoodFlavor == 1  ~ 'ST_Loss',
  Symptom_ChangesSmell == 1  ~ 'ST_Loss'
))

# formr
formr = formr |> mutate(Index_ST_Loss = case_when(
  Experienced_SmellTasteLoss == "No"  ~ 'No_ST_Loss',
  Experienced_SmellTasteLoss == "Yes"  ~ 'ST_Loss',
  Symptom_ChangesFoodFlavor == 1  ~ 'ST_Loss',
  Symptom_ChangesSmell == 1  ~ 'ST_Loss'
))



######################### Index_Health
# smoking counted once; oral health replaces second smoke count

# qualtrics
qualtrics = qualtrics |> mutate(Smoke_Vape_Daily = case_when(
  Smoke_Vape_Daily == 1  ~ 1, TRUE ~ 0
))

qualtrics = qualtrics |> mutate(Oral_Condition_Binary = case_when(
  Oral_Condition_Lickert %in% c("Excellent", "Very_good") ~ 0,
  Oral_Condition_Lickert %in% c("Good", "Fair", "Poor") ~ 1
))

health_cols = c("Chronic_Illnesses", "Cancer_Treatment", "Daily_Medications", "Oral_Condition_Binary", "Smoke_Vape_Daily")

qualtrics$Index_Health = rowSums(qualtrics[ , health_cols], na.rm=TRUE)

# formr
formr = formr |> mutate(Smoke_Vape_Daily = case_when(
  Smoke_Vape_Daily == 1  ~ 1, TRUE ~ 0
))

formr = formr |> mutate(Oral_Condition_Binary = case_when(
  Oral_Condition_Lickert %in% c("Excellent", "Very_good") ~ 0,
  Oral_Condition_Lickert %in% c("Good", "Fair", "Poor") ~ 1
))

health_cols = c("Chronic_Illnesses", "Cancer_Treatment", "Daily_Medications", "Oral_Condition_Binary", "Smoke_Vape_Daily")

formr$Index_Health = rowSums(formr[ , health_cols], na.rm=TRUE)



######################### Index_Olfactory_Change / Recovery / Long_Haul (pre-reg items 80-82)
# qualtrics
qualtrics$Index_Olfactory_Change = (qualtrics$OlfactoryAbility_DuringSmellLoss - qualtrics$OlfactoryAbility_BeforeSmellLoss)
qualtrics$Index_Olfactory_Recovery = (qualtrics$OlfactoryAbility_Current - qualtrics$OlfactoryAbility_DuringSmellLoss)
qualtrics$Index_Olfactory_Long_Haul = (qualtrics$OlfactoryAbility_Current - qualtrics$OlfactoryAbility_BeforeSmellLoss)

# formr
formr$Index_Olfactory_Change = (formr$OlfactoryAbility_DuringSmellLoss - formr$OlfactoryAbility_BeforeSmellLoss)
formr$Index_Olfactory_Recovery = (formr$OlfactoryAbility_Current - formr$OlfactoryAbility_DuringSmellLoss)
formr$Index_Olfactory_Long_Haul = (formr$OlfactoryAbility_Current - formr$OlfactoryAbility_BeforeSmellLoss)


######################### Odor Awareness Index QUALTRICS
# (pre-reg item 77, items 59-62)
#item 59 (Odor_Importance): no "not applicable" option, 5-point scale

# qualtrics
qualtrics = qualtrics |> mutate(Index_Odor_Importance = case_when(
  Odor_Importance == 'Not_important' ~ 1,
  Odor_Importance == 'Slightly_important' ~ 2,
  Odor_Importance == 'Moderately_Important' ~ 3,
  Odor_Importance == 'Important' ~ 4,
  Odor_Importance == 'Very_important' ~ 5
))

#items 60-62: pre-reg specifies "not applicable" and "never" BOTH coded as 1
qualtrics = qualtrics |> mutate(Index_Notice_Other_House_Smells = case_when(
  Notice_Other_House_Smells == 'Not_applicable' ~ 1,
  Notice_Other_House_Smells == 'Never' ~ 1,
  Notice_Other_House_Smells == 'Seldom' ~ 2,
  Notice_Other_House_Smells == 'Sometimes' ~ 3,
  Notice_Other_House_Smells == 'Often' ~ 4,
  Notice_Other_House_Smells == 'Always' ~ 5
))

qualtrics = qualtrics |> mutate(Index_Odors_Revive_Memories_Emotions = case_when(
  Odors_Revive_Memories_Emotions == 'Not_applicable' ~ 1,
  Odors_Revive_Memories_Emotions == 'Never' ~ 1,
  Odors_Revive_Memories_Emotions == 'Seldom' ~ 2,
  Odors_Revive_Memories_Emotions == 'Sometimes' ~ 3,
  Odors_Revive_Memories_Emotions == 'Often' ~ 4,
  Odors_Revive_Memories_Emotions == 'Always' ~ 5
))

qualtrics = qualtrics |> mutate(Index_Notice_Odors = case_when(
  Notice_Odors == 'Not_applicable' ~ 1,
  Notice_Odors == 'Never' ~ 1,
  Notice_Odors == 'Seldom' ~ 2,
  Notice_Odors == 'Sometimes' ~ 3,
  Notice_Odors == 'Often' ~ 4,
  Notice_Odors == 'Always' ~ 5
))


# factor analysis + scree plot to determine number of underlying factors

oa_cols = c("Index_Odor_Importance", "Index_Notice_Other_House_Smells",
            "Index_Odors_Revive_Memories_Emotions", "Index_Notice_Odors")
oa_cor = cor(qualtrics[ , oa_cols], use = "pairwise.complete.obs")
oa_eigen = eigen(oa_cor)$values

png("artifacts/odor_awareness_scree_plot-qualtrics.png")
plot(oa_eigen, type = "b", main = "Odor Awareness Items: Scree Plot",
     xlab = "Factor", ylab = "Eigenvalue")
abline(h = 1, lty = 2)
dev.off()

#Kaiser criterion (eigenvalue > 1) as a starting point
n_factors_kaiser = sum(oa_eigen > 1)
print(paste("Kaiser criterion suggests", n_factors_kaiser, "factor(s) -- confirm against scree plot"))

## unidimensional/1 factor confirmed, sum 4items into a single index, 4-20
qualtrics = qualtrics |> mutate(Index_OA = rowSums(across(all_of(oa_cols)), na.rm = TRUE))

# b/c never/na were counted as 1, the index falsely starts at 4
# substract the never/na responses to get a clean scale
qualtrics$Index_OA = qualtrics$Index_OA -4



######################### Odor Awareness Index FORMR
#item 59 (Odor_Importance): no "not applicable" option, 5-point scale

# formr
formr = formr |> mutate(Index_Odor_Importance = case_when(
  Odor_Importance == 'Not_important' ~ 1,
  Odor_Importance == 'Slightly_important' ~ 2,
  Odor_Importance == 'Moderately_Important' ~ 3,
  Odor_Importance == 'Important' ~ 4,
  Odor_Importance == 'Very_important' ~ 5
))

#items 60-62: pre-reg specifies "not applicable" and "never" BOTH coded as 1
formr = formr |> mutate(Index_Notice_Other_House_Smells = case_when(
  Notice_Other_House_Smells == 'Not_applicable' ~ 1,
  Notice_Other_House_Smells == 'Never' ~ 1,
  Notice_Other_House_Smells == 'Seldom' ~ 2,
  Notice_Other_House_Smells == 'Sometimes' ~ 3,
  Notice_Other_House_Smells == 'Often' ~ 4,
  Notice_Other_House_Smells == 'Always' ~ 5
))

formr = formr |> mutate(Index_Odors_Revive_Memories_Emotions = case_when(
  Odors_Revive_Memories_Emotions == 'Not_applicable' ~ 1,
  Odors_Revive_Memories_Emotions == 'Never' ~ 1,
  Odors_Revive_Memories_Emotions == 'Seldom' ~ 2,
  Odors_Revive_Memories_Emotions == 'Sometimes' ~ 3,
  Odors_Revive_Memories_Emotions == 'Often' ~ 4,
  Odors_Revive_Memories_Emotions == 'Always' ~ 5
))

formr = formr |> mutate(Index_Notice_Odors = case_when(
  Notice_Odors == 'Not_applicable' ~ 1,
  Notice_Odors == 'Never' ~ 1,
  Notice_Odors == 'Seldom' ~ 2,
  Notice_Odors == 'Sometimes' ~ 3,
  Notice_Odors == 'Often' ~ 4,
  Notice_Odors == 'Always' ~ 5
))


# factor analysis + scree plot to determine number of underlying factors

oa_cols = c("Index_Odor_Importance", "Index_Notice_Other_House_Smells",
            "Index_Odors_Revive_Memories_Emotions", "Index_Notice_Odors")
oa_cor = cor(formr[ , oa_cols], use = "pairwise.complete.obs")
oa_eigen = eigen(oa_cor)$values

png("artifacts/odor_awareness_scree_plot-formr.png")
plot(oa_eigen, type = "b", main = "Odor Awareness Items: Scree Plot",
     xlab = "Factor", ylab = "Eigenvalue")
abline(h = 1, lty = 2)
dev.off()

#Kaiser criterion (eigenvalue > 1) as a starting point
n_factors_kaiser = sum(oa_eigen > 1)
print(paste("Kaiser criterion suggests", n_factors_kaiser, "factor(s) -- confirm against scree plot"))

## unidimensional/1 factor confirmed, sum 4items into a single index, 4-20
formr = formr |> mutate(Index_OA = rowSums(across(all_of(oa_cols)), na.rm = TRUE))

# b/c never/na were counted as 1, the index falsely starts at 4
# substract the never/na responses to get a clean scale
formr$Index_OA = formr$Index_OA -4




######################### SAVE INDEXED DATA
write.csv(qualtrics, "data/data-qualtrics-final.csv", quote=FALSE, row.names = FALSE)

write.csv(formr, "data/data-formr-final.csv", quote=FALSE, row.names = FALSE)



######################### TIDY
rm(list = ls())
gc()