######################### LOAD PACKAGES
library(dplyr)       # data wrangling
library(ggplot2)     # plots
library(qqplotr)     # plots
library(car)         # VIF test for colinearity
library(MASS)        # logistic regression model
library(brant)       # proportional odds test
library(factoextra)  # elbow/silhouette for k-means
library(viridis)     # colorblind-friendly palettes
library(gridExtra)   # side-by-side diagnostic panels
library(usmap)       # US state map geometries



######################### HELPER FUNCTIONS: TABLE AND FIGURE EXPORT

# build odds ratio table (term, OR, CI, p) from a glm model
build_or_table_glm = function(model) {
  ct = coef(summary(model))
  or = exp(coef(model))
  ci = exp(confint(model))
  or_table = data.frame(Term = names(coef(model)), OR = or, CI_Low = ci[, 1], CI_High = ci[, 2], P_Value = ct[, "Pr(>|z|)"])
  return(or_table)
}

# forest plot from an OR table, intercept excluded
plot_or_forest = function(or_table) {
  or_table_plot = or_table |> dplyr::filter(Term != "(Intercept)")
  forest_plot = ggplot(or_table_plot, aes(x = OR, y = Term)) +
    geom_point(color = viridis(3, end = 0.6)[2], size = 3) +
    geom_errorbarh(aes(xmin = CI_Low, xmax = CI_High), height = 0.2, color = viridis(3, end = 0.6)[2]) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    labs(x = "Odds Ratio (95% CI)", y = "") +
    theme_classic()
  return(forest_plot)
}



######################### DATA
covid = read.csv('data/data-formr-final.csv', stringsAsFactors = TRUE)



######################### ST LOSS CAUSE CLASSIFICATIONS
# recodes of SmellTasteLoss_Cause_General/VirusSpecific/Diagnosis_Method -- not composite
# indexes, so built here rather than in index.R

# virus vs non-virus cause of smell/taste loss
covid = covid |> mutate(SmellTasteLoss_Cause_Viral = case_when(
  SmellTasteLoss_Cause_General == "Medication_or_Cancer"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Unknown"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Other"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Occupational"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Injury"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Congenital"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Bacterial"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid' & Diagnosis_Method == "Lab_Test" ~ 'Viral',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid' & Diagnosis_Method == "Clinical"   ~ 'Viral',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid'  ~ 'Viral',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Other'  ~ 'Viral',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'NonCovid_Virus'  ~ 'Viral',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Flu'  ~ 'Viral'
))

# pooled covid vs non-covid viral vs non-viral
covid = covid |> mutate(SmellTasteLoss_Cause_Covid = case_when(
  SmellTasteLoss_Cause_General == "Medication_or_Cancer"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Unknown"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Other"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Occupational"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Injury"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Congenital"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Bacterial"  ~ 'NonViral',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid' & Diagnosis_Method == "Lab_Test" ~ 'Viral_Covid',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid' & Diagnosis_Method == "Clinical"   ~ 'Viral_Covid',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Covid'  ~ 'Viral_Covid',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Other'  ~ 'Viral_nonCovid',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'NonCovid_Virus'  ~ 'Viral_nonCovid',
  SmellTasteLoss_Cause_General == "Viral" & SmellTasteLoss_Cause_VirusSpecific == 'Flu'  ~ 'Viral_nonCovid'
))



######################### CHOROPLETH MAP OF LOCATION BY STATE
state_counts = covid |>
  filter(!is.na(State)) |>
  count(State) |>
  mutate(Proportion = n / sum(n)) |>
  rename(state = State)

state_map = plot_usmap(data = state_counts, values = "Proportion", regions = "states") +
  scale_fill_gradient(low = "#f0f9f8", high = "#21908C", name = "Sample Share", na.value = "grey80") +
  theme(legend.position = "right")

ggsave("artifacts/sample_share_by_state_map-formr.png", state_map, width = 8, height = 5)
rm(state_counts, state_map)



######################### GEOGRAPHY AND HOUSEHOLD (EXPLORATORY, PRE-REG)
#### multi-gen is defined in pre-reg as 3+ even though 2 is two generations

### does household index affect covid incidence differentially?
## run per pre-reg
# crosstab
table(covid$Index_Household, covid$SmellTasteLoss_Cause_Viral)

covid$SmellTasteLoss_Cause_Viral = as.factor(covid$SmellTasteLoss_Cause_Viral)
covid$SmellTasteLoss_Cause_Covid = as.factor(covid$SmellTasteLoss_Cause_Covid)

# model
model_household = glm(SmellTasteLoss_Cause_Viral ~ Index_Household,
                      data = covid,
                      family = binomial)

summary(model_household)
# odds ratios with 95% CI
exp(cbind(OR = coef(model_household), confint(model_household)))

# table + figure export
exploratory_household_table = build_or_table_glm(model_household)
write.csv(exploratory_household_table, "artifacts/exploratory_household_model-formr.csv", quote = FALSE, row.names = FALSE)
exploratory_household_forest = plot_or_forest(exploratory_household_table)
exploratory_household_forest
ggsave("artifacts/exploratory_or_household-formr.png", exploratory_household_forest, width = 6, height = 4)


## run with multi
# create index
covid = covid |> mutate(Index_Household_Modified = case_when(
  Location == "Urban" & Generations_per_Household == 1 ~ 'Urban_1_Generation',
  Location == "Urban" & Generations_per_Household == 2 ~ 'Urban_2_Generations',
  Location == "Urban" & Generations_per_Household > 2  ~ 'Urban_3Plus_Generations',
  Location == "Rural" & Generations_per_Household == 1 ~ 'Rural_1_Generation',
  Location == "Rural" & Generations_per_Household == 2 ~ 'Rural_2_Generations',
  Location == "Rural" & Generations_per_Household > 2  ~ 'Rural_3Plus_Generations'
))
covid$Index_Household_Modified = as.factor(covid$Index_Household_Modified)

# crosstab
table(covid$Index_Household_Modified, covid$SmellTasteLoss_Cause_Viral)

# model
model_household_v2 = glm(SmellTasteLoss_Cause_Viral ~ Index_Household_Modified,
                         data = covid,
                         family = binomial)
summary(model_household_v2)
exp(cbind(OR = coef(model_household_v2), confint(model_household_v2)))

# table + figure export, supplementary/diagnostic only
exploratory_household_v2_table = build_or_table_glm(model_household_v2)
write.csv(exploratory_household_v2_table, "artifacts/exploratory_household_v2_model-formr.csv", quote = FALSE, row.names = FALSE)
exploratory_household_v2_forest = plot_or_forest(exploratory_household_v2_table)
exploratory_household_v2_forest
ggsave("artifacts/exploratory_or_household_v2-formr.png", exploratory_household_v2_forest, width = 7, height = 5)


### do geography or environment impact
model_location_env = glm(SmellTasteLoss_Cause_Viral ~ Location * Index_Environment,
                         data = covid,
                         family = binomial)
summary(model_location_env)
exp(cbind(OR = coef(model_location_env), confint(model_location_env)))

# table + figure export
exploratory_location_env_table = build_or_table_glm(model_location_env)
write.csv(exploratory_location_env_table, "artifacts/exploratory_location_environment_model-formr.csv", quote = FALSE, row.names = FALSE)
exploratory_location_env_forest = plot_or_forest(exploratory_location_env_table)
exploratory_location_env_forest
ggsave("artifacts/exploratory_or_location_environment-formr.png", exploratory_location_env_forest, width = 6, height = 4)



## RESULTS: sparse cells prevent interpretation



######################### ORAL HEALTH (EXPLORATORY, PRE-REG)
## virus
model_oral_viral_int = glm(SmellTasteLoss_Cause_Viral ~ Oral_Condition_Binary * Index_SES + Oral_Condition_Binary * Index_Needs,
                           data = covid, family = binomial)
summary(model_oral_viral_int)

model_oral_viral = glm(SmellTasteLoss_Cause_Viral ~ Oral_Condition_Binary + Index_SES + Index_Needs,
                       data = covid, family = binomial)
oral_viral_anova = anova(model_oral_viral, model_oral_viral_int, test = "Chisq")
oral_viral_anova
summary(model_oral_viral)

# table + figure export
oralhealth_viral_table = build_or_table_glm(model_oral_viral)
write.csv(oralhealth_viral_table, "artifacts/oralhealth_viral_model-formr.csv", quote = FALSE, row.names = FALSE)
oralhealth_viral_forest = plot_or_forest(oralhealth_viral_table)
oralhealth_viral_forest
ggsave("artifacts/oralhealth_or_viral-formr.png", oralhealth_viral_forest, width = 6, height = 4)

# interaction model + anova, supplementary
oralhealth_viral_int_table = build_or_table_glm(model_oral_viral_int)
write.csv(oralhealth_viral_int_table, "artifacts/oralhealth_viral_interaction_model-formr.csv", quote = FALSE, row.names = FALSE)
oral_viral_anova_table = as.data.frame(oral_viral_anova)
write.csv(oral_viral_anova_table, "artifacts/oralhealth_viral_anova-formr.csv", quote = FALSE, row.names = TRUE)

# covid only
covid_covid_vs_nonviral = covid[covid$SmellTasteLoss_Cause_Covid != "Viral_nonCovid", ]
covid_covid_vs_nonviral$Covid_Binary = ifelse(covid_covid_vs_nonviral$SmellTasteLoss_Cause_Covid == "Viral_Covid", 1, 0)

model_oral_covid_int = glm(Covid_Binary ~ Oral_Condition_Binary * Index_SES + Oral_Condition_Binary * Index_Needs,
                           data = covid_covid_vs_nonviral, family = binomial)
summary(model_oral_covid_int)

model_oral_covid = glm(Covid_Binary ~ Oral_Condition_Binary + Index_SES + Index_Needs,
                       data = covid_covid_vs_nonviral, family = binomial)
summary(model_oral_covid)

oral_covid_anova = anova(model_oral_covid, model_oral_covid_int, test = "Chisq")
oral_covid_anova

# table + figure export
oralhealth_covid_table = build_or_table_glm(model_oral_covid)
write.csv(oralhealth_covid_table, "artifacts/oralhealth_covid_model-formr.csv", quote = FALSE, row.names = FALSE)
oralhealth_covid_forest = plot_or_forest(oralhealth_covid_table)
oralhealth_covid_forest
ggsave("artifacts/oralhealth_or_covid-formr.png", oralhealth_covid_forest, width = 6, height = 4)

# interaction model + anova, supplementary
oralhealth_covid_int_table = build_or_table_glm(model_oral_covid_int)
write.csv(oralhealth_covid_int_table, "artifacts/oralhealth_covid_interaction_model-formr.csv", quote = FALSE, row.names = FALSE)
oral_covid_anova_table = as.data.frame(oral_covid_anova)
write.csv(oral_covid_anova_table, "artifacts/oralhealth_covid_anova-formr.csv", quote = FALSE, row.names = TRUE)


## RESULTS: not significant



######################### DIFF IN OA BY SES AND RACE
##### ARE THERE BASELINE DIFFERENCES IN CHEMOSENSORY ABILITY IN DIFFERENT SES AND RACE GROUPS?
# FormR H1: multivariate regression, OA as DV, household income/resources (Index_SES, Index_Needs),
# environmental exposure, and health items as IVs

#### ASSUMPTION TESTING
# testing assumptions for linear regression

### NORMAL DISTRIBUTION
qq_oa_current = ggplot(covid, aes(sample = OlfactoryAbility_Current)) +
  stat_qq_band(distribution = "norm") +
  stat_qq_line(distribution = "norm") +
  stat_qq_point(distribution = "norm") +
  labs(x = "Normal Distribution Quantiles", y = "Current OA") +
  theme_classic()
qq_oa_current
ggsave("artifacts/normalTest-ability-formr.png", qq_oa_current, width = 6, height = 5)

### INDEPENDENCE OF OBSERVATIONS
# satisfied by design (paid survey, independent individual respondents)


### MULTICOLINEARITY
vif(polr(as.ordered(OlfactoryAbility_Current) ~ Index_SES + Index_Needs + Index_Environment + Index_Health, data = covid))


### PROPORTIONAL ODDS FOR LOGISTIC REGRESSION
# test cell sizes and distribution for Brant test
table(covid$OlfactoryAbility_Current, covid$Index_SES)
table(covid$OlfactoryAbility_Current, covid$MinorityEthnic_Race_Status_Binary)
table(covid$OlfactoryAbility_Current, covid$Index_Needs)
table(covid$OlfactoryAbility_Current, covid$Index_Environment)
table(covid$OlfactoryAbility_Current, covid$Index_Health)

#### collapse to fewer ranks
# 0-3 mild / >3-7 moderate / >7-10 severe

## references:
# source: Fokkens WJ, Lund VJ, Mullol J, Bachert C, Alobid I, Baroody F, et al. "EPOS 2012: European position paper on rhinosinusitis and nasal polyps 2012. A summary for otorhinolaryngologists." Rhinology. 2012 Mar;50(1):1-12.
# application: Izquierdo-Dominguez A, Rojas-Lechuga MJ, Chiesa-Estomba C, et al. "Smell and Taste Dysfunction in COVID-19 Is Associated With Younger Age in Ambulatory Settings: A Multicenter Cross-Sectional Study." Journal of Investigational Allergology and Clinical Immunology.

# reduce OA current into fewer categories based on above
covid = covid |> mutate(OlfactoryAbility_Current_Reduced = case_when(
  OlfactoryAbility_Current >= 7 & OlfactoryAbility_Current <= 10 ~ 'Mild_or_No_Loss', OlfactoryAbility_Current >= 3 & OlfactoryAbility_Current < 7   ~ 'Moderate_Loss', OlfactoryAbility_Current >= 0 & OlfactoryAbility_Current < 3   ~ 'Severe_Loss'
))
covid$OlfactoryAbility_Current_Reduced = factor(covid$OlfactoryAbility_Current_Reduced, levels = c('Severe_Loss', 'Moderate_Loss', 'Mild_or_No_Loss'), ordered = TRUE)

### BRANT TEST
brant(polr(as.ordered(OlfactoryAbility_Current_Reduced) ~ Index_SES, data = covid)) #holds
brant(polr(as.ordered(OlfactoryAbility_Current_Reduced) ~ MinorityEthnic_Race_Status_Binary, data = covid)) # holds
brant(polr(as.ordered(OlfactoryAbility_Current_Reduced) ~ Index_SES + Index_Needs + Index_Environment + Index_Health, data = covid)) # violation



## RESULTS: assumption violated, not run




######################### HYPOTHESIS 2: IS ST LOSS PREDICTIVE FOR DX BY RACE/SES (PRE-REG MODEL)
##### IS SMELL LOSS THE PRIMARY PREDICTIVE SYMPTOM OF COVID-19 IN MARGINALIZED POPULATIONS?
# filter to only those that responded to having experienced a prior smell loss (whether due to COVID or other respiratory illnesses)

# filter
covid2 = covid |> filter(
  Experienced_SmellTasteLoss == "Yes",
  SmellTasteLoss_Cause_VirusSpecific == "Covid"
)

## recode diagnosis variable to 0/1
covid2 = covid2 |> mutate(Dx_Binary = case_when(
  Diagnosis_due_to_SmellTasteLoss == 1 ~ 1,
  Diagnosis_due_to_SmellTasteLoss == 2 ~ 0
))

#### ASSUMPTION TESTING
### INDEPENDENCE OF OBSERVATIONS
# satisfied by design (paid survey, independent individual respondents)


### MULTICOLINEARITY
model_dx = glm(Dx_Binary ~ Index_SES + MinorityEthnic_Race_Status_Binary + Index_Health, data = covid2, family = binomial)
vif(model_dx)


### LINEARITY OF THE LOGIT
covid2 = covid2 |> mutate(Index_Health_Modified = Index_Health + 1)
covid2 = covid2 |> mutate(Health_LogInteraction = Index_Health_Modified * log(Index_Health_Modified))
# only index_health tested because others are binary

model_linearity_check = glm(Dx_Binary ~ Index_Health_Modified + Health_LogInteraction, data = covid2, family = binomial)
summary(model_linearity_check)


#### MODEL AS LITERALLY SPECIFIED IN PRE-REG ANALYSIS PARAGRAPH
## multivariate regression using diagnosis as the dependent variable and SES and race as the independent variable, health as confounder

# model
model_dx = glm(Dx_Binary ~ Index_SES + MinorityEthnic_Race_Status_Binary + Index_Health, data = covid2, family = binomial)
summary(model_dx)
exp(cbind(OR = coef(model_dx), confint(model_dx)))

# table + figure export
dx_model_table = build_or_table_glm(model_dx)
write.csv(dx_model_table, "artifacts/h2_dx_model-formr.csv", quote = FALSE, row.names = FALSE)
dx_model_forest = plot_or_forest(dx_model_table)
dx_model_forest
ggsave("artifacts/h2_dx_or_forest-formr.png", dx_model_forest, width = 6, height = 4)


#### POWER ANALYSIS (SIMULATION-BASED)
n_sims = 1000
alpha = 0.05
n_obs = nrow(covid2)

dx_coefs = coef(model_dx)
dx_predictors = covid2 |> dplyr::select(Index_SES, MinorityEthnic_Race_Status_Binary, Index_Health)

sig_count_dx = matrix(0, nrow = 1, ncol = length(dx_coefs) - 1)
colnames(sig_count_dx) = names(dx_coefs)[-1]

for (i in 1:n_sims) {
  sim_idx = sample(1:n_obs, n_obs, replace = TRUE)
  sim_predictors = dx_predictors[sim_idx, ]

  X = model.matrix(~ Index_SES + MinorityEthnic_Race_Status_Binary + Index_Health, data = sim_predictors)
  eta = X %*% dx_coefs
  prob = plogis(eta)
  sim_y = rbinom(n_obs, 1, prob)

  sim_data = cbind(sim_predictors, Dx_Binary = sim_y)
  fit = tryCatch(glm(Dx_Binary ~ Index_SES + MinorityEthnic_Race_Status_Binary + Index_Health, data = sim_data, family = binomial),
                 error = function(e) NULL)
  if (!is.null(fit)) {
    ct = coef(summary(fit))
    for (v in colnames(sig_count_dx)) {
      if (v %in% rownames(ct) && ct[v, "Pr(>|z|)"] < alpha) {
        sig_count_dx[1, v] = sig_count_dx[1, v] + 1
      }
    }
  }
}
power_dx = sig_count_dx / n_sims
print(power_dx)


## RESULTS: not significant with trend for high income brackets slightly more likely to seek DX, underpowered test



######################### CLUSTER ANALYSIS 1: OA MEASURES AGAINST RACE/SES (PRE-REG)
## cluster analysis for SES and for race (separately) and the following variables: ability to smell before infection, ability to smell after infection, and ability to smell currently.
# prediction: changes in smell and taste will be less variable in lower SES and minority race.

## create oa vars
oa_traj_vars = covid2 |> dplyr::select(OlfactoryAbility_BeforeSmellLoss, OlfactoryAbility_DuringSmellLoss, OlfactoryAbility_Current) |>
  na.omit()

# determine k clusters, elbow
oa_traj_wss = fviz_nbclust(oa_traj_vars, kmeans, method = "wss")

# determine k clusters, silhouette
oa_traj_sil = fviz_nbclust(oa_traj_vars, kmeans, method = "silhouette")

# combined diagnostic panel, A = elbow, B = silhouette
oa_traj_diagnostic = grid.arrange(oa_traj_wss, oa_traj_sil, ncol = 2)
ggsave("artifacts/h2_elbow_silhouette_oa_trajectory-formr.png", oa_traj_diagnostic, width = 10, height = 5)


## SES ANALYSIS
set.seed(123)
cluster_ses = kmeans(oa_traj_vars, centers = 2, nstart = 25)

covid2_cluster_ses = covid2 |> dplyr::filter(!is.na(OlfactoryAbility_BeforeSmellLoss) &
                                               !is.na(OlfactoryAbility_DuringSmellLoss) &
                                               !is.na(OlfactoryAbility_Current))
covid2_cluster_ses$OA_Cluster = cluster_ses$cluster

table(covid2_cluster_ses$OA_Cluster, covid2_cluster_ses$Index_SES)

# Fisher's exact (recommended given small cells)
fisher.test(table(covid2_cluster_ses$OA_Cluster, covid2_cluster_ses$Index_SES))

# check whether the lower-SES cluster has smaller sd for 'less variable' prediction
covid2_cluster_ses |>
  group_by(OA_Cluster) |>
  summarise(
    sd_before = sd(OlfactoryAbility_BeforeSmellLoss, na.rm=TRUE),
    sd_during = sd(OlfactoryAbility_DuringSmellLoss, na.rm=TRUE),
    sd_current = sd(OlfactoryAbility_Current, na.rm=TRUE),
    n = n()
  )

# cluster plot, SES
h2_cluster_oa_traj_ses = fviz_cluster(cluster_ses, data = oa_traj_vars, geom = "point") +
  scale_color_viridis_d(end = 0.6) +
  scale_fill_viridis_d(end = 0.6) +
  theme_classic()
h2_cluster_oa_traj_ses
ggsave("artifacts/h2_cluster_oa_trajectory_ses-formr.png", h2_cluster_oa_traj_ses, width = 6, height = 5)

## RACE ANALYSIS
set.seed(123)
cluster_race = kmeans(oa_traj_vars, centers = 2, nstart = 25)

covid2_cluster_race = covid2 |> dplyr::filter(!is.na(OlfactoryAbility_BeforeSmellLoss) &
                                                !is.na(OlfactoryAbility_DuringSmellLoss) &
                                                !is.na(OlfactoryAbility_Current))
covid2_cluster_race$OA_Cluster = cluster_race$cluster

table(covid2_cluster_race$OA_Cluster, covid2_cluster_race$MinorityEthnic_Race_Status_Binary)

# Fisher's exact (recommended given small cells)
fisher.test(table(covid2_cluster_race$OA_Cluster, covid2_cluster_race$MinorityEthnic_Race_Status_Binary))

# check whether the minority-race cluster has smaller sd for 'less variable' prediction
covid2_cluster_race |>
  group_by(OA_Cluster) |>
  summarise(
    sd_before = sd(OlfactoryAbility_BeforeSmellLoss, na.rm=TRUE),
    sd_during = sd(OlfactoryAbility_DuringSmellLoss, na.rm=TRUE),
    sd_current = sd(OlfactoryAbility_Current, na.rm=TRUE),
    n = n()
  )

# cluster plot, race
h2_cluster_oa_traj_race = fviz_cluster(cluster_race, data = oa_traj_vars, geom = "point") +
  scale_color_viridis_d(end = 0.6) +
  scale_fill_viridis_d(end = 0.6) +
  theme_classic()
h2_cluster_oa_traj_race
ggsave("artifacts/h2_cluster_oa_trajectory_race-formr.png", h2_cluster_oa_traj_race, width = 6, height = 5)

## RESULTS: significant, but the smaller-SD cluster skewed higher-SES — prediction not upheld (opposite direction), and even weaker support given the "during" SD tie.



######################### CLUSTER ANALYSIS 2: ODOR AWARENESS MEASURES AGAINST RACE/SES (PRE-REG)
## cluster analysis for SES and for race (separately) and the following variables: odor importance, noticing house smells, odors evoking memories/emotions, noticing odors
# prediction: lower SES and race will be less likely to agree with those statements

# create odor awareness vars
odor_vars = covid2 |> dplyr::select(Index_Odor_Importance, Index_Notice_Other_House_Smells, Index_Odors_Revive_Memories_Emotions, Index_Notice_Odors) |> na.omit()

# determine k clusters, elbow
odor_wss = fviz_nbclust(odor_vars, kmeans, method = "wss")

# determine k clusters, silhouette
odor_sil = fviz_nbclust(odor_vars, kmeans, method = "silhouette")

# combined diagnostic panel, A = elbow, B = silhouette
odor_diagnostic = grid.arrange(odor_wss, odor_sil, ncol = 2)
ggsave("artifacts/h2_elbow_silhouette_odor_importance-formr.png", odor_diagnostic, width = 10, height = 5)

## SES ANALYSIS
set.seed(123)
cluster_odor_ses = kmeans(odor_vars, centers = 2, nstart = 25)
covid2_cluster_odor_ses = covid2 |> dplyr::filter(!is.na(Index_Odor_Importance) &
                                                    !is.na(Index_Notice_Other_House_Smells) &
                                                    !is.na(Index_Odors_Revive_Memories_Emotions) &
                                                    !is.na(Index_Notice_Odors))
covid2_cluster_odor_ses$Odor_Cluster = cluster_odor_ses$cluster
table(covid2_cluster_odor_ses$Odor_Cluster, covid2_cluster_odor_ses$Index_SES)

# Fisher's exact (recommended given small cells)
fisher.test(table(covid2_cluster_odor_ses$Odor_Cluster, covid2_cluster_odor_ses$Index_SES))

# check whether the lower-SES cluster shows lower agreement (lower index values) for 'less likely to agree' prediction
covid2_cluster_odor_ses |>
  group_by(Odor_Cluster) |>
  summarise(
    mean_importance = mean(Index_Odor_Importance, na.rm=TRUE),
    mean_house_smells = mean(Index_Notice_Other_House_Smells, na.rm=TRUE),
    mean_memories = mean(Index_Odors_Revive_Memories_Emotions, na.rm=TRUE),
    mean_notice = mean(Index_Notice_Odors, na.rm=TRUE),
    n = n()
  )

# cluster plot, SES
h2_cluster_odor_ses_plot = fviz_cluster(cluster_odor_ses, data = odor_vars, geom = "point") +
  scale_color_viridis_d(end = 0.6) +
  scale_fill_viridis_d(end = 0.6) +
  theme_classic()
h2_cluster_odor_ses_plot
ggsave("artifacts/h2_cluster_odor_importance_ses-formr.png", h2_cluster_odor_ses_plot, width = 6, height = 5)


## RACE ANALYSIS
set.seed(123)
cluster_odor_race = kmeans(odor_vars, centers = 2, nstart = 25)
covid2_cluster_odor_race = covid2 |> dplyr::filter(!is.na(Index_Odor_Importance) &
                                                     !is.na(Index_Notice_Other_House_Smells) &
                                                     !is.na(Index_Odors_Revive_Memories_Emotions) &
                                                     !is.na(Index_Notice_Odors))
covid2_cluster_odor_race$Odor_Cluster = cluster_odor_race$cluster
table(covid2_cluster_odor_race$Odor_Cluster, covid2_cluster_odor_race$MinorityEthnic_Race_Status_Binary)

# Fisher's exact (recommended given small cells)
fisher.test(table(covid2_cluster_odor_race$Odor_Cluster, covid2_cluster_odor_race$MinorityEthnic_Race_Status_Binary))

# check whether the minority-race cluster shows lower agreement for 'less likely to agree' prediction
covid2_cluster_odor_race |>
  group_by(Odor_Cluster) |>
  summarise(
    mean_importance = mean(Index_Odor_Importance, na.rm=TRUE),
    mean_house_smells = mean(Index_Notice_Other_House_Smells, na.rm=TRUE),
    mean_memories = mean(Index_Odors_Revive_Memories_Emotions, na.rm=TRUE),
    mean_notice = mean(Index_Notice_Odors, na.rm=TRUE),
    n = n()
  )

# cluster plot, race
h2_cluster_odor_race_plot = fviz_cluster(cluster_odor_race, data = odor_vars, geom = "point") +
  scale_color_viridis_d(end = 0.6) +
  scale_fill_viridis_d(end = 0.6) +
  theme_classic()
h2_cluster_odor_race_plot
ggsave("artifacts/h2_cluster_odor_importance_race-formr.png", h2_cluster_odor_race_plot, width = 6, height = 5)

## RESULTS: SES not sig; Race sig--odor awareness higher in predominantly (but not exlcusively) minority cluster



######################### H1 EXPLORATORY QUESTIONS (PRE-REG)

#### RESPIRATORY ILLNESS RATES BY RACE/SES
## Does respiratory illness occur at similar rates across race/SES groups, or is there a racial buffer for whites?

## set No (non-minority) as reference category for race comparisons
covid = covid |> mutate(MinorityEthnic_Race_Status_Binary = relevel(as.factor(MinorityEthnic_Race_Status_Binary), ref = "No"))

## race only
rate_respill_race = glm(SmellTasteLoss_Cause_Viral ~ MinorityEthnic_Race_Status_Binary, data = covid, family = binomial)
summary(rate_respill_race)
exp(cbind(OR = coef(rate_respill_race), confint(rate_respill_race)))

## SES only
rate_respill_ses = glm(SmellTasteLoss_Cause_Viral ~ Index_SES, data = covid, family = binomial)
summary(rate_respill_ses)
exp(cbind(OR = coef(rate_respill_ses), confint(rate_respill_ses)))


#### COVID INFECTION RATES

## Are there differences across races and SES in COVID-19 symptoms?
covid_viral_only = covid |> dplyr::filter(SmellTasteLoss_Cause_Viral == "Viral") |>
  mutate(SmellTasteLoss_Cause_Covid = droplevels(as.factor(SmellTasteLoss_Cause_Covid)))

## set No (non-minority) as reference category for race comparisons
covid_viral_only = covid_viral_only |> mutate(MinorityEthnic_Race_Status_Binary = relevel(as.factor(MinorityEthnic_Race_Status_Binary), ref = "No"))

## race only
rate_covid_race = glm(SmellTasteLoss_Cause_Covid ~ MinorityEthnic_Race_Status_Binary, data = covid_viral_only, family = binomial)
summary(rate_covid_race)
exp(cbind(OR = coef(rate_covid_race), confint(rate_covid_race)))

## SES only
rate_covid_ses = glm(SmellTasteLoss_Cause_Covid ~ Index_SES, data = covid_viral_only, family = binomial)
summary(rate_covid_ses)
exp(cbind(OR = coef(rate_covid_ses), confint(rate_covid_ses)))


## RESULTS: not sig and rate_covid_race has sparse cells/model failed



######################### ODOR AWARENESS SES AND RACE (PRE-REG HYPOTHESIS)
### normality of odor awareness index (Index_OA)
qq_index_oa = ggplot(covid, aes(sample = Index_OA)) +
  stat_qq_band(distribution = "norm") +
  stat_qq_line(distribution = "norm") +
  stat_qq_point(distribution = "norm") +
  labs(x = "Normal Distribution Quantiles", y = "Odor Awareness Index") +
  theme_classic()
qq_index_oa
ggsave("artifacts/oa_qq_index_oa-formr.png", qq_index_oa, width = 6, height = 5)

### wilcoxon rank-sum: minority vs non-minority on odor awareness index
# race is already binary (MinorityEthnic_Race_Status_Binary) -- this replaces both the
# qualtrics-script wilcoxon (binary) and kruskal-wallis (3-way) tests, since there is no
# 3-way race variable in the formr data
oa_wilcoxon_result = wilcox.test(Index_OA ~ MinorityEthnic_Race_Status_Binary, data = covid)
oa_wilcoxon_result
# table export
oa_wilcoxon_table = data.frame(Statistic = "W", Value = oa_wilcoxon_result$statistic, P_Value = oa_wilcoxon_result$p.value)
write.csv(oa_wilcoxon_table, "artifacts/oa_wilcoxon_race_binary-formr.csv", quote = FALSE, row.names = FALSE)

# boxplot, binary race
# replaces the qualtrics-script 3-way race boxplot, since formr race is binary
oa_race_boxplot = ggplot(covid, aes(x = MinorityEthnic_Race_Status_Binary, y = Index_OA, fill = MinorityEthnic_Race_Status_Binary)) +
  geom_boxplot() +
  scale_fill_viridis_d(end = 0.6, guide = "none") +
  labs(x = "Minority Race Status", y = "Odor Awareness Index") +
  theme_classic()
oa_race_boxplot
ggsave("artifacts/oa_boxplot_index_oa_by_race-formr.png", oa_race_boxplot, width = 6, height = 5)

### kruskal-wallis: ses (3-level) vs odor awareness index
kruskal_ses = kruskal.test(Index_OA ~ Index_SES, data = covid)
kruskal_ses
# table export
oa_kruskal_ses_table = data.frame(Statistic = "Chi_Squared", Value = kruskal_ses$statistic, DF = kruskal_ses$parameter, P_Value = kruskal_ses$p.value)
write.csv(oa_kruskal_ses_table, "artifacts/oa_kruskal_ses-formr.csv", quote = FALSE, row.names = FALSE)

### wilcoxon rank-sum: needs (2-level) vs odor awareness index
wilcoxon_needs = wilcox.test(Index_OA ~ Index_Needs, data = covid)
wilcoxon_needs

### kruskal-wallis: household (nominal, 4-level) vs odor awareness index
kruskal_household = kruskal.test(Index_OA ~ Index_Household, data = covid)
kruskal_household
# table export
oa_kruskal_household_table = data.frame(Statistic = "Chi_Squared", Value = kruskal_household$statistic, DF = kruskal_household$parameter, P_Value = kruskal_household$p.value)
write.csv(oa_kruskal_household_table, "artifacts/oa_kruskal_household-formr.csv", quote = FALSE, row.names = FALSE)

### kendall's tau: environment vs odor awareness index
kendall_environment = cor.test(covid$Index_Environment, covid$Index_OA, method = "kendall")
kendall_environment

# combined table export, wilcoxon + kendall
oa_other_table = data.frame(
  Predictor = c("Index_Needs", "Index_Environment"),
  Statistic = c(wilcoxon_needs$statistic, kendall_environment$estimate),
  P_Value = c(wilcoxon_needs$p.value, kendall_environment$p.value)
)
write.csv(oa_other_table, "artifacts/oa_wilcoxon_kendall_needs_environment-formr.csv", quote = FALSE, row.names = FALSE)


## RESULTS: not significant



######################### TIDY
rm(list = ls())
gc()
