######################### LOAD PACKAGES
library(dplyr)       # data wrangling
library(classInt)    # jenks breaks for indexes
library(usmap)       # US state map geometries
library(ggplot2)     # plots
library(MASS)        # logistic regression model
library(brant)       # proportional odds test
library(emmeans)     # post-hoc comparative tests
library(viridis)     # colorblind-friendly palettes



######################### DATA
## qualtrics, only low income sample
qualtrics = read.csv('data/data-qualtrics-final.csv', stringsAsFactors = TRUE)

# change name of income bracket to mirror formr
qualtrics = qualtrics |> rename(Index_SES_US = Index_SES)

# create new index for index_ses to mirror formr
qualtrics = qualtrics |> mutate(Index_SES = as.factor(case_when(
  Compare_Income_GeneralPopulation %in% c("Much_less", "Moderately_less", "Slightly_less") ~ "less",
  Compare_Income_GeneralPopulation == "About_the_same" ~ "same",
  Compare_Income_GeneralPopulation %in% c("Slightly_more", "Moderately_more", "Much_more") ~ "more"
)))

# remove multigen household, missing in formr
qualtrics = qualtrics |> dplyr::select(-MultiGeneration_Household)

## formr
formr = read.csv('data/data-formr-final.csv', stringsAsFactors = TRUE)
# change formr to number
formr = formr |> mutate(Diagnosis_due_to_SmellTasteLoss = case_when(
  Diagnosis_due_to_SmellTasteLoss == 1 ~ "Yes",
  Diagnosis_due_to_SmellTasteLoss == 2 ~ "No"
))


## join data
covid = bind_rows(qualtrics, formr)
rm(formr, qualtrics)






######################### IS ABILITY RELATED TO AWARENESS?
# given the null findings and lack of prior information on relationship between awareness and self-reported ability, we updated the approach to first check if awareness was confounding ability

kendall_oa_ability = cor.test(covid$Index_OA, covid$OlfactoryAbility_Current, method = "kendall")
kendall_oa_ability


# table export for 10 point since results are similar
oa_ability_kendall_table = data.frame(
  Predictor = "OlfactoryAbility_Current",
  Tau = kendall_oa_ability$estimate,
  P_Value = kendall_oa_ability$p.value
)
write.csv(oa_ability_kendall_table, "outputs/oa_kendall_ability_current.csv", quote = FALSE, row.names = FALSE)
# values are highly positively correlated, suggesting masking
# z = 6.7, p-value = 0.00000000002, tau = 0.1589
# IMPLICATION: the use of self-reported OA may be obscuring hypothesis testing since this subjective relationship is strong

# scatter/jitter, both variables are discrete/ordinal
oa_ability_scatter = ggplot(covid, aes(x = OlfactoryAbility_Current, y = Index_OA)) +
  geom_jitter(color = viridis(3, end = 0.6)[2], width = 0.2, height = 0.2, alpha = 0.5) +
  labs(x = "Current Olfactory Ability", y = "Odor Awareness Index") +
  theme_classic()
oa_ability_scatter
ggsave("outputs/oa_scatter_awareness_by_ability.png", oa_ability_scatter, width = 6, height = 5)


### AWARENESS-ADJUSTED ABILITY INDEX
# ability regressed on awareness; residuals added back to grand mean to keep adjusted
# score on the original 0-10 scale rather than centered at zero
model_ability_awareness = lm(OlfactoryAbility_Current ~ Index_OA, data = covid)
summary(model_ability_awareness)

covid = covid |> mutate(
  ability_scale_to_awareness = mean(OlfactoryAbility_Current, na.rm = TRUE) + resid(model_ability_awareness)
)

# reapply the same clinical severity cutpoints used for Index_OlfactoryAbility_Current
covid = covid |> mutate(OlfactoryAbility_Current_Scaled = case_when(
  ability_scale_to_awareness >= 7 ~ 'Mild_or_No_Loss',
  ability_scale_to_awareness >= 3 & ability_scale_to_awareness < 7 ~ 'Moderate_Loss',
  ability_scale_to_awareness < 3 ~ 'Severe_Loss'
))

# make factors
covid$OlfactoryAbility_Current_Scaled = factor(covid$OlfactoryAbility_Current_Scaled, levels = c('Severe_Loss', 'Moderate_Loss', 'Mild_or_No_Loss'), ordered = TRUE)





######################### IS INDEX_SES BUILT ON VALID GROUPS?
## testing prior assumptions about index_ses groups
# index_ses was created as close as possible to pre-reg but wasn't test for validation b/c that procedure was not in pre-reg
# testing what the natural breaks are in the var used for index_ses

# jenks by var approach, ability
ability_by_level = covid |>
  dplyr::filter(!is.na(Compare_Income_GeneralPopulation), Compare_Income_GeneralPopulation != "Unsure") |>
  group_by(Compare_Income_GeneralPopulation) |>
  summarise(mean_ability = mean(ability_scale_to_awareness, na.rm = TRUE))
ability_by_level
classIntervals(ability_by_level$mean_ability, n = 3, style = "jenks")

# jenks by var approach, health
health_by_level = covid |>
  dplyr::filter(!is.na(Compare_Income_GeneralPopulation), Compare_Income_GeneralPopulation != "Unsure") |>
  group_by(Compare_Income_GeneralPopulation) |>
  summarise(mean_health = mean(Index_Health, na.rm = TRUE))
health_by_level
classIntervals(health_by_level$mean_health, n = 3, style = "jenks")

# jenks by var approach, dx
## recode diagnosis variable to 0/1
covid = covid |> mutate(Dx_Binary = case_when(
  Diagnosis_due_to_SmellTasteLoss == "Yes" ~ 1,
  Diagnosis_due_to_SmellTasteLoss == "No" ~ 0
))


dx_by_level = covid |>
  dplyr::filter(!is.na(Compare_Income_GeneralPopulation), Compare_Income_GeneralPopulation != "Unsure") |>
  group_by(Compare_Income_GeneralPopulation) |>
  summarise(mean_dx = mean(Dx_Binary, na.rm = TRUE))
dx_by_level
classIntervals(dx_by_level$mean_dx, n = 3, style = "jenks")

# table export
write.csv(ability_by_level, "outputs/jenks_ability_by_level.csv", quote = FALSE, row.names = FALSE)
write.csv(health_by_level, "outputs/jenks_health_by_level.csv", quote = FALSE, row.names = FALSE)
write.csv(dx_by_level, "outputs/jenks_dx_by_level.csv", quote = FALSE, row.names = FALSE)


## RESULTS: bins don't perfectly track any single outcome's natural structure, decision to retain original bins based on pre-reg.



######################### HELPER FUNCTIONS: TABLE AND FIGURE EXPORT
# build odds ratio table (term, OR, CI, p) from a glm model
build_or_table_glm = function(model) {
  ct = coef(summary(model))
  or = exp(coef(model))
  ci = exp(confint(model))
  or_table = data.frame(Term = names(coef(model)), OR = or, CI_Low = ci[, 1], CI_High = ci[, 2], P_Value = ct[, "Pr(>|z|)"])
  return(or_table)
}

# build odds ratio table (term, OR, CI, p) from a polr model, dropping cutpoints
build_or_table_polr = function(model) {
  ct = coef(summary(model))
  ct = ct[!grepl("\\|", rownames(ct)), , drop = FALSE]
  p_value = pnorm(abs(ct[, "t value"]), lower.tail = FALSE) * 2
  or = exp(ct[, "Value"])
  ci = exp(confint(model))
  if (is.null(dim(ci))) ci = matrix(ci, nrow = 1, dimnames = list(rownames(ct), c("2.5 %", "97.5 %")))
  or_table = data.frame(Term = rownames(ct), OR = or, CI_Low = ci[, 1], CI_High = ci[, 2], P_Value = p_value)
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





######################### ASSUMPTION TESTING
### independence of observations
## RESULTS: satisfied by design (paid survey, independent individual respondents)

### proportional odds
brant(polr(as.ordered(OlfactoryAbility_Current_Scaled) ~ Index_SES, data = covid))
brant(polr(as.ordered(OlfactoryAbility_Current_Scaled) ~ MinorityEthnic_Race_Status_Binary, data = covid))



######################### ABLITY DIFFERENCES
### the research questions was never formed as a hypothesis in pre-reg; the specified model assessed factors that may alter ability but differences

## SES MODEL
model_ses_ability = polr(OlfactoryAbility_Current_Scaled ~ Index_SES, data = covid, Hess = TRUE)
summary(model_ses_ability)
ctable_ses_ability = coef(summary(model_ses_ability)) # p-values
p_ses_ability = pnorm(abs(ctable_ses_ability[, "t value"]), lower.tail = FALSE) * 2
ctable_ses_ability = round(cbind(ctable_ses_ability, "p value" = p_ses_ability), 4)
ctable_ses_ability[!grepl("\\|", rownames(ctable_ses_ability)), , drop = FALSE]

# table + figure export
ses_ability_table = build_or_table_polr(model_ses_ability)
write.csv(ses_ability_table, "outputs/ses_ability_model.csv", quote = FALSE, row.names = FALSE)
ses_ability_forest = plot_or_forest(ses_ability_table)
ses_ability_forest
ggsave("outputs/ses_ability_or_forest.png", ses_ability_forest, width = 6, height = 4)

# posthoc
ses_ability_posthoc = emmeans::emmeans(model_ses_ability, pairwise ~ Index_SES)
ses_ability_posthoc
ses_ability_posthoc_table = as.data.frame(ses_ability_posthoc$contrasts)
write.csv(ses_ability_posthoc_table, "outputs/ses_ability_posthoc_contrasts.csv", quote = FALSE, row.names = FALSE)

## achieved power
set.seed(123)
n_sims = 1000
alpha = 0.05
covid_ses_ability_complete = covid[!is.na(covid$Index_SES) & !is.na(covid$OlfactoryAbility_Current_Scaled), ]
n_obs = nrow(covid_ses_ability_complete)
sig_count_ses_ability = 0
for (i in 1:n_sims) {
  sim_idx = sample(1:n_obs, n_obs, replace = TRUE)
  sim_data = covid_ses_ability_complete[sim_idx, ]
  fit = tryCatch(polr(OlfactoryAbility_Current_Scaled ~ Index_SES, data = sim_data, Hess = TRUE), error = function(e) NULL)
  if (!is.null(fit)) {
    ct = coef(summary(fit))
    p_vals = pnorm(abs(ct[!grepl("\\|", rownames(ct)), "t value"]), lower.tail = FALSE) * 2
    if (any(p_vals < alpha)) sig_count_ses_ability = sig_count_ses_ability + 1
  }
}
power_ses_ability = sig_count_ses_ability / n_sims
print(paste("SES Ability model power:", power_ses_ability))


## RESULTS: sig result but those with comparatively more rate their ability lower, after adjusting for awareness.-- sig for less/more contrast but trend persists with more/same







######################### COVID/RACE/SES: FACTORS ACTING ON ABLITY
### the research questions was never formed as a hypothesis in pre-reg; the specified model assessed factors that may alter ability but differences

## FACTORS MODEL
model_factors = polr(OlfactoryAbility_Current_Scaled ~ Index_SES + Index_Needs + Index_Health + Index_Environment, data = covid, Hess = TRUE)
summary(model_factors)
ctable_factors = coef(summary(model_factors)) # p-values
p_factors = pnorm(abs(ctable_factors[, "t value"]), lower.tail = FALSE) * 2
ctable_factors = round(cbind(ctable_factors, "p value" = p_factors), 4)
ctable_factors[!grepl("\\|", rownames(ctable_factors)), , drop = FALSE]

# table + figure export
factors_table = build_or_table_polr(model_factors)
write.csv(factors_table, "outputs/factors_ability_model.csv", quote = FALSE, row.names = FALSE)
factors_forest = plot_or_forest(factors_table)
factors_forest
ggsave("outputs/factors_ability_or_forest.png", factors_forest, width = 6, height = 4)

## achieved power
set.seed(123)
n_sims = 1000
alpha = 0.05
covid_factors_complete = covid[!is.na(covid$Index_SES) & !is.na(covid$Index_Needs) & !is.na(covid$Index_Health) & !is.na(covid$Index_Environment) & !is.na(covid$OlfactoryAbility_Current_Scaled), ]
n_obs = nrow(covid_factors_complete)
sig_count_factors = matrix(0, nrow = 1, ncol = 5)
colnames(sig_count_factors) = c("Index_SESmore", "Index_SESsame", "Index_NeedsNeeds_Not_Met", "Index_Health", "Index_Environment")
for (i in 1:n_sims) {
  sim_idx = sample(1:n_obs, n_obs, replace = TRUE)
  sim_data = covid_factors_complete[sim_idx, ]
  fit = tryCatch(polr(OlfactoryAbility_Current_Scaled ~ Index_SES + Index_Needs + Index_Health + Index_Environment, data = sim_data, Hess = TRUE), error = function(e) NULL)
  if (!is.null(fit)) {
    ct = coef(summary(fit))
    for (v in colnames(sig_count_factors)) {
      if (v %in% rownames(ct)) {
        p_val = pnorm(abs(ct[v, "t value"]), lower.tail = FALSE) * 2
        if (p_val < alpha) sig_count_factors[1, v] = sig_count_factors[1, v] + 1
      }
    }
  }
}
power_factors = sig_count_factors / n_sims
print(power_factors)

## RESULTS: ENV sig; health trending sig, with both negative suggesting worse ability associated with more exposure/problems; power varies across tests with ENV fully powered and SES close but the others are underpowered so only real conclusion is ENV with sufficient power


### is environment impacted by ses
environment_ses_xtab = table(covid$Index_SES, covid$Index_Environment, useNA = "ifany")
environment_ses_xtab
environment_ses_kruskal = kruskal.test(Index_Environment ~ Index_SES, data = covid)
environment_ses_kruskal
# test pairwise
environment_ses_pairwise = pairwise.wilcox.test(covid$Index_Environment, covid$Index_SES, p.adjust.method = "bonferroni")
environment_ses_pairwise

# table export
environment_ses_kruskal_table = data.frame(Statistic = "Chi_Squared", Value = environment_ses_kruskal$statistic, DF = environment_ses_kruskal$parameter, P_Value = environment_ses_kruskal$p.value)
write.csv(environment_ses_kruskal_table, "outputs/environment_ses_kruskal.csv", quote = FALSE, row.names = FALSE)
write.csv(as.data.frame(environment_ses_xtab), "outputs/environment_ses_crosstab.csv", quote = FALSE, row.names = FALSE)
write.csv(as.data.frame(environment_ses_pairwise$p.value), "outputs/environment_ses_pairwise_wilcox.csv", quote = FALSE, row.names = TRUE)

## RESULTS: trending towards sig with more at 0.07--same group with high power for model--likely a link with a higher powered sample



######################### DIAGNOSIS
# filter to covid only
covid2 = covid |> filter(
  Experienced_SmellTasteLoss == "Yes",
  SmellTasteLoss_Cause_VirusSpecific == "Covid"
)

# ses
model_dx_ses_ses = glm(Dx_Binary ~ Index_SES, data = covid2, family = binomial)
summary(model_dx_ses_ses)
ctable_dx_ses = coef(summary(model_dx_ses_ses)) # p-values
p_dx_ses = pnorm(abs(ctable_dx_ses[, "z value"]), lower.tail = FALSE) * 2
ctable_dx_ses = round(cbind(ctable_dx_ses, "p value" = p_dx_ses), 4)
ctable_dx_ses[!grepl("\\|", rownames(ctable_dx_ses)), , drop = FALSE]
## RESULTS: not sig but higher incomes slightly more likely to seek dx

# table + figure export
dx_ses_table = build_or_table_glm(model_dx_ses_ses)
write.csv(dx_ses_table, "outputs/dx_ses_model.csv", quote = FALSE, row.names = FALSE)
dx_ses_forest = plot_or_forest(dx_ses_table)
dx_ses_forest
ggsave("outputs/dx_ses_or_forest.png", dx_ses_forest, width = 6, height = 4)

# achieved power
set.seed(123)
n_sims = 1000
alpha = 0.05
covid_dx_ses_complete = covid2[!is.na(covid2$Index_SES) & !is.na(covid2$Dx_Binary), ]
n_obs = nrow(covid_dx_ses_complete)
sig_count_dx_ses = matrix(0, nrow = 1, ncol = 2)
colnames(sig_count_dx_ses) = c("Index_SESmore", "Index_SESsame")
for (i in 1:n_sims) {
  sim_idx = sample(1:n_obs, n_obs, replace = TRUE)
  sim_data = covid_dx_ses_complete[sim_idx, ]
  fit = tryCatch(glm(Dx_Binary ~ Index_SES, data = sim_data, family = binomial), error = function(e) NULL)
  if (!is.null(fit)) {
    ct = coef(summary(fit))
    for (v in colnames(sig_count_dx_ses)) {
      if (v %in% rownames(ct) && ct[v, "Pr(>|z|)"] < alpha) {
        sig_count_dx_ses[1, v] = sig_count_dx_ses[1, v] + 1
      }
    }
  }
}
power_dx_ses = sig_count_dx_ses / n_sims
print(power_dx_ses)

## RESULTS: not sig but underpowered; trend towards high ses seeking dx due to st


# factors
model_dx_factors_factors = glm(Dx_Binary ~ Index_SES + Index_Needs, data = covid2, family = binomial)
summary(model_dx_factors_factors)
ctable_dx_factors = coef(summary(model_dx_factors_factors)) # p-values
p_dx_factors = pnorm(abs(ctable_dx_factors[, "z value"]), lower.tail = FALSE) * 2
ctable_dx_factors = round(cbind(ctable_dx_factors, "p value" = p_dx_factors), 4)
ctable_dx_factors[!grepl("\\|", rownames(ctable_dx_factors)), , drop = FALSE]
# not sig, but higher income more like and needs not met less likely to seek dx due to ST loss

# table + figure export
dx_factors_table = build_or_table_glm(model_dx_factors_factors)
write.csv(dx_factors_table, "outputs/dx_factors_model.csv", quote = FALSE, row.names = FALSE)
dx_factors_forest = plot_or_forest(dx_factors_table)
dx_factors_forest
ggsave("outputs/dx_factors_or_forest.png", dx_factors_forest, width = 6, height = 4)

# achieved power
set.seed(123)
n_sims = 1000
alpha = 0.05
covid_dx_factors_complete = covid2[!is.na(covid2$Index_SES) & !is.na(covid2$Index_Needs) & !is.na(covid2$Dx_Binary), ]
n_obs = nrow(covid_dx_factors_complete)
sig_count_dx_factors = matrix(0, nrow = 1, ncol = 3)
colnames(sig_count_dx_factors) = c("Index_SESmore", "Index_SESsame", "Index_NeedsNeeds_Not_Met")
for (i in 1:n_sims) {
  sim_idx = sample(1:n_obs, n_obs, replace = TRUE)
  sim_data = covid_dx_factors_complete[sim_idx, ]
  fit = tryCatch(glm(Dx_Binary ~ Index_SES + Index_Needs, data = sim_data, family = binomial), error = function(e) NULL)
  if (!is.null(fit)) {
    ct = coef(summary(fit))
    for (v in colnames(sig_count_dx_factors)) {
      if (v %in% rownames(ct) && ct[v, "Pr(>|z|)"] < alpha) {
        sig_count_dx_factors[1, v] = sig_count_dx_factors[1, v] + 1
      }
    }
  }
}
power_dx_factors = sig_count_dx_factors / n_sims
print(power_dx_factors)

## RESULTS: not sig and underpowered but trend for higher ses more likely to see dx and needs not met less likely







######################### US SUBSET: INCOME BRACKET ANALYSIS (INDEX_SES_US)
### repeats the ability, factors, health, and diagnosis models using the income-bracket
### variable (Below32/Upto41/41K_Plus) on the US-only subset, rather than the relative-
### comparison Index_SES used above

### filter second dataset for US only
# restrict to those with US income brackets to match qualtrics
covid_US = covid |> dplyr::filter(Live_in_USA == "Yes")




######################### CHOROPLETH MAP OF LOCATION BY STATE
state_counts = covid_US |>
  filter(!is.na(State)) |>
  count(State) |>
  mutate(Proportion = n / sum(n)) |>
  rename(state = State)
write.csv(state_counts, "outputs/map-statecounts.csv", quote = FALSE, row.names = FALSE)

state_map = plot_usmap(data = state_counts, values = "Proportion", regions = "states") +
  scale_fill_gradient(low = "#f0f9f8", high = "#21908C", name = "Sample Share", na.value = "grey80") +
  theme(legend.position = "right")

ggsave("outputs/sample_share_by_state_map.png", state_map, width = 8, height = 5)
# 5 states missing data: "AK" "MT" "ND" "VT" "WY"
# Fl, TX, and CA have the most
rm(state_counts, state_map)


## Assumptions Testing, proportional odds
brant(polr(as.ordered(OlfactoryAbility_Current_Scaled) ~ Index_SES_US, data = covid_US))


## SES US MODEL
model_ses_us = polr(OlfactoryAbility_Current_Scaled ~ Index_SES_US, data = covid_US, Hess = TRUE)
summary(model_ses_us)
ctable_ses_us = coef(summary(model_ses_us)) # p-values
p_ses_us = pnorm(abs(ctable_ses_us[, "t value"]), lower.tail = FALSE) * 2
ctable_ses_us = round(cbind(ctable_ses_us, "p value" = p_ses_us), 4)
ctable_ses_us[!grepl("\\|", rownames(ctable_ses_us)), , drop = FALSE]

# table + figure export
ses_us_table = build_or_table_polr(model_ses_us)
write.csv(ses_us_table, "outputs/ses_us_ability_model.csv", quote = FALSE, row.names = FALSE)
ses_us_forest = plot_or_forest(ses_us_table)
ses_us_forest
ggsave("outputs/ses_us_ability_or_forest.png", ses_us_forest, width = 6, height = 4)

# posthoc
ses_us_posthoc = emmeans::emmeans(model_ses_us, pairwise ~ Index_SES_US)
ses_us_posthoc
ses_us_posthoc_table = as.data.frame(ses_us_posthoc$contrasts)
write.csv(ses_us_posthoc_table, "outputs/ses_us_posthoc_contrasts.csv", quote = FALSE, row.names = FALSE)

## achieved power
set.seed(123)
n_sims = 1000
alpha = 0.05
covid_ses_us_complete = covid_US[!is.na(covid_US$Index_SES_US) & !is.na(covid_US$OlfactoryAbility_Current_Scaled), ]
n_obs = nrow(covid_ses_us_complete)
sig_count_ses_us = 0
for (i in 1:n_sims) {
  sim_idx = sample(1:n_obs, n_obs, replace = TRUE)
  sim_data = covid_ses_us_complete[sim_idx, ]
  fit = tryCatch(polr(OlfactoryAbility_Current_Scaled ~ Index_SES_US, data = sim_data, Hess = TRUE), error = function(e) NULL)
  if (!is.null(fit)) {
    ct = coef(summary(fit))
    p_vals = pnorm(abs(ct[!grepl("\\|", rownames(ct)), "t value"]), lower.tail = FALSE) * 2
    if (any(p_vals < alpha)) sig_count_ses_us = sig_count_ses_us + 1
  }
}
power_ses_us = sig_count_ses_us / n_sims
print(paste("SES US model power:", power_ses_us))

## RESULTS: sig differences with high power; inverse relationship between income and ability



## FACTORS US MODEL
model_factors_us = polr(OlfactoryAbility_Current_Scaled ~ Index_SES_US + Index_Needs + Index_Health + Index_Environment, data = covid_US, Hess = TRUE)
summary(model_factors_us)
ctable_factors_us = coef(summary(model_factors_us)) # p-values
p_factors_us = pnorm(abs(ctable_factors_us[, "t value"]), lower.tail = FALSE) * 2
ctable_factors_us = round(cbind(ctable_factors_us, "p value" = p_factors_us), 4)
ctable_factors_us[!grepl("\\|", rownames(ctable_factors_us)), , drop = FALSE]

# table + figure export
factors_us_table = build_or_table_polr(model_factors_us)
write.csv(factors_us_table, "outputs/factors_us_ability_model.csv", quote = FALSE, row.names = FALSE)
factors_us_forest = plot_or_forest(factors_us_table)
factors_us_forest
ggsave("outputs/factors_us_ability_or_forest.png", factors_us_forest, width = 6, height = 4)

## achieved power
set.seed(123)
n_sims = 1000
alpha = 0.05
covid_factors_us_complete = covid_US[!is.na(covid_US$Index_SES_US) & !is.na(covid_US$Index_Needs) & !is.na(covid_US$Index_Health) & !is.na(covid_US$Index_Environment) & !is.na(covid_US$OlfactoryAbility_Current_Scaled), ]
n_obs = nrow(covid_factors_us_complete)
sig_count_factors_us = matrix(0, nrow = 1, ncol = 5)
colnames(sig_count_factors_us) = c("Index_SES_USUpto41", "Index_SES_US41K_Plus", "Index_NeedsNeeds_Not_Met", "Index_Health", "Index_Environment")
for (i in 1:n_sims) {
  sim_idx = sample(1:n_obs, n_obs, replace = TRUE)
  sim_data = covid_factors_us_complete[sim_idx, ]
  fit = tryCatch(polr(OlfactoryAbility_Current_Scaled ~ Index_SES_US + Index_Needs + Index_Health + Index_Environment, data = sim_data, Hess = TRUE), error = function(e) NULL)
  if (!is.null(fit)) {
    ct = coef(summary(fit))
    for (v in colnames(sig_count_factors_us)) {
      if (v %in% rownames(ct)) {
        p_val = pnorm(abs(ct[v, "t value"]), lower.tail = FALSE) * 2
        if (p_val < alpha) sig_count_factors_us[1, v] = sig_count_factors_us[1, v] + 1
      }
    }
  }
}
power_factors_us = sig_count_factors_us / n_sims
print(power_factors_us)

## RESULTS: needs not met only one not sig but that was underpowered--higher income and worse health/env linked to poorer ability; all other tests high power



### is health impacted by ses (us subset)
health_ses_us_xtab = table(covid_US$Index_SES_US, covid_US$Index_Health, useNA = "ifany")
health_ses_us_xtab
health_ses_us_kruskal = kruskal.test(Index_Health ~ Index_SES_US, data = covid_US)
health_ses_us_kruskal
# test pairwise
health_ses_us_pairwise = pairwise.wilcox.test(covid_US$Index_Health, covid_US$Index_SES_US, p.adjust.method = "bonferroni")
health_ses_us_pairwise

# table export
health_ses_us_kruskal_table = data.frame(Statistic = "Chi_Squared", Value = health_ses_us_kruskal$statistic, DF = health_ses_us_kruskal$parameter, P_Value = health_ses_us_kruskal$p.value)
write.csv(health_ses_us_kruskal_table, "outputs/health_ses_us_kruskal.csv", quote = FALSE, row.names = FALSE)
write.csv(as.data.frame(health_ses_us_xtab), "outputs/health_ses_us_crosstab.csv", quote = FALSE, row.names = FALSE)
write.csv(as.data.frame(health_ses_us_pairwise$p.value), "outputs/health_ses_us_pairwise_wilcox.csv", quote = FALSE, row.names = TRUE)
## sig

### is environment impacted by ses (us subset)
environment_ses_us_xtab = table(covid_US$Index_SES_US, covid_US$Index_Environment, useNA = "ifany")
environment_ses_us_xtab
environment_ses_us_kruskal = kruskal.test(Index_Environment ~ Index_SES_US, data = covid_US)
environment_ses_us_kruskal
# test pairwise
environment_ses_us_pairwise = pairwise.wilcox.test(covid_US$Index_Environment, covid_US$Index_SES_US, p.adjust.method = "bonferroni")
environment_ses_us_pairwise
# sig

# table export
environment_ses_us_kruskal_table = data.frame(Statistic = "Chi_Squared", Value = environment_ses_us_kruskal$statistic, DF = environment_ses_us_kruskal$parameter, P_Value = environment_ses_us_kruskal$p.value)
write.csv(environment_ses_us_kruskal_table, "outputs/environment_ses_us_kruskal.csv", quote = FALSE, row.names = FALSE)
write.csv(as.data.frame(environment_ses_us_xtab), "outputs/environment_ses_us_crosstab.csv", quote = FALSE, row.names = FALSE)
write.csv(as.data.frame(environment_ses_us_pairwise$p.value), "outputs/environment_ses_us_pairwise_wilcox.csv", quote = FALSE, row.names = TRUE)


## RESULTS:
# Health: Kruskal-Wallis p=0.005 (Below32 vs 41K_Plus pairwise significant, p=0.004; other two pairs not).
# Environment: Kruskal-Wallis p<0.0001, and all three pairwise comparisons significant (p=0.0004, p<0.0001, p<0.0001) — environment shows a much stronger, fully-separated SES gradient than health does in the US subset.


## DIAGNOSIS US MODELS
covid_US2 = covid_US |> filter(
  Experienced_SmellTasteLoss == "Yes",
  SmellTasteLoss_Cause_VirusSpecific == "Covid"
)
# ses
model_dx_ses_us = glm(Dx_Binary ~ Index_SES_US, data = covid_US2, family = binomial)
summary(model_dx_ses_us)
ctable_dx_ses_us = coef(summary(model_dx_ses_us)) # p-values
p_dx_ses_us = pnorm(abs(ctable_dx_ses_us[, "z value"]), lower.tail = FALSE) * 2
ctable_dx_ses_us = round(cbind(ctable_dx_ses_us, "p value" = p_dx_ses_us), 4)
ctable_dx_ses_us[!grepl("\\|", rownames(ctable_dx_ses_us)), , drop = FALSE]

# table + figure export
dx_ses_us_table = build_or_table_glm(model_dx_ses_us)
write.csv(dx_ses_us_table, "outputs/dx_ses_us_model.csv", quote = FALSE, row.names = FALSE)
dx_ses_us_forest = plot_or_forest(dx_ses_us_table)
dx_ses_us_forest
ggsave("outputs/dx_ses_us_or_forest.png", dx_ses_us_forest, width = 6, height = 4)

# achieved power
set.seed(123)
n_sims = 1000
alpha = 0.05
covid_dx_ses_us_complete = covid_US2[!is.na(covid_US2$Index_SES_US) & !is.na(covid_US2$Dx_Binary), ]
n_obs = nrow(covid_dx_ses_us_complete)
sig_count_dx_ses_us = matrix(0, nrow = 1, ncol = 2)
colnames(sig_count_dx_ses_us) = c("Index_SES_USUpto41", "Index_SES_US41K_Plus")
for (i in 1:n_sims) {
  sim_idx = sample(1:n_obs, n_obs, replace = TRUE)
  sim_data = covid_dx_ses_us_complete[sim_idx, ]
  fit = tryCatch(glm(Dx_Binary ~ Index_SES_US, data = sim_data, family = binomial), error = function(e) NULL)
  if (!is.null(fit)) {
    ct = coef(summary(fit))
    for (v in colnames(sig_count_dx_ses_us)) {
      if (v %in% rownames(ct) && ct[v, "Pr(>|z|)"] < alpha) {
        sig_count_dx_ses_us[1, v] = sig_count_dx_ses_us[1, v] + 1
      }
    }
  }
}
power_dx_ses_us = sig_count_dx_ses_us / n_sims
print(power_dx_ses_us)

## Results, not sig and underpowered but higher incomes slightly more likely to see DX from ST loss


# factors
model_dx_factors_us = glm(Dx_Binary ~ Index_SES_US + Index_Needs, data = covid_US2, family = binomial)
summary(model_dx_factors_us)
ctable_dx_factors_us = coef(summary(model_dx_factors_us)) # p-values
p_dx_factors_us = pnorm(abs(ctable_dx_factors_us[, "z value"]), lower.tail = FALSE) * 2
ctable_dx_factors_us = round(cbind(ctable_dx_factors_us, "p value" = p_dx_factors_us), 4)
ctable_dx_factors_us[!grepl("\\|", rownames(ctable_dx_factors_us)), , drop = FALSE]

# table + figure export
dx_factors_us_table = build_or_table_glm(model_dx_factors_us)
write.csv(dx_factors_us_table, "outputs/dx_factors_us_model.csv", quote = FALSE, row.names = FALSE)
dx_factors_us_forest = plot_or_forest(dx_factors_us_table)
dx_factors_us_forest
ggsave("outputs/dx_factors_us_or_forest.png", dx_factors_us_forest, width = 6, height = 4)

## achieved power
set.seed(123)
n_sims = 1000
alpha = 0.05
covid_dx_factors_us_complete = covid_US2[!is.na(covid_US2$Index_SES_US) & !is.na(covid_US2$Index_Needs) & !is.na(covid_US2$Dx_Binary), ]
n_obs = nrow(covid_dx_factors_us_complete)
sig_count_dx_factors_us = matrix(0, nrow = 1, ncol = 3)
colnames(sig_count_dx_factors_us) = c("Index_SES_USUpto41", "Index_SES_US41K_Plus", "Index_NeedsNeeds_Not_Met")
for (i in 1:n_sims) {
  sim_idx = sample(1:n_obs, n_obs, replace = TRUE)
  sim_data = covid_dx_factors_us_complete[sim_idx, ]
  fit = tryCatch(glm(Dx_Binary ~ Index_SES_US + Index_Needs, data = sim_data, family = binomial), error = function(e) NULL)
  if (!is.null(fit)) {
    ct = coef(summary(fit))
    for (v in colnames(sig_count_dx_factors_us)) {
      if (v %in% rownames(ct) && ct[v, "Pr(>|z|)"] < alpha) {
        sig_count_dx_factors_us[1, v] = sig_count_dx_factors_us[1, v] + 1
      }
    }
  }
}
power_dx_factors_us = sig_count_dx_factors_us / n_sims
print(power_dx_factors_us)

## RESULTS: not sig but higher incomes slightly more likely to see DX from ST loss and needs not met less likely


######################### TIDY
rm(list = ls())
gc()
