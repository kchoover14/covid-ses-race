# Qualtrics Data Cleaning

## Quality Control and Ethics Check

- Filter surveys to gc = 1 which indicates the survey is complete, the person is above 18 years of age, the person agreed to the informed consent: 888 total survey responses reduced to 540 complete responses
- Strip PII (zip code), share clean script but not raw data

## Handling implausible responses listed in pre-reg

- Reported smell and taste loss (32) but no change on 10-point scales before and during illness (40-41)
  - Handling notes: There was an error in the pre-registration logic: 32 is a general/lifetime question, not one specific to the COVID experience, while 40-41 reference "your most recent smell or taste loss" — which may or may not be the same as the viral illness being asked about elsewhere.
  - Decision: these are plausible responses and data were retained

- Reported smell loss (32) but no "smell changes" as a symptom during illness (37)
  - Handling notes: There was an error in the pre-registration logic: 32 is a general/lifetime question, not one specific to the COVID experience, while 37 is explicitly scoped to "symptoms with your recent viral illness"
  - Decision: these are plausible responses and data were retained

- Reported "smell changes" as a symptom during illness (37), but no change on 10-point scales before and during illness (40, 41)
  - Handling notes: exclude data for implausible pairs but retain data for questions that are valid

- Reported smell loss as a factor in seeking diagnosis (38), but no change on 10-point scales before and during illness (40, 41)
  - Handling notes: exclude data for implausible pairs but retain data for questions that are valid

- Reported smell loss as a factor in seeking diagnosis, but no "smell changes" as a symptom during illness (38, 37)
  - Handling notes: exclude data for implausible pairs but retain data for questions that are valid

- Reported more than two generations in household (19) but then gives a number lower than 3 on the question how many (20)
  - Handling notes: exclude data for implausible pairs but retain data for questions that are valid

## Handling implausible responses not listed in pre-reg

- **MinorityEthnic_Race_Status:** 60 individuals responded "unsure"
  - *Decision: Retain only yes and no responses in new variable (`MinorityEthnic_Race_Status_Binary`)*

- **SES: Index_SES:** SES Index (70): codebook/pre-reg describe income and resources assessed within-group and within-country, but all individuals reporting no income (n=72) also reported an income bracket: 63 Below32, 9 Upto41
  - *Decision: create 2-level `Index_SES` excluding Have_Income/Needs*

- **Household:**
  - 2 nonsense responses to household size
    - *Decision: remove values in cells*
  - Missing value imputation for multi-generational household
    - *Decision: Use household size for missing values*

## Pre-registration problems found during analysis

- **COVID RACE and SES**
  - Section label should be "research questions," not "hypotheses": hypotheses are found in the bullets specific to each dataset.
  - Non-parametric analysis poorly developed and not parallel to parametric analysis.
    - *Decision: implement non-parametric version of parametric analysis*
  - Research question not directly assessed (ability x SES/race)
    - *Decision: add simple models to test*

- **SES Index** creates too many bins given the data size, and needs met for income and resources are identical
  - *Decision: use the logic for needs index and rename to SES because meeting needs is tied to SES*

- **Symptom Index (74):** codebook/pre-reg refers to 0-13 points, but there are 18, not 13 symptoms.
  - *Decision: Use actual number of symptoms in the survey*

- **Health Index (78):** codebook creates index from items 63-67 (Chronic_Illnesses, Cancer_Treatment, Daily_Medications, Smoke_Vape_Daily, Smoke_Type), which counts smoking twice and excludes oral health as a health factor.
  - *Decision: Use only Smoke_Vape_Daily, drop Smoke_Type, add oral health.*

- **Household:** Greater risk is not assessable because there is no sample of people without respiratory incidence
  - *Decision: Compare viral to other types but insufficient sample sizes across bins whether using multi (2+ or 3+)*

- **Train/Test using Qualtrics on FormR:** no significant results in Qualtrics make this item null
  - *Decision: limit FormR to US-only to expand income bracket data to include higher incomes and test the hypothesis*
