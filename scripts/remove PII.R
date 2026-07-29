######################### LOAD PACKAGES
library(dplyr)    # data wrangling
library(janitor)  # clean names
library(stringr)  # string wrangling
library(zipcodeR)   # zip-to-state lookup

######################### DATA
# raw data not shared due to possible PII

# qualtrics
qualtrics = read.table('data/data-qualtrics-raw.csv', header=TRUE, skip = 1, sep=",", dec=".", stringsAsFactors = TRUE)

#formr
formr = read.csv('data/data-formr-raw.csv', stringsAsFactors = TRUE)


######################### FILTER SURVEYS
# qualtrics only: filter GC which is quality control score for completeness and inclusion criteria (18+, consented)
qualtrics = filter(qualtrics, gc == 1)



######################### CLEAN NAMES
qualtrics = clean_names(qualtrics)
formr = clean_names(formr)



######################### REMOVE SURVEY METADATA VARIABLES
# qualtrics only
qualtrics = qualtrics |> dplyr::select(-matches("irb_alaska_edu"))

qualtrics = dplyr::select(qualtrics, -start_date, -end_date, -progress, -finished, -q_recaptcha_score, -q_relevant_id_duplicate, -q_relevant_id_duplicate_score, -q_relevant_id_fraud_score, -gc)

# remove extra covid col in formr
formr = dplyr::select(formr, -covid)


######################### REMOVE TASTE PREFERENCES DATA
# data belong to someone else and are not included/shared
qualtrics = qualtrics |> dplyr::select(-matches("please_read"))
formr = formr |> dplyr::select(-matches("rate"))
formr = formr |> dplyr::select(-eating_less, -eating_more, -fattier_foods, -saltier_foods, -sweeter_foods, -somato_foods, -non_alc_bev_covid, -non_alc_bev_smell_loss)




######################### REPLACE ZIP CODE WITH STATE TO PROTECT PRIVACY
qualtrics = qualtrics |>
  mutate(please_enter_your_zipcode_5_digits = str_pad(please_enter_your_zipcode_5_digits, 5, pad = "0", side = "left")) |>
  left_join(zip_code_db |> dplyr::select(zipcode, state), by = c("please_enter_your_zipcode_5_digits" = "zipcode")) |>
  rename(State = state) |>
  dplyr::select(-please_enter_your_zipcode_5_digits)

formr = formr |>
  mutate(zip_code = str_pad(zip_code, 5, pad = "0", side = "left")) |>
  left_join(zip_code_db |> dplyr::select(zipcode, state), by = c("zip_code" = "zipcode")) |>
  rename(State = state) |> dplyr::select(-zip_code)



######################### SAVE CLEAN DATA FOR DOWNSTREAM ANALYSIS
write.csv(qualtrics, "data/data-qualtrics-noPII.csv", row.names = FALSE)
write.csv(formr, "data/data-formr-noPII.csv", row.names = FALSE)



######################### TIDY
rm(list = ls())
gc()
