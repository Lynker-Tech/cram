# Angus Watters
# Example for using process_cram() function

remove(list = ls())

library(tidyverse)
library(readr)

source("R/utils.R")

date_convert <- readr::read_csv(here::here("date", "qm_to_date_conversion.csv"))

# process_cram function takes in 5 arguments:

# model_version     - notes the version number and places this text in a column in the final returned dataframe
# base_year         - notes the year of interest and places this text in a column in the final returned dataframe
# base_folder       - highest level directory housing all the model folders
# base_model_folder - specific model folder name that OUTPUT_SHEETS should be pulled and processed from
# date_convert      - date conversion file, read in at top of script (qm_to_date_conversion.csv)
# return_wide       - if TRUE, function returns a wide dataset with a column for each header found in the original OUTPUT_SHEET.txt files, otherwise long dataset w/ multiple rows per timestamp

# Returns a dataframe that has all of the data from the OUTPUT_SHEETS found in the model folder that the function is pointed too.

## Examples:

cram1 <- process_cram2(
  model_version     = "v0.464",
  base_year         = 2022,
  base_folder       = here::here("runs"),
  base_model_folder = "/2022 v0.464",
  date_convert      = date_convert,
  return_wide       = TRUE
)


cram2 <- process_cram2(
  model_version     = "v0.466",
  base_year         = 2025,
  base_folder       = here::here("runs"),
  base_model_folder = "/2025 v0.464",
  date_convert      = date_convert,
  return_wide       = TRUE
)

cram3 <- process_cram2(
  model_version     = "v0.463",
  base_year         = 2025,
  base_folder       = here::here("runs"),
  base_model_folder = "/2025 v0.463",
  date_convert      = date_convert,
  return_wide       = TRUE
)

cram4 <- process_cram2(
  model_version     = "v0.466",
  base_year         = 2025,
  base_folder       = here::here("runs"),
  base_model_folder = "/2025 MoS Triggers v0.466",
  date_convert      = date_convert,
  return_wide       = TRUE
)
