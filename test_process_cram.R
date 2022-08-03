# Angus Watters
# Example for using process_cram() function

library(cram)

rcompendium::add_dependencies_badge()
devtools::document()
devtools::load_all()
devtools::build_readme()
devtools::install_local('.')

## Examples:
base_folder       = "C:/Users/angus/Downloads/runs"
(models = parse_directory(base_folder))

cram1 <- process_cram(
  base_year         = models$base_year[1],
  model_version     = models$model_version[1],
  base_folder       = models$base_folder[1],
  return_wide       = TRUE
)



cram2 <- process_cram(
  model_version     = "v0.466",
  base_year         = 2025,
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
