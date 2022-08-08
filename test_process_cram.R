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
  model_directory      = models[1,],
  return_wide          = TRUE
)
