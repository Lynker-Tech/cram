process_cram = function(model_version = "v0.446",
                        year_of_simulation = 2045,
                        base_folder ="D:/Projects/Aurora/2021_IWMP2_CRAMupdate/Model/ModelRuns",
                        base_model_folder = "2045 v0.446",
                        base_model_ID = "ARWM 2022-04-11 0.446",
                        base_prefix = "000.",
                        base_run_ID = "",
                        compare_model_folder = "2045 v0.446",
                        compare_model_ID = "ARWM 2022-04-11 0.446"  ,
                        compare_prefix = "219.",
                        compare_run_ID = "",
                        year_start = 1950,
                        year_end = 2017){


  pct_reduction = NA

  tmp = substring(compare_prefix, 1, 1)

  if (tmp == 0) {
    pct_reduction = "No CC"
  } else if (tmp == 2) {
    pct_reduction = "25pct CC"
  } else{
    pct_reduction = "37pct CC"
  }

  scenario_name <-
    c(
      paste(year_of_simulation, "Historical Hydrology"),
      paste0(
        year_of_simulation,
        " ",
        pct_reduction,
        " ",
        "(",
        substring(compare_prefix, 1, 3),
        ")"
      )
    )

  n_scenario_name <- length(scenario_name)
  scenario_folder <- paste0(model_version, " ", scenario_name[1], " vs ", scenario_name[2])
  levels_list     <- scenario_name


  output_folder <- file.path(base_folder, "output", scenario_folder)
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)


  ## Meaningless!
  df = data.frame(x = 1:10, y = 1:10)

  ggplot(data = df ) +
    geom_point(aes(x = x, y = y)) +
    labs(title = "Example Plot")

  ggsave(filename = file.path(output_folder, "example_image.png") )
}





