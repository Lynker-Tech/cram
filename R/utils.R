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


# Seperate tab seperated values and makes a tibble
tab_vect <- function(vect) {
  # vect <- lst[i]
  # m  <- unlist(strsplit(vect, "\t"))

  tvect <-
    unlist(strsplit(vect, "\t")) %>%
    matrix(
      nrow = 1, ncol = length(.)
    ) %>%
    as.data.frame() %>%
    tibble::tibble()

  return(tvect)
}

# Replace problems that arise when reading in tab seperated text file using readr::read_tsv() then calling problems()
replace_probs <- function(df, prob_df) {

  if(nrow(prob_df) < 1) {

    logger::log_info("\nNo problems!")

    return(df)

  } else {

    logger::log_info("\nFixing data in {nrow(prob_df)} rows")

    df <-
      df %>%
      dplyr::mutate(across(where(is.numeric), as.character))

    for (z in 1:nrow(prob_df)) {

      zrow     <- prob_df$row[z]
      zcol     <- prob_df$col[z]
      zreplace <- prob_df$actual[z]

      # logger::log_info("\n\n{z} of {nrow(prob_df)}\nRow: {zrow}\nCol: {zcol}\nReplace with: {zreplace}")

      df[zrow, zcol] <- zreplace

    }

    return(df)
  }

}

process_cram2 = function(
    model_version        = "v0.464",
    base_year            = 2022,
    base_folder          = here::here("runs"),
    base_model_folder    = NULL,
    date_convert         = NULL,
    return_wide          = TRUE

) {

  # check if date conversion file is supplied
  if(is.null(date_convert)) {

    logger::log_error("\n\nPlease enter a date_convert dataframe to convert QM to date:\nPotential name: qm_to_date_conversion.csv")
    return(NULL)

  } else if(is.null(base_model_folder)) {  # check if path to folder is entered

    logger::log_error("\n\nPlease enter a valid base_model_folder path")
    return(NULL)

  }

  # Base model year directory
  base_model_dir <- grep(
    base_model_folder,
    # paste(c(base_year, reversion_txt, model_version), collapse = "|"),
    list.files(base_folder, full.names = T),
    value = T
  )

  logger::log_info("\n\nLooking for model year and version folder in:\n{base_folder}")

  # Base model year directory
  # base_model_dir <- grep(
  #   # paste(base_year, model_version),
  #   path,
  #   # paste(c(base_year, reversion_txt, model_version), collapse = "|"),
  #   list.files(base_folder, full.names = T),
  #   value = T
  # )
  # base_model_dir

  # Error if model folder not found
  if(length(base_model_dir) == 0) {

    logger::log_error("\n\nNo model folder matching:\n{base_model_folder}\nAvaliable Model folders are:\n{paste(list.files(base_folder, full.names = F), collapse = ', ')}")
    return(NULL)

  }
  # else {

  # Paths to base model output sheet .txt files
  base_output_paths <- list.files(
    base_model_dir,
    full.names = T,
    pattern    = "OUTPUT_SHEET.txt"
  )

  if(length(base_output_paths) == 0) {

    # logger::log_error("\n\nNo file matching:\nModel year: {base_year}\nModel version: {model_version}\n\nAvaliable Model folders are:\n{paste(list.files(base_folder, full.names = F), collapse = ', ')}")
    logger::log_error("\n\nNo model folder matching:\n{base_model_folder}\nAvaliable Model folders are:\n{paste(list.files(base_folder, full.names = F), collapse = ', ')}")

    return(NULL)

  }

  # Paths to base model output sheet .txt files, extract file name
  file_names <- tibble::tibble(
    file = list.files(
      base_model_dir,
      full.names = F,
      pattern    = "OUTPUT_SHEET.txt"
    )
  ) %>%
    dplyr::mutate(
      output_sheet = dplyr::case_when(
        grepl("ARKANSAS", file)     ~ "ARKANSAS",
        grepl("SOUTH_PLATTE", file) ~ "SOUTH_PLATTE",
        grepl("PWP", file)          ~ "PWP",
        grepl("WILDHORSE", file)    ~ "WILDHORSE"
      )
    )

  sheet_lst <- list()

  # loop through the number of OUTPUT_SHEET.txt files in runs/<modelyear_modelversion> directory, clean and bind all rows
  for (i in 1:length(base_output_paths)) {

    logger::log_info("\n\nReading:\n{file_names$output_sheet[i]}")

    # # Read in output sheet for extracting names later on
    out_tbl_names <- readr::read_tsv(
      base_output_paths[i],
      col_names      = F,
      show_col_types = FALSE
    )

    # # See if any problems in read in of data
    prob <- problems(out_tbl_names)

    # Replace problem cases w/ recommended strings from problems() func
    out_tbl_names <-
      out_tbl_names %>%
      replace_probs(df = ., prob_df = prob)

    # number of rows to skip, when "Step" is seen in column 1, read in data after that
    skiprows <- grep("Step", out_tbl_names$X1)

    # read in dataset and get all data in each row in a single string
    out_tbl <- readr::read_fwf(
      file           = base_output_paths[i],
      skip           = skiprows,
      show_col_types = FALSE
    ) %>%
      na.omit()

    # # column one has each row of data as a single string, split by tabs
    line_lst <- out_tbl$X1[1:nrow(out_tbl)]

    # split strings into columns by tab seperator and transform matrix into dataframe
    out <- sapply(X = 1:length(line_lst), FUN = function(x){
      tab_vect(line_lst[x])
    }) %>%
      t() %>%
      as.data.frame()


    # names for data in out_tvect
    tbl_names <- out_tbl_names[5, 1:ncol(out)]

    # Loop through data and replace NA column headers w/ --> "empty_{column number}"
    for (c in 1:ncol(tbl_names)) {
      # logger::log_info("{c} of {ncol(tbl_cols)}")
      if(is.na(tbl_names[, c]) | tbl_names[, c] == "#N/A") {
        tbl_names[,c] <- paste0("empty_", c)
      }

    }

    # Clean columns by removing special characters & whitespace, replacing w/ underscore
    clean_cols <- gsub(
      '([[:punct:]])|\\s+',
      '_',
      tolower(tbl_names)
    )

    # Matrix w/ cleaned names for processing
    name_mat <- matrix(
      data = unlist(clean_cols),
      ncol = length(clean_cols)
    ) %>%
      as.data.frame()

    out <-
      out %>%
      setNames(c("year", "qm", "step",   name_mat[1, 4:ncol(name_mat)])) %>%
      tidyr::unnest()

    # Clean date conversion dataframe
    cdate <-
      date_convert %>%
      janitor::clean_names() %>%
      dplyr::mutate(across(where(is.numeric), as.character))

    param_df <-
      tibble::tibble(
        desc         = unlist(as.vector(out_tbl_names[3, 4:ncol(out)])),
        parameter    = unlist(as.vector(out_tbl_names[4, 4:ncol(out)])),
        name         = unlist(as.vector(name_mat[1, 4:ncol(name_mat)]))
      )

    out_data <-
      out %>%
      dplyr::left_join(
        dplyr::select(cdate, qm, year = water_year, start_date, end_date),
        by = c("qm", "year")
      ) %>%
      dplyr::mutate(
        model_year    = base_year,
        model_version = model_version
      ) %>%
      dplyr::relocate(model_year, model_version, year, qm, step, start_date, end_date) %>%
      dplyr::mutate(
        start_date = as.Date(start_date, "%m/%d/%Y"),
        end_date   = as.Date(end_date, "%m/%d/%Y")
      ) %>%
      dplyr::mutate(across(where(is.numeric), as.character)) %>%
      tidyr::pivot_longer(cols = 8:last_col()) %>%
      dplyr::left_join(
        param_df,
        by = "name"
      ) %>%
      dplyr::mutate(
        file         = file_names$file[i],
        output_sheet = file_names$output_sheet[i]
      )

    sheet_lst[[i]] <- out_data
  }

  sheet_df <- dplyr::bind_rows(sheet_lst)

  if(return_wide == TRUE) {
    sheet_df <-
      sheet_df %>%
      tidyr::pivot_wider(
        id_cols     = c(-desc, -parameter),
        names_from  = name,
        values_from = value
      )

    return(sheet_df)

  } else {

    return(sheet_df)

  }


}



