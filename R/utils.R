#' Seperate tab seperated values and makes a tibble
#' @param vect a vector of values
#' @return data.frame
#' @export
#' @importFrom tibble tibble
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

#' Remove Problems
#' @description Replace problems that arise when reading in tab seperated text file
#' using readr::read_tsv() then calling problems()
#' @param df original df
#' @param prob_df problem df
#' @return data.frame
#' @export
#' @importFrom logger log_info
#' @importFrom dplyr mutate across
replace_probs <- function(df, prob_df) {

  if(nrow(prob_df) < 1) {

    log_info("No problems!")

    return(df)

  } else {

    log_info("Fixing data in {nrow(prob_df)} rows")

    df <- mutate(df, across(where(is.numeric), as.character))

    for (z in 1:nrow(prob_df)) {

      zrow     <- prob_df$row[z]
      zcol     <- prob_df$col[z]
      zreplace <- prob_df$actual[z]

      df[zrow, zcol] <- zreplace

    }

    return(df)
  }

}
#' Parse Year and Model
#' @param base_folder highest level directory housing all the model folders
#' @return data.frame
#' @export
#' @importFrom tidyr extract
#' @importFrom dplyr mutate

parse_directory = function(base_folder){
  x <- data.frame(x = basename(list.dirs(base_folder))[-1])
  extract(x, col = x, into = c("base_year", "model_version"), regex = "^(\\w+)\\s?(.*)$") %>%
    mutate(base_folder = base_folder)
}

#' Remove dataframe columns with only NA in all rows
#'
#' @description Helper function that removes columns with only NA values
#' @param df A dataframe expected to have entire columns filled with NA
#' @return data.frame with NA only columns removed
#' @export

rm_na_cols <- function(df) {

  new_df <- df[ , colSums(is.na(df)) < nrow(df)]  # Remove columns with NA only

  return(new_df)
}

#' Process CRAM output
#' @param model_directory dataframe with base_year, model_version, and base_folder columns, indicating the location of a CRAM model folder, a single row from the output dataframe from parse_directory function
#' @param return_wide if TRUE, function returns a wide dataset with a column for each header found in the original OUTPUT_SHEET.txt files, otherwise long dataset w/ multiple rows per timestamp
#' @return a dataframe that has all of the data from the OUTPUT_SHEETS found in the model folder that the function is pointed too.
#' @export
#' @importFrom logger log_error log_info
#' @importFrom dplyr mutate case_when left_join inner_join select relocate bind_rows across last_col all_of `%>%`
#' @importFrom tibble tibble
#' @importFrom readr read_tsv read_fwf problems
#' @importFrom stats na.omit setNames step
#' @importFrom tidyr unnest pivot_longer pivot_wider
#' @importFrom janitor clean_names
#' @importFrom purrr reduce
process_cram = function(
    model_directory      = NULL,
    return_wide          = TRUE
) {

  if(is.null(model_directory)) {
    logger::log_error("\n\nmodel_directory input is NULL\nPlease enter a single row from the output dataframe created by using the parse_directory() function")
  }

  # Retrieve model year, version, and base folder path from single row of model_dirs() ouput
  base_year     <- model_directory$base_year
  model_version <- model_directory$model_version
  base_folder   <- model_directory$base_folder

  base_model_folder = paste(base_year, model_version)

  # Base model year directory
  base_model_dir <- grep(
    base_model_folder,
    list.files(base_folder, full.names = T),
    value = TRUE
  )

  logger::log_info("\n\nReading From: {base_folder}/{base_model_folder}")

  # Error if model folder not found
  if(length(base_model_dir) == 0) {

    logger::log_error("\n\nNo model folder matching: {base_model_folder}: \nAvaliable Model folders are: {paste(list.files(base_folder, full.names = F), collapse = ', ')}")

    return(NULL)

  }

  # Paths to base model output sheet .txt files
  base_output_paths <- list.files(
    base_model_dir,
    full.names = TRUE,
    pattern    = "OUTPUT_SHEET.txt"
  )

  # Error, check if any output sheets were found in base_model_folder
  if(length(base_output_paths) == 0) {

    logger::log_error("\n\nNo model folder matching: {base_model_folder} ...\nAvaliable Model folders are: {paste(list.files(base_folder, full.names = F), collapse = ', ')}")

    return(NULL)
  }

  # Paths to base model output sheet .txt files, extract file name
  file_names <- tibble::tibble(
    file      = list.files(
      base_model_dir,
      full.names = FALSE,
      pattern    = "OUTPUT_SHEET.txt"
    ),
    full_path = list.files(
      base_model_dir,
      full.names = TRUE,
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

  # get the unique models in folder and split into list of models
  model_check <-
    file_names %>%
    dplyr::group_by(file) %>%
    dplyr::mutate(
        model_num  = sub(
          "[.]$", "",
          gsub(paste0(output_sheet, "_", "OUTPUT_SHEET.txt"), "", file)
          )
      ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(model_num) %>%
    dplyr::group_split()


  # empty list to add models to
  model_lst <- list()

  # Loop through each model found in parsed directory dataframe
  for (z in 1:length(model_check)) {

    # paths to model output sheets
    model_paths <- model_check[[z]]

    logger::log_info("\n\nModel {z} of {length(model_check)}\nModel scenario: {model_paths$model_num[1]}")

    # Empty list to add processed output sheets to at end of for loop
    sheet_lst <- list()

    # loop through the number of OUTPUT_SHEET.txt files in runs/<modelyear_modelversion> directory, clean and bind all rows
    for (i in 1:nrow(model_paths)) {
    # for (i in 1:length(base_output_paths)) {

      logger::log_info("\n\nOutput sheet: {model_paths$output_sheet[i]}")

      # # Read in output sheet for extracting names later on
      out_tbl_names <- readr::read_tsv(
        model_paths$full_path[i],
        col_names      = FALSE,
        show_col_types = FALSE,
        progress       = FALSE
      )

      # # See if any problems in read in of data
      prob <- readr::problems(out_tbl_names)

      # Replace problem cases w/ recommended strings from problems() func
      out_tbl_names <-
        out_tbl_names %>%
        replace_probs(df = ., prob_df = prob)

      # number of rows to skip, when "Step" is seen in column 1, read in data after that
      skiprows <- grep("Step", out_tbl_names$X1)

      # read in dataset and get all data in each row in a single string
      out_tbl <- readr::read_fwf(
        file           = model_paths$full_path[i],
        skip           = skiprows,
        show_col_types = FALSE,
        progress       = FALSE
      ) %>%
        dplyr::filter(!is.na(X1)) %>%
        dplyr::select(X1)

      # # column one has each row of data as a single string, split by tabs
      line_lst <- out_tbl$X1[1:nrow(out_tbl)]

      # split strings into columns by tab seperator and transform matrix into dataframe
      out <- sapply(X = 1:length(line_lst), FUN = function(x){
        tab_vect(line_lst[x])
      }) %>%
        t() %>%
        as.data.frame()

      # locate row that contains descriptions
      desc_index   <- grep("Major", out_tbl_names$X1)

      # locate row that contains parameters
      param_index  <- grep("Time", out_tbl_names$X1)

      # locate row that contains name
      name_index   <- grep("Step", out_tbl_names$X1)

      # descriptions for data in out
      tbl_desc     <- out_tbl_names[desc_index, 1:ncol(out)]
      desc_vect    <- unlist(as.vector(tbl_desc[1, ]))

      # params for data in out
      tbl_params   <- out_tbl_names[param_index, 1:ncol(out)]
      param_vect   <- unlist(as.vector(tbl_params[1, ]))

      # names for data in out
      tbl_names    <- out_tbl_names[name_index, 1:ncol(out)]


      # c <- 5
      # error_lst <- list()

      # Loop through tbl_names and replace NA column headers w/ --> "empty_{column number}"
      for (c in 1:ncol(tbl_names)) {

        if(is.na(tbl_names[, c]) | tbl_names[, c] == "#N/A") {

          # message(paste0("error in column ", c))

          tbl_names[,c] <- paste0("empty_", c)

          # error_lst[[c]] <- c
        }

      }

      # error_df  <- unlist(error_lst)

      # find columns to use to fix broken/missing column headers
      # fix_index <- min(desc_index, param_index, name_index) - 2

      # j <- 17
      # for (j in error_df) {
      #
      #   message(paste0(j))
      #
      #   # out_tbl_names[c(1:fix_index), j]
      #
      #   # string to replace NA/empties with
      #   fix_col   <- out_tbl_names[c(1:fix_index),j] %>%
      #     stats::na.omit() %>%
      #     stats::setNames(c("fix_names")) %>%
      #     dplyr::filter(fix_names != "#N/A")
      #
      #   # collapse text into new column name
      #   fixed_str <- paste0(tolower(fix_col$fix_names), collapse = "_")
      #
      #   # replace "empty_" text in tbl_names w/ fixed_str
      #   tbl_names[, j] <- fixed_str
      #
      # }
      #
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

      # set proper names
      out <-
        out %>%
        stats::setNames(c("year", "qm", "step",   name_mat[1, 4:ncol(name_mat)]))

      # unnest
      out <- tidyr::unnest(out, cols = names(out))

      # Clean date conversion dataframe
      cdate <-
        date_convert %>%
        janitor::clean_names() %>%
        dplyr::mutate(dplyr::across(where(is.numeric), as.character))

      # Collect descriptions, parameters, and names to join back w/ processed output sheet
      param_df <-
        tibble::tibble(
          desc         = desc_vect[-1:-3],
          parameter    = param_vect[!param_vect %in% c("Time", "Time", "Operation")],
          name         = clean_cols[!clean_cols %in% "step"]
        ) %>%
        dplyr::mutate(                           # If NA or "#N/A" in desc/parameter, replace w/ name of variable
          desc = dplyr::case_when(
            is.na(desc) | desc == "#N/A" ~ name,
            TRUE                         ~ desc
          ),
          parameter = dplyr::case_when(
            is.na(parameter) | parameter == "#N/A" ~ name,
            TRUE                                   ~ parameter
          )
        )

      # Join CRAM values (date/value columns) w/ the cleaned date data frame to get start/end dates
      out_data <-
        out %>%
        dplyr::left_join(
          dplyr::select(cdate, qm, year = water_year, start_date, end_date),
          by = c("qm", "year")
        ) %>%
        dplyr::mutate(
          model_year     = base_year,
          model_version  = model_version,
          model_scenario = model_paths$model_num[i]
        ) %>%
        dplyr::relocate(model_year, model_version, model_scenario, year, qm, step, start_date, end_date) %>%
        dplyr::mutate(
          start_date = as.Date(start_date, "%m/%d/%Y"),
          end_date   = as.Date(end_date,   "%m/%d/%Y")
        ) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), as.character)) %>%
        tidyr::pivot_longer(cols = c(-model_year, -model_version, -model_scenario,
                                     -year, -qm, -step, -start_date, -end_date)) %>%
        dplyr::left_join(
          param_df,
          by = "name"
        ) %>%
        dplyr::mutate(
          file         = model_paths$full_path[i],
          output_sheet = model_paths$output_sheet[i]
        ) %>%
        dplyr::relocate(model_year, model_version, model_scenario, output_sheet, file) %>%
        dplyr::relocate(name, value, parameter, desc, .after = dplyr::last_col())

    # suppress warnings when pivoting data wider
    suppressWarnings(
      out_data <-
        out_data %>%
        dplyr::mutate(
          value = dplyr::case_when(
            value == "#N/A" ~ "NA",
            TRUE            ~ value
          )
        ) %>%
        dplyr::select(-output_sheet, -file, -parameter, -desc) %>%
        tidyr::pivot_wider(
          names_from  = name,
          values_from = value
        ) %>%
        dplyr::arrange(start_date) %>%
        dplyr::mutate(across(c(-model_year:-end_date), as.numeric))
      )

    # make unique names if output sheet is WILDHORSE
    if(model_paths$output_sheet[i] == "WILDHORSE") {

      # add custom names for wildhorse columns due to them repeating in other output sheets
      out_data <-
        out_data %>%
        stats::setNames(c(names(out_data)[1:8], paste0(names(out_data)[9:ncol(out_data)], "_wildhorse")))

    }

      sheet_lst[[i]] <- out_data
      # rm(out, out_tbl, out_tbl_names)
    }

    # Reduce list of dataframes and inner join by info columns
    split_df <- purrr::reduce(
      sheet_lst,
      inner_join,
      by = c("model_year", "model_version", "model_scenario",
             "year", "qm", "step", "start_date", "end_date")
      )

    # add all output sheets from model to list of models
    model_lst[[z]] <- split_df

  }

  # get columns of each model dataframe
  col_lst <- lapply(model_lst, FUN = function(x){
    names(x)
  })

  # get the length of unique columns in each model dataframe
  df_length <- lapply(col_lst, FUN = function(x){
    length(unique(x))
  })

  # position of model with fewer columns
  min_position <- which.min(unlist(df_length))

  # names of columns in model w/ fewer columns
  min_names <- names(model_lst[[min_position]])

  # select columns in each model to match the model with the fewest number of columns
  for (n in 1:length(model_lst)) {

    # select only relevant names and replace model_lst dataframe back into list
    model_lst[[n]] <- dplyr::select(model_lst[[n]], c(all_of(min_names)))

  }

  # bind model(s) together into single dataframe
  model_df <- dplyr::bind_rows(model_lst)

  # Return a wide dataframe if return_wide == TRUE, otherwise return long dataframe. Default is to return wide dataframe
  if(return_wide == TRUE) {

    return(model_df)

  } else {

    # convert wide dataframe to long
    model_df <-
      model_df %>%
      tidyr::pivot_longer(
        cols      = c(-model_year:-end_date),
        names_to  = "name",
        values_to = "value"
      )

    return(model_df)

  }

}

#' Retrieve available columns in processed CRAM output sheets
#' @description Helper function for obtaining variable names to use in plotting functions, plot_cram and plot_multiple_cram
#' @param df data.frame
#' @return tibble containing the two columns. one column with the names of the columns of input df, and a second column with the column names put into title case
#' @export
#' @importFrom dplyr mutate group_by
#' @importFrom tibble tibble
#' @importFrom stringr str_to_title
#' @importFrom stats na.omit setNames step
find_variables <- function(
    df = NULL
) {

  # if no dataframe is given, return error message
  if(is.null(df)) {
    stop(paste0("Invalid input 'df' in find_variables()"))
  } else {


  # make tibble with column names and title read names
  name_df <-
    tibble::tibble(
       names = names(df)
       ) %>%
     dplyr::mutate(
       title_names = stringr::str_to_title(gsub("_", " ", names))
     )

  return(name_df)

  }

}

#' Plots desired variables from processed CRAM output sheets
#' @param cram_df data.frame containing Processed CRAM output files, output from process_cram function w/ wide = TRUE
#' @param plot_vars character vector containing the names of columns in the processed CRAM dataframe that should be plotted. Default is NULL and will plot the first 2 variables
#' @param start character string, start date YYYY-MM-DD
#' @param end character string, end date YYYY-MM-DD
#' @param wrap logical, whether plot should use facet_wrap or facet_grid for plotting. Default is FALSE, uses facet_grid
#' @return ggplot object
#' @export
#' @importFrom dplyr filter select mutate
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 aes geom_line facet_wrap facet_grid theme_bw labs
#' @importFrom stats na.omit setNames step
plot_cram <- function(
    cram_df   = NULL,
    plot_vars = NULL,
    start     = "1900-01-01",
    end       = "2099-01-01",
    wrap      = FALSE
) {

  # if cram_df is NULL, no dataframe is entered, stop function
  if(is.null(cram_df)) {

    stop(paste0("Invalid input 'cram_df', use output from 'process_cram()' function"))

  }

  # if no plotting variables are specified, select first 2 variables, (8 info columns, 2 variables = [1:10])
  if(is.null(plot_vars)) {

    plot_vars <- find_variables(df = cram_df) %>%
      .$names %>%
      .[9:10]

  }

  # subset data
  cram_sub <-
    cram_df %>%
    dplyr::select(model_scenario, start_date, qm, all_of(plot_vars)) %>%
    dplyr::filter(
      start_date >= start,          # Filter starting date
      start_date <= end             # Filter ending date
    )

  # title case colnames
  title_vars <-
    cram_sub %>%
    find_variables() %>%
    dplyr::filter(!names %in% c("model_scenario", "start_date", "qm"))

  # use title case names for plotting
  names(cram_sub)[4:ncol(cram_sub)] <- title_vars$title_names
  # names(cram_sub)[4:ncol(cram_sub)] <- title_vars$title_names[4:nrow(title_vars)]

  # pivot data longer for plotting
  cram_sub <-
    cram_sub %>%
    tidyr::pivot_longer(cols = c(-1:-3))
  # tidyr::pivot_longer(cols = cram_vars$names[9:12])


  message("Plotting...")

  cram_plot <-
    cram_sub %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(aes(x = start_date, y = value, col = model_scenario))

  # if plots should use facet_wrap or facet_grid to seperate variables
  if(wrap == TRUE) {
    # plotting using facet_wrap
    cram_plot <-
      cram_plot +
      ggplot2::facet_wrap(~name) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        title    = "CRAM Model Scenarios",
        x        = "Date",
        y        = "Value",
        col      = "Model Scenario"
      )

  } else {

    # plotting using facet_grid
    cram_plot <-
      cram_plot +
      ggplot2::facet_grid(name~.) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        title    = "CRAM Model Scenarios",
        x        = "Date",
        y        = "Value",
        col      = "Model Scenario"
      )

  }

  return(cram_plot)

}
