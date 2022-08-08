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
    tibble()

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

#' Process CRAM output

#' @param model_directory dataframe with base_year, model_version, and base_folder columns, indicating the location of a CRAM model folder, a single row from the output dataframe from parse_directory function
#' @param return_wide if TRUE, function returns a wide dataset with a column for each header found in the original OUTPUT_SHEET.txt files, otherwise long dataset w/ multiple rows per timestamp
#' @return a dataframe that has all of the data from the OUTPUT_SHEETS found in the model folder that the function is pointed too.
#' @export
#' @importFrom logger log_error log_info
#' @importFrom tibble tibble
#' @importFrom dplyr mutate case_when left_join select relocate bind_rows `%>%`
#' @importFrom readr read_tsv read_fwf problems
#' @importFrom stats na.omit
#' @importFrom tidyr unnest pivot_longer pivot_wider
#' @importFrom janitor clean_names

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
  file_names <- dplyr::tibble(
    file = list.files(
      base_model_dir,
      full.names = FALSE,
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

  # Empty list to add processed output sheets to at end of for loop
  sheet_lst <- list()

  # loop through the number of OUTPUT_SHEET.txt files in runs/<modelyear_modelversion> directory, clean and bind all rows
  for (i in 1:length(base_output_paths)) {

    logger::log_info("\n\nOutput sheet: {file_names$output_sheet[i]}")

    # # Read in output sheet for extracting names later on
    out_tbl_names <- readr::read_tsv(
      base_output_paths[i],
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
      file           = base_output_paths[i],
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
    tbl_desc    <- out_tbl_names[desc_index, 1:ncol(out)]
    desc_vect   <- unlist(as.vector(tbl_desc[1, ]))

    # params for data in out
    tbl_params  <- out_tbl_names[param_index, 1:ncol(out)]
    param_vect  <- unlist(as.vector(tbl_params[1, ]))

    # names for data in out
    tbl_names   <- out_tbl_names[name_index, 1:ncol(out)]


    # Loop through tbl_names and replace NA column headers w/ --> "empty_{column number}"
    for (c in 1:ncol(tbl_names)) {
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
      tibble(
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
        model_year    = base_year,
        model_version = model_version
      ) %>%
      dplyr::relocate(model_year, model_version, year, qm, step, start_date, end_date) %>%
      mutate(
        start_date = as.Date(start_date, "%m/%d/%Y"),
        end_date   = as.Date(end_date,   "%m/%d/%Y")
      ) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), as.character)) %>%
      tidyr::pivot_longer(cols = c(-model_year, -model_version, -year, -qm, -step, -start_date, -end_date)) %>%
      dplyr::left_join(
        param_df,
        by = "name"
      ) %>%
      dplyr::mutate(
        file         = file_names$file[i],
        output_sheet = file_names$output_sheet[i]
      ) %>%
      dplyr::relocate(name, value, output_sheet, file, .after = dplyr::last_col())


    sheet_lst[[i]] <- out_data
  }

  sheet_df <- dplyr::bind_rows(sheet_lst)

  # Return a wide dataframe if return_wide == TRUE, otherwise return long dataframe
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

#' Retrieve available columns in CRAM output sheet of interest
#' @description Helper function for obtaining variable names to use in plotting functions, plot_cram and plot_multiple_cram
#' @param cram_df data.frame containing Processed CRAM output files, output from process_cram function w/ wide = TRUE
#' @param out_sheet character string, name of the output sheet that variables should be returned for, if exact output sheet name is not given a fuzzy match will be returned.
#' @param start character string, start date year-month-day
#' @param end character string, end date year-month-day
#' @return data.frame containing 5 columns, the last 2 columns being the variables in the output sheet of interest and the type of parameter each variable is.
#' @export
#' @importFrom dplyr mutate filter group_by arrange ungroup
#' @importFrom logger log_info
#' @importFrom utils tail adist
get_variables <- function(
    cram_df,
    out_sheet = "SOUTH_PLATTE",
    start     = "1900-01-01",
    end       = "2099-01-01"
) {
  # Make sheet name uppercase
  out_sheet   <- toupper(c(out_sheet))

  # Avaliable output sheet names
  outsheet_df <-  data.frame(
    output_sheets = c("ARKANSAS", "SOUTH_PLATTE", "PWP", "WILDHORSE")
  )

  # if inputted output sheet doesn't match output sheets, fuzzy match w/ possible output sheet names
  if(!out_sheet %in% outsheet_df$output_sheets) {

    fdist <- adist(
      out_sheet,
      outsheet_df$output_sheets
    ) %>%
      t() %>%
      as.data.frame()

    out_sheet <-
      outsheet_df %>%
      dplyr::mutate(dist = fdist$V1) %>%
      dplyr::filter(dist == min(dist)) %>%
      .$output_sheets

    logger::log_info("\n\nSelected output sheet: {out_sheet}")

  } else {

    out_sheet <- out_sheet
    logger::log_info("\n\nSelected output sheet:{out_sheet}")

  }

  # Subset cram output sheets to output sheet of interest and remove columns w/ only NA values
  cram_sub <-
    cram_df %>%
    dplyr::filter(
      output_sheet == out_sheet,    # Filter to desired output sheet
      start_date >= start,          # Filter starting date
      start_date <= end             # Filter ending date
    ) %>%
    rm_na_cols()                    # Remove columns w/ only NA values

  skip_vars <- c("model_year", "model_version", "year", "qm", "step", "start_date", "end_date", "file", "output_sheet")

  # Collapse all avaliable column variables, excluding skip_vars
  cram_vars     <-sort(names(cram_sub)[!names(cram_sub) %in% skip_vars])

  vars_df <-
    data.frame(
      model_year     = cram_sub$model_year[1],
      model_version  = cram_sub$model_version[1],
      output_sheet   = out_sheet,
      var            = cram_vars,
      param          = sapply(strsplit(cram_vars, "_"), tail, 1)
    ) %>%
    dplyr::group_by(param) %>%
    dplyr::arrange(var, .by_group = TRUE) %>%
    dplyr::ungroup()

  return(vars_df)

}


#' Plots desired variables from processed CRAM output sheets
#'
#' @param cram_df data.frame containing Processed CRAM output files, output from process_cram function w/ wide = TRUE
#' @param out_sheet character string, name of the output sheet that variables should be returned for, if exact output sheet name is not given a fuzzy match will be returned.
#' @param plot_vars character vector containing the names of columns in the processed CRAM output sheet of interest, available columns can be found by using the get_variables function
#' @param start character string, start date year-month-day
#' @param end character string, end date year-month-day
#' @return ggplot object
#' @export
#' @importFrom dplyr filter select contains mutate
#' @importFrom utils adist
#' @importFrom tidyr pivot_longer
#' @importFrom logger log_info
#' @importFrom janitor make_clean_names
#' @importFrom ggplot2 aes geom_line facet_grid theme_bw labs
plot_cram <- function(
    cram_df,
    out_sheet = "SOUTH_PLATTE",
    plot_vars = NULL,
    start     = "1900-01-01",
    end       = "2099-01-01"
) {
  # Make sheet name uppercase
  out_sheet   <- toupper(c(out_sheet))

  # Avaliable output sheet names
  outsheet_df <-  data.frame(
    output_sheets = c("ARKANSAS", "SOUTH_PLATTE", "PWP", "WILDHORSE")
  )

  # if inputted output sheet doesn't match output sheets, fuzzy match w/ possible output sheet names
  if(!out_sheet %in% outsheet_df$output_sheets) {

    fdist <- adist(
      out_sheet,
      outsheet_df$output_sheets
    ) %>%
      t() %>%
      as.data.frame()

    out_sheet <-
      outsheet_df %>%
      dplyr::mutate(dist = fdist$V1) %>%
      dplyr::filter(dist == min(dist)) %>%
      .$output_sheets

    logger::log_info("\n\nSelected output sheet: {out_sheet}")

  } else {

    out_sheet <- out_sheet
    logger::log_info("\n\nSelected output sheet:{out_sheet}")

  }

  # Subset cram output sheets to output sheet of interest and remove columns w/ only NA values
  cram_sub <-
    cram_df %>%
    dplyr::filter(
      output_sheet == out_sheet,    # Filter to desired output sheet
      start_date >= start,          # Filter starting date
      start_date <= end             # Filter ending date
    ) %>%
    rm_na_cols()                    # Remove columns w/ only NA values

  # If variables for plotting are not specified, default NULL will select all avaliable columns
  if(is.null(plot_vars)) {

    skip_vars <- c("model_year", "model_version", "year", "qm", "step", "start_date", "end_date", "file", "output_sheet")

    # Collapse vars to or statements (|) to select all columns
    pvars     <- paste0(
      names(cram_sub)[!names(cram_sub) %in% skip_vars],
      collapse = "|"
    )

    # Get names that match column names
    model_vars <- grep(
      pvars,
      names(cram_sub),
      value = T
    )

    logger::log_info("\n\nSelecting {length(model_vars)} columns for plotting")

    # Selecting desired plotting variables
  } else {

    # Collapse vars to or statements (|) to select only desired columns
    pvars      <- paste0(plot_vars, collapse = "|")

    # Get names that match column names
    model_vars <- grep(
      pvars,
      names(cram_sub),
      value = T
    )

    logger::log_info("\n\nSelecting {length(model_vars)} columns for plotting")

  }

  # Select only desired columns and pivot longer for plotting
  plot_df <-
    cram_sub %>%
    dplyr::select(model_year:output_sheet, dplyr::contains(model_vars)) %>%
    tidyr::pivot_longer(cols = c(-model_year:-output_sheet)) %>%
    dplyr::mutate(value = as.numeric(value))

  # Model year string
  modyear_txt  <- unique(plot_df$model_year)

  # model version string
  modver_txt   <- unique(plot_df$model_version)

  # Output sheet string (title)
  output_title <- janitor::make_clean_names(
    string =  unique(plot_df$output_sheet),
    case   = "title"
  )

  logger::log_info("\n\nPlotting...")

  # plot
  cram_plot <-
    plot_df %>%
    ggplot2::ggplot() +
    ggplot2::geom_line(aes(x = start_date, y = value, col = name)) +
    ggplot2::facet_grid(name~.) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title    = paste0(output_title),
      subtitle = paste0(modyear_txt, " - ", modver_txt),
      x        = "Date",
      y        = "Value",
      col      = ""
    )

  return(cram_plot)

}

#' Title
#'
#' @param cram_mod1 data.frame containing Processed CRAM output files, output from process_cram function w/ wide = TRUE
#' @param cram_mod2 data.frame containing Processed CRAM output files that should be compared to cram_mod1 input. This will also be the output from process_cram function w/ wide = TRUE
#' @param out_sheet character string, name of the output sheet that variables should be returned for, if exact output sheet name is not given a fuzzy match will be returned.
#' @param plot_vars character vector containing the names of columns in the processed CRAM output sheet of interest, available columns can be found by using the get_variables function
#' @param start character string, start date year-month-day
#' @param end character string, end date year-month-day
#' @param wrap logical indicating if output plots should use facet_wrap if TRUE and facet_grid if FALSE, default is FALSE
#' @importFrom dplyr filter select contains mutate bind_rows relocate
#' @importFrom tidyr pivot_longer
#' @importFrom utils adist
#' @importFrom logger log_info log_error
#' @importFrom janitor make_clean_names
#' @importFrom ggplot2 aes geom_line facet_grid facet_wrap theme_bw labs
#' @return
#' @export

plot_multi_cram <- function(
    cram_mod1,
    cram_mod2,
    out_sheet = "SOUTH_PLATTE",
    plot_vars = NULL,
    start     = "1900-01-01",
    end       = "2099-01-01",
    wrap      = FALSE
) {
  # Make sheet name uppercase
  out_sheet   <- toupper(c(out_sheet))

  # Avaliable output sheet names
  outsheet_df <-  data.frame(
    output_sheets = c("ARKANSAS", "SOUTH_PLATTE", "PWP", "WILDHORSE")
  )

  # if inputted output sheet doesn't match output sheets, fuzzy match w/ possible output sheet names
  if(!out_sheet %in% outsheet_df$output_sheets) {

    fdist <- adist(
      out_sheet,
      outsheet_df$output_sheets
    ) %>%
      t() %>%
      as.data.frame()

    out_sheet <-
      outsheet_df %>%
      dplyr::mutate(dist = fdist$V1) %>%
      dplyr::filter(dist == min(dist)) %>%
      .$output_sheets

    logger::log_info("\n\nSelected output sheet: {out_sheet}")

  } else {

    out_sheet <- out_sheet
    logger::log_info("\n\nSelected output sheet:{out_sheet}")

  }

  # Subset cram model 1
  # Subset cram output sheets to output sheet of interest and remove columns w/ only NA values
  cram_sub1 <-
    cram_mod1 %>%
    dplyr::filter(
      output_sheet == out_sheet,    # Filter to desired output sheet
      start_date >= start,          # Filter starting date
      start_date <= end             # Filter ending date
    ) %>%
    rm_na_cols()                    # Remove columns w/ only NA values

  # Subset cram model 2
  # Subset cram output sheets to output sheet of interest and remove columns w/ only NA values
  cram_sub2 <-
    cram_mod2 %>%
    dplyr::filter(
      output_sheet == out_sheet,    # Filter to desired output sheet
      start_date >= start,          # Filter starting date
      start_date <= end             # Filter ending date
    ) %>%
    rm_na_cols()                    # Remove columns w/ only NA values

  cram_sub <- dplyr::bind_rows(cram_sub1, cram_sub2)

  # If variables for plotting are not specified, default NULL will select all available columns
  if(is.null(plot_vars)) {

    logger::log_error("\n\nPlease enter valid input variables\nUse get_variables() function to identify avaliable varaibles within a model version output sheet")

    return(NULL)

    # Selecting desired plotting variables
  } else {

    # Collapse vars to or statements (|) to select only desired columns
    pvars      <- paste0(plot_vars, collapse = "|")

    # Get names that match column names
    model_vars <- grep(
      pvars,
      names(cram_sub),
      value = T
    )

    logger::log_info("\n\nSelecting {length(model_vars)} columns for plotting")

  }

  # Select only desired columns and pivot longer for plotting
  plot_df <-
    cram_sub %>%
    dplyr::select(model_year:output_sheet, dplyr::contains(model_vars)) %>%
    tidyr::pivot_longer(cols = c(-model_year:-output_sheet)) %>%
    dplyr::mutate(
      value              = as.numeric(value),
      model_year_version = paste(model_year, model_version)
    ) %>%
    dplyr::relocate(model_year, model_version, model_year_version)

  # Model year and version string
  mod_year_ver1  <- unique(plot_df$model_year_version)[1]
  mod_year_ver2  <- unique(plot_df$model_year_version)[2]

  # Output sheet string (title)
  output_title <- janitor::make_clean_names(
    string =  unique(plot_df$output_sheet),
    case   = "title"
  )

  # Text indicating which model year versions are being compared
  comp_txt <- paste0(mod_year_ver1, " vs. ", mod_year_ver2)

  logger::log_info("\n\nPlotting...\n{output_title}\n{comp_txt}")


  if(wrap == TRUE) {

    # Facet wrap plots

    # plot
    cram_plot <-
      plot_df %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(aes(x = start_date, y = value, col = model_year_version)) +
      ggplot2::facet_wrap(~name) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        title    = paste0(output_title),
        subtitle = paste0(comp_txt),
        x        = "Date",
        y        = "Value",
        col      = "Models"
      )

    return(cram_plot)

  } else {

    # Facet Grid Plots

    # plot
    cram_plot <-
      plot_df %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(aes(x = start_date, y = value, col = model_year_version)) +
      ggplot2::facet_grid(name~.) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        title    = paste0(output_title),
        subtitle = paste0(comp_txt),
        x        = "Date",
        y        = "Value",
        col      = "Models"
      )

    return(cram_plot)

  }
}
