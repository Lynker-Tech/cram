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

#' @param base_year notes the year of interest and places this text in a column in the final returned dataframe
#' @param model_version notes the version number and places this text in a column in the final returned dataframe*
#' @param base_folder highest level directory housing all the model folders
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
    base_year            = 2022,
    model_version        = "v0.464",
    base_folder          = NULL,
    return_wide          = TRUE) {

  base_model_folder = paste(base_year, model_version)

  # Base model year directory
  base_model_dir <- grep(
    base_model_folder,
    list.files(base_folder, full.names = T),
    value = TRUE
  )

  log_info("Reading From: {base_folder}/{base_model_folder}")

  # Error if model folder not found
  if(length(base_model_dir) == 0) {

    log_error("No model folder matching: {base_model_folder}: \nAvaliable Model folders are: {paste(list.files(base_folder, full.names = F), collapse = ', ')}")
    return(NULL)

  }

  # Paths to base model output sheet .txt files
  base_output_paths <- list.files(
    base_model_dir,
    full.names = TRUE,
    pattern    = "OUTPUT_SHEET.txt"
  )

  if(length(base_output_paths) == 0) {
    log_error("No model folder matching: {base_model_folder} ...\nAvaliable Model folders are: {paste(list.files(base_folder, full.names = F), collapse = ', ')}")
    return(NULL)
  }

  # Paths to base model output sheet .txt files, extract file name
  file_names <- tibble(
    file = list.files(
      base_model_dir,
      full.names = FALSE,
      pattern    = "OUTPUT_SHEET.txt"
    )
  ) %>%
    mutate(
      output_sheet = case_when(
        grepl("ARKANSAS", file)     ~ "ARKANSAS",
        grepl("SOUTH_PLATTE", file) ~ "SOUTH_PLATTE",
        grepl("PWP", file)          ~ "PWP",
        grepl("WILDHORSE", file)    ~ "WILDHORSE"
      )
    )

  sheet_lst <- list()

  # loop through the number of OUTPUT_SHEET.txt files in runs/<modelyear_modelversion> directory, clean and bind all rows
  for (i in 1:length(base_output_paths)) {

    log_info("Reading: {file_names$output_sheet[i]}")

    # # Read in output sheet for extracting names later on
    out_tbl_names <- read_tsv(
      base_output_paths[i],
      col_names      = FALSE,
      show_col_types = FALSE,
      progress = FALSE
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
    out_tbl <- read_fwf(
      file           = base_output_paths[i],
      skip           = skiprows,
      show_col_types = FALSE,
      progress = FALSE
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
      setNames(c("year", "qm", "step",   name_mat[1, 4:ncol(name_mat)]))

    out <- unnest(out, cols = names(out))

    # Clean date conversion dataframe
    cdate <-
      date_convert %>%
      clean_names() %>%
      mutate(across(where(is.numeric), as.character))

    param_df <-
      tibble(
        desc         = unlist(as.vector(out_tbl_names[3, 4:ncol(out)])),
        parameter    = unlist(as.vector(out_tbl_names[4, 4:ncol(out)])),
        name         = unlist(as.vector(name_mat[1, 4:ncol(name_mat)]))
      )

    out_data <-
      out %>%
      left_join(
        select(cdate, qm, year = water_year, start_date, end_date),
        by = c("qm", "year")
      ) %>%
      mutate(
        model_year    = base_year,
        model_version = model_version
      ) %>%
      relocate(model_year, model_version, year, qm, step, start_date, end_date) %>%
      mutate(
        start_date = as.Date(start_date, "%m/%d/%Y"),
        end_date   = as.Date(end_date,   "%m/%d/%Y")
      ) %>%
      mutate(across(where(is.numeric), as.character)) %>%
      pivot_longer(cols = 8:last_col()) %>%
      left_join(
        param_df,
        by = "name"
      ) %>%
      mutate(
        file         = file_names$file[i],
        output_sheet = file_names$output_sheet[i]
      )

    sheet_lst[[i]] <- out_data
  }

  sheet_df <- bind_rows(sheet_lst)

  if(return_wide == TRUE) {
    sheet_df <- sheet_df %>%
      pivot_wider(
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

