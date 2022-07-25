# Purpose: Read raw (quarter-monthly) data from City of Aurora's CRAM model and output interesting data/graphics
# This loops through 'n' number of output sheets & merges them together for use in making graphics
# Author: Bill Szafranski
# Date: 1/1/2022

# clear R working directory
rm(list = ls())
cat("\014")

# load necessary R packages
# will need to install the first time
library(tidyverse)
library(lubridate)
library(gridExtra)
library(RColorBrewer)
library(gt)


# User Parameters ---------------------------------------------------------


### User controlled data
model_version <- c("v0.446")
year_of_simulation <- 2045

# base_model_folder <- "2022 v0.412 Current Conditions"
base_model_folder <- "2045 v0.446"
base_model_ID <- "ARWM 2022-04-11 0.446"
base_prefix <- "000."  #include the "." unless blank
base_run_ID <- ""

compare_model_folder <- "2045 v0.446"
compare_model_ID <- "ARWM 2022-04-11 0.446"  
compare_prefix <- "219."   #include the "." unless blank
compare_run_ID <- ""


# CRAM data sheet names to import
output_sheets <- c("SOUTH_PLATTE_OUTPUT_SHEET", "ARKANSAS_OUTPUT_SHEET", "PWP_OUTPUT_SHEET")
# specify the number of columns each worksheet has, so it doesn't import bad data
output_sheet_columns <- c(181, 325, 212)

# set the project working directory
#base_folder <- "G:/My Drive/PROJECTS/Aurora Water/IWMP2/Task 5 - QAQC/ARWM 2019-09-13 Master and CRAM"
base_folder <-"D:/Projects/Aurora/2021_IWMP2_CRAMupdate/Model/ModelRuns"

# historical model year start and end
year_start <- 1950
year_end <- 2017

# plot size parameters
title_size = 10
xaxis_size = 9
point_size = 1
output_device = ".png"   # '.png' or '.pdf'


# Process the data --------------------------------------------------------

# Calculate avg climate change percent reduction
pct_reduction = NA
if(substring(compare_prefix, 1, 1) == 0){
  pct_reduction = "No CC"
  
}else if(substring(compare_prefix, 1, 1)== 2){
  pct_reduction = "25pct CC"
  
}else if(substring(compare_prefix, 1, 1)== 2){
  pct_reduction = "25pct CC"
  
}else if(substring(compare_prefix, 1, 1)== 3){
  pct_reduction = "37pct CC"
  
}

# calculate/write scenario name
scenario_name <- c(paste0(year_of_simulation, " Historical Hydrology"), 
                   paste0(year_of_simulation, " ", pct_reduction, " ", "(", substring(compare_prefix, 1, 3), ")"))

n_scenario_name <- length(scenario_name)
scenario_folder <- paste0(model_version, " ", scenario_name[1], " vs ", scenario_name[2])
levels_list <- scenario_name      # set this for the plot ordering

# set the base folder
currwd <- base_folder
setwd(currwd)
output_folder <- paste0(base_folder, "/output/", scenario_folder)


# calculate model sheet/folder names
n_output_sheets <- length(output_sheets)
file_prefix <- c(paste0(base_prefix, base_model_ID, base_run_ID), 
                 paste0(compare_prefix, compare_model_ID, compare_run_ID))
n_file_prefix <- length(file_prefix)
folder_prefix <- c(base_model_folder, compare_model_folder)

# create the data list to store the data
data_list <- list()
data_outputsheet_list <- list()
column_names_list <- list()
column_parameter_list <- list()
column_descriptions_list <- list()
column_output_type_list <- list()

# read in the quarter-monthly to date converter
qm_convert <- read_csv("qm_to_date_conversion.csv")


# import the data by model run, merge together and save in a data object
# repeat for each model run (model runs = file_prefix)
for (i in 1:n_file_prefix){
  
  # read all output sheets (2, or # stored in variable 'output_sheets') per model run
  # South Platte, Ark, WISE, etc.
  for (j in 1:n_output_sheets){
    
    # read in the quarter-monthly CRAM model data
    data <- read_tsv(paste0(folder_prefix[i], "/", file_prefix[i], ".", output_sheets[j], ".txt"),
                     col_names = FALSE)
    
    # only need to do this once, not for the comparison model run
    if(i == 1){
      # get the column name & column descriptions
      column_names_list[[j]] <- data[5, ]
      column_parameter_list[[j]] <- data[4, ]
      column_descriptions_list[[j]] <- data[3, ]
      # get the sheet name and put in same format (lots of columns of data/names in 1 row)
      # .name_repair 'unique' makes sure names are unique and not empty to be compliant w/new code (we won't use these names though)
      column_output_type_list[[j]] <- as_tibble(matrix(rep(output_sheets[j], length(data[3, ])),
                                             nrow = 1), .name_repair = "unique")
    }
    
    # name the columns using row 5
    temp_colnames <- as.character(data[5,])
    # rename the first 3 columns
    temp_colnames[1:3] <- c("year", "qm", "step") 
    # set the column names
    colnames(data) <- temp_colnames
    # remove rows 1-5, extract all columns needed
    data <- data[6:dim(data)[1], 1:output_sheet_columns[j]]
    # store the data in the list object
    data_outputsheet_list[[j]] <- data
    
  }
  
  # get the size of the second csv, to use the # of columns next.
  dim_outputsheet2 <- dim(data_outputsheet_list[[2]])
  # get the size of the third list, to use the # of columns next.
  dim_outputsheet3 <- dim(data_outputsheet_list[[3]])
  # merge (column bind) the output sheets together (South Platte & Ark & PWP Output)
  # skip the first 3 columns (year, qm, op step) of dataset 2
  data_temp <- cbind(data_outputsheet_list[[1]], data_outputsheet_list[[2]][,4:dim_outputsheet2[2]],
                     data_outputsheet_list[[3]][,4:dim_outputsheet3[2]])
  
  
  # rename basic data components
  data_list[[i]] <- data_temp %>%
    # make a year-qm column to attach qm-lookup
    mutate(wyqm = paste(year, qm, sep = '-')) %>%
    left_join(., qm_convert, by = "wyqm") %>%
    mutate(Date = mdy(Start.Date)) %>%
    mutate(ModelRun = scenario_name[i]) %>%
    filter(year >= year_start & year <= year_end)
  data_list[[i]]
  
  
  # set up the dictionary
  # we only need to do this once (i==1), not for both model output comparison versions (the output names should be the same)
  if(i == 1){
    # combine column names from the 2 output sheets, but skip the first 3 values
    column_names <- cbind(column_names_list[[1]][, 4:output_sheet_columns[1]],
                          column_names_list[[2]][, 4:output_sheet_columns[2]],
                          column_names_list[[3]][, 4:output_sheet_columns[3]])
    column_parameter <- cbind(column_parameter_list[[1]][, 4:output_sheet_columns[1]],
                              column_parameter_list[[2]][, 4:output_sheet_columns[2]],
                              column_parameter_list[[3]][, 4:output_sheet_columns[3]])
    column_descriptions <- cbind(column_descriptions_list[[1]][, 4:output_sheet_columns[1]],
                                 column_descriptions_list[[2]][, 4:output_sheet_columns[2]],
                                 column_descriptions_list[[3]][, 4:output_sheet_columns[3]])
    column_output_type <- cbind(column_output_type_list[[1]][, 4:output_sheet_columns[1]],
                                column_output_type_list[[2]][, 4:output_sheet_columns[2]],
                                column_output_type_list[[3]][, 4:output_sheet_columns[3]])
  }

  definitions <- data.frame(Name = t(column_names), Description = t(column_descriptions),
                            Parameter = t(column_parameter), OutputSheet = t(column_output_type))
}


# Build the definitions ---------------------------------------------------


# make a look up for parameter units
units.df <- data.frame(Parameter = c("Flow", "High", "Content", "Shortage", "Priority", "Evaporation", "Low"),
                       Units = c("Flow (af)", "Flow (af)", "Contents (af)", "Flow (af)", "Value",
                                 "Flow (af)", "Flow af)"))
# bind the units to the definitions table
definitions <- left_join(definitions, units.df, by = "Parameter")



# add new parameters here
new_data <- data.frame(Name = c("DW_WISE_Water", "Total_WISE_Deliveries", 
                                "Aurora_WISE_Water", "Total_Treated_Water", "Aurora_Drought_Stage",
                                "Aurora_South_Platte_Storage", "Aurora_Ark_Col_Storage",
                                "Total_WTP_Capacity", "Aurora_Total_Storage", "Aurora_Arkansas_Storage",
                                "Aurora_Colorado_Storage", "Aurora_Drought_Reliability",
                                "Griswold_WTP_Indoor_Shortage", "Griswold_WTP_Outdoor_Shortage",
                                "Griswold_WTP_Total_Shortage", "Wemlinger_WTP_Indoor_Shortage",
                                "Wemlinger_WTP_Outdoor_Shortage", "Wemlinger_WTP_Total_Shortage",
                                "Binney_WTP_Indoor_Shortage", "Binney_WTP_Outdoor_Shortage",
                                "Binney_WTP_Total_Shortage", "Fourth_WTP_Indoor_Shortage",
                                "Fourth_WTP_Outdoor_Shortage", "Fourth_WTP_Total_Shortage",
                                "Aurora_LSP_Storage", "Aurora_Total_Annual_Demand",
                                "Aurora_Untreated_WISE", "Aurora_Treated_WISE", "Aurora_PWP_to_Treatment",
                                "PWP_Mass_Balance", "WISE_Unblended_Shortage", "Unused_RBF_capacity_for_WISE",
                                "Unused_PWP_Pipeline_Capacity_for_WISE", "Aurora_Total_ASR_Injection",
                                "Aurora_Total_ASR_Extraction"), 
                       Description = c("Denver Water WISE Contribution", "Total delivery to WISE",
                                       "Aurora Total WISE Contribution", "Aurora Actual Total Treated Water",
                                       "Aurora drought stage", 
                                       "Spinney, Strontia, Jefferson, Qunicy, Aurora, Rampart, WH",
                                       "Homestake, Turquoise, Twin, Box Creek",
                                       "Wemlinger+Griswold+Binney Capacity", "Aurora Total Reservoir Storage",
                                       "Henry&Meredith, Pueblo", "Homestake, Turquoise, Twin, Box Creek",
                                       "Percent Drought Stage (1-3) are Triggered",
                                       "Griswold WTP Indoor Shortage", "Griswold WTP Outdoor Shortage",
                                       "Griswold WTP Total Shortage", "Wemlinger WTP Indoor Shortage",
                                       "Wemlinger WTP Outdoor Shortage", "Wemlinger WTP Total Shortage",
                                       "Binney WTP Indoor Shortage", "Binney WTP Outdoor Shortage",
                                       "Binney WTP Total Shortage", "Fourth WTP Indoor Shortage",
                                       "Fourth WTP Outdoor Shortage", "Fourth WTP Total Shortage",
                                       "Aurora Lower South Platte Storage", "Total City Target Demand",
                                       "Aurora Untreated Water WISE Contribution", "Aurora Treated Water WISE Contribution",
                                       "Aurora PWP Water Treated by All WTPs", 
                                       "Aurora WISE + Aurora PWP Treatmented + DW Conveyed", 
                                       "WISE Unblended Shortage", "Unused RBF capacity for WISE",
                                       "UnusedPWP Pipeline Capacity for WISE", "Total Injection: Gris+Wem+Binney+Fourth",
                                       "Total Extraction: Gris+Wem+Binney+Fourth"),
                       Parameter = c("Flow", "Flow", "Flow", "Flow", "Stage", "Storage", "Storage",
                                     "Flow", "Storage", "Storage", "Storage", "Drought Trigger",
                                     rep("Shortage", 12), "Storage", "Flow", rep("Flow", 4),
                                     "Shortage", "Flow", "Flow", "Flow", "Flow"),
                       OutputSheet = c("Calculated", "Calculated", "Calculated", "Calculated",
                                       "Calculated", "Calculated", "Calculated", "Calculated",
                                       "Calculated", "Calculated", "Calculated", "Calculated",
                                       rep("Calculated", 12), "Calculated", "Calculated", rep("Calculated", 4),
                                       rep("Calculated", 3), rep("Calculated", 2)),
                       Units = c("Flow (af)", "Flow (af)", "Flow (af)", "Flow (af)", "Stage",
                                 "Storage (af)", "Storage (af)", "Flow (af)", "Storage (af)",
                                 "Storage (af)", "Storage (af)", "Percent", rep("Shortage (af)", 12),
                                 "Storage (af)", "Flow (kaf)", rep("Flow (af)", 4),
                                 "Shortage (af)", "Flow (af)", "Flow (af)", "Flow (af)", "Flow (af)")
                       )



definitions <- rbind(definitions, new_data)

tail(definitions)
#definitions <- definitions[1:707, ]

# remove datasets
rm(data)

# Read in South Platte Controls Sheet & Print -------------------------------------


settings <- list()
for (i in 1:n_file_prefix){
#for (i in 1:1){
  
  # read in the quarter-monthly CRAM model data
  data <- read_tsv(paste0(folder_prefix[i], "/", file_prefix[i], ".", "SOUTH_PLATTE_CONTROLS", ".txt"),
                   col_names = FALSE)
  data <- data[1:1100, 2:16]
  
  
  # store the necessary model inputs (add more with new rows of data)
  settings[[i]] <- tibble(Name=NA, Value=NA, Type=NA, Units=NA, OutputSheet=NA)
  settings[[i]][1, 1] <- data[which(data[, 1] == "Total Aurora Reservoirs"), ][1]
  temp <- data[which(data[, 1] == "Total Aurora Reservoirs"), ][2]
  settings[[i]][1, 2] <- as.character(round(as.numeric(as.matrix(temp)), 0))
  
  settings[[i]][2, 1] <- data[which(data[, 1] == "Wild Horse"), ][1]
  settings[[i]][2, 2] <- data[which(data[, 1] == "Wild Horse"), ][2]
  
  settings[[i]][3, 1] <- data[which(data[, 1] == "Box Creek"), ][1]
  temp <- data[which(data[, 1] == "Box Creek"), ][2]
  settings[[i]][3, 2] <- as.character(round(as.numeric(as.matrix(temp)), 0))
  
  settings[[i]][4, 1] <- data[which(data[, 1] == "Whitney Creek"), ][1]
  settings[[i]][4, 2] <- data[which(data[, 1] == "Whitney Creek"), ][2]
  settings[[i]][1:4, 3] <- "Reservoirs"
  settings[[i]][1:4, 4] <- "af"
  settings[[i]][5, 1] <- data[which(data[, 1] == "Total Aurora Annual Demand"), ][1]
  settings[[i]][5, 2] <- data[which(data[, 1] == "Total Aurora Annual Demand"), ][4]
  settings[[i]][5, 3] <- "Demands"
  settings[[i]][5, 4] <- "kaf"
  settings[[i]][6, 1] <- data[which(data[, 1] == "Brighton Gravel Pit"), ][1]
  settings[[i]][6, 2] <- data[which(data[, 1] == "Brighton Gravel Pit"), ][3]
  settings[[i]][7, 1] <- data[which(data[, 1] == "Everist"), ][1]
  settings[[i]][7, 2] <- data[which(data[, 1] == "Everist"), ][3]
  settings[[i]][8, 1] <- data[which(data[, 1] == "Gilcrest Reservoir"), ][1]
  settings[[i]][8, 2] <- data[which(data[, 1] == "Gilcrest Reservoir"), ][3]
  settings[[i]][9, 1] <- data[which(data[, 1] == "Middle South Platte Reservoir"), ][1]
  settings[[i]][9, 2] <- data[which(data[, 1] == "Middle South Platte Reservoir"), ][3]
  settings[[i]][10, 1] <- data[which(data[, 1] == "Lost Creek Aquifer (ASR)"), ][1]
  settings[[i]][10, 2] <- data[which(data[, 1] == "Lost Creek Aquifer (ASR)"), ][3]
  settings[[i]][11, 1] <- data[which(data[, 1] == "Box Elder Creek Aquifer (ASR)"), ][1]
  settings[[i]][11, 2] <- data[which(data[, 1] == "Box Elder Creek Aquifer (ASR)"), ][3]
  settings[[i]][6:11, 3] <- "LSP Reservoirs"
  settings[[i]][6:11, 4] <- "af"
  settings[[i]][12, 1] <- data[which(data[, 1] == "Prairie Waters Project Pipeline Capacity Direct to City"), ][1]
  settings[[i]][12, 2] <- data[which(data[, 1] == "Prairie Waters Project Pipeline Capacity Direct to City"), ][5]
  settings[[i]][13, 1] <- data[which(data[, 1] == "A. R. R. Treatment Rate"), ][1]
  settings[[i]][13, 2] <- data[which(data[, 1] == "A. R. R. Treatment Rate"), ][5]
  settings[[i]][12:13, 3] <- "PWP"
  settings[[i]][12:13, 4] <- "mgd"
  settings[[i]][14, 1] <- data[which(data[, 1] == "Griswold WTP Processing Capacity"), ][1]
  settings[[i]][14, 2] <- data[which(data[, 1] == "Griswold WTP Processing Capacity"), ][4]
  settings[[i]][15, 1] <- data[which(data[, 1] == "Wemlinger WTP Processing Capacity"), ][1]
  settings[[i]][15, 2] <- data[which(data[, 1] == "Wemlinger WTP Processing Capacity"), ][4]
  settings[[i]][16, 1] <- data[which(data[, 1] == "BINNEY WTP Processing Capacity"), ][1]
  settings[[i]][16, 2] <- data[which(data[, 1] == "BINNEY WTP Processing Capacity"), ][4]
  settings[[i]][14:16, 3] <- "Treatment"
  settings[[i]][14:16, 4] <- "mgd"
  settings[[i]][17, 1] <- data[which(data[, 1] == "Defer filling Denver Aquifer"), ][1]
  settings[[i]][17, 2] <- data[which(data[, 1] == "Defer filling Denver Aquifer"), ][2]
  settings[[i]][18, 1] <- data[which(data[, 1] == "Denver Aquifer Water Right Yield"), ][1]
  settings[[i]][18, 2] <- data[which(data[, 1] == "Denver Aquifer Water Right Yield"), ][2]
  settings[[i]][19, 1] <- data[which(data[, 1] == "Griswold Injection Well Capacity"), ][1]
  settings[[i]][19, 2] <- data[which(data[, 1] == "Griswold Injection Well Capacity"), ][2]
  settings[[i]][20, 1] <- data[which(data[, 1] == "Griswold Extraction Well Capacity"), ][1]
  settings[[i]][20, 2] <- data[which(data[, 1] == "Griswold Extraction Well Capacity"), ][2]
  settings[[i]][21, 1] <- data[which(data[, 1] == "Wemlinger Injection Well Capacity"), ][1]
  settings[[i]][21, 2] <- data[which(data[, 1] == "Wemlinger Injection Well Capacity"), ][2]
  settings[[i]][22, 1] <- data[which(data[, 1] == "Wemlinger Extraction Well Capacity"), ][1]
  settings[[i]][22, 2] <- data[which(data[, 1] == "Wemlinger Extraction Well Capacity"), ][2]
  settings[[i]][23, 1] <- data[which(data[, 1] == "Binney Injection Well Capacity"), ][1]
  settings[[i]][23, 2] <- data[which(data[, 1] == "Binney Injection Well Capacity"), ][2]
  settings[[i]][24, 1] <- data[which(data[, 1] == "Binney Extraction Well Capacity"), ][1]
  settings[[i]][24, 2] <- data[which(data[, 1] == "Binney Extraction Well Capacity"), ][2]
  settings[[i]][25, 1] <- data[which(data[, 1] == "Fourth WTP Injection Well Capacity"), ][1]
  settings[[i]][25, 2] <- data[which(data[, 1] == "Fourth WTP Injection Well Capacity"), ][2]
  settings[[i]][26, 1] <- data[which(data[, 1] == "Fourth WTP Extraction Well Capacity"), ][1]
  settings[[i]][26, 2] <- data[which(data[, 1] == "Fourth WTP Extraction Well Capacity"), ][2]
  settings[[i]][17:26, 3] <- "ASR"
  settings[[i]][17:18, 4] <- "af/yr"
  settings[[i]][19:26, 4] <- "mgd"
  settings[[i]][27, 1] <- data[which(data[, 1] == "Aurora Pipeline Capacity from Otero Pipeline to Upper SP Reservoir"), ][1]
  settings[[i]][27, 2] <- data[which(data[, 1] == "Aurora Pipeline Capacity from Otero Pipeline to Upper SP Reservoir"), ][4]
  settings[[i]][28, 1] <- data[which(data[, 1] == "Aurora Pipeline Capacity from Wild Horse to Spinney Reservoir"), ][1]
  settings[[i]][28, 2] <- data[which(data[, 1] == "Aurora Pipeline Capacity from Wild Horse to Spinney Reservoir"), ][4]
  settings[[i]][27:28, 3] <- "Pipelines"
  settings[[i]][27:28, 4] <- "mgd"
  
  settings[[i]][29, 1] <- data[which(data[, 10] == "St Vrain Storage"), ][10]
  settings[[i]][29, 2] <- data[which(data[, 10] == "St Vrain Storage"), ][13]
  settings[[i]][30, 1] <- data[which(data[, 10] == "Big Thompson Storage"), ][10]
  settings[[i]][30, 2] <- data[which(data[, 10] == "Big Thompson Storage"), ][13]
  settings[[i]][31, 1] <- data[which(data[, 10] == "Whitney Storage"), ][10]
  settings[[i]][31, 2] <- data[which(data[, 10] == "Whitney Storage"), ][13]
  settings[[i]][29:31, 3] <- "LSP Reservoirs"
  settings[[i]][29:31, 4] <- "af"
  
  settings[[i]][, 5] <- scenario_name[i]
  
  
  
}



# merge the 'baseline', & 'Comparison' model data together
settings_merge <- rbind(settings[[1]], settings[[2]])

# save the data as a table object (gt)
settings_extract <- settings_merge %>%
  # change the output from 'long' to 'wide' by 'outputsheet'
  group_by(OutputSheet) %>%
  pivot_wider(names_from = c(OutputSheet), values_from = c(Value)) %>%
  gt() %>%
  tab_header(
    title = md("0a. CRAM Model Settings"),
    subtitle = md(paste0(scenario_name[1], " vs ", scenario_name[2]))
  )
settings_extract


# define the table export name & save as pdf
plot_title <- "0a. Model Settings"
file_name <- paste0(plot_title, " table ")

gtsave(data = settings_extract, 
       filename = paste0(output_folder, "/", file_name, model_version, output_device),
       zoom = 0.9
)



# Delete old objects ------------------------------------------------------


rm(column_names, column_parameter, column_descriptions, column_output_type)
rm(column_names_list, column_parameter_list, column_descriptions_list, column_output_type_list)
rm(data_temp, qm_convert, data_outputsheet_list, dim_outputsheet2, dim_outputsheet3, 
   temp_colnames, new_data, units.df)


### make a summary plot list
# This will be used to store plots already made for later use
g_summary <- list()
g_sp_summary <- list()
g_ark_col_summary <- list()
g_demands_summary <- list()
g_WISE_summary <- list()


# Merge the data together --------------------------------------------------


# merge the 'baseline', 'Box Creek' model data together
data_list_merge <- rbind(data_list[[1]], data_list[[2]])


### Check the factor & levels for the 'ModelRun' column (we will plot by this)
# This is likely unique to R and how things plot using ggplot & data frames
factor(data_list_merge$ModelRun)
levels(data_list_merge$ModelRun)
# set the factor 'levels' to the correct plotting order
data_list_merge$ModelRun <-factor(data_list_merge$ModelRun, 
                                  levels = c(scenario_name))

# Levels should be updated now
levels(data_list_merge$ModelRun)


### Convert all data from character to numeric
# get the size
n_all_data <- dim(data_list_merge)
# exclude the appended 'qm_convert' data
end_all_data_format <- n_all_data[2]-13
# set the loop size
i <- seq(1, end_all_data_format, by = 1)
data_list_merge2 = data_list_merge
data_list_merge2[ , i] <- apply(data_list_merge[ , i], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))


# remove the old (non-numeric) dataset & un-merged dataset
rm(data_list, data_list_merge)


# South Platte (min) Apr 30 (QM 28) Reservoir Contents -------------------------------------------------------------


# Max contents are QMs 36-39 (use 37)
# Min contents are QMs 27-29 (use 28)

# Reservoir 15 - spinney
# Res 22 - Aurora Res
# Res 21 - Quincy
# Res 20 - Rampart
# Res 25 - Strontia
# Res 19 - Jefferson Lake
# Res 13 - Wild Horse

# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "Reservoir_15_Content", 
                    "Reservoir_19_Content", "Reservoir_20_Content", "Reservoir_21_Content",
                    "Reservoir_22_Content", "Reservoir_25_Content", "Reservoir_13_Content")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add new calculated column name
site_selection_short <- c(site_selection_short, "Aurora_South_Platte_Storage")
n_site_selection_short <- length(site_selection_short)


# extract the data selected above, put in ggplot format
extract_data <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  # filter for May 1 reservoir contents
  filter(qm == 28) %>%
  # group
  group_by(year, ModelRun) %>%
  # Sum South Platte Res storage
  rowwise() %>% mutate(Aurora_South_Platte_Storage = sum(Reservoir_15_Content, Reservoir_19_Content, 
                                                         Reservoir_20_Content, Reservoir_21_Content,
                                                         Reservoir_22_Content, Reservoir_25_Content,
                                                         Reservoir_13_Content)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
             names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")


# get the max yaxis value
y_max_temp <- max(filter(extract_data, Name == "Aurora_South_Platte_Storage")$Output)*1.05
# set plot parameters
y_axis_max_list <- c(60000, 3500, 1800, 3000, 35000, 1800, 100000, y_max_temp)


### plot the output
p <- list()
for (i in 1:n_site_selection_short){
  
  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])
  
  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p[[i]] <- ggplot(extract_plot, aes_string(x = "Date", y = "Output", color = "ModelRun",
                                            linetype = "ModelRun")) +
    geom_line() +  #col = color_list[i]
    theme_bw() +
    ylab(extract_plot$Units[i]) +
    xlab("Water Year") +
    scale_y_continuous(labels = scales::label_comma(),
                       limits = c(0, y_axis_max_list[i])) +
    ggtitle(paste0(extract_plot$Name[1], " - ", extract_plot$Description[1])) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  #p[[i]]
  
}



# define the plot name
plot_title <- "1a. April 30 (~min) Annual South Platte Reservoir Contents"
file_name <- paste0(plot_title, " 4x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], p[[7]], p[[8]], nrow = 4,
               top = plot_title,
               right = ""))


# save some data to the output list
g_sp_summary[[1]] <- p[[8]]


# South Platte (max) July 8 (QM 37) Reservoir Contents -------------------------------------------------------------


# Max contents are QMs 36-39 (use 37)
# Min contents are QMs 27-29 (use 28)

# Reservoir 15 - spinney
# Res 22 - Aurora Res
# Res 21 - Quincy
# Res 20 - Rampart
# Res 25 - Strontia
# Res 19 - Jefferson Lake
# Res 13 - Wild Horse

# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "Reservoir_15_Content", 
                    "Reservoir_19_Content", "Reservoir_20_Content", "Reservoir_21_Content",
                    "Reservoir_22_Content", "Reservoir_25_Content", "Reservoir_13_Content")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add new calculated column name
site_selection_short <- c(site_selection_short, "Aurora_South_Platte_Storage")
n_site_selection_short <- length(site_selection_short)


# extract the data selected above, put in ggplot format
extract_data <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  # filter for May 1 reservoir contents
  filter(qm == 37) %>%
  # group
  group_by(year, ModelRun) %>%
  # Sum South Platte Res storage
  rowwise() %>% mutate(Aurora_South_Platte_Storage = sum(Reservoir_15_Content, Reservoir_19_Content, 
                                                         Reservoir_20_Content, Reservoir_21_Content,
                                                         Reservoir_22_Content, Reservoir_25_Content,
                                                         Reservoir_13_Content)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")


# get the max yaxis value
y_max_temp <- max(filter(extract_data, Name == "Aurora_South_Platte_Storage")$Output)*1.05
# set plot parameters
y_axis_max_list <- c(60000, 3500, 1800, 3000, 35000, 1800, 100000, y_max_temp)


### plot the output
p <- list()
for (i in 1:n_site_selection_short){
  
  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])
  
  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p[[i]] <- ggplot(extract_plot, aes_string(x = "Date", y = "Output", color = "ModelRun",
                                            linetype = "ModelRun")) +
    geom_line() +  #col = color_list[i]
    theme_bw() +
    ylab(extract_plot$Units[i]) +
    xlab("Water Year") +
    scale_y_continuous(labels = scales::label_comma(),
                       limits = c(0, y_axis_max_list[i])) +
    ggtitle(paste0(extract_plot$Name[1], " - ", extract_plot$Description[1])) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  #p[[i]]
  
}



# define the plot name
plot_title <- "1b. July 8 (~max) Annual South Platte Reservoir Contents"
file_name <- paste0(plot_title, " 4x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], p[[7]], p[[8]], nrow = 4,
               top = plot_title,
               right = ""))



# South Platte Average QM Reservoir Contents -------------------------------------------------------------

# Reservoir 15 - spinney
# Res 22 - Aurora Res
# Res 21 - Quincy
# Res 20 - Rampart
# Res 25 - Strontia
# Res 19 - Jefferson Lake
# Res 13 - Wild Horse

# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "Reservoir_15_Content", 
                    "Reservoir_19_Content", "Reservoir_20_Content", "Reservoir_21_Content",
                    "Reservoir_22_Content", "Reservoir_25_Content", "Reservoir_13_Content")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add new calculated column name
site_selection_short <- c(site_selection_short, "Aurora_South_Platte_Storage")
n_site_selection_short <- length(site_selection_short)


# extract the data selected above, put in ggplot format
extract_data <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  # group
  group_by(qm, ModelRun) %>%
  # filter for May 1 reservoir contents
  summarise(across(site_selection[site_start_no]:site_selection[n_site_selection], mean)) %>%
  # Sum South Platte Res storage
  rowwise() %>% mutate(Aurora_South_Platte_Storage = sum(Reservoir_15_Content, Reservoir_19_Content, 
                                                         Reservoir_20_Content, Reservoir_21_Content,
                                                         Reservoir_22_Content, Reservoir_25_Content,
                                                         Reservoir_13_Content)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")


# get the max yaxis value
y_max_temp <- max(filter(extract_data, Name == "Aurora_South_Platte_Storage")$Output)*1.05
# set plot parameters
y_axis_max_list <- c(60000, 3500, 1800, 3000, 35000, 1800, 100000, y_max_temp)


### plot the output
p <- list()
for (i in 1:n_site_selection_short){
  
  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])
  
  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p[[i]] <- ggplot(extract_plot, aes_string(x = "qm", y = "Output", color = "ModelRun",
                                            linetype = "ModelRun")) +
    geom_line() +  #col = color_list[i]
    theme_bw() +
    ylab(extract_plot$Units[i]) +
    xlab("Water Year") +
    scale_y_continuous(labels = scales::label_comma(), limits = c(0, y_axis_max_list[i])) +
    ggtitle(paste0(extract_plot$Name[1], " - ", extract_plot$Description[1])) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  
}


# define the plot name
plot_title <- "1c. Average QM South Platte Reservoir Contents"
file_name <- paste0(plot_title, " 4x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], p[[7]], p[[8]], nrow = 4,
               top = plot_title,
               right = ""))



# South Platte Quarter-Monthly Reservoir Plots -----------------------------------------

# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "Reservoir_15_Content", 
                    "Reservoir_19_Content", "Reservoir_20_Content", "Reservoir_21_Content",
                    "Reservoir_22_Content",  "Reservoir_25_Content", "Reservoir_13_Content")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add new calculated column name
site_selection_short <- c(site_selection_short, "Aurora_South_Platte_Storage")
n_site_selection_short <- length(site_selection_short)


# extract the data selected above, put in ggplot format
extract_data <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  group_by(Date, ModelRun) %>%
  # Sum South Platte Res storage
  rowwise() %>% mutate(Aurora_South_Platte_Storage = sum(Reservoir_15_Content, Reservoir_19_Content, 
                                                         Reservoir_20_Content, Reservoir_21_Content,
                                                         Reservoir_22_Content, Reservoir_25_Content,
                                                         Reservoir_13_Content)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")


# get the max yaxis value
y_max_temp <- max(filter(extract_data, Name == "Aurora_South_Platte_Storage")$Output)*1.05
# set plot parameters
y_axis_max_list <- c(60000, 3500, 1800, 3000, 35000, 1800, 100000, y_max_temp)


### plot the output
p <- list()
for (i in 1:n_site_selection_short){
  
  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])
  
  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p[[i]] <- ggplot(extract_plot, aes_string(x = "Date", y = "Output", color = "ModelRun",
                                            linetype = "ModelRun")) +
    geom_line() +  #col = color_list[i]
    theme_bw() +
    ylab(extract_plot$Units[i]) +
    xlab("Water Year") +
    scale_y_continuous(labels = scales::label_comma(), limits = c(0, y_axis_max_list[i])) +
    ggtitle(paste0(extract_plot$Name[1], " - ", extract_plot$Description[1])) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  
  
}



# # define the plot name
# plot_title <- "1b. Quarter-Monthly South Platte Reservoir Contents"
# file_name <- paste0(plot_title, " 4x2 ")
# 
# # save the plot
# ggsave(
#   paste0(output_folder, "/", file_name, model_version, output_device),
#   width = 14, height = 8,
#   grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], p[[7]], p[[8]], nrow = 4,
#                top = plot_title,
#                right = ""))





# Ark/Colorado (min) April 30 (QM 28) Reservoir Contents -------------------------------------------------

# Res 2 = Homestake (aurora)
# Link 23 - Turquoise total storage (Aurora )
# DataObject 9 - Auroras Twin Lakes Account
# Link 555 = Box Ck Res (Aurora portion)
# DataObject 13 - Aurora's Pueblo Water
# DataObject 7 - Auroras Henry & Meredith

# DataObject 9 - Auroras Twin Lakes Account
# DataObject 10 - Colo Springs Twin Lakes Account
# DataObject 11 - PBWW Twin Lakes Account
# DataObject 12 - Other Twin Lakes Account

### Not used
# Link 575 - Aurora's Twin Lakes allocation
# Link 87 - Aurora total to Twin Storage
# DataObject 1 - Aurora's Homestake in Turquoise Res
# DataObject 2 - Aurora's CF&I acct in Turquoise Res
# Res 9 = Box Creek (tota)
# Res 6 = Henry/Meredith (total)
# Res 5 = Pueblo Res (total res storage)
# Res 27 = Holbrook
# Res 4 = Twin Lake
# Res 1 = Turquoise


# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "Reservoir_2_Content", 
                    "Link_23_Flow", "Link_555_Flow", "DataObject_13_Flow", 
                    "DataObject_7_Flow", "DataObject_9_Flow",  
                    "DataObject_10_Flow", "DataObject_11_Flow", "DataObject_12_Flow")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add new calculated column name
site_selection_short <- c(site_selection_short, "Aurora_Colorado_Storage", "Aurora_Arkansas_Storage",
                          "Aurora_Ark_Col_Storage")
n_site_selection_short <- length(site_selection_short)


# extract the data selected above, put in ggplot format
extract_data <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  # filter for April 30 reservoir contents
  filter(qm == 28) %>%
  group_by(year, ModelRun) %>%
  # Sum Colorado Basin Res storage
  rowwise() %>% mutate(Aurora_Colorado_Storage = sum(Reservoir_2_Content, Link_23_Flow, 
                                                    DataObject_9_Flow, Link_555_Flow)) %>%
  # Sum Arkansas Basin Res storage
  rowwise() %>% mutate(Aurora_Arkansas_Storage = sum(DataObject_13_Flow, DataObject_7_Flow)) %>%
  # Sum Ark/Colo Res storage
  rowwise() %>% mutate(Aurora_Ark_Col_Storage = sum(Reservoir_2_Content, Link_23_Flow, 
                                                    DataObject_9_Flow, Link_555_Flow,
                                                    DataObject_13_Flow, DataObject_7_Flow)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")


# get the max yaxis value
y_max_temp <- max(filter(extract_data, Name == "Aurora_Ark_Col_Storage")$Output)*1.05
# set plot parameters
y_axis_max_list <- c(rep(y_max_temp, n_site_selection_short))


### plot the output
p <- list()
for (i in 1:n_site_selection_short){
  
  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])
  
  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p[[i]] <- ggplot(extract_plot, aes_string(x = "Date", y = "Output", color = "ModelRun",
                                            linetype = "ModelRun")) +
    geom_line() +  #col = color_list[i]
    theme_bw() +
    ylab(extract_plot$Units[i]) +
    xlab("Water Year") +
    scale_y_continuous(labels = scales::label_comma(),
                       limits = c(0, NA)) +
    ggtitle(paste0(extract_plot$Name[1], " - ", extract_plot$Description[1])) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  #p[[i]]
  
}



# define the plot name
plot_title <- "2a. April 30 (~min) Annual Ark-Colo Reservoir Contents"
file_name <- paste0(plot_title, " 5x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[1]], p[[2]], p[[6]], p[[3]], p[[10]],
               p[[5]], p[[4]], p[[11]], 
               p[[12]], nrow = 5,
               top = plot_title, right = ""))

# # define the plot name
# plot_title <- "2b. May 1 Annual Twin Lake Reservoir Contents"
# file_name <- paste0(plot_title, " 4x1 ")
# 
# # save the plot
# ggsave(
#   paste0(output_folder, "/", file_name, model_version, " ", output_device),
#   width = 14, height = 8,
#   grid.arrange(p[[6]], p[[7]], p[[8]], p[[9]], nrow = 4,
#                top = plot_title, right = ""))


# save some data to the output list
g_ark_col_summary[[1]] <- p[[10]]
g_ark_col_summary[[2]] <- p[[11]]
g_ark_col_summary[[3]] <- p[[12]]


# Ark/Colorado (max) July 17 (QM 38) Reservoir Contents -------------------------------------------------

# max is QM 37 - 40 (use QM 38)

# Res 2 = Homestake (aurora)
# Link 23 - Turquoise total storage (Aurora )
# DataObject 9 - Auroras Twin Lakes Account
# Link 555 = Box Ck Res (Aurora portion)
# DataObject 13 - Aurora's Pueblo Water
# DataObject 7 - Auroras Henry & Meredith


# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "Reservoir_2_Content", 
                    "Link_23_Flow", "Link_555_Flow", "DataObject_13_Flow", 
                    "DataObject_7_Flow", "DataObject_9_Flow",  
                    "DataObject_10_Flow", "DataObject_11_Flow", "DataObject_12_Flow")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add new calculated column name
site_selection_short <- c(site_selection_short, "Aurora_Colorado_Storage", "Aurora_Arkansas_Storage",
                          "Aurora_Ark_Col_Storage")
n_site_selection_short <- length(site_selection_short)


# extract the data selected above, put in ggplot format
extract_data <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  # filter for April 30 reservoir contents
  filter(qm == 38) %>%
  group_by(year, ModelRun) %>%
  # Sum Colorado Basin Res storage
  rowwise() %>% mutate(Aurora_Colorado_Storage = sum(Reservoir_2_Content, Link_23_Flow, 
                                                     DataObject_9_Flow, Link_555_Flow)) %>%
  # Sum Arkansas Basin Res storage
  rowwise() %>% mutate(Aurora_Arkansas_Storage = sum(DataObject_13_Flow, DataObject_7_Flow)) %>%
  # Sum Ark/Colo Res storage
  rowwise() %>% mutate(Aurora_Ark_Col_Storage = sum(Reservoir_2_Content, Link_23_Flow, 
                                                    DataObject_9_Flow, Link_555_Flow,
                                                    DataObject_13_Flow, DataObject_7_Flow)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")


# get the max yaxis value
y_max_temp <- max(filter(extract_data, Name == "Aurora_Ark_Col_Storage")$Output)*1.05
# set plot parameters
y_axis_max_list <- c(rep(y_max_temp, n_site_selection_short))


### plot the output
p <- list()
for (i in 1:n_site_selection_short){
  
  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])
  
  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p[[i]] <- ggplot(extract_plot, aes_string(x = "Date", y = "Output", color = "ModelRun",
                                            linetype = "ModelRun")) +
    geom_line() +  #col = color_list[i]
    theme_bw() +
    ylab(extract_plot$Units[i]) +
    xlab("Water Year") +
    scale_y_continuous(labels = scales::label_comma(),
                       limits = c(0, NA)) +
    ggtitle(paste0(extract_plot$Name[1], " - ", extract_plot$Description[1])) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  #p[[i]]
  
}



# define the plot name
plot_title <- "2b. July 17 (~max) Annual Ark-Colo Reservoir Contents"
file_name <- paste0(plot_title, " 5x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[1]], p[[2]], p[[6]], p[[3]], p[[10]],
               p[[5]], p[[4]], p[[11]], 
               p[[12]], nrow = 5,
               top = plot_title, right = ""))



# Ark/Colorado Average QM Reservoir Contents -------------------------------------------------

# Res 2 = Homestake (aurora)
# Link 23 - Turquoise total storage (Aurora )
# DataObject 9 - Auroras Twin Lakes Account
# Link 555 = Box Ck Res (Aurora portion)
# DataObject 13 - Aurora's Pueblo Water
# DataObject 7 - Auroras Henry & Meredith

# DataObject 9 - Auroras Twin Lakes Account
# DataObject 10 - Colo Springs Twin Lakes Account
# DataObject 11 - PBWW Twin Lakes Account
# DataObject 12 - Other Twin Lakes Account


# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "Reservoir_2_Content", 
                    "Link_23_Flow", "Link_555_Flow", "DataObject_13_Flow", 
                    "DataObject_7_Flow", "DataObject_9_Flow",  
                    "DataObject_10_Flow", "DataObject_11_Flow", "DataObject_12_Flow")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add new calculated column name
site_selection_short <- c(site_selection_short, "Aurora_Colorado_Storage", "Aurora_Arkansas_Storage",
                          "Aurora_Ark_Col_Storage")
n_site_selection_short <- length(site_selection_short)


# extract the data selected above, put in ggplot format
extract_data <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  # group
  group_by(qm, ModelRun) %>%
  # filter for May 1 reservoir contents
  summarise(across(site_selection[site_start_no]:site_selection[n_site_selection], mean)) %>%
  # Sum Colorado Basin Res storage
  rowwise() %>% mutate(Aurora_Colorado_Storage = sum(Reservoir_2_Content, Link_23_Flow, 
                                                     DataObject_9_Flow, Link_555_Flow)) %>%
  # Sum Arkansas Basin Res storage
  rowwise() %>% mutate(Aurora_Arkansas_Storage = sum(DataObject_13_Flow, DataObject_7_Flow)) %>%
  # Sum Ark/Colo Res storage
  rowwise() %>% mutate(Aurora_Ark_Col_Storage = sum(Reservoir_2_Content, Link_23_Flow, 
                                                    DataObject_9_Flow, Link_555_Flow,
                                                    DataObject_13_Flow, DataObject_7_Flow)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")


# get the max yaxis value
y_max_temp <- max(filter(extract_data, Name == "Aurora_Ark_Col_Storage")$Output)*1.05
# set plot parameters
y_axis_max_list <- c(rep(y_max_temp, n_site_selection_short))


### plot the output
p <- list()
for (i in 1:n_site_selection_short){
  
  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])
  
  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p[[i]] <- ggplot(extract_plot, aes_string(x = "qm", y = "Output", color = "ModelRun",
                                            linetype = "ModelRun")) +
    geom_line() +  #col = color_list[i]
    theme_bw() +
    ylab(extract_plot$Units[i]) +
    xlab("Water Year") +
    scale_y_continuous(labels = scales::label_comma(),
                       limits = c(0, NA)) +
    ggtitle(paste0(extract_plot$Name[1], " - ", extract_plot$Description[1])) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  
}



# define the plot name
plot_title <- "2c. Average Quarter-Monthly Ark-Colo Reservoir Contents"
file_name <- paste0(plot_title, " 5x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[1]], p[[2]], p[[6]], p[[3]], p[[10]],
               p[[5]], p[[4]], p[[11]], p[[12]], nrow = 5,
               top = plot_title, right = ""))



# Akr/Col Quarter-Monthly Time Series Reservoirs --------------------------------------

# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "Reservoir_2_Content", 
                    "Link_23_Flow", "Link_555_Flow", "DataObject_13_Flow", 
                    "DataObject_7_Flow", "DataObject_9_Flow",  
                    "DataObject_10_Flow", "DataObject_11_Flow", "DataObject_12_Flow")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add new calculated column name
site_selection_short <- c(site_selection_short, "Aurora_Colorado_Storage", "Aurora_Arkansas_Storage",
                          "Aurora_Ark_Col_Storage")
n_site_selection_short <- length(site_selection_short)



# extract the data selected above, put in ggplot format
extract_data <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  group_by(Date, ModelRun) %>%
  rowwise() %>% mutate(Aurora_Colorado_Storage = sum(Reservoir_2_Content, Link_23_Flow, 
                                                     DataObject_9_Flow, Link_555_Flow)) %>%
  # Sum Arkansas Basin Res storage
  rowwise() %>% mutate(Aurora_Arkansas_Storage = sum(DataObject_13_Flow, DataObject_7_Flow)) %>%
  # Sum Ark/Colo Res storage
  rowwise() %>% mutate(Aurora_Ark_Col_Storage = sum(Reservoir_2_Content, Link_23_Flow, 
                                                    DataObject_9_Flow, Link_555_Flow,
                                                    DataObject_13_Flow, DataObject_13_Flow)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")




### plot the output
p <- list()
for (i in 1:n_site_selection_short){
  
  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])
  
  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p[[i]] <- ggplot(extract_plot, aes_string(x = "Date", y = "Output", color = "ModelRun",
                                            linetype = "ModelRun")) +
    geom_line() +  #col = color_list[i]
    theme_bw() +
    #ylim(0, y_axis_max_list[i]) +
    ylab(extract_plot$Units[i]) +
    xlab("Water Year") +
    scale_y_continuous(labels = scales::label_comma(),
                       limits = c(0, NA)) +
    ggtitle(paste0(extract_plot$Name[1], " - ", extract_plot$Description[1])) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  #p[[i]]
  
}


# # define the plot name
# plot_title <- "2c. Quarter-Monthly Ark-Colo Reservoir Contents"
# file_name <- paste0(plot_title, " 4x2 ")
# 
# # save the plot
# ggsave(
#   paste0(output_folder, "/", file_name, model_version, output_device),
#   width = 14, height = 8,
#   grid.arrange(p[[1]], p[[2]], p[[6]], p[[3]], p[[10]],
#                p[[5]], p[[4]], p[[11]], 
#                p[[12]], nrow = 5,
#                top = plot_title, right = ""))

# # define the plot name
# plot_title <- "2d. Quarter-Monthly Twin Lake Reservoir Contents"
# file_name <- paste0(plot_title, " 4x1 ")
# 
# # save the plot
# ggsave(
#   paste0(output_folder, "/", file_name, model_version, " ", output_device),
#   width = 14, height = 8,
#   grid.arrange(p[[6]], p[[7]], p[[8]], p[[9]], nrow = 4,
#                top = plot_title, right = ""))


# Box Creek Reservoir Plots -----------------------------------------------

### Placeholder data to build Box Creek Res output
# 
# # select the sites you want to plot
# site_selection <- c("year", "qm", "Date", "ModelRun", "Link_555_Flow", 
#                     "Link_1191_Flow", "Link_1226_Flow", "Link_1227_Flow",
#                     "Reservoir_9_Content",  "Link_540_Flow", "Link_1231_Flow",
#                     "Link_1228_Flow", "Link_555_Flow")
# n_site_selection <- length(site_selection)
# site_start_no <- 5
# site_selection_short <- site_selection[site_start_no:n_site_selection]
# n_site_selection_short <- length(site_selection_short)
# 
# 


# Demands Plots ---------------------------------------------------------------

# demand 71 - treated water demand
# Link 1137 - Binney indoor contribution
# Link 1138 _ Binney Outdoor contribution
# Link 780 - griswold indoor contribution
# Link 937 Griswold outdoor contribution
# Link 781 Wemlinger indoor
# Link 938 Wemlinger outdoor

# DataObject 98 - demand reduction due to drought level triggers
# DataObject 172 - reliable supply
# DataObject 173 - operating reserve
# Data object 174 = months of supply
# Data object 129 = drought trigger level

# DataObject 96 - actual indoor treatment
# DataObject 97 - actual outdoor treatment

# Link 826 - griswold treat cap
# Link 823 - Wemlinger treat cap
# Link_1136_Flow - Binney treat cap

# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "Link_1137_Flow", "Link_1138_Flow", 
                    "Link_780_Flow", "Link_937_Flow", "Link_781_Flow", "Link_938_Flow",
                    "Demand_71_Flow", "Demand_155_Flow",
                    "DataObject_83_Flow", "DataObject_86_Flow",
                    "DataObject_174_Flow", "DataObject_129_Flow", "DataObject_172_Flow",
                    "DataObject_173_Flow", "DataObject_98_Flow",
                    "DataObject_96_Flow", "DataObject_97_Flow",
                    "Link_826_Flow", "Link_823_Flow", "Link_1136_Flow")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add new calculated column name
site_selection_short <- c(site_selection_short, "Total_WTP_Capacity")
n_site_selection_short <- length(site_selection_short)


# extract the data selected above, put in ggplot format
extract_data <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  # group by year & ModelRun
  group_by(year, ModelRun) %>%
  # sum to annual by existing groups
  summarise(across(site_selection[site_start_no]:site_selection[n_site_selection], sum)) %>%
  # calculate total WTP capacity
  rowwise() %>% mutate(Total_WTP_Capacity = sum(Link_826_Flow, Link_823_Flow,
                                             Link_1136_Flow)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")

# calculate max values to set the y-axis dynamically
y_max_temp <- max(filter(extract_data, Name == "Demand_71_Flow")$Output)*1.1
y_max_temp2 <- max(filter(extract_data, Name=="Link_1137_Flow"| Name =="Link_1138_Flow" | 
                        Name =="Link_780_Flow" | Name =="Link_937_Flow" | 
                        Name =="Link_781_Flow" | Name =="Link_938_Flow")$Output)*1.1
y_max_temp3 <- max(filter(extract_data, Name == "Total_WTP_Capacity")$Output)*1.05
y_axis_max_list <- c(rep(y_max_temp2, 6), y_max_temp, rep(NA, 7), y_max_temp,
                     y_max_temp, y_max_temp, rep(y_max_temp3, 5))


### plot the output
p <- list()
for (i in 1:n_site_selection_short){
  
  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])
  
  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p[[i]] <- ggplot(extract_plot, aes_string(x = "year", y = "Output", color = "ModelRun", linetype = "ModelRun")) + 
    geom_line() +  #col = color_list[i]
    theme_bw() +
    scale_fill_brewer(palette="Dark2") +
    scale_y_continuous(labels = scales::label_comma(),
                       limits = c(0, y_axis_max_list[i])) +
    ylab(extract_plot$Units[i]) +
    xlab("Water Year") +
    ggtitle(paste0(site_selection_short[i]," - ", extract_plot$Description[1])) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  
}

p[[1]]

# define the plot name
plot_title <- "4a. Aurora Annual Demands - Total Treatment Plant Deliveries"
file_name <- paste0(plot_title, " 3x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[18]], p[[19]], p[[20]], p[[21]], p[[7]], nrow = 3,
               top = plot_title,
               right = ""))


# define the plot name
plot_title <- "4b. Aurora Annual Demands - Water Treatment Plant Deliveries"
file_name <- paste0(plot_title, " 3x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, " ", output_device),
  width = 14, height = 8,
  grid.arrange(p[[3]], p[[4]], p[[5]], p[[6]], p[[1]], p[[2]], nrow = 3,
               top = plot_title, right = ""))


# define the plot name
plot_title <- "4c. Aurora Annual Demands - Indoor and Outdoor Treatment"
file_name <- paste0(plot_title, " 2x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[7]], p[[15]], p[[16]], p[[17]], nrow = 2,
               top = plot_title, right = ""))


# # define the plot name
# plot_title <- "4d. Aurora Annual Demands - Color of Water"
# file_name <- paste0(plot_title, " 2x2 ")
# 
# # save the plot
# ggsave(
#   paste0(output_folder, "/", file_name, model_version, " ", output_device),
#   width = 14, height = 8,
#   grid.arrange(p[[9]], p[[10]], p[[8]], nrow = 2,
#                top = plot_title, right = ""))


# save some data to the output list
g_demands_summary[[1]] <- p[[7]]
g_demands_summary[[2]] <- p[[15]]

rm(y_max_temp, y_max_temp2, y_max_temp3)



# Demands Shortage Plots (4d, 4e, 4f) ---------------------------------------------------------------

# Link 1137 - Binney indoor contribution
# Link 1138 _ Binney Outdoor contribution
# Link 780 - griswold indoor contribution
# Link 937 Griswold outdoor contribution
# Link 781 Wemlinger indoor
# Link 938 Wemlinger outdoor
# Link 914 - Fourth Indoor WTP Demand
# Link 939 - Fourth Outdoor WTP Demand

# Demand_114_Shortage - Effluent Commitments Shortage
# Demand_126_Shortage - Cherry Creek Effluent Shortage
# WISE_Unblended_Shortage: Demand_141_High  MINUS  Demand_141_Flow
# Demand_143_Shortage - WISE Blended Shortage
# Unused_RBF_capacity_for_WISE:  Demand_134_Low MINUS Demand_134_Flow TIMES -1
# Unused_PWP_Pipeline_Capacity_for_WISE: Link_995_High MINUS Link_995_Flow


# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "Link_1137_Flow", "Link_1138_Flow", 
                    "Link_780_Flow", "Link_937_Flow", "Link_781_Flow", "Link_938_Flow",
                    "Link_914_Flow", "Link_939_Flow",
                    "Link_1137_High", "Link_1138_High", "Link_780_High", "Link_937_High", 
                    "Link_781_High", "Link_938_High", "Link_914_High", "Link_939_High",
                    "Demand_114_Shortage", "Demand_126_Shortage", "Demand_141_High",
                    "Demand_141_Flow", "Demand_143_Shortage", "Demand_134_Low", "Demand_134_Flow",
                    "Link_995_High", "Link_995_Flow")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add new calculated column name
site_selection_short <- c("Griswold_WTP_Indoor_Shortage", "Griswold_WTP_Outdoor_Shortage",
                          "Griswold_WTP_Total_Shortage", "Wemlinger_WTP_Indoor_Shortage",
                          "Wemlinger_WTP_Outdoor_Shortage", "Wemlinger_WTP_Total_Shortage",
                          "Binney_WTP_Indoor_Shortage", "Binney_WTP_Outdoor_Shortage",
                          "Binney_WTP_Total_Shortage", "Fourth_WTP_Indoor_Shortage",
                          "Fourth_WTP_Outdoor_Shortage", "Fourth_WTP_Total_Shortage",
                          "Demand_114_Shortage", "Demand_126_Shortage", "Demand_143_Shortage", 
                          "WISE_Unblended_Shortage",
                          "Unused_RBF_capacity_for_WISE", "Unused_PWP_Pipeline_Capacity_for_WISE"
                          )
n_site_selection_short <- length(site_selection_short)


# extract the data selected above, put in ggplot format
extract_data <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  # group by year & ModelRun
  group_by(year, ModelRun) %>%
  # sum to annual by existing groups
  summarise(across(site_selection[site_start_no]:site_selection[n_site_selection], sum)) %>%
  # calculate total WTP capacity
  rowwise() %>% mutate(Griswold_WTP_Indoor_Shortage = (Link_780_High - Link_780_Flow),
                       Griswold_WTP_Outdoor_Shortage = (Link_937_High - Link_937_Flow),
                       Griswold_WTP_Total_Shortage = (Griswold_WTP_Indoor_Shortage + Griswold_WTP_Outdoor_Shortage),
                       Wemlinger_WTP_Indoor_Shortage = (Link_781_High - Link_781_Flow),
                       Wemlinger_WTP_Outdoor_Shortage = (Link_938_High - Link_938_Flow),
                       Wemlinger_WTP_Total_Shortage = (Wemlinger_WTP_Indoor_Shortage + Wemlinger_WTP_Outdoor_Shortage),
                       Binney_WTP_Indoor_Shortage = (Link_1137_High - Link_1137_Flow),
                       Binney_WTP_Outdoor_Shortage = (Link_1138_High - Link_1138_Flow),
                       Binney_WTP_Total_Shortage = (Binney_WTP_Indoor_Shortage + Binney_WTP_Outdoor_Shortage),
                       Fourth_WTP_Indoor_Shortage = (Link_914_High - Link_914_Flow),
                       Fourth_WTP_Outdoor_Shortage = (Link_939_High - Link_939_Flow),
                       Fourth_WTP_Total_Shortage = (Fourth_WTP_Indoor_Shortage + Fourth_WTP_Outdoor_Shortage),
                       WISE_Unblended_Shortage = (Demand_141_High - Demand_141_Flow),
                       Unused_RBF_capacity_for_WISE = ((Demand_134_Low - Demand_134_Flow) * -1),
                       Unused_PWP_Pipeline_Capacity_for_WISE = (Link_995_High - Link_995_Flow)) %>%
  # select only the paratmerts to identify date, model run and to plot
  select(year, ModelRun, all_of(site_selection_short)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")



### plot the output
p <- list()
for (i in 1:n_site_selection_short){
  
  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])
  
  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p[[i]] <- ggplot(extract_plot, aes_string(x = "year", y = "Output", color = "ModelRun", linetype = "ModelRun")) + 
    geom_line() +  #col = color_list[i]
    theme_bw() +
    scale_fill_brewer(palette="Dark2") +
    ylim(0, NA) +
    ylab(extract_plot$Units[i]) +
    xlab("Water Year") +
    ggtitle(paste0(site_selection_short[i]," - ", extract_plot$Description[1])) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  
}

p[[1]]

# define the plot name
plot_title <- "4d. Aurora Annual Indoor & Outdoor Shortages by WTP"
file_name <- paste0(plot_title, " 4x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[1]], p[[2]], p[[4]], p[[5]], p[[7]], p[[8]], 
               p[[10]], p[[11]], nrow = 4,
               top = plot_title,
               right = ""))


# define the plot name
plot_title <- "4e. Aurora Total Annual Shortages by WTP"
file_name <- paste0(plot_title, " 2x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[3]], p[[6]], p[[9]], p[[12]], nrow = 2,
               top = plot_title,
               right = ""))


# define the plot name
plot_title <- "4f. Other Annual Shortages"
file_name <- paste0(plot_title, " 3x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[13]], p[[14]], p[[15]], p[[16]], p[[17]], p[[18]], nrow = 3,
               top = plot_title,
               right = ""))


# Drought Reliability Criteria --------------------------------------------


# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "DataObject_129_Flow")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add new calculated column name
site_selection_short <- c(site_selection_short, "Aurora_Drought_Stage", "Aurora_Drought_Reliability")
# remove CRAM drought stage to use only the aurora drought stage
n_site_selection_short <- length(site_selection_short)


# extract the data selected above, put in ggplot format
gt_table <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  # group by year & ModelRun
  group_by(year, ModelRun) %>%
  # get May 1 data
  filter(qm == 29) %>%
  # convert CRAM model drought metrics (0, 3, 4, 5) to Aurora drought metrics (0, 1, 2, 3)
  mutate(Aurora_Drought_Stage = if_else(DataObject_129_Flow > 0, DataObject_129_Flow-2, 0)) %>%
  select(-DataObject_129_Flow) %>%
  ungroup() %>%
  group_by(ModelRun) %>%
  # get the count of drought triggers by category (0, 1, 2, or 3 & by ModelRun)
  count(Aurora_Drought_Stage, name = "Count") %>%
  # calculate a % of drought trigger
  mutate(Percent = round(Count/(year_end - year_start + 1)*100, 1)) %>%
  gt() %>%
  tab_header(
    title = md("0d. Drought Reliability Criteria"),
    subtitle = md(paste0(scenario_name[1], " vs ", scenario_name[2]))
  )
gt_table

# alternative calculation
# nrow(extract_data[extract_data$Aurora_Drought_Stage == 0 & extract_data$ModelRun == scenario_name[2], ])  


# define the table export name & save as pdf
plot_title <- "0d. Aurora Annual Drought Reliability Triggers - May 1"
file_name <- paste0(plot_title, " table ")

gtsave(data = gt_table,
       filename = paste0(output_folder, "/", file_name, model_version, output_device),
       zoom = 0.9
)


rm(gt_table)



# May 1 drought demands (0c.) ---------------------------------------------------


# DataObject 98 - demand reduction due to drought level triggers
# DataObject 172 - reliable supply
# DataObject 173 - operating reserve
# Data object 174 = months of supply
# Data object 129 = drought trigger level
# DataObject_175_Flow - SystemStorageContents

# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "DataObject_157_Flow",
                    "DataObject_173_Flow", "DataObject_174_Flow", "DataObject_172_Flow",
                    "DataObject_129_Flow")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add new calculated column name
site_selection_short <- c(site_selection_short, "Aurora_Drought_Stage", "Aurora_Total_Annual_Demand")
# remove CRAM drought stage to use only the aurora drought stage
site_selection_short <- site_selection_short[! site_selection_short %in% c('DataObject_129_Flow')]
n_site_selection_short <- length(site_selection_short)


# get aurora annual demand from the settings output (save to "Aurora_Total_Annual_Demand")
# base demand
annual_demand1 <- filter(settings_merge, Name=="Total Aurora Annual Demand" & OutputSheet==scenario_name[1])$Value
annual_demand1 <- as.numeric(annual_demand1)
annual_demand1 <-data.frame(Aurora_Total_Annual_Demand = annual_demand1, scenario=scenario_name[1])
# comparison demand
annual_demand2 <- filter(settings_merge, Name=="Total Aurora Annual Demand" & OutputSheet==scenario_name[2])$Value
annual_demand2 <- as.numeric(annual_demand2)
annual_demand2 <-data.frame(Aurora_Total_Annual_Demand = annual_demand2, scenario=scenario_name[2])
# merge data & add units
temp.df <- rbind(annual_demand1, annual_demand2)
temp.df$Units = "kaf"



# extract the data selected above, put in ggplot format
extract_data <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  # group by year & ModelRun
  group_by(year, ModelRun) %>%
  # get May 1 data
  filter(qm == 29) %>%
  # convert CRAM model drought metrics (0, 3, 4, 5) to Aurora drought metrics (0, 1, 2, 3)
  mutate(Aurora_Drought_Stage = if_else(DataObject_129_Flow > 0, DataObject_129_Flow-2, 0)) %>%
  # match annual demand to scenario
  left_join(., temp.df, by = c("ModelRun" = "scenario")) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name") 



# set plot parameters
y_axis_lab_list <- c("Storage (af)", "Volume (af)", "Months", 
                     "Volume (af)", "Drought Trigger", "Annual Demand (kaf)")
y_axis_max_list <- c(NA, NA, NA, NA, 3, NA)


### plot the output
p <- list()
for (i in 1:n_site_selection_short){
  
  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])
  
  ## add horizontal drought tier lines for months of storage plot
  if(i == 3){
    
    # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
    p[[i]] <- ggplot(extract_plot, aes_string(x = "year", y = "Output", color = "ModelRun")) + 
      geom_line() +  #col = color_list[i]
      geom_hline(yintercept = 30, color = "gold2", linetype = "dashed") +
      geom_hline(yintercept = 24, color = "darkorange", linetype = "dashed") +
      geom_hline(yintercept = 12, color = "red2", linetype = "dashed") +
      theme_bw() +
      scale_fill_brewer(palette="Dark2") +
      #ylim(0, y_axis_max_list[i]) +
      ylab(y_axis_lab_list[i]) +
      xlab("Water Year") +
      ylim(0, y_axis_max_list[i]) +
      ggtitle(paste0(site_selection_short[i]," - ", extract_plot$Description[1])) +
      theme(plot.title = element_text(size = title_size),
            axis.title = element_text(size = xaxis_size))
    #p[[i]]
  }else{
    
    # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
    p[[i]] <- ggplot(extract_plot, aes_string(x = "year", y = "Output", color = "ModelRun", linetype = "ModelRun")) + 
      geom_line() +  #col = color_list[i]
      theme_bw() +
      scale_fill_brewer(palette="Dark2") +
      #ylim(0, y_axis_max_list[i]) +
      ylab(y_axis_lab_list[i]) +
      xlab("Water Year") +
      ylim(0, y_axis_max_list[i]) +
      ggtitle(paste0(site_selection_short[i]," - ", extract_plot$Description[1])) +
      theme(plot.title = element_text(size = title_size),
            axis.title = element_text(size = xaxis_size))
    
  }
  
  
}


p[[1]]

# define the plot name
plot_title <- "0c. Aurora Annual Drought Metrics - May 1"
file_name <- paste0(plot_title, " 3x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[1]], p[[2]], p[[6]], p[[4]], p[[3]], p[[5]], nrow = 3,
               top = plot_title, right = ""))


# save some data to the output list
g_summary[[1]] <- p[[3]]
g_summary[[2]] <- p[[5]]


rm(annual_demand1, annual_demand2, temp.df)

# WISE -----------------------------------------------------------------


### WISE
# Demand 143 - wise treated deliver (blended delivery to WISE)
# Demand 141 - unblended delivery to WISE
# Link 1184 is Denver's "Mountain water" used for blending delivered from DIA and transported by the PWP
# Link 1186 is Denver's Strontia supply of "Mountain water" used for blending
# Link 1185 is Denver's Reusable Water delivered at the RBF
# Delivery to WISE = Demands 141 + 143.


# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "Link_1184_Flow", "Link_1185_Flow", 
                    "Link_1186_Flow", "Demand_141_Flow", "Demand_143_Flow")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add calculated column name
site_selection_short <- c(site_selection_short, "DW_WISE_Water", "Aurora_WISE_Water", "Total_WISE_Deliveries")
n_site_selection_short <- length(site_selection_short)


# extract the data selected above, put in ggplot format
extract_data <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  # group by year & ModelRun
  group_by(year, ModelRun) %>%
  # sum to annual
  summarise(across(site_selection[site_start_no]:site_selection[n_site_selection], sum)) %>%
  # calculate Denver WISE
  rowwise() %>% mutate(DW_WISE_Water = sum(Link_1184_Flow, Link_1185_Flow, Link_1186_Flow)) %>%
  # Aurora's WISE portion
  rowwise() %>% mutate(Aurora_WISE_Water = sum(Demand_141_Flow, Demand_143_Flow) - sum(Link_1184_Flow, Link_1185_Flow, Link_1186_Flow)) %>%
  # calculate total WISE deliveries
  rowwise() %>% mutate(Total_WISE_Deliveries = sum(Demand_141_Flow, Demand_143_Flow)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")


## Extract the data & calculate the 10-year WISE deliveries
extract_data_10yr <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  # build a WISE year classification that begins on June 1 (qm 33)
  mutate(WISEyear = case_when(qm >= 33 ~ year,
                              qm < 33 ~ (year-1))) %>%
  # group by year & ModelRun
  group_by(WISEyear, ModelRun) %>%
  # sum to annual
  summarise(across(site_selection[site_start_no]:site_selection[n_site_selection], sum)) %>%
  # calculate Denver WISE
  rowwise() %>% mutate(DW_WISE_Water = sum(Link_1184_Flow, Link_1185_Flow, Link_1186_Flow)) %>%
  # Aurora's WISE portion
  rowwise() %>% mutate(Aurora_WISE_Water = sum(Demand_141_Flow, Demand_143_Flow) - sum(Link_1184_Flow, Link_1185_Flow, Link_1186_Flow)) %>%
  # calculate total WISE deliveries
  rowwise() %>% mutate(Total_WISE_Deliveries = sum(Demand_141_Flow, Demand_143_Flow)) %>%
  # calculate the decade
  mutate(decade = case_when(WISEyear >= 1941 & WISEyear <= 1950 ~ 1940,
                            WISEyear >= 1951 & WISEyear <= 1960 ~ 1950,
                            WISEyear >= 1961 & WISEyear <= 1970 ~ 1960,
                            WISEyear >= 1971 & WISEyear <= 1980 ~ 1970,
                            WISEyear >= 1981 & WISEyear <= 1990 ~ 1980,
                            WISEyear >= 1991 & WISEyear <= 2000 ~ 1990,
                            WISEyear >= 2001 & WISEyear <= 2010 ~ 2000,
                            WISEyear >= 2011 & WISEyear <= 2020 ~ 2010)) %>%
  # remove the first decade (<qm 33 of 1950)
  filter(decade != 1940) %>%
  # change year column to character so bar chart will plot discrete date axis
  mutate(decade = as.character(decade)) %>%
  # move the new decade column to the start of data frame (for pivot)
  relocate(decade, .after = WISEyear) %>%
  ungroup() %>%
  # group by decade & ModelRun
  group_by(decade, ModelRun) %>%
  # sum to Decade
  summarise(across(all_of(site_selection_short), sum)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name") %>%
  # convert output to kaf
  mutate(Output_kaf = Output/1000) 

y_max_temp1 <- max(filter(extract_data, Name == "Total_WISE_Deliveries")$Output)*1.1
# this is improved filter (gets max value from all created 'wise' columns)
y_max_temp1 <- max(filter(extract_data, grepl("WISE", Name))$Output)*1.1
y_max_temp2 <- max(filter(extract_data_10yr, Name == "Total_WISE_Deliveries")$Output_kaf)*1.2
# set plot parameters
y_axis_max_list1 <- c(rep(NA, 5), rep(y_max_temp1, 3))
y_axis_max_list2 <- c(rep(NA, 3), rep(y_max_temp2, 5))



### plot the output
p <- list()
g <- list()
for (i in 1:n_site_selection_short){
  
  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])
  
  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p[[i]] <- ggplot(extract_plot, aes_string(x = "year", y = "Output", color = "ModelRun", linetype = "ModelRun")) + 
    geom_line() +  #col = color_list[i]
    theme_bw() +
    scale_fill_brewer(palette="Dark2") +
    scale_y_continuous(labels = scales::label_comma(),
                       limits = c(0, y_axis_max_list1[i])) +
    ylab(extract_plot$Units[i]) +
    xlab("Water Year") +
    ggtitle(paste0(site_selection_short[i]," - ", extract_plot$Description[1])) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  
  
  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data_10yr %>%
    filter(Name == site_selection_short[i])
  
  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  g[[i]] <- ggplot(extract_plot, aes_string(x = "decade", y = "Output_kaf", fill = "ModelRun")) + 
    geom_col(position = "dodge", col = "black") +  
    geom_text(aes(label= round(Output_kaf,1)), vjust = 2,
              position = position_dodge(0.9), size = 1.9)+
    theme_bw() +
    #scale_fill_brewer(palette="Dark2") +
    ylab("Flow (kaf)") +
    scale_y_continuous(labels = scales::label_comma(),
                       limits = c(0, y_axis_max_list2[i])) +
    xlab("Water Year") +
    ggtitle(paste0(site_selection_short[i]," - ", extract_plot$Description[1])) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  
  
}

#p[[1]]

# ggplot(extract_plot, aes_string(x = "decade", y = "Output", fill = "ModelRun")) + 
#   geom_col(position = "dodge", col = "black") +theme_bw() +
#   geom_text(aes(label= round(Output/1000,1)), vjust = 1.6,
#             position = position_dodge(0.9), size=3.5)+
#   # vjust=1.6, color="white"
#   #scale_fill_brewer(palette="Dark2") +
#   #xlim(1945, 2005) +
#   #scale_x_discrete(limits = c("1950","1960", "1970", "1980", "1990", "2000", "2010")) +
#   ylab(extract_plot$Units[i]) +
#   scale_y_continuous(labels = scales::label_comma(),
#                      limits = c(0, y_axis_max_list[i])) +
#   xlab("Water Year") +
#   ggtitle(paste0(site_selection_short[i]," - ", extract_plot$Description[1])) +
#   theme(plot.title = element_text(size = title_size),
#         axis.title = element_text(size = xaxis_size))


# define the plot name
plot_title <- "5a. WISE Annual Output"
file_name <- paste0(plot_title, " 4x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[8]], p[[7]], p[[6]], p[[5]], p[[4]], 
               p[[3]], p[[2]], p[[1]], nrow = 4,
               top = plot_title, right = ""))


# define the plot name
plot_title <- "5b. WISE Decadal Output"
file_name <- paste0(plot_title, " 3x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(g[[8]], g[[7]], g[[6]], g[[5]], g[[4]], nrow = 3,
               top = plot_title, right = ""))


# save some data to the output list
g_WISE_summary[[1]] <- g[[8]]
g_WISE_summary[[2]] <- g[[7]]


# remove some data 
rm(extract_data_10yr, g, y_max_temp1, y_max_temp2, y_axis_max_list1, y_axis_max_list2)


# Total Reservoir Storage --------------------------------------------------

# get data for May 8 (QM 29, which is a proxy for pre-time step QM 29 (may 1))

# Reservoir 15 - spinney (x)
# Res 22 - Aurora Res (x)
# Res 21 - Quincy (x)
# Res 20 - Rampart (x)
# Res 25 - Strontia (x)
# Res 19 - Jefferson Lake (x)

# Res 2 = Homestake (aurora) (x)
# DataObject 1 - Aurora's Homestake in Turquoise Res (x)
# DataObject 2 - Aurora's CF&I acct in Turquoise Res (x)
# DataObject 13 - Aurora's Pueblo Water
# DataObject 7 - Auroras Henry & Meredith
# Link 23 - Aurora - Turquoise total storage
# DataObject 9 - Auroras Twin Lakes Account (y)
# Res 9 = Box Creek
# Res 13 = Wild Horse Res 

# DataObject_157_Flow - Aurora total system res contents

# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "Reservoir_15_Content", 
                    "Reservoir_19_Content", "Reservoir_20_Content", "Reservoir_21_Content",
                    "Reservoir_22_Content", "Reservoir_25_Content", "Reservoir_13_Content",
                    "Reservoir_2_Content", "Link_23_Flow", "Link_555_Flow", "DataObject_13_Flow", 
                    "DataObject_7_Flow", "DataObject_9_Flow", "DataObject_157_Flow", 
                    "Reservoir_35_Content", "Reservoir_24_Content", "Reservoir_31_Content", 
                    "Reservoir_36_Content", "Reservoir_38_Content", "Reservoir_40_Content")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add new calculated column name
site_selection_short <- c("Aurora_South_Platte_Storage","Aurora_Colorado_Storage","Aurora_Arkansas_Storage",
                          "Aurora_LSP_Storage", "Aurora_Total_Storage")
n_site_selection_short <- length(site_selection_short)



# extract the data selected above, put in ggplot format
extract_data <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  # filter for May 1 reservoir contents
  filter(qm == 29) %>%
  # group
  group_by(year, ModelRun) %>%
  # Sum South Platte Res storage
  rowwise() %>% mutate(Aurora_South_Platte_Storage = sum(Reservoir_15_Content, Reservoir_19_Content, 
                                                         Reservoir_20_Content, Reservoir_21_Content,
                                                         Reservoir_22_Content, Reservoir_25_Content,
                                                         Reservoir_13_Content)) %>%
  # Sum Colorado Basin Res storage
  rowwise() %>% mutate(Aurora_Colorado_Storage = sum(Reservoir_2_Content, Link_23_Flow, 
                                                     DataObject_9_Flow, Link_555_Flow)) %>%
  # Sum Arkansas Basin Res storage
  rowwise() %>% mutate(Aurora_Arkansas_Storage = sum(DataObject_13_Flow, DataObject_7_Flow)) %>%
  # Sum Ark/Colo Res storage
  rowwise() %>% mutate(Aurora_Ark_Col_Storage = sum(Reservoir_2_Content, Link_23_Flow, 
                                                    DataObject_9_Flow, Link_555_Flow,
                                                    DataObject_13_Flow, DataObject_7_Flow)) %>%
  # Sum Lower South Platte Res storage
  rowwise() %>% mutate(Aurora_LSP_Storage = sum(Reservoir_35_Content, Reservoir_24_Content, 
                                                Reservoir_31_Content, Reservoir_36_Content,
                                                Reservoir_38_Content, Reservoir_40_Content)) %>%
  # Sum Ark/Colo Res storage
  rowwise() %>% mutate(Aurora_Total_Storage = sum(Aurora_South_Platte_Storage, Aurora_Ark_Col_Storage)) %>%
  # reduce the number of output in data frame
  select(year, ModelRun, Aurora_South_Platte_Storage, Aurora_Colorado_Storage, 
         Aurora_Arkansas_Storage, Aurora_LSP_Storage, Aurora_Total_Storage) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name") 


# get the max yaxis value
y_max_temp <- max(filter(extract_data, Name == "Aurora_Total_Storage")$Output)*1.05
# set plot parameters
y_axis_max_list <- c(rep(y_max_temp, n_site_selection_short))



### plot the output
p <- list()
for (i in 1:n_site_selection_short){
  
  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])
  
  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p[[i]] <- ggplot(extract_plot, aes_string(x = "year", y = "Output", color = "ModelRun",
                                            linetype = "ModelRun")) +
    geom_line() +  #col = color_list[i]
    theme_bw() +
    ylab("Storage (kaf)") +
    xlab("Water Year") +
    scale_y_continuous(labels = scales::label_comma(),
                       limits = c(0, y_axis_max_list[i])) +
    ggtitle(paste0(extract_plot$Name[1], ": ", extract_plot$Description[1])) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  
}


# define the plot name
plot_title <- "0e. May 8 (QM 29) Annual Total Aurora Reservoir Contents"
file_name <- paste0(plot_title, " 3x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], nrow = 3,
               top = plot_title, right = ""))


# save some data to the output list
g_summary[[3]] <- p[[5]]


# ASR EOY Contents --------------------------------------------------------


# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "Decree_76_Content", "Reservoir_34_Content")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# # add new calculated column name
# site_selection_short <- c(site_selection_short, "Total_WTP_Capacity")
# n_site_selection_short <- length(site_selection_short)


# extract the data selected above, put in ggplot format
extract_data <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  # group by year & ModelRun
  group_by(year, ModelRun) %>%
  # get end of water year data
  filter(qm == 48) %>%
  # # sum to annual by existing groups
  # summarise(across(site_selection[site_start_no]:site_selection[n_site_selection], sum)) %>%
  # calculate total WTP capacity
  # rowwise() %>% mutate(Total_WTP_Capacity = sum(Link_826_Flow, Link_823_Flow,
  #                                               Link_1136_Flow)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")


### plot the output
g <- list()
for (i in 1:n_site_selection_short){
  
  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])
  
  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  g[[i]] <- ggplot(extract_plot, aes_string(x = "year", y = "Output", color = "ModelRun", linetype = "ModelRun")) + 
    geom_line() +  #col = color_list[i]
    theme_bw() +
    scale_fill_brewer(palette="Dark2") +
    # scale_y_continuous(labels = scales::label_comma(),
    #                    limits = c(0, y_axis_max_list[i])) +
    ylab(extract_plot$Units[i]) +
    xlab("Water Year") +
    ggtitle(paste0(site_selection_short[i]," - ", extract_plot$Description[1])) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  
}

g[[1]]
g[[2]]

# # define the plot name
# plot_title <- "3b. Aurora ASR 1"
# file_name <- paste0(plot_title, " 3x2 ")
# 
# # save the plot
# ggsave(
#   paste0(output_folder, "/", file_name, model_version, output_device),
#   width = 14, height = 8,
#   grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], nrow = 3,
#                top = plot_title,
#                right = ""))


# ASR Results (7a 7b) -------------------------------------------------------------

# Denver Aquifer Mining: SPOS - Decree_76_Flow
# Denver ASR contents (injected): SPOS - Reservoir_34_Content
# Denver ASR Griswold Injection: SPOS - Link_1233_Flow
# Denver ASR Wemlinger Injeciton: SPOS - Link_1235_Flow
# Denver ASR Binney Injection: SPOS - Link_1237_Flow
# Denver ASR Fourth WTP Injection: PWPOS - Link_1301_Flow

# Denver ASR Griswold Extraction: Link_1239_Flow
# Denver ASR Wemlinger Extraction: Link_1240_Flow
# Denver ASR Binney Extraction: Link_1241_Flow
# Denver ASR Fourth WTP Extraction: PWPPOS - Link_1303_Flow
# Effluent Commitments Shortage: PWPOS - Demand_114_Shortage


# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", 
                    "Link_1233_Flow", "Link_1235_Flow", "Link_1237_Flow", "Link_1301_Flow",
                    "Link_1239_Flow", "Link_1240_Flow","Link_1241_Flow", "Link_1303_Flow", 
                    "Decree_76_Flow")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add new calculated column name
site_selection_short <- c(site_selection_short, "Aurora_Total_ASR_Injection", "Aurora_Total_ASR_Extraction")
n_site_selection_short <- length(site_selection_short)


# extract the data selected above, put in ggplot format
extract_data <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  # group by year & ModelRun
  group_by(year, ModelRun) %>%
  # sum to annual by existing groups
  summarise(across(site_selection[site_start_no]:site_selection[n_site_selection], sum)) %>%
  # calculate total Aurora ASR injection/extraction
  rowwise() %>% mutate(Aurora_Total_ASR_Injection = sum(Link_1233_Flow, Link_1235_Flow,
                                                        Link_1237_Flow, Link_1301_Flow),
                       Aurora_Total_ASR_Extraction = sum(Link_1239_Flow, Link_1240_Flow,
                                                         Link_1241_Flow, Link_1303_Flow)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")


### plot the output
p <- list()
for (i in 1:n_site_selection_short){
  
  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])
  
  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p[[i]] <- ggplot(extract_plot, aes_string(x = "year", y = "Output", color = "ModelRun", linetype = "ModelRun")) + 
    geom_line() +  #col = color_list[i]
    theme_bw() +
    scale_fill_brewer(palette="Dark2") +
    # scale_y_continuous(labels = scales::label_comma(),
    #                    limits = c(0, y_axis_max_list[i])) +
    ylab(extract_plot$Units[i]) +
    xlab("Water Year") +
    ggtitle(paste0(site_selection_short[i]," - ", extract_plot$Description[1])) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  
}

p[[1]]

# define the plot name
plot_title <- "7b. Aurora Annual ASR Injection & Extraction"
file_name <- paste0(plot_title, " 4x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], p[[7]], p[[8]], nrow = 4,
               top = plot_title,
               right = ""))

# define the plot name
plot_title <- "7a. Aurora Annual ASR"
file_name <- paste0(plot_title, " 3x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[10]], p[[11]], p[[9]], 
               g[[1]], g[[2]], nrow = 3,
               top = plot_title,
               right = ""))

rm(g)


# Lower South Platte (min) Apr 30 (QM 28) Reservoir Contents -------------------------------------------------------------


# Max contents are QMs 36-39 (use 37)
# Min contents are QMs 27-29 (use 28)

# Reservoir 35 - Gilcrest
# Res 24 - Walker Storage
# Res 31 - Everist Storage
# Reservoir 36 - Middle South Platte Reservoir
# Res 38 - St Vrain Aurora Storage
# Res 40 - Whitney Ditch (CLP) Aurora Storage


# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "Reservoir_35_Content", "Reservoir_24_Content",
                    "Reservoir_31_Content", "Reservoir_36_Content", "Reservoir_38_Content",
                    "Reservoir_40_Content")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add new calculated column name
site_selection_short <- c(site_selection_short, "Aurora_LSP_Storage")
n_site_selection_short <- length(site_selection_short)


# extract the data selected above, put in ggplot format
extract_data <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  # filter for May 1 reservoir contents
  filter(qm == 28) %>%
  # group
  group_by(year, ModelRun) %>%
  # Sum South Platte Res storage
  rowwise() %>% mutate(Aurora_LSP_Storage = sum(Reservoir_35_Content, Reservoir_24_Content, 
                                                         Reservoir_31_Content, Reservoir_36_Content,
                                                         Reservoir_38_Content, Reservoir_40_Content)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")



# set plot parameters
y_axis_max_list <- c(12000, 12000, 12000, 12000, 4000, 2000, NA)


### plot the output
p <- list()
for (i in 1:n_site_selection_short){
  
  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])
  
  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p[[i]] <- ggplot(extract_plot, aes_string(x = "Date", y = "Output", color = "ModelRun",
                                            linetype = "ModelRun")) +
    geom_line() +  #col = color_list[i]
    theme_bw() +
    ylab(extract_plot$Units[i]) +
    xlab("Water Year") +
    ylim(0, y_axis_max_list[i]) +
    # scale_y_continuous(labels = scales::label_comma(),
    #                    limits = c(0, y_axis_max_list[i])) +
    ggtitle(paste0(extract_plot$Name[1], " - ", extract_plot$Description[1])) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  #p[[i]]
  
}



# define the plot name
plot_title <- "3a. April 30 (~min) Annual Lower South Platte Reservoir Contents"
file_name <- paste0(plot_title, " 4x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], p[[7]], nrow = 4,
               top = plot_title,
               right = ""))


# Lower South Platte (max) Aug 24 (QM 43) Reservoir Contents -------------------------------------------------------------


# Max contents are QMs 36-39 (use 37)
# Min contents are QMs 27-29 (use 28)

# Reservoir 35 - Gilcrest
# Res 24 - Walker Storage
# Res 31 - Everist Storage
# Reservoir 36 - Middle South Platte Reservoir
# Res 38 - St Vrain Aurora Storage
# Res 40 - Whitney Ditch (CLP) Aurora Storage


# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "Reservoir_35_Content", "Reservoir_24_Content",
                    "Reservoir_31_Content", "Reservoir_36_Content", "Reservoir_38_Content",
                    "Reservoir_40_Content")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add new calculated column name
site_selection_short <- c(site_selection_short, "Aurora_LSP_Storage")
n_site_selection_short <- length(site_selection_short)


# extract the data selected above, put in ggplot format
extract_data <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  # filter for May 1 reservoir contents
  filter(qm == 43) %>%
  # group
  group_by(year, ModelRun) %>%
  # Sum South Platte Res storage
  rowwise() %>% mutate(Aurora_LSP_Storage = sum(Reservoir_35_Content, Reservoir_24_Content, 
                                                Reservoir_31_Content, Reservoir_36_Content,
                                                Reservoir_38_Content, Reservoir_40_Content)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")


# set plot parameters
y_axis_max_list <- c(12000, 12000, 12000, 12000, 4000, 2000, NA)


### plot the output
p <- list()
for (i in 1:n_site_selection_short){
  
  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])
  
  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p[[i]] <- ggplot(extract_plot, aes_string(x = "Date", y = "Output", color = "ModelRun",
                                            linetype = "ModelRun")) +
    geom_line() +  #col = color_list[i]
    theme_bw() +
    ylab(extract_plot$Units[i]) +
    xlab("Water Year") +
    ylim(0, y_axis_max_list[i]) +
    # scale_y_continuous(labels = scales::label_comma(),
    #                    limits = c(0, y_axis_max_list[i])) +
    ggtitle(paste0(extract_plot$Name[1], " - ", extract_plot$Description[1])) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  #p[[i]]
  
}



# define the plot name
plot_title <- "3b. Aug 24 (~max) Annual Lower South Platte Reservoir Contents"
file_name <- paste0(plot_title, " 4x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], p[[7]], nrow = 4,
               top = plot_title,
               right = ""))

# Lower South Platte Average QM Reservoir Contents -------------------------------------------------------------


# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "Reservoir_35_Content", "Reservoir_24_Content",
                    "Reservoir_31_Content", "Reservoir_36_Content", "Reservoir_38_Content",
                    "Reservoir_40_Content")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add new calculated column name
site_selection_short <- c(site_selection_short, "Aurora_LSP_Storage")
n_site_selection_short <- length(site_selection_short)


# extract the data selected above, put in ggplot format
extract_data <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  # group
  group_by(qm, ModelRun) %>%
  # filter for May 1 reservoir contents
  summarise(across(site_selection[site_start_no]:site_selection[n_site_selection], mean)) %>%
  # Sum South Platte Res storage
  rowwise() %>% mutate(Aurora_LSP_Storage = sum(Reservoir_35_Content, Reservoir_24_Content, 
                                                Reservoir_31_Content, Reservoir_36_Content,
                                                Reservoir_38_Content, Reservoir_40_Content)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")


# set plot parameters
y_axis_max_list <- c(12000, 12000, 12000, 12000, 4000, 2000, NA)


### plot the output
p <- list()
for (i in 1:n_site_selection_short){
  
  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])
  
  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p[[i]] <- ggplot(extract_plot, aes_string(x = "qm", y = "Output", color = "ModelRun",
                                            linetype = "ModelRun")) +
    geom_line() +  #col = color_list[i]
    theme_bw() +
    ylab(extract_plot$Units[i]) +
    xlab("Water Year") +
    ylim(0, y_axis_max_list[i]) +
    #scale_y_continuous(labels = scales::label_comma(), limits = c(0, y_axis_max_list[i])) +
    ggtitle(paste0(extract_plot$Name[1], " - ", extract_plot$Description[1])) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  
}


# define the plot name
plot_title <- "3c. Average QM Lower South Platte Reservoir Contents"
file_name <- paste0(plot_title, " 4x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], p[[7]], nrow = 4,
               top = plot_title,
               right = ""))



# PWP Output --------------------------------------------------------------

# Link_1275 - Box Elder to PWP pipeline
# Link_1207 - Lost Creek to ARR PWP

# Link_917 - Prairie Waters Project Pipeline Capacity Pipe 1
# link_1064 - PWP Pipeline directly to Treatment
# Link_995 - A. R. R. system release to pipeline.
# Link_1182 - PWP Treatment Train
# Link 1134 - Prarie Waters Project to BINNEY WTP
# Link_960 - PWP to Wemlinger WTP
# Link_1015 - PWP to Griswold
# Link_913 - PWP to Fourth WTP
# Link_916 - PWP Pipeline to East Reservoir
# Demand_112 - PWP Water to Others low priority
# DataObject_127 - PWP trigger levels

# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "Link_1275_Flow", "Link_1207_Flow",
                    "Link_995_Flow", "Link_995_High",
                    "Link_917_Flow", "Link_917_High", "Link_1182_Flow", "Link_1064_Flow", 
                    "Link_1134_Flow", "Link_960_Flow", "Link_1015_Flow", "Link_913_Flow",
                    "Link_916_Flow", "Demand_112_Flow", "DataObject_127_Flow", 
                    "Demand_141_Flow", "Demand_143_Flow", "Link_1184_Flow", "Link_1185_Flow", "Link_1186_Flow")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)
# add new calculated column name
site_selection_short <- c(site_selection_short, "Aurora_Untreated_WISE", "Aurora_Treated_WISE", 
                          "Aurora_PWP_to_Treatment", "PWP_Mass_Balance")
# remove variables only used to calculate variables
site_selection_short <- site_selection_short[! site_selection_short %in% c("Demand_141_Flow", "Demand_143_Flow",
                                                                           "Link_1184_Flow", "Link_1186_Flow")]
n_site_selection_short <- length(site_selection_short)



# extract the data selected above, put in ggplot format
extract_data <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  # group by year & ModelRun
  group_by(year, ModelRun) %>%
  # sum to annual by existing groups
  summarise(across(site_selection[site_start_no]:site_selection[n_site_selection], sum)) %>%
  # calculate the Aurora WISE untreated water
  rowwise() %>% mutate(Aurora_Untreated_WISE = sum(Demand_141_Flow) - sum(Link_1184_Flow, Link_1185_Flow, Link_1186_Flow)) %>%
  # calculate the Aurora WISE Treated water
  rowwise() %>% mutate(Aurora_Treated_WISE = max(sum(Demand_143_Flow) - sum(Link_1184_Flow, Link_1185_Flow, Link_1186_Flow),0)) %>%
  # calculate the Aurora PWP Water to Aurora WTPs
  rowwise() %>% mutate(Aurora_PWP_to_Treatment = sum(Link_1134_Flow, Link_960_Flow, Link_1015_Flow, Link_913_Flow)) %>%
  # calculate the total PWP Water Balance (aurora WISE, aurora PWP to treatment + DW transported water) 
  rowwise() %>% mutate(PWP_Mass_Balance = sum(Aurora_Untreated_WISE, Aurora_Treated_WISE, Aurora_PWP_to_Treatment, Link_1185_Flow)) %>%
  # since we sum the PWP trigger, divide by # of quarter months to get the state (1-5)
  mutate(DataObject_127_Flow = DataObject_127_Flow/48) %>%
  # remove columns only used for calculating other columns
  select(-Demand_141_Flow, -Demand_143_Flow, -Link_1184_Flow, -Link_1186_Flow) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")

# # calculate max values to set the y-axis dynamically
# y_max_temp <- max(filter(extract_data, Name == "Demand_71_Flow")$Output)*1.1
# y_max_temp2 <- max(filter(extract_data, Name=="Link_1137_Flow"| Name =="Link_1138_Flow" | 
#                             Name =="Link_780_Flow" | Name =="Link_937_Flow" | 
#                             Name =="Link_781_Flow" | Name =="Link_938_Flow")$Output)*1.1
# y_max_temp3 <- max(filter(extract_data, Name == "Total_WTP_Capacity")$Output)*1.1
# y_axis_max_list <- c(rep(y_max_temp2, 6), y_max_temp, rep(NA, 7), y_max_temp,
#                      y_max_temp, y_max_temp, rep(y_max_temp, 4), y_max_temp3)
y_max_temp1 <- max(filter(extract_data, Name == "Link_995_High")$Output)*1.05
y_max_temp2 <- max(filter(extract_data, Name == "Link_917_High")$Output)*1.05
y_max_temp3 <- max(filter(extract_data, Name=="Link_1134_Flow"| Name =="Link_960_Flow" |
                            Name =="Link_1015_Flow" | Name =="Link_913_Flow" |
                            Name =="Link_916_Flow" | Name =="Demand_112_Flow")$Output)*1.1
y_max_temp4 <- max(filter(extract_data, Name == "Link_1182_Flow" |  Name =="Link_1064_Flow")$Output)*1.05
# set plot parameters
y_axis_max_list1 <- c(NA, NA, rep(y_max_temp1, 2), rep(y_max_temp2, 2), NA, NA,
                      rep(y_max_temp3, 6), NA, rep(y_max_temp4, 5))



### plot the output
p <- list()
for (i in 1:n_site_selection_short){
  
  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])
  
  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p[[i]] <- ggplot(extract_plot, aes_string(x = "year", y = "Output", color = "ModelRun", linetype = "ModelRun")) + 
    geom_line() +  #col = color_list[i]
    theme_bw() +
    scale_fill_brewer(palette="Dark2") +
    ylim(0, y_axis_max_list1[i]) +
    # scale_y_continuous(labels = scales::label_comma(),
    #                    limits = c(0, y_axis_max_list[i])) +
    ylab(extract_plot$Units[i]) +
    xlab("Water Year") +
    ggtitle(paste0(site_selection_short[i]," - ", extract_plot$Description[1])) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  
}

p[[1]]

# define the plot name
plot_title <- "6a. PWP Annual Output"
file_name <- paste0(plot_title, " 4x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]], p[[7]], p[[8]], nrow = 4,
               top = plot_title,
               right = ""))


# define the plot name
plot_title <- "6b. PWP Annual Output by WTP"
file_name <- paste0(plot_title, " 4x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[9]], p[[10]], p[[11]], p[[12]], p[[13]], p[[14]], p[[15]], nrow = 4,
               top = plot_title,
               right = ""))


# define the plot name
plot_title <- "6c. PWP Annual Mass Balance"
file_name <- paste0(plot_title, " 4x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[5]], p[[7]], p[[8]], p[[16]], p[[17]], p[[18]], p[[19]], p[[20]], nrow = 4,
               top = plot_title,
               right = ""))

rm(y_max_temp1, y_max_temp2, y_max_temp3, y_max_temp4, y_axis_max_list1)


# Water Qualiy Plots (8.) ----------------------------------------------------

# Binney Water Quality: DataObject_153_Flow
# Wemlinger Water Quality: DataObject_122_Flow
# Griswold Water Quality: DataObject_128_Flow
# Fourth WTP Water Quality: DataObject_176_Flow

# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "DataObject_153_Flow",
                    "DataObject_122_Flow", "DataObject_128_Flow", "DataObject_176_Flow",
                    "DataObject_94_Flow", "DataObject_95_Flow")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)


# extract the data selected above, put in ggplot format
extract_data <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  # group by year & ModelRun
  group_by(year, ModelRun) %>%
  # sum to annual by existing groups
  summarise(across(site_selection[site_start_no]:site_selection[n_site_selection], mean)) %>%
  # calculate total WTP capacity
  # rowwise() %>% mutate(Total_WTP_Capacity = sum(Link_826_Flow, Link_823_Flow,
  #                                               Link_1136_Flow)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")


### plot the output
p <- list()
for (i in 1:n_site_selection_short){

  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])
  
  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p[[i]] <- ggplot(extract_plot, aes_string(x = "year", y = "Output", color = "ModelRun", linetype = "ModelRun")) +
    geom_line() +  #col = color_list[i]
    theme_bw() +
    scale_fill_brewer(palette="Dark2") +
    ylim(0, NA) +
    ylab("TDS (mg/L)") +
    xlab("Water Year") +
    ggtitle(paste0(site_selection_short[i]," - ", extract_plot$Description[1])) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  
}

#p[[1]]

# define the plot name
plot_title <- "8a. Aurora Average Annual Water Quality"
file_name <- paste0(plot_title, " 2x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], nrow = 2,
               top = plot_title,
               right = ""))



# Reusable Flow (8b) -----------------------------------------------------------

# DataObject_94_Flow is Indoor Reusable Return Flow
# DataObject_95_Flow is Total Outdoor Reusable Return Flow


# select the sites you want to plot
site_selection <- c("year", "qm", "Date", "ModelRun", "DataObject_94_Flow", "DataObject_95_Flow", 
                    "DataObject_83_Flow", "DataObject_84_Flow", "DataObject_85_Flow", "DataObject_86_Flow")
n_site_selection <- length(site_selection)
site_start_no <- 5
site_selection_short <- site_selection[site_start_no:n_site_selection]
n_site_selection_short <- length(site_selection_short)


# extract the data selected above, put in ggplot format
extract_data <- data_list_merge2 %>%
  # select the columns of interest from the vector above using !!!syms to read it properly
  select(!!!syms(site_selection)) %>%
  # group by year & ModelRun
  group_by(year, ModelRun) %>%
  # sum to annual by existing groups
  summarise(across(site_selection[site_start_no]:site_selection[n_site_selection], sum)) %>%
  # convert from 'wide' to 'long' format for plotting w/ ggplot
  pivot_longer(., cols = all_of(site_selection_short),
               names_to = "Name", values_to = "Output") %>%
  #join the CRAM model descriptions to this dataset
  left_join(., definitions, by = "Name")


### plot the output
p <- list()
for (i in 1:n_site_selection_short){
  
  # select 1 flow data to plot per i iteration of loop
  extract_plot <- extract_data %>%
    filter(Name == site_selection_short[i])
  
  # use 'aes_string' instead of the normal aes to read the site name column headers as strings!
  p[[i]] <- ggplot(extract_plot, aes_string(x = "year", y = "Output", color = "ModelRun", linetype = "ModelRun")) +
    geom_line() +  #col = color_list[i]
    theme_bw() +
    scale_fill_brewer(palette="Dark2") +
    ylim(0, NA) +
    ylab(extract_plot$Units[i]) +
    xlab("Water Year") +
    ggtitle(paste0(site_selection_short[i]," - ", extract_plot$Description[1])) +
    theme(plot.title = element_text(size = title_size),
          axis.title = element_text(size = xaxis_size))
  
}

p[[1]]

# define the plot name
plot_title <- "8b. Aurora Annual Water Type"
file_name <- paste0(plot_title, " 3x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(p[[3]], p[[4]], p[[5]], p[[6]], p[[1]], p[[2]], nrow = 3,
               top = plot_title,
               right = ""))


# Aurora Summary Plots ----------------------------------------------------

## Master Aurora Dashboard plot
# Data has been saved in g_xxx_summary index objects
# g_summary
# g_sp_summary
# g_demands_summary
# g_WISE_summary
# g_ark_col_summary

# define the plot name
plot_title <- "0b. Aurora Annual May 1 Summary Dashboard"
file_name <- paste0(plot_title, " 4x2 ")

# save the plot
ggsave(
  paste0(output_folder, "/", file_name, model_version, output_device),
  width = 14, height = 8,
  grid.arrange(g_summary[[3]], g_sp_summary[[1]],
               g_demands_summary[[1]], g_demands_summary[[2]], 
               g_summary[[1]], g_summary[[2]],
               g_WISE_summary[[1]], g_WISE_summary[[2]],
               nrow = 4,
               top = plot_title, right = ""))

