# funs_preprocess_plate_data_new.R
# vincent de boer
# December 29th, 2021
# derived from first funs_preprocess_plate_data.R function (all read scripts were moved to funs_read_plate_data.R)

library(tidyverse)
library(readxl)

# MAIN FUNCTION preprocess_plate_data ------------------------------------

#' Preprocessing the plate data that was read in funs_read_plate_data.R 
#' 
#' @description 
#' The preprocess_plate_data function preprocesses the data by:
#'   * changing columns names
#'   * adding new time columns
#'   * adding injection info columns
#'   * add plate_id column
#'   * calculating background data
#'   * calculating raw pH emission data
#'   
#' @param plate_df A list of all data read from the original excel wace exported file
#' @return XFe96data_tibble : a list with the new dataframe and the assay_info. A new dataframe called XFe96data_tibble is returned. 
#' The preprocessed dataframe has the following columns:
#'   * plate_id
#'   * well
#'   * measurement
#'   * tick
#'   * timescale
#'   * minutes
#'   * group
#'   * interval
#'   * injection
#'   * O2_em_corr
#'   * pH_em_corr
#'   * O2_mmHg
#'   * pH
#'   * pH_em_corr_corr
#'   * backgrounds for O2_em_corr, pH_em_corr, O2_mmHg, pH, pH_em_corr_corr
#' 
#' The assay_info has the following information:
#'   * ....
#'   
#' @examples
#' preprocess_plate_data(plate_df)
#' 

preprocess_plate_data_2 <- function(plate_df) {
  XFe96data <- plate_df[[1]] # raw data
  assay_info <- plate_df[[2]] # assay info
  injection_info <- plate_df[[3]] # injection info
  pH_calibration <- plate_df[[4]] # pH calibration data
  norm_info <- plate_df[[5]]
  bufferfactor <- plate_df[[6]]
  OCR_from_excel <- plate_df[[7]]
  filePathSeahorse <- plate_df[[8]]
  # flagged_wells <- plate_df[[9]]
  
  # rename columns
  XFe96data <- rename_columns(XFe96data)
  
  # make new time columns
  XFe96data <- convert_timestamp(XFe96data)
  
  # add injection info
  XFe96data <- left_join(XFe96data, injection_info, by = "measurement")
  
  # pH preprocessing
  pH_calibration$well <- as.factor(pH_calibration$well)
  XFe96data <- left_join(XFe96data, pH_calibration, by = "well")
  
  # calculate the pH emission corrected for calibration not reaching target emission
  pH_targetEmission <- assay_info$pH_targetEmission[[1]]
  XFe96data$pH_em_corr_corr <- 
    (pH_targetEmission / XFe96data$pH_cal_em) * XFe96data$pH_em_corr
  
  # calculate backgrounds and join
  background <- XFe96data %>%
    select(group, well, measurement, timescale, O2_em_corr, 
           pH_em_corr, O2_mmHg, pH, pH_em_corr_corr) %>%
    filter(group == "Background") %>%
    group_by(measurement, timescale) %>%
    summarize(
      O2_em_corr_bkg = mean(O2_em_corr),
      pH_em_corr_bkg = mean(pH_em_corr),
      O2_mmHg_bkg = mean(O2_mmHg),
      pH_bkgd = mean(pH),
      pH_em_corr_corr_bkg = mean(pH_em_corr_corr)
    )
  XFe96data <- XFe96data %>% left_join(background, by = c("measurement", "timescale"))
  
  #add plate_id to df
  XFe96data$plate_id <- assay_info$plate_id
  
  #add norm_info
  XFe96data <- XFe96data %>% left_join(norm_info, by = c("well"))
  
  #add bufferfactor
  XFe96data <- XFe96data %>% left_join(bufferfactor, by = c("well"))
  
  #add norm_info to rate data
  OCR_from_excel <- OCR_from_excel %>% left_join(norm_info, by = c("well"))
  
  #add flag well columnn
  # XFe96data$flagged_well <- FALSE
  # XFe96data$flagged_well[XFe96data$well %in% flagged_wells] <- TRUE
  
  # OCR_from_excel$flagged_well <- FALSE
  # OCR_from_excel$flagged_well[OCR_from_excel$well %in% flagged_wells] <- TRUE
  
  # select columns that are needed
  XFe96data <- XFe96data %>% select(
    plate_id, well, measurement, tick, timescale, minutes, group, interval, injection,
    O2_em_corr, pH_em_corr, O2_mmHg, pH, pH_em_corr_corr, O2_em_corr_bkg,
    pH_em_corr_bkg, O2_mmHg_bkg, pH_bkgd, pH_em_corr_corr_bkg, bufferfactor, cell_n # flagged_well
  )
  
  XFe96data_tibble <- XFe96data %>% 
    group_by(plate_id) %>% 
    nest() %>% 
    mutate(filePathSeahorse = filePathSeahorse,
           date = assay_info$date_run,
           assay_info = list(tibble(assay_info)),
           OCR_from_excel = list(tibble(OCR_from_excel)),
           injection_info = list(tibble(injection_info))) %>% 
    select(plate_id, filePathSeahorse, date, assay_info, injection_info, raw_data = data, rate_data = OCR_from_excel)
  
  assay_info <- XFe96data_tibble[[4]]
  assay_info <- assay_info[[1]]
  
  return(XFe96data_tibble)
}

# DEPENDENT FUNCTIONS ---------------------------------------------------------------

## rename_columns() --------------------------------------------------------

#' Rename the columns of the Raw data sheet 
#' 
#' @description
#' This function is called in the preprocessing function and renames the columns of the dataframe that was
#'   read in by read_excel from the WAVE excel output file, excel sheet "Raw". 
#'
#' @param plate_df A dataframe that was read by read_excel from the WAVE excel output file (sheet "Raw")
#' @return A new dataframe with adjusted and selected column names. The selected columns with new names are:
#'   ("measurement","tick", "well", "group", "time", "O2_em_corr","pH_em_corr", "O2_mmHg", "pH")
#' @examples
#' rename_columns(XFe96data)
rename_columns <- function(plate_df) {
  
  # change column names into terms without spaces
  colnames(plate_df) <- c(
    "measurement", "tick", "well", "group",
    "time", "temp_well", "temp_env", "O2_isvalid", "O2_mmHg",
    "O2_light", "O2_dark", "O2ref_light", "O2ref_dark",
    "O2_em_corr", "pH_isvalid", "pH", "pH_light", "pH_dark",
    "pHref_light",
    "pHref_dark", "pH_em_corr"
  )
  
  plate_df <- plate_df %>%
    select(
      "measurement", "tick", "well",
      "group", "time", "O2_em_corr",
      "pH_em_corr", "O2_mmHg", "pH"
    )
  
  
  return(plate_df)
} # called in preprocessing


## convert_timestamp() -----------------------------------------------------

#' Convert the the time column in the WAVE input dataframe to a time scale in seconds
#'
#' @param plate_df A dataframe generated in adjust_columns
#' @return A new dataframe with new columns added  to \code{plate_df}. New columns 
#'  are: "totalMinutes", "minutes", "timescale"
#' @examples
#' convert_timestamp(XFe96data)
convert_timestamp <- function(plate_df) {
  
  # first make sure that the data is sorted correctly
  plate_df <- arrange(plate_df, tick, well)
  
  # add three columns to df (totalMinutes, minutes and time) by converting the timestamp into seconds
  plate_df$time <- as.character((plate_df$time))
  times <- strsplit(plate_df$time, ":")
  plate_df$totalMinutes <- sapply(times, function(x) {
    x <- as.numeric(x)
    x[1] * 60 + x[2] + x[3] / 60
  })
  plate_df$minutes <- plate_df$totalMinutes - plate_df$totalMinutes[1] # first row needs to be first timepoint!
  plate_df$timescale <- round(plate_df$minutes * 60)
  
  return(plate_df)
} #called in preprocessing


# END ---------------------------------------------------------------------


