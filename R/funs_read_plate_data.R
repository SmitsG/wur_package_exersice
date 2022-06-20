# funs_read_plate_data.r
# vincent de boer
# December 29th, 2021

library(tidyverse)
library(readxl)
library(tidyxl)
library(logger)

# read_plate_data() -------------------------------------------------------
#' Title : Read Plate data to return all nessecary Seahorse information.
#'
#' @param filePathSeahorse Absolute path to the Seahorse excel data derived from the Agilent Seahorse XF Wave software. assay result file (.asyr) files must converted into .excel files.  
#' @param injscheme Type of injection Scheme
#'
#' @return plate_df tibble (Note: tibble has type list) with all the information important data derived from the Seahorse Excel file. 
#' @export
#'
#' @examples 
#' read_plate_data("data/PBMC/sci_rep/20191219 SciRep PBMCs donor A.xlsx", "HAP")
#' read_plate_data("data/PBMC/sci_rep/20200110 SciRep PBMCs donor B.xlsx", "HAP)
#' read_plate_data("data/PBMC/sci_rep/20200110 SciRep PBMCs donor C.xlsx", "HAP)
read_plate_data <- function(filePathSeahorse, injscheme) {
  log_info("Start reading plate data to return all nessecary Seahorse information")
  # Show the sheets of the excel file.
  sheets <- readxl::excel_sheets(filePathSeahorse)

  # XFe96data <- WurPackageExersice::import_excel_data(filePathSeahorse, sheet = "Raw")
  
  # read_excel reads the 'Raw' data sheet from the Seahorse Excel file.
  XFe96data <- read_excel(filePathSeahorse, sheet = "Raw")
  
  # get Assay information from the Seahorse Assay Configuratoin file and set parameters from the Seahorse Excel file.
  assay_info <- get_assay_info(filePathSeahorse)
  
  # get the interval and injection info
  if (injscheme == "HAP") {
    injection_info <- get_injection_info_H(filePathSeahorse)
  }
  
  if (injscheme == "Maas") {
    injection_info <- get_injection_info_M(filePathSeahorse)
  }
  # get the norm info (Normalization Values: )
  norm_info <- get_platelayout_data(filePathSeahorse,
                                    my_sheet = "Assay Configuration",
                                    my_range = "B84:N92",
                                    my_param = "cell_n"
                                    
  )
  
  #check whether norm info is available
  if (sum(is.na(norm_info$cell_n)) >90){
    norm_available <- FALSE
  } else {
    norm_available <- TRUE}
  
  # get the buffer factor (capacity) info
  bufferfactor_info <- get_platelayout_data(filePathSeahorse,
                                            my_sheet = "Assay Configuration",
                                            my_range = "B96:N104",
                                            my_param = "bufferfactor"
                                            
  )
  
  # get the pH calibration emission data
  pH_calibration <- get_platelayout_data(filePathSeahorse,
                                         my_sheet = "Calibration",
                                         my_range = "P16:AB24",
                                         my_param = "pH_cal_em"
  )
  
  # get the OCR from the excel file
  OCR_from_excel_list <- get_originalRateTable(filePathSeahorse)
  OCR_from_excel <- OCR_from_excel_list[[1]]
  
  # add info to assay_info
  assay_info$norm_available <- norm_available
  assay_info$excel_OCR_background_corrected <- OCR_from_excel_list[[2]] #mark whether excel_ocr data is background corrected
  
  # get the flagged wells and assign to logical column in raw and rate
  # flagged_wells <- get_flagged_wells(filePathSeahorse)
  
  # make the output list
  plate_df <- list(
    XFe96data = XFe96data,
    assay_info = assay_info,
    injection_info = injection_info,
    pH_calibration = pH_calibration,
    norm_info = norm_info, #return only the data not the logical
    bufferfactor_info = bufferfactor_info,
    OCR_from_excel = OCR_from_excel, #return only the data not the logical
    filePathSeahorse = filePathSeahorse
    # flagged_wells = flagged_wells
  )
  log_info("Reading plate data finished, returning plate_df.")
  
  return(plate_df)
}

## get_assay_info() ----------------------------------------------------
#' Title : Get useful information from the Assay Configuration sheet from the Seahorse Excel file.
#'
#' @param filePathSeahorse Absolute path to the Seahorse excel data derived from the Agilent Seahorse XF Wave software. assay result file (.asyr) files must converted into .excel files.  
#'
#' @return assayConfigurationTibble : A tibble with all the nessecary information derived from the Seahorse Assay Configuration sheet.
#' @export
#'
#' @examples
get_assay_info <- function(filePathSeahorse) {

  # read Assay Configuration sheet from the Seahorse Exel file. 
  meta_df <- read_excel(filePathSeahorse,
                        sheet = "Assay Configuration",
                        col_names = c("parameter", "value"),
                        range = "A1:B83"
  )
  meta_df <- meta_df[!is.na(meta_df$parameter), ]
  
  # read Assay Configuration sheet gain1 (Gain Equation)
  gain1 <- read_excel(filePathSeahorse,
                      sheet = "Assay Configuration",
                      col_names = c("value"),
                      range = "D70"
  )
  
  # read Assay Configuration sheet gain2 (Gain Equation)
  gain2 <- read_excel(filePathSeahorse,
                      sheet = "Assay Configuration",
                      col_names = c("value"),
                      range = "E70"
  )
  
  # read O2 target emission cells (O2 Target Emission)
  O2_target_emission <- read_excel(filePathSeahorse,
                                   sheet = "Calibration",
                                   col_names = FALSE,
                                   range = "B4"
  )
  
  # read pH target emission cells (pH Target Emission)
  pH_target_emission <- read_excel(filePathSeahorse,
                                   sheet = "Calibration",
                                   col_names = FALSE,
                                   range = "P4"
  )
  
  F0 <- as.double(meta_df$value[meta_df$parameter == "Calculated FO"])
  V_C <- as.double(meta_df$value[meta_df$parameter == "Pseudo Volume"])
  Tau_AC <- as.double(meta_df$value[meta_df$parameter == "TAC"])
  Tau_W <- as.double(meta_df$value[meta_df$parameter == "TW"])
  Tau_C <- as.double(meta_df$value[meta_df$parameter == "TC"])
  Tau_P <- as.double(meta_df$value[meta_df$parameter == "TP"])
  KSV <- as.double(meta_df$value[meta_df$parameter == "ksv"])
  
  pH_0 <- as.double(meta_df$value[meta_df$parameter == "Calibration pH"])
  plate_id <- meta_df$value[meta_df$parameter == "Plate Barcode"]
  cartridge_barcode <- meta_df$value[meta_df$parameter == "Cartridge Barcode"]
  date_run <- lubridate::mdy_hm(meta_df$value[meta_df$parameter == "Last Run"]) #be carefull with the data format in excel! either mdy or dmy
  assay_name <- meta_df$value[meta_df$parameter == "Assay Name"]
  instrument_serial <- meta_df$value[meta_df$parameter == "Instrument Serial"]
  
  pH_targetEmission <- as.double(pH_target_emission[[1]])
  O2_targetEmission <- as.double(O2_target_emission[[1]])
  gain1 <- as.double(gain1[[1]])
  gain2 <- as.double(gain2[[1]])
  # other constants
  O2_0_mmHg <- 151.6900241
  O2_0_mM <- 0.214
  
  assayConfigurationTibble <- tibble(
    F0,
    V_C,
    Tau_AC, Tau_W,
    Tau_C, Tau_P,
    KSV,
    gain1,
    gain2,
    pH_0,
    pH_targetEmission,
    O2_targetEmission,
    plate_id,
    cartridge_barcode,
    date_run,
    assay_name,
    instrument_serial,
    O2_0_mmHg,
    O2_0_mM
  )
  return(assayConfigurationTibble)
}

## get_injection_info_H() --------------------------------------------------

#' Read the injection information from the "Operation log" sheet H-version
#' 
#' IMPORTANT: the output of the injection information is different on different XFe96 instruments. Two version of
#'   this function are available. The get_injection_info_H assumes the names of the injections are listed in the
#'   "operation log file". The other (get_injection_info_M) function uses a manual assignment of the injection names.
#'   In both cases, the number of measurements per injection ARE read by the function
#'   
#' @param filePathSeahorse Absolute path to the Seahorse excel data derived from the Agilent Seahorse XF Wave software. assay result file (.asyr) files must converted into .excel files.
#'
#' @return A new dataframe called measurement_info The df has three columns: 
#'   $measurement, $interval, $injection
#' @examples
#' get_injection_info_H(filePathSeahorse)
get_injection_info_H <- function (filePathSeahorse){
  
  #read injection strategy and measurements
  suppressMessages(info_sh<-read_excel(filePathSeahorse, sheet = "Operation Log")
  )
  colnames(info_sh) <- c("instruction_name","command_name","command_index","start_time","end_time", "completion_status")
  
  #assumes injection names are available in operation log file (this is default for HAP experiments)
  measurement_info <- filter(info_sh, command_name == "Measure")
  measurement_info$interval <- measurement_info$command_index -1
  measurement_info$measurement <- 1:nrow(measurement_info)
  measurement_info <- measurement_info %>% select(measurement, interval, injection=instruction_name)
  
  return(measurement_info)
}  #called in preprocessing


## get_injection_info_M() ----------------------------------------------------

#' Read the injection information from the "Operation log" sheet M-version
#' 
#' IMPORTANT: the output of the injection information is different on different XFe96 instruments. Two version of
#'   this function are available. The get_injection_info_H assumes the names of the injections are listed in the
#'   "operation log file". The other (get_injection_info_M) function uses a manual assignment of the injection names.
#'   In both cases, the number of measurements per injection ARE read by the function.
#'   The default names for the get_injection_info_M are c("basal", "OM", "FCCP", "AM/rot").
#'   
#' @param filePathSeahorse Absolute path to the Seahorse excel data derived from the Agilent Seahorse XF Wave software. assay result file (.asyr) files must converted into .excel files.
#' @return A new dataframe called measurement_info The df has three columns: 
#'   $measurement, $interval, $injection
#' @examples
#' get_injection_info_M(filePathSeahorse)
get_injection_info_M <- function (filePathSeahorse){
  
  #read injection strategy and measurements
  suppressMessages(info_sh<-read_excel(filePathSeahorse, sheet = "Operation Log")
  )
  
  colnames(info_sh) <- c("instruction_name","command_name","command_index","start_time","end_time", "completion_status")
  
  # No info in instruction name therefore there will be manually adding the name
  measurement_info <- filter(info_sh, command_name %in% c("XF - PC_Measure", "XF - PC_Inject"))
  
  # this loop puts in the interval numbers based on the command index
  interval = 1
  for (i in 1:nrow(measurement_info)){
    if(measurement_info$command_index[i] == 0){
      print("HH")
      measurement_info$command_index[i] <-  interval } else {
        interval <-  interval +1
        measurement_info$command_index[i] <-  interval}
  }
  colnames(measurement_info)[3] <- "interval"
  measurement_info <- filter(measurement_info, command_name == "XF - PC_Measure")
  measurement_info$measurement <- 1:nrow(measurement_info)
  measurement_info <- measurement_info %>% select(measurement, interval)
  
  #gives name of the injection manually
  # case mitostress
  injections_mitostress <- tibble(interval = 1:4, injection=c("basal", "OM", "FCCP", "AM/rot"))
  measurement_info <- left_join(measurement_info, injections_mitostress, by = c("interval"))
  
  ## case glycostress
  #injections_glycostress <- tibble(interval = 1:4, injection=c("basal", "glucose", "OM", "2DG"))
  #measurement_info <- left_join(measurement_info, injections_glycostress, by = c("interval"))
  
  return(measurement_info)
}  #called in preprocessing

## get_platelayout_data() -------------------------------------------------
#' Title: Get plate layout data.
#'
#' @param filePathSeahorse Absolute path to the Seahorse excel data derived from the Agilent Seahorse XF Wave software. assay result file (.asyr) files must converted into .excel files.
#' @param my_sheet Sheet of the Seahorse Excel file
#' @param my_range Range of the cells in the Seahorse Excel file
#' @param my_param Summarised name of the parameter which will include the data that is collected
#'
#' @return dataframe with plate layout data.
#' @export
#'
#' @examples get_platelayout_data("data/PBMC/sci_rep/20191219 SciRep PBMCs donor A.xlsx", "Assay Configuration", "B96:N104", "bufferfactor")
get_platelayout_data <- function(filePathSeahorse, my_sheet,my_range, my_param ){
  
  df <- read_excel(filePathSeahorse, sheet = my_sheet, range = my_range)
  
  colnames(df)[1] <- "firstCol"
  
  df <-  gather(df, key = "key", value = "my_value", -firstCol) %>%
    mutate(firstCol = paste0(firstCol, key) ) %>% 
    select(well = firstCol, my_value) %>%
    arrange(gsub("\\d", "", well, as.numeric(gsub("\\D", "", well)))) 
  
  colnames(df)[2] <- my_param
  
  # add a zero between letter and number if wellname has 2 characters for normalization data
  for (i in 1:nrow(df)){ 
    if (nchar(df$well[i]) ==  2) { 
      wellName <- sub("(.{1})(.*)", "\\10\\2", df$well[i])
    } else {
      wellName <- df$well[i]
    }
    df$well[i] <- wellName
  }
  
  return(df)
  
}


## get_originalRateTable() -------------------------------------------------


#' Title: 
#'
#' @param filePathSeahorse Absolute path to the Seahorse excel data derived from the Agilent Seahorse XF Wave software. assay result file (.asyr) files must converted into .excel files.  
#'
#' @return original_rate_df_list
#' @export 
#'
#' @examples
#' get_originalRateTable("data/PBMC/sci_rep/20191219 SciRep PBMCs donor A.xlsx")
#' get_originalRateTable("data/PBMC/sci_rep/20200110 SciRep PBMCs donor B.xlsx")
#' get_originalRateTable("data/PBMC/sci_rep/20200110 SciRep PBMCs donor C.xlsx")
get_originalRateTable<- function(filePathSeahorse){
  # 
  original_rate_df <-read_excel(filePathSeahorse, sheet = "Rate")
  
  # because rate data can be either background corrected or not this should be checked first
  check_background <- original_rate_df %>% filter(Group == "Background") %>% select(OCR) %>% 
    summarise(mean = mean(OCR)) %>% pull(mean)
  
  if (check_background == 0) {
    corrected_allready <- TRUE
  } else {
    corrected_allready <-  FALSE
  }
  
  if (corrected_allready == TRUE){
    colnames(original_rate_df) <- c("measurement","well", "group", "time_wave", "OCR_wave_bc", "ECAR_wave_bc", "PER_wave_bc")
    original_rate_df <- original_rate_df %>%
      mutate(OCR_wave = 0, ECAR_wave = 0)
    
    original_rate_df <- original_rate_df %>%
      select(measurement, well, group, time_wave, OCR_wave, OCR_wave_bc, ECAR_wave, ECAR_wave_bc)
    
  } else{
    colnames(original_rate_df) <- c("measurement","well", "group", "time_wave", "OCR_wave", "ECAR_wave", "PER_wave")
    
    #do background substraction for wave table
    background<-original_rate_df %>%
      filter(group=="Background") %>%
      group_by(measurement) %>%
      summarize(bkg_OCR_wave = mean(OCR_wave),
                bkg_ECAR_wave = mean(ECAR_wave)
      )
    original_rate_df<-left_join(original_rate_df, background, by = c("measurement"), copy = TRUE)
    
    original_rate_df$OCR_wave_bc <- original_rate_df$OCR_wave - original_rate_df$bkg_OCR_wave
    original_rate_df$ECAR_wave_bc <- original_rate_df$ECAR_wave - original_rate_df$bkg_ECAR_wave
    
    original_rate_df <- original_rate_df %>%
      select(measurement, well, group, time_wave, OCR_wave, OCR_wave_bc, ECAR_wave, ECAR_wave_bc)
  }
  
  original_rate_df_list <- list(original_rate_df, corrected_allready)
  
  return(original_rate_df_list)
  
}


## get_flagged_wells() -----------------------------------------------------

#' Title: Get the flagged (unselected) wells of the Seahorse Excel data
#'
#' @param filePathSeahorse Absolute path to the Seahorse excel data derived from the Agilent Seahorse XF Wave software. assay result file (.asyr) files must converted into .excel files.  
#'
#' @return flagged_vector
#' @export
#'
#' @examples
#' get_flagged_wells("data/PBMC/sci_rep/20191219 SciRep PBMCs donor A.xlsx")
#' 
get_flagged_wells <- function(filePathSeahorse){
  
  #read excel file using todyxl
  
  x <- xlsx_cells(filePathSeahorse, "Assay Configuration", include_blank_cells = FALSE)
  formats <- xlsx_formats(filePathSeahorse, "Assay Configuration", include_blank_cells = FALSE)
  
  # subset to only the platelayout with the cells that show the "unselected" wells by user
  subset_x <- x %>% filter(row %in% c(12:19)) %>% filter(col %in% c(3:14))
  
  # get the "unselected" (flagged) wells (based on color fill)
  flagged_df <- subset_x[subset_x$local_format_id %in%
                           which(formats$local$fill$patternFill$fgColor$rgb == "FFFFFFFF"),
                         c("address")]
  
  # optionally alignment format can be used
  subset_x[subset_x$local_format_id %in%
             which(formats$local$alignment$horizontal == "center"),
           c("address")]
  
  # changed the cell address to well names
  new_col_names <- flagged_df %>% 
    pull(address) %>% substr(1,1) %>% 
    str_c(collapse = "---") %>% 
    str_replace_all(c("C" = "01", "D" = "02", "E" = "03", "F" = "04", "G" = "05", "H" = "06", 
                      "I" = "07", "J" = "08", "K" = "09", "L" = "10", "M" = "11", "N" = "12"))
  new_col_names <-   unlist(str_split(new_col_names, "---"))
  
  new_row_names <- flagged_df %>% 
    pull(address) %>% substr(2,3) %>% 
    str_c(collapse = "---") %>% 
    str_replace_all(c("12" = "A", "13" = "B", "14" = "C", "15" = "D", "16" = "E", "17" = "F", 
                      "18" = "G", "18" = "H"))
  new_row_names <-   unlist(str_split(new_row_names, "---"))
  
  # output the wells that were "unselected" (flagged)
  flagged_vector <- paste0(new_row_names, new_col_names)
  
  return(flagged_vector)
}
