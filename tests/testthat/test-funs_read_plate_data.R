library(testthat)

# tests for funs_read_plate_data.R
# funs_read_plate_data.R returns a tibble called plate_df
# plate_df (Note: Contains tibbles within in a list) contains all the information important data derived from the Seahorse Excel file. 

# get the project directory to work from.
working_directory <- getwd()
working_directory <- setwd('../..')
working_directory <- getwd()

# source test-funs_preprocess_plate_data_new.R
source(paste(working_directory,"/tests/testthat/test-funs_preprocess_plate_data_new.R", sep=""))

# check if column names exists in df.
check_column_names <- function(df, df_columns_list){
  for(columname in df_columns_list){
    expect_true(columname %in% names(df))
  }
}

# check plate_df$XFe96data columns (length, type)
test_XFe96data <- function(plate_df){
  expect_length(plate_df$XFe96data, 21)
  expect_true(any(plate_df$XFe96data$Measurement > 0))
  expect_type(plate_df$XFe96data$Measurement, "double")
  expect_type(plate_df$XFe96data$Tick, "double")
  expect_type(plate_df$XFe96data$Well, "character")
  # expect_vector(plate_df$XFe96data$Well, 3)
  # expect_true(grepl("^[A-H](0[1-9]|1[0-1])\b$|$", plate_df$XFe96data$Well))
  expect_type(plate_df$XFe96data$Group, "character")
  expect_type(plate_df$XFe96data$TimeStamp, "character")
  expect_type(plate_df$XFe96data$`Well Temperature`, "double")
  expect_type(plate_df$XFe96data$`O2 is Valid`, "character")
  expect_type(plate_df$XFe96data$`O2 (mmHg)`, "double")
  expect_type(plate_df$XFe96data$`O2 Light Emission`, "double")
  expect_type(plate_df$XFe96data$`O2 Dark Emission`, "double")
  expect_type(plate_df$XFe96data$`O2 Ref Light`, "double")
  expect_type(plate_df$XFe96data$`O2 Ref Dark`, "double")
  expect_type(plate_df$XFe96data$`O2 Corrected Em.`, "double")
  expect_type(plate_df$XFe96data$`pH Is Valid`, "character")
  expect_type(plate_df$XFe96data$pH, "double")
  expect_type(plate_df$XFe96data$`pH Light`, "double")
  expect_type(plate_df$XFe96data$`pH Dark`, "double")
  expect_type(plate_df$XFe96data$`pH Ref Light`, "double")
  expect_type(plate_df$XFe96data$`pH Ref Dark`, "double")
  expect_type(plate_df$XFe96data$`pH Corrected Em.`, "double")
}

test_assay_info <- function (plate_df){
  expect_type(plate_df$assay_info$F0, "double")
  expect_type(plate_df$assay_info$V_C, "double")
  expect_type(plate_df$assay_info$Tau_AC, "double")
  expect_type(plate_df$assay_info$Tau_W, "double")
  expect_type(plate_df$assay_info$Tau_C, "double")
  expect_type(plate_df$assay_info$Tau_P, "double")
  expect_type(plate_df$assay_info$KSV, "double")
  expect_type(plate_df$assay_info$gain1, "double")
  expect_type(plate_df$assay_info$gain2, "double")
  expect_type(plate_df$assay_info$pH_0, "double")
  expect_type(plate_df$assay_info$pH_targetEmission, "double")
  expect_type(plate_df$assay_info$O2_targetEmission, "double")
  expect_type(plate_df$assay_info$plate_id, "character")
  expect_type(plate_df$assay_info$cartridge_barcode, "character")
  expect_type(plate_df$assay_info$date_run, "date-time")
  expect_type(plate_df$assay_info$assay_name, "character")
  expect_type(plate_df$assay_info$instrument_serial, "character")
  expect_type(plate_df$assay_info$O2_0_mmHg, "double")
  expect_type(plate_df$assay_info$O2_0_mM, "double")
  expect_type(plate_df$assay_info$norm_available, "logical")
  expect_type(plate_df$assay_info$excel_OCR_background_corrected, "logical")
}

test_injection_info <- function(plate_df){
  expect_type(plate_df$injection_info$measurement, "integer")
  expect_type(plate_df$injection_info$interval, "double")
  expect_type(plate_df$injection_info$injection, "character")
}

test_pH_calibaration <- function(plate_df){
  expect_type(plate_df$pH_calibration$well, "character")
  expect_type(plate_df$pH_calibration$pH_cal_em, "double")
}

test_norm_info <- function(plate_df){
  expect_type(plate_df$norm_info$well, "character")
  expect_type(plate_df$norm_info$cell_n, "double")
}

test_bufferfactor_info <- function(plate_df){
  expect_type(plate_df$bufferfactor_info$well, "character")
  expect_type(plate_df$bufferfactor_info$bufferfactor, "character")
}

test_bufferfactor_info <- function(plate_df){
  expect_type(plate_df$OCR_from_excel$measurement, "double")
  expect_type(plate_df$OCR_from_excel$well, "character")
  expect_type(plate_df$OCR_from_excel$group, "character")
  expect_type(plate_df$OCR_from_excel$time_wave, "double")
  expect_type(plate_df$OCR_from_excel$OCR_wave, "double")
  expect_type(plate_df$OCR_from_excel$OCR_wave_bc, "double")
  expect_type(plate_df$OCR_from_excel$ECAR_wave, "double")
  expect_type(plate_df$OCR_from_excel$ECAR_wave_bc, "double")
}

# Columnames for each vector in plate_df tibble. The returned tibble consists of 8 columns (vectors).
plate_df_columns_list <<- list("XFe96data", 
                              "assay_info", 
                              "injection_info", 
                              "pH_calibration", 
                              "norm_info", 
                              "bufferfactor_info", 
                              "OCR_from_excel", 
                              "filePathSeahorse")


# Check if a data tibble is returned, and check data tibble. (Note: tibble has type list)
test_that("a data tibble is returned (list format)", {
  # check if output is tibble.
  plate_df <- expect_type(read_plate_data(file.path(working_directory, paste("data/raw_data/20200110_SciRep_PBMCs_donor_C.xlsx")), "HAP"), "list")
  # check if all columns from list exist.
  check_column_names(plate_df, plate_df_columns_list)
  # check XFe96data columns on type.
  test_XFe96data(plate_df)
  test_assay_info(plate_df)
  test_injection_info(plate_df)
  test_pH_calibaration(plate_df)
  test_norm_info(plate_df)
  test_bufferfactor_info(plate_df)
  # check if preprocess_plate_data output is tibble.
  XFe96data_tibble <- expect_type_preprocess_plate_data_return(plate_df)
  # check length of preprocess_plate_data tibble.
  expect_length_preprocess_plate_data_return(plate_df)
  # check if all columns from list exist.
  check_column_names(XFe96data_tibble, XFe96data_tibble_list)
})

# Check if a data tibble is returned, and check data tibble. (Note: tibble has type list)
test_that("a data tibble is returned (list format)", {
  # check if output is tibble.
  plate_df <- expect_type(read_plate_data(file.path(working_directory, paste("data/raw_data/20191219_SciRep_PBMCs_donor_A.xlsx")), "HAP"), "list")
  # check if all columns from list exist.
  check_column_names(plate_df, plate_df_columns_list)
  # check XFe96data columns on type.
  test_XFe96data(plate_df)
  # check if preprocess_plate_data output is tibble.
  XFe96data_tibble <- expect_type_preprocess_plate_data_return(plate_df)
  # check length of preprocess_plate_data tibble.
  expect_length_preprocess_plate_data_return(plate_df)
  # check if all columns from list exist.
  check_column_names(XFe96data_tibble, XFe96data_tibble_list)
})

# Check if a data tibble is returned, and check data tibble. (Note: tibble has type list) 
test_that("a data tibble is returned (list format)", {
  # check if output is tibble.
  plate_df <- expect_type(read_plate_data(file.path(working_directory, paste("data/raw_data/20200110_SciRep_PBMCs_donor_B.xlsx")), "HAP"), "list")
  # check if all columns from list exist.
  check_column_names(plate_df, plate_df_columns_list)
  # check XFe96data columns on type.
  test_XFe96data(plate_df)
  # check if preprocess_plate_data output is tibble.
  XFe96data_tibble <- expect_type_preprocess_plate_data_return(plate_df)
  # check length of preprocess_plate_data tibble.
  expect_length_preprocess_plate_data_return(plate_df)
  # check if all columns from list exist.
  check_column_names(XFe96data_tibble, XFe96data_tibble_list)
  
})

# Check if a data tibble is returned, and check data tibble. (Note: tibble has type list)
test_that("a data tibble is returned (list format)", {
  plate_df <- expect_type(read_plate_data(file.path(working_directory, paste("data/raw_data/vb211014_evelien.xlsx")), "HAP"), "list")
  check_column_names(plate_df, plate_df_columns_list)
  test_XFe96data(plate_df)
  XFe96data_tibble <- expect_type_preprocess_plate_data_return(plate_df)
  expect_length_preprocess_plate_data_return(plate_df)
  check_column_names(XFe96data_tibble, XFe96data_tibble_list)
})

# Check the length of the returned data tibble. The returned tibble consists of 8 columns (vectors).
test_that("a tibble of length 8 is expected", {
  expect_length(read_plate_data(file.path(working_directory, paste("data/raw_data/20200110_SciRep_PBMCs_donor_C.xlsx")), "HAP"), 8)
})

# Check the length of the returned data tibble. The returned tibble consists of 8 columns (vectors).
test_that("a tibble of length 8 is expected", {
  expect_length(read_plate_data(file.path(working_directory, paste("data/raw_data/20191219_SciRep_PBMCs_donor_A.xlsx")), "HAP"), 8) 
})

# Check the length of the returned data tibble. The returned tibble consists of 8 columns (vectors).
test_that("a tibble of length 8 is expected", {
  expect_length(read_plate_data(file.path(working_directory, paste("data/raw_data/20200110_SciRep_PBMCs_donor_B.xlsx")), "HAP"), 8) 
})

# Check the length of the returned data tibble. The returned tibble consists of 8 columns (vectors).
test_that("a data list is returned", {
  expect_length(read_plate_data(file.path(working_directory, paste("data/raw_data/vb211014_evelien.xlsx")), "HAP"), 8) 
})




