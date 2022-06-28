library(logger)
library(readxl)
library(dplyr)
library(tidyr)

working_directory <- "/home/xiang/Documents/Documents Gerwin/Projects/r_exersice/wur_package_exersice"
source(paste(working_directory,"/tests/testthat/test-funs_read_plate_data.R", sep=""))

# Check if a data tibble is returned, and check data tibble. (Note: tibble has type list) 
test_that("a data tibble is returned (list format)", {
  plate_df <- expect_type(read_plate_data(file.path(working_directory, paste("data/raw_data/20200110_SciRep_PBMCs_donor_B.xlsx")), "HAP"), "list")
  check_column_names(plate_df, plate_df_columns_list)
  test_XFe96data(plate_df)
})