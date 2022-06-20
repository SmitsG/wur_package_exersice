# check if column names exists in df.
check_column_names <- function(df, df_columns_list){
  for(columname in df_columns_list){
    expect_true(columname %in% names(plate_df))
  }
}

# Check if a data tibble is returned, and check data tibble. (Note: tibble has type list) 
test_that("a data tibble is returned (list format)", {
  plate_df <- expect_type(read_plate_data(file.path(working_directory, paste("data/raw_data/20200110_SciRep_PBMCs_donor_B.xlsx")), "HAP"), "list")
  check_column_names(plate_df, plate_df_columns_list)
  test_XFe96data(plate_df)
})