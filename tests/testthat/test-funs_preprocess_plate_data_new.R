# tests for preprocess_plate_data_new.R
# preprocess_plate_data_new.R returns a tibble called XFe96data_tibble (a list with the new dataframe and the assay_info)

# These functions are called by testthat from the test-funs_read_plate_data.R script.
# To test the output of preprocess_plate_data_new, the output of funs_read_plate_data is used (plate_df).

XFe96data_tibble_list <<- list("plate_id",
                               "filePathSeahorse",
                               "date",
                               "assay_info",
                               "injection_info",
                               "raw_data",
                               "rate_data")

# Check if a data tibble is returned, and check data tibble. (Note: tibble has type list) 
expect_type_preprocess_plate_data_return <- function(plate_df){
    XFe96data_tibble <- expect_type(preprocess_plate_data_2(plate_df), "list")
    return(XFe96data_tibble)
}

# Check the length of the returned data tibble. The returned tibble consists of 7 columns (vectors).
expect_length_preprocess_plate_data_return <- function(plate_df){
    expect_length(preprocess_plate_data_2(plate_df), 7)
}

