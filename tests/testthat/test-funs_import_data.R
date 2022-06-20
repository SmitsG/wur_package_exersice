# The current absolute path contains /script/shiny/
working_directory <- getwd()
# Go 2 steps back in the absolute path, with ../..
working_directory <- setwd('../..')
# So you get the absolute path of the rwave.Rproj file.
# This path contains all directories with the nessecary functions inside. 
working_directory <- getwd()

test_that("a data list is returned", {
  expect_type(import_excel_data(excel_data_path, sheet = "Raw"), "list")
})

test_that("a data list is returned", {
  expect_type(import_excel_data(excel_data_path, sheet = "Rate"), "list")
})

test_that("a data list is returned", {
  expect_type(import_excel_data(excel_data_path, sheet = "Assay Configuration"), "list")
})

test_that("a data list is returned", {
  expect_type(import_excel_data(excel_data_path, sheet = "Normalized Rate (Plates)"), "list")
})


