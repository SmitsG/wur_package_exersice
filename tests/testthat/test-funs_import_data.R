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

