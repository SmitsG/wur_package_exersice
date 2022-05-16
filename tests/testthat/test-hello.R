test_that("a data list is returned", {
  expect_type(import_excel_data(excel_data_path, sheet = "Raw"), "list")
})

test_that("check if warning occurs", {
  expect_error(import_excel_data(), 'ERROR : argument "excel_data_path" is missing, with no default')
})
