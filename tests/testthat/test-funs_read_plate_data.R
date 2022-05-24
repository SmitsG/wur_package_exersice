working_directory <- getwd()
working_directory <- setwd('../..')
working_directory <- getwd()

flnme <- file.path(working_directory, paste("data/raw_data/20191219 SciRep PBMCs donor A.xlsx"))

test_that("a data list is returned", {
  expect_type(read_plate_data(flnme, "HAP"), "list")
})

flnme <- file.path(working_directory, paste("data/raw_data/20200110 SciRep PBMCs donor B.xlsx"))

test_that("a data list is returned", {
  expect_type(read_plate_data(flnme, "HAP"), "list")
})

flnme <- file.path(working_directory, paste("data/raw_data/20200110 SciRep PBMCs donor C.xlsx"))

test_that("a data list is returned", {
  expect_type(read_plate_data(flnme, "HAP"), "list")
})

flnme <- file.path(working_directory, paste("data/raw_data/vb211014_evelien.xlsx"))

test_that("a data list is returned", {
  expect_type(read_plate_data(flnme, "HAP"), "list")
})





# test_that("a data list is returned", {
#   expect_error(read_plate_data("HAP"), "Error: `path` does not exist: 'HAP'")
# })




