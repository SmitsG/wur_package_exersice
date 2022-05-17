# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library(tidyverse)
library(readxl)
library(logger)
library(testthat)
library(devtools)
library(roxygen2)

# get the project directory of the working directory
working_directory <- getwd()

# get the path to the data Excel file.
excel_data_path <- paste(c(working_directory, "data/raw_data/data.xlsx"), collapse="/")

## To show sheetnames of the Excel file use the following command:
# excel_sheets(excel_data_path)

#' Imports an excel file.
#'
#' @param excel_data_path Relative path, from project directory to the excel file located at the data.
#' @param sheet Sheet to read. Either a string (the name of a sheet), or an integer (the position of the sheet). Ignored if the sheet is specified via range. If neither argument specifies the sheet, defaults to the first sheet.
#' @param range A cell range to read from, as described in cell-specification. Includes typical Excel ranges like "B3:D87", possibly including the sheet name like "Budget!B2:G14", and more. Interpreted strictly, even if the range forces the inclusion of leading or trailing empty rows or columns. Takes precedence over skip, n_max and sheet.
#' @param col_names TRUE to use the first row as column names, FALSE to get default names, or a character vector giving a name for each column. If user provides col_types as a vector, col_names can have one entry per column, i.e. have the same length as col_types, or one entry per unskipped column.
#' @param col_types Either NULL to guess all from the spreadsheet or a character vector containing one entry per column from these options: "skip", "guess", "logical", "numeric", "date", "text" or "list". If exactly one col_type is specified, it will be recycled. The content of a cell in a skipped column is never read and that column will not appear in the data frame output. A list cell loads a column as a list of length 1 vectors, which are typed using the type guessing logic from col_types = NULL, but on a cell-by-cell basis.
#'
#' @return A data tibble if the Excel file exists, or an error or warning with the conditionaMessage.
#' @export
#'
#' @examples out <- import_excel_data(excel_data_path, sheet = "Raw")
import_excel_data <- function(excel_data_path, sheet=NULL, range = NULL, col_names = TRUE, col_types = NULL) {
  out <- tryCatch(
    {
      message("Import data")
      readxl::read_excel(excel_data_path,
                 sheet = sheet,
                 range = range,
                 col_names = col_names,
                 col_types = col_types)
    },
    error=function(err) {
      print(err)
      # Show the error message.
      x <- cat("ERROR :", conditionMessage(err), "\n")
      return(x)
      log_error(conditionMessage(err), "\n")
    },
    warning=function(war) {
      # Show the warning message.
      x<- cat("WARNING :", conditionMessage(war), "\n")
      return(x)
      log_warn(conditionMessage(war), "\n")
    },
    finally={
      print("finally Executed")
    }
  )
  return(out)
}

import_excel_data(excel_data_path, sheet = "Raw")
import_excel_data()
