library(tidyxl)
library(logger)

# Read an Excel file with certain imput parameters
#' Title
#'
#' @param path 
#' @param sheet 
#' @param range 
#' @param col_names 
#' @param col_types 
#' @param na 
#' @param trim_ws 
#' @param skip 
#' @param n_max 
#' @param guess_max 
#' @param progress 
#' @param .name_repair 
#'
#' @return
#' @export
#'
#' @examples
read_excel_rwave <- function( path,
                              sheet = NULL,
                              range = NULL,
                              col_names = TRUE,
                              col_types = NULL,
                              na = "",
                              trim_ws = TRUE,
                              skip = 0,
                              n_max = Inf,
                              guess_max = min(1000, n_max),
                              progress = readxl_progress(),
                              .name_repair = "unique") {
  out <- tryCatch(
    {
      log_info("Reading excel data")
      meta_df <- read_excel(filePathSeahorse,
                            sheet = sheetName,
                            col_names = colNames,
                            range = range
      )
      log_info("The excel data has been loaded.")
    },
    error=function(err) {
      # Show the error message.
      cat("ERROR :", conditionMessage(err), "\n")
      log_error(conditionMessage(err), "\n")
    },
    warning=function(war) {
      # Show the warning message.
      cat("WARNING :", conditionMessage(war), "\n")
      log_warn(conditionMessage(war), "\n")
    },
    finally={
      print("finally Executed")
    }
  )
  return(out)
}