# By: Gerwin Smits
# Wageningen University

###### ISSUE: NOTES OCCUR WHEN USING devtools:check().
# When using the devtools:check() function (R CMD check), the following (example) NOTES will occur. 
# plot_raw_BKGD_means: no visible binding for global variable ‘measurement’
# plot_raw_BKGD_means: no visible binding for global variable ‘well’

# The following solutions will will prevent these [no visible binding for global variable '<variable>'.] NOTES.
# Variable(s) are now bound to object(s) and so the R CMD check has nothing to complain about. 

#####Solution 1: Adding variables to globalVariables. 
# This can be done manually.
# Or by using the checkhelper package.
# Example of using the checkhelper pakcage
# use checkhelper::print_globals(quiet = TRUE) in the RStudio Console and copy variables to this script (globals.R)
# This will find lots of variables, but not all. 
# Example: 
# before checkhelper - 548 notes with devtools::check()
# after checkhelper - 358 notes with devtools::check()
# checkhelper package is still work in progress, so hopefuly this package will include more variables in the future, so we get less NOTES when using devtools:check().

#####Solution 2: Add these variables to the first line of a function.
# Example:
# background_QC_1 <- function(XFe96data_tibble, working_directory){
# scale_color_manual_interactive <- fileName <- date_run <- map <- bkgd_QC <- iqr <- NULL  
# .. }

globalVariables(unique(c(
  # background_QC_1: 
  "IQR", "total_score", "well_stats_scirep", 
  # convert_timestamp: 
  "well", 
  # get_bkg_quality_colors: 
  ".", "total_score", "well", 
  # get_BKGD_auc: 
  ".data", "group", "measurement", "tick", "timescale", "well", 
  # get_BKGD_auc : background_drift3: 
  "auc_func", 
  # get_flagged_wells: 
  "address", 
  # get_injection_info_H: 
  "command_name", "instruction_name", "interval", "measurement", 
  # get_injection_info_M: 
  "command_name", "measurement", 
  # get_originalRateTable: 
  "ECAR_wave", "ECAR_wave_bc", "group", "Group", "measurement", "OCR", "OCR_wave", "OCR_wave_bc", "time_wave", "well", 
  # get_platelayout_data: 
  "firstCol", "key", "my_value", "well", 
  # get_score: 
  ".data", "skim_variable", 
  # get_well_scores: 
  "well_stats_scirep", 
  # heatmap_groupMaker: 
  "column", "group", "measurement", "well2", 
  # make_range_plot: 
  "interval", "m_max", "m_min", "measurement", 
  # make_range_plot : get_range: 
  ".data", "group", "interval", "measurement", "minutes", "tick", "timescale", "well", 
  # plot_allWells_firstTicks: 
  ".data", "emission", "group", "measurement", "minutes", "tick", "timescale", "well", 
  # plot_BKGD_auc_facet: 
  ".data", "measurement", "well", 
  # plot_BKGD_auc_facet : get_BKGD_auc: 
  ".data", "group", "measurement", "tick", "timescale", "well", 
  # plot_BKGD_auc_facet : make_raw_em_corr_BKGD_plot: 
  "BKG_auc", "measurement", "well", 
  # plot_group_emissions: 
  ".data", "emission", "group", "m_em", "measurement", "minutes", "sd_em", "tick", "timescale", "well", 
  # plot_raw_BKGD: 
  ".data", "emission", "group", "measurement", "minutes", "tick", "timescale", "well", 
  # plot_raw_BKGD_means: 
  ".data", "emission", "group", "measurement", "minutes", "O2_em_corr_mean", "sd", "tick", "timescale", "well", 
  # preprocess_plate_data_2: 
  "cell_n", "data", "group", "injection", "interval", "measurement", "minutes", "O2_em_corr", "O2_em_corr_bkg", "O2_mmHg", "O2_mmHg_bkg", "pH", "pH_bkgd", "pH_em_corr", "pH_em_corr_bkg", "pH_em_corr_corr", "pH_em_corr_corr_bkg", "plate_id", "tick", "timescale", "well", 
  # read_excel_rwave: 
  "colNames", "filePathSeahorse", "sheetName"
)))
globalVariables(c(":=", "!!", '%>%'))