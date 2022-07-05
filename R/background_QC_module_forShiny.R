library(knitr)
#initialize
library(tidyverse)
library(bayestestR)
library(ggiraph)
library(dplyr)

# library(skimr)


knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir =   here::here())


#' Get the quality information of the background wells
#' @description The quality information gives us information about techniqual variation of our analysis.
#'
#' @param well_scores_tibble_list tibble with well scores
#' 
# example: well_scores_tibble_list looks like: 
# A tibble: 4 × 2
#   well  total_score
# <chr>       <dbl>
#1 A01            11
#2 A12             6
#3 H01            16
#4 H12             7
# Note: Tibble is of type list
#' 
#' @param plate_scores_tibble_list tibble with plate scores
#' 
# example: plate_scores_tibble_list looks like:
# A tibble: 1 × 2
# plate_id     total_score
# <chr>              <dbl>
# 1 V0174416419V      7
# Note: Tibble is of type list
#' @keywords internal
#'
#' @return qc_color_list : a list which includes the different background wells, the quality score, color (green, red or yellow), 
#' the plate_id (example "V0174416419V") and the calculated quality score of the plate.



get_bkg_quality_colors <- function(well_scores_tibble_list, plate_scores_tibble_list){
  # Add the right color to the data
  # (green = good quality, orange = medium quality, red = bad quality)
  validate_well_scores_green <- . %>%
    filter_all(all_vars(total_score %in% c(6, 7, 8, 9))) %>%
    mutate(color = "green")
  
  validate_well_scores_orange <- . %>%
    filter_all(all_vars(total_score %in% c(10, 11, 12))) %>%
    mutate(color = "orange")
  
  validate_well_scores_red <- . %>%
    filter_all(all_vars(total_score %in% c(13, 14, 15, 16, 17, 18))) %>%
    mutate(color = "red")
  
  validate_plate_scores_green <- . %>%
    filter_all(all_vars(total_score %in% c(3, 4))) %>%
    mutate(color = "green")
  
  validate_plate_scores_orange <- . %>%
    filter_all(all_vars(total_score %in% c(5, 6))) %>%
    mutate(color = "orange")
  
  validate_plate_scores_red <- . %>%
    filter_all(all_vars(total_score %in% c(7, 8, 9))) %>%
    mutate(color = "red")
  
  # put right color to each score.
  green_well_scores <- well_scores_tibble_list %>% validate_well_scores_green()
  orange_well_scores <- well_scores_tibble_list %>% validate_well_scores_orange()
  red_well_scores <- well_scores_tibble_list %>% validate_well_scores_red()
  
  green_plate_scores <- plate_scores_tibble_list %>% validate_plate_scores_green()
  orange_plate_scores <- plate_scores_tibble_list %>% validate_plate_scores_orange()
  red_plate_scores <- plate_scores_tibble_list %>% validate_plate_scores_red()
  
  # Put all tibbles with colors together.
  all_scores_color <- bind_rows(list(
    green_well_scores,
    red_well_scores,
    orange_well_scores
  ))
  all_plate_scores_color <- bind_rows(list(
    green_plate_scores,
    red_plate_scores,
    orange_plate_scores
  ))
  
  # Get the well, total_score and color from the tibble.
  wells <- all_scores_color %>%
    pull(well) %>%
    as.character()
  score <- all_scores_color %>%
    pull(total_score) %>%
    as.integer()
  color <- all_scores_color %>%
    pull(color) %>%
    as.character()
  
  # Get plate_id, plate_score and plate_color from tibble.
  plate_id <- all_plate_scores_color %>%
    pull(plate_id) %>%
    as.character()
  plate_score <- all_plate_scores_color %>%
    pull(total_score) %>%
    as.integer()
  plate_color <- all_plate_scores_color %>%
    pull(color) %>%
    as.character()
  
  qc_color_list <- list("wells" = wells, 
                        "score" = score, 
                        "color" = color, 
                        "plate_id" = plate_id, 
                        "plate_score" = plate_score, 
                        "plate_color" = plate_color)

  return(qc_color_list)
  
}


#' @title Function used to collect background parameters
#'
#' @param total_df The preprocessed Raw data sheet of the Seahorse Excel file (converted from .asyr)
#' @param var The type of Raw Emission correction, "O2_em_corr" or "pH_em_corr"
#' @param targetEMS The target Emission (12500 Au environmental fluorescence emission)
#' @keywords internal
#' @return AUC_bkgd background parameters for area under the curve (in list)
# # A tibble: 48 × 5 (note: Tibble is of type list)
# well  measurement      auc    auc2 dev_fromTarget
# <chr>       <dbl>    <dbl>   <dbl>          <dbl>
#   1 A01             1 0.00600  -33026.         -77.9 
#  


get_BKGD_auc <- function(total_df, var, targetEMS){

  background_drift <- function(x,y){
    y <- y/min(y)-1
    auc_func <- area_under_curve(x, y, method = "trapezoid")
    auc_func <- auc_func/max(x)
    return(auc_func)
  }
  background_drift2 <- function(x,y, baseline){
    y <- y-baseline
    auc_func <- area_under_curve(x, y, method = "trapezoid")
    return(auc_func)
  }
  
  background_drift3 <- function(x,y, baseline){
    y[1] <- y-baseline
    return(auc_func)
  }
  
  df<- total_df %>% filter(group == "Background") %>% 
    select(measurement, well, group,timescale, tick, emission = .data[[var]])
  df <- df[!is.na(df$emission), ]
  
  temp_df <- NULL
  temp_auc <- tibble(well = 0, measurement = 0, auc = 0)
  AUC_bkgd <- NULL
  wellList <- unique(as.vector((df$well)))
  measurementList <- unique(as.vector((df$measurement)))
  for (j in wellList){
    for (k in measurementList){
      temp_df <- df %>% filter(well == j & measurement == k)
      temp_auc$auc <- background_drift(temp_df$timescale, temp_df$emission)
      temp_auc$auc2 <- background_drift2(temp_df$timescale, temp_df$emission, targetEMS)
      temp_auc$dev_fromTarget <-temp_df$emission[1] - targetEMS #takes only the first tick here!!
      temp_auc$well <- j
      temp_auc$measurement <- k
      
      AUC_bkgd <- rbind(AUC_bkgd, temp_auc)
    }
  }
  return(AUC_bkgd)
}
# Functions for calculating the scores per plate
#' Calculating the scores per plate
#'
#' @param df Stat summaries for NEW PLATES - well
#' 
# A tibble: 1 × 8
# Groups:   plate_id [1]
# plate_id       well Maximum Minimum First  Last Range(M-M) Range(F-L)
# <chr>          <chr> <dbl>   <dbl> <dbl> <dbl>        <dbl>        <dbl>
# 1 V0174416419V  H12   95.7   -274. -274. -95.7         178.         178.
#'  
#' @param qc The stats for the complete PBMC dataset - well stats
#' 
# A tibble: 3 × 6
#skim_variable n_missing complete_rate    p25   p75   iqr
#<chr>             <int>         <dbl>  <dbl> <dbl> <dbl>
#1 Maximum               0             1   20.4  177.  156.
#2 Minimum               0             1 -610.  -367.  243.
#3 Range(F-L)            0             1  459.   700.  241.
#'
#' @param var Name of the variable
#' [1] "Maximum"
#' [1] "Minimum"
#' [1] "Range(F-L)"
#' [1] "First"
#' [1] "Last"
#' [1] "Range(M-M)"
#' [1] "Maximum"
#' [1] "Minimum"
#' [1] "Range(F-L)"
#' [1] "First"
#' [1] "Last"
#' [1] "Range(M-M)"
#' [1] "Maximum"
#' [1] "Minimum"
#' [1] "Range(F-L)"
#' [1] "First"
#' [1] "Last"
#' [1] "Range(M-M)"
#' [1] "Maximum"
#' [1] "Minimum"
#' [1] "Range(F-L)"
#' [1] "First"
#' [1] "Last"
#' [1] "Range(M-M)"
#' [1] "Maximum"
#' [1] "Minimum"
#' [1] "Range(F-L)"
#' @keywords internal
#'
#' @return scores per plate
#' example : 1 or 2, or 3 etc. (double)
#' 
#'
# @examples get_score(well_stats_scirep %>% slice(w), qc_well, "Maximum")
get_score <- function(df, qc, var){
  qc <- qc %>% filter(skim_variable == var)
  Q25 <- qc$p25[[1]]
  Q75 <- qc$p75[[1]]
  iqr <- qc$iqr[[1]]
  x <- df %>% pull(.data[[var]])
  
  score = 3
  if(between(x, Q25, Q75)){
    score = 1
  }
  if(between(x, Q25-1.5*iqr, Q25) |
     between(x, Q75,Q75 +1.5*iqr)){
    score = 2
  }

  return(score)
}

#' Getting well scores
#'
#' @param df Stat summaries for NEW PLATES - well
#' 
# A tibble: 1 × 8
# Groups:   plate_id [1]
# plate_id       well Maximum Minimum First  Last Range(M-M) Range(F-L)
# <chr>          <chr> <dbl>   <dbl> <dbl> <dbl>        <dbl>        <dbl>
# 1 V0174416419V  H12   95.7   -274. -274. -95.7         178.         178.
#' 
#' @param qc_well # the stats for the complete PBMC dataset - well stats
#  @export
#' @return df with scores 
#'
## A tibble: 4 × 15
# Groups:   plate_id [1]
# plate_id     well  Maximum Minimum  First  Last Range(M-M) Range(F-L) max_score min_score rangeFL_score first_score last_score rangeMM_score total_score
# <chr>        <chr>   <dbl>   <dbl>  <dbl> <dbl>        <dbl>        <dbl>     <dbl>     <dbl>         <dbl>       <dbl>      <dbl>         <dbl>       <dbl>
#  1 V0174416419V A01      76.3   -77.9  -77.9  66.3         154.         144.         2         2             1           2          2             2          11
#'
#'
# @examples get_well_scores(well_stats_scirep, qc_well)
get_well_scores <- function(df, qc_well){
  df$max_score <-  0
  df$min_score <-  0
  df$rangeFL_score <-  0
  df$first_score <-  0 
  df$last_score <-  0 
  df$rangeMM_score <-  0
  
  for(w in 1:nrow(well_stats_scirep)){
    df$max_score[w] <- get_score(well_stats_scirep %>% slice(w), qc_well, "Maximum")
    df$min_score[w] <- get_score(well_stats_scirep %>% slice(w), qc_well, "Minimum")
    df$rangeFL_score[w] <- get_score(well_stats_scirep %>% slice(w), qc_well, "Range(F-L)")
    df$first_score[w] <- get_score(well_stats_scirep %>% slice(w), qc_well, "First")
    df$last_score[w] <- get_score(well_stats_scirep %>% slice(w), qc_well, "Last")
    df$rangeMM_score[w] <- get_score(well_stats_scirep %>% slice(w), qc_well, "Range(M-M)")
  }
  df$total_score <- df$max_score + df$min_score +
    df$rangeFL_score + df$first_score + df$last_score + df$rangeMM_score
  
  return(df)
  
}

#' Getting plate scores
#'
#' @param df Stat summaries for NEW PLATES - well (note : this is a tibble in list)
#' 
#' A tibble: 1 × 8
# Groups:   plate_id [1]
#' plate_id       well Maximum Minimum First  Last Range(M-M) Range(F-L)
#' <chr>          <chr> <dbl>   <dbl> <dbl> <dbl>        <dbl>        <dbl>
#' 1 V0174416419V  H12   95.7   -274. -274. -95.7         178.         178.
#'  
#' @param qc_plate # the stats for the complete PBMC dataset - plate stats (note : this is a tibble in list)
#' 
# A tibble: 3 × 6
#  skim_variable n_missing complete_rate    p25   p75   iqr
#    <chr>             <int>         <dbl>  <dbl> <dbl> <dbl>
# 1 Maximum               0             1   20.4  177.  156.
# 2 Minimum               0             1 -610.  -367.  243.
# 3 Range(F-L)            0             1  459.   700.  241.
#' @return df with scores (note : Tibble is of type list)

# @examples get_plate_scores(df, qc_plate)
#'
#' 
# Returned dataframe contains the following columns: 
# #' Groups:   plate_id [1]
# plate_id (<chr>) : (example : V0174416419V)
# well (<chr>) : (example : A01)
# Maxiumum (<dbl>) : (example : 76.3)
# Minimum (<dbl>) : (example : -77.9)
# First (<dbl>) : (example: -77.9)
# Last (<dbl>) : (example: 66.3)
# Range(M-M) (<dbl>) (example: 154.)
# Range(F-L) (<dbl>) (example: 144.)
# max_score (<dbl>) (example: 2)
# min_score (<dbl>) (example: 2)
# rangeFL_score (<dbl>) (example: 1)
# first_score (<dbl>) (example: 2)
# last_score (<dbl>) (example: 2)
# rangeMM_score (<dbl>) (example: 2)
# total_score (<dbl>) (example: 11)

get_plate_scores <- function(df, qc_plate){
  df$max_score <- get_score(df, qc_plate, "Maximum")
  df$min_score <- get_score(df, qc_plate, "Minimum")
  df$rangeFL_score <- get_score(df, qc_plate, "Range(F-L)")
  df$total_score <- df$max_score + df$min_score +df$rangeFL_score 
  return(df)
}

#' Get all background quality scores in a list (well_scores, plate_scores, plots and colors).
#'
#' @param XFe96data_tibble A list with the new dataframe and the assay_info.
#' @param working_directory The project directory (which contains the .Rproj file)
#' @keywords Internal
#' @return background_qc : Get all background quality scores in a list (well_scores, plate_scores, plots and colors).
#' 
# example: print(background_qc) gives:
# $background_quality_list$well_scores
# A tibble: 4 × 2
# well         total_score
# <chr>          <dbl>
# 1 A01            11
# 2 A12             6
# 3 H01            16
# 4 H12             7

# background_quality_list$plate_scores
# A tibble:              1 × 2
# plate_id            total_score
# <chr>                  <dbl>
# 1 V0174416419V           7
# 

background_QC_1 <- function(XFe96data_tibble, working_directory){
  # to prevent no visible binding for global variable '<variable>'. notes, when using command devtools::check().
  # Variable(s) are now bound to object(s) and so the R CMD check has nothing to complain about. 
  group_by <- plate_id <- summarize <- dev_fromTarget <- Maximum <- Minimum <- well <- first <- last <- Last <- First <- ungroup <- select <- measurement <- summarise <- quantile <- unnest_wider <- left_join <- mutate <- X75 <- X25 <- ggplot <- aes <- geom_ribbon <- X50 <- whisker_up <- whisker_down <- geom_line <- geom_hline <- theme_classic <- labs <- scale_x_continuous <- tibble <- raw_data <- filePathSeahorse <- slice <- unnest <- geom_line_interactive <- group <- O2_em_corr <- scale_color_manual_interactive <- fileName <- date_run <- map <- bkgd_QC <- iqr <- NULL
  
  # adjusted skim function
  # skimr is designed to provide summary statistics about variables in data frames, tibbles, data tables and vectors. 
  # It is opinionated in its defaults, but easy to modify.
  
  my_skim <- skimr::skim_with(numeric = skimr::sfl(iqr = IQR, p0 = NULL,p50 = NULL,p100 = NULL,
                                                   mean = NULL,sd = NULL,hist = NULL))
  
  ########
  #read the reference dataset (PBMC data)
  bkgQC_PBMC_large <- readRDS(file = paste(working_directory, "/data/raw_data/bkgQC_PBMC_large.Rda", sep="")) #please note the missing "d" in filename
  #calculate plate stats
  plate_stats <- bkgQC_PBMC_large %>%
    group_by(plate_id) %>%
    summarize(Maximum = max(dev_fromTarget),
              Minimum = min(dev_fromTarget),
              'Range(F-L)' = Maximum-Minimum)
  
  #calculate well stats
  well_stats <- bkgQC_PBMC_large %>% 
    group_by(plate_id, well) %>% 
    summarize(Maximum = max(dev_fromTarget),
              Minimum = min(dev_fromTarget),
              First = first(dev_fromTarget),
              Last = last(dev_fromTarget),
              'Range(M-M)' = Maximum-Minimum,
              'Range(F-L)' = Last-First)
  
  #get the stats for the complete PBMC dataset - plate stats
  qc_plate <- my_skim(plate_stats, -plate_id) %>% 
    skimr::yank("numeric") %>% 
    tibble::as_tibble()
  
  #get the stats for the complete PBMC dataset - well stats
  qc_well <- well_stats %>% 
    ungroup() %>% 
    select(-plate_id, -well) %>% 
    my_skim() %>% 
    skimr::yank("numeric") %>% 
    tibble::as_tibble()
  ##########
  
  #make the overall QC plot from PBMC large data file 
  
  #calculate the quantiles
  quantiles_wells <- bkgQC_PBMC_large %>%
    group_by(measurement) %>% 
    summarise(q = list(quantile(dev_fromTarget))) %>% 
    unnest_wider(q)
  
  #rename the columns to avoid numeric column titles
  names(quantiles_wells) <- make.names(names(quantiles_wells))
  names(quantiles_wells) <- gsub("\\.", "", names(quantiles_wells))
  
  
  #calculate the iqt
  iqr_wells <- bkgQC_PBMC_large %>%
    group_by(measurement) %>% 
    summarise(iqr = IQR(dev_fromTarget))
  
  #build plot
  pbmc_bkgd_plot <- quantiles_wells %>% 
    left_join(iqr_wells, by= c("measurement")) %>% 
    mutate(whisker_up = X75 +1.5*iqr,
           whisker_down = X25-1.5*iqr) %>% 
    ggplot(aes(x = measurement))+
    geom_ribbon(aes(ymin = X50 - (X50-X25), ymax = X50 + (X75-X50)), fill = "grey70")+
    geom_ribbon(aes(ymin = whisker_up - (whisker_up-X75), ymax = whisker_up), fill = "grey90")+
    geom_ribbon(aes(ymin = whisker_down, ymax = whisker_down + (X25-whisker_down)), fill = "grey90")+
    geom_line(aes(y = X50),size = 1, linetype = "solid")+
    geom_line(aes(y = whisker_up),size = 0.3, linetype = "solid")+
    geom_line(aes(y = whisker_down),size = 0.3, linetype = "solid")+
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.4, color = "#D16103")+
    theme_classic()+
    labs(y = "(\u0394) Emission from target (AU)")+
    scale_x_continuous(breaks = c(1:18))
  ############
  
  #EXAMPLE INPUT
  #this is a dataset which includes three plates
  
  my_data_PBMC <- readRDS(file = paste(working_directory, "/data/raw_data/my_data_PBMC.Rda", sep=""))
  
  extra_info_my_data_PBMC_1 <- tibble(plate_id = "V0174416419V",
                                      file_name = "20191219 SciRep PBMCs donor A",
                                      date_run = lubridate::dmy_hm("19-12-2019 17:25"))
  
  extra_info_my_data_PBMC_2 <- tibble(plate_id = "V0194115019V",
                                      file_name = "20200110 SciRep PBMCs donor B",
                                      date_run = lubridate::dmy_hm("10-1-2020 16:59"))
  
  extra_info_my_data_PBMC_3 <- tibble(plate_id = "V0194715019V",
                                      file_name = "20200110 SciRep PBMCs donor C",
                                      date_run = lubridate::dmy_hm("15-1-2020 17:26"))
  
  extra_info_my_data_PBMC <- rbind(extra_info_my_data_PBMC_1,
                                   extra_info_my_data_PBMC_2,
                                   extra_info_my_data_PBMC_3)
  
  my_data_PBMC <- my_data_PBMC %>% 
    left_join(extra_info_my_data_PBMC, by = c("plate_id"))
  
  ############
  
  # #add one plate with 4 corner wells to plot
  # flnme <- "/cloud/project/data/vb211014_evelien.xlsx"
  # injscheme <- "HAP"
  # plate_df <- read_plate_data(flnme, injscheme)
  # XFe96data_tibble <- preprocess_plate_data_2(plate_df)
  
  one_plate <- XFe96data_tibble %>%
    select(plate_id, raw_data, filePathSeahorse) %>%
    slice(1) %>% # get first plate
    unnest(cols = c(raw_data)) %>%
    group_by(well, measurement) %>% 
    slice(1) %>% #get only first tick
    ungroup()
  
  custom.col <- c("#D16103","#4E84C4","#52854C","#C4961A",  "#FFDB6D", "#C4961A", "#F4EDCA") 
  
  bkgd_plot_xfe96data <- pbmc_bkgd_plot +
    geom_line_interactive(data = subset(one_plate, group == "Background"), 
                          mapping = aes(x = measurement, y = O2_em_corr-12500, group = well, color = well))+
    scale_color_manual_interactive(values = custom.col)+
    labs(title = paste0(one_plate$plate_id))
  bkgd_plot_xfe96data 
  
  
  # plate 1 in my_data_PBMC
  one_plate <- my_data_PBMC %>% 
    select(plate_id, raw_data, fileName, date_run) %>%
    slice(1) %>% # get first plate
    unnest(cols = c(raw_data)) %>% 
    group_by(well, measurement) %>% 
    slice(1) %>% #get only first tick
    ungroup()
  
  pbmc_bkgd_plot_1 <- pbmc_bkgd_plot +
    geom_line_interactive(data = subset(one_plate, group == "Background"), 
                          mapping = aes(x = measurement, y = O2_em_corr-12500, group = well, color = well))+
    scale_color_manual_interactive(values = custom.col)+
    labs(title = paste0(one_plate$plate_id, " -> ", one_plate$date_run))
  
  # plate 2 in my_data_PBMC
  one_plate <- my_data_PBMC %>% 
    select(plate_id, raw_data, fileName, date_run) %>%
    slice(2) %>% 
    unnest(cols = c(raw_data)) %>% 
    group_by(well, measurement) %>% 
    slice(1) %>% 
    ungroup()
  
  pbmc_bkgd_plot_2 <- pbmc_bkgd_plot +
    geom_line_interactive(data = subset(one_plate, group == "Background"), 
                          mapping = aes(x = measurement, y = O2_em_corr-12500, group = well, color = well))+
    scale_color_manual_interactive(values = custom.col)+
    labs(title = paste0(one_plate$plate_id, " -> ", one_plate$date_run))
  
  # plate 3 in my_data_PBMC
  one_plate <- my_data_PBMC %>% 
    select(plate_id, raw_data, fileName, date_run) %>%
    slice(3) %>% 
    unnest(cols = c(raw_data)) %>% 
    group_by(well, measurement) %>% 
    slice(1) %>% 
    ungroup()
  
  pbmc_bkgd_plot_3 <- pbmc_bkgd_plot +
    geom_line_interactive(data = subset(one_plate, group == "Background"), 
                          mapping = aes(x = measurement, y = O2_em_corr-12500, group = well, color = well))+
    scale_color_manual_interactive(values = custom.col)+
    labs(title = paste0(one_plate$plate_id, " -> ", one_plate$date_run))

  #specify which plate to use
  plate_number = 1
  
  #calculate background parameters for NEW PLATES
  bkgQC_PBMC_scirep <- XFe96data_tibble %>% 
    select(plate_id, raw_data) %>% 
    mutate(bkgd_QC = map(.x = raw_data, .f = ~{get_BKGD_auc(.x, "O2_em_corr", 12500)})) %>% 
    select(plate_id, bkgd_QC) %>% 
    unnest(cols = c(bkgd_QC)) 
  
  #calculate stat summaries for NEW PLATES - plate
  plate_stats_scirep <- bkgQC_PBMC_scirep %>%
    group_by(plate_id) %>%
    summarize(Maximum = max(dev_fromTarget),
              Minimum = min(dev_fromTarget),
              'Range(F-L)' = Maximum-Minimum)
  
  #calculate stat summaries for NEW PLATES - well
  well_stats_scirep <<- bkgQC_PBMC_scirep %>% 
    group_by(plate_id, well) %>% 
    summarize(Maximum = max(dev_fromTarget),
              Minimum = min(dev_fromTarget),
              First = first(dev_fromTarget),
              Last = last(dev_fromTarget),
              'Range(M-M)' = Maximum-Minimum,
              'Range(F-L)' = Last-First)
  
  #calculate scores for a plate
  well_scores <- get_well_scores(well_stats_scirep, qc_well) %>% 
    ungroup() %>%
    select(well,total_score) #minimum score = 6 -> max = 18
  
  # green: score = 6 - 9
  # orange: score = 10-12
  # red: score = 13_18
  
  #calculate scores for wells
  plate_scores <- get_plate_scores(plate_stats_scirep, qc_plate) %>% 
    select(plate_id, total_score) #minimum score = 3 -> max = 9
  
  # green: score = 3 - 4
  # orange: score = 5 - 6
  # red: score = 7 - 9
  
  background_quality_list <- list("well_scores" = well_scores, "plate_scores" = plate_scores)
  
  # Extract the well_scores and plate scores from the list.
  well_scores_tibble_list <- background_quality_list[[1]]
  plate_scores_tibble_list <- background_quality_list[[2]]
  qc_color_list <- get_bkg_quality_colors(well_scores_tibble_list, plate_scores_tibble_list)
  
  background_qc <- list("background_quality_list" = background_quality_list, 
                        "pbmc_bkgd_plot_1" = pbmc_bkgd_plot_1, 
                        "pbmc_bkgd_plot_2" = pbmc_bkgd_plot_2, 
                        "pbmc_bkgd_plot_3" = pbmc_bkgd_plot_3, 
                        "bkgd_plot_xfe96data" = bkgd_plot_xfe96data,
                        "qc_color_list" = qc_color_list)

  return(background_qc)
  
}

