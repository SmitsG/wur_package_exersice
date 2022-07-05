
<!-- README.md is generated from README.Rmd. Please edit that file -->

# WurPackageExersice

<!-- badges: start -->

<!-- badges: end -->

The goal of WurPackageExersice is to …

## Installation

You can install the development version of WurPackageExersice from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SmitsG/wur_package_exersice")
```

# devtools::document() Use roxygen to document a package.

Set the working directory.

This is the directory where the .Rproj file is located.

Note: Our RWave App does this automaticly. (Through App.R)

``` r
working_directory <- getwd()
print(working_directory)
#> [1] "/home/xiang/Documents/Documents Gerwin/Projects/r_exersice/wur_package_exersice"
```

Source the functions and libraries.

Scripts are sourced to make the functions of our .R scripts available.

Note: This is also done at our App.R.

``` r
# Source the libraries.
# Use the paste function to concatenate paths. 
source(paste(working_directory,"/R/funs_plots.R", sep=""))
source(paste(working_directory,"/R/funs_read_plate_data.R", sep=""))
source(paste(working_directory,"/R/funs_preprocess_plate_data_new.R", sep=""))
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
#> ✔ tibble  3.1.7     ✔ dplyr   1.0.9
#> ✔ tidyr   1.2.0     ✔ stringr 1.4.0
#> ✔ readr   2.1.2     ✔ forcats 0.5.1
#> ✔ purrr   0.3.4
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
source(paste(working_directory,"/R/background_QC_module_forShiny.R", sep=""))
```

The .asyr Wave file is imported into seahorseanalytics.agilent.com,
followed by export to Excel.

The xls data files are used as input file.

Set the file path to the Seahorse Excel file.

The ‘import data’ tab from the Rwave Shiny app allows you to import your
Excel data sheet.

The location of the Excel file will be used in our
analysis.

``` r
filePathSeahorse <- file.path(working_directory, paste("data/raw_data/20191219_SciRep_PBMCs_donor_A.xlsx"))
injscheme <- "HAP"
print(filePathSeahorse)
#> [1] "/home/xiang/Documents/Documents Gerwin/Projects/r_exersice/wur_package_exersice/data/raw_data/20191219_SciRep_PBMCs_donor_A.xlsx"
```

Read all the nessecary Seahorse information from the Excel file,

including data from the ‘Assay Configuration’ and ‘Raw’ sheet.

A tibble with these data will be returned.

``` r
plate_df <- read_plate_data(filePathSeahorse, injscheme)
#> New names:
#> New names:
#> • `` -> `...1`
#> Warning: All formats failed to parse. No formats found.
#> New names:
#> New names:
#> New names:
#> • `` -> `...1`
print(plate_df)
#> $XFe96data
#> # A tibble: 13,824 × 21
#>    Measurement  Tick Well  Group     TimeStamp `Well Temperat…` `Env. Temperat…`
#>          <dbl> <dbl> <chr> <chr>     <chr>                <dbl>            <dbl>
#>  1           1     0 A01   Backgrou… 00:37:14              37.0             36.0
#>  2           1     0 A02   50.000    00:37:14              37.0             36.0
#>  3           1     0 A03   100.000   00:37:14              37.0             36.0
#>  4           1     0 A04   100.000   00:37:14              37.0             36.0
#>  5           1     0 A05   150.000   00:37:14              37.0             36.0
#>  6           1     0 A06   200.000   00:37:14              37.0             36.0
#>  7           1     0 A07   150.000   00:37:14              37.0             36.0
#>  8           1     0 A08   200.000   00:37:14              37.0             36.0
#>  9           1     0 A09   250.000   00:37:14              37.0             36.0
#> 10           1     0 A10   250.000   00:37:14              37.0             36.0
#> # … with 13,814 more rows, and 14 more variables: `O2 is Valid` <chr>,
#> #   `O2 (mmHg)` <dbl>, `O2 Light Emission` <dbl>, `O2 Dark Emission` <dbl>,
#> #   `O2 Ref Light` <dbl>, `O2 Ref Dark` <dbl>, `O2 Corrected Em.` <dbl>,
#> #   `pH Is Valid` <chr>, pH <dbl>, `pH Light` <dbl>, `pH Dark` <dbl>,
#> #   `pH Ref Light` <dbl>, `pH Ref Dark` <dbl>, `pH Corrected Em.` <dbl>
#> 
#> $assay_info
#> # A tibble: 1 × 21
#>       F0   V_C Tau_AC Tau_W Tau_C Tau_P    KSV    gain1 gain2  pH_0
#>    <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl>    <dbl> <dbl> <dbl>
#> 1 54025.  9.15    746   296   246  60.9 0.0219 0.000405  1.01   7.4
#> # … with 11 more variables: pH_targetEmission <dbl>, O2_targetEmission <dbl>,
#> #   plate_id <chr>, cartridge_barcode <chr>, date_run <dttm>, assay_name <chr>,
#> #   instrument_serial <chr>, O2_0_mmHg <dbl>, O2_0_mM <dbl>,
#> #   norm_available <lgl>, excel_OCR_background_corrected <lgl>
#> 
#> $injection_info
#> # A tibble: 12 × 3
#>    measurement interval injection       
#>          <int>    <dbl> <chr>           
#>  1           1        1 Baseline        
#>  2           2        1 Baseline        
#>  3           3        1 Baseline        
#>  4           4        2 FCCP            
#>  5           5        2 FCCP            
#>  6           6        2 FCCP            
#>  7           7        3 AM/ROT          
#>  8           8        3 AM/ROT          
#>  9           9        3 AM/ROT          
#> 10          10        4 Monensin/Hoechst
#> 11          11        4 Monensin/Hoechst
#> 12          12        4 Monensin/Hoechst
#> 
#> $pH_calibration
#> # A tibble: 96 × 2
#>    well  pH_cal_em
#>    <chr>     <dbl>
#>  1 A01       29785
#>  2 A02       29700
#>  3 A03       29062
#>  4 A04       29806
#>  5 A05       29473
#>  6 A06       29534
#>  7 A07       29617
#>  8 A08       29366
#>  9 A09       29554
#> 10 A10       29961
#> # … with 86 more rows
#> 
#> $norm_info
#> # A tibble: 96 × 2
#>    well  cell_n
#>    <chr>  <dbl>
#>  1 A01        0
#>  2 A02    32472
#>  3 A03   114732
#>  4 A04    83567
#>  5 A05   153510
#>  6 A06   162266
#>  7 A07   185274
#>  8 A08   184938
#>  9 A09   272018
#> 10 A10   263477
#> # … with 86 more rows
#> 
#> $bufferfactor_info
#> # A tibble: 96 × 2
#>    well  bufferfactor
#>    <chr>        <dbl>
#>  1 A01            0  
#>  2 A02            2.4
#>  3 A03            2.4
#>  4 A04            2.4
#>  5 A05            2.4
#>  6 A06            2.4
#>  7 A07            2.4
#>  8 A08            2.4
#>  9 A09            2.4
#> 10 A10            2.4
#> # … with 86 more rows
#> 
#> $OCR_from_excel
#> # A tibble: 1,152 × 8
#>    measurement well  group time_wave OCR_wave OCR_wave_bc ECAR_wave ECAR_wave_bc
#>          <dbl> <chr> <chr>     <dbl>    <dbl>       <dbl>     <dbl>        <dbl>
#>  1           1 A01   Back…      1.31     0         -0.597      0         -0.0825
#>  2           1 A02   50.0…      1.31     6.22       5.63       2.90       2.82  
#>  3           1 A03   100.…      1.31    26.6       26.0        5.87       5.79  
#>  4           1 A04   100.…      1.31    21.4       20.8        4.40       4.32  
#>  5           1 A05   150.…      1.31     3.08       2.49      12.4       12.3   
#>  6           1 A06   200.…      1.31    41.1       40.5        8.98       8.90  
#>  7           1 A07   150.…      1.31    39.5       38.9        9.27       9.19  
#>  8           1 A08   200.…      1.31    40.4       39.8        4.75       4.67  
#>  9           1 A09   250.…      1.31    58.8       58.2        7.39       7.30  
#> 10           1 A10   250.…      1.31    59.4       58.8        6.88       6.80  
#> # … with 1,142 more rows
#> 
#> $filePathSeahorse
#> [1] "/home/xiang/Documents/Documents Gerwin/Projects/r_exersice/wur_package_exersice/data/raw_data/20191219_SciRep_PBMCs_donor_A.xlsx"
```

Next run the preprocessing on the plate\_df file.

The preprocess\_plate\_data function preprocesses the data by: \*
changing columns names \* adding new time columns \* adding injection
info columns \* add plate\_id column \* calculating background data \*
calculating raw pH emission data

A tibble is returned. This tibble 7 columns: \# plate\_id (Barcode from
the ‘Assay Configuration’ sheet from the Seahorse Excel file) \#
filePathSeahorse (path to Excel file) \# date - (derived from Assay
Configuration Sheet of the Seahorse Excel file. \# assay\_info -
(tibble) with information from the ‘Assay Configuration’ sheet from the
Seahorse Excel file. \# raw\_info - (tibble) with information from the
‘Raw’ sheet from the Seahorse Excel file. \# rate\_data - With
information from the ‘Rate’ sheet from the Seahorse Excel file.

``` r
XFe96data_tibble <- preprocess_plate_data_2(plate_df)
#> `summarise()` has grouped output by 'measurement'. You can override using the
#> `.groups` argument.
#> # A tibble: 1 × 7
#> # Groups:   plate_id [1]
#>   plate_id     filePathSeahorse    date   assay_info        injection_info   
#>   <chr>        <chr>               <dttm> <list>            <list>           
#> 1 V0174416419V /home/xiang/Docume… NA     <tibble [1 × 21]> <tibble [12 × 3]>
#> # … with 2 more variables: raw_data <list>, rate_data <list>
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
