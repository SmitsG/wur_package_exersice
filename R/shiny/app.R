### General information ####
# Rwave Shiny Script
# By: G.Smits
# Date: 14-04-2022 
# Description: Extra information about the App structure and functionality can be found within the README.md file.

### Libraries ####
# Import libraries used for the Shiny web application. 
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(validate)
### Get working directory ####
# Get the project directory of the rwave.Rproj script.
# The current absolute path contains /script/shiny/
working_directory <- getwd()
# Go 2 steps back in the absolute path, with ../..
working_directory <- setwd('../..')
# So you get the absolute path of the rwave.Rproj file.
# This path contains all directories with the nessecary functions inside. 
working_directory <- getwd()


# Source the libraries.
# Use the paste function to concatenate paths. 
source(paste(working_directory,"/R/funs_plots.R", sep=""))
source(paste(working_directory,"/R/funs_read_plate_data.R", sep=""))
source(paste(working_directory,"/R/funs_preprocess_plate_data_new.R", sep=""))
source(paste(working_directory,"/R/background_QC_module_forShiny.R", sep=""))


check_excel_sheet <- function(filePathSeahorse){
  sheets <- readxl::excel_sheets(filePathSeahorse)
  if ('Raw' %in% sheets) {
    "The Excel file contains a Raw sheet and is most likely a Seahorse file"
  } else {
    NULL
  }
}
  

#' Title ### function: bkg_quality_check (For PBMC Seahorse data) ####
#' A bkg_quality_check function, which adds a quality score to the background wells. 
#' The quality will be visualized within a quality chart, and quality boxes. 
#' High quality means less techniqual variation. Note: Because PBMC data is used as a reference, you will
#' only get a reliable results for imported PBMC data sets.
#'
#' @param input Inputs are what gives users a way to interact with a Shiny app.
#' @param output Shiny provides a family of functions that turn R objects into output for your user interface
#' @param XFe96data_tibble 
#'
#' @return
#' @export
#'
#' @examples bkg_quality_check(input, output, XFe96data_tibble)
bkg_quality_check <- function(input, output, XFe96data_tibble){
    # Get all background quality scores in a list (well_scores, plate_scores, plots and colors).
    background_qc <- background_QC_1(XFe96data_tibble, working_directory)
    # If the user clicks on the action button.
    observeEvent(input$bkgd_plot_xfe96data_action_button, {
        # A background quality plot plot will be visualized with ggiraph.
        output$plot_quality_xfe96data <- renderggiraph({
            gg_plot <- background_qc$bkgd_plot_xfe96data
            return(girafe(ggobj = gg_plot))
        })
    })
    # Quality visualisation of background wells with info boxes. 
    # Boxes are "red" (low quality), "yellow" (medium quality) or "green (high quality).
    output$wells_qc_1_1 <- renderInfoBox({
        infoBox(title = background_qc$qc_color_list$wells[1], 
                color = background_qc$qc_color_list$color[1], 
                icon = shiny::icon("chart-bar"))
    })
    output$wells_qc_2_1 <- renderInfoBox({
        infoBox(title = background_qc$qc_color_list$wells[2], 
                color = background_qc$qc_color_list$color[2], 
                icon = shiny::icon("chart-bar"))
    })
    output$wells_qc_3_1 <- renderInfoBox({
        infoBox(title = background_qc$qc_color_list$wells[3], 
                color = background_qc$qc_color_list$color[3], 
                icon = shiny::icon("chart-bar"))
    })
    output$wells_qc_4_1 <- renderInfoBox({
        infoBox(title = background_qc$qc_color_list$wells[4], 
                color = background_qc$qc_color_list$color[4], 
                icon = shiny::icon("chart-bar"))
    })
    
    # Quality visualisation of the plate with info boxes. 
    # Boxes are "red" (bad quality), "yellow" (medium quality) or "green (good quality).
    output$info_qc_plate <- renderInfoBox({
        infoBox(
            title = background_qc$qc_color_list$plate_id[1],
            color = background_qc$qc_color_list$plate_color[1],
            icon = shiny::icon("chart-bar")
        )
    })
}


shinyApp( ####
### UI : lay out the user interface of your app. ####
    # DashboarrdPagePlus creates a dashboard page for the Shiny app. 
    ui = dashboardPage( #####
        #Set header
        dashboardHeader(title = "Rwave"),
        # Set sidebar
        sidebar <- dashboardSidebar(
            # The sidebar contains multiple items...:
            sidebarMenu(
                # 1. Several MenuItems.
                menuItem("Import data",
                         tabName = "import",
                         icon = icon("file-import")
                ),
                menuItem("Data",
                         tabName = "data",
                         icon = icon("th")
                ),
                menuItem("Set parameters",
                         tabName = "parameters",
                         icon = icon("th")
                ),
                # Expandable 'Charts' menu (contains a submenu).
                menuItem("Charts",
                         icon = icon("bar-chart-o"),
                         startExpanded = TRUE,
                         menuSubItem("Raw Background",
                                     tabName = "raw_bkg",
                         ),
                         menuSubItem("Means Raw Background",
                                     tabName = "means_raw_bkg"
                         ),
                         menuSubItem("Auc facet Background",
                                     tabName = "auc_facet_bkg"
                         ),
                         menuSubItem("Group Emissions",
                                     tabName = "group_emissions"
                         ),
                         menuSubItem("Range Plot Emissions",
                                     tabName = "make_range_plot"
                         ),
                         menuSubItem("Group location",
                                     tabName = "group_location"
                         ),
                         # menuSubItem("Wells first ticks",
                         #             tabName = "allWells_firstTicks"
                         # ),
                         menuSubItem("Background Quality Control",
                                     tabName = "bkg_quality"
                         )
                ),
                menuItem("Q&A",
                         tabName = "question_answers",
                         icon = icon("question")
                )
            )
        ),
        dashboardBody(
            # Text at the top bar of the application.
            tags$head(tags$style(HTML(
                '.myClass {
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '
            ))),
            tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> ...... </span>\');
      })
     ')),
            
            # Force the scroll bar to appear.
            tags$style("html, body {overflow: visible !important;"),
            
            # When the user clicks on a tab,
            # the page will visualize the information within the following tab Items.
            tabItems(
                # The import data page.
                tabItem(
                    "import",
                    # Create a new row.
                    fluidRow(
                        # With a box inside of it.
                        box(
                            # Define box attributes.
                            width = 4,
                            title = "Run analysis",
                            status = "primary",
                            solidHeader = TRUE,
                            # Create input file for importing Excel Seahorse data.
                            fileInput(inputId = "file",
                                      label = "Select your Seahorse excel input file:",
                                      accept = c(
                                          ".xlsx",
                                          ".xls"
                                      )
                            ),
                            # Some extra information about the imported data file.
                            p("Class of `file_input`"),
                            verbatimTextOutput("file_input_class"),
                            p("Object"),
                            verbatimTextOutput("file_input_object"),
                            p("File Name"),
                            verbatimTextOutput("file_name")
                            

                        )
                    )
                ),
                # The data page, useful data from the experiment will be visualized.
                tabItem(
                    "data", "data",
                    renderTable("table"),
                    # Visualize different assay info information.
                    span(textOutput("assay_name")),
                    textOutput("assay_instrument_serial"),
                    textOutput("assay_cartridge_barcode"),
                    textOutput("assay_plate_id"),
                    textOutput("assay_date_run"),
                    dataTableOutput("table"),
                    textOutput("assay")
                ),
                # The parameter page, where the user can select global parameters.
                tabItem(
                    "parameters",
                    box(
                        width = 4,
                        title = "Set Parameters",
                        status = "primary",
                        solidHeader = TRUE,
                        # Parameters 1
                        injection_scheme <- selectInput("injection_scheme",
                                                        "Choose an injection scheme",
                                                        choices = c("HAP")
                        ),
                        # Set format of the parameter text boxes.
                        tags$head(tags$style(HTML(".selectize-input {
                                      height: 10px;
                                      width: 160px;
                                      font-size: 20px;}")))
                    ),
                    box(
                      width = 4,
                      title = "Parameters",
                      status = "primary",
                      solidHeader = TRUE,
                      raw_em_corr <- selectInput("raw_em_corr",
                                                 "Choose raw em corr",
                                                 choices = c("O2_em_corr", "pH_em_corr")
                      )
                )),
                # Chart page 1.
                tabItem(
                    "raw_bkg",
                    # Create a new row.
                    fluidRow(
                        # With boxes.
                        box(
                            width = 4,
                            title = "Plot Raw Background",
                            status = "primary",
                            solidHeader = TRUE,
                            box(
                                width = 12,
                                title = "Description",
                                status = "primary",
                                solidHeader = TRUE,
                                # Give the user information about the plot visualization.
                                tags$style(HTML(".man_made_class{color:#f2f205;
                                text-align: center;}")),
                                # Show plot Description in HTML format.
                                HTML(
                                  "<p> Visualizes the fluorescent emission (Au) of each background well against time. 
                                  The user can compare the Au of different background wells. 
                                  The fluorescence emission of the oxygen tension in the atmosphere is 151.7 mmHg. 
                                  This emission is set to 12500 Au during the calibration of the cartridge before the 
                                  performance of each experiment. A plot will be created when the user presses 
                                  the 'visualize Analysis' button. The dotted threshold line shows the Emission target
                                  of 12500 Au.</p>"
                                ),
                                # When the user clicks on the action button the Raw Background plot will be
                                # visualized.
                                actionButton("plot_raw_bkg_action_button",
                                             "Visualize Analysis",
                                             icon("play"),
                                             style = "color: #fff;
                             background-color: #337ab7; border-color: #2e6da4"
                                )
                            )
 
                            )
                        ),
                        # The Raw Background plot will be visualized in the main Panel.
                        mainPanel(
                            box(
                                width = 12,
                                title = "Plot Raw Background",
                                status = "primary",
                                solidHeader = TRUE,
                                ggiraphOutput("plot_raw_bkg")
                            )
                        
                    )
                ),
                # Chart page 2.
                tabItem(
                    "means_raw_bkg",
                    # Create a new row.
                    fluidRow(
                        # With boxes.
                        box(
                            width = 4,
                            title = "Means Raw Background",
                            status = "primary",
                            solidHeader = TRUE,
                            box(
                                width = 12,
                                title = "Description",
                                status = "primary",
                                solidHeader = TRUE,
                                # Show plot Description in HTML format.
                                HTML(
                                  "<p>Visualizes the mean fluorescence emission (Au) of each background well against time. 
                                  This will provide an overview of the average fluorescence emission of the different background wells. 
                                  When the user clicks on the <em>'Visualize Analysis'</em> button, a plot will be created. 
                                  The dotted threshold line shows the emission target of 12500 Au. 
                                  The Error Bars are used to visualize the variability of the plotted data.</p>"
                                ),
                                # When the user clicks on the action button the Means Raw Background plot will be
                                # visualized.
                                actionButton("plot_means_raw_bkg_action_button",
                                             "Visualize Analysis",
                                             icon("play"),
                                             style = "color: #fff;
                             background-color: #337ab7;
                             border-color: #2e6da4"
                                )
                            )

                        ),
                        # The Means Raw Background plot will be visualized in the main Panel.
                        mainPanel(
                            box(
                                width = 12,
                                title = "Means Raw Background",
                                status = "primary",
                                solidHeader = TRUE,
                                plotOutput("plot_means_raw_bkg")
                            )
                        )
                    )
                ),
                # Chart page 3.
                tabItem(
                    "auc_facet_bkg",
                    fluidRow(
                        box(
                            width = 4,
                            title = "Plot Auc Facet Background",
                            status = "primary",
                            solidHeader = TRUE,
                            box(
                                width = 12,
                                title = "Description",
                                status = "primary",
                                solidHeader = TRUE,
                                HTML(
                                  "<p>Visualizes the fluorescence emission (Au) of each background well and is based on the area under the curve (auc). 
                                  Multiple plots will be plotted. This allows for a comparison of the fluorescence emission between different background wells. 
                                  When the user clicks on the <em>'Visualize Analysis'</em>, a plot will be created. 
                                  There are two options. The first option will calculate the AUC for the first tick of each measurement. 
                                  The second option calculates the surface area of each measurement against the 12500 Au.</p>"
                                ),
                                actionButton("plot_auc_facet_bkg_action_button",
                                             "Visualize Analysis",
                                             icon("play"),
                                             style = "color: #fff;
                             background-color: #337ab7; border-color: #2e6da4"
                                )
                            ),
                            box(
                                width = 12,
                                title = "Parameters",
                                status = "primary",
                                solidHeader = TRUE,
                                # parameters 2
                                auc_var <- selectInput("auc_var",
                                                       "Choose aus var",
                                                       choices = c("auc", "auc2")
                                )
                            )
                        ),
                        mainPanel(
                            box(
                                width = 12,
                                title = "Auc Facet Background",
                                status = "primary",
                                solidHeader = TRUE,
                                ggiraphOutput("plot_auc_facet_bkg")
                            )
                        )
                    )
                ),
                # Chart page 4.
                tabItem(
                    "group_emissions",
                    fluidRow(
                        box(
                            width = 4,
                            title = "Plot Group Emissions",
                            status = "primary",
                            solidHeader = TRUE,
                            
                            # Style of the selectize dropdown menu.
                            tags$style(
                                type = "text/css", "
              .selectize-input {
              font-size: 10px;
              line-height: 10px;
              height: 50px;
              width: 400px;
              padding-top: 5px;}

              .selectize-dropdown {
              font-size: 10px;
              line-height: 10px;}
              "
                            ),
                            box(
                                width = 12,
                                title = "Description",
                                status = "primary",
                                solidHeader = TRUE,
                                HTML(
                                  "<p>Visualizes the fluorescence emission (Au) of each well for different Seahorse groups. 
                                  The first tick of every measurement will be visualized. 
                                  We can compare the difference in Au between Groups. 
                                  When the user presses the <em>'Visualize Analysis'</em> button, a plot will be created. 
                                  You can select different Seahorse Groups through the <em>'Multiselect'</em> dropdown menu, up to a maximum of 4. </p>"
                                ),
                                actionButton("plot_group_emissions_action_button",
                                             "Visualize Analysis",
                                             icon("play"),
                                             style =
                                                 "color: #fff;
                  background-color: #337ab7;
                  border-color: #2e6da4"
                                )
                            ),
                            box(
                                uiOutput("seahorse_dropdown_menu")
                                
                            )
                        ),
                        mainPanel(
                            box(
                                width = 12,
                                title = "Group Emissions",
                                status = "primary",
                                solidHeader = TRUE,
                                ggiraphOutput("plot_group_emissions")
                            )
                        )
                    )
                ),
                # Chart page 5.
                tabItem(
                    "make_range_plot",
                    fluidRow(
                        box(
                            width = 4,
                            title = "Make Range Plot",
                            status = "primary",
                            solidHeader = TRUE,
                            box(
                                width = 12,
                                title = "Description",
                                status = "primary",
                                solidHeader = TRUE,
                                HTML(
                                  "<p> When the user clicks on the action button the oxygen 
                  concentration (mmHg) of the background wells will be visualized within a 
                  range plot. This is done for each measurement. The range is the highest and 
                  lowest level of oxygen that is calculated. We set a threshold for these range 
                  value to get an impression of the quality of the range.
                  </p>"
                                  
                                ),
                                actionButton("make_range_plot_action_button",
                                             "Visualize Analysis",
                                             icon("play"),
                                             style =
                                                 "color: #fff;
                             background-color: #337ab7;
                             border-color: #2e6da4"
                                )
                            ),
                            box(
                                width = 12,
                                title = "Parameters",
                                status = "primary",
                                solidHeader = TRUE,
                  
                                # parameters 4
                                selectInput("param_to_plot",
                                            "Choose param to plot",
                                            choices = c("pH", "O2_mmHg")
                                )
                            )
                        ),
                        mainPanel(
                            box(
                                width = 12,
                                title = "Range plot",
                                status = "primary",
                                solidHeader = TRUE,
                                ggiraphOutput("make_range_plot")
                            )
                        )
                    )
                ),
                # Chart page 6.
                tabItem(
                    "group_location",
                    fluidRow(
                        box(
                            width = 4,
                            title = "Group location",
                            status = "primary",
                            solidHeader = TRUE,
                            box(
                                width = 12,
                                title = "Description",
                                status = "primary",
                                solidHeader = TRUE,
                                HTML(
                                  "<p>Visualizes the location of different Seahorse Groups on a Seahorse 96-wells plate. 
                                  Each group is represented with a different color. When the user presses the <em>'Visualize Analysis'</em> button, 
                                  a plot will be created. </p>"
                                ),
                                actionButton("group_location_action_button",
                                             "Visualize Analysis",
                                             icon("play"),
                                             style =
                                                 "color: #fff;
                             background-color: #337ab7;
                             border-color: #2e6da4"
                                )
                            )
                        ),
                        mainPanel(
                            box(
                                width = 12,
                                title = "Group location",
                                status = "primary",
                                solidHeader = TRUE,
                                ggiraphOutput("group_location")
                            )
                        )
                    )
                ),
                # Chart page 7.
                tabItem(
                    "allWells_firstTicks",
                    fluidRow(
                        box(
                            width = 4,
                            title = "All Wells first ticks",
                            status = "primary", solidHeader = TRUE,
                            box(
                                width = 12,
                                title = "Description",
                                status = "primary",
                                solidHeader = TRUE,
                                HTML(
                                    "<p>You can select different groups to show in the plot, up to
                  a maximum of 4. After you've selected the groups, you can click
                  on the <em>'visualize plot'</em> button to visualize.</p>"
                                ),
                                actionButton("plot_allWells_firstTicks_action_button",
                                             "Visualize Analysis",
                                             icon("play"),
                                             style = "
                             color: #fff;
                             background-color: #337ab7;
                             border-color: #2e6da4"
                                )
                            )
                        ),
                        mainPanel(
                            box(
                                width = 12,
                                title = "Plot All Wells First Ticks",
                                status = "primary",
                                solidHeader = TRUE,
                                ggiraphOutput("plot_allWells_firstTicks")
                            )
                        )
                    )
                ),
                # Chart page 8.
                tabItem(
                    "bkg_quality",
                    fluidRow(
                        box(
                            width = 4,
                            title = "(Background) Quality control",
                            status = "primary",
                            solidHeader = TRUE,
                            box(
                                width = 12,
                                title = "Description",
                                status = "primary",
                                solidHeader = TRUE,
                                # Give the user information about the plot visualization.
                                tags$style(HTML(".man_made_class{color:#f2f205;
                                text-align: center;}")),
                                HTML(
                                    "<p> Technical variation may cause differences in the target 
                  Emission (Au). Rwave will perform a quality check on data for 
                  background wells and plate. It measures if the Au of background 
                  wells is between a certain treshold. When the Au of the data is 
                  beyond the treshold, the reliablity of the experiment decreases, 
                  because technical variation is to aberrant. Therefore, a high amount 
                  of techniqual variation will result in lower quality of the data. 
                  PBMC background. Seahorse data is used as a reference. Seahorse 
                  background well/plate data of an experiment is compared to these 
                  reference data. The quality will be visualized within the Shiny App. 
                  Different colors are used: red (low quality), yellow (medium quality),
                  green (high quality).
                  </p>"
                                ),
                                actionButton("bkgd_plot_xfe96data_action_button",
                                             "Visualize Analysis",
                                             icon("play"),
                                             style = "
                             color: #fff;
                             background-color: #337ab7;
                             border-color: #2e6da4"
                                )
                            ),
                            # Shorten the info boxes.
                            tags$head(
                                tags$style(
                                    HTML(".info-box {min-height: 45px;}
                     .info-box-icon {height: 45px; line-height: 45px;}
                     .info-box-content {padding-top: 0px; padding-bottom: 0px;}")
                                )
                            ),
                            box(
                                width = 12,
                                title = "Data",
                                status = "primary",
                                solidHeader = TRUE,
                                
                                # Show quality of different wells.
                                fluidRow(
                                    infoBoxOutput(
                                        "wells_qc_1_1",
                                        width = 12
                                    )
                                ),
                                fluidRow(
                                    infoBoxOutput(
                                        "wells_qc_2_1",
                                        width = 12
                                    )
                                ),
                                fluidRow(
                                    infoBoxOutput(
                                        "wells_qc_3_1",
                                        width = 12
                                    )
                                ),
                                fluidRow(
                                    infoBoxOutput(
                                        "wells_qc_4_1",
                                        width = 12
                                    )
                                )
                            ),
                            box(
                                width = 12,
                                title = "Group quality",
                                status = "primary",
                                solidHeader = TRUE,
                                fluidRow(
                                    # Show plate quality.
                                    infoBoxOutput(
                                        "info_qc_plate",
                                        width = 12
                                    )
                                )
                            )
                        ),
                        # Show quality Plot.
                        mainPanel(
                            box(
                                width = 12,
                                title = "Qualitycheck",
                                status = "primary",
                                solidHeader = TRUE,
                                ggiraphOutput("plot_quality_xfe96data")
                            )
                        )
                    )
                ),
                tabItem("question_answers", "Q&A")
            )
        )
    ),
    
    
###Shiny Server####
    # It builds a web server specifically designed to host Shiny apps.
    # With Shiny Server you can host your apps in a controlled environment.
    server = function(input, output, session) {
      # By default, Shiny limits file uploads to 5MB per file. You can modify this limit by using the shiny.maxRequestSize option.
      # Used for 'import file' function at the 'import' tab.
      options(shiny.maxRequestSize = 30 * 1024^2)
      
      ### 1. The app will run a test data set (20191219_SciRep_PBMCs_donor_A.xlsx) when the applications starts. ####
        flnme <- file.path(working_directory, paste("data/raw_data/20191219_SciRep_PBMCs_donor_A.xlsx"))
        injscheme <- "HAP"
        # read the plate data and return plate information (plate_df).
        plate_df <- read_plate_data(flnme, injscheme)
        # Perform preprocessing on the received plate information.
        XFe96data_tibble <- preprocess_plate_data_2(plate_df)
        # Put the raw data sheet in a variable called XFe96data. 
        # as.data.frame is a functions to check if an object is a data frame, or coerce it if possible.
        XFe96data <- as.data.frame(XFe96data_tibble$raw_data)
        # Perorm a background quality check on the preprocessed data. (Currently only reliable for PBMC data)
        # High quality scores means there is less techniqual variation in your analysis.
        bkg_quality_check(input, output, XFe96data_tibble)
        # Combine dataframe. Same as XFe96data_tibble$raw_data, changes the 'list with tibble' structure [[1]] to a normal data frame structure. 
        # So you can work with it more easily.
        total_df <- do.call(rbind.data.frame, XFe96data_tibble$raw_data)
        # Extracts assay info list. Results will be visualized in the 'Data' tab.
        assay_info <- XFe96data_tibble[[4]]
        # Get the first value of the assay info list (which is list with tibble)
        assay_info <- assay_info[[1]]
        # Get the O2_targetEmission
        O2_targetEmission <- assay_info[[12]]
        # Get the pH_targetEmission
        pH_targetEmission <- assay_info[[11]]
        
        # Get most relevant assay info.
        
        
        # Assay info: Cartridge Barcode
        output$assay_cartridge_barcode <- renderText({
          paste(
            "Cartridge Barcode: ",
            paste(assay_info$cartridge_barcode,
                  collapse = ", "
            )
          )
        })
        
        # Assay info: Instrument Serial
        output$assay_instrument_serial <- renderText({
          paste(
            "Instrument Serial: ",
            paste(assay_info$instrument_serial, collapse = ", ")
          )
        })
        
        # Assay info: Assay Name
        output$assay_name <- renderText({
          paste(
            "Assay Name: ",
            paste(assay_info$assay_name, collapse = ", ")
          )
        })
        
        # Assay info: Assay Plate ID
        output$assay_plate_id <- renderText({
          paste(
            "Assay Plate ID: ",
            paste(assay_info$plate_id, collapse = ", ")
          )
        })
        
        # Assay info: Date run
        output$assay_date_run <- renderText({
          paste(
            "Date run: ",
            paste(assay_info$date_run, collapse = ", ")
          )
        })
      
        observeEvent(input$plot_raw_bkg_action_button, {
          # 'Charts' tab > 'Raw Background' : plot the raw bkg.
          # Visualizes the fluorescence emission (AU) of each background well, versus time.
          output$plot_raw_bkg <- renderggiraph({
            gg_plot <- plot_raw_BKGD(
              input$raw_em_corr,
              total_df,
              O2_targetEmission,
              pH_targetEmission,
              input$file$name
            )
            return(girafe(
              ggobj = gg_plot
            ))
          })
        })
       
        observeEvent(input$plot_means_raw_bkg_action_button, {
          # 'Charts' tab > 'Means Raw Background'
          # Visualizes the mean fluorescence (AU) of different background wells, versus time. 
          output$plot_means_raw_bkg <- renderPlot({
            gg_plot <- plot_raw_BKGD_means(
              input$raw_em_corr,
              total_df,
              O2_targetEmission,
              pH_targetEmission,
              input$file$name
            )
            return(gg_plot)
          })
        })
        
        observeEvent(input$plot_auc_facet_bkg_action_button, {
          # 'Charts' tab > 'Auc facet background'
          # Visualizes the fluorescence emission (Au) trough an area under the curve (auc) plot, for each background well.
          output$plot_auc_facet_bkg <- renderggiraph({
            gg_plot <- plot_BKGD_auc_facet(
              input$raw_em_corr,
              input$auc_var,
              total_df,
              O2_targetEmission,
              pH_targetEmission,
              input$file$name
            )
            return(girafe(ggobj = gg_plot))
          })
        })
        
        
        # Seahorse_group_dropdown (Outside the visualize function to get a
        # dropdown menu from the beginning.) Used for seahorse group function.
        output$seahorse_dropdown_menu <- renderUI({
          selectizeInput("seahorse_group_dropdown",
                         "Multi-select",
                         choices = XFe96data$group,
                         multiple = TRUE,
                         options = list(maxItems = 4)
          )
          
        })
        
        observeEvent(input$plot_group_emissions_action_button, {
        
          seahorse_group <- reactive({
            # Required fields.
            req(input$seahorse_group_dropdown)
            
            XFe96data <- XFe96data %>%
              filter(group %in% input$seahorse_group_dropdown)
          })
          
          # Put seahorse_group function in variable.
          seahorse_groups <- seahorse_group()
          
          # Plot group emissions - first tick emission (facet_wrap)
          output$plot_group_emissions <- renderggiraph({
            # 'Charts' tab > 'Group Emissions'
            #### The fluorescence emission (Au) of each well is displayed. (Per Seahorse Group and for each measurement).
            # This makes it possible to compare the fluorescence emission between different groups. # Up to 4 groups can be selected.
            # Note: Only the first tick of each measurement will be visualized.
            gg_plot <- plot_group_emissions(
              input$raw_em_corr,
              seahorse_groups,
              O2_targetEmission,
              pH_targetEmission,
              input$file$name
            )
            return(girafe(
              ggobj = gg_plot,
              width_svg = (20),
              height_svg = (10)
            ))
          })
        })
          
        observeEvent(input$make_range_plot_action_button, {
          # 'Charts' tab > 'Range Plot Emissions' (facet_wrap)
          # Visualizes the oxygen concentration of the background wells using a range plot (per measurement).
          # This is the only graph where O2 (in mmHg) is plotted. The range is the highest and lowest level of O2 that is measured per measurement. 
          # Using this range, we can indicate limits that are acceptable, so that we can  say whether the range is good or not.
          output$make_range_plot <- renderggiraph({
            gg_plot <- make_range_plot(
              input$raw_em_corr,
              total_df,
              input$file$name
            )
            return(girafe(
              ggobj = gg_plot
            ))
          })
        })
        
        observeEvent(input$group_location_action_button, {
          # 'Charts' tab > 'Heatmap' 
          # Provides the user with an image of the location of different Seahorse groups, on a 96-well Seahorse plate.
          output$group_location <- renderggiraph({
            levels(as.factor(XFe96data$group))
            gg_plot <- heatmap_groupMaker(XFe96data, input$file$name)
            return(girafe(
              ggobj = gg_plot,
              width_svg = (60),
              height_svg = (40)
            ))
          })
        })
        
        observeEvent(input$plot_allWells_firstTicks_action_button, {

          # Plot all wells first tick raw (with highlight on tooltip)
          output$plot_allWells_firstTicks <- renderggiraph({
            gg_plot <- plot_allWells_firstTicks(
              input$raw_em_corr,
              XFe96data,
              O2_targetEmission,
              pH_targetEmission,
              input$file$name
            )
            return(girafe(
              ggobj = gg_plot, width_svg = (20), height_svg = (10)
            ))
          })
        })
        
      

      
      ### 2. Analysis will run when the user imports a Seahorse .xlsx dataset. (located at the import data tab) ####
        
        observeEvent(input$file, {
          
            # Some general info about the imported file. Note: only readable for programmers so could be removed.
            output$file_input_class <- 
                renderPrint({
                    class(input$file)
                })
            
            output$file_input_object <-
                renderPrint({
                    input$file
                })
            
            output$file_name <- 
                renderPrint({
                    input$file$name
                })
            
            
            plate_df <- read_plate_data(input$file$datapath, input$injection_scheme)
            
            # preprocess the plate data. # <<- updates the XFe96data_tibble variable.
            XFe96data_tibble <<- preprocess_plate_data_2(plate_df)
            
            bkg_quality_check(input, output, XFe96data_tibble)
            
            # change tibble to dataframe? This doesn't really work because typof shows
            # XFe96data as a list.
            XFe96data <<- as.data.frame(XFe96data_tibble$raw_data)
            
            #### Background check and plots.

            total_df <- do.call(rbind.data.frame, XFe96data_tibble$raw_data)
            
            # Extracts assay info list.
            assay_info <- XFe96data_tibble[[4]]
            # Get the first value of the assay info list (which is list with tibble)
            assay_info <- assay_info[[1]]
            
            # Get the O2_targetEmission
            O2_targetEmission <- assay_info[[12]]
            # Get the pH_targetEmission
            pH_targetEmission <- assay_info[[11]]
            
            # Get most relevant assay info.
            output$assay_cartridge_barcode <- renderText({
              paste(
                "Cartridge Barcode: ",
                paste(assay_info$cartridge_barcode,
                      collapse = ", "
                )
              )
            })
            
            output$assay_instrument_serial <- renderText({
              
              paste(
                "Instrument Serial: ",
                
                paste(assay_info$instrument_serial, collapse = ", ")
              )
            })
            
            output$assay_name <- renderText({
              paste(
                "Assay Name: ",
                paste(assay_info$assay_name, collapse = ", ")
              )
            })
            
            output$assay_plate_id <- renderText({
              paste(
                "Assay Plate ID: ",
                paste(assay_info$plate_id, collapse = ", ")
              )
            })
            
            output$assay_date_run <- renderText({
              paste(
                "Date run: ",
                paste(XFe96data_tibble$date, collapse = ", ")
              )
            })
            
            # Seahorse_group_dropdown (Outside the visualize function to get a
            # dropdown menu from the beginning.)
            output$seahorse_dropdown_menu <- renderUI({
              selectizeInput("seahorse_group_dropdown",
                             "Multi-select",
                             choices = XFe96data$group,
                             multiple = TRUE,
                             options = list(maxItems = 4)
              )
              
            })
            
            observeEvent(input$plot_raw_bkg_action_button, {
              # plot the raw bkg.
              output$plot_raw_bkg <- renderggiraph({
                gg_plot <- plot_raw_BKGD(
                  input$raw_em_corr,
                  total_df,
                  O2_targetEmission,
                  pH_targetEmission,
                  input$file$name
                )
                return(girafe(
                  ggobj = gg_plot
                ))
              })
            })
            
            observeEvent(input$plot_means_raw_bkg_action_button, {
              # plot_means_raw_bkg
              output$plot_means_raw_bkg <- renderPlot({
                gg_plot <- plot_raw_BKGD_means(
                  input$raw_em_corr,
                  total_df,
                  O2_targetEmission,
                  pH_targetEmission,
                  input$file$name
                )
                return(gg_plot)
              })
            })
            
            observeEvent(input$plot_auc_facet_bkg_action_button, {
              # auc_facet_bkg
              output$plot_auc_facet_bkg <- renderggiraph({
                gg_plot <- plot_BKGD_auc_facet(
                  input$raw_em_corr,
                  input$auc_var,
                  total_df,
                  O2_targetEmission,
                  pH_targetEmission,
                  input$file$name
                )
                return(girafe(ggobj = gg_plot))
              })
            })
            # observeEvent(input$make_range_plot_action_button, {
            #   # range plot emission (facet_wrap)
            #   output$make_range_plot <- renderggiraph({
            #     # input$param_to_plot of raw_em_corr ???
            #     gg_plot <- make_range_plot(
            #       input$raw_em_corr,
            #       total_df,
            #       input$file$name
            #     )
            #     return(girafe(
            #       ggobj = gg_plot,
            #       # list(opts_sizing(rescale = FALSE)),
            #       # width_svg = (12),
            #       # height_svg = (8)
            #     ))
            #   })
            # })
            
            observeEvent(input$group_location_action_button, {
              # Plate layout heatmap
              output$group_postion <- renderggiraph({
                levels(as.factor(XFe96data$group))
                gg <- heatmap_groupMaker(XFe96data, input$file$name)
                return(girafe(
                  ggobj = gg,
                  width_svg = (60),
                  height_svg = (40)
                ))
              })
            })
            
            
            # Note: when switching from PBMC data set to Evelien data set you get an error.
            # Error: 'to' must be a finite number
            # This error should be fixed in feature updates.
            # If you don't include this code, you don't get an error, but the plots will only update when you choose your Seahorse groups
            # in the dropdown menu.
            observeEvent(input$plot_group_emissions_action_button, {
              
              #### Check the seahorse_group_dropdown and store given data.
              seahorse_group <- reactive({
                # Required fields.
                req(input$seahorse_group_dropdown)
                
                XFe96data <- XFe96data %>%
                  filter(group %in% input$seahorse_group_dropdown)
              })
              
              # Put seahorse_group function in variable.
              seahorse_groups <- seahorse_group()
              
              # Plot group emissions - first tick emission (facet_wrap)
              output$plot_group_emissions <- renderggiraph({
                gg_plot <- plot_group_emissions(
                  input$raw_em_corr,
                  seahorse_groups,
                  O2_targetEmission,
                  pH_targetEmission,
                  input$file$name
                )
                return(girafe(
                  ggobj = gg_plot,
                  width_svg = (20),
                  height_svg = (10)
                ))
              })
            })
            
        })
  
    }
)


