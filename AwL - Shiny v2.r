# ACTwatch Lite Data Cleaning and Analysis
# R Shiny Application



library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(data.table)
library(readxl)
library(writexl)
library(here)
library(fs)
library(labelled)
library(lubridate)
library(glue)
library(rmarkdown)


# Source functions
tryCatch({
  source(here("Scripts", "00_functions.R"))
}, error = function(e) {
  message("Warning: Could not source 00_functions.R - ", e$message)
})

country <- "TST"
year <- 2040

while(sink.number() > 0) sink()

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "ACTwatch Lite Data Cleaning and Analysis",
    tags$li(class = "dropdown",
            style = "margin: 8px 15px 8px 0px;",
            tags$div(
              style = "color: white; padding: 5px 10px;",
              tags$label("Country:", style = "font-size: 12px; margin-right: 5px;"),
              textInput("country", 
                        label = NULL, 
                        value = "TST",
                        placeholder = "Country",
                        width = "120px")
            )
    ),
    tags$li(class = "dropdown",
            style = "margin: 8px 15px 8px 0px;",
            tags$div(
              style = "color: white; padding: 5px 10px;",
              tags$label("Year:", style = "font-size: 12px; margin-right: 5px;"),
              numericInput("year", 
                           label = NULL, 
                           value = 2040,
                           min = 2000,
                           max = 2050,
                           step = 1,
                           width = "100px")
            )
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("", tabName = "blank", icon = NULL, selected = FALSE),
                menuItem("Data Cleaning", tabName = "cleaning", icon = icon("broom"), selected = TRUE),
                menuItem("Data Management", tabName = "management", icon = icon("database")),
                menuItem("Data Analysis", tabName = "analysis", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .status-complete { color: green; font-weight: bold; }
        .status-pending { color: gray; }
        .status-running { color: blue; font-weight: bold; }
        .status-error { color: red; font-weight: bold; }
        
        /* Hide the blank spacer menu item but keep its space */
        .sidebar-menu li:first-child {
          visibility: hidden;
          height: 44px;
          pointer-events: none;
        }
        
        /* Ensure sidebar menu items are clickable */
        .sidebar-menu li a {
          pointer-events: auto !important;
          cursor: pointer !important;
        }
      "))
    ),
    tabItems(
      # Data Cleaning Tab
      tabItem(tabName = "cleaning",
              fluidRow(
                column(6,
                       box(
                         title = "Data Cleaning Scripts", status = "success", solidHeader = TRUE,
                         width = 12,
                         
                         actionButton("run_all_cleaning", "Run All Cleaning Scripts", 
                                      class = "btn-success btn-lg", icon = icon("play-circle"),
                                      style = "margin-bottom: 20px; width: 100%;"),
                         hr(),
                         
                         h4("1. Product List Cleaning", 
                            span(id = "status_product_cleaning", class = "status-pending", "\u25CF")),
                         actionButton("run_product_cleaning", "Run Product List Cleaning", 
                                      class = "btn-success", icon = icon("play")),
                         br(), br(),
                         
                         h4("2. Outlet Cleaning", 
                            span(id = "status_outlet_cleaning", class = "status-pending", "\u25CF")),
                         actionButton("run_outlet_cleaning", "Run Outlet Cleaning", 
                                      class = "btn-success", icon = icon("play")),
                         br(), br(),
                         
                         h4("3. Antimalarial Cleaning", 
                            span(id = "status_antimalarial_cleaning", class = "status-pending", "\u25CF")),
                         actionButton("run_antimalarial_cleaning", "Run Antimalarial Cleaning", 
                                      class = "btn-success", icon = icon("play")),
                         br(), br(),
                         
                         h4("4. RDT Cleaning", 
                            span(id = "status_rdt_cleaning", class = "status-pending", "\u25CF")),
                         actionButton("run_rdt_cleaning", "Run RDT Cleaning", 
                                      class = "btn-success", icon = icon("play")),
                         br(), br(),
                         
                         h4("5. Supplier Cleaning", 
                            span(id = "status_supplier_cleaning", class = "status-pending", "\u25CF")),
                         actionButton("run_supplier_cleaning", "Run Supplier Cleaning", 
                                      class = "btn-success", icon = icon("play"))
                       )
                ),
                column(6,
                       box(
                         title = "Output", status = "success", solidHeader = TRUE,
                         width = 12, height = "600px",
                         verbatimTextOutput("cleaning_output")
                       )
                )
              )
      ),
      
      # Data Management Tab
      tabItem(tabName = "management",
              fluidRow(
                column(6,
                       box(
                         title = "Data Management Scripts", status = "warning", solidHeader = TRUE,
                         width = 12,
                         
                         actionButton("run_all_management", "Run All Management Scripts", 
                                      class = "btn-warning btn-lg", icon = icon("play-circle"),
                                      style = "margin-bottom: 20px; width: 100%;"),
                         hr(),
                         
                         h4("1. Long Dataset", 
                            span(id = "status_long_dataset", class = "status-pending", "\u25CF")),
                         actionButton("run_long_dataset", "Run Long Dataset Creation", 
                                      class = "btn-warning", icon = icon("play")),
                         br(), br(),
                         
                         h4("2. Weights", 
                            span(id = "status_weights", class = "status-pending", "\u25CF")),
                         actionButton("run_weights", "Run Weights", 
                                      class = "btn-warning", icon = icon("play")),
                         br(), br(),
                         
                         h4("3. Denominators", 
                            span(id = "status_denoms", class = "status-pending", "\u25CF")),
                         actionButton("run_denoms", "Run Denominators", 
                                      class = "btn-warning", icon = icon("play")),
                         br(), br(),
                         
                         h4("4. Outlet Categories", 
                            span(id = "status_outlet_categories", class = "status-pending", "\u25CF")),
                         actionButton("run_outlet_categories", "Run Outlet Categories", 
                                      class = "btn-warning", icon = icon("play")),
                         br(), br(),
                         
                         h4("5. Blood Test Categories", 
                            span(id = "status_bloodtest_categories", class = "status-pending", "\u25CF")),
                         actionButton("run_bloodtest_categories", "Run Blood Test Categories", 
                                      class = "btn-warning", icon = icon("play")),
                         br(), br(),
                         
                         h4("6. Antimalarial Categories", 
                            span(id = "status_antimalarial_categories", class = "status-pending", "\u25CF")),
                         actionButton("run_antimalarial_categories", "Run Antimalarial Categories", 
                                      class = "btn-warning", icon = icon("play")),
                         br(), br(),
                         
                         h4("7. Stocking", 
                            span(id = "status_stocking", class = "status-pending", "\u25CF")),
                         actionButton("run_stocking", "Run Stocking", 
                                      class = "btn-warning", icon = icon("play")),
                         br(), br(),
                         
                         h4("8. AETD", 
                            span(id = "status_aetd", class = "status-pending", "\u25CF")),
                         actionButton("run_aetd", "Run AETD", 
                                      class = "btn-warning", icon = icon("play")),
                         br(), br(),
                         
                         h4("9. Macro and Prices", 
                            span(id = "status_macro_prices", class = "status-pending", "\u25CF")),
                         actionButton("run_macro_prices", "Run Macro and Prices", 
                                      class = "btn-warning", icon = icon("play")),
                         br(), br(),
                         
                         h4("10. Volume", 
                            span(id = "status_volume", class = "status-pending", "\u25CF")),
                         actionButton("run_volume", "Run Volume", 
                                      class = "btn-warning", icon = icon("play")),
                         br(), br(),
                         
                         h4("11. Provider", 
                            span(id = "status_provider", class = "status-pending", "\u25CF")),
                         actionButton("run_provider", "Run Provider", 
                                      class = "btn-warning", icon = icon("play")),
                         br(), br(),
                         
                         h4("12. Final Datasets", 
                            span(id = "status_finaldatasets", class = "status-pending", "\u25CF")),
                         actionButton("run_finaldatasets", "Run Final Datasets", 
                                      class = "btn-warning", icon = icon("play")),
                         br(), br(),
                         
                         h4("13. Sensitivity Analyses", 
                            span(id = "status_sensitivity", class = "status-pending", "\u25CF")),
                         actionButton("run_sensitivity", "Run Sensitivity Analyses", 
                                      class = "btn-warning", icon = icon("play"))
                       )
                ),
                column(6,
                       box(
                         title = "Output", status = "warning", solidHeader = TRUE,
                         width = 12, height = "600px",
                         verbatimTextOutput("management_output")
                       )
                )
              )
      ),
      
      # Data Analysis Tab
      tabItem(tabName = "analysis",
              tabsetPanel(
                id = "analysis_tabs",
                
                # Market Composition Subtab
                tabPanel("Market Composition",
                         fluidRow(
                           column(6,
                                  box(
                                    title = "Market Composition Indicators", status = "info", solidHeader = TRUE,
                                    width = 12,
                                    
                                    h4("Indicator 1.1: Antimalarial-Stocking Outlets", 
                                       span(id = "status_indicator_1_1", class = "status-pending", "\u25CF")),
                                    p("Distribution of outlet types among outlets with antimalarials in stock", style = "font-size: 11px; color: #666;"),
                                    actionButton("run_indicator_1_1", "Run Indicator 1.1", 
                                                 class = "btn-info", icon = icon("play")),
                                    br(), br(),
                                    
                                    h4("Indicator 1.2: Malaria Blood-Testing Outlets", 
                                       span(id = "status_indicator_1_2", class = "status-pending", "\u25CF")),
                                    p("Distribution of outlet types among outlets with blood testing available", style = "font-size: 11px; color: #666;"),
                                    actionButton("run_indicator_1_2", "Run Indicator 1.2", 
                                                 class = "btn-info", icon = icon("play"))
                                  )
                           ),
                           column(6,
                                  box(
                                    title = "Output", status = "info", solidHeader = TRUE,
                                    width = 12, height = "500px",
                                    verbatimTextOutput("analysis_output_composition")
                                  )
                           )
                         )
                ),
                
                # Availability Subtab
                tabPanel("Availability",
                         fluidRow(
                           column(6,
                                  box(
                                    title = "Availability Indicators", status = "info", solidHeader = TRUE,
                                    width = 12,
                                    
                                    h4("Indicator 2.1: Antimalarials in All Outlets", 
                                       span(id = "status_indicator_2_1", class = "status-pending", "\u25CF")),
                                    p("Proportion of all enumerated outlets with antimalarials in stock", style = "font-size: 11px; color: #666;"),
                                    actionButton("run_indicator_2_1", "Run Indicator 2.1", 
                                                 class = "btn-info", icon = icon("play")),
                                    br(), br(),
                                    
                                    h4("Indicator 2.2: Antimalarials by Type", 
                                       span(id = "status_indicator_2_2", class = "status-pending", "\u25CF")),
                                    p("Proportion of AM-stocking outlets with specific types in stock", style = "font-size: 11px; color: #666;"),
                                    actionButton("run_indicator_2_2", "Run Indicator 2.2", 
                                                 class = "btn-info", icon = icon("play")),
                                    br(), br(),
                                    
                                    h4("Indicator 2.3: Blood Testing in All Outlets", 
                                       span(id = "status_indicator_2_3", class = "status-pending", "\u25CF")),
                                    p("Proportion of all enumerated outlets with blood testing available", style = "font-size: 11px; color: #666;"),
                                    actionButton("run_indicator_2_3", "Run Indicator 2.3", 
                                                 class = "btn-info", icon = icon("play")),
                                    br(), br(),
                                    
                                    h4("Indicator 2.4: Blood Testing in AM-Stocking Outlets", 
                                       span(id = "status_indicator_2_4", class = "status-pending", "\u25CF")),
                                    p("Proportion of AM-stocking outlets with blood testing available", style = "font-size: 11px; color: #666;"),
                                    actionButton("run_indicator_2_4", "Run Indicator 2.4", 
                                                 class = "btn-info", icon = icon("play"))
                                  )
                           ),
                           column(6,
                                  box(
                                    title = "Output", status = "info", solidHeader = TRUE,
                                    width = 12, height = "500px",
                                    verbatimTextOutput("analysis_output_availability")
                                  )
                           )
                         )
                ),
                
                # Sales Volume Subtab
                tabPanel("Sales Volume",
                         fluidRow(
                           column(6,
                                  box(
                                    title = "Sales Volume Indicators", status = "info", solidHeader = TRUE,
                                    width = 12,
                                    
                                    h4("Indicator 3.1: Median AM Sales (All Outlets)", 
                                       span(id = "status_indicator_3_1", class = "status-pending", "\u25CF")),
                                    p("Median AETDs sold in previous week, all AM-stocking outlets", style = "font-size: 11px; color: #666;"),
                                    actionButton("run_indicator_3_1", "Run Indicator 3.1", 
                                                 class = "btn-info", icon = icon("play")),
                                    br(), br(),
                                    
                                    h4("Indicator 3.2: Median AM Sales (Outlets with Sales)", 
                                       span(id = "status_indicator_3_2", class = "status-pending", "\u25CF")),
                                    p("Median AETDs sold in previous week, outlets with any sales", style = "font-size: 11px; color: #666;"),
                                    actionButton("run_indicator_3_2", "Run Indicator 3.2", 
                                                 class = "btn-info", icon = icon("play")),
                                    br(), br(),
                                    
                                    h4("Indicator 3.3: Median Blood Test Volume (All Outlets)", 
                                       span(id = "status_indicator_3_3", class = "status-pending", "\u25CF")),
                                    p("Median blood tests conducted/sold in previous week", style = "font-size: 11px; color: #666;"),
                                    actionButton("run_indicator_3_3", "Run Indicator 3.3", 
                                                 class = "btn-info", icon = icon("play")),
                                    br(), br(),
                                    
                                    h4("Indicator 3.4: Median Blood Test Volume (Outlets with Sales)", 
                                       span(id = "status_indicator_3_4", class = "status-pending", "\u25CF")),
                                    p("Median blood tests conducted/sold, outlets with any sales", style = "font-size: 11px; color: #666;"),
                                    actionButton("run_indicator_3_4", "Run Indicator 3.4", 
                                                 class = "btn-info", icon = icon("play"))
                                  )
                           ),
                           column(6,
                                  box(
                                    title = "Output", status = "info", solidHeader = TRUE,
                                    width = 12, height = "500px",
                                    verbatimTextOutput("analysis_output_volume")
                                  )
                           )
                         )
                ),
                
                # Market Share Subtab
                tabPanel("Market Share",
                         fluidRow(
                           column(6,
                                  box(
                                    title = "Market Share Indicators", status = "info", solidHeader = TRUE,
                                    width = 12,
                                    
                                    h4("Indicator 4.1: Market Share of Antimalarials", 
                                       span(id = "status_indicator_4_1", class = "status-pending", "\u25CF")),
                                    p("Proportion of AETDs sold by outlet and antimalarial type", style = "font-size: 11px; color: #666;"),
                                    actionButton("run_indicator_4_1", "Run Indicator 4.1", 
                                                 class = "btn-info", icon = icon("play")),
                                    br(), br(),
                                    
                                    h4("Indicator 4.2: Market Share of Blood Testing", 
                                       span(id = "status_indicator_4_2", class = "status-pending", "\u25CF")),
                                    p("Proportion of blood tests sold by outlet and test type", style = "font-size: 11px; color: #666;"),
                                    actionButton("run_indicator_4_2", "Run Indicator 4.2", 
                                                 class = "btn-info", icon = icon("play")),
                                    br(), br(),
                                    
                                    h4("Indicator 4.3: Market Share by Brand/Manufacturer", 
                                       span(id = "status_indicator_4_3", class = "status-pending", "\u25CF")),
                                    p("Proportion of antimalarials sold by outlet and top brands", style = "font-size: 11px; color: #666;"),
                                    actionButton("run_indicator_4_3", "Run Indicator 4.3", 
                                                 class = "btn-info", icon = icon("play"))
                                  )
                           ),
                           column(6,
                                  box(
                                    title = "Output", status = "info", solidHeader = TRUE,
                                    width = 12, height = "500px",
                                    verbatimTextOutput("analysis_output_marketshare")
                                  )
                           )
                         )
                ),
                
                # Pricing Subtab
                tabPanel("Pricing",
                         fluidRow(
                           column(6,
                                  box(
                                    title = "Pricing Indicators", status = "info", solidHeader = TRUE,
                                    width = 12,
                                    
                                    h4("Indicator 5.1a: Retail Price - Tablet AETDs", 
                                       span(id = "status_indicator_5_1a", class = "status-pending", "\u25CF")),
                                    p("Median retail price of AETD for tablet formulations", style = "font-size: 11px; color: #666;"),
                                    actionButton("run_indicator_5_1a", "Run Indicator 5.1a", 
                                                 class = "btn-info", icon = icon("play")),
                                    br(), br(),
                                    
                                    h4("Indicator 5.2a: Retail Price - Pre-packaged ACTs", 
                                       span(id = "status_indicator_5_2a", class = "status-pending", "\u25CF")),
                                    p("Median retail price of selected pre-packaged therapy", style = "font-size: 11px; color: #666;"),
                                    actionButton("run_indicator_5_2a", "Run Indicator 5.2a", 
                                                 class = "btn-info", icon = icon("play")),
                                    br(), br(),
                                    
                                    h4("Indicator 5.3: Retail Price - Blood Testing", 
                                       span(id = "status_indicator_5_3", class = "status-pending", "\u25CF")),
                                    p("Median retail price including consultation/service fees", style = "font-size: 11px; color: #666;"),
                                    actionButton("run_indicator_5_3", "Run Indicator 5.3", 
                                                 class = "btn-info", icon = icon("play")),
                                    br(), br(),
                                    
                                    h4("Indicator 6.1b: Purchase Price - AM AETDs", 
                                       span(id = "status_indicator_6_1b", class = "status-pending", "\u25CF")),
                                    p("Median purchase price of AETD from suppliers", style = "font-size: 11px; color: #666;"),
                                    actionButton("run_indicator_6_1b", "Run Indicator 6.1b", 
                                                 class = "btn-info", icon = icon("play")),
                                    br(), br(),
                                    
                                    h4("Indicator 6.2: Purchase Price - RDTs", 
                                       span(id = "status_indicator_6_2", class = "status-pending", "\u25CF")),
                                    p("Median purchase price of RDTs from suppliers", style = "font-size: 11px; color: #666;"),
                                    actionButton("run_indicator_6_2", "Run Indicator 6.2", 
                                                 class = "btn-info", icon = icon("play"))
                                  )
                           ),
                           column(6,
                                  box(
                                    title = "Output", status = "info", solidHeader = TRUE,
                                    width = 12, height = "500px",
                                    verbatimTextOutput("analysis_output_pricing")
                                  )
                           )
                         )
                ),
                
                # Stock Outs Subtab
                tabPanel("Stock Outs",
                         fluidRow(
                           column(6,
                                  box(
                                    title = "Stock Out Indicators", status = "info", solidHeader = TRUE,
                                    width = 12,
                                    
                                    h4("Indicator 7.1 & 7.2: Stock Outs", 
                                       span(id = "status_indicator_7_1_7_2", class = "status-pending", "\u25CF")),
                                    p("Stock outs of antimalarials and RDTs", style = "font-size: 11px; color: #666;"),
                                    actionButton("run_indicator_7_1_7_2", "Run Indicator 7.1 & 7.2", 
                                                 class = "btn-info", icon = icon("play"))
                                  )
                           ),
                           column(6,
                                  box(
                                    title = "Output", status = "info", solidHeader = TRUE,
                                    width = 12, height = "500px",
                                    verbatimTextOutput("analysis_output_stockouts")
                                  )
                           )
                         )
                )
              )
      )
    )
  )
)

# Define Server  
server <- function(input, output, session) {
  
  # Helper function to update status
  update_status <- function(step_id, status) {
    if (status == "running") {
      runjs(sprintf("$('#status_%s').removeClass('status-pending status-complete status-error').addClass('status-running').text('\u27F3');", step_id))
    } else if (status == "complete") {
      runjs(sprintf("$('#status_%s').removeClass('status-pending status-running status-error').addClass('status-complete').text('\u2713');", step_id))
    } else if (status == "error") {
      runjs(sprintf("$('#status_%s').removeClass('status-pending status-running status-complete').addClass('status-error').text('\u2717');", step_id))
    } else {
      runjs(sprintf("$('#status_%s').removeClass('status-running status-complete status-error').addClass('status-pending').text('\u25CF');", step_id))
    }
  }
  
  # Helper function to run R scripts from subfolders
  run_script <- function(subfolder, script_name, country, year) {
    tryCatch({
      # Set global variables
      assign("country", country, envir = .GlobalEnv)
      assign("year", year, envir = .GlobalEnv)
      
      # Construct script path with subfolder
      script_path <- file.path("Scripts", subfolder, paste0(script_name, ".R"))
      
      # Check if script exists
      if (!file.exists(script_path)) {
        return(list(success = FALSE, message = paste("Error: Script not found at", script_path)))
      }
      
      # Source the script and capture output
      output_text <- capture.output(source(script_path, echo = FALSE))
      
      if (length(output_text) > 0) {
        return(list(success = TRUE, message = paste(output_text, collapse = "\n")))
      } else {
        return(list(success = TRUE, message = paste("Script completed successfully:", script_name)))
      }
      
    }, error = function(e) {
      return(list(success = FALSE, message = paste("Error running", script_name, ":", e$message)))
    })
  }
  
  # Helper function to run RMarkdown files
  run_rmarkdown <- function(subfolder, rmd_name, country, year, tabver = "v1") {
    tryCatch({
      # Construct RMD path
      rmd_path <- file.path("Scripts", subfolder, paste0(rmd_name, ".Rmd"))
      
      # Check if file exists
      if (!file.exists(rmd_path)) {
        return(list(success = FALSE, message = paste("Error: RMarkdown file not found at", rmd_path)))
      }
      
      # Extract indicator info
      if (!exists("extract_indicator_info", mode = "function")) {
        return(list(success = FALSE, message = "Error: extract_indicator_info function not found. Check 00_functions.R"))
      }
      
      info <- extract_indicator_info(rmd_path)
      
      # Create output directory
      output_dir <- here("Results", paste("Indicator", info$indicator_id))
      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
      
      # Generate output filename
      output_file <- paste0("Indicator ", info$indicator_id, "_", 
                            country, "_", year, ".docx")
      
      # Get the project root directory
      project_root <- here()
      
      # Run in separate R session to avoid graphics device conflicts with Shiny
      result <- callr::r(
        function(rmd_path, output_file, output_dir, country, year, tabver, project_root) {
          # Set working directory
          setwd(project_root)
          
          # Load required libraries (must match what RMarkdown needs)
          library(here)
          library(rmarkdown)
          library(dplyr)
          library(srvyr)
          library(data.table)
          library(stringr)
          library(ggplot2)
          library(forcats)
          library(openxlsx)
          
          # Source functions file to make functions available
          tryCatch({
            source(here("Scripts", "00_functions.R"))
          }, error = function(e) {
            stop("Error sourcing 00_functions.R: ", e$message)
          })
          
          # Set global variables
          assign("country", country, envir = .GlobalEnv)
          assign("year", year, envir = .GlobalEnv)
          assign("tabver", tabver, envir = .GlobalEnv)
          
          # Render the document with error capture
          tryCatch({
            rmarkdown::render(
              rmd_path,
              params = list(
                country = country,
                year = year,
                tabver = tabver
              ),
              output_file = output_file,
              output_dir = output_dir,
              quiet = FALSE
            )
            return(list(success = TRUE))
          }, error = function(e) {
            return(list(success = FALSE, error = e$message, trace = paste(capture.output(traceback()), collapse = "\n")))
          })
        },
        args = list(
          rmd_path = rmd_path,
          output_file = output_file,
          output_dir = output_dir,
          country = country,
          year = year,
          tabver = tabver,
          project_root = project_root
        ),
        show = TRUE  # Show output in console for debugging
      )
      
      # Check if render was successful
      if (!is.null(result$success) && !result$success) {
        return(list(success = FALSE, message = paste("Render error:", result$error, "\n\nTrace:", result$trace)))
      }
      
      success_msg <- paste0(
        "Successfully generated: ", output_file, "\n",
        "Location: ", output_dir, "\n",
        "Indicator ID: ", info$indicator_id, "\n",
        "File suffix: ", info$file_suffix
      )
      
      return(list(success = TRUE, message = success_msg))
      
    }, error = function(e) {
      return(list(success = FALSE, message = paste("Error rendering", rmd_name, ":", e$message)))
    })
  }
  
  # Check inputs function
  check_inputs <- function() {
    if (input$country == "" || is.na(input$year)) {
      return("Please enter country and year before running scripts.")
    }
    return(NULL)
  }
  
  # Run All Cleaning Scripts
  observeEvent(input$run_all_cleaning, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$cleaning_output <- renderText(error_msg)
      return()
    }
    
    disable("run_all_cleaning")
    
    cleaning_scripts <- list(
      list(id = "product_cleaning", script = "01_product_list_cleaning", name = "Product List Cleaning"),
      list(id = "outlet_cleaning", script = "02_outlet_cleaning", name = "Outlet Cleaning"),
      list(id = "antimalarial_cleaning", script = "03_antimalarial_cleaning", name = "Antimalarial Cleaning"),
      list(id = "rdt_cleaning", script = "04_rdt_cleaning", name = "RDT Cleaning"),
      list(id = "supplier_cleaning", script = "05_supplier_cleaning", name = "Supplier Cleaning")
    )
    
    all_output <- ""
    success <- TRUE
    
    for (script_info in cleaning_scripts) {
      update_status(script_info$id, "running")
      output$cleaning_output <- renderText(paste(all_output, "\n\n=== Running:", script_info$name, "==="))
      
      result <- run_script("1_Cleaning", script_info$script, input$country, input$year)
      
      if (result$success) {
        update_status(script_info$id, "complete")
        all_output <- paste(all_output, "\n\n=== Completed:", script_info$name, "===\n", result$message)
      } else {
        update_status(script_info$id, "error")
        all_output <- paste(all_output, "\n\n=== Error in:", script_info$name, "===\n", result$message)
        success <- FALSE
        break
      }
    }
    
    output$cleaning_output <- renderText(all_output)
    enable("run_all_cleaning")
  })
  
  # Run All Management Scripts
  observeEvent(input$run_all_management, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$management_output <- renderText(error_msg)
      return()
    }
    
    disable("run_all_management")
    
    management_scripts <- list(
      list(id = "long_dataset", script = "01_long_dataset", name = "Long Dataset"),
      list(id = "weights", script = "02_weights", name = "Weights"),
      list(id = "denoms", script = "03_denoms", name = "Denominators"),
      list(id = "outlet_categories", script = "04_outlet_categories", name = "Outlet Categories"),
      list(id = "bloodtest_categories", script = "05_bloodtest_categories", name = "Blood Test Categories"),
      list(id = "antimalarial_categories", script = "06_antimalarial_categories", name = "Antimalarial Categories"),
      list(id = "stocking", script = "07_stocking", name = "Stocking"),
      list(id = "aetd", script = "08_aetd", name = "AETD"),
      list(id = "macro_prices", script = "09_macro_and_prices", name = "Macro and Prices"),
      list(id = "volume", script = "10_volume", name = "Volume"),
      list(id = "provider", script = "11_provider", name = "Provider"),
      list(id = "finaldatasets", script = "12_finaldatasets", name = "Final Datasets"),
      list(id = "sensitivity", script = "13_sensitivity_analyses", name = "Sensitivity Analyses")
    )
    
    all_output <- ""
    success <- TRUE
    
    for (script_info in management_scripts) {
      update_status(script_info$id, "running")
      output$management_output <- renderText(paste(all_output, "\n\n=== Running:", script_info$name, "==="))
      
      result <- run_script("2_Management", script_info$script, input$country, input$year)
      
      if (result$success) {
        update_status(script_info$id, "complete")
        all_output <- paste(all_output, "\n\n=== Completed:", script_info$name, "===\n", result$message)
      } else {
        update_status(script_info$id, "error")
        all_output <- paste(all_output, "\n\n=== Error in:", script_info$name, "===\n", result$message)
        success <- FALSE
        break
      }
    }
    
    output$management_output <- renderText(all_output)
    enable("run_all_management")
  })
  
  # Individual Cleaning Observers (existing code continues...)
  observeEvent(input$run_product_cleaning, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$cleaning_output <- renderText(error_msg)
      return()
    }
    disable("run_product_cleaning")
    update_status("product_cleaning", "running")
    output$cleaning_output <- renderText("Processing Product List Cleaning...")
    result <- run_script("1_Cleaning", "01_product_list_cleaning", input$country, input$year)
    if (result$success) {
      update_status("product_cleaning", "complete")
    } else {
      update_status("product_cleaning", "error")
    }
    output$cleaning_output <- renderText(result$message)
    enable("run_product_cleaning")
  })
  
  observeEvent(input$run_outlet_cleaning, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$cleaning_output <- renderText(error_msg)
      return()
    }
    disable("run_outlet_cleaning")
    update_status("outlet_cleaning", "running")
    output$cleaning_output <- renderText("Processing Outlet Cleaning...")
    result <- run_script("1_Cleaning", "02_outlet_cleaning", input$country, input$year)
    if (result$success) {
      update_status("outlet_cleaning", "complete")
    } else {
      update_status("outlet_cleaning", "error")
    }
    output$cleaning_output <- renderText(result$message)
    enable("run_outlet_cleaning")
  })
  
  observeEvent(input$run_antimalarial_cleaning, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$cleaning_output <- renderText(error_msg)
      return()
    }
    disable("run_antimalarial_cleaning")
    update_status("antimalarial_cleaning", "running")
    output$cleaning_output <- renderText("Processing Antimalarial Cleaning...")
    result <- run_script("1_Cleaning", "03_antimalarial_cleaning", input$country, input$year)
    if (result$success) {
      update_status("antimalarial_cleaning", "complete")
    } else {
      update_status("antimalarial_cleaning", "error")
    }
    output$cleaning_output <- renderText(result$message)
    enable("run_antimalarial_cleaning")
  })
  
  observeEvent(input$run_rdt_cleaning, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$cleaning_output <- renderText(error_msg)
      return()
    }
    disable("run_rdt_cleaning")
    update_status("rdt_cleaning", "running")
    output$cleaning_output <- renderText("Processing RDT Cleaning...")
    result <- run_script("1_Cleaning", "04_rdt_cleaning", input$country, input$year)
    if (result$success) {
      update_status("rdt_cleaning", "complete")
    } else {
      update_status("rdt_cleaning", "error")
    }
    output$cleaning_output <- renderText(result$message)
    enable("run_rdt_cleaning")
  })
  
  observeEvent(input$run_supplier_cleaning, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$cleaning_output <- renderText(error_msg)
      return()
    }
    disable("run_supplier_cleaning")
    update_status("supplier_cleaning", "running")
    output$cleaning_output <- renderText("Processing Supplier Cleaning...")
    result <- run_script("1_Cleaning", "05_supplier_cleaning", input$country, input$year)
    if (result$success) {
      update_status("supplier_cleaning", "complete")
    } else {
      update_status("supplier_cleaning", "error")
    }
    output$cleaning_output <- renderText(result$message)
    enable("run_supplier_cleaning")
  })
  
  # Individual Management Observers (existing code...)
  observeEvent(input$run_long_dataset, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$management_output <- renderText(error_msg)
      return()
    }
    disable("run_long_dataset")
    update_status("long_dataset", "running")
    output$management_output <- renderText("Processing Long Dataset...")
    result <- run_script("2_Management", "01_long_dataset", input$country, input$year)
    if (result$success) {
      update_status("long_dataset", "complete")
    } else {
      update_status("long_dataset", "error")
    }
    output$management_output <- renderText(result$message)
    enable("run_long_dataset")
  })
  
  observeEvent(input$run_weights, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$management_output <- renderText(error_msg)
      return()
    }
    disable("run_weights")
    update_status("weights", "running")
    output$management_output <- renderText("Processing Weights...")
    result <- run_script("2_Management", "02_weights", input$country, input$year)
    if (result$success) {
      update_status("weights", "complete")
    } else {
      update_status("weights", "error")
    }
    output$management_output <- renderText(result$message)
    enable("run_weights")
  })
  
  observeEvent(input$run_denoms, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$management_output <- renderText(error_msg)
      return()
    }
    disable("run_denoms")
    update_status("denoms", "running")
    output$management_output <- renderText("Processing Denominators...")
    result <- run_script("2_Management", "03_denoms", input$country, input$year)
    if (result$success) {
      update_status("denoms", "complete")
    } else {
      update_status("denoms", "error")
    }
    output$management_output <- renderText(result$message)
    enable("run_denoms")
  })
  
  observeEvent(input$run_outlet_categories, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$management_output <- renderText(error_msg)
      return()
    }
    disable("run_outlet_categories")
    update_status("outlet_categories", "running")
    output$management_output <- renderText("Processing Outlet Categories...")
    result <- run_script("2_Management", "04_outlet_categories", input$country, input$year)
    if (result$success) {
      update_status("outlet_categories", "complete")
    } else {
      update_status("outlet_categories", "error")
    }
    output$management_output <- renderText(result$message)
    enable("run_outlet_categories")
  })
  
  observeEvent(input$run_bloodtest_categories, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$management_output <- renderText(error_msg)
      return()
    }
    disable("run_bloodtest_categories")
    update_status("bloodtest_categories", "running")
    output$management_output <- renderText("Processing Blood Test Categories...")
    result <- run_script("2_Management", "05_bloodtest_categories", input$country, input$year)
    if (result$success) {
      update_status("bloodtest_categories", "complete")
    } else {
      update_status("bloodtest_categories", "error")
    }
    output$management_output <- renderText(result$message)
    enable("run_bloodtest_categories")
  })
  
  observeEvent(input$run_antimalarial_categories, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$management_output <- renderText(error_msg)
      return()
    }
    disable("run_antimalarial_categories")
    update_status("antimalarial_categories", "running")
    output$management_output <- renderText("Processing Antimalarial Categories...")
    result <- run_script("2_Management", "06_antimalarial_categories", input$country, input$year)
    if (result$success) {
      update_status("antimalarial_categories", "complete")
    } else {
      update_status("antimalarial_categories", "error")
    }
    output$management_output <- renderText(result$message)
    enable("run_antimalarial_categories")
  })
  
  observeEvent(input$run_stocking, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$management_output <- renderText(error_msg)
      return()
    }
    disable("run_stocking")
    update_status("stocking", "running")
    output$management_output <- renderText("Processing Stocking...")
    result <- run_script("2_Management", "07_stocking", input$country, input$year)
    if (result$success) {
      update_status("stocking", "complete")
    } else {
      update_status("stocking", "error")
    }
    output$management_output <- renderText(result$message)
    enable("run_stocking")
  })
  
  observeEvent(input$run_aetd, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$management_output <- renderText(error_msg)
      return()
    }
    disable("run_aetd")
    update_status("aetd", "running")
    output$management_output <- renderText("Processing AETD...")
    result <- run_script("2_Management", "08_aetd", input$country, input$year)
    if (result$success) {
      update_status("aetd", "complete")
    } else {
      update_status("aetd", "error")
    }
    output$management_output <- renderText(result$message)
    enable("run_aetd")
  })
  
  observeEvent(input$run_macro_prices, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$management_output <- renderText(error_msg)
      return()
    }
    disable("run_macro_prices")
    update_status("macro_prices", "running")
    output$management_output <- renderText("Processing Macro and Prices...")
    result <- run_script("2_Management", "09_macro_and_prices", input$country, input$year)
    if (result$success) {
      update_status("macro_prices", "complete")
    } else {
      update_status("macro_prices", "error")
    }
    output$management_output <- renderText(result$message)
    enable("run_macro_prices")
  })
  
  observeEvent(input$run_volume, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$management_output <- renderText(error_msg)
      return()
    }
    disable("run_volume")
    update_status("volume", "running")
    output$management_output <- renderText("Processing Volume...")
    result <- run_script("2_Management", "10_volume", input$country, input$year)
    if (result$success) {
      update_status("volume", "complete")
    } else {
      update_status("volume", "error")
    }
    output$management_output <- renderText(result$message)
    enable("run_volume")
  })
  
  observeEvent(input$run_provider, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$management_output <- renderText(error_msg)
      return()
    }
    disable("run_provider")
    update_status("provider", "running")
    output$management_output <- renderText("Processing Provider...")
    result <- run_script("2_Management", "11_provider", input$country, input$year)
    if (result$success) {
      update_status("provider", "complete")
    } else {
      update_status("provider", "error")
    }
    output$management_output <- renderText(result$message)
    enable("run_provider")
  })
  
  observeEvent(input$run_finaldatasets, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$management_output <- renderText(error_msg)
      return()
    }
    disable("run_finaldatasets")
    update_status("finaldatasets", "running")
    output$management_output <- renderText("Processing Final Datasets...")
    result <- run_script("2_Management", "12_finaldatasets", input$country, input$year)
    if (result$success) {
      update_status("finaldatasets", "complete")
    } else {
      update_status("finaldatasets", "error")
    }
    output$management_output <- renderText(result$message)
    enable("run_finaldatasets")
  })
  
  observeEvent(input$run_sensitivity, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$management_output <- renderText(error_msg)
      return()
    }
    disable("run_sensitivity")
    update_status("sensitivity", "running")
    output$management_output <- renderText("Processing Sensitivity Analyses...")
    result <- run_script("2_Management", "13_sensitivity_analyses", input$country, input$year)
    if (result$success) {
      update_status("sensitivity", "complete")
    } else {
      update_status("sensitivity", "error")
    }
    output$management_output <- renderText(result$message)
    enable("run_sensitivity")
  })
  
  # Data Analysis Observers - ALL INDICATORS
  # Composition
  observeEvent(input$run_indicator_1_1, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$analysis_output_composition <- renderText(error_msg)
      return()
    }
    disable("run_indicator_1_1")
    update_status("indicator_1_1", "running")
    output$analysis_output_composition <- renderText("Processing Indicator 1.1...")
    
    result <- run_rmarkdown("3_Analysis", "Indicator 1.1", 
                            input$country, input$year, "v1")
    
    if (result$success) {
      update_status("indicator_1_1", "complete")
      output$analysis_output_composition <- renderText(paste("\u2713 Complete\n\n", result$message))
    } else {
      update_status("indicator_1_1", "error")
      output$analysis_output_composition <- renderText(paste("\u2717 Error\n\n", result$message))
    }
    enable("run_indicator_1_1")
  })
  
  observeEvent(input$run_indicator_1_2, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$analysis_output_composition <- renderText(error_msg)
      return()
    }
    disable("run_indicator_1_2")
    update_status("indicator_1_2", "running")
    output$analysis_output_composition <- renderText("Processing Indicator 1.2...")
    
    result <- run_rmarkdown("3_Analysis", "Indicator 1.2", 
                            input$country, input$year, "v1")
    
    if (result$success) {
      update_status("indicator_1_2", "complete")
      output$analysis_output_composition <- renderText(paste("\u2713 Complete\n\n", result$message))
    } else {
      update_status("indicator_1_2", "error")
      output$analysis_output_composition <- renderText(paste("\u2717 Error\n\n", result$message))
    }
    enable("run_indicator_1_2")
  })
  
  # Availability
  observeEvent(input$run_indicator_2_1, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$analysis_output_availability <- renderText(error_msg)
      return()
    }
    disable("run_indicator_2_1")
    update_status("indicator_2_1", "running")
    output$analysis_output_availability <- renderText("Processing Indicator 2.1...")
    
    result <- run_rmarkdown("3_Analysis", "Indicator 2.1", 
                            input$country, input$year, "v1")
    
    if (result$success) {
      update_status("indicator_2_1", "complete")
      output$analysis_output_availability <- renderText(paste("\u2713 Complete\n\n", result$message))
    } else {
      update_status("indicator_2_1", "error")
      output$analysis_output_availability <- renderText(paste("\u2717 Error\n\n", result$message))
    }
    enable("run_indicator_2_1")
  })
  
  observeEvent(input$run_indicator_2_2, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$analysis_output_availability <- renderText(error_msg)
      return()
    }
    disable("run_indicator_2_2")
    update_status("indicator_2_2", "running")
    output$analysis_output_availability <- renderText("Processing Indicator 2.2...")
    
    result <- run_rmarkdown("3_Analysis", "Indicator 2.2", 
                            input$country, input$year, "v1")
    
    if (result$success) {
      update_status("indicator_2_2", "complete")
      output$analysis_output_availability <- renderText(paste("\u2713 Complete\n\n", result$message))
    } else {
      update_status("indicator_2_2", "error")
      output$analysis_output_availability <- renderText(paste("\u2717 Error\n\n", result$message))
    }
    enable("run_indicator_2_2")
  })
  
  observeEvent(input$run_indicator_2_3, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$analysis_output_availability <- renderText(error_msg)
      return()
    }
    disable("run_indicator_2_3")
    update_status("indicator_2_3", "running")
    output$analysis_output_availability <- renderText("Processing Indicator 2.3...")
    
    result <- run_rmarkdown("3_Analysis", "Indicator 2.3", 
                            input$country, input$year, "v1")
    
    if (result$success) {
      update_status("indicator_2_3", "complete")
      output$analysis_output_availability <- renderText(paste("\u2713 Complete\n\n", result$message))
    } else {
      update_status("indicator_2_3", "error")
      output$analysis_output_availability <- renderText(paste("\u2717 Error\n\n", result$message))
    }
    enable("run_indicator_2_3")
  })
  
  observeEvent(input$run_indicator_2_4, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$analysis_output_availability <- renderText(error_msg)
      return()
    }
    disable("run_indicator_2_4")
    update_status("indicator_2_4", "running")
    output$analysis_output_availability <- renderText("Processing Indicator 2.4...")
    
    result <- run_rmarkdown("3_Analysis", "Indicator 2.4", 
                            input$country, input$year, "v1")
    
    if (result$success) {
      update_status("indicator_2_4", "complete")
      output$analysis_output_availability <- renderText(paste("\u2713 Complete\n\n", result$message))
    } else {
      update_status("indicator_2_4", "error")
      output$analysis_output_availability <- renderText(paste("\u2717 Error\n\n", result$message))
    }
    enable("run_indicator_2_4")
  })
  
  # Sales Volume
  observeEvent(input$run_indicator_3_1, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$analysis_output_volume <- renderText(error_msg)
      return()
    }
    disable("run_indicator_3_1")
    update_status("indicator_3_1", "running")
    output$analysis_output_volume <- renderText("Processing Indicator 3.1...")
    
    result <- run_rmarkdown("3_Analysis", "Indicator 3.1", 
                            input$country, input$year, "v1")
    
    if (result$success) {
      update_status("indicator_3_1", "complete")
      output$analysis_output_volume <- renderText(paste("\u2713 Complete\n\n", result$message))
    } else {
      update_status("indicator_3_1", "error")
      output$analysis_output_volume <- renderText(paste("\u2717 Error\n\n", result$message))
    }
    enable("run_indicator_3_1")
  })
  
  observeEvent(input$run_indicator_3_2, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$analysis_output_volume <- renderText(error_msg)
      return()
    }
    disable("run_indicator_3_2")
    update_status("indicator_3_2", "running")
    output$analysis_output_volume <- renderText("Processing Indicator 3.2...")
    
    result <- run_rmarkdown("3_Analysis", "Indicator 3.2", 
                            input$country, input$year, "v1")
    
    if (result$success) {
      update_status("indicator_3_2", "complete")
      output$analysis_output_volume <- renderText(paste("\u2713 Complete\n\n", result$message))
    } else {
      update_status("indicator_3_2", "error")
      output$analysis_output_volume <- renderText(paste("\u2717 Error\n\n", result$message))
    }
    enable("run_indicator_3_2")
  })
  
  observeEvent(input$run_indicator_3_3, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$analysis_output_volume <- renderText(error_msg)
      return()
    }
    disable("run_indicator_3_3")
    update_status("indicator_3_3", "running")
    output$analysis_output_volume <- renderText("Processing Indicator 3.3...")
    
    result <- run_rmarkdown("3_Analysis", "Indicator 3.3", 
                            input$country, input$year, "v1")
    
    if (result$success) {
      update_status("indicator_3_3", "complete")
      output$analysis_output_volume <- renderText(paste("\u2713 Complete\n\n", result$message))
    } else {
      update_status("indicator_3_3", "error")
      output$analysis_output_volume <- renderText(paste("\u2717 Error\n\n", result$message))
    }
    enable("run_indicator_3_3")
  })
  
  observeEvent(input$run_indicator_3_4, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$analysis_output_volume <- renderText(error_msg)
      return()
    }
    disable("run_indicator_3_4")
    update_status("indicator_3_4", "running")
    output$analysis_output_volume <- renderText("Processing Indicator 3.4...")
    
    result <- run_rmarkdown("3_Analysis", "Indicator 3.4", 
                            input$country, input$year, "v1")
    
    if (result$success) {
      update_status("indicator_3_4", "complete")
      output$analysis_output_volume <- renderText(paste("\u2713 Complete\n\n", result$message))
    } else {
      update_status("indicator_3_4", "error")
      output$analysis_output_volume <- renderText(paste("\u2717 Error\n\n", result$message))
    }
    enable("run_indicator_3_4")
  })
  
  # Market Share
  observeEvent(input$run_indicator_4_1, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$analysis_output_marketshare <- renderText(error_msg)
      return()
    }
    disable("run_indicator_4_1")
    update_status("indicator_4_1", "running")
    output$analysis_output_marketshare <- renderText("Processing Indicator 4.1...")
    
    result <- run_rmarkdown("3_Analysis", "Indicator 4.1", 
                            input$country, input$year, "v1")
    
    if (result$success) {
      update_status("indicator_4_1", "complete")
      output$analysis_output_marketshare <- renderText(paste("\u2713 Complete\n\n", result$message))
    } else {
      update_status("indicator_4_1", "error")
      output$analysis_output_marketshare <- renderText(paste("\u2717 Error\n\n", result$message))
    }
    enable("run_indicator_4_1")
  })
  
  observeEvent(input$run_indicator_4_2, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$analysis_output_marketshare <- renderText(error_msg)
      return()
    }
    disable("run_indicator_4_2")
    update_status("indicator_4_2", "running")
    output$analysis_output_marketshare <- renderText("Processing Indicator 4.2...")
    
    result <- run_rmarkdown("3_Analysis", "Indicator 4.2", 
                            input$country, input$year, "v1")
    
    if (result$success) {
      update_status("indicator_4_2", "complete")
      output$analysis_output_marketshare <- renderText(paste("\u2713 Complete\n\n", result$message))
    } else {
      update_status("indicator_4_2", "error")
      output$analysis_output_marketshare <- renderText(paste("\u2717 Error\n\n", result$message))
    }
    enable("run_indicator_4_2")
  })
  
  observeEvent(input$run_indicator_4_3, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$analysis_output_marketshare <- renderText(error_msg)
      return()
    }
    disable("run_indicator_4_3")
    update_status("indicator_4_3", "running")
    output$analysis_output_marketshare <- renderText("Processing Indicator 4.3...")
    
    result <- run_rmarkdown("3_Analysis", "Indicator 4.3", 
                            input$country, input$year, "v1")
    
    if (result$success) {
      update_status("indicator_4_3", "complete")
      output$analysis_output_marketshare <- renderText(paste("\u2713 Complete\n\n", result$message))
    } else {
      update_status("indicator_4_3", "error")
      output$analysis_output_marketshare <- renderText(paste("\u2717 Error\n\n", result$message))
    }
    enable("run_indicator_4_3")
  })
  
  # Pricing
  observeEvent(input$run_indicator_5_1a, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$analysis_output_pricing <- renderText(error_msg)
      return()
    }
    disable("run_indicator_5_1a")
    update_status("indicator_5_1a", "running")
    output$analysis_output_pricing <- renderText("Processing Indicator 5.1a...")
    
    result <- run_rmarkdown("3_Analysis", "Indicator 5.1a", 
                            input$country, input$year, "v1")
    
    if (result$success) {
      update_status("indicator_5_1a", "complete")
      output$analysis_output_pricing <- renderText(paste("\u2713 Complete\n\n", result$message))
    } else {
      update_status("indicator_5_1a", "error")
      output$analysis_output_pricing <- renderText(paste("\u2717 Error\n\n", result$message))
    }
    enable("run_indicator_5_1a")
  })
  
  observeEvent(input$run_indicator_5_2a, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$analysis_output_pricing <- renderText(error_msg)
      return()
    }
    disable("run_indicator_5_2a")
    update_status("indicator_5_2a", "running")
    output$analysis_output_pricing <- renderText("Processing Indicator 5.2a...")
    
    result <- run_rmarkdown("3_Analysis", "Indicator 5.2a", 
                            input$country, input$year, "v1")
    
    if (result$success) {
      update_status("indicator_5_2a", "complete")
      output$analysis_output_pricing <- renderText(paste("\u2713 Complete\n\n", result$message))
    } else {
      update_status("indicator_5_2a", "error")
      output$analysis_output_pricing <- renderText(paste("\u2717 Error\n\n", result$message))
    }
    enable("run_indicator_5_2a")
  })
  
  observeEvent(input$run_indicator_5_3, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$analysis_output_pricing <- renderText(error_msg)
      return()
    }
    disable("run_indicator_5_3")
    update_status("indicator_5_3", "running")
    output$analysis_output_pricing <- renderText("Processing Indicator 5.3...")
    
    result <- run_rmarkdown("3_Analysis", "Indicator 5.3", 
                            input$country, input$year, "v1")
    
    if (result$success) {
      update_status("indicator_5_3", "complete")
      output$analysis_output_pricing <- renderText(paste("\u2713 Complete\n\n", result$message))
    } else {
      update_status("indicator_5_3", "error")
      output$analysis_output_pricing <- renderText(paste("\u2717 Error\n\n", result$message))
    }
    enable("run_indicator_5_3")
  })
  
  observeEvent(input$run_indicator_6_1b, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$analysis_output_pricing <- renderText(error_msg)
      return()
    }
    disable("run_indicator_6_1b")
    update_status("indicator_6_1b", "running")
    output$analysis_output_pricing <- renderText("Processing Indicator 6.1b...")
    
    result <- run_rmarkdown("3_Analysis", "Indicator 6.1b", 
                            input$country, input$year, "v1")
    
    if (result$success) {
      update_status("indicator_6_1b", "complete")
      output$analysis_output_pricing <- renderText(paste("\u2713 Complete\n\n", result$message))
    } else {
      update_status("indicator_6_1b", "error")
      output$analysis_output_pricing <- renderText(paste("\u2717 Error\n\n", result$message))
    }
    enable("run_indicator_6_1b")
  })
  
  observeEvent(input$run_indicator_6_2, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$analysis_output_pricing <- renderText(error_msg)
      return()
    }
    disable("run_indicator_6_2")
    update_status("indicator_6_2", "running")
    output$analysis_output_pricing <- renderText("Processing Indicator 6.2...")
    
    result <- run_rmarkdown("3_Analysis", "Indicator 6.2", 
                            input$country, input$year, "v1")
    
    if (result$success) {
      update_status("indicator_6_2", "complete")
      output$analysis_output_pricing <- renderText(paste("\u2713 Complete\n\n", result$message))
    } else {
      update_status("indicator_6_2", "error")
      output$analysis_output_pricing <- renderText(paste("\u2717 Error\n\n", result$message))
    }
    enable("run_indicator_6_2")
  })
  
  # Stock Outs
  observeEvent(input$run_indicator_7_1_7_2, {
    error_msg <- check_inputs()
    if (!is.null(error_msg)) {
      output$analysis_output_stockouts <- renderText(error_msg)
      return()
    }
    disable("run_indicator_7_1_7_2")
    update_status("indicator_7_1_7_2", "running")
    output$analysis_output_stockouts <- renderText("Processing Indicator 7.1 & 7.2...")
    
    result <- run_rmarkdown("3_Analysis", "Indicator 7.1 & 7.2", 
                            input$country, input$year, "v1")
    
    if (result$success) {
      update_status("indicator_7_1_7_2", "complete")
      output$analysis_output_stockouts <- renderText(paste("\u2713 Complete\n\n", result$message))
    } else {
      update_status("indicator_7_1_7_2", "error")
      output$analysis_output_stockouts <- renderText(paste("\u2717 Error\n\n", result$message))
    }
    enable("run_indicator_7_1_7_2")
  })
}

# Run the application
shinyApp(ui = ui, server = server)