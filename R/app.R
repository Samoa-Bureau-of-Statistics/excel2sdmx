#' Run the SDMX Converter Dashboard
#'
#' @export
run_sbsSDMX <- function() {

# -------------------------------------------------
# Load libraries
# -------------------------------------------------
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(zip)

# -------------------------------------------------
# Helper function: transformations
# -------------------------------------------------
add_transformations <- function(table_long) {

  table_long <- table_long |>
    group_by(across(-c(TIME_PERIOD, OBS_VALUE))) |>
    arrange(TIME_PERIOD, .by_group = TRUE) |>
    mutate(
      G1M = (OBS_VALUE / lag(OBS_VALUE, 1) - 1) * 100,
      G1Y = (OBS_VALUE / lag(OBS_VALUE, 12) - 1) * 100
    ) |>
    ungroup()

  base_data <- table_long |>
    mutate(TRANSFORMATION = "N") |>
    select(-G1M, -G1Y)

  g1m_data <- table_long |>
    mutate(
      OBS_VALUE = G1M,
      TRANSFORMATION = "G1M",
      UNIT_MEASURE = "PT",
      UNIT_MULT = NA_real_
    ) |>
    select(-G1M, -G1Y)

  g1y_data <- table_long |>
    mutate(
      OBS_VALUE = G1Y,
      TRANSFORMATION = "G1Y",
      UNIT_MEASURE = "PT",
      UNIT_MULT = NA_real_
    ) |>
    select(-G1M, -G1Y)

  bind_rows(base_data, g1m_data, g1y_data)
}

# -------------------------------------------------
# UI
# -------------------------------------------------
ui <- dashboardPage(

  dashboardHeader(title = "SDMX Converter"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("IMTS Converter", tabName = "imts", icon = icon("ship")),
      menuItem("CPI Converter", tabName = "cpi", icon = icon("shopping-cart")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),

  dashboardBody(
    tabItems(

      # ---------------- IMTS TAB ----------------
      tabItem(
        tabName = "imts",

        fluidRow(

          box(
            title = "Upload Files",
            status = "primary",
            solidHeader = TRUE,
            width = 4,

            fileInput("imts_file",
                      "Upload IMTS Excel file (.xlsx)",
                      accept = ".xlsx"),

            actionButton("imts_process", "Process File", icon = icon("play")),
            br(), br(),
            downloadButton("imts_download_zip", "Download Output ZIP")
          ),

          box(
            title = "IMTS Processing Log",
            status = "warning",
            solidHeader = TRUE,
            width = 8,
            verbatimTextOutput("log")
          )
        ),

        fluidRow(
          box(
            title = "IMTS Preview (First 10 Rows)",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            tableOutput("preview")
          )
        )
      ),

      # ---------------- CPI TAB ----------------
      tabItem(
        tabName = "cpi",

        fluidRow(

          box(
            title = "Upload Files",
            status = "primary",
            solidHeader = TRUE,
            width = 4,

            fileInput("cpi_file",
                      "Upload CPI Excel file (.xlsx)",
                      accept = ".xlsx"),

            actionButton("cpi_process", "Process File", icon = icon("play")),
            br(), br(),
            downloadButton("cpi_download_zip", "Download Output ZIP")
          ),

          box(
            title = "CPI Processing Log",
            status = "warning",
            solidHeader = TRUE,
            width = 8,
            verbatimTextOutput("log")
          )
        ),

        fluidRow(
          box(
            title = "CPI Preview (First 10 Rows)",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            tableOutput("preview")
          )
        )
      ),

      # ---------------- ABOUT TAB ----------------
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "About This Application",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            p("This application converts Excel files into SDMX-compliant CSV format."),
            p("Upload your Excel file and process all sheets automatically."),
            p("Download results as a ZIP file.")
          )
        )
      )
    )
  )
)

# -------------------------------------------------
# SERVER
# -------------------------------------------------
server <- function(input, output, session) {

  log_text <- reactiveVal("")
  output_files <- reactiveVal(NULL)
  preview_data <- reactiveVal(NULL)

  add_log <- function(msg) {
    log_text(paste(log_text(), msg, sep = "\n"))
  }

  #### ---------------- IMTS PROCESSING ---------------- ####
  observeEvent(input$imts_process, {

    req(input$imts_file)

    log_text("")
    add_log("Starting processing...")

    file_path <- input$imts_file$datapath
    sheet_names <- excel_sheets(file_path)

    temp_dir <- tempdir()
    created_files <- c()

    for (sheet in sheet_names) {

      add_log(paste("Processing sheet:", sheet))

      if (sheet == "DF_IMTS_TABLE1") {

        table <- read_excel(file_path, sheet = sheet)

        table <- table |>
          mutate(across(starts_with("HS_"), ~ as.numeric(gsub(",", "", .))))

        table_long <- table |>
          pivot_longer(
            cols = -c(DATAFLOW:TIME_PERIOD),
            names_to = "TRADE_FLOW",
            values_to = "OBS_VALUE"
          )

      } else if (sheet %in% c("DF_IMTS_TABLE2", "DF_IMTS_TABLE3", "DF_IMTS_TABLE5", "DF_IMTS_TABLE6")) {

        table <- read_excel(file_path, sheet = sheet)

        table <- table |>
          mutate(across(starts_with("HS_"), ~ as.numeric(gsub(",", "", .))))

        table_long <- table |>
          pivot_longer(
            cols = -c(DATAFLOW:TIME_PERIOD),
            names_to = "COMMODITY",
            values_to = "OBS_VALUE"
          )

      } else if (sheet %in% c("DF_IMTS_TABLE4", "DF_IMTS_TABLE7")) {

        table <- read_excel(file_path, sheet = sheet)

        table_long <- table |>
          mutate(across(-(DATAFLOW:TIME_PERIOD),
                        ~ as.numeric(gsub(",", "", trimws(.))))) |>
          pivot_longer(
            cols = -(DATAFLOW:TIME_PERIOD),
            names_to = "COUNTERPART_AREA",
            values_to = "OBS_VALUE"
          )

      } else {
        add_log(paste("Skipping sheet:", sheet))
        next
      }

      # Apply transformations
      table_long <- add_transformations(table_long)

      # Replace NA and inf with blanks
      table_long <- table_long |>
        mutate(
          UNIT_MULT = ifelse(is.na(UNIT_MULT), "", UNIT_MULT),
          OBS_STATUS = ifelse(is.na(OBS_STATUS), "", OBS_STATUS),
          COMMENT = ifelse(is.na(COMMENT), "", COMMENT),
          OBS_VALUE = ifelse(
            is.na(OBS_VALUE) | is.infinite(OBS_VALUE),
            "",
            OBS_VALUE
          )

        ) |>
        select(DATAFLOW, FREQ, REF_AREA, TRADE_FLOW, COMMODITY, COUNTERPART_AREA, TRANSFORMATION, TIME_PERIOD, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, COMMENT, DECIMALS)

      # Save CSV
      file_name <- file.path(temp_dir, paste0(sheet, ".csv"))
      write.csv(table_long, file_name, row.names = FALSE)

      created_files <- c(created_files, file_name)

      # Preview first sheet only
      if (is.null(preview_data())) {
        preview_data(head(table_long, 10))
      }
    }

    output_files(created_files)
    add_log(paste("Processing completed:", length(created_files), "files created ✔"))
  })

  # Preview
  output$preview <- renderTable({
    req(preview_data())
    preview_data()
  })

  # Log
  output$log <- renderText({
    log_text()
  })

  # Download ZIP
  output$imts_download_zip <- downloadHandler(
    filename = function() {
      paste0("IMTS_output_", Sys.Date(), ".zip")
    },
    content = function(file) {

      req(output_files())

      zip::zipr(
        zipfile = file,
        files = output_files()
      )
    }
  )

  #### ---------------- CPI PROCESSING ---------------- ####
  observeEvent(input$cpi_process, {

    req(input$cpi_file)

    log_text("")
    add_log("Starting processing...")

    file_path <- input$cpi_file$datapath
    sheet_names <- excel_sheets(file_path)

    temp_dir <- tempdir()
    created_files <- c()

    for (sheet in sheet_names) {

      add_log(paste("Processing sheet:", sheet))

      if (sheet %in% c("DFCPI_TABLE3", "DF_CPI_TABLE8")) {

        table <- read_excel(file_path, sheet = sheet)

        table <- table |>
          mutate(across(starts_with("ITEM_"), ~ as.numeric(gsub(",", "", .))))

        table_long <- table |>
          pivot_longer(
            cols = -c(DATAFLOW:DECIMALS),
            names_to = "ITEM",
            values_to = "OBS_VALUE"
          )

      } else if (sheet %in% c("DF_CPI_TABLE1", "DF_CPI_TABLE2", "DF_CPI_TABLE4", "DF_CPI_TABLE5", "DF_CPI_TABLE6", "DF_CPI_TABLE7")) {

        table <- read_excel(file_path, sheet = sheet)

        table <- table |>
          mutate(across(starts_with("ITEM_"), ~ as.numeric(gsub(",", "", .))))

        table_long <- table |>
          pivot_longer(
            cols = -c(DATAFLOW:DECIMALS),
            names_to = "ITEM",
            values_to = "OBS_VALUE"
          )

        # Apply transformations
        table_long <- add_transformations(table_long)

        # Replace NA and inf with blanks
        table_long <- table_long |>
          filter(!(INDICATOR == "WGT" & TRANSFORMATION %in% c("G1M", "G1Y"))) |>
          mutate(
            #UNIT_MULT = ifelse(is.na(UNIT_MULT), "", UNIT_MULT),
            OBS_STATUS = ifelse(is.na(OBS_STATUS), "", OBS_STATUS),
            COMMENT = ifelse(is.na(COMMENT), "", COMMENT),
            OBS_VALUE = ifelse(
              is.na(OBS_VALUE) | is.infinite(OBS_VALUE),
              "",
              OBS_VALUE
            )

          )

      } else {
        add_log(paste("Skipping sheet:", sheet))
        next
      }

      table_long <- table_long |>
        select(DATAFLOW, FREQ, REF_AREA, INDICATOR_TYPE, INDICATOR, ITEM, TRANSFORMATION, TIME_PERIOD, OBS_VALUE, UNIT_MEASURE, BASE_PER, OBS_STATUS, COMMENT, DECIMALS)

      # Save CSV
      file_name <- file.path(temp_dir, paste0(sheet, ".csv"))
      write.csv(table_long, file_name, row.names = FALSE)

      created_files <- c(created_files, file_name)

      # Preview first sheet only
      if (is.null(preview_data())) {
        preview_data(head(table_long, 10))
      }
    }

    output_files(created_files)
    add_log(paste("Processing completed:", length(created_files), "files created ✔"))
  })

  # Preview
  output$preview <- renderTable({
    req(preview_data())
    preview_data()
  })

  # Log
  output$log <- renderText({
    log_text()
  })

  # Download ZIP
  output$cpi_download_zip <- downloadHandler(
    filename = function() {
      paste0("CPI_output_", Sys.Date(), ".zip")
    },
    content = function(file) {

      req(output_files())

      zip::zipr(
        zipfile = file,
        files = output_files()
      )
    }
  )

} # End Server function

# -------------------------------------------------
# Run App
# -------------------------------------------------
shinyApp(ui, server)
}
