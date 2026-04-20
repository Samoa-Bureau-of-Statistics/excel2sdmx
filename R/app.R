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
        menuItem("GDP Converter", tabName = "gdp", icon = icon("chart-line")),
        menuItem("Employment Converter", tabName = "employment", icon = icon("users")),
        menuItem("Government Finance (GFS)", tabName = "gfs", icon = icon("table")),
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
              verbatimTextOutput("log_imts")
            )
          ),

          fluidRow(
            box(
              title = "IMTS Preview (First 10 Rows)",
              status = "success",
              solidHeader = TRUE,
              width = 12,
              tableOutput("preview_imts")
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
              verbatimTextOutput("log_cpi")
            )
          ),

          fluidRow(
            box(
              title = "CPI Preview (First 10 Rows)",
              status = "success",
              solidHeader = TRUE,
              width = 12,
              tableOutput("preview_cpi")
            )
          )
        ),

        # ---------------- GDP TAB ----------------
        tabItem(
          tabName = "gdp",

          fluidRow(

            box(
              title = "Upload Files",
              status = "primary",
              solidHeader = TRUE,
              width = 4,

              fileInput("gdp_file",
                        "Upload gdp Excel file (.xlsx)",
                        accept = ".xlsx"),

              actionButton("gdp_process", "Process File", icon = icon("play")),
              br(), br(),
              downloadButton("gdp_download_zip", "Download Output ZIP")
            ),

            box(
              title = "GDP Processing Log",
              status = "warning",
              solidHeader = TRUE,
              width = 8,
              verbatimTextOutput("log_gdp")
            )
          ),

          fluidRow(
            box(
              title = "GDP Preview (First 10 Rows)",
              status = "success",
              solidHeader = TRUE,
              width = 12,
              tableOutput("preview_gdp")
            )
          )
        ),

        # ---------------- Employment TAB ----------------
        tabItem(
          tabName = "employment",

          fluidRow(

            box(
              title = "Upload Files",
              status = "primary",
              solidHeader = TRUE,
              width = 4,

              fileInput("emp_file",
                        "Upload Employment Excel file (.xlsx)",
                        accept = ".xlsx"),

              actionButton("emp_process", "Process File", icon = icon("play")),
              br(), br(),
              downloadButton("emp_download_zip", "Download Output ZIP")
            ),

            box(
              title = "Employment Processing Log",
              status = "warning",
              solidHeader = TRUE,
              width = 8,
              verbatimTextOutput("log_emp")
            )
          ),

          fluidRow(
            box(
              title = "Employment Preview (First 10 Rows)",
              status = "success",
              solidHeader = TRUE,
              width = 12,
              tableOutput("preview_emp")
            )
          )
        ),

        # ---------------- GFS TAB ----------------

        tabItem(
          tabName = "gfs",

          fluidRow(
            box(
              title = "Upload GFS Excel File",
              status = "primary",
              solidHeader = TRUE,
              width = 4,
              fileInput("gfs_file", "Choose GFS Excel File (.xlsx)", accept = c(".xlsx")),
              actionButton("process_gfs", "Process Data", icon = icon("play")),
              br(), br(),
              downloadButton("gfs_download_zip", "Download ZIP")
            ),
            box(
              title = "Government Finance processing log",
              status = "warning",
              solidHeader = TRUE,
              width = 8,
              verbatimTextOutput("log_gfs")
            )
          ),

          fluidRow(
            box(
              title = "Preview Processed Data",
              status = "success",
              solidHeader = TRUE,
              width = 12,
              dataTableOutput("preview_gfs")
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

    # ---------------- GLOBAL HELPERS ----------------
    add_log <- function(rv, msg) {
      rv(paste(rv(), msg, sep = "\n"))
    }

    # =================================================
    # ---------------- IMTS ----------------
    # =================================================
    imts_log <- reactiveVal("")
    imts_preview <- reactiveVal(NULL)
    imts_files <- reactiveVal(NULL)

    observeEvent(input$imts_process, {

      req(input$imts_file)

      imts_log("")
      imts_preview(NULL)

      add_log(imts_log, "Starting IMTS processing...")

      file_path <- input$imts_file$datapath
      sheet_names <- excel_sheets(file_path)

      temp_dir <- tempdir()
      created_files <- c()

      for (sheet in sheet_names) {

        add_log(imts_log, paste("Processing sheet:", sheet))

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

        } else if (sheet %in% c("DF_IMTS_TABLE2","DF_IMTS_TABLE3","DF_IMTS_TABLE5","DF_IMTS_TABLE6")) {

          table <- read_excel(file_path, sheet = sheet)

          table <- table |>
            mutate(across(starts_with("HS_"), ~ as.numeric(gsub(",", "", .))))

          table_long <- table |>
            pivot_longer(
              cols = -c(DATAFLOW:TIME_PERIOD),
              names_to = "COMMODITY",
              values_to = "OBS_VALUE"
            )

        } else if (sheet %in% c("DF_IMTS_TABLE4","DF_IMTS_TABLE7")) {

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
          add_log(imts_log, paste("Skipping sheet:", sheet))
          next
        }

        table_long <- add_transformations(table_long)

        table_long <- table_long |>
          mutate(
            UNIT_MULT = ifelse(is.na(UNIT_MULT), "", UNIT_MULT),
            OBS_STATUS = ifelse(is.na(OBS_STATUS), "", OBS_STATUS),
            COMMENT = ifelse(is.na(COMMENT), "", COMMENT),
            OBS_VALUE = ifelse(is.na(OBS_VALUE) | is.infinite(OBS_VALUE), "", OBS_VALUE)
          ) |>
          select(DATAFLOW, FREQ, REF_AREA, TRADE_FLOW, COMMODITY,
                 COUNTERPART_AREA, TRANSFORMATION, TIME_PERIOD,
                 OBS_VALUE, UNIT_MEASURE, UNIT_MULT,
                 OBS_STATUS, COMMENT, DECIMALS)

        file_name <- file.path(temp_dir, paste0(sheet, ".csv"))
        write.csv(table_long, file_name, row.names = FALSE)

        created_files <- c(created_files, file_name)

        if (is.null(imts_preview())) {
          imts_preview(head(table_long, 10))
        }
      }

      imts_files(created_files)
      add_log(imts_log, paste("Completed:", length(created_files), "files ✔"))
    })

    output$preview_imts <- renderTable({
      req(imts_preview())
      imts_preview()
    })

    output$log_imts <- renderText({
      imts_log()
    })

    output$imts_download_zip <- downloadHandler(
      filename = function() paste0("IMTS_output_", Sys.Date(), ".zip"),
      content = function(file) {
        req(imts_files())
        zip::zipr(zipfile = file, files = imts_files())
      }
    )

    # =================================================
    # ---------------- CPI ----------------
    # =================================================
    cpi_log <- reactiveVal("")
    cpi_preview <- reactiveVal(NULL)
    cpi_files <- reactiveVal(NULL)

    observeEvent(input$cpi_process, {

      req(input$cpi_file)

      cpi_log("")
      cpi_preview(NULL)

      add_log(cpi_log, "Starting CPI processing...")

      file_path <- input$cpi_file$datapath
      sheet_names <- excel_sheets(file_path)

      temp_dir <- tempdir()
      created_files <- c()

      for (sheet in sheet_names) {

        add_log(cpi_log, paste("Processing sheet:", sheet))

        table <- read_excel(file_path, sheet = sheet)

        table_long <- table |>
          pivot_longer(cols = -c(DATAFLOW:DECIMALS),
                       names_to = "ITEM",
                       values_to = "OBS_VALUE")

        table_long <- add_transformations(table_long)
        table_long <- table_long |>
          select(DATAFLOW, FREQ, REF_AREA, INDICATOR_TYPE, INDICATOR, ITEM, TRANSFORMATION, TIME_PERIOD, OBS_VALUE, UNIT_MEASURE, BASE_PER, OBS_STATUS, COMMENT, DECIMALS) |>
          mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

        file_name <- file.path(temp_dir, paste0(sheet, ".csv"))
        write.csv(table_long, file_name, row.names = FALSE)

        created_files <- c(created_files, file_name)

        if (is.null(cpi_preview())) {
          cpi_preview(head(table_long, 10))
        }
      }

      cpi_files(created_files)
      add_log(cpi_log, "CPI processing completed ✔")
    })

    output$preview_cpi <- renderTable({
      req(cpi_preview())
      cpi_preview()
    })

    output$log_cpi <- renderText({
      cpi_log()
    })


    output$cpi_download_zip <- downloadHandler(
      filename = function() paste0("CPI_output_", Sys.Date(), ".zip"),
      content = function(file) {
        req(cpi_files())
        zip::zipr(zipfile = file, files = cpi_files())
      }
    )

    # =================================================
    # ---------------- GDP ----------------------------
    # =================================================
    gdp_log <- reactiveVal("")
    gdp_preview <- reactiveVal(NULL)
    gdp_files <- reactiveVal(NULL)

    observeEvent(input$gdp_process, {

      req(input$gdp_file)

      gdp_log("")
      gdp_preview(NULL)

      add_log(gdp_log, "Starting GDP processing...")

      file_path <- input$gdp_file$datapath
      sheet_names <- excel_sheets(file_path)

      temp_dir <- tempdir()
      created_files <- c()

      for (sheet in sheet_names) {

        table <- read_excel(file_path, sheet = sheet)
        add_log(gdp_log, paste("Processing sheet:", sheet))

        table_long <- table |>
          pivot_longer(cols = -c(DATAFLOW:REPYEARSTART),
                       names_to = "TIME_PERIOD",
                       values_to = "OBS_VALUE") |>
          select(DATAFLOW, FREQ, REF_PERIOD_DETAIL, REF_AREA, INDICATOR, INDUSTRY, TIME_PERIOD, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, BASE_PER, OBS_STATUS, COMMENT, DECIMALS, REPYEARSTART) |>
          mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

        table_long$FREQ <- ifelse(grepl("Q", table_long$TIME_PERIOD), "Q", table_long$FREQ)

        file_name <- file.path(temp_dir, paste0(sheet, ".csv"))
        write.csv(table_long, file_name, row.names = FALSE)

        created_files <- c(created_files, file_name)

        if (is.null(gdp_preview())) {
          gdp_preview(head(table_long, 10))
        }
      }

      gdp_files(created_files)
      add_log(gdp_log, "GDP processing completed ✔")
    })

    output$log_gdp <- renderText({
      gdp_log()
    })

    output$preview_gdp <- renderTable({
      req(gdp_preview())
      gdp_preview()
    })

    output$gdp_download_zip <- downloadHandler(
      filename = function() paste0("GDP_output_", Sys.Date(), ".zip"),
      content = function(file) {
        req(gdp_files())
        zip::zipr(zipfile = file, files = gdp_files())
      }
    )

    # =================================================
    # ---------------- EMPLOYMENT ----------------
    # =================================================
    emp_log <- reactiveVal("")
    emp_preview <- reactiveVal(NULL)
    emp_files <- reactiveVal(NULL)

    observeEvent(input$emp_process, {

      req(input$emp_file)

      emp_log("")
      emp_preview(NULL)

      add_log(emp_log, "Starting Employment processing...")

      file_path <- input$emp_file$datapath
      sheet_names <- excel_sheets(file_path)

      temp_dir <- tempdir()
      created_files <- c()

      for (sheet in sheet_names) {

        table <- read_excel(file_path, sheet = sheet)
        add_log(emp_log, paste("Processing sheet:", sheet))

        table_long <- table |>
          pivot_longer(cols = -c(DATAFLOW:DECIMALS),
                       names_to = "TIME_PERIOD",
                       values_to = "OBS_VALUE") |>
          select(DATAFLOW, FREQ, REF_AREA, INDICATOR, SEX, INDUSTRY, TRANSFORMATION, TIME_PERIOD, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, BASE_PER, OBS_STATUS, COMMENT, DECIMALS) |>
          mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

        file_name <- file.path(temp_dir, paste0(sheet, ".csv"))
        write.csv(table_long, file_name, row.names = FALSE)

        created_files <- c(created_files, file_name)

        if (is.null(emp_preview())) {
          emp_preview(head(table_long, 10))
        }
      }

      emp_files(created_files)
      add_log(emp_log, "Employment processing completed ✔")
    })

    output$preview_emp <- renderTable({
      req(emp_preview())
      emp_preview()
    })

    output$log_emp <- renderText({
      emp_log()
    })

    output$emp_download_zip <- downloadHandler(
      filename = function() paste0("EMP_output_", Sys.Date(), ".zip"),
      content = function(file) {
        req(emp_files())
        zip::zipr(zipfile = file, files = emp_files())
      }
    )

    # =================================================
    # ---------------- GFS ----------------
    # =================================================
    gfs_log <- reactiveVal("")
    gfs_preview <- reactiveVal(NULL)
    gfs_files <- reactiveVal(NULL)

    observeEvent(input$process_gfs, {

      req(input$gfs_file)

      gfs_log("")
      gfs_preview(NULL)

      gfs_log(paste(gfs_log(), "Starting GFS processing...", sep = "\n"))

      file_path <- input$gfs_file$datapath
      sheet_names <- excel_sheets(file_path)

      temp_dir <- tempdir()
      created_files <- c()

      for (sheet in sheet_names) {

        table <- read_excel(file_path, sheet = sheet, col_types = "text")

        gfs_log(paste(gfs_log(), paste("Processing sheet:", sheet), sep = "\n"))

        table_long <- table |>
          pivot_longer(
            cols = -c(DATAFLOW:DECIMALS),
            names_to = "TIME_PERIOD",
            values_to = "OBS_VALUE"
          ) |>
          select(DATAFLOW, FREQ, REF_AREA, STO, TIME_PERIOD, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, COMMENT, REPYEARSTART, DECIMALS) |>
          mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

        file_name <- file.path(temp_dir, paste0(sheet, ".csv"))
        write.csv(table_long, file_name, row.names = FALSE)

        created_files <- c(created_files, file_name)

        if (is.null(gfs_preview())) {
          gfs_preview(head(table_long, 10))
        }
      }

      gfs_files(created_files)

      gfs_log(paste(gfs_log(), "GFS processing completed ✔", sep = "\n"))
    })

    output$preview_gfs <- renderDataTable({
      req(gfs_preview())
      gfs_preview()
    })

    output$log_gfs <- renderText({
      gfs_log()
    })

    output$gfs_download_zip <- downloadHandler(
      filename = function() paste0("GFS_output_", Sys.Date(), ".zip"),
      content = function(file) {
        req(gfs_files())
        zip::zipr(zipfile = file, files = gfs_files())
      }
    )

  }

  # -------------------------------------------------
  # ---------------- Run App ------------------------
  # -------------------------------------------------
  shinyApp(ui, server)

}
