# ============================================================
#  R/mod_upload.R
#  Module    : Data Upload
#  Description : Handles all data ingestion.  Supports three
#                input methods:
#                  A) Load all sheets from an .xlsx / .xls file
#                  B) Load a single named sheet
#                  C) Paste tab-separated text directly
#                CSV files are also accepted.  Each loaded sheet
#                is stored as a named entry in rv$datasets.
#
#  Data type   : Any tabular data (Excel sheets, CSV, pasted TSV)
#  Writes to   : rv$datasets (named list of data frames)
# ============================================================


# ── UI ───────────────────────────────────────────────────────

#' UI for the Data Upload tab.
#' @return A `shinydashboard::tabItem` object.
uploadUI <- function() {
  tabItem("upload",
    fluidRow(
      box(width = 5, title = "Upload File", status = "primary", solidHeader = TRUE,
        fileInput("file_main", "Choose .xlsx / .xls / .csv",
                  accept = c(".xlsx", ".xls", ".csv")),
        checkboxInput("has_header", "First row is header", TRUE),
        hr(),
        h5("Option A \u2013 all sheets"),
        actionButton("load_all", "Load ALL Sheets",
                     icon = icon("layer-group"), class = "btn-success btn-block"),
        hr(),
        h5("Option B \u2013 one sheet"),
        uiOutput("sheet_selector_single"),
        actionButton("load_one", "Load Selected Sheet",
                     icon = icon("table"), class = "btn-primary btn-block"),
        hr(),
        h5("Option C \u2013 paste tab-separated data"),
        textAreaInput("paste_data", "Paste here", "", rows = 4),
        textInput("paste_name", "Dataset name", "pasted_data"),
        actionButton("load_paste", "Import Pasted Data",
                     icon = icon("paste"), class = "btn-warning btn-block")
      ),
      box(width = 7, title = "Loaded Datasets", status = "success", solidHeader = TRUE,
        uiOutput("loaded_datasets_summary"),
        hr(),
        h5("Auto-detected column roles (first dataset):"),
        uiOutput("auto_detect_summary")
      )
    ),
    fluidRow(
      box(width = 12, title = "Data Preview", status = "success", solidHeader = TRUE,
        uiOutput("preview_sheet_picker"),
        DTOutput("preview_table")
      )
    )
  )
}


# ── Server ───────────────────────────────────────────────────

#' Server logic for the Data Upload tab.
#'
#' Registers observers for all three load buttons and renders
#' the dataset summary, auto-detect summary and preview table.
#'
#' @param input   Shiny `input` object (shared with main server).
#' @param output  Shiny `output` object.
#' @param session Shiny `session` object.
#' @param rv      Shared `reactiveValues` with `$datasets`.
uploadServer <- function(input, output, session, rv) {

  # ── Sheet selector for single-sheet loading ──────────────
  output$sheet_selector_single <- renderUI({
    req(input$file_main)
    if (tolower(tools::file_ext(input$file_main$name)) == "csv") return(NULL)
    sheets <- readxl::excel_sheets(input$file_main$datapath)
    selectInput("sheet_single", "Sheet", choices = sheets)
  })

  # ── Internal sheet loader ────────────────────────────────
  load_sheet <- function(path, sheet = 1, header = TRUE) {
    if (tolower(tools::file_ext(path)) == "csv")
      return(read.csv(path, header = header, stringsAsFactors = FALSE))
    readxl::read_excel(path, sheet = sheet, col_names = header) %>%
      as.data.frame()
  }

  # ── Option A: Load all sheets ────────────────────────────
  observeEvent(input$load_all, {
    req(input$file_main)
    if (tolower(tools::file_ext(input$file_main$name)) == "csv") {
      rv$datasets[["data"]] <- load_sheet(input$file_main$datapath)
      showNotification("CSV loaded as 'data'", type = "message")
      return()
    }
    sheets <- readxl::excel_sheets(input$file_main$datapath)
    ok <- fail <- character(0)
    for (sh in sheets) {
      tryCatch({
        rv$datasets[[sh]] <-
          load_sheet(input$file_main$datapath, sh, input$has_header)
        ok <- c(ok, sh)
      }, error = function(e) { fail <<- c(fail, sh) })
    }
    showNotification(
      paste0("Loaded: ", paste(ok, collapse = ", "),
             if (length(fail))
               paste0("  Failed: ", paste(fail, collapse = ", "))
             else ""),
      type = "message", duration = 8
    )
  })

  # ── Option B: Load one sheet ─────────────────────────────
  observeEvent(input$load_one, {
    req(input$file_main)
    sh <- if (!is.null(input$sheet_single)) input$sheet_single else 1
    tryCatch({
      rv$datasets[[as.character(sh)]] <-
        load_sheet(input$file_main$datapath, sh, input$has_header)
      showNotification(paste0("Loaded '", sh, "'"), type = "message")
    }, error = function(e)
      showNotification(paste("Error:", e$message), type = "error"))
  })

  # ── Option C: Pasted data ────────────────────────────────
  observeEvent(input$load_paste, {
    req(nzchar(input$paste_data))
    tryCatch({
      df <- read.delim(textConnection(input$paste_data),
                       sep = "\t", header = input$has_header,
                       stringsAsFactors = FALSE)
      nm <- input$paste_name %||% "pasted_data"
      rv$datasets[[nm]] <- df
      showNotification(paste0("'", nm, "' imported \u2013 ", nrow(df), " rows"),
                       type = "message")
    }, error = function(e)
      showNotification(paste("Error:", e$message), type = "error"))
  })

  # ── Dataset summary panel ────────────────────────────────
  output$loaded_datasets_summary <- renderUI({
    nms <- names(rv$datasets)
    if (!length(nms))
      return(p(style = "color:#999;", "No datasets loaded yet."))
    tags$ul(lapply(nms, function(nm) {
      df  <- rv$datasets[[nm]]
      mp  <- rv$mappings[[nm]]
      badge <- if (!is.null(mp))
        span(style = "color:#00a65a;font-size:11px;", " mapped")
      else
        span(style = "color:#c00;font-size:11px;", " not mapped")
      tags$li(
        span(class = "dataset-badge", nm), badge,
        " \u2013 ", nrow(df), "\u00d7", ncol(df),
        " | ", em(paste(names(df), collapse = ", "))
      )
    }))
  })

  # ── Auto-detect summary panel ────────────────────────────
  output$auto_detect_summary <- renderUI({
    nms <- names(rv$datasets)
    req(length(nms))
    df  <- rv$datasets[[nms[1]]]
    ad  <- auto_detect_cols(df)
    tags$table(class = "table table-condensed table-bordered",
      tags$thead(tags$tr(tags$th("Role"), tags$th("Detected column"))),
      tags$tbody(lapply(names(ad), function(r) {
        v <- ad[[r]]
        tags$tr(tags$td(r),
                tags$td(if (!is.null(v) && !is.na(v))
                  tagList(v, span(class = "auto-badge", "AUTO"))
                else
                  em(style = "color:#c00;", "not found")))
      }))
    )
  })

  # ── Data preview ─────────────────────────────────────────
  output$preview_sheet_picker <- renderUI({
    nms <- names(rv$datasets)
    req(length(nms))
    selectInput("preview_sheet", "Preview sheet:",
                choices = nms, width = "300px")
  })

  output$preview_table <- renderDT({
    req(input$preview_sheet, rv$datasets[[input$preview_sheet]])
    datatable(rv$datasets[[input$preview_sheet]],
              options = list(scrollX = TRUE, pageLength = 10),
              rownames = FALSE)
  })
}
