# ============================================================
#  R/mod_export.R
#  Module    : Plot Export
#  Description : Provides a single, consistent download handler
#                for all plots rendered by the other modules.
#                Supports four raster/vector formats at
#                user-specified dimensions and DPI.
#
#  Data type   : N/A (uses rv$plots to retrieve rendered ggplot objects)
#  Formats     : PNG, PDF, SVG, TIFF
# ============================================================


# ── UI ───────────────────────────────────────────────────────

#' UI for the Plot Export tab.
#' @return A `shinydashboard::tabItem` object.
exportUI <- function() {
  tabItem("export",
    fluidRow(
      box(width = 5, title = "Export Settings",
          status = "primary", solidHeader = TRUE,
        selectInput("exp_which", "Which plot to export",
          c("Best Plots"          = "best",
            "Docking Heatmap"     = "dock",
            "Score Explorer"      = "scoreex",
            "Classification"      = "classif",
            "TCM Network"         = "net",
            "PPI Network"         = "ppi",
            "Sankey / Alluvial"   = "sankey",
            "Degree Plot"         = "degree",
            "Venn / UpSet"        = "venn",
            "Enrichment"          = "enrich"),
          selected = "best"),
        selectInput("exp_fmt", "File format",
                    c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "TIFF" = "tiff"),
                    "png"),
        numericInput("exp_w",   "Width (inches)",  10,  1, 40, step = 0.5),
        numericInput("exp_h",   "Height (inches)",  8,  1, 40, step = 0.5),
        numericInput("exp_dpi", "Resolution (DPI)", 300, 72, 1200, step = 50),
        hr(),
        downloadButton("do_export", "Download Plot",
                       class = "btn-primary btn-lg")
      ),
      box(width = 7, title = "Export guide",
          status = "info", solidHeader = TRUE,
        p("1. Draw the desired plot in its own tab."),
        p("2. Return to this tab and select the plot name."),
        p("3. Choose format, dimensions and DPI."),
        p("4. Click \u201cDownload Plot\u201d."),
        hr(),
        tags$table(class = "table table-condensed",
          tags$thead(tags$tr(
            tags$th("Format"), tags$th("Best for"), tags$th("Typical DPI")
          )),
          tags$tbody(
            tags$tr(tags$td("PNG"),  tags$td("Web, presentation"),  tags$td("150\u2013300")),
            tags$tr(tags$td("TIFF"), tags$td("Journal submission"), tags$td("300\u2013600")),
            tags$tr(tags$td("PDF"),  tags$td("Vector / editable"),  tags$td("N/A")),
            tags$tr(tags$td("SVG"),  tags$td("Scalable / Inkscape"),tags$td("N/A"))
          )
        )
      )
    )
  )
}


# ── Server ───────────────────────────────────────────────────

#' Server logic for the Plot Export tab.
#'
#' @param input   Shiny `input` object.
#' @param output  Shiny `output` object.
#' @param session Shiny `session` object.
#' @param rv      Shared `reactiveValues` with `$plots`.
exportServer <- function(input, output, session, rv) {
  output$do_export <- downloadHandler(
    filename = function()
      paste0("TCMNP_", input$exp_which, "_",
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$exp_fmt),
    content = function(file) {
      p <- rv$plots[[input$exp_which]]
      validate(need(!is.null(p), "Draw the plot first."))
      w   <- input$exp_w
      h   <- input$exp_h
      dpi <- input$exp_dpi
      switch(input$exp_fmt,
        png  = { grDevices::png(file,  width = w, height = h,
                                units = "in", res = dpi)
                 print(p); grDevices::dev.off() },
        pdf  = { grDevices::pdf(file,  width = w, height = h)
                 print(p); grDevices::dev.off() },
        svg  = { grDevices::svg(file,  width = w, height = h)
                 print(p); grDevices::dev.off() },
        tiff = { grDevices::tiff(file, width = w, height = h,
                                 units = "in", res = dpi,
                                 compression = "lzw")
                 print(p); grDevices::dev.off() }
      )
    }
  )
}
