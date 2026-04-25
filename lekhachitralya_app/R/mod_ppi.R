# ============================================================
#  R/mod_ppi.R
#  Module    : PPI Network
#  Description : Visualises a protein–protein interaction (PPI)
#                network using TCMNP::ppi_plot().  The user can
#                control node colour palette, layout algorithm,
#                label degree threshold, label repulsion and
#                label font size.
#
#  Data type   : PPI data (bipartite / unipartite)
#  Required columns : node1, node2
#  Optional columns : score (edge weight)
# ============================================================


# ── UI ───────────────────────────────────────────────────────

#' UI for the PPI Network tab.
#' @return A `shinydashboard::tabItem` object.
ppiUI <- function() {
  tabItem("ppi",
    fluidRow(
      box(width = 3, title = "Parameters", status = "purple", solidHeader = TRUE,
        dataset_picker("ds_ppi"),
        hr(),
        div(class = "override-group",
          tags$h6("Column roles"),
          uiOutput("ppi_col_node1"),
          uiOutput("ppi_col_node2"),
          uiOutput("ppi_col_score")
        ),
        hr(),
        selectInput("ppi_color",     "Node colour", PALETTES, "Set2"),
        selectInput("ppi_layout",    "Graph layout",
          c("stress","fr","kk","circle","star","nicely","grid","tree"),
          selected = "stress"),
        numericInput("ppi_label_deg", "Label degree threshold", 2, 1, 20),
        checkboxInput("ppi_repel",    "Repel labels",  TRUE),
        sliderInput("ppi_label_sz",   "Label size",    1, 10, 3.5, step = 0.5),
        actionButton("draw_ppi", "Draw PPI Network",
                     class = "btn-lg", style = "background:#7b2d8b;color:#fff;width:100%")
      ),
      box(width = 9, title = "Protein\u2013Protein Interaction Network",
          status = "purple", solidHeader = TRUE,
        label_theme_box("ppi", show_xy = FALSE),
        withSpinner(plotOutput("plot_ppi", height = "600px"), type = 6)
      )
    )
  )
}


# ── Server ───────────────────────────────────────────────────

#' Server logic for the PPI Network tab.
#'
#' @param input   Shiny `input` object.
#' @param output  Shiny `output` object.
#' @param session Shiny `session` object.
#' @param rv      Shared `reactiveValues`.
ppiServer <- function(input, output, session, rv) {

  make_ds_picker("ds_ppi", "Dataset - ppi", output, input, rv)
  make_col_override("ppi_col_node1", "node1", "sel_ds_ppi",
                    c("node1"), output, input, rv)
  make_col_override("ppi_col_node2", "node2", "sel_ds_ppi",
                    c("node2"), output, input, rv)
  make_col_override("ppi_col_score", "score", "sel_ds_ppi",
                    c("score","docking score"), output, input, rv)

  make_ppi_plot <- reactive({
    req(input$draw_ppi)
    isolate({
      df <- get_mapped("ppi", input, rv)
      df <- apply_col_override(df, list(
        c("ppi_col_node1", "node1"),
        c("ppi_col_node2", "node2"),
        c("ppi_col_score", "score")
      ), input)
      need_cols(df, c("node1", "node2"))
      p <- TCMNP::ppi_plot(df,
                           node.color  = input$ppi_color,
                           graph.layout = input$ppi_layout,
                           label.degree = input$ppi_label_deg,
                           label.repel  = input$ppi_repel,
                           label.size   = input$ppi_label_sz)
      apply_labels(p, "ppi", input, show_xy = FALSE)
    })
  })

  output$plot_ppi <- renderPlot({
    p <- make_ppi_plot()
    rv$plots[["ppi"]] <- p
    p
  })
}
