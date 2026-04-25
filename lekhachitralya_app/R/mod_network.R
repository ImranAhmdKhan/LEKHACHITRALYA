# ============================================================
#  R/mod_network.R
#  Module    : TCM Network
#  Description : Renders a tripartite herb–molecule–target network
#                using TCMNP::tcm_net().  The user can filter by
#                subsetting to a random sample to manage visual
#                complexity, choose graph layout and toggle
#                node-label threshold.
#
#  Data type   : Network data (tripartite)
#  Required columns : herb (sample fraction / plant), molecule, target
# ============================================================


# ── UI ───────────────────────────────────────────────────────

#' UI for the TCM Network tab.
#' @return A `shinydashboard::tabItem` object.
networkUI <- function() {
  tabItem("net",
    fluidRow(
      box(width = 3, title = "Parameters", status = "success", solidHeader = TRUE,
        dataset_picker("ds_net"),
        hr(),
        div(class = "override-group",
          tags$h6("Column roles"),
          uiOutput("net_col_herb"),
          uiOutput("net_col_molecule"),
          uiOutput("net_col_target")
        ),
        hr(),
        numericInput("net_n", "Max rows (0 = all)", 300, 0, 5000),
        numericInput("net_label_deg", "Label degree threshold", 2, 1, 20),
        checkboxInput("net_rem_dis", "Remove disconnected nodes", TRUE),
        selectInput("net_layout", "Graph layout",
          c("stress","fr","kk","circle","star","nicely","grid","tree"),
          selected = "stress"),
        actionButton("draw_net", "Draw Network",
                     class = "btn-success btn-lg", width = "100%")
      ),
      box(width = 9, title = "Herb \u2013 Molecule \u2013 Target Network",
          status = "success", solidHeader = TRUE,
        label_theme_box("net", show_xy = FALSE),
        withSpinner(plotOutput("plot_net", height = "600px"), type = 6)
      )
    )
  )
}


# ── Server ───────────────────────────────────────────────────

#' Server logic for the TCM Network tab.
#'
#' @param input   Shiny `input` object.
#' @param output  Shiny `output` object.
#' @param session Shiny `session` object.
#' @param rv      Shared `reactiveValues`.
networkServer <- function(input, output, session, rv) {

  gc <- function(id, df, pats = NULL) get_col(id, df, pats, input)

  make_ds_picker("ds_net", "Dataset - net", output, input, rv)
  make_col_override("net_col_herb",     "herb col",     "sel_ds_net",
                    c("fractions","fraction","herb"), output, input, rv)
  make_col_override("net_col_molecule", "molecule col", "sel_ds_net",
                    c("title","molecule"), output, input, rv)
  make_col_override("net_col_target",   "target col",   "sel_ds_net",
                    c("pdb id","target"), output, input, rv)

  make_net_plot <- reactive({
    req(input$draw_net)
    isolate({
      nm <- input$sel_ds_net; req(nm, rv$datasets[[nm]])
      df <- rv$datasets[[nm]]
      h  <- gc("net_col_herb",     df, c("fractions","fraction","herb"))
      m  <- gc("net_col_molecule", df, c("title","molecule","compound"))
      tg <- gc("net_col_target",   df, c("pdb id","pdb_id","target"))
      nd <- df %>%
        dplyr::rename(herb = all_of(h), molecule = all_of(m), target = all_of(tg)) %>%
        dplyr::select(herb, molecule, target) %>% dplyr::distinct()
      if (input$net_n > 0 && nrow(nd) > input$net_n)
        nd <- dplyr::sample_n(nd, input$net_n)
      p <- TCMNP::tcm_net(nd,
                          label.degree    = input$net_label_deg,
                          rem.dis.inter   = input$net_rem_dis,
                          graph.layout    = input$net_layout)
      apply_labels(p, "net", input, show_xy = FALSE)
    })
  })

  output$plot_net <- renderPlot({
    p <- make_net_plot()
    rv$plots[["net"]] <- p
    p
  })
}
