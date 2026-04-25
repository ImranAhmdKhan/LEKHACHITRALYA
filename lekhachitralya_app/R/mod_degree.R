# ============================================================
#  R/mod_degree.R
#  Module    : Degree Plots
#  Description : Analyses node degree (the number of edges
#                attached to a node) in the herb–molecule–target
#                network.  Ten chart types highlight high-degree
#                hubs, cumulative degree accumulation and the
#                Pareto structure of network connectivity.
#
#  Data type   : Network data (herb–molecule–target)
#  Required columns : herb, molecule, target
#
#  Plot types  :
#    degree_plot – TCMNP::degree_plot() (barplot or Cleveland dot)
#    radial      – Radial polar bar of degree per node
#    lollipop    – Lollipop chart of node degrees
#    dot         – Dot chart (size ∝ degree, colour = type)
#    area        – Area chart of rank vs degree per node type
#    cumulative  – Cumulative degree accumulation by type
#    heatstrip   – Degree heat-strip per node type
#    slope       – Mean vs max degree slope per node type
#    pareto      – Pareto chart (bar + cumulative % line)
#    facetbar    – Faceted bar chart by node type
# ============================================================


# ── UI ───────────────────────────────────────────────────────

#' UI for the Degree Plots tab.
#' @return A `shinydashboard::tabItem` object.
degreeUI <- function() {
  tabItem("degree",
    fluidRow(
      box(width = 3, title = "Parameters", status = "primary", solidHeader = TRUE,
        dataset_picker("ds_degree"),
        hr(),
        div(class = "override-group",
          tags$h6("Column roles"),
          uiOutput("deg_col_herb"),
          uiOutput("deg_col_molecule"),
          uiOutput("deg_col_target")
        ),
        hr(),
        selectInput("deg_type", "Plot type",
          c("TCMNP degree plot"  = "degree_plot",
            "Radial bar"         = "radial",
            "Lollipop"           = "lollipop",
            "Dot plot"           = "dot",
            "Area profile"       = "area",
            "Cumulative degree"  = "cumulative",
            "Degree heat strip"  = "heatstrip",
            "Mean vs max slope"  = "slope",
            "Pareto chart"       = "pareto",
            "Facet bar by type"  = "facetbar"),
          selected = "degree_plot"),
        selectInput("deg_orient", "TCMNP orientation",
                    c("vertical" = "v", "horizontal" = "h"), "v"),
        numericInput("deg_top_n",  "Top N nodes / type", 15, 5, 60),
        numericInput("deg_text_w", "Label wrap width",   18, 8, 40),
        sliderInput("deg_alpha",   "Alpha", 0.2, 1, 0.85, step = 0.05),
        selectInput("deg_color",   "Colour palette", PALETTES, "Set2"),
        actionButton("draw_degree", "Draw Degree Plot",
                     class = "btn-primary btn-lg", width = "100%")
      ),
      box(width = 9, title = "Node Degree Analysis",
          status = "primary", solidHeader = TRUE,
        label_theme_box("degree"),
        withSpinner(plotOutput("plot_degree", height = "560px"), type = 6)
      )
    )
  )
}


# ── Server ───────────────────────────────────────────────────

#' Server logic for the Degree Plots tab.
#'
#' @param input   Shiny `input` object.
#' @param output  Shiny `output` object.
#' @param session Shiny `session` object.
#' @param rv      Shared `reactiveValues`.
degreeServer <- function(input, output, session, rv) {

  gc <- function(id, df, pats = NULL) get_col(id, df, pats, input)

  make_ds_picker("ds_degree", "Dataset - degree", output, input, rv)
  make_col_override("deg_col_herb",     "herb",     "sel_ds_degree",
                    c("fractions","fraction","herb"), output, input, rv)
  make_col_override("deg_col_molecule", "molecule", "sel_ds_degree",
                    c("title","molecule"), output, input, rv)
  make_col_override("deg_col_target",   "target",   "sel_ds_degree",
                    c("pdb id","target"), output, input, rv)

  make_degree_plot <- reactive({
    req(input$draw_degree)
    isolate({
      nm <- input$sel_ds_degree; req(nm, rv$datasets[[nm]])
      df <- rv$datasets[[nm]]
      h  <- gc("deg_col_herb",     df, c("fractions","fraction","herb"))
      m  <- gc("deg_col_molecule", df, c("title","molecule"))
      tg <- gc("deg_col_target",   df, c("pdb id","target"))
      df2 <- df %>%
        dplyr::rename(herb = all_of(h), molecule = all_of(m), target = all_of(tg)) %>%
        dplyr::select(herb, molecule, target) %>% dplyr::distinct()

      deg_df <- make_degree_df(df2) %>%
        dplyr::group_by(type) %>%
        dplyr::slice_max(order_by = degree, n = input$deg_top_n, with_ties = FALSE) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(type, dplyr::desc(degree))
      deg_df$id_wrap <- wrap_text_vec(deg_df$id, input$deg_text_w %||% 18)
      deg_df$id_wrap <- factor(deg_df$id_wrap, levels = deg_df$id_wrap)
      alp <- input$deg_alpha %||% 0.85

      p <- switch(input$deg_type,
        degree_plot = TCMNP::degree_plot(df2, plot.set = input$deg_orient),
        radial = {
          ggplot(deg_df, aes(x = id_wrap, y = degree, fill = type)) +
            geom_col(alpha = alp) +
            coord_polar() +
            scale_fill_brewer(palette = input$deg_color %||% "Set2") +
            labs(x = NULL, y = "Degree", title = "Radial degree profile") +
            theme_minimal()
        },
        lollipop = {
          ggplot(deg_df, aes(x = degree, y = id_wrap, colour = type)) +
            geom_segment(aes(x = 0, xend = degree, yend = id_wrap),
                         linewidth = 0.9, alpha = alp) +
            geom_point(size = 3) +
            scale_colour_brewer(palette = input$deg_color %||% "Set2") +
            labs(x = "Degree", y = "Node", title = "Lollipop degree plot") +
            theme_classic()
        },
        dot = {
          ggplot(deg_df, aes(x = degree, y = id_wrap,
                             colour = type, size = degree)) +
            geom_point(alpha = alp) +
            scale_colour_brewer(palette = input$deg_color %||% "Set2") +
            labs(x = "Degree", y = "Node", title = "Dot degree plot") +
            theme_classic()
        },
        area = {
          ad <- deg_df %>%
            dplyr::group_by(type) %>%
            dplyr::arrange(dplyr::desc(degree), .by_group = TRUE) %>%
            dplyr::mutate(rank = dplyr::row_number())
          ggplot(ad, aes(x = rank, y = degree, fill = type, colour = type)) +
            geom_area(alpha = 0.25, position = "identity") +
            geom_line(linewidth = 0.9) +
            labs(x = "Rank within node type", y = "Degree",
                 title = "Area profile of node degree") +
            theme_classic()
        },
        cumulative = {
          cd <- deg_df %>%
            dplyr::group_by(type) %>%
            dplyr::arrange(dplyr::desc(degree), .by_group = TRUE) %>%
            dplyr::mutate(rank = dplyr::row_number(),
                          cum_degree = cumsum(degree))
          ggplot(cd, aes(x = rank, y = cum_degree, colour = type)) +
            geom_line(linewidth = 1) + geom_point(size = 1.8) +
            labs(x = "Rank", y = "Cumulative degree",
                 title = "Cumulative degree accumulation") +
            theme_classic()
        },
        heatstrip = {
          hd <- deg_df %>%
            dplyr::group_by(type) %>%
            dplyr::arrange(dplyr::desc(degree), .by_group = TRUE) %>%
            dplyr::mutate(rank = dplyr::row_number())
          ggplot(hd, aes(x = rank, y = type, fill = degree)) +
            geom_tile(colour = "white", alpha = alp) +
            scale_fill_distiller(palette = "YlOrRd", direction = 1,
                                 name = "Degree") +
            labs(x = "Rank", y = "Node type", title = "Degree heat strip") +
            theme_classic()
        },
        slope = {
          sd <- deg_df %>%
            dplyr::group_by(type) %>%
            dplyr::summarise(mean_degree = mean(degree),
                             max_degree  = max(degree), .groups = "drop") %>%
            tidyr::pivot_longer(c(mean_degree, max_degree),
                                names_to = "metric", values_to = "value")
          ggplot(sd, aes(x = metric, y = value, group = type, colour = type)) +
            geom_line(linewidth = 1.1) + geom_point(size = 3) +
            labs(x = NULL, y = "Degree",
                 title = "Mean vs max degree slope by node type") +
            theme_classic()
        },
        pareto = {
          pd <- deg_df %>%
            dplyr::arrange(dplyr::desc(degree)) %>%
            dplyr::mutate(rank    = dplyr::row_number(),
                          cum_pct = 100 * cumsum(degree) / sum(degree))
          ggplot(pd, aes(x = rank)) +
            geom_col(aes(y = degree, fill = type), alpha = alp) +
            geom_line(aes(y = max(degree) * cum_pct / 100),
                      colour = "black", linewidth = 1) +
            labs(x = "Node rank", y = "Degree", fill = "Type",
                 title = "Pareto chart of node degree") +
            theme_classic()
        },
        facetbar = {
          ggplot(deg_df, aes(x = id_wrap, y = degree, fill = type)) +
            geom_col(alpha = alp, show.legend = FALSE) +
            facet_wrap(~type, scales = "free_y") +
            labs(x = "Node", y = "Degree",
                 title = "Facet degree bars by node type") +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8))
        }
      )
      apply_labels(p, "degree", input)
    })
  })

  output$plot_degree <- renderPlot({
    p <- make_degree_plot()
    rv$plots[["degree"]] <- p
    p
  })
}
