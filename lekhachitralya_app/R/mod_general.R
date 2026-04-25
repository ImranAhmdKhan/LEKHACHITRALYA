# ============================================================
#  R/mod_general.R
#  Module    : General Purpose Plots
#  Description : A flexible, general-purpose visualisation module
#                that works with any tabular dataset uploaded to
#                the app.  The user picks X, Y and optional Color,
#                Size and Label columns, then selects from 82
#                ggplot2-based chart types organised into 10
#                thematic groups.
#
#  Data type   : Any tabular data (numeric or categorical columns)
#  Required    : At minimum one column for X
#  Optional    : Y, Color/Group, Size, Label columns
#
#  Plot groups (82 types)
#  ─────────────────────
#  A. Bar & Column        (15 types)
#  B. Histogram & Density (10 types)
#  C. Box & Violin        ( 8 types)
#  D. Scatter & Bubble    (11 types)
#  E. Line & Area         (10 types)
#  F. Heatmap & Tile      ( 6 types)
#  G. Part-to-Whole       ( 7 types)
#  H. Ranking & Comparison( 5 types)
#  I. Multivariate        ( 5 types)
#  J. Statistical         ( 5 types)
# ============================================================


# ── Helpers ──────────────────────────────────────────────────

# Safely get a palette; fall back to "Spectral"
.gp_pal <- function(name, n) {
  tryCatch(RColorBrewer::brewer.pal(max(3, n), name),
           error = function(e) RColorBrewer::brewer.pal(3, "Spectral"))
}

# Coerce a column to numeric, warning gracefully
.as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

# Wrap long labels
.wl <- function(x, w = 25) stringr::str_wrap(as.character(x), width = w)

# Check if a package is installed
.has_pkg <- function(pkg) requireNamespace(pkg, quietly = TRUE)


# ── UI ───────────────────────────────────────────────────────

#' UI for the General Purpose Plots tab.
#' @return A `shinydashboard::tabItem` object.
generalUI <- function() {
  tabItem("general",
    fluidRow(
      # ── Left panel ────────────────────────────────────────
      box(width = 3, title = "Parameters", status = "warning",
          solidHeader = TRUE,

        dataset_picker("ds_general"),
        hr(),

        # Column selectors (populated by server)
        div(class = "override-group",
          tags$h6("Column roles"),
          uiOutput("gen_col_x"),
          uiOutput("gen_col_y"),
          uiOutput("gen_col_color"),
          uiOutput("gen_col_size"),
          uiOutput("gen_col_label")
        ),
        hr(),

        # Plot-type picker organised into optgroups via list()
        selectInput("gen_type", "Plot type",
          choices = list(
            "A \u2013 Bar & Column" = c(
              "Vertical bar"                = "bar_v",
              "Horizontal bar"              = "bar_h",
              "Grouped bar"                 = "bar_grouped",
              "Stacked bar"                 = "bar_stacked",
              "100% stacked bar"            = "bar_fill",
              "Diverging bar"               = "bar_diverging",
              "Bar + error bars (mean\u00b1SD)" = "bar_error",
              "Lollipop chart"              = "bar_lollipop",
              "Dumbbell / barbell"          = "bar_dumbbell",
              "Cleveland dot plot"          = "bar_cleveland",
              "Waterfall chart"             = "bar_waterfall",
              "Polar / radial bar"          = "bar_polar",
              "Population pyramid"          = "bar_pyramid",
              "Faceted bar"                 = "bar_facet",
              "Pareto chart"                = "bar_pareto"
            ),
            "B \u2013 Histogram & Density" = c(
              "Standard histogram"          = "hist_std",
              "Filled histogram by group"   = "hist_filled",
              "Cumulative histogram"        = "hist_cum",
              "Mirror histogram"            = "hist_mirror",
              "Density curve"               = "density_std",
              "Filled density curves"       = "density_filled",
              "Ridge / Joy plot"            = "density_ridge",
              "ECDF"                        = "ecdf_std",
              "ECDF by group"               = "ecdf_grp",
              "Q-Q plot"                    = "qq_std"
            ),
            "C \u2013 Box & Violin" = c(
              "Box plot"                    = "box_std",
              "Notched box plot"            = "box_notched",
              "Box + jitter"                = "box_jitter",
              "Violin plot"                 = "box_violin",
              "Violin + box combo"          = "box_violin_box",
              "Half violin"                 = "box_half_violin",
              "Letter-value plot"           = "box_letter",
              "Strip / jitter plot"         = "box_strip"
            ),
            "D \u2013 Scatter & Bubble" = c(
              "Scatter plot"                = "scatter_std",
              "Bubble chart"                = "scatter_bubble",
              "Scatter + linear fit"        = "scatter_lm",
              "Scatter + LOESS smoother"    = "scatter_smooth",
              "Scatter + ellipses"          = "scatter_ellipse",
              "Hexagonal binning"           = "scatter_hexbin",
              "2D density overlay"          = "scatter_2d_density",
              "Jitter plot"                 = "scatter_jitter",
              "Labeled scatter (repel)"     = "scatter_label",
              "Connected scatter"           = "scatter_connected",
              "Scatter + rug"               = "scatter_rug",
              "Faceted scatter"             = "scatter_facet",
              "Size + color scatter"        = "scatter_size_color"
            ),
            "E \u2013 Line & Area" = c(
              "Line chart"                  = "line_std",
              "Multi-line chart"            = "line_multi",
              "Line + points"               = "line_pts",
              "Smoothed line"               = "line_smooth",
              "Step line"                   = "line_step",
              "Slope chart"                 = "line_slope",
              "Bump chart"                  = "line_bump",
              "Ribbon / band"               = "line_ribbon",
              "Area chart"                  = "area_std",
              "Stacked area"                = "area_stacked",
              "100% stacked area"           = "area_fill100",
              "Faceted line"                = "line_facet"
            ),
            "F \u2013 Heatmap & Tile" = c(
              "Heatmap (tile)"              = "heat_std",
              "Correlation heatmap"         = "heat_corr",
              "Diverging heatmap"           = "heat_diverging",
              "Bubble / dot heatmap"        = "heat_bubble",
              "Annotated heatmap"           = "heat_annotated",
              "Faceted tile"                = "heat_facet"
            ),
            "G \u2013 Part-to-Whole" = c(
              "Pie chart"                   = "pie_std",
              "Donut chart"                 = "donut_std",
              "Polar area / Rose"           = "polar_rose",
              "Treemap"                     = "treemap_std",
              "Waffle chart"                = "waffle_std",
              "Funnel chart"                = "funnel_std",
              "Population pyramid"          = "pyramid_std"
            ),
            "H \u2013 Ranking & Comparison" = c(
              "Sorted ranking bar"          = "rank_bar",
              "Ranking lollipop"            = "rank_lollipop",
              "Dumbbell ranking"            = "rank_dumbbell",
              "Slope ranking"               = "rank_slope",
              "Bump ranking"                = "rank_bump"
            ),
            "I \u2013 Multivariate" = c(
              "Radar / spider chart"        = "radar_std",
              "Parallel coordinates"        = "parallel_coords",
              "Scatter matrix (pairs)"      = "scatter_matrix",
              "Correlation matrix plot"     = "corr_matrix",
              "Facet grid"                  = "facet_grid_std"
            ),
            "J \u2013 Statistical" = c(
              "Error bar (mean \u00b1 SD/SE)" = "error_bar",
              "Confidence ribbon"           = "error_ribbon",
              "Forest plot"                 = "forest_plot",
              "Stat summary plot"           = "stat_summary_plot",
              "Interaction plot"            = "interaction_plot"
            )
          ),
          selected = "bar_v"
        ),

        hr(),
        # ── Common controls ─────────────────────────────────
        numericInput("gen_top_n",   "Top N rows (0 = all)",  20, 0, 5000),
        numericInput("gen_bins",    "Histogram bins",         30, 5, 100),
        sliderInput( "gen_alpha",   "Alpha",   0.2, 1, 0.8, step = 0.05),
        selectInput( "gen_palette", "Colour palette", PALETTES, "Spectral"),
        checkboxInput("gen_flip",   "Flip coordinates", FALSE),

        actionButton("draw_general", "Draw Plot",
                     class = "btn-warning btn-lg", width = "100%")
      ),

      # ── Right panel ───────────────────────────────────────
      box(width = 9, title = "General Purpose Plot",
          status = "warning", solidHeader = TRUE,
        label_theme_box("general"),
        withSpinner(plotOutput("plot_general", height = "580px"), type = 6)
      )
    )
  )
}


# ── Server ───────────────────────────────────────────────────

#' Server logic for the General Purpose Plots tab.
#'
#' @param input   Shiny `input` object.
#' @param output  Shiny `output` object.
#' @param session Shiny `session` object.
#' @param rv      Shared `reactiveValues`.
generalServer <- function(input, output, session, rv) {

  # ── Column-selector helpers ──────────────────────────────
  make_ds_picker("ds_general", "Dataset", output, input, rv)

  .col_sel <- function(out_id, label, include_none = TRUE) {
    output[[out_id]] <- renderUI({
      nm   <- input$sel_ds_general
      req(nm, rv$datasets[[nm]])
      cols <- names(rv$datasets[[nm]])
      ch   <- if (include_none) c("(none)" = "", cols) else cols
      selectInput(out_id, label, choices = ch)
    })
  }

  .col_sel("gen_col_x",     "X column",            include_none = FALSE)
  .col_sel("gen_col_y",     "Y column")
  .col_sel("gen_col_color", "Color / Group column")
  .col_sel("gen_col_size",  "Size column (bubble)")
  .col_sel("gen_col_label", "Label column")

  # ── Resolve optional column ──────────────────────────────
  .opt <- function(id) {
    v <- input[[id]] %||% ""
    if (nzchar(v)) v else NULL
  }

  # ── Main reactive ────────────────────────────────────────
  make_general_plot <- reactive({
    req(input$draw_general)
    isolate({
      nm <- input$sel_ds_general
      req(nm, rv$datasets[[nm]])
      df <- rv$datasets[[nm]]
      mp <- rv$mappings[[nm]]
      if (!is.null(mp)) df <- remap_df(df, mp)

      x_col   <- input$gen_col_x   %||% names(df)[1]
      y_col   <- .opt("gen_col_y")
      col_col <- .opt("gen_col_color")
      siz_col <- .opt("gen_col_size")
      lab_col <- .opt("gen_col_label")

      req(x_col %in% names(df))

      ptype  <- input$gen_type   %||% "bar_v"
      pal    <- input$gen_palette %||% "Spectral"
      alp    <- input$gen_alpha   %||% 0.8
      top_n  <- input$gen_top_n   %||% 20
      bins   <- input$gen_bins    %||% 30
      do_flip <- isTRUE(input$gen_flip)

      # Optionally limit rows for ranking plots
      if (top_n > 0 && nrow(df) > top_n &&
          ptype %in% c("bar_v","bar_h","bar_lollipop","bar_dumbbell",
                       "bar_cleveland","rank_bar","rank_lollipop",
                       "rank_dumbbell","rank_slope","rank_bump",
                       "bar_pareto","bar_waterfall")) {
        if (!is.null(y_col) && y_col %in% names(df)) {
          df[[y_col]] <- .as_num(df[[y_col]])
          df <- df[order(-df[[y_col]], na.last = TRUE), ]
        }
        df <- head(df, top_n)
      }

      # Scale colours
      pal_scale_fill   <- scale_fill_brewer(palette = pal,   na.value = "#ccc")
      pal_scale_colour <- scale_color_brewer(palette = pal,  na.value = "#ccc")
      pal_scale_cont   <- scale_fill_distiller(palette = pal)

      # ── Dispatch ────────────────────────────────────────
      p <- tryCatch({
        switch(ptype,

          # ══════════════════════════════════════════════════
          # A. BAR & COLUMN
          # ══════════════════════════════════════════════════

          bar_v = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            aes_base <- if (!is.null(col_col))
              aes(x = reorder(.data[[x_col]], .data[[y_col]]),
                  y = .data[[y_col]], fill = .data[[col_col]])
            else
              aes(x = reorder(.data[[x_col]], .data[[y_col]]),
                  y = .data[[y_col]], fill = .data[[x_col]])
            ggplot(df, aes_base) +
              geom_col(alpha = alp) +
              pal_scale_fill +
              labs(x = x_col, y = y_col) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1),
                    legend.position = "none")
          },

          bar_h = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            aes_base <- if (!is.null(col_col))
              aes(x = .data[[y_col]],
                  y = reorder(.data[[x_col]], .data[[y_col]]),
                  fill = .data[[col_col]])
            else
              aes(x = .data[[y_col]],
                  y = reorder(.data[[x_col]], .data[[y_col]]),
                  fill = .data[[x_col]])
            ggplot(df, aes_base) +
              geom_col(alpha = alp) +
              pal_scale_fill +
              labs(x = y_col, y = x_col, fill = col_col %||% x_col) +
              theme(legend.position = "none")
          },

          bar_grouped = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[col_col]])) +
              geom_col(position = "dodge", alpha = alp) +
              pal_scale_fill +
              labs(x = x_col, y = y_col, fill = col_col) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          },

          bar_stacked = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[col_col]])) +
              geom_col(position = "stack", alpha = alp) +
              pal_scale_fill +
              labs(x = x_col, y = y_col, fill = col_col) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          },

          bar_fill = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[col_col]])) +
              geom_col(position = "fill", alpha = alp) +
              scale_y_continuous(labels = scales::percent) +
              pal_scale_fill +
              labs(x = x_col, y = "Proportion", fill = col_col) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          },

          bar_diverging = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            df$sign <- ifelse(df[[y_col]] >= 0, "Positive", "Negative")
            ggplot(df, aes(x = reorder(.data[[x_col]], .data[[y_col]]),
                           y = .data[[y_col]], fill = sign)) +
              geom_col(alpha = alp) +
              geom_hline(yintercept = 0, colour = "black", linewidth = 0.5) +
              coord_flip() +
              scale_fill_manual(values = c("Positive" = "#2ecc71",
                                           "Negative" = "#e74c3c")) +
              labs(x = x_col, y = y_col, fill = NULL)
          },

          bar_error = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            grp <- col_col %||% x_col
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[grp]])) +
              stat_summary(fun = mean, geom = "bar",
                           position = "dodge", alpha = alp) +
              stat_summary(fun.data = mean_sd, geom = "errorbar",
                           position = position_dodge(0.9), width = 0.25) +
              pal_scale_fill +
              labs(x = x_col, y = paste0("Mean ", y_col), fill = grp) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          },

          bar_lollipop = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            col_val <- if (!is.null(col_col)) .data[[col_col]] else .data[[x_col]]
            ggplot(df, aes(x = reorder(.data[[x_col]], .data[[y_col]]),
                           y = .data[[y_col]])) +
              geom_segment(aes(xend = .data[[x_col]], yend = 0),
                           colour = "grey60", linewidth = 0.8) +
              geom_point(aes(colour = col_val), size = 3, alpha = alp) +
              pal_scale_colour +
              coord_flip() +
              labs(x = x_col, y = y_col, colour = col_col %||% x_col) +
              theme(legend.position = "none")
          },

          bar_dumbbell = {
            req(!is.null(col_col), y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            # Two groups per x_col
            grps <- unique(df[[col_col]])
            validate(need(length(grps) >= 2, "Dumbbell needs ≥ 2 color groups"))
            wide <- df %>%
              dplyr::group_by(.data[[x_col]], .data[[col_col]]) %>%
              dplyr::summarise(val = mean(.data[[y_col]], na.rm = TRUE),
                               .groups = "drop") %>%
              tidyr::pivot_wider(names_from = col_col, values_from = val)
            g1 <- as.character(grps[1]); g2 <- as.character(grps[2])
            if (!(g1 %in% names(wide)) || !(g2 %in% names(wide)))
              stop("Cannot pivot dumbbell data")
            ggplot(wide, aes(y = reorder(.data[[x_col]],
                                         .data[[g1]]))) +
              geom_segment(aes(x = .data[[g1]], xend = .data[[g2]],
                               yend = .data[[x_col]]),
                           colour = "grey70", linewidth = 1.5) +
              geom_point(aes(x = .data[[g1]]), colour = "#e74c3c",
                         size = 4, alpha = alp) +
              geom_point(aes(x = .data[[g2]]), colour = "#2980b9",
                         size = 4, alpha = alp) +
              labs(x = y_col, y = x_col,
                   caption = paste0("\u25cf ", g1, "  \u25cf ", g2))
          },

          bar_cleveland = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[y_col]],
                           y = reorder(.data[[x_col]], .data[[y_col]]))) +
              geom_point(size = 3, colour = "#2c3e50", alpha = alp) +
              geom_segment(aes(x = 0, xend = .data[[y_col]],
                               yend = .data[[x_col]]),
                           colour = "grey80", linewidth = 0.5) +
              labs(x = y_col, y = x_col)
          },

          bar_waterfall = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            df <- df[!is.na(df[[y_col]]), ]
            df$cumend   <- cumsum(df[[y_col]])
            df$cumstart <- c(0, head(df$cumend, -1))
            df$sign     <- ifelse(df[[y_col]] >= 0, "Up", "Down")
            ggplot(df, aes(x = .data[[x_col]], fill = sign)) +
              geom_rect(aes(xmin = as.numeric(factor(.data[[x_col]])) - 0.4,
                            xmax = as.numeric(factor(.data[[x_col]])) + 0.4,
                            ymin = cumstart, ymax = cumend),
                        alpha = alp) +
              scale_fill_manual(values = c("Up" = "#27ae60",
                                           "Down" = "#e74c3c")) +
              labs(x = x_col, y = paste0("Cumulative ", y_col), fill = NULL) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          },

          bar_polar = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[x_col]])) +
              geom_col(alpha = alp) +
              coord_polar() +
              pal_scale_fill +
              labs(x = NULL, y = NULL, fill = x_col) +
              theme(axis.text.y = element_blank(),
                    axis.ticks  = element_blank())
          },

          bar_pyramid = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[y_col]] <- .as_num(df[[y_col]])
            grps <- unique(df[[col_col]])
            validate(need(length(grps) == 2,
                          "Population pyramid needs exactly 2 color groups"))
            g1 <- grps[1]; g2 <- grps[2]
            df2 <- df %>%
              dplyr::mutate(val2 = ifelse(.data[[col_col]] == g1,
                                          -.data[[y_col]],
                                           .data[[y_col]]))
            ggplot(df2, aes(x = .data[[x_col]], y = val2,
                            fill = .data[[col_col]])) +
              geom_col(alpha = alp) +
              coord_flip() +
              scale_y_continuous(labels = function(v) abs(v)) +
              scale_fill_manual(values = c("#3498db","#e74c3c")) +
              labs(x = x_col, y = y_col, fill = col_col)
          },

          bar_facet = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[x_col]])) +
              geom_col(alpha = alp) +
              facet_wrap(as.formula(paste0("~", col_col)),
                         scales = "free_y") +
              pal_scale_fill +
              labs(x = x_col, y = y_col) +
              theme(legend.position = "none",
                    axis.text.x = element_text(angle = 45, hjust = 1))
          },

          bar_pareto = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            df <- df[order(-df[[y_col]], na.last = TRUE), ]
            df[[x_col]] <- factor(df[[x_col]], levels = df[[x_col]])
            df$cum_pct  <- cumsum(df[[y_col]]) / sum(df[[y_col]], na.rm=TRUE)*100
            ggplot(df, aes(x = .data[[x_col]])) +
              geom_col(aes(y = .data[[y_col]]), fill = "#3498db",
                       alpha = alp) +
              geom_line(aes(y = cum_pct / 100 * max(df[[y_col]],
                                                     na.rm = TRUE),
                            group = 1),
                        colour = "#e74c3c", linewidth = 1) +
              geom_point(aes(y = cum_pct / 100 * max(df[[y_col]],
                                                       na.rm = TRUE)),
                         colour = "#e74c3c", size = 2) +
              scale_y_continuous(
                sec.axis = sec_axis(
                  ~ . / max(df[[y_col]], na.rm = TRUE) * 100,
                  name = "Cumulative %"
                )
              ) +
              labs(x = x_col, y = y_col) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          },

          # ══════════════════════════════════════════════════
          # B. HISTOGRAM & DENSITY
          # ══════════════════════════════════════════════════

          hist_std = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            fill_aes <- if (!is.null(col_col))
              aes(x = .data[[y_col]], fill = .data[[col_col]])
            else aes(x = .data[[y_col]])
            ggplot(df, fill_aes) +
              geom_histogram(bins = bins, alpha = alp, colour = "white") +
              (if (!is.null(col_col)) pal_scale_fill else
                scale_fill_manual(values = "#3498db")) +
              labs(x = y_col, y = "Count", fill = col_col %||% NULL)
          },

          hist_filled = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[y_col]], fill = .data[[col_col]])) +
              geom_histogram(bins = bins, alpha = alp, colour = "white",
                             position = "fill") +
              scale_y_continuous(labels = scales::percent) +
              pal_scale_fill +
              labs(x = y_col, y = "Proportion", fill = col_col)
          },

          hist_cum = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[y_col]])) +
              geom_histogram(aes(y = cumsum(after_stat(count))),
                             bins = bins, fill = "#2980b9", alpha = alp,
                             colour = "white") +
              labs(x = y_col, y = "Cumulative count")
          },

          hist_mirror = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[y_col]] <- .as_num(df[[y_col]])
            grps <- unique(df[[col_col]])
            validate(need(length(grps) >= 2,
                          "Mirror histogram needs ≥ 2 color groups"))
            g1 <- grps[1]; g2 <- grps[2]
            d1 <- df[df[[col_col]] == g1, ]
            d2 <- df[df[[col_col]] == g2, ]
            brks <- seq(min(df[[y_col]], na.rm=TRUE),
                        max(df[[y_col]], na.rm=TRUE),
                        length.out = bins + 1)
            ggplot() +
              geom_histogram(data = d1,
                             aes(x = .data[[y_col]]),
                             breaks = brks, fill = "#3498db",
                             alpha = alp, colour = "white") +
              geom_histogram(data = d2,
                             aes(x = .data[[y_col]],
                                 y = -after_stat(count)),
                             breaks = brks, fill = "#e74c3c",
                             alpha = alp, colour = "white") +
              geom_hline(yintercept = 0, colour = "black") +
              scale_y_continuous(labels = abs) +
              labs(x = y_col, y = "Count",
                   caption = paste0("\u25b2 ", g1, "  \u25bc ", g2))
          },

          density_std = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            fill_col <- col_col %||% NULL
            aes_d <- if (!is.null(fill_col))
              aes(x = .data[[y_col]], fill = .data[[fill_col]],
                  colour = .data[[fill_col]])
            else aes(x = .data[[y_col]])
            ggplot(df, aes_d) +
              geom_density(alpha = alp * 0.7) +
              (if (!is.null(fill_col)) pal_scale_fill else
                scale_fill_manual(values = "#3498db")) +
              (if (!is.null(fill_col)) pal_scale_colour else
                scale_color_manual(values = "#3498db")) +
              labs(x = y_col, y = "Density", fill = fill_col %||% NULL)
          },

          density_filled = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[y_col]], fill = .data[[col_col]],
                           colour = .data[[col_col]])) +
              geom_density(alpha = alp * 0.6) +
              pal_scale_fill + pal_scale_colour +
              labs(x = y_col, y = "Density", fill = col_col)
          },

          density_ridge = {
            req(y_col %in% names(df), !is.null(col_col))
            validate(need(.has_pkg("ggridges"),
                          "Install ggridges: install.packages('ggridges')"))
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[y_col]],
                           y = .data[[col_col]],
                           fill = .data[[col_col]])) +
              ggridges::geom_density_ridges(alpha = alp, scale = 1.2,
                                            rel_min_height = 0.01) +
              pal_scale_fill +
              labs(x = y_col, y = col_col) +
              theme(legend.position = "none")
          },

          ecdf_std = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[y_col]])) +
              stat_ecdf(linewidth = 1, colour = "#2c3e50") +
              labs(x = y_col, y = "Cumulative probability")
          },

          ecdf_grp = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[y_col]],
                           colour = .data[[col_col]])) +
              stat_ecdf(linewidth = 1, alpha = alp) +
              pal_scale_colour +
              labs(x = y_col, y = "Cumulative probability", colour = col_col)
          },

          qq_std = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            if (!is.null(col_col))
              ggplot(df, aes(sample = .data[[y_col]],
                             colour = .data[[col_col]])) +
                stat_qq(alpha = alp) + stat_qq_line() +
                pal_scale_colour +
                labs(x = "Theoretical quantiles",
                     y = paste0("Sample quantiles (", y_col, ")"),
                     colour = col_col)
            else
              ggplot(df, aes(sample = .data[[y_col]])) +
                stat_qq(alpha = alp, colour = "#2c3e50") +
                stat_qq_line(colour = "#e74c3c") +
                labs(x = "Theoretical quantiles",
                     y = paste0("Sample quantiles (", y_col, ")"))
          },

          # ══════════════════════════════════════════════════
          # C. BOX & VIOLIN
          # ══════════════════════════════════════════════════

          box_std = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            aes_b <- if (!is.null(col_col))
              aes(x = .data[[x_col]], y = .data[[y_col]],
                  fill = .data[[col_col]])
            else aes(x = .data[[x_col]], y = .data[[y_col]],
                     fill = .data[[x_col]])
            ggplot(df, aes_b) +
              geom_boxplot(alpha = alp, outlier.alpha = 0.5) +
              pal_scale_fill +
              labs(x = x_col, y = y_col,
                   fill = col_col %||% x_col) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1),
                    legend.position = "none")
          },

          box_notched = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[x_col]])) +
              geom_boxplot(notch = TRUE, alpha = alp,
                           outlier.alpha = 0.5) +
              pal_scale_fill +
              labs(x = x_col, y = y_col) +
              theme(legend.position = "none",
                    axis.text.x = element_text(angle = 45, hjust = 1))
          },

          box_jitter = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[x_col]])) +
              geom_boxplot(alpha = alp * 0.7, outlier.shape = NA) +
              geom_jitter(width = 0.15, alpha = alp * 0.5, size = 1.5) +
              pal_scale_fill +
              labs(x = x_col, y = y_col) +
              theme(legend.position = "none",
                    axis.text.x = element_text(angle = 45, hjust = 1))
          },

          box_violin = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[x_col]])) +
              geom_violin(alpha = alp, draw_quantiles = c(0.25,0.5,0.75)) +
              pal_scale_fill +
              labs(x = x_col, y = y_col) +
              theme(legend.position = "none",
                    axis.text.x = element_text(angle = 45, hjust = 1))
          },

          box_violin_box = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[x_col]])) +
              geom_violin(alpha = alp * 0.6, colour = NA) +
              geom_boxplot(width = 0.12, alpha = 0.9,
                           outlier.shape = NA) +
              pal_scale_fill +
              labs(x = x_col, y = y_col) +
              theme(legend.position = "none",
                    axis.text.x = element_text(angle = 45, hjust = 1))
          },

          box_half_violin = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[x_col]])) +
              geom_violin(alpha = alp, trim = FALSE) +
              stat_summary(fun.data = mean_sdl, fun.args = list(mult=1),
                           geom = "pointrange", size = 0.5) +
              pal_scale_fill +
              coord_flip() +
              labs(x = x_col, y = y_col) +
              theme(legend.position = "none")
          },

          box_letter = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            # Approximate letter-value with stacked quantile boxes
            quants <- c(0.05,0.125,0.25,0.5,0.75,0.875,0.95)
            lvl_data <- df %>%
              dplyr::group_by(.data[[x_col]]) %>%
              dplyr::summarise(
                q5   = quantile(.data[[y_col]], 0.05, na.rm=TRUE),
                q125 = quantile(.data[[y_col]], 0.125,na.rm=TRUE),
                q25  = quantile(.data[[y_col]], 0.25, na.rm=TRUE),
                q50  = quantile(.data[[y_col]], 0.5,  na.rm=TRUE),
                q75  = quantile(.data[[y_col]], 0.75, na.rm=TRUE),
                q875 = quantile(.data[[y_col]], 0.875,na.rm=TRUE),
                q95  = quantile(.data[[y_col]], 0.95, na.rm=TRUE),
                .groups = "drop"
              )
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[x_col]])) +
              geom_boxplot(alpha = alp * 0.5, outlier.shape = NA,
                           coef = 3) +
              geom_violin(alpha = alp * 0.3, colour = NA) +
              pal_scale_fill +
              labs(x = x_col, y = y_col,
                   subtitle = "Letter-value approximation (violin + wide box)") +
              theme(legend.position = "none",
                    axis.text.x = element_text(angle = 45, hjust = 1))
          },

          box_strip = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            aes_s <- if (!is.null(col_col))
              aes(x = .data[[x_col]], y = .data[[y_col]],
                  colour = .data[[col_col]])
            else aes(x = .data[[x_col]], y = .data[[y_col]],
                     colour = .data[[x_col]])
            ggplot(df, aes_s) +
              geom_jitter(width = 0.2, alpha = alp, size = 2) +
              pal_scale_colour +
              labs(x = x_col, y = y_col,
                   colour = col_col %||% x_col) +
              theme(legend.position = "none",
                    axis.text.x = element_text(angle = 45, hjust = 1))
          },

          # ══════════════════════════════════════════════════
          # D. SCATTER & BUBBLE
          # ══════════════════════════════════════════════════

          scatter_std = {
            req(y_col %in% names(df))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            aes_s <- if (!is.null(col_col))
              aes(x = .data[[x_col]], y = .data[[y_col]],
                  colour = .data[[col_col]])
            else aes(x = .data[[x_col]], y = .data[[y_col]])
            ggplot(df, aes_s) +
              geom_point(alpha = alp, size = 2.5) +
              (if (!is.null(col_col)) pal_scale_colour else
                scale_color_manual(values = "#2c3e50")) +
              labs(x = x_col, y = y_col, colour = col_col %||% NULL)
          },

          scatter_bubble = {
            req(y_col %in% names(df), !is.null(siz_col))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            df[[siz_col]] <- .as_num(df[[siz_col]])
            aes_b <- if (!is.null(col_col))
              aes(x = .data[[x_col]], y = .data[[y_col]],
                  size = .data[[siz_col]], colour = .data[[col_col]])
            else aes(x = .data[[x_col]], y = .data[[y_col]],
                     size = .data[[siz_col]])
            ggplot(df, aes_b) +
              geom_point(alpha = alp) +
              scale_size_area(max_size = 16) +
              (if (!is.null(col_col)) pal_scale_colour else
                scale_color_manual(values = "#3498db")) +
              labs(x = x_col, y = y_col,
                   size = siz_col, colour = col_col %||% NULL)
          },

          scatter_lm = {
            req(y_col %in% names(df))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            aes_s <- if (!is.null(col_col))
              aes(x = .data[[x_col]], y = .data[[y_col]],
                  colour = .data[[col_col]])
            else aes(x = .data[[x_col]], y = .data[[y_col]])
            ggplot(df, aes_s) +
              geom_point(alpha = alp, size = 2) +
              geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                          alpha = 0.2) +
              (if (!is.null(col_col)) pal_scale_colour else
                scale_color_manual(values = "#2c3e50")) +
              labs(x = x_col, y = y_col, colour = col_col %||% NULL)
          },

          scatter_smooth = {
            req(y_col %in% names(df))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            aes_s <- if (!is.null(col_col))
              aes(x = .data[[x_col]], y = .data[[y_col]],
                  colour = .data[[col_col]])
            else aes(x = .data[[x_col]], y = .data[[y_col]])
            ggplot(df, aes_s) +
              geom_point(alpha = alp * 0.5, size = 1.8) +
              geom_smooth(method = "loess", formula = y ~ x,
                          se = TRUE, alpha = 0.2) +
              (if (!is.null(col_col)) pal_scale_colour else
                scale_color_manual(values = "#2c3e50")) +
              labs(x = x_col, y = y_col, colour = col_col %||% NULL)
          },

          scatter_ellipse = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           colour = .data[[col_col]])) +
              geom_point(alpha = alp, size = 2) +
              stat_ellipse(level = 0.95, linewidth = 1) +
              pal_scale_colour +
              labs(x = x_col, y = y_col, colour = col_col)
          },

          scatter_hexbin = {
            req(y_col %in% names(df))
            validate(need(.has_pkg("hexbin"),
                          "Install hexbin: install.packages('hexbin')"))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
              geom_hex(bins = 25, alpha = alp) +
              scale_fill_distiller(palette = pal, direction = 1) +
              labs(x = x_col, y = y_col, fill = "Count")
          },

          scatter_2d_density = {
            req(y_col %in% names(df))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
              geom_point(alpha = alp * 0.4, size = 1.5,
                         colour = "#2c3e50") +
              geom_density_2d_filled(alpha = 0.5, bins = 8) +
              scale_fill_brewer(palette = pal) +
              labs(x = x_col, y = y_col, fill = "Density level")
          },

          scatter_jitter = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            aes_j <- if (!is.null(col_col))
              aes(x = .data[[x_col]], y = .data[[y_col]],
                  colour = .data[[col_col]])
            else aes(x = .data[[x_col]], y = .data[[y_col]],
                     colour = .data[[x_col]])
            ggplot(df, aes_j) +
              geom_jitter(width = 0.25, alpha = alp, size = 2.5) +
              pal_scale_colour +
              labs(x = x_col, y = y_col,
                   colour = col_col %||% x_col) +
              theme(legend.position = "none",
                    axis.text.x = element_text(angle = 45, hjust = 1))
          },

          scatter_label = {
            req(y_col %in% names(df))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            lbl <- lab_col %||% row.names(df)
            if (.has_pkg("ggrepel")) {
              aes_s <- if (!is.null(col_col))
                aes(x = .data[[x_col]], y = .data[[y_col]],
                    colour = .data[[col_col]],
                    label = if (!is.null(lab_col)) .data[[lab_col]]
                            else rownames(df))
              else
                aes(x = .data[[x_col]], y = .data[[y_col]],
                    label = if (!is.null(lab_col)) .data[[lab_col]]
                            else rownames(df))
              ggplot(df, aes_s) +
                geom_point(alpha = alp, size = 2) +
                ggrepel::geom_text_repel(size = 3, max.overlaps = 20) +
                (if (!is.null(col_col)) pal_scale_colour else
                  scale_color_manual(values = "#2c3e50")) +
                labs(x = x_col, y = y_col, colour = col_col %||% NULL)
            } else {
              ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
                geom_point(alpha = alp, size = 2, colour = "#2c3e50") +
                geom_text(
                  aes(label = if (!is.null(lab_col)) .data[[lab_col]]
                               else rownames(df)),
                  size = 3, vjust = -0.8, check_overlap = TRUE) +
                labs(x = x_col, y = y_col)
            }
          },

          scatter_connected = {
            req(y_col %in% names(df))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
              geom_path(colour = "#2c3e50", alpha = alp, linewidth = 0.8) +
              geom_point(colour = "#e74c3c", size = 2.5, alpha = alp) +
              labs(x = x_col, y = y_col)
          },

          scatter_rug = {
            req(y_col %in% names(df))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            aes_s <- if (!is.null(col_col))
              aes(x = .data[[x_col]], y = .data[[y_col]],
                  colour = .data[[col_col]])
            else aes(x = .data[[x_col]], y = .data[[y_col]])
            ggplot(df, aes_s) +
              geom_point(alpha = alp, size = 2) +
              geom_rug(alpha = 0.3, linewidth = 0.4) +
              (if (!is.null(col_col)) pal_scale_colour else
                scale_color_manual(values = "#2c3e50")) +
              labs(x = x_col, y = y_col, colour = col_col %||% NULL)
          },

          scatter_facet = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
              geom_point(alpha = alp, size = 2, colour = "#2c3e50") +
              geom_smooth(method = "lm", formula = y ~ x,
                          se = FALSE, colour = "#e74c3c", linewidth = 0.8) +
              facet_wrap(as.formula(paste0("~", col_col))) +
              labs(x = x_col, y = y_col)
          },

          scatter_size_color = {
            req(y_col %in% names(df), !is.null(col_col),
                !is.null(siz_col))
            df[[x_col]]   <- .as_num(df[[x_col]])
            df[[y_col]]   <- .as_num(df[[y_col]])
            df[[siz_col]] <- .as_num(df[[siz_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           size = .data[[siz_col]],
                           colour = .data[[col_col]])) +
              geom_point(alpha = alp) +
              scale_size_area(max_size = 14) +
              pal_scale_colour +
              labs(x = x_col, y = y_col,
                   size = siz_col, colour = col_col)
          },

          # ══════════════════════════════════════════════════
          # E. LINE & AREA
          # ══════════════════════════════════════════════════

          line_std = {
            req(y_col %in% names(df))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            grp <- col_col %||% "1"
            aes_l <- if (!is.null(col_col))
              aes(x = .data[[x_col]], y = .data[[y_col]],
                  colour = .data[[col_col]], group = .data[[col_col]])
            else aes(x = .data[[x_col]], y = .data[[y_col]], group = 1)
            ggplot(df, aes_l) +
              geom_line(linewidth = 0.9, alpha = alp) +
              (if (!is.null(col_col)) pal_scale_colour else
                scale_color_manual(values = "#2c3e50")) +
              labs(x = x_col, y = y_col, colour = col_col %||% NULL)
          },

          line_multi = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           colour = .data[[col_col]],
                           group  = .data[[col_col]])) +
              geom_line(linewidth = 1, alpha = alp) +
              pal_scale_colour +
              labs(x = x_col, y = y_col, colour = col_col)
          },

          line_pts = {
            req(y_col %in% names(df))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            aes_l <- if (!is.null(col_col))
              aes(x = .data[[x_col]], y = .data[[y_col]],
                  colour = .data[[col_col]], group = .data[[col_col]])
            else aes(x = .data[[x_col]], y = .data[[y_col]], group = 1)
            ggplot(df, aes_l) +
              geom_line(linewidth = 0.9, alpha = alp) +
              geom_point(size = 2.5, alpha = alp) +
              (if (!is.null(col_col)) pal_scale_colour else
                scale_color_manual(values = "#2c3e50")) +
              labs(x = x_col, y = y_col, colour = col_col %||% NULL)
          },

          line_smooth = {
            req(y_col %in% names(df))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            aes_s <- if (!is.null(col_col))
              aes(x = .data[[x_col]], y = .data[[y_col]],
                  colour = .data[[col_col]])
            else aes(x = .data[[x_col]], y = .data[[y_col]])
            ggplot(df, aes_s) +
              geom_smooth(method = "loess", formula = y ~ x,
                          se = TRUE, alpha = 0.2, linewidth = 1) +
              (if (!is.null(col_col)) pal_scale_colour else
                scale_color_manual(values = "#2c3e50")) +
              labs(x = x_col, y = y_col, colour = col_col %||% NULL)
          },

          line_step = {
            req(y_col %in% names(df))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            aes_l <- if (!is.null(col_col))
              aes(x = .data[[x_col]], y = .data[[y_col]],
                  colour = .data[[col_col]], group = .data[[col_col]])
            else aes(x = .data[[x_col]], y = .data[[y_col]], group = 1)
            ggplot(df, aes_l) +
              geom_step(linewidth = 0.9, alpha = alp) +
              (if (!is.null(col_col)) pal_scale_colour else
                scale_color_manual(values = "#2c3e50")) +
              labs(x = x_col, y = y_col, colour = col_col %||% NULL)
          },

          line_slope = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[y_col]] <- .as_num(df[[y_col]])
            # Two groups on x_col (before/after)
            grps <- sort(unique(df[[x_col]]))
            validate(need(length(grps) >= 2,
                          "Slope chart: X column needs ≥ 2 unique values"))
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           group = .data[[col_col]],
                           colour = .data[[col_col]])) +
              geom_line(linewidth = 1.2, alpha = alp) +
              geom_point(size = 4, alpha = alp) +
              pal_scale_colour +
              labs(x = x_col, y = y_col, colour = col_col)
          },

          line_bump = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           colour = .data[[col_col]],
                           group  = .data[[col_col]])) +
              geom_line(linewidth = 1.2, alpha = alp) +
              geom_point(size = 3, alpha = alp) +
              scale_y_reverse(breaks = seq(1, nrow(df))) +
              pal_scale_colour +
              labs(x = x_col, y = paste0("Rank (", y_col, ")"),
                   colour = col_col)
          },

          line_ribbon = {
            req(!is.null(col_col), !is.null(siz_col))
            df[[x_col]]   <- .as_num(df[[x_col]])
            df[[col_col]] <- .as_num(df[[col_col]])
            df[[siz_col]] <- .as_num(df[[siz_col]])
            ggplot(df, aes(x = .data[[x_col]])) +
              geom_ribbon(aes(ymin = .data[[col_col]],
                              ymax = .data[[siz_col]]),
                          fill = "#3498db", alpha = alp * 0.4) +
              geom_line(aes(y = (.data[[col_col]] +
                                   .data[[siz_col]]) / 2),
                        linewidth = 1, colour = "#2c3e50") +
              labs(x = x_col,
                   y = paste0(col_col, " \u2013 ", siz_col, " band"))
          },

          area_std = {
            req(y_col %in% names(df))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            aes_a <- if (!is.null(col_col))
              aes(x = .data[[x_col]], y = .data[[y_col]],
                  fill = .data[[col_col]], group = .data[[col_col]])
            else aes(x = .data[[x_col]], y = .data[[y_col]], group = 1)
            ggplot(df, aes_a) +
              geom_area(alpha = alp) +
              (if (!is.null(col_col)) pal_scale_fill else
                scale_fill_manual(values = "#3498db")) +
              labs(x = x_col, y = y_col, fill = col_col %||% NULL)
          },

          area_stacked = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[col_col]],
                           group = .data[[col_col]])) +
              geom_area(position = "stack", alpha = alp) +
              pal_scale_fill +
              labs(x = x_col, y = y_col, fill = col_col)
          },

          area_fill100 = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[col_col]],
                           group = .data[[col_col]])) +
              geom_area(position = "fill", alpha = alp) +
              scale_y_continuous(labels = scales::percent) +
              pal_scale_fill +
              labs(x = x_col, y = "Proportion", fill = col_col)
          },

          line_facet = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           group = 1)) +
              geom_line(linewidth = 0.9, alpha = alp,
                        colour = "#2c3e50") +
              geom_point(size = 2, colour = "#e74c3c", alpha = alp) +
              facet_wrap(as.formula(paste0("~", col_col)),
                         scales = "free_y") +
              labs(x = x_col, y = y_col)
          },

          # ══════════════════════════════════════════════════
          # F. HEATMAP & TILE
          # ══════════════════════════════════════════════════

          heat_std = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[col_col]] <- .as_num(df[[col_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[col_col]])) +
              geom_tile(colour = "white", alpha = alp) +
              pal_scale_cont +
              labs(x = x_col, y = y_col, fill = col_col) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          },

          heat_corr = {
            num_cols <- names(df)[sapply(df, function(c)
              is.numeric(c) || suppressWarnings(!all(is.na(as.numeric(c)))))]
            num_cols <- num_cols[sapply(df[num_cols], function(c)
              !all(is.na(suppressWarnings(as.numeric(c)))))]
            validate(need(length(num_cols) >= 2,
                          "Need ≥ 2 numeric columns for correlation heatmap"))
            num_df <- df[num_cols]
            num_df[] <- lapply(num_df, .as_num)
            cor_mat  <- cor(num_df, use = "pairwise.complete.obs")
            cor_df   <- as.data.frame(as.table(cor_mat))
            names(cor_df) <- c("Var1","Var2","Correlation")
            ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
              geom_tile(colour = "white") +
              geom_text(aes(label = sprintf("%.2f", Correlation)),
                        size = 3) +
              scale_fill_gradient2(low = "#e74c3c", mid = "white",
                                   high = "#2980b9", midpoint = 0,
                                   limits = c(-1, 1)) +
              labs(x = NULL, y = NULL, fill = "r") +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          },

          heat_diverging = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[col_col]] <- .as_num(df[[col_col]])
            mid_val <- mean(df[[col_col]], na.rm = TRUE)
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[col_col]])) +
              geom_tile(colour = "white", alpha = alp) +
              scale_fill_gradient2(low = "#e74c3c", mid = "white",
                                   high = "#2980b9",
                                   midpoint = mid_val) +
              labs(x = x_col, y = y_col, fill = col_col) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          },

          heat_bubble = {
            req(y_col %in% names(df), !is.null(col_col))
            siz <- siz_col %||% col_col
            df[[col_col]] <- .as_num(df[[col_col]])
            df[[siz]]     <- .as_num(df[[siz]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           size = .data[[siz]],
                           colour = .data[[col_col]])) +
              geom_point(alpha = alp) +
              scale_size_area(max_size = 12) +
              scale_color_distiller(palette = pal, direction = 1) +
              labs(x = x_col, y = y_col,
                   size = siz, colour = col_col) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          },

          heat_annotated = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[col_col]] <- .as_num(df[[col_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[col_col]])) +
              geom_tile(colour = "white", alpha = alp) +
              geom_text(aes(label = sprintf("%.1f", .data[[col_col]])),
                        size = 3, colour = "black") +
              pal_scale_cont +
              labs(x = x_col, y = y_col, fill = col_col) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          },

          heat_facet = {
            req(y_col %in% names(df), !is.null(col_col),
                !is.null(siz_col))
            df[[col_col]] <- .as_num(df[[col_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[col_col]])) +
              geom_tile(colour = "white", alpha = alp) +
              pal_scale_cont +
              facet_wrap(as.formula(paste0("~", siz_col))) +
              labs(x = x_col, y = y_col, fill = col_col) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          },

          # ══════════════════════════════════════════════════
          # G. PART-TO-WHOLE
          # ══════════════════════════════════════════════════

          pie_std = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            df <- df[!is.na(df[[y_col]]), ]
            ggplot(df, aes(x = "", y = .data[[y_col]],
                           fill = .data[[x_col]])) +
              geom_col(alpha = alp, colour = "white") +
              coord_polar(theta = "y") +
              pal_scale_fill +
              labs(x = NULL, y = NULL, fill = x_col) +
              theme_void() +
              theme(legend.position = "right")
          },

          donut_std = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            df <- df[!is.na(df[[y_col]]), ]
            ggplot(df, aes(x = 2, y = .data[[y_col]],
                           fill = .data[[x_col]])) +
              geom_col(alpha = alp, colour = "white") +
              coord_polar(theta = "y") +
              xlim(0.5, 2.5) +
              pal_scale_fill +
              labs(x = NULL, y = NULL, fill = x_col) +
              theme_void() +
              theme(legend.position = "right")
          },

          polar_rose = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[x_col]])) +
              geom_col(alpha = alp, colour = "white") +
              coord_polar(start = 0) +
              pal_scale_fill +
              labs(x = NULL, y = NULL, fill = x_col,
                   subtitle = "Polar area / Rose chart") +
              theme(legend.position = "right",
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank())
          },

          treemap_std = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            df <- df[!is.na(df[[y_col]]) & df[[y_col]] > 0, ]
            validate(need(nrow(df) > 0, "No positive values for treemap"))
            if (.has_pkg("treemapify")) {
              aes_t <- if (!is.null(col_col))
                aes(area = .data[[y_col]],
                    fill = .data[[col_col]],
                    label = .data[[x_col]])
              else aes(area = .data[[y_col]],
                       fill = .data[[x_col]],
                       label = .data[[x_col]])
              ggplot(df, aes_t) +
                treemapify::geom_treemap(alpha = alp) +
                treemapify::geom_treemap_text(
                  colour = "white", place = "centre",
                  size = 11, grow = FALSE) +
                (if (!is.null(col_col)) pal_scale_fill else
                  scale_fill_brewer(palette = pal)) +
                labs(fill = col_col %||% x_col) +
                theme(legend.position = "right")
            } else {
              # Approximate treemap with scaled tiles
              df <- df[order(-df[[y_col]]), ]
              df$tile_x <- seq_len(nrow(df)) %% 5
              df$tile_y <- seq_len(nrow(df)) %/% 5
              ggplot(df, aes(x = tile_x, y = tile_y,
                             fill = .data[[x_col]],
                             width  = sqrt(.data[[y_col]]) / max(sqrt(df[[y_col]])),
                             height = sqrt(.data[[y_col]]) / max(sqrt(df[[y_col]])))) +
                geom_tile(alpha = alp) +
                geom_text(aes(label = paste0(.data[[x_col]], "\n",
                                             round(.data[[y_col]], 1))),
                          size = 3) +
                pal_scale_fill +
                labs(subtitle = "Treemap (install treemapify for true treemap)",
                     fill = x_col) +
                theme_void()
            }
          },

          waffle_std = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            df <- df[!is.na(df[[y_col]]) & df[[y_col]] > 0, ]
            total <- sum(df[[y_col]])
            df$cells <- round(df[[y_col]] / total * 100)
            waffle_df <- do.call(rbind, lapply(seq_len(nrow(df)), function(i)
              if (df$cells[i] > 0)
                data.frame(cat = df[[x_col]][i],
                           seq = seq_len(df$cells[i]))
              else NULL
            ))
            if (is.null(waffle_df) || nrow(waffle_df) == 0)
              stop("No cells to draw")
            waffle_df$x <- (seq_len(nrow(waffle_df)) - 1) %% 10
            waffle_df$y <- (seq_len(nrow(waffle_df)) - 1) %/% 10
            ggplot(waffle_df, aes(x = x, y = y, fill = cat)) +
              geom_tile(colour = "white", linewidth = 0.8, alpha = alp) +
              coord_equal() +
              pal_scale_fill +
              labs(fill = x_col,
                   subtitle = "1 square = 1% of total") +
              theme_void() +
              theme(legend.position = "right")
          },

          funnel_std = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            df <- df[order(-df[[y_col]], na.last = TRUE), ]
            df[[x_col]] <- factor(df[[x_col]], levels = rev(df[[x_col]]))
            ggplot(df, aes(x = .data[[y_col]],
                           y = .data[[x_col]],
                           fill = .data[[x_col]])) +
              geom_col(alpha = alp) +
              geom_text(aes(label = scales::comma(.data[[y_col]])),
                        hjust = -0.1, size = 3.5) +
              pal_scale_fill +
              labs(x = y_col, y = NULL, fill = NULL) +
              theme(legend.position = "none")
          },

          pyramid_std = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[y_col]] <- .as_num(df[[y_col]])
            grps <- unique(df[[col_col]])
            validate(need(length(grps) == 2,
                          "Population pyramid needs exactly 2 groups"))
            df2 <- df %>%
              dplyr::mutate(val2 = ifelse(
                .data[[col_col]] == grps[1],
                -.data[[y_col]], .data[[y_col]]))
            ggplot(df2, aes(x = val2, y = .data[[x_col]],
                            fill = .data[[col_col]])) +
              geom_col(alpha = alp) +
              geom_vline(xintercept = 0, linewidth = 0.6) +
              scale_x_continuous(labels = function(v) abs(v)) +
              scale_fill_manual(values = c("#3498db","#e74c3c")) +
              labs(x = y_col, y = x_col, fill = col_col)
          },

          # ══════════════════════════════════════════════════
          # H. RANKING & COMPARISON
          # ══════════════════════════════════════════════════

          rank_bar = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            df <- df[order(-df[[y_col]], na.last = TRUE), ]
            df[[x_col]] <- factor(df[[x_col]], levels = rev(df[[x_col]]))
            ggplot(df, aes(x = .data[[y_col]],
                           y = .data[[x_col]],
                           fill = .data[[x_col]])) +
              geom_col(alpha = alp) +
              pal_scale_fill +
              labs(x = y_col, y = x_col) +
              theme(legend.position = "none")
          },

          rank_lollipop = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            df <- df[order(-df[[y_col]], na.last = TRUE), ]
            df[[x_col]] <- factor(df[[x_col]], levels = rev(df[[x_col]]))
            ggplot(df, aes(x = .data[[y_col]],
                           y = .data[[x_col]])) +
              geom_segment(aes(x = 0, xend = .data[[y_col]],
                               yend = .data[[x_col]]),
                           colour = "grey70", linewidth = 0.8) +
              geom_point(aes(colour = .data[[x_col]]),
                         size = 4, alpha = alp) +
              pal_scale_colour +
              labs(x = y_col, y = x_col) +
              theme(legend.position = "none")
          },

          rank_dumbbell = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[y_col]] <- .as_num(df[[y_col]])
            grps <- unique(df[[col_col]])
            validate(need(length(grps) >= 2,
                          "Dumbbell needs ≥ 2 groups in Color column"))
            wide <- df %>%
              dplyr::group_by(.data[[x_col]], .data[[col_col]]) %>%
              dplyr::summarise(val = mean(.data[[y_col]], na.rm = TRUE),
                               .groups = "drop") %>%
              tidyr::pivot_wider(names_from  = col_col,
                                 values_from = val)
            g1 <- as.character(grps[1]); g2 <- as.character(grps[2])
            if (!(g1 %in% names(wide)) || !(g2 %in% names(wide)))
              stop("Dumbbell pivot failed: ensure ≥2 groups in Color column")
            wide <- wide[order(wide[[g1]], na.last = TRUE), ]
            wide[[x_col]] <- factor(wide[[x_col]],
                                    levels = wide[[x_col]])
            ggplot(wide, aes(y = .data[[x_col]])) +
              geom_segment(aes(x = .data[[g1]], xend = .data[[g2]],
                               yend = .data[[x_col]]),
                           colour = "grey70", linewidth = 1.5) +
              geom_point(aes(x = .data[[g1]]), colour = "#e74c3c",
                         size = 4, alpha = alp) +
              geom_point(aes(x = .data[[g2]]), colour = "#2980b9",
                         size = 4, alpha = alp) +
              labs(x = y_col, y = x_col,
                   caption = paste0("\u25cf ", g1, " (red)  ",
                                    "\u25cf ", g2, " (blue)"))
          },

          rank_slope = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[y_col]] <- .as_num(df[[y_col]])
            grps <- sort(unique(df[[x_col]]))
            validate(need(length(grps) >= 2, "Slope chart needs ≥ 2 X values"))
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           group = .data[[col_col]],
                           colour = .data[[col_col]])) +
              geom_line(linewidth = 1.2, alpha = alp) +
              geom_point(size = 4, alpha = alp) +
              pal_scale_colour +
              labs(x = x_col, y = y_col, colour = col_col)
          },

          rank_bump = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           colour = .data[[col_col]],
                           group  = .data[[col_col]])) +
              geom_line(linewidth = 1.2, alpha = alp) +
              geom_point(size = 3, alpha = alp) +
              scale_y_reverse() +
              pal_scale_colour +
              labs(x = x_col, y = paste0("Rank (", y_col, ")"),
                   colour = col_col)
          },

          # ══════════════════════════════════════════════════
          # I. MULTIVARIATE
          # ══════════════════════════════════════════════════

          radar_std = {
            num_cols <- names(df)[sapply(df, function(c)
              !all(is.na(suppressWarnings(as.numeric(c)))))]
            validate(need(length(num_cols) >= 3,
                          "Radar chart needs ≥ 3 numeric columns"))
            num_df <- df[, num_cols, drop = FALSE]
            num_df[] <- lapply(num_df, .as_num)
            # Normalize each column to 0-1
            num_df <- as.data.frame(lapply(num_df, function(v) {
              r <- range(v, na.rm = TRUE)
              if (diff(r) == 0) return(rep(0.5, length(v)))
              (v - r[1]) / diff(r)
            }))
            grp_col <- col_col %||% x_col
            grp_vals <- if (grp_col %in% names(df))
              as.character(df[[grp_col]])
            else rep("Group", nrow(df))
            num_df[[".grp"]] <- grp_vals
            long_df <- tidyr::pivot_longer(num_df,
                         cols = -.grp,
                         names_to = "variable",
                         values_to = "value")
            ggplot(long_df,
                   aes(x = variable, y = value,
                       group = .grp, colour = .grp)) +
              geom_polygon(fill = NA, linewidth = 1, alpha = alp) +
              geom_point(size = 2, alpha = alp) +
              coord_polar() +
              facet_wrap(~.grp) +
              pal_scale_colour +
              labs(x = NULL, y = "Normalised value",
                   colour = grp_col) +
              theme(axis.text.x = element_text(size = 8),
                    legend.position = "none")
          },

          parallel_coords = {
            num_cols <- names(df)[sapply(df, function(c)
              !all(is.na(suppressWarnings(as.numeric(c)))))]
            validate(need(length(num_cols) >= 2,
                          "Parallel coords needs ≥ 2 numeric columns"))
            num_df <- df[, num_cols, drop = FALSE]
            num_df[] <- lapply(num_df, .as_num)
            # Normalise
            num_df <- as.data.frame(lapply(num_df, function(v) {
              r <- range(v, na.rm = TRUE)
              if (diff(r) == 0) return(rep(0.5, length(v)))
              (v - r[1]) / diff(r)
            }))
            grp_col <- col_col %||% x_col
            grp_vals <- if (grp_col %in% names(df))
              as.character(df[[grp_col]])
            else as.character(seq_len(nrow(df)))
            num_df[[".id"]]  <- seq_len(nrow(num_df))
            num_df[[".grp"]] <- grp_vals
            long_df <- tidyr::pivot_longer(num_df,
                         cols = c(-.id, -.grp),
                         names_to = "variable",
                         values_to = "value")
            ggplot(long_df,
                   aes(x = variable, y = value,
                       group = .id, colour = .grp)) +
              geom_line(alpha = min(alp, 0.5), linewidth = 0.4) +
              pal_scale_colour +
              labs(x = "Variable", y = "Normalised value",
                   colour = grp_col) +
              theme(axis.text.x = element_text(angle = 30, hjust = 1))
          },

          scatter_matrix = {
            num_cols <- names(df)[sapply(df, function(c)
              !all(is.na(suppressWarnings(as.numeric(c)))))]
            validate(need(length(num_cols) >= 2,
                          "Scatter matrix needs ≥ 2 numeric columns"))
            num_df <- df[, head(num_cols, 5), drop = FALSE]
            num_df[] <- lapply(num_df, .as_num)
            grp_col <- col_col %||% NULL
            if (!is.null(grp_col) && grp_col %in% names(df))
              num_df[[".grp"]] <- as.factor(df[[grp_col]])
            # Manual SPLOM via faceting
            pairs_list <- do.call(rbind, lapply(names(num_df)[names(num_df) != ".grp"], function(vx) {
              do.call(rbind, lapply(names(num_df)[names(num_df) != ".grp"], function(vy) {
                d <- data.frame(x = num_df[[vx]], y = num_df[[vy]],
                                xvar = vx, yvar = vy)
                if (!is.null(grp_col) && ".grp" %in% names(num_df))
                  d$.grp <- num_df[[".grp"]]
                d
              }))
            }))
            aes_sp <- if (!is.null(grp_col) && ".grp" %in% names(pairs_list))
              aes(x = x, y = y, colour = .grp)
            else aes(x = x, y = y)
            ggplot(pairs_list, aes_sp) +
              geom_point(alpha = min(alp, 0.4), size = 0.8) +
              (if (!is.null(grp_col)) pal_scale_colour else
                scale_color_manual(values = "#2c3e50")) +
              facet_grid(yvar ~ xvar, scales = "free") +
              labs(colour = grp_col %||% NULL) +
              theme(strip.text = element_text(size = 8),
                    axis.text  = element_text(size = 6))
          },

          corr_matrix = {
            num_cols <- names(df)[sapply(df, function(c)
              !all(is.na(suppressWarnings(as.numeric(c)))))]
            validate(need(length(num_cols) >= 2,
                          "Correlation matrix needs ≥ 2 numeric columns"))
            num_df <- df[, num_cols, drop = FALSE]
            num_df[] <- lapply(num_df, .as_num)
            cor_mat <- cor(num_df, use = "pairwise.complete.obs")
            cor_df  <- as.data.frame(as.table(cor_mat))
            names(cor_df) <- c("Var1","Var2","r")
            ggplot(cor_df, aes(x = Var1, y = Var2, fill = r,
                               label = sprintf("%.2f", r))) +
              geom_tile(colour = "white") +
              geom_text(size = 3) +
              scale_fill_gradient2(low = "#c0392b", mid = "white",
                                   high = "#2980b9", midpoint = 0,
                                   limits = c(-1,1), name = "r") +
              labs(x = NULL, y = NULL) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          },

          facet_grid_std = {
            req(y_col %in% names(df), !is.null(col_col),
                !is.null(siz_col))
            df[[x_col]] <- .as_num(df[[x_col]])
            df[[y_col]] <- .as_num(df[[y_col]])
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
              geom_point(alpha = alp, size = 2, colour = "#2c3e50") +
              facet_grid(
                rows = vars(.data[[siz_col]]),
                cols = vars(.data[[col_col]]),
                scales = "free"
              ) +
              labs(x = x_col, y = y_col)
          },

          # ══════════════════════════════════════════════════
          # J. STATISTICAL
          # ══════════════════════════════════════════════════

          error_bar = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            grp <- col_col %||% x_col
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           colour = .data[[grp]])) +
              stat_summary(fun = mean, geom = "point",
                           size = 3, position = position_dodge(0.3)) +
              stat_summary(fun.data = mean_sd, geom = "errorbar",
                           width = 0.2,
                           position = position_dodge(0.3)) +
              pal_scale_colour +
              labs(x = x_col, y = paste0("Mean \u00b1 SD of ", y_col),
                   colour = grp) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          },

          error_ribbon = {
            req(y_col %in% names(df), !is.null(col_col),
                !is.null(siz_col))
            df[[x_col]]   <- .as_num(df[[x_col]])
            df[[y_col]]   <- .as_num(df[[y_col]])
            df[[col_col]] <- .as_num(df[[col_col]])
            df[[siz_col]] <- .as_num(df[[siz_col]])
            ggplot(df, aes(x = .data[[x_col]],
                           y    = .data[[y_col]],
                           ymin = .data[[col_col]],
                           ymax = .data[[siz_col]])) +
              geom_ribbon(fill = "#3498db", alpha = alp * 0.4) +
              geom_line(colour = "#2c3e50", linewidth = 1) +
              labs(x = x_col, y = y_col,
                   subtitle = paste0("Band: ", col_col,
                                     " to ", siz_col))
          },

          forest_plot = {
            req(y_col %in% names(df), !is.null(col_col),
                !is.null(siz_col))
            df[[y_col]]   <- .as_num(df[[y_col]])
            df[[col_col]] <- .as_num(df[[col_col]])
            df[[siz_col]] <- .as_num(df[[siz_col]])
            df[[x_col]]   <- factor(df[[x_col]],
                                    levels = rev(unique(df[[x_col]])))
            ggplot(df, aes(x = .data[[y_col]],
                           y = .data[[x_col]],
                           xmin = .data[[col_col]],
                           xmax = .data[[siz_col]])) +
              geom_point(size = 3, colour = "#2c3e50", alpha = alp) +
              geom_errorbarh(height = 0.3, colour = "#2c3e50",
                             linewidth = 0.8) +
              geom_vline(xintercept = 0, linetype = "dashed",
                         colour = "#e74c3c") +
              labs(x = y_col, y = x_col,
                   subtitle = paste0("CI: ", col_col, " to ", siz_col))
          },

          stat_summary_plot = {
            req(y_col %in% names(df))
            df[[y_col]] <- .as_num(df[[y_col]])
            grp <- col_col %||% x_col
            ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]],
                           fill = .data[[grp]])) +
              stat_summary(fun = median,
                           fun.min = function(v) quantile(v, 0.25,
                                                           na.rm=TRUE),
                           fun.max = function(v) quantile(v, 0.75,
                                                           na.rm=TRUE),
                           geom = "crossbar", alpha = alp, width = 0.5) +
              stat_summary(fun = mean, geom = "point",
                           shape = 23, fill = "white",
                           size = 3) +
              pal_scale_fill +
              labs(x = x_col,
                   y = paste0("IQR of ", y_col,
                               " (diamond = mean)"),
                   fill = grp) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          },

          interaction_plot = {
            req(y_col %in% names(df), !is.null(col_col))
            df[[y_col]] <- .as_num(df[[y_col]])
            smry <- df %>%
              dplyr::group_by(.data[[x_col]], .data[[col_col]]) %>%
              dplyr::summarise(mean_y = mean(.data[[y_col]],
                                             na.rm = TRUE),
                               .groups = "drop")
            ggplot(smry, aes(x = .data[[x_col]],
                             y = mean_y,
                             colour = .data[[col_col]],
                             group  = .data[[col_col]])) +
              geom_line(linewidth = 1.2, alpha = alp) +
              geom_point(size = 4, alpha = alp) +
              pal_scale_colour +
              labs(x = x_col, y = paste0("Mean ", y_col),
                   colour = col_col,
                   subtitle = "Interaction plot") +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          },

          {  # default
            ggplot() +
              annotate("text", x = 0.5, y = 0.5,
                       label = paste0("Plot type '", ptype,
                                      "' is not yet implemented."),
                       size = 6, colour = "#e74c3c") +
              xlim(0,1) + ylim(0,1) +
              theme_void()
          }
        ) # end switch
      },  # end tryCatch expr
      error = function(e) {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = paste0("Error: ", conditionMessage(e),
                                  "\n\nCheck column selections and data."),
                   size = 5, colour = "#c0392b",
                   hjust = 0.5, vjust = 0.5) +
          xlim(0,1) + ylim(0,1) +
          theme_void()
      })

      # Apply flip if requested
      if (do_flip && inherits(p, "gg") &&
          !ptype %in% c("bar_h","bar_pyramid","pyramid_std",
                        "bar_cleveland","bar_waterfall","bar_diverging",
                        "rank_bar","rank_lollipop","rank_dumbbell",
                        "bar_dumbbell","bar_lollipop","funnel_std",
                        "forest_plot","pie_std","donut_std",
                        "polar_rose","treemap_std","waffle_std"))
        p <- p + coord_flip()

      apply_labels(p, "general", input)
    })
  })

  output$plot_general <- renderPlot({
    make_general_plot()
  })
}
