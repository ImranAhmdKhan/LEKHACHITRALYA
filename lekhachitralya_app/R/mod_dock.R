# ============================================================
#  R/mod_dock.R
#  Module    : Docking Heatmap
#  Description : Visualises the full docking-score matrix between
#                molecules and protein targets.  Integrates with
#                the TCMNP::dock_plot() function and offers eight
#                additional ggplot2-based alternatives when TCMNP
#                cannot render.
#
#  Data type   : Docking data
#  Required columns : molecule (rows), target (columns), score
#
#  Plot types  :
#    heatmap    – TCMNP square/circle heatmap (default)
#    bubble     – Bubble matrix (size ∝ |score|, colour = score)
#    strip      – Raster strip heatmap (fast for large matrices)
#    contour    – Filled contour map of score landscape
#    targetbar  – Mean score bar chart for top targets
#    targetbox  – Score box-plot per target
#    moleculebox– Score box-plot per molecule
#    radial     – Radial mean-score polar chart
#    rankline   – Rank-ordered score line plot
# ============================================================


# ── UI ───────────────────────────────────────────────────────

#' UI for the Docking Heatmap tab.
#' @return A `shinydashboard::tabItem` object.
dockUI <- function() {
  tabItem("dock",
    fluidRow(
      box(width = 3, title = "Parameters", status = "primary", solidHeader = TRUE,
        dataset_picker("ds_dock"),
        hr(),
        div(class = "override-group",
          tags$h6("Column roles"),
          uiOutput("dock_col_molecule"),
          uiOutput("dock_col_target"),
          uiOutput("dock_col_score")
        ),
        hr(),
        div(class = "filter-group",
          tags$h6("Filters"),
          uiOutput("dock_filter_fraction"),
          sliderInput("dock_score_thresh", "Min |score| threshold",
                      0, 15, 0, step = 0.5),
          numericInput("dock_top_n",  "Top N molecules", 20, 5, 200),
          numericInput("dock_bins",   "Bins / contours", 12, 5, 50)
        ),
        hr(),
        radioButtons("dock_type", "Plot family",
          c("TCMNP heatmap"    = "heatmap",
            "Bubble matrix"    = "bubble",
            "Strip heatmap"    = "strip",
            "Contour score map"= "contour",
            "Target summary bar" = "targetbar",
            "Target boxplot"   = "targetbox",
            "Molecule boxplot" = "moleculebox",
            "Radial mean score"= "radial",
            "Rank line"        = "rankline"),
          selected = "heatmap"),
        hr(),
        radioButtons("dock_pivot_by", "Heatmap rows =",
                     c("Molecules" = "mol", "Targets (PDB ID)" = "tgt"),
                     selected = "mol"),
        radioButtons("dock_shape", "Tile shape",
                     c("Square" = "square", "Circle" = "circle"),
                     selected = "square"),
        selectInput("dock_palette",   "Colour palette", PALETTES, "RdYlBu"),
        numericInput("dock_legend_h", "Legend height",  3, 1, 10),
        sliderInput("dock_alpha",     "Tile alpha",     0.2, 1, 0.9, step = 0.05),
        numericInput("dock_text_sz",  "Axis text size", 8, 4, 18),
        actionButton("draw_dock", "Draw Heatmap",
                     class = "btn-success btn-lg", width = "100%")
      ),
      box(width = 9, title = "Molecular Docking Score Heatmap",
          status = "success", solidHeader = TRUE,
        label_theme_box("dock"),
        withSpinner(plotOutput("plot_dock", height = "600px"), type = 6),
        p(class = "plot-caption",
          "Colour = docking score (kcal/mol). More negative = stronger binding.")
      )
    )
  )
}


# ── Server ───────────────────────────────────────────────────

#' Server logic for the Docking Heatmap tab.
#'
#' @param input   Shiny `input` object.
#' @param output  Shiny `output` object.
#' @param session Shiny `session` object.
#' @param rv      Shared `reactiveValues`.
dockServer <- function(input, output, session, rv) {

  gc <- function(id, df, pats = NULL) get_col(id, df, pats, input)

  make_ds_picker("ds_dock", "Dataset - dock", output, input, rv)
  make_col_override("dock_col_molecule", "Molecule column (rows)", "sel_ds_dock",
                    c("title","molecule","compound"), output, input, rv)
  make_col_override("dock_col_target",   "Target column (cols)",  "sel_ds_dock",
                    c("pdb id","pdb_id","target"), output, input, rv)
  make_col_override("dock_col_score",    "Score column",          "sel_ds_dock",
                    c("docking score","score","affinity"), output, input, rv)
  make_filter_ui("dock_filter_fraction", "frac", "sel_ds_dock",
                 "Filter Fractions:", output, input, rv)

  make_dock_plot <- reactive({
    req(input$draw_dock)
    isolate({
      nm <- input$sel_ds_dock; req(nm, rv$datasets[[nm]])
      df <- rv$datasets[[nm]]
      mol_col   <- gc("dock_col_molecule", df, c("title","molecule","compound"))
      tgt_col   <- gc("dock_col_target",   df, c("pdb id","pdb_id","target"))
      score_col <- gc("dock_col_score",    df, c("docking score","score","affinity"))
      df[[score_col]] <- suppressWarnings(as.numeric(df[[score_col]]))

      frac_real <- {
        nms <- tolower(names(df))
        names(df)[grepl("frac", nms)][1]
      }
      if (!is.null(frac_real) && !is.na(frac_real) &&
          !is.null(input$dock_filter_fraction))
        df <- df[as.character(df[[frac_real]]) %in% input$dock_filter_fraction, ]

      if (input$dock_score_thresh > 0)
        df <- df[abs(df[[score_col]]) >= input$dock_score_thresh, ]
      req(nrow(df) > 0, cancelOutput = TRUE)

      top_mols <- df %>%
        dplyr::group_by(.data[[mol_col]]) %>%
        dplyr::summarise(m = mean(.data[[score_col]], na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(m) %>% dplyr::slice_head(n = input$dock_top_n) %>% dplyr::pull(1)
      df <- df[df[[mol_col]] %in% top_mols, ]

      row_col <- if (input$dock_pivot_by == "mol") mol_col else tgt_col
      col_col <- if (input$dock_pivot_by == "mol") tgt_col else mol_col

      mat <- tryCatch(
        pivot_to_matrix(df, row_col, col_col, score_col),
        error = function(e) {
          showNotification(paste("Pivot error:", e$message), type = "error"); NULL })
      req(!is.null(mat))

      mat_df <- as.data.frame(mat) %>%
        tibble::rownames_to_column("row_id") %>%
        tidyr::pivot_longer(-row_id, names_to = "col_id", values_to = "score")

      pal    <- input$dock_palette %||% "RdYlBu"
      txt_sz <- input$dock_text_sz %||% 8
      alp    <- input$dock_alpha   %||% 0.9
      bins   <- input$dock_bins    %||% 12

      tgt_sum <- df %>%
        dplyr::group_by(tgt = .data[[tgt_col]]) %>%
        dplyr::summarise(mean_sc = mean(.data[[score_col]], na.rm = TRUE),
                         med_sc  = median(.data[[score_col]], na.rm = TRUE),
                         n = dplyr::n(), .groups = "drop") %>%
        dplyr::arrange(mean_sc) %>%
        dplyr::slice_head(n = min(input$dock_top_n, 20))
      mol_sum <- df %>%
        dplyr::group_by(mol = .data[[mol_col]]) %>%
        dplyr::summarise(mean_sc = mean(.data[[score_col]], na.rm = TRUE),
                         best    = min(.data[[score_col]], na.rm = TRUE),
                         n = dplyr::n(), .groups = "drop") %>%
        dplyr::arrange(mean_sc) %>%
        dplyr::slice_head(n = min(input$dock_top_n, 20))

      row_levels <- unique(mat_df$row_id)
      col_levels <- unique(mat_df$col_id)
      mat_df$row_num <- match(mat_df$row_id, row_levels)
      mat_df$col_num <- match(mat_df$col_id, col_levels)

      final_p <- switch(input$dock_type,
        heatmap = {
          p <- if (input$dock_shape == "square") {
            ggplot(mat_df, aes(x = col_id, y = row_id, fill = score)) +
              geom_tile(colour = "white", linewidth = 0.25, alpha = alp) +
              scale_fill_distiller(palette = pal, name = "Score\n(kcal/mol)",
                                   direction = -1, na.value = "grey90")
          } else {
            ggplot(mat_df, aes(x = col_id, y = row_id,
                               colour = score, size = abs(score))) +
              geom_point(alpha = alp) +
              scale_colour_distiller(palette = pal, name = "Score\n(kcal/mol)",
                                     direction = -1) +
              scale_size_continuous(name = "|Score|", range = c(1, 8))
          }
          p <- p + theme_classic(base_size = txt_sz) +
            theme(axis.text.x = element_text(angle = 60, hjust = 1, size = txt_sz - 1),
                  axis.text.y = element_text(size = txt_sz - 1),
                  axis.title = element_blank(), legend.position = "right",
                  plot.margin = margin(10, 10, 10, 10))
          p2 <- tryCatch(
            TCMNP::dock_plot(mat, shape = input$dock_shape,
                             legend.height = input$dock_legend_h),
            error = function(e) NULL)
          if (!is.null(p2)) p2 else p
        },
        bubble = {
          ggplot(mat_df, aes(x = col_id, y = row_id,
                             size = abs(score), colour = score)) +
            geom_point(alpha = alp) +
            scale_colour_distiller(palette = pal, direction = -1, name = "Score") +
            scale_size_continuous(name = "|Score|", range = c(1, 10)) +
            labs(x = NULL, y = NULL, title = "Bubble matrix of docking scores") +
            theme_classic(base_size = txt_sz) +
            theme(axis.text.x = element_text(angle = 60, hjust = 1))
        },
        strip = {
          ggplot(mat_df, aes(x = col_id, y = row_id, fill = score)) +
            geom_raster() +
            scale_fill_distiller(palette = pal, direction = -1, name = "Score") +
            labs(x = NULL, y = NULL, title = "Docking strip heatmap") +
            theme_minimal(base_size = txt_sz) +
            theme(axis.text.x = element_text(angle = 60, hjust = 1))
        },
        contour = {
          ggplot(mat_df, aes(x = col_num, y = row_num, z = score)) +
            geom_contour_filled(bins = bins, alpha = alp) +
            scale_x_continuous(breaks = seq_along(col_levels), labels = col_levels) +
            scale_y_continuous(breaks = seq_along(row_levels), labels = row_levels) +
            labs(x = if (input$dock_pivot_by == "mol") "Target" else "Molecule",
                 y = if (input$dock_pivot_by == "mol") "Molecule" else "Target",
                 title = "Contour map of docking intensities") +
            theme_classic(base_size = txt_sz) +
            theme(axis.text.x = element_text(angle = 60, hjust = 1, size = txt_sz - 1),
                  axis.text.y = element_text(size = txt_sz - 1))
        },
        targetbar = {
          ggplot(tgt_sum, aes(x = mean_sc,
                              y = forcats::fct_reorder(tgt, mean_sc),
                              fill = n)) +
            geom_col(alpha = alp) +
            scale_fill_distiller(palette = pal, direction = 1, name = "Pairs") +
            labs(x = "Mean score (kcal/mol)", y = "Target",
                 title = "Target summary bar plot") +
            theme_classic(base_size = txt_sz)
        },
        targetbox = {
          bx <- df %>%
            dplyr::filter(.data[[tgt_col]] %in% tgt_sum$tgt) %>%
            dplyr::mutate(tgt = forcats::fct_reorder(
              .data[[tgt_col]], .data[[score_col]], .fun = median))
          ggplot(bx, aes(x = .data[[score_col]], y = tgt, fill = tgt)) +
            geom_boxplot(alpha = alp, outlier.alpha = 0.15, show.legend = FALSE) +
            scale_fill_brewer(palette = "Set3") +
            labs(x = "Docking score (kcal/mol)", y = "Target",
                 title = "Distribution of docking scores across top targets") +
            theme_classic(base_size = txt_sz)
        },
        moleculebox = {
          bx <- df %>%
            dplyr::filter(.data[[mol_col]] %in% mol_sum$mol) %>%
            dplyr::mutate(mol = forcats::fct_reorder(
              .data[[mol_col]], .data[[score_col]], .fun = median))
          ggplot(bx, aes(x = .data[[score_col]], y = mol, fill = mol)) +
            geom_boxplot(alpha = alp, outlier.alpha = 0.15, show.legend = FALSE) +
            scale_fill_brewer(palette = "Set3") +
            labs(x = "Docking score (kcal/mol)", y = "Molecule",
                 title = "Distribution of docking scores across top molecules") +
            theme_classic(base_size = txt_sz)
        },
        radial = {
          rd <- mol_sum %>%
            dplyr::mutate(mol = wrap_text_vec(mol, 18), mean_abs = abs(mean_sc))
          ggplot(rd, aes(x = factor(mol, levels = mol),
                         y = mean_abs, fill = mean_sc)) +
            geom_col(alpha = alp) +
            coord_polar() +
            scale_fill_distiller(palette = pal, direction = -1, name = "Mean score") +
            labs(x = NULL, y = "|Mean score|", title = "Radial mean-score profile") +
            theme_minimal(base_size = txt_sz)
        },
        rankline = {
          rk <- df %>%
            dplyr::arrange(.data[[score_col]]) %>%
            dplyr::slice_head(n = min(nrow(df), max(50, input$dock_top_n * 10))) %>%
            dplyr::mutate(rank = dplyr::row_number())
          ggplot(rk, aes(x = rank, y = .data[[score_col]],
                         colour = .data[[tgt_col]])) +
            geom_line(alpha = alp) +
            geom_point(alpha = alp, size = 1.4) +
            labs(x = "Ranked interaction", y = "Docking score (kcal/mol)",
                 colour = "Target", title = "Rank-ordered docking score line plot") +
            theme_classic(base_size = txt_sz)
        }
      )
      apply_labels(final_p, "dock", input)
    })
  })

  output$plot_dock <- renderPlot({
    p <- make_dock_plot()
    rv$plots[["dock"]] <- p
    p
  })
}
