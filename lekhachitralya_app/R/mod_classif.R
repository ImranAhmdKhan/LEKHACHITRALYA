# ============================================================
#  R/mod_classif.R
#  Module    : Classification Analysis
#  Description : Focuses on the PDB protein-class dimension of
#                docking data.  Fourteen plot types explore class
#                frequency, score profiles, fraction composition
#                and active-hit rates across classifications.
#
#  Data type   : Docking data
#  Required columns : classification (PDB CLASSIFICATION)
#  Optional columns : score, fraction, molecule
#
#  Plot types  :
#    count        – Horizontal / vertical bar of class frequency
#    meanscore    – Mean docking score per class (bar)
#    bestdot      – Best score dot plot per class
#    stackbar     – Stacked bar: class × fraction counts
#    classheat    – Class × molecule mean-score heatmap
#    violin       – Violin + box per class
#    radialbar    – Radial abundance plot of top classes
#    medianbar    – Median score bar per class
#    fracprop     – Within-class fractional composition
#    activepct    – Active-hit percentage per class
#    classstrip   – Strip plot: scores per class coloured by fraction
#    dotline      – Best vs mean dot-line per class
#    moleculecount– Unique molecule count per class
#    fractionheat – Fraction × class count heatmap
# ============================================================


# ── UI ───────────────────────────────────────────────────────

#' UI for the Classification Analysis tab.
#' @return A `shinydashboard::tabItem` object.
classifUI <- function() {
  tabItem("classif",
    fluidRow(
      box(width = 3, title = "Parameters", status = "warning", solidHeader = TRUE,
        dataset_picker("ds_classif"),
        hr(),
        div(class = "override-group",
          tags$h6("Column roles"),
          uiOutput("classif_col_classification"),
          uiOutput("classif_col_score"),
          uiOutput("classif_col_fraction"),
          uiOutput("classif_col_molecule")
        ),
        hr(),
        div(class = "filter-group",
          tags$h6("Filters"),
          uiOutput("classif_filter_fraction"),
          sliderInput("classif_thresh", "Score cutoff (0 = off)",
                      min = -15, max = 0, value = 0, step = 0.5),
          numericInput("classif_top_n",  "Top N classes", 15, 5, 60)
        ),
        hr(),
        selectInput("classif_type", "Plot type",
          c("Class count bar"                = "count",
            "Mean score bar"                  = "meanscore",
            "Best score dot"                  = "bestdot",
            "Stacked count (fraction)"        = "stackbar",
            "Class \u00d7 molecule heatmap"   = "classheat",
            "Class violin"                    = "violin",
            "Radial abundance"                = "radialbar",
            "Median score bar"                = "medianbar",
            "Fraction composition bar"        = "fracprop",
            "Active % bar"                    = "activepct",
            "Class strip plot"                = "classstrip",
            "Dot-line (best vs mean)"         = "dotline",
            "Molecule count per class"        = "moleculecount",
            "Fraction \u00d7 class heatmap"   = "fractionheat"),
          selected = "count"),
        radioButtons("classif_orient", "Orientation",
                     c("Horizontal" = "horiz", "Vertical" = "vert"), "horiz"),
        selectInput("classif_palette",  "Colour palette", PALETTES, "Set2"),
        sliderInput("classif_alpha",    "Alpha", 0.2, 1, 0.8, step = 0.05),
        actionButton("draw_classif", "Draw Classification Plot",
                     class = "btn-warning btn-lg", width = "100%")
      ),
      box(width = 9, title = "Classification Analysis",
          status = "warning", solidHeader = TRUE,
        label_theme_box("classif"),
        withSpinner(plotOutput("plot_classif", height = "560px"), type = 6)
      )
    )
  )
}


# ── Server ───────────────────────────────────────────────────

#' Server logic for the Classification Analysis tab.
#'
#' @param input   Shiny `input` object.
#' @param output  Shiny `output` object.
#' @param session Shiny `session` object.
#' @param rv      Shared `reactiveValues`.
classifServer <- function(input, output, session, rv) {

  gc <- function(id, df, pats = NULL) get_col(id, df, pats, input)

  make_ds_picker("ds_classif", "Dataset - classif", output, input, rv)
  make_col_override("classif_col_classification", "Classification", "sel_ds_classif",
                    c("pdb classification","classification"), output, input, rv)
  make_col_override("classif_col_score",          "Score",          "sel_ds_classif",
                    c("docking score","score"), output, input, rv)
  make_col_override("classif_col_fraction",       "Fraction",       "sel_ds_classif",
                    c("fractions","fraction"), output, input, rv)
  make_col_override("classif_col_molecule",       "Molecule",       "sel_ds_classif",
                    c("title","molecule"), output, input, rv)
  make_filter_ui("classif_filter_fraction", "frac", "sel_ds_classif",
                 "Filter Fractions:", output, input, rv)

  make_classif_plot <- reactive({
    req(input$draw_classif)
    isolate({
      nm <- input$sel_ds_classif; req(nm, rv$datasets[[nm]])
      df <- rv$datasets[[nm]]
      class_col <- gc("classif_col_classification", df, c("pdb classification","classification"))
      score_col <- gc("classif_col_score",          df, c("docking score","score"))
      frac_col  <- gc("classif_col_fraction",       df, c("fractions","fraction"))
      mol_col   <- gc("classif_col_molecule",       df, c("title","molecule"))
      df[[score_col]] <- suppressWarnings(as.numeric(df[[score_col]]))
      if (!is.null(input$classif_filter_fraction) &&
          length(input$classif_filter_fraction) > 0 && frac_col %in% names(df))
        df <- df[as.character(df[[frac_col]]) %in% input$classif_filter_fraction, ]
      if (input$classif_thresh < 0)
        df <- df[!is.na(df[[score_col]]) & df[[score_col]] <= input$classif_thresh, ]
      req(nrow(df) > 0, cancelOutput = TRUE)
      pal     <- input$classif_palette %||% "Set2"
      top_n   <- input$classif_top_n
      horiz   <- input$classif_orient == "horiz"
      alpha_v <- input$classif_alpha %||% 0.8

      p <- switch(input$classif_type,
        count = {
          cd <- df %>% dplyr::count(.data[[class_col]], name = "n") %>%
            dplyr::arrange(dplyr::desc(n)) %>% dplyr::slice_head(n = top_n) %>%
            dplyr::mutate(cls = forcats::fct_reorder(.data[[class_col]], n))
          if (horiz)
            ggplot(cd, aes(x = n, y = cls, fill = cls)) +
              geom_col(show.legend = FALSE, alpha = alpha_v) +
              scale_fill_brewer(palette = pal) +
              labs(x = "Count", y = "PDB Classification", title = "Class count")
          else
            ggplot(cd, aes(x = cls, y = n, fill = cls)) +
              geom_col(show.legend = FALSE, alpha = alpha_v) +
              scale_fill_brewer(palette = pal) +
              labs(y = "Count", x = "") +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
        },
        meanscore = {
          ms <- df %>%
            dplyr::group_by(across(all_of(class_col))) %>%
            dplyr::summarise(mean_sc = mean(.data[[score_col]], na.rm = TRUE),
                             n = dplyr::n(), .groups = "drop") %>%
            dplyr::arrange(mean_sc) %>% dplyr::slice_head(n = top_n) %>%
            dplyr::mutate(cls = forcats::fct_reorder(.data[[class_col]], mean_sc))
          if (horiz)
            ggplot(ms, aes(x = mean_sc, y = cls, fill = mean_sc)) +
              geom_col(show.legend = FALSE, alpha = alpha_v) +
              scale_fill_distiller(palette = pal, direction = -1) +
              labs(x = "Mean score (kcal/mol)", y = "PDB Classification",
                   title = "Mean score by class")
          else
            ggplot(ms, aes(x = cls, y = mean_sc, fill = mean_sc)) +
              geom_col(show.legend = FALSE, alpha = alpha_v) +
              scale_fill_distiller(palette = pal, direction = -1) +
              labs(y = "Mean score", x = "") +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
        },
        bestdot = {
          bd <- df %>%
            dplyr::group_by(across(all_of(class_col))) %>%
            dplyr::summarise(best = min(.data[[score_col]], na.rm = TRUE),
                             n = dplyr::n(), .groups = "drop") %>%
            dplyr::arrange(best) %>% dplyr::slice_head(n = top_n) %>%
            dplyr::mutate(cls = forcats::fct_reorder(.data[[class_col]], best))
          ggplot(bd, aes(x = best, y = cls, colour = best, size = n)) +
            geom_point() +
            geom_vline(xintercept = -7, linetype = "dashed", colour = "red", alpha = 0.5) +
            scale_colour_distiller(palette = pal, direction = -1, name = "Best score") +
            scale_size_continuous(name = "Count", range = c(2, 10)) +
            labs(x = "Best score (kcal/mol)", y = "PDB Classification",
                 title = "Best score by class", caption = "Red = -7 kcal/mol threshold")
        },
        stackbar = {
          req(frac_col %in% names(df))
          sb <- df %>%
            dplyr::count(.data[[class_col]], .data[[frac_col]], name = "n") %>%
            dplyr::group_by(across(all_of(class_col))) %>%
            dplyr::mutate(total = sum(n)) %>% dplyr::ungroup() %>%
            dplyr::arrange(dplyr::desc(total)) %>%
            dplyr::filter(.data[[class_col]] %in%
                            unique(.data[[class_col]])[seq_len(top_n)]) %>%
            dplyr::mutate(cls = forcats::fct_reorder(.data[[class_col]], total))
          ggplot(sb, aes(x = n, y = cls, fill = .data[[frac_col]])) +
            geom_col(position = "stack", alpha = alpha_v) +
            scale_fill_brewer(palette = pal, name = "Fraction") +
            labs(x = "Count", y = "PDB Classification", title = "Class count by fraction")
        },
        classheat = {
          tc <- df %>% dplyr::count(.data[[class_col]]) %>%
            dplyr::arrange(dplyr::desc(n)) %>%
            dplyr::slice_head(n = top_n) %>% dplyr::pull(1)
          tm <- df %>%
            dplyr::group_by(across(all_of(mol_col))) %>%
            dplyr::summarise(m = min(.data[[score_col]], na.rm = TRUE), .groups = "drop") %>%
            dplyr::arrange(m) %>% dplyr::slice_head(n = 30) %>% dplyr::pull(1)
          ch <- df %>%
            dplyr::filter(.data[[class_col]] %in% tc, .data[[mol_col]] %in% tm) %>%
            dplyr::group_by(across(all_of(c(mol_col, class_col)))) %>%
            dplyr::summarise(ms = mean(.data[[score_col]], na.rm = TRUE), .groups = "drop")
          ggplot(ch, aes(x = .data[[mol_col]], y = .data[[class_col]], fill = ms)) +
            geom_tile(colour = "white") +
            scale_fill_distiller(palette = pal, direction = -1, name = "Mean score") +
            labs(x = "Molecule", y = "Classification",
                 title = "Mean score: top molecules \u00d7 top classes") +
            theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7))
        },
        violin = {
          vl <- df %>%
            dplyr::count(.data[[class_col]], name = "n") %>%
            dplyr::arrange(dplyr::desc(n)) %>%
            dplyr::slice_head(n = top_n) %>% dplyr::pull(1)
          vdf <- df %>%
            dplyr::filter(.data[[class_col]] %in% vl) %>%
            dplyr::mutate(cls = forcats::fct_reorder(
              .data[[class_col]], .data[[score_col]], .fun = median))
          ggplot(vdf, aes(x = .data[[score_col]], y = cls, fill = cls)) +
            geom_violin(alpha = alpha_v, trim = FALSE, show.legend = FALSE) +
            geom_boxplot(width = 0.12, fill = "white", outlier.alpha = 0.12) +
            scale_fill_brewer(palette = pal) +
            labs(x = "Docking score (kcal/mol)", y = "PDB Classification",
                 title = "Class-level score distribution") +
            theme_classic()
        },
        radialbar = {
          rb <- df %>% dplyr::count(.data[[class_col]], name = "n") %>%
            dplyr::arrange(dplyr::desc(n)) %>% dplyr::slice_head(n = top_n) %>%
            dplyr::mutate(cls = wrap_text_vec(.data[[class_col]], 20))
          ggplot(rb, aes(x = factor(cls, levels = cls), y = n, fill = n)) +
            geom_col(alpha = alpha_v) +
            coord_polar() +
            scale_fill_distiller(palette = pal, direction = 1, guide = "none") +
            labs(x = NULL, y = "Count", title = "Radial class abundance plot") +
            theme_minimal()
        },
        medianbar = {
          md <- df %>%
            dplyr::group_by(across(all_of(class_col))) %>%
            dplyr::summarise(median_sc = median(.data[[score_col]], na.rm = TRUE),
                             n = dplyr::n(), .groups = "drop") %>%
            dplyr::arrange(median_sc) %>% dplyr::slice_head(n = top_n) %>%
            dplyr::mutate(cls = forcats::fct_reorder(.data[[class_col]], median_sc))
          ggplot(md, aes(x = median_sc, y = cls, fill = n)) +
            geom_col(alpha = alpha_v) +
            scale_fill_distiller(palette = pal, direction = 1, name = "Count") +
            labs(x = "Median score (kcal/mol)", y = "PDB Classification",
                 title = "Median score by class") +
            theme_classic()
        },
        fracprop = {
          req(frac_col %in% names(df))
          fp_base <- df %>% dplyr::count(.data[[class_col]], .data[[frac_col]], name = "n")
          top_cls <- fp_base %>%
            dplyr::group_by(across(all_of(class_col))) %>%
            dplyr::summarise(total = sum(n), .groups = "drop") %>%
            dplyr::arrange(dplyr::desc(total)) %>% dplyr::slice_head(n = top_n)
          fp <- fp_base %>%
            dplyr::inner_join(top_cls, by = class_col) %>%
            dplyr::group_by(across(all_of(class_col))) %>%
            dplyr::mutate(prop = 100 * n / total) %>% dplyr::ungroup() %>%
            dplyr::mutate(cls = forcats::fct_reorder(.data[[class_col]], total))
          ggplot(fp, aes(x = prop, y = cls, fill = .data[[frac_col]])) +
            geom_col(alpha = alpha_v) +
            scale_fill_brewer(palette = pal, name = "Fraction") +
            labs(x = "Within-class percentage", y = "PDB Classification",
                 title = "Fractional composition within each class") +
            theme_classic()
        },
        activepct = {
          ap <- df %>%
            dplyr::group_by(across(all_of(class_col))) %>%
            dplyr::summarise(
              active_pct = 100 * mean(.data[[score_col]] <= input$classif_thresh,
                                      na.rm = TRUE),
              n = dplyr::n(), .groups = "drop") %>%
            dplyr::arrange(dplyr::desc(active_pct)) %>% dplyr::slice_head(n = top_n) %>%
            dplyr::mutate(cls = forcats::fct_reorder(.data[[class_col]], active_pct))
          ggplot(ap, aes(x = active_pct, y = cls, fill = n)) +
            geom_col(alpha = alpha_v) +
            scale_fill_distiller(palette = pal, direction = 1, name = "Count") +
            labs(x = "Active percentage", y = "PDB Classification",
                 title = "Active docking percentage by class") +
            theme_classic()
        },
        classstrip = {
          top_cls <- df %>% dplyr::count(.data[[class_col]], name = "n") %>%
            dplyr::arrange(dplyr::desc(n)) %>% dplyr::slice_head(n = top_n) %>% dplyr::pull(1)
          sd <- df %>%
            dplyr::filter(.data[[class_col]] %in% top_cls) %>%
            dplyr::mutate(cls = forcats::fct_reorder(
              .data[[class_col]], .data[[score_col]], .fun = median))
          ggplot(sd, aes(x = .data[[score_col]], y = cls,
                         colour = .data[[frac_col]])) +
            geom_jitter(height = 0.2, alpha = alpha_v, size = 1.1) +
            labs(x = "Docking score (kcal/mol)", y = "PDB Classification",
                 colour = "Fraction", title = "Class strip plot with per-entry scores") +
            theme_classic()
        },
        dotline = {
          dl <- df %>%
            dplyr::group_by(across(all_of(class_col))) %>%
            dplyr::summarise(best    = min(.data[[score_col]], na.rm = TRUE),
                             mean_sc = mean(.data[[score_col]], na.rm = TRUE),
                             .groups = "drop") %>%
            dplyr::arrange(best) %>% dplyr::slice_head(n = top_n) %>%
            dplyr::mutate(cls = forcats::fct_reorder(.data[[class_col]], best))
          ggplot(dl, aes(y = cls)) +
            geom_segment(aes(x = mean_sc, xend = best, yend = cls),
                         colour = "grey70", linewidth = 1) +
            geom_point(aes(x = mean_sc), shape = 21, fill = "white",
                       colour = "black", size = 2.5) +
            geom_point(aes(x = best, colour = best), size = 3, alpha = alpha_v) +
            scale_colour_distiller(palette = pal, direction = -1, name = "Best score") +
            labs(x = "Docking score (kcal/mol)", y = "PDB Classification",
                 title = "Dot-line class profile") +
            theme_classic()
        },
        moleculecount = {
          mc <- df %>%
            dplyr::group_by(across(all_of(class_col))) %>%
            dplyr::summarise(n_mol = dplyr::n_distinct(.data[[mol_col]]),
                             n = dplyr::n(), .groups = "drop") %>%
            dplyr::arrange(dplyr::desc(n_mol)) %>% dplyr::slice_head(n = top_n) %>%
            dplyr::mutate(cls = forcats::fct_reorder(.data[[class_col]], n_mol))
          ggplot(mc, aes(x = n_mol, y = cls, fill = n)) +
            geom_col(alpha = alpha_v) +
            scale_fill_distiller(palette = pal, direction = 1, name = "Entries") +
            labs(x = "Unique molecules", y = "PDB Classification",
                 title = "Molecule diversity by class") +
            theme_classic()
        },
        fractionheat = {
          req(frac_col %in% names(df))
          fh <- df %>%
            dplyr::count(.data[[class_col]], .data[[frac_col]], name = "n") %>%
            dplyr::group_by(across(all_of(class_col))) %>%
            dplyr::mutate(total = sum(n)) %>% dplyr::ungroup() %>%
            dplyr::arrange(dplyr::desc(total)) %>%
            dplyr::filter(.data[[class_col]] %in% unique(.data[[class_col]])[seq_len(top_n)])
          ggplot(fh, aes(x = .data[[frac_col]], y = .data[[class_col]], fill = n)) +
            geom_tile(colour = "white", alpha = alpha_v) +
            scale_fill_distiller(palette = pal, direction = 1, name = "Count") +
            labs(x = "Fraction", y = "PDB Classification",
                 title = "Fraction-by-class heatmap") +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 30, hjust = 1))
        }
      )
      apply_labels(p, "classif", input)
    })
  })

  output$plot_classif <- renderPlot({
    p <- make_classif_plot()
    rv$plots[["classif"]] <- p
    p
  })
}
