# ============================================================
#  R/mod_scoreex.R
#  Module    : Score Explorer
#  Description : An interactive drill-down explorer for docking
#                scores.  Offers 14 rapid plot types that examine
#                score distributions, molecule performance and
#                target profiles, with support for fraction
#                filtering and score cutoffs.
#
#  Data type   : Docking data
#  Required columns : molecule, target, score
#  Optional columns : fraction, classification
#
#  Plot types  :
#    topbar      – Top N molecules by best score (horizontal bar)
#    dist        – Score distribution histogram
#    fracbox     – Score box-plot per fraction
#    classdot    – Mean score dot plot per PDB class
#    scatter     – Score jitter scatter by molecule
#    density     – Score density curves by group
#    targetbox   – Score box-plot per top target
#    ecdf        – Empirical CDF by fraction / target
#    fracviolin  – Violin per fraction
#    targetdot   – Target dot summary (best vs count)
#    fracmean    – Mean + median score per fraction bar
#    classbox    – Class-level boxplot
#    molebubble  – Molecule bubble: mean vs best vs target count
#    rankline    – Rank-ordered score line
#    rug         – Density + rug overlay by group
# ============================================================


# ── UI ───────────────────────────────────────────────────────

#' UI for the Score Explorer tab.
#' @return A `shinydashboard::tabItem` object.
scoreexUI <- function() {
  tabItem("scoreex",
    fluidRow(
      box(width = 3, title = "Parameters", status = "info", solidHeader = TRUE,
        dataset_picker("ds_scoreex"),
        hr(),
        div(class = "override-group",
          tags$h6("Column roles"),
          uiOutput("scoreex_col_molecule"),
          uiOutput("scoreex_col_target"),
          uiOutput("scoreex_col_score"),
          uiOutput("scoreex_col_fraction"),
          uiOutput("scoreex_col_classification")
        ),
        hr(),
        div(class = "filter-group",
          tags$h6("Filters"),
          uiOutput("scoreex_filter_fraction"),
          sliderInput("scoreex_thresh", "Score cutoff (0 = off)",
                      min = -15, max = 0, value = 0, step = 0.5),
          numericInput("scoreex_top_n", "Top N",  20, 5, 100),
          sliderInput("scoreex_alpha",  "Alpha",  0.2, 1, 0.8, step = 0.05),
          numericInput("scoreex_bins",  "Histogram bins", 30, 5, 80)
        ),
        hr(),
        selectInput("scoreex_type", "Plot type",
          c("Top N bar (best score)"       = "topbar",
            "Score distribution histogram"  = "dist",
            "Boxplot by fraction"           = "fracbox",
            "Dot plot by class"             = "classdot",
            "Scatter by molecule"           = "scatter",
            "Density by group"              = "density",
            "Target boxplot"                = "targetbox",
            "ECDF by group"                 = "ecdf",
            "Violin by fraction"            = "fracviolin",
            "Target dot (best vs count)"    = "targetdot",
            "Fraction mean/median bar"      = "fracmean",
            "Class boxplot"                 = "classbox",
            "Molecule bubble"               = "molebubble",
            "Rank-ordered line"             = "rankline",
            "Density + rug"                 = "rug"),
          selected = "topbar"),
        selectInput("scoreex_palette",  "Colour palette", PALETTES, "Spectral"),
        actionButton("draw_scoreex", "Explore Scores",
                     class = "btn-info btn-lg", width = "100%")
      ),
      box(width = 9, title = "Score Explorer",
          status = "info", solidHeader = TRUE,
        label_theme_box("scoreex"),
        withSpinner(plotOutput("plot_scoreex", height = "560px"), type = 6)
      )
    )
  )
}


# ── Server ───────────────────────────────────────────────────

#' Server logic for the Score Explorer tab.
#'
#' @param input   Shiny `input` object.
#' @param output  Shiny `output` object.
#' @param session Shiny `session` object.
#' @param rv      Shared `reactiveValues`.
scoreexServer <- function(input, output, session, rv) {

  gc <- function(id, df, pats = NULL) get_col(id, df, pats, input)

  make_ds_picker("ds_scoreex", "Dataset - scoreex", output, input, rv)
  make_col_override("scoreex_col_molecule",       "Molecule", "sel_ds_scoreex",
                    c("title","molecule"), output, input, rv)
  make_col_override("scoreex_col_target",         "Target",   "sel_ds_scoreex",
                    c("pdb id","target"), output, input, rv)
  make_col_override("scoreex_col_score",          "Score",    "sel_ds_scoreex",
                    c("docking score","score"), output, input, rv)
  make_col_override("scoreex_col_fraction",       "Fraction", "sel_ds_scoreex",
                    c("fractions","fraction"), output, input, rv)
  make_col_override("scoreex_col_classification", "Class",    "sel_ds_scoreex",
                    c("pdb classification","classification"), output, input, rv)
  make_filter_ui("scoreex_filter_fraction", "frac", "sel_ds_scoreex",
                 "Filter Fractions:", output, input, rv)

  make_scoreex_plot <- reactive({
    req(input$draw_scoreex)
    isolate({
      nm <- input$sel_ds_scoreex; req(nm, rv$datasets[[nm]])
      df <- rv$datasets[[nm]]
      mol_col   <- gc("scoreex_col_molecule",       df, c("title","molecule"))
      tgt_col   <- gc("scoreex_col_target",         df, c("pdb id","target"))
      score_col <- gc("scoreex_col_score",          df, c("docking score","score"))
      frac_col  <- gc("scoreex_col_fraction",       df, c("fractions","fraction"))
      class_col <- gc("scoreex_col_classification", df, c("pdb classification","classification"))
      df[[score_col]] <- suppressWarnings(as.numeric(df[[score_col]]))
      if (!is.null(input$scoreex_filter_fraction) &&
          length(input$scoreex_filter_fraction) > 0 &&
          frac_col %in% names(df))
        df <- df[as.character(df[[frac_col]]) %in% input$scoreex_filter_fraction, ]
      if (input$scoreex_thresh < 0)
        df <- df[!is.na(df[[score_col]]) & df[[score_col]] <= input$scoreex_thresh, ]
      req(nrow(df) > 0, cancelOutput = TRUE)
      pal     <- input$scoreex_palette %||% "Spectral"
      top_n   <- input$scoreex_top_n
      alpha_v <- input$scoreex_alpha  %||% 0.8
      bins_v  <- input$scoreex_bins   %||% 30

      p <- switch(input$scoreex_type,
        topbar = {
          td <- df %>%
            dplyr::group_by(across(all_of(mol_col))) %>%
            dplyr::summarise(best = min(.data[[score_col]], na.rm = TRUE), .groups = "drop") %>%
            dplyr::arrange(best) %>% dplyr::slice_head(n = top_n) %>%
            dplyr::mutate(mol = forcats::fct_reorder(.data[[mol_col]], best))
          ggplot(td, aes(x = mol, y = best, fill = best)) +
            geom_col(show.legend = FALSE) +
            scale_fill_distiller(palette = pal, direction = -1) +
            coord_flip() +
            labs(x = "Molecule", y = "Best score (kcal/mol)",
                 title = paste0("Top ", top_n, " by best score"))
        },
        dist = {
          ggplot(df, aes(x = .data[[score_col]])) +
            geom_histogram(aes(fill = after_stat(x)),
                           bins = bins_v, colour = "white", alpha = alpha_v) +
            scale_fill_distiller(palette = pal, direction = -1, guide = "none") +
            geom_vline(xintercept = mean(df[[score_col]], na.rm = TRUE),
                       linetype = "dashed", colour = "black") +
            labs(x = "Docking score (kcal/mol)", y = "Count",
                 title = "Score distribution")
        },
        fracbox = {
          req(frac_col %in% names(df))
          ggplot(df, aes(x = .data[[frac_col]], y = .data[[score_col]],
                         fill = .data[[frac_col]])) +
            geom_boxplot(outlier.alpha = 0.3, alpha = alpha_v) +
            geom_jitter(width = 0.15, alpha = 0.2, size = 0.6) +
            scale_fill_brewer(palette = pal, guide = "none") +
            labs(x = "Fraction", y = "Docking score (kcal/mol)",
                 title = "Score by fraction") +
            theme(axis.text.x = element_text(angle = 30, hjust = 1))
        },
        classdot = {
          req(class_col %in% names(df))
          cd <- df %>%
            dplyr::group_by(across(all_of(class_col))) %>%
            dplyr::summarise(mean_sc = mean(.data[[score_col]], na.rm = TRUE),
                             n = dplyr::n(), .groups = "drop") %>%
            dplyr::arrange(mean_sc) %>% dplyr::slice_head(n = top_n) %>%
            dplyr::mutate(cls = forcats::fct_reorder(.data[[class_col]], mean_sc))
          ggplot(cd, aes(x = mean_sc, y = cls, colour = mean_sc, size = n)) +
            geom_point() +
            scale_colour_distiller(palette = pal, direction = -1, name = "Mean score") +
            scale_size_continuous(name = "Count", range = c(2, 10)) +
            labs(x = "Mean score (kcal/mol)", y = "PDB Classification",
                 title = "Mean score by classification")
        },
        scatter = {
          sd2 <- if (nrow(df) > 2000) dplyr::sample_n(df, 2000) else df
          cv  <- if (frac_col %in% names(df)) frac_col else mol_col
          ggplot(sd2, aes(x = .data[[mol_col]], y = .data[[score_col]],
                          colour = .data[[cv]])) +
            geom_jitter(alpha = alpha_v, size = 1.2, width = 0.3) +
            scale_colour_brewer(palette = pal) +
            labs(x = "Molecule", y = "Score (kcal/mol)", colour = cv,
                 title = "Score scatter by molecule") +
            theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7))
        },
        density = {
          grp <- if (frac_col %in% names(df)) frac_col else
            if (class_col %in% names(df)) class_col else tgt_col
          ggplot(df, aes(x = .data[[score_col]],
                         fill = .data[[grp]], colour = .data[[grp]])) +
            geom_density(alpha = 0.18, linewidth = 0.8) +
            labs(x = "Docking score (kcal/mol)", y = "Density",
                 fill = grp, colour = grp, title = "Score density by analytical group") +
            theme_classic()
        },
        targetbox = {
          tb <- df %>%
            dplyr::count(.data[[tgt_col]], name = "n") %>%
            dplyr::arrange(dplyr::desc(n)) %>%
            dplyr::slice_head(n = top_n) %>% dplyr::pull(1)
          bx <- df %>%
            dplyr::filter(.data[[tgt_col]] %in% tb) %>%
            dplyr::mutate(tgt = forcats::fct_reorder(
              .data[[tgt_col]], .data[[score_col]], .fun = median))
          ggplot(bx, aes(x = .data[[score_col]], y = tgt, fill = tgt)) +
            geom_boxplot(alpha = alpha_v, outlier.alpha = 0.18, show.legend = FALSE) +
            scale_fill_brewer(palette = pal) +
            labs(x = "Docking score (kcal/mol)", y = "Target",
                 title = "Score spread across top targets") +
            theme_classic()
        },
        ecdf = {
          grp <- if (frac_col %in% names(df)) frac_col else tgt_col
          ggplot(df, aes(x = .data[[score_col]], colour = .data[[grp]])) +
            stat_ecdf(linewidth = 0.9, alpha = alpha_v) +
            labs(x = "Docking score (kcal/mol)", y = "Cumulative proportion",
                 colour = grp, title = "Empirical cumulative score profile") +
            theme_classic()
        },
        fracviolin = {
          req(frac_col %in% names(df))
          ggplot(df, aes(x = .data[[frac_col]], y = .data[[score_col]],
                         fill = .data[[frac_col]])) +
            geom_violin(alpha = alpha_v, trim = FALSE, show.legend = FALSE) +
            geom_boxplot(width = 0.1, fill = "white", outlier.alpha = 0.15) +
            scale_fill_brewer(palette = pal) +
            labs(x = "Fraction", y = "Docking score (kcal/mol)",
                 title = "Fraction-wise violin score profile") +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 30, hjust = 1))
        },
        targetdot = {
          td <- df %>%
            dplyr::group_by(across(all_of(tgt_col))) %>%
            dplyr::summarise(mean_sc = mean(.data[[score_col]], na.rm = TRUE),
                             best = min(.data[[score_col]], na.rm = TRUE),
                             n = dplyr::n(), .groups = "drop") %>%
            dplyr::arrange(best) %>% dplyr::slice_head(n = top_n) %>%
            dplyr::mutate(tgt = forcats::fct_reorder(.data[[tgt_col]], best))
          ggplot(td, aes(x = best, y = tgt, size = n, colour = mean_sc)) +
            geom_point(alpha = alpha_v) +
            scale_colour_distiller(palette = pal, direction = -1, name = "Mean score") +
            scale_size_continuous(name = "Count", range = c(3, 10)) +
            labs(x = "Best score (kcal/mol)", y = "Target",
                 title = "Target dot summary") +
            theme_classic()
        },
        fracmean = {
          req(frac_col %in% names(df))
          fm <- df %>%
            dplyr::group_by(across(all_of(frac_col))) %>%
            dplyr::summarise(mean_sc = mean(.data[[score_col]], na.rm = TRUE),
                             med_sc  = median(.data[[score_col]], na.rm = TRUE),
                             n = dplyr::n(), .groups = "drop") %>%
            dplyr::mutate(frac = forcats::fct_reorder(.data[[frac_col]], mean_sc))
          ggplot(fm, aes(x = frac, y = mean_sc, fill = n)) +
            geom_col(alpha = alpha_v) +
            geom_point(aes(y = med_sc), colour = "black", size = 2) +
            scale_fill_distiller(palette = pal, direction = 1, name = "Count") +
            labs(x = "Fraction", y = "Mean score (kcal/mol)",
                 title = "Mean and median score by fraction",
                 subtitle = "Black point marks median") +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 30, hjust = 1))
        },
        classbox = {
          req(class_col %in% names(df))
          cls <- df %>%
            dplyr::count(.data[[class_col]], name = "n") %>%
            dplyr::arrange(dplyr::desc(n)) %>%
            dplyr::slice_head(n = top_n) %>% dplyr::pull(1)
          bx <- df %>%
            dplyr::filter(.data[[class_col]] %in% cls) %>%
            dplyr::mutate(cls = forcats::fct_reorder(
              .data[[class_col]], .data[[score_col]], .fun = median))
          ggplot(bx, aes(x = .data[[score_col]], y = cls, fill = cls)) +
            geom_boxplot(alpha = alpha_v, show.legend = FALSE, outlier.alpha = 0.15) +
            scale_fill_brewer(palette = pal) +
            labs(x = "Docking score (kcal/mol)", y = "Class",
                 title = "Class-level boxplot explorer") +
            theme_classic()
        },
        molebubble = {
          mb <- df %>%
            dplyr::group_by(across(all_of(mol_col))) %>%
            dplyr::summarise(mean_sc = mean(.data[[score_col]], na.rm = TRUE),
                             best    = min(.data[[score_col]], na.rm = TRUE),
                             n_tgt   = dplyr::n_distinct(.data[[tgt_col]]),
                             .groups = "drop") %>%
            dplyr::arrange(best) %>% dplyr::slice_head(n = top_n)
          ggplot(mb, aes(x = mean_sc, y = best, size = n_tgt, colour = mean_sc)) +
            geom_point(alpha = alpha_v) +
            scale_colour_distiller(palette = pal, direction = -1, name = "Mean score") +
            scale_size_continuous(name = "# Targets", range = c(3, 10)) +
            labs(x = "Mean score (kcal/mol)", y = "Best score (kcal/mol)",
                 title = "Molecule bubble summary") +
            theme_classic()
        },
        rankline = {
          rk <- df %>% dplyr::arrange(.data[[score_col]]) %>%
            dplyr::mutate(rank = dplyr::row_number())
          ggplot(rk, aes(x = rank, y = .data[[score_col]])) +
            geom_line(colour = "#2c7fb8", linewidth = 0.9, alpha = alpha_v) +
            geom_point(aes(colour = .data[[tgt_col]]), alpha = alpha_v, size = 1.3) +
            labs(x = "Rank", y = "Docking score (kcal/mol)", colour = "Target",
                 title = "Rank-ordered score line") +
            theme_classic()
        },
        rug = {
          grp <- if (frac_col %in% names(df)) frac_col else tgt_col
          ggplot(df, aes(x = .data[[score_col]], colour = .data[[grp]])) +
            geom_density(linewidth = 0.9, alpha = 0.25) +
            geom_rug(alpha = alpha_v) +
            labs(x = "Docking score (kcal/mol)", y = "Density",
                 colour = grp, title = "Density plus rug score explorer") +
            theme_classic()
        }
      )
      apply_labels(p, "scoreex", input)
    })
  })

  output$plot_scoreex <- renderPlot({
    p <- make_scoreex_plot()
    rv$plots[["scoreex"]] <- p
    p
  })
}
