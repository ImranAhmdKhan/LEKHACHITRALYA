# ============================================================
#  R/mod_best.R
#  Module    : Best Plots
#  Description : Eighteen publication-quality plot types designed
#                specifically for molecular docking long-format data.
#                Highlights the most promising molecules and targets
#                from multiple analytical angles.
#
#  Data type   : Docking data
#  Required columns (auto-detected or via overrides):
#    molecule   – compound identifier (e.g. CID)
#    target     – PDB ID or protein name
#    score      – docking score (kcal/mol, negative = stronger)
#    fraction   – sample fraction / group label
#    classification – PDB protein class
#    romanid    – compound group label
#
#  Plot types  :
#    1.  lollipop       – Top N molecules ranked by best score
#    2.  violin         – Score spread per fraction (violin + box + jitter)
#    3.  bubble         – Classification × Fraction bubble matrix
#    4.  hitrate        – % targets with score ≤ threshold per molecule
#    5.  radar          – Target-class profile for a selected molecule
#    6.  heatmap        – Clustered molecule × target heatmap
#    7.  romanid        – ROMANID group comparison (dumbbell)
#    8.  classbox       – Boxplot of scores by protein class
#    9.  dumbbell       – Best vs mean score dumbbell per molecule
#    10. targettile     – Top-molecule × top-target tile map
#    11. radialbar      – Radial bar of top-binding molecules
#    12. slope          – Best vs median slope per molecule
#    13. fracstack      – Active / inactive composition by fraction
#    14. targetlollipop – Target-level lollipop profile
#    15. targetcount    – Targets ranked by active docking count
#    16. densityclass   – Score density per protein class
#    17. ecdf           – Empirical cumulative distribution by fraction
#    18. meanscatter    – Mean vs best score scatter per molecule
# ============================================================


# ── UI ───────────────────────────────────────────────────────

#' UI for the Best Plots tab.
#' @return A `shinydashboard::tabItem` object.
bestUI <- function() {
  tabItem("best",
    fluidRow(
      box(width = 3, title = "Parameters", status = "danger", solidHeader = TRUE,
        dataset_picker("ds_best"),
        hr(),
        div(class = "override-group",
          tags$h6("Column roles"),
          uiOutput("best_col_molecule"),
          uiOutput("best_col_target"),
          uiOutput("best_col_score"),
          uiOutput("best_col_fraction"),
          uiOutput("best_col_classification"),
          uiOutput("best_col_romanid")
        ),
        hr(),
        div(class = "filter-group",
          tags$h6("Filters"),
          uiOutput("best_filter_fraction"),
          sliderInput("best_score_thresh",
                      "Score threshold (active if score \u2264 x)",
                      min = -15, max = 0, value = -7, step = 0.5),
          numericInput("best_top_n",  "Top N molecules", 20, 5, 100),
          sliderInput("best_alpha",   "Layer alpha",     0.2, 1, 0.85, step = 0.05),
          numericInput("best_wrap",   "Label wrap width", 24, 10, 60)
        ),
        hr(),
        radioButtons("best_type", "Plot type",
          c("Lollipop - top hits"            = "lollipop",
            "Violin - score by fraction"      = "violin",
            "Bubble - class x fraction"       = "bubble",
            "Hit-rate bar - % active"         = "hitrate",
            "Radar - molecule profile"        = "radar",
            "Heatmap (clustered)"             = "heatmap",
            "ROMANID comparison"              = "romanid",
            "Boxplot - score per class"       = "classbox",
            "Dumbbell - best vs mean"         = "dumbbell",
            "Target tile - molecule x target" = "targettile",
            "Radial bar - top molecules"      = "radialbar",
            "Slope - best vs median"          = "slope",
            "Fraction active stack"           = "fracstack",
            "Target lollipop"                 = "targetlollipop",
            "Target count bar"                = "targetcount",
            "Density by class"                = "densityclass",
            "ECDF by fraction"                = "ecdf",
            "Mean vs best scatter"            = "meanscatter"),
          selected = "lollipop"),
        conditionalPanel("input.best_type == 'radar'",
          uiOutput("best_radar_mol_picker")
        ),
        selectInput("best_palette",  "Colour palette",  PALETTES, "Spectral"),
        numericInput("best_base_sz", "Base font size",  12, 6, 22),
        actionButton("draw_best", "Draw Best Plot",
                     class = "btn-danger btn-lg", width = "100%")
      ),
      box(width = 9, title = "Publication-Ready Plot",
          status = "danger", solidHeader = TRUE,
        label_theme_box("best"),
        withSpinner(plotOutput("plot_best", height = "600px"), type = 6),
        hr(),
        uiOutput("best_stats_ui")
      )
    )
  )
}


# ── Server ───────────────────────────────────────────────────

#' Server logic for the Best Plots tab.
#'
#' @param input   Shiny `input` object.
#' @param output  Shiny `output` object.
#' @param session Shiny `session` object.
#' @param rv      Shared `reactiveValues`.
bestServer <- function(input, output, session, rv) {

  # ── Local helpers ────────────────────────────────────────
  gc <- function(id, df, pats = NULL) get_col(id, df, pats, input)

  # ── Dataset picker + column overrides ───────────────────
  make_ds_picker("ds_best", "Dataset - best", output, input, rv)
  make_col_override("best_col_molecule",      "Molecule (Title)",  "sel_ds_best",
                    c("title","molecule","compound","cid"), output, input, rv)
  make_col_override("best_col_target",        "Target (PDB ID)",   "sel_ds_best",
                    c("pdb id","pdb_id","target"), output, input, rv)
  make_col_override("best_col_score",         "Score column",      "sel_ds_best",
                    c("docking score","score","affinity"), output, input, rv)
  make_col_override("best_col_fraction",      "Fraction column",   "sel_ds_best",
                    c("fractions","fraction","group"), output, input, rv)
  make_col_override("best_col_classification","Classification col","sel_ds_best",
                    c("pdb classification","classification","class"), output, input, rv)
  make_col_override("best_col_romanid",       "ROMANID column",    "sel_ds_best",
                    c("romanid","roman_id","roman"), output, input, rv)
  make_filter_ui("best_filter_fraction", "frac", "sel_ds_best",
                 "Filter Fractions:", output, input, rv)

  output$best_radar_mol_picker <- renderUI({
    nm <- input$sel_ds_best; req(nm, rv$datasets[[nm]])
    df  <- rv$datasets[[nm]]
    mol_col <- gc("best_col_molecule", df, c("title","molecule","compound","cid"))
    mols <- sort(unique(as.character(df[[mol_col]])))
    selectInput("best_radar_mol", "Select molecule for radar:",
                choices = mols, selected = mols[1])
  })

  # ── Plot reactive ────────────────────────────────────────
  make_best_plot <- reactive({
    req(input$draw_best)
    isolate({
      nm <- input$sel_ds_best; req(nm, rv$datasets[[nm]])
      df <- rv$datasets[[nm]]

      mol_col   <- gc("best_col_molecule",       df, c("title","molecule","compound","cid"))
      tgt_col   <- gc("best_col_target",         df, c("pdb id","pdb_id","target"))
      score_col <- gc("best_col_score",          df, c("docking score","score","affinity"))
      frac_col  <- gc("best_col_fraction",       df, c("fractions","fraction","group"))
      class_col <- gc("best_col_classification", df, c("pdb classification","classification","class"))
      roman_col <- gc("best_col_romanid",        df, c("romanid","roman_id","roman"))

      df[[score_col]] <- suppressWarnings(as.numeric(df[[score_col]]))

      frac_vals <- input$best_filter_fraction
      if (!is.null(frac_vals) && length(frac_vals) > 0 && frac_col %in% names(df))
        df <- df[as.character(df[[frac_col]]) %in% frac_vals, ]
      req(nrow(df) > 0, cancelOutput = TRUE)

      thresh  <- input$best_score_thresh
      top_n   <- input$best_top_n
      pal     <- input$best_palette %||% "Spectral"
      base_s  <- input$best_base_sz %||% 12
      alpha_v <- input$best_alpha   %||% 0.85
      wrap_w  <- input$best_wrap    %||% 24

      # 1. LOLLIPOP ──────────────────────────────────────────
      if (input$best_type == "lollipop") {
        top_df <- df %>%
          dplyr::group_by(mol = .data[[mol_col]]) %>%
          dplyr::summarise(
            best  = min(.data[[score_col]], na.rm = TRUE),
            mean  = mean(.data[[score_col]], na.rm = TRUE),
            n_tgt = dplyr::n_distinct(.data[[tgt_col]]),
            .groups = "drop") %>%
          dplyr::arrange(best) %>%
          dplyr::slice_head(n = top_n) %>%
          dplyr::mutate(mol = forcats::fct_reorder(mol, best))

        p <- ggplot(top_df, aes(x = best, y = mol)) +
          geom_segment(aes(x = mean, xend = best, y = mol, yend = mol),
                       colour = "grey70", linewidth = 0.8) +
          geom_point(aes(colour = best, size = n_tgt)) +
          geom_vline(xintercept = thresh, linetype = "dashed",
                     colour = "red", alpha = 0.7) +
          scale_colour_distiller(palette = pal, direction = -1,
                                 name = "Best score\n(kcal/mol)") +
          scale_size_continuous(name = "# Targets", range = c(3, 10)) +
          annotate("text", x = thresh + 0.15, y = 1,
                   label = paste0("Threshold\n(", thresh, ")"),
                   colour = "red", size = 3, hjust = 0) +
          labs(x = "Docking score (kcal/mol)", y = "Molecule (CID)",
               title = paste0("Top ", top_n, " molecules \u2013 best binding score"),
               subtitle = "Dot = best score  |  Segment left end = mean score  |  Dot size = no. targets",
               caption = "More negative = stronger binding") +
          theme_classic(base_size = base_s) +
          theme(plot.title = element_text(face = "bold"),
                axis.text.y = element_text(size = base_s - 1))
        return(apply_labels(p, "best", input))
      }

      # 2. VIOLIN ────────────────────────────────────────────
      if (input$best_type == "violin") {
        req(frac_col %in% names(df))
        p <- ggplot(df, aes(x = .data[[frac_col]],
                            y = .data[[score_col]],
                            fill = .data[[frac_col]])) +
          geom_violin(alpha = 0.7, trim = FALSE, colour = "white") +
          geom_boxplot(width = 0.08, fill = "white", outlier.shape = NA) +
          geom_jitter(aes(colour = .data[[score_col]]),
                      width = 0.18, alpha = 0.25, size = 0.9) +
          geom_hline(yintercept = thresh, linetype = "dashed",
                     colour = "red", alpha = 0.8) +
          scale_fill_brewer(palette = pal, guide = "none") +
          scale_colour_distiller(palette = "RdYlBu", direction = -1,
                                 name = "Score", guide = "none") +
          annotate("text", x = 0.6, y = thresh - 0.2,
                   label = paste0("Active threshold (", thresh, ")"),
                   colour = "red", size = 3, hjust = 0) +
          labs(x = "Fraction", y = "Docking score (kcal/mol)",
               title = "Score distribution by sample fraction",
               subtitle = "Inner box = IQR  |  Jitter = individual scores",
               caption = paste0("n = ", nrow(df), " docking entries")) +
          theme_classic(base_size = base_s) +
          theme(axis.text.x = element_text(angle = 25, hjust = 1),
                plot.title = element_text(face = "bold"))
        return(apply_labels(p, "best", input))
      }

      # 3. BUBBLE ────────────────────────────────────────────
      if (input$best_type == "bubble") {
        req(class_col %in% names(df), frac_col %in% names(df))
        top_classes <- df %>%
          dplyr::count(.data[[class_col]]) %>%
          dplyr::arrange(dplyr::desc(n)) %>%
          dplyr::slice_head(n = min(top_n, 20)) %>%
          dplyr::pull(1)
        bub <- df %>%
          dplyr::filter(.data[[class_col]] %in% top_classes) %>%
          dplyr::group_by(cls = .data[[class_col]], frac = .data[[frac_col]]) %>%
          dplyr::summarise(mean_score = mean(.data[[score_col]], na.rm = TRUE),
                           n = dplyr::n(), .groups = "drop")
        p <- ggplot(bub, aes(x = frac, y = cls, colour = mean_score, size = n)) +
          geom_point(alpha = 0.85) +
          scale_colour_distiller(palette = pal, direction = -1,
                                 name = "Mean score\n(kcal/mol)") +
          scale_size_continuous(name = "Count", range = c(3, 16)) +
          labs(x = "Fraction", y = "PDB Classification",
               title = "Target class \u00d7 fraction bubble chart",
               subtitle = "Colour = mean score  |  Size = number of docking entries") +
          theme_classic(base_size = base_s) +
          theme(axis.text.x = element_text(angle = 25, hjust = 1),
                axis.text.y = element_text(size = base_s - 2),
                plot.title = element_text(face = "bold"))
        return(apply_labels(p, "best", input))
      }

      # 4. HIT-RATE BAR ──────────────────────────────────────
      if (input$best_type == "hitrate") {
        hr_df <- df %>%
          dplyr::group_by(mol = .data[[mol_col]]) %>%
          dplyr::summarise(
            total   = dplyr::n(),
            hits    = sum(.data[[score_col]] <= thresh, na.rm = TRUE),
            hit_pct = 100 * hits / total,
            mean_sc = mean(.data[[score_col]], na.rm = TRUE),
            .groups = "drop") %>%
          dplyr::filter(hits > 0) %>%
          dplyr::arrange(dplyr::desc(hit_pct)) %>%
          dplyr::slice_head(n = top_n) %>%
          dplyr::mutate(mol = forcats::fct_reorder(mol, hit_pct))

        p <- ggplot(hr_df, aes(x = hit_pct, y = mol, fill = mean_sc)) +
          geom_col(alpha = 0.85) +
          geom_text(aes(label = paste0(hits, "/", total)),
                    hjust = -0.1, size = 3, colour = "grey30") +
          scale_fill_distiller(palette = pal, direction = -1,
                               name = "Mean score\n(kcal/mol)") +
          scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
          labs(x = paste0("% targets with score \u2264 ", thresh, " kcal/mol"),
               y = "Molecule (CID)",
               title = paste0("Hit rate \u2013 % targets bound at \u2264 ", thresh, " kcal/mol"),
               subtitle = "Label = hits / total targets tested  |  Colour = mean score") +
          theme_classic(base_size = base_s) +
          theme(plot.title = element_text(face = "bold"),
                axis.text.y = element_text(size = base_s - 1))
        return(apply_labels(p, "best", input))
      }

      # 5. RADAR ─────────────────────────────────────────────
      if (input$best_type == "radar") {
        req(class_col %in% names(df))
        sel_mol <- input$best_radar_mol
        req(!is.null(sel_mol) && nzchar(sel_mol))
        rad <- df %>%
          dplyr::filter(.data[[mol_col]] == sel_mol) %>%
          dplyr::group_by(cls = .data[[class_col]]) %>%
          dplyr::summarise(mean_sc = mean(.data[[score_col]], na.rm = TRUE),
                           .groups = "drop") %>%
          dplyr::arrange(mean_sc) %>%
          dplyr::slice_head(n = 12)
        p <- ggplot(rad, aes(x = reorder(cls, mean_sc),
                             y = abs(mean_sc), fill = mean_sc)) +
          geom_col(width = 0.8, alpha = 0.85) +
          geom_text(aes(label = round(mean_sc, 2)), vjust = -0.3, size = 3) +
          coord_polar(start = 0) +
          scale_fill_distiller(palette = pal, direction = -1,
                               name = "Mean score\n(kcal/mol)") +
          labs(x = NULL, y = "|Mean docking score|",
               title = paste0("Target class profile \u2013 molecule ", sel_mol),
               subtitle = "Each spoke = one PDB classification  |  Length = |mean score|") +
          theme_minimal(base_size = base_s) +
          theme(axis.text.x = element_text(size = base_s - 2, colour = "grey30"),
                plot.title = element_text(face = "bold"),
                panel.grid = element_line(colour = "grey88"))
        return(apply_labels(p, "best", input, show_xy = FALSE))
      }

      # 6. CLUSTERED HEATMAP ─────────────────────────────────
      if (input$best_type == "heatmap") {
        top_mols <- df %>%
          dplyr::group_by(.data[[mol_col]]) %>%
          dplyr::summarise(m = min(.data[[score_col]], na.rm = TRUE), .groups = "drop") %>%
          dplyr::arrange(m) %>% dplyr::slice_head(n = top_n) %>% dplyr::pull(1)
        top_tgts <- df %>%
          dplyr::filter(.data[[mol_col]] %in% top_mols) %>%
          dplyr::group_by(.data[[tgt_col]]) %>%
          dplyr::summarise(m = min(.data[[score_col]], na.rm = TRUE), .groups = "drop") %>%
          dplyr::arrange(m) %>% dplyr::slice_head(n = 40) %>% dplyr::pull(1)
        heat <- df %>%
          dplyr::filter(.data[[mol_col]] %in% top_mols, .data[[tgt_col]] %in% top_tgts) %>%
          dplyr::group_by(mol = .data[[mol_col]], tgt = .data[[tgt_col]]) %>%
          dplyr::summarise(score = mean(.data[[score_col]], na.rm = TRUE), .groups = "drop")
        wide_h <- heat %>%
          tidyr::pivot_wider(names_from = tgt, values_from = score, values_fill = 0)
        mat_h  <- as.matrix(wide_h[, -1])
        rownames(mat_h) <- wide_h$mol
        ord_row <- if (nrow(mat_h) > 1)
          rownames(mat_h)[hclust(dist(mat_h), method = "ward.D2")$order] else rownames(mat_h)
        ord_col <- if (ncol(mat_h) > 1)
          colnames(mat_h)[hclust(dist(t(mat_h)), method = "ward.D2")$order] else colnames(mat_h)
        heat$mol <- factor(heat$mol, levels = rev(ord_row))
        heat$tgt <- factor(heat$tgt, levels = ord_col)
        txt_s <- max(5, base_s - 4)
        p <- ggplot(heat, aes(x = tgt, y = mol, fill = score)) +
          geom_tile(colour = "white", linewidth = 0.25) +
          scale_fill_distiller(palette = pal, direction = -1,
                               name = "Score\n(kcal/mol)", na.value = "grey90") +
          labs(x = "Target (PDB ID)", y = "Molecule (CID)",
               title = paste0("Clustered docking heatmap \u2013 top ", top_n, " molecules"),
               subtitle = "Rows and columns ordered by hierarchical clustering (Ward.D2)",
               caption = "Score = mean of duplicate entries") +
          theme_classic(base_size = base_s) +
          theme(axis.text.x = element_text(angle = 70, hjust = 1, size = txt_s),
                axis.text.y = element_text(size = txt_s),
                legend.position = "right",
                plot.title = element_text(face = "bold"),
                plot.margin = margin(10, 10, 10, 10))
        return(apply_labels(p, "best", input))
      }

      # 7. ROMANID COMPARISON ────────────────────────────────
      if (input$best_type == "romanid") {
        req(roman_col %in% names(df))
        rom_df <- df %>%
          dplyr::group_by(rom = .data[[roman_col]]) %>%
          dplyr::summarise(
            best   = min(.data[[score_col]], na.rm = TRUE),
            mean   = mean(.data[[score_col]], na.rm = TRUE),
            median = median(.data[[score_col]], na.rm = TRUE),
            n_mol  = dplyr::n_distinct(.data[[mol_col]]),
            n_ent  = dplyr::n(),
            hit_n  = sum(.data[[score_col]] <= thresh, na.rm = TRUE),
            .groups = "drop") %>%
          dplyr::mutate(hit_pct = 100 * hit_n / n_ent,
                        rom = forcats::fct_reorder(rom, mean))
        p <- ggplot(rom_df) +
          geom_segment(aes(x = rom, xend = rom, y = median, yend = best),
                       colour = "grey60", linewidth = 0.7) +
          geom_point(aes(x = rom, y = best,   colour = best,   size = n_mol), shape = 19) +
          geom_point(aes(x = rom, y = median, colour = median, size = n_mol), shape = 1, stroke = 1.2) +
          geom_hline(yintercept = thresh, linetype = "dashed", colour = "red", alpha = 0.7) +
          scale_colour_distiller(palette = pal, direction = -1, name = "Score\n(kcal/mol)") +
          scale_size_continuous(name = "# Molecules", range = c(3, 10)) +
          coord_flip() +
          labs(x = "ROMANID (compound group)", y = "Docking score (kcal/mol)",
               title = "ROMANID compound group comparison",
               subtitle = "Filled dot = best score  |  Open dot = median  |  Size = no. unique molecules",
               caption = paste0("Red line = active threshold (", thresh, " kcal/mol)")) +
          theme_classic(base_size = base_s) +
          theme(plot.title = element_text(face = "bold"))
        return(apply_labels(p, "best", input))
      }

      # 8. BOXPLOT BY CLASS ──────────────────────────────────
      if (input$best_type == "classbox") {
        req(class_col %in% names(df))
        top_cls <- df %>%
          dplyr::count(.data[[class_col]]) %>%
          dplyr::arrange(dplyr::desc(n)) %>%
          dplyr::slice_head(n = top_n) %>% dplyr::pull(1)
        cb_df <- df %>%
          dplyr::filter(.data[[class_col]] %in% top_cls) %>%
          dplyr::mutate(cls = forcats::fct_reorder(.data[[class_col]],
                                                   .data[[score_col]], .fun = median))
        p <- ggplot(cb_df, aes(x = .data[[score_col]], y = cls, fill = cls)) +
          geom_boxplot(alpha = 0.7, outlier.alpha = 0.2, show.legend = FALSE) +
          scale_fill_brewer(palette = pal) +
          geom_vline(xintercept = thresh, linetype = "dashed", colour = "red", alpha = 0.7) +
          labs(x = "Docking score (kcal/mol)", y = "PDB Classification",
               title = "Score distribution by target protein class",
               subtitle = paste0("Top ", top_n, " most-targeted classes"),
               caption = paste0("Red line = ", thresh, " kcal/mol threshold")) +
          theme_classic(base_size = base_s) +
          theme(axis.text.y = element_text(size = base_s - 2),
                plot.title = element_text(face = "bold"))
        return(apply_labels(p, "best", input))
      }

      # 9. DUMBBELL ──────────────────────────────────────────
      if (input$best_type == "dumbbell") {
        db_df <- df %>%
          dplyr::group_by(mol = .data[[mol_col]]) %>%
          dplyr::summarise(
            best   = min(.data[[score_col]], na.rm = TRUE),
            mean   = mean(.data[[score_col]], na.rm = TRUE),
            median = median(.data[[score_col]], na.rm = TRUE),
            n_tgt  = dplyr::n_distinct(.data[[tgt_col]]),
            .groups = "drop") %>%
          dplyr::arrange(best) %>%
          dplyr::slice_head(n = top_n) %>%
          dplyr::mutate(mol = forcats::fct_reorder(mol, best))
        p <- ggplot(db_df, aes(y = mol)) +
          geom_segment(aes(x = mean, xend = best, yend = mol),
                       colour = "grey70", linewidth = 1) +
          geom_point(aes(x = mean), shape = 21, fill = "white",
                     colour = "#2c3e50", size = 3.2, stroke = 1) +
          geom_point(aes(x = best, colour = best, size = n_tgt), alpha = 0.9) +
          geom_vline(xintercept = thresh, linetype = "dashed", colour = "red", alpha = 0.7) +
          scale_colour_distiller(palette = pal, direction = -1, name = "Best score") +
          scale_size_continuous(name = "# Targets", range = c(3, 9)) +
          labs(x = "Docking score (kcal/mol)", y = "Molecule",
               title = "Best vs mean docking score per molecule",
               subtitle = "Open dot = mean score | Filled dot = best score | Segment = spread") +
          theme_classic(base_size = base_s) +
          theme(plot.title = element_text(face = "bold"))
        return(apply_labels(p, "best", input))
      }

      # 10. TARGET TILE ──────────────────────────────────────
      if (input$best_type == "targettile") {
        top_mols <- df %>%
          dplyr::group_by(.data[[mol_col]]) %>%
          dplyr::summarise(best = min(.data[[score_col]], na.rm = TRUE), .groups = "drop") %>%
          dplyr::arrange(best) %>% dplyr::slice_head(n = min(top_n, 20)) %>% dplyr::pull(1)
        top_tgts <- df %>%
          dplyr::count(.data[[tgt_col]], name = "n") %>%
          dplyr::arrange(dplyr::desc(n)) %>%
          dplyr::slice_head(n = min(top_n, 18)) %>% dplyr::pull(1)
        tt <- df %>%
          dplyr::filter(.data[[mol_col]] %in% top_mols, .data[[tgt_col]] %in% top_tgts) %>%
          dplyr::group_by(mol = .data[[mol_col]], tgt = .data[[tgt_col]]) %>%
          dplyr::summarise(score = mean(.data[[score_col]], na.rm = TRUE), .groups = "drop")
        p <- ggplot(tt, aes(x = tgt, y = mol, fill = score)) +
          geom_tile(colour = "white", linewidth = 0.25) +
          scale_fill_distiller(palette = pal, direction = -1, name = "Mean score") +
          labs(x = "Target (PDB ID)", y = "Molecule",
               title = "Top molecules \u00d7 targets tile map",
               subtitle = "Colour encodes mean docking score for each molecule-target pair") +
          theme_classic(base_size = base_s) +
          theme(axis.text.x = element_text(angle = 60, hjust = 1, size = base_s - 3),
                axis.text.y = element_text(size = base_s - 2),
                plot.title = element_text(face = "bold"))
        return(apply_labels(p, "best", input))
      }

      # 11. RADIAL BAR ───────────────────────────────────────
      if (input$best_type == "radialbar") {
        rb <- df %>%
          dplyr::group_by(mol = .data[[mol_col]]) %>%
          dplyr::summarise(best = min(.data[[score_col]], na.rm = TRUE),
                           hits = sum(.data[[score_col]] <= thresh, na.rm = TRUE),
                           .groups = "drop") %>%
          dplyr::arrange(best) %>%
          dplyr::slice_head(n = top_n) %>%
          dplyr::mutate(mol = wrap_text_vec(mol, wrap_w),
                        mol = factor(mol, levels = mol),
                        score_abs = abs(best))
        p <- ggplot(rb, aes(x = mol, y = score_abs, fill = best)) +
          geom_col(alpha = alpha_v, width = 0.85) +
          coord_polar() +
          scale_fill_distiller(palette = pal, direction = -1, name = "Best score") +
          labs(x = NULL, y = "|Best score|",
               title = "Radial bar plot of top-binding molecules",
               subtitle = "Bar length reflects absolute best docking score") +
          theme_minimal(base_size = base_s) +
          theme(axis.text.x = element_text(size = max(6, base_s - 3)),
                plot.title = element_text(face = "bold"))
        return(apply_labels(p, "best", input, show_xy = FALSE))
      }

      # 12. SLOPE ────────────────────────────────────────────
      if (input$best_type == "slope") {
        sl <- df %>%
          dplyr::group_by(mol = .data[[mol_col]]) %>%
          dplyr::summarise(best   = min(.data[[score_col]], na.rm = TRUE),
                           median = median(.data[[score_col]], na.rm = TRUE),
                           .groups = "drop") %>%
          dplyr::arrange(best) %>%
          dplyr::slice_head(n = top_n) %>%
          dplyr::mutate(mol = wrap_text_vec(mol, wrap_w))
        sl2 <- sl %>%
          tidyr::pivot_longer(c(best, median), names_to = "metric", values_to = "score")
        p <- ggplot(sl2, aes(x = metric, y = score, group = mol, colour = mol)) +
          geom_line(alpha = alpha_v) +
          geom_point(size = 2.2) +
          guides(colour = "none") +
          labs(x = NULL, y = "Docking score (kcal/mol)",
               title = "Best versus median score slope profile",
               subtitle = "Each line represents one molecule") +
          theme_classic(base_size = base_s)
        return(apply_labels(p, "best", input))
      }

      # 13. FRACTION STACK ───────────────────────────────────
      if (input$best_type == "fracstack") {
        req(frac_col %in% names(df))
        fs <- df %>%
          dplyr::mutate(status = ifelse(.data[[score_col]] <= thresh, "Active", "Inactive")) %>%
          dplyr::count(frac = .data[[frac_col]], status, name = "n") %>%
          dplyr::group_by(frac) %>%
          dplyr::mutate(pct = 100 * n / sum(n)) %>%
          dplyr::ungroup()
        p <- ggplot(fs, aes(x = frac, y = pct, fill = status)) +
          geom_col(alpha = alpha_v) +
          scale_fill_brewer(palette = "Set1") +
          labs(x = "Fraction", y = "Percent of docking entries",
               title = "Active versus inactive composition by fraction",
               subtitle = paste0("Active defined as score \u2264 ", thresh)) +
          theme_classic(base_size = base_s) +
          theme(axis.text.x = element_text(angle = 25, hjust = 1))
        return(apply_labels(p, "best", input))
      }

      # 14. TARGET LOLLIPOP ──────────────────────────────────
      if (input$best_type == "targetlollipop") {
        tl <- df %>%
          dplyr::group_by(tgt = .data[[tgt_col]]) %>%
          dplyr::summarise(best  = min(.data[[score_col]], na.rm = TRUE),
                           mean  = mean(.data[[score_col]], na.rm = TRUE),
                           n_mol = dplyr::n_distinct(.data[[mol_col]]),
                           .groups = "drop") %>%
          dplyr::arrange(best) %>%
          dplyr::slice_head(n = top_n) %>%
          dplyr::mutate(tgt = forcats::fct_reorder(wrap_text_vec(tgt, wrap_w), best))
        p <- ggplot(tl, aes(x = best, y = tgt)) +
          geom_segment(aes(x = mean, xend = best, yend = tgt),
                       colour = "grey75", linewidth = 0.9) +
          geom_point(aes(size = n_mol, colour = best), alpha = alpha_v) +
          scale_colour_distiller(palette = pal, direction = -1, name = "Best score") +
          scale_size_continuous(name = "# Molecules", range = c(3, 10)) +
          labs(x = "Docking score (kcal/mol)", y = "Target",
               title = "Target-level lollipop profile",
               subtitle = "Highlights targets attracting the strongest molecule scores") +
          theme_classic(base_size = base_s)
        return(apply_labels(p, "best", input))
      }

      # 15. TARGET COUNT ─────────────────────────────────────
      if (input$best_type == "targetcount") {
        tc <- df %>%
          dplyr::group_by(tgt = .data[[tgt_col]]) %>%
          dplyr::summarise(n      = dplyr::n(),
                           active = sum(.data[[score_col]] <= thresh, na.rm = TRUE),
                           .groups = "drop") %>%
          dplyr::arrange(dplyr::desc(active), dplyr::desc(n)) %>%
          dplyr::slice_head(n = top_n) %>%
          dplyr::mutate(tgt = forcats::fct_reorder(wrap_text_vec(tgt, wrap_w), active))
        p <- ggplot(tc, aes(x = active, y = tgt, fill = n)) +
          geom_col(alpha = alpha_v) +
          scale_fill_distiller(palette = pal, direction = 1, name = "Total pairs") +
          labs(x = "Active molecule-target pairs", y = "Target",
               title = "Targets ranked by active docking counts",
               subtitle = "Useful for spotting broadly bindable targets") +
          theme_classic(base_size = base_s)
        return(apply_labels(p, "best", input))
      }

      # 16. DENSITY BY CLASS ─────────────────────────────────
      if (input$best_type == "densityclass") {
        req(class_col %in% names(df))
        top_cls <- df %>% dplyr::count(.data[[class_col]], name = "n") %>%
          dplyr::arrange(dplyr::desc(n)) %>%
          dplyr::slice_head(n = min(6, top_n)) %>% dplyr::pull(1)
        dd <- df %>% dplyr::filter(.data[[class_col]] %in% top_cls)
        p <- ggplot(dd, aes(x = .data[[score_col]],
                            fill   = .data[[class_col]],
                            colour = .data[[class_col]])) +
          geom_density(alpha = 0.18, linewidth = 0.9) +
          labs(x = "Docking score (kcal/mol)", y = "Density",
               fill = "Class", colour = "Class",
               title = "Density of docking scores by top protein classes") +
          theme_classic(base_size = base_s)
        return(apply_labels(p, "best", input))
      }

      # 17. ECDF ─────────────────────────────────────────────
      if (input$best_type == "ecdf") {
        grp <- if (frac_col %in% names(df)) frac_col else tgt_col
        p <- ggplot(df, aes(x = .data[[score_col]], colour = .data[[grp]])) +
          stat_ecdf(linewidth = 0.9, alpha = alpha_v) +
          geom_vline(xintercept = thresh, linetype = "dashed",
                     colour = "red", alpha = 0.6) +
          labs(x = "Docking score (kcal/mol)", y = "Cumulative proportion",
               colour = grp, title = "Empirical cumulative docking profile") +
          theme_classic(base_size = base_s)
        return(apply_labels(p, "best", input))
      }

      # 18. MEAN SCATTER ─────────────────────────────────────
      if (input$best_type == "meanscatter") {
        ms <- df %>%
          dplyr::group_by(mol = .data[[mol_col]]) %>%
          dplyr::summarise(best  = min(.data[[score_col]], na.rm = TRUE),
                           mean  = mean(.data[[score_col]], na.rm = TRUE),
                           n_tgt = dplyr::n_distinct(.data[[tgt_col]]),
                           .groups = "drop") %>%
          dplyr::arrange(best) %>%
          dplyr::slice_head(n = top_n)
        p <- ggplot(ms, aes(x = mean, y = best, size = n_tgt, colour = best)) +
          geom_abline(slope = 1, intercept = 0,
                      linetype = "dashed", colour = "grey65") +
          geom_point(alpha = alpha_v) +
          scale_colour_distiller(palette = pal, direction = -1, name = "Best score") +
          scale_size_continuous(name = "# Targets", range = c(3, 10)) +
          labs(x = "Mean score (kcal/mol)", y = "Best score (kcal/mol)",
               title = "Mean versus best score per molecule",
               subtitle = "Points farther below the diagonal show stronger best-case gains") +
          theme_classic(base_size = base_s)
        return(apply_labels(p, "best", input))
      }
    })
  })

  # ── Render ───────────────────────────────────────────────
  output$plot_best <- renderPlot({
    p <- tryCatch(make_best_plot(),
                  error = function(e) {
                    showNotification(paste("Plot error:", e$message), type = "error")
                    NULL
                  })
    req(!is.null(p))
    rv$plots[["best"]] <- p
    p
  })

  # ── Summary stats ────────────────────────────────────────
  output$best_stats_ui <- renderUI({
    nm <- input$sel_ds_best
    req(nm, rv$datasets[[nm]], input$draw_best)
    df        <- isolate(rv$datasets[[nm]])
    score_col <- isolate(gc("best_col_score",    df, c("docking score","score","affinity")))
    mol_col   <- isolate(gc("best_col_molecule", df, c("title","molecule","compound","cid")))
    tgt_col   <- isolate(gc("best_col_target",   df, c("pdb id","pdb_id","target")))
    thresh    <- isolate(input$best_score_thresh)
    sc        <- suppressWarnings(as.numeric(df[[score_col]]))
    sc        <- sc[!is.na(sc)]
    n_active  <- sum(sc <= thresh)
    fluidRow(
      valueBox(length(sc),
               "Total docking entries",     icon = icon("database"), color = "green",  width = 3),
      valueBox(dplyr::n_distinct(df[[mol_col]]),
               "Unique molecules",          icon = icon("flask"),    color = "blue",   width = 3),
      valueBox(dplyr::n_distinct(df[[tgt_col]]),
               "Unique targets (PDB IDs)",  icon = icon("dna"),      color = "purple", width = 3),
      valueBox(paste0(n_active, " (", round(100 * n_active / length(sc), 1), "%)"),
               paste0("Active (score \u2264 ", thresh, ")"),
               icon = icon("star"), color = "red", width = 3)
    )
  })
}
