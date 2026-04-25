# ============================================================
#  R/mod_enrich.R
#  Module    : Enrichment Visualisation
#  Description : The most feature-rich module.  Renders 30+
#                publication-ready charts for GO / KEGG / pathway
#                enrichment results.  Integrates TCMNP's native
#                enrichment plotters and extends them with
#                custom ggplot2 variants covering heatmaps,
#                scatter plots, histograms, radial views,
#                waterfall plots and ontology summaries.
#
#  Data type   : Enrichment data (clusterProfiler / custom format)
#  Required columns (auto-imputed if missing):
#    Description – term label
#    Count       – gene count per term
#    pvalue      – raw p-value
#    GeneRatio   – "hits/tested" ratio string
#    BgRatio     – "bg/total" ratio string
#    geneID      – slash-delimited gene list
#  Optional columns : p.adjust, ONTOLOGY (GO subontology)
#
#  Plot types (selection):
#    bar / dot / lollipop / cir / bubble     – TCMNP core
#    gobar / godot / golollipop / gocir      – TCMNP GO
#    pathcir / pathcc / tfcir / dotsan       – TCMNP path/TF
#    richheat     – Multi-metric enrichment heatmap
#    countp       – Count vs rich-factor scatter
#    genelink     – Gene-link burden bar
#    radialbar    – Radial bar by -log10(p)
#    richbar      – Rich-factor ranking bar
#    pbar         – Significance bar (-log10 p)
#    ontbar       – Ontology term count bar
#    ontdot       – Ontology dot summary
#    termtile     – Term × metric tile view
#    termscatter  – Count vs -log10(p) scatter
#    phist        – P-value histogram
#    counthist    – Gene count histogram
#    linkheat     – Gene-link heatmap
#    polarbubble  – Polar bubble enrichment
#    waterfall    – Waterfall of significance increments
#    rankline     – Ranked enrichment line profile
#    ontstack     – Ontology stacked metric totals
# ============================================================


# ── UI ───────────────────────────────────────────────────────

#' UI for the Enrichment Visualisation tab.
#' @return A `shinydashboard::tabItem` object.
enrichUI <- function() {
  tabItem("enrich",
    fluidRow(
      box(width = 3, title = "Parameters", status = "danger", solidHeader = TRUE,
        dataset_picker("ds_enrich"),
        hr(),
        div(class = "override-group",
          tags$h6("Column overrides"),
          uiOutput("enr_col_desc"),
          uiOutput("enr_col_count"),
          uiOutput("enr_col_pvalue"),
          uiOutput("enr_col_generatio"),
          uiOutput("enr_col_bgratio"),
          uiOutput("enr_col_geneid")
        ),
        hr(),
        selectInput("enrich_type", "Plot type",
          c(
            "TCMNP \u2013 bar"         = "bar",
            "TCMNP \u2013 dot"         = "dot",
            "TCMNP \u2013 lollipop"    = "lollipop",
            "TCMNP \u2013 circular"    = "cir",
            "TCMNP \u2013 bubble"      = "bubble",
            "TCMNP GO \u2013 bar"      = "gobar",
            "TCMNP GO \u2013 dot"      = "godot",
            "TCMNP GO \u2013 lollipop" = "golollipop",
            "TCMNP GO \u2013 circular" = "gocir",
            "TCMNP GO circular (diff)" = "gocirdiff",
            "TCMNP pathway circle"     = "pathcir",
            "TCMNP pathway CC"         = "pathcc",
            "TCMNP TF circular"        = "tfcir",
            "Dot Sankey"               = "dotsan",
            "Enrichment heatmap"       = "richheat",
            "Count vs rich-factor"     = "countp",
            "Gene-link burden bar"     = "genelink",
            "Radial bar"               = "radialbar",
            "Rich-factor bar"          = "richbar",
            "Significance bar"         = "pbar",
            "Ontology term count bar"  = "ontbar",
            "Ontology dot summary"     = "ontdot",
            "Term\u00d7metric tile"    = "termtile",
            "Term scatter"             = "termscatter",
            "P-value histogram"        = "phist",
            "Gene count histogram"     = "counthist",
            "Gene-link heatmap"        = "linkheat",
            "Polar bubble"             = "polarbubble",
            "Waterfall"                = "waterfall",
            "Ranked line profile"      = "rankline",
            "Ontology stacked metrics" = "ontstack"
          ), selected = "bar"),
        textInput("enrich_title",  "Plot title",    "Enrichment Analysis"),
        numericInput("enrich_top_n", "Top N terms", 20, 5, 60),
        numericInput("enrich_wrap",  "Label wrap",  28, 8, 60),
        selectInput("enrich_color",  "Colour palette", PALETTES, "Spectral"),
        sliderInput("enrich_alpha",  "Alpha", 0.2, 1, 0.85, step = 0.05),
        actionButton("draw_enrich", "Draw Enrichment Plot",
                     class = "btn-danger btn-lg", width = "100%")
      ),
      box(width = 9, title = "Enrichment Visualisation",
          status = "danger", solidHeader = TRUE,
        label_theme_box("enrich"),
        withSpinner(plotOutput("plot_enrich", height = "600px"), type = 6)
      )
    )
  )
}


# ── Server ───────────────────────────────────────────────────

#' Server logic for the Enrichment Visualisation tab.
#'
#' @param input   Shiny `input` object.
#' @param output  Shiny `output` object.
#' @param session Shiny `session` object.
#' @param rv      Shared `reactiveValues`.
enrichServer <- function(input, output, session, rv) {

  make_ds_picker("ds_enrich", "Dataset - enrich", output, input, rv)
  make_col_override("enr_col_desc",      "Description", "sel_ds_enrich",
                    c("description","Description"), output, input, rv)
  make_col_override("enr_col_count",     "Count",       "sel_ds_enrich",
                    c("count","Count"), output, input, rv)
  make_col_override("enr_col_pvalue",    "pvalue",      "sel_ds_enrich",
                    c("pvalue","p.value","p-value"), output, input, rv)
  make_col_override("enr_col_generatio", "GeneRatio",   "sel_ds_enrich",
                    c("generatio","gene_ratio","GeneRatio"), output, input, rv)
  make_col_override("enr_col_bgratio",   "BgRatio",     "sel_ds_enrich",
                    c("bgratio","bg_ratio","BgRatio"), output, input, rv)
  make_col_override("enr_col_geneid",    "geneID",      "sel_ds_enrich",
                    c("geneid","geneID","gene"), output, input, rv)

  make_enrich_plot <- reactive({
    req(input$draw_enrich)
    isolate({
      df <- get_mapped("enrich", input, rv)
      df <- apply_col_override(df, list(
        c("enr_col_desc",      "Description"),
        c("enr_col_count",     "Count"),
        c("enr_col_pvalue",    "pvalue"),
        c("enr_col_generatio", "GeneRatio"),
        c("enr_col_bgratio",   "BgRatio"),
        c("enr_col_geneid",    "geneID")
      ), input)
      df <- normalise_enrich_df(df)
      if ("pvalue" %in% names(df))
        df <- df %>% dplyr::arrange(pvalue) %>%
          dplyr::slice_head(n = input$enrich_top_n)
      ttl   <- input$enrich_title %||% "Enrichment"
      alp   <- input$enrich_alpha %||% 0.85
      wrap_w <- input$enrich_wrap %||% 28
      df$Description_wrapped <- wrap_text_vec(df$Description, wrap_w)

      p <- switch(input$enrich_type,
        bar        = TCMNP::bar_plot(df,    title = ttl),
        dot        = TCMNP::dot_plot(df,    title = ttl),
        lollipop   = TCMNP::lollipop_plot(df, title = ttl),
        cir        = TCMNP::cir_plot(df),
        bubble     = TCMNP::bubble_plot(df),
        gobar      = TCMNP::go_barplot(df),
        godot      = TCMNP::go_dotplot(df),
        golollipop = TCMNP::go_lollipop(df),
        gocir      = TCMNP::go_cir(df),
        pathcir    = TCMNP::cir_plot(df),
        gocirdiff  = TCMNP::go_cir(df),
        pathcc     = TCMNP::pathway_ccplot(df, root = ttl),
        tfcir = {
          tf_df <- explode_links(df$Description, df$geneID)
          req(nrow(tf_df) > 0, cancelOutput = TRUE)
          TCMNP::tf_cirplot(tf_df)
        },
        dotsan     = TCMNP::dot_sankey(df),
        richheat = {
          heat_df <- df %>%
            dplyr::transmute(
              Description = Description_wrapped,
              Count       = Count,
              RichFactor  = RichFactor,
              `-log10(p)` = neglog10p
            ) %>%
            tidyr::pivot_longer(-Description,
                                names_to  = "metric",
                                values_to = "value") %>%
            dplyr::group_by(metric) %>%
            dplyr::mutate(value_scaled = (value - min(value, na.rm = TRUE)) /
                            ifelse(diff(range(value, na.rm = TRUE)) == 0, 1,
                                   diff(range(value, na.rm = TRUE)))) %>%
            dplyr::ungroup()
          ggplot(heat_df,
                 aes(x = metric,
                     y = forcats::fct_rev(Description),
                     fill = value_scaled)) +
            geom_tile(colour = "white", linewidth = 0.3) +
            scale_fill_distiller(palette = input$enrich_color %||% "Spectral",
                                 direction = 1, name = "Scaled\nvalue") +
            labs(x = NULL, y = "Enrichment term",
                 title = "Enrichment metric heatmap",
                 subtitle = "Scaled comparison across count, rich factor, and significance") +
            theme_classic() +
            theme(axis.text.x = element_text(face = "bold"),
                  axis.text.y = element_text(size = 8))
        },
        countp = {
          lab_df <- df %>% dplyr::slice_head(n = min(8, nrow(df)))
          ggplot(df, aes(x = Count, y = RichFactor,
                         size = gene_ratio_num, colour = neglog10p)) +
            geom_point(alpha = alp) +
            geom_text(data = lab_df,
                      aes(label = Description_wrapped),
                      hjust = -0.05, vjust = 0.5, size = 3,
                      inherit.aes = FALSE,
                      x = lab_df$Count, y = lab_df$RichFactor) +
            scale_colour_distiller(palette = input$enrich_color %||% "Spectral",
                                   direction = 1, name = "-log10(p)") +
            scale_size_continuous(name = "Gene hits", range = c(3, 10)) +
            scale_x_continuous(expand = expansion(mult = c(0.02, 0.2))) +
            labs(x = "Count", y = "Rich factor",
                 title = "Count versus enrichment strength",
                 subtitle = "Larger points indicate more genes associated with the term") +
            theme_classic()
        },
        genelink = {
          link_df <- explode_links(df$Description_wrapped, df$geneID) %>%
            dplyr::count(term, name = "gene_links") %>%
            dplyr::arrange(dplyr::desc(gene_links)) %>%
            dplyr::slice_head(n = min(15, nrow(.))) %>%
            dplyr::mutate(term = forcats::fct_reorder(term, gene_links))
          ggplot(link_df, aes(x = gene_links, y = term, fill = gene_links)) +
            geom_col(alpha = alp) +
            scale_fill_distiller(palette = input$enrich_color %||% "Spectral",
                                 direction = 1, guide = "none") +
            labs(x = "Linked genes", y = "Enrichment term",
                 title = "Gene-link burden by enrichment term",
                 subtitle = "Useful for spotting broad multi-gene pathways") +
            theme_classic()
        },
        radialbar = {
          rd <- df %>% dplyr::arrange(pvalue) %>%
            dplyr::mutate(term = factor(Description_wrapped,
                                        levels = Description_wrapped))
          ggplot(rd, aes(x = term, y = Count, fill = neglog10p)) +
            geom_col(alpha = alp) +
            coord_polar() +
            scale_fill_distiller(palette = input$enrich_color %||% "Spectral",
                                 direction = 1, name = "-log10(p)") +
            labs(x = NULL, y = "Count", title = "Radial bar enrichment view") +
            theme_minimal()
        },
        richbar = {
          rb <- df %>% dplyr::arrange(RichFactor) %>%
            dplyr::mutate(term = forcats::fct_reorder(Description_wrapped, RichFactor))
          ggplot(rb, aes(x = RichFactor, y = term, fill = Count)) +
            geom_col(alpha = alp) +
            scale_fill_distiller(palette = input$enrich_color %||% "Spectral",
                                 direction = 1, name = "Count") +
            labs(x = "Rich factor", y = "Term",
                 title = "Rich-factor ranking bar plot") +
            theme_classic()
        },
        pbar = {
          pb <- df %>% dplyr::arrange(pvalue) %>%
            dplyr::mutate(term = forcats::fct_reorder(Description_wrapped, neglog10p))
          ggplot(pb, aes(x = neglog10p, y = term, fill = neglog10p)) +
            geom_col(alpha = alp) +
            scale_fill_distiller(palette = input$enrich_color %||% "Spectral",
                                 direction = 1, guide = "none") +
            labs(x = "-log10(p)", y = "Term",
                 title = "Significance-ranked enrichment bar") +
            theme_classic()
        },
        ontbar = {
          od <- df %>% dplyr::count(ONTOLOGY, name = "n")
          ggplot(od, aes(x = ONTOLOGY, y = n, fill = ONTOLOGY)) +
            geom_col(alpha = alp, show.legend = FALSE) +
            scale_fill_brewer(palette = "Set2") +
            labs(x = "Ontology", y = "Term count",
                 title = "Ontology term counts") +
            theme_classic()
        },
        ontdot = {
          od <- df %>%
            dplyr::group_by(ONTOLOGY) %>%
            dplyr::summarise(mean_count = mean(Count,      na.rm = TRUE),
                             mean_rich  = mean(RichFactor, na.rm = TRUE),
                             n = dplyr::n(), .groups = "drop")
          ggplot(od, aes(x = mean_count, y = mean_rich, size = n,
                         colour = ONTOLOGY)) +
            geom_point(alpha = alp) +
            labs(x = "Mean count", y = "Mean rich factor", size = "Terms",
                 title = "Ontology dot summary") +
            theme_classic()
        },
        termtile = {
          td <- df %>% dplyr::transmute(
            term       = Description_wrapped,
            Count      = Count,
            RichFactor = RichFactor,
            Significance = neglog10p
          ) %>%
            tidyr::pivot_longer(-term,
                                names_to  = "metric",
                                values_to = "value")
          ggplot(td, aes(x = metric,
                         y = forcats::fct_rev(term),
                         fill = value)) +
            geom_tile(colour = "white", alpha = alp) +
            scale_fill_distiller(palette = input$enrich_color %||% "Spectral",
                                 direction = 1, name = "Value") +
            labs(x = NULL, y = "Term", title = "Term metric tile view") +
            theme_classic() +
            theme(axis.text.y = element_text(size = 8))
        },
        termscatter = {
          ggplot(df, aes(x = Count, y = neglog10p,
                         size = RichFactor, colour = ONTOLOGY)) +
            geom_point(alpha = alp) +
            labs(x = "Count", y = "-log10(p)",
                 size = "Rich factor", colour = "Ontology",
                 title = "Enrichment term scatter") +
            theme_classic()
        },
        phist = {
          ggplot(df, aes(x = pvalue)) +
            geom_histogram(bins = 20, fill = "#8c6bb1",
                           colour = "white", alpha = alp) +
            labs(x = "p-value", y = "Terms",
                 title = "P-value histogram") +
            theme_classic()
        },
        counthist = {
          ggplot(df, aes(x = Count)) +
            geom_histogram(bins = 20, fill = "#41ab5d",
                           colour = "white", alpha = alp) +
            labs(x = "Count", y = "Terms",
                 title = "Gene count histogram") +
            theme_classic()
        },
        linkheat = {
          link_df <- explode_links(df$Description_wrapped, df$geneID)
          top_genes <- link_df %>% dplyr::count(gene, name = "n") %>%
            dplyr::arrange(dplyr::desc(n)) %>%
            dplyr::slice_head(n = 20) %>% dplyr::pull(gene)
          top_terms <- df$Description_wrapped[seq_len(min(15, nrow(df)))]
          hd <- link_df %>%
            dplyr::filter(gene %in% top_genes, term %in% top_terms) %>%
            dplyr::mutate(val = 1)
          ggplot(hd, aes(x = gene, y = term, fill = val)) +
            geom_tile(colour = "white", alpha = alp) +
            scale_fill_distiller(palette = "YlOrRd",
                                 direction = 1, guide = "none") +
            labs(x = "Gene", y = "Term",
                 title = "Gene-link heatmap") +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
                  axis.text.y = element_text(size = 8))
        },
        polarbubble = {
          pd <- df %>%
            dplyr::mutate(
              term = factor(Description_wrapped, levels = Description_wrapped),
              idx  = dplyr::row_number())
          ggplot(pd, aes(x = idx, y = Count,
                         size = RichFactor, colour = neglog10p)) +
            geom_point(alpha = alp) +
            coord_polar() +
            scale_colour_distiller(palette = input$enrich_color %||% "Spectral",
                                   direction = 1, name = "-log10(p)") +
            labs(x = NULL, y = "Count", size = "Rich factor",
                 title = "Polar bubble enrichment view") +
            theme_minimal()
        },
        waterfall = {
          wf <- df %>% dplyr::arrange(neglog10p) %>%
            dplyr::mutate(
              delta = c(neglog10p[1], diff(neglog10p)),
              term  = forcats::fct_reorder(Description_wrapped, neglog10p))
          ggplot(wf, aes(x = term, y = delta, fill = delta > 0)) +
            geom_col(alpha = alp, show.legend = FALSE) +
            coord_flip() +
            labs(x = "Term", y = "Increment in -log10(p)",
                 title = "Waterfall of enrichment significance") +
            theme_classic()
        },
        rankline = {
          rk <- df %>% dplyr::arrange(pvalue) %>%
            dplyr::mutate(rank = dplyr::row_number())
          ggplot(rk, aes(x = rank, y = neglog10p, colour = ONTOLOGY)) +
            geom_line(linewidth = 0.9) +
            geom_point(aes(size = Count), alpha = alp) +
            labs(x = "Rank", y = "-log10(p)",
                 size = "Count", colour = "Ontology",
                 title = "Ranked enrichment line profile") +
            theme_classic()
        },
        ontstack = {
          st <- df %>%
            dplyr::group_by(ONTOLOGY) %>%
            dplyr::summarise(total_count = sum(Count,      na.rm = TRUE),
                             total_rich  = sum(RichFactor, na.rm = TRUE),
                             .groups = "drop") %>%
            tidyr::pivot_longer(c(total_count, total_rich),
                                names_to  = "metric",
                                values_to = "value")
          ggplot(st, aes(x = ONTOLOGY, y = value, fill = metric)) +
            geom_col(position = "stack", alpha = alp) +
            labs(x = "Ontology", y = "Aggregated value", fill = "Metric",
                 title = "Ontology stacked metric totals") +
            theme_classic()
        }
      )
      apply_labels(p, "enrich", input)
    })
  })

  output$plot_enrich <- renderPlot({
    p <- make_enrich_plot()
    rv$plots[["enrich"]] <- p
    p
  })
}
