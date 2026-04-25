# ============================================================
#  R/mod_venn.R
#  Module    : Venn / UpSet Set Comparison
#  Description : Compares gene / target sets across datasets or
#                analytical groups.  Ten chart types cover classic
#                Venn diagrams, UpSet-style intersection bars,
#                pairwise overlap heatmaps and gene-membership
#                frequency histograms.
#
#  Data type   : Gene-set / target-set data
#  Required columns :
#    gene    – gene symbol, target name or any item identifier
#    dataset – group label (fraction, condition, dataset name)
#
#  Plot types  :
#    v1             – TCMNP Venn type 1
#    v2             – TCMNP Venn type 2
#    vnet           – TCMNP Venn network overlay
#    upset          – UpSet-style intersection bar chart
#    datasetbar     – Dataset size bar chart
#    overlapheat    – Pairwise Jaccard / overlap heatmap
#    membership     – Membership size distribution bar
#    genefreq       – Gene frequency histogram across datasets
#    pairbar        – Top pairwise overlap bar chart
#    datasetheat    – Dataset × gene membership heatmap
# ============================================================


# ── UI ───────────────────────────────────────────────────────

#' UI for the Venn / UpSet tab.
#' @return A `shinydashboard::tabItem` object.
vennUI <- function() {
  tabItem("venn",
    fluidRow(
      box(width = 3, title = "Parameters", status = "info", solidHeader = TRUE,
        dataset_picker("ds_venn"),
        hr(),
        div(class = "override-group",
          tags$h6("Column roles"),
          uiOutput("venn_col_gene"),
          uiOutput("venn_col_dataset")
        ),
        hr(),
        selectInput("venn_type", "Plot type",
          c("Venn type 1"             = "v1",
            "Venn type 2"             = "v2",
            "Venn network"            = "vnet",
            "UpSet intersection bar"  = "upset",
            "Dataset size bar"        = "datasetbar",
            "Pairwise overlap heatmap"= "overlapheat",
            "Membership distribution" = "membership",
            "Gene frequency histogram"= "genefreq",
            "Top pairwise overlap bar"= "pairbar",
            "Dataset \u00d7 gene heatmap" = "datasetheat"),
          selected = "v1"),
        numericInput("venn_net_top",  "Top N for network / UpSet", 30, 5, 200),
        numericInput("venn_label_deg","Label degree threshold",     2,  1, 20),
        checkboxInput("venn_normalize","Normalise overlaps (Jaccard)", FALSE),
        sliderInput("venn_alpha",     "Alpha", 0.2, 1, 0.85, step = 0.05),
        actionButton("draw_venn", "Draw Set Plot",
                     class = "btn-info btn-lg", width = "100%")
      ),
      box(width = 9, title = "Venn / UpSet Set Comparison",
          status = "info", solidHeader = TRUE,
        label_theme_box("venn", show_xy = FALSE),
        withSpinner(plotOutput("plot_venn", height = "560px"), type = 6)
      )
    )
  )
}


# ── Server ───────────────────────────────────────────────────

#' Server logic for the Venn / UpSet tab.
#'
#' @param input   Shiny `input` object.
#' @param output  Shiny `output` object.
#' @param session Shiny `session` object.
#' @param rv      Shared `reactiveValues`.
vennServer <- function(input, output, session, rv) {

  gc <- function(id, df, pats = NULL) get_col(id, df, pats, input)

  make_ds_picker("ds_venn", "Dataset - venn", output, input, rv)
  make_col_override("venn_col_gene",    "gene / item",  "sel_ds_venn",
                    c("pdb id","gene","target"), output, input, rv)
  make_col_override("venn_col_dataset", "dataset / group", "sel_ds_venn",
                    c("fractions","fraction","dataset"), output, input, rv)

  make_venn_plot <- reactive({
    req(input$draw_venn)
    isolate({
      nm <- input$sel_ds_venn; req(nm, rv$datasets[[nm]])
      df <- rv$datasets[[nm]]
      g  <- gc("venn_col_gene",    df, c("pdb id","gene","target"))
      d  <- gc("venn_col_dataset", df, c("fractions","fraction","dataset"))
      df2 <- df %>%
        dplyr::rename(gene = all_of(g), dataset = all_of(d)) %>%
        dplyr::select(gene, dataset) %>% dplyr::distinct()
      alp <- input$venn_alpha %||% 0.85

      overlap_df <- pairwise_overlap_df(df2)
      if (isTRUE(input$venn_normalize) && nrow(overlap_df)) {
        set_sizes <- df2 %>% dplyr::count(dataset, name = "size")
        overlap_df <- overlap_df %>%
          dplyr::left_join(set_sizes, by = c("set1" = "dataset")) %>%
          dplyr::rename(size1 = size) %>%
          dplyr::left_join(set_sizes, by = c("set2" = "dataset")) %>%
          dplyr::rename(size2 = size) %>%
          dplyr::mutate(
            overlap = ifelse(size1 + size2 - overlap > 0,
                             overlap / (size1 + size2 - overlap), 0))
      }

      p <- switch(input$venn_type,
        v1 = TCMNP::venn_plot(df2, type = 1),
        v2 = TCMNP::venn_plot(df2, type = 2),
        vnet = {
          tg <- names(sort(table(df2$gene), decreasing = TRUE))[
            seq_len(min(input$venn_net_top, nrow(df2)))]
          TCMNP::venn_net(
            rbind(dplyr::sample_n(df2, min(100, nrow(df2))),
                  df2[df2$gene %in% tg, ]),
            label.degree = input$venn_label_deg)
        },
        upset = {
          combo <- df2 %>%
            dplyr::group_by(gene) %>%
            dplyr::summarise(combo  = paste(sort(unique(dataset)), collapse = " & "),
                             n_sets = dplyr::n_distinct(dataset),
                             .groups = "drop") %>%
            dplyr::count(combo, n_sets, name = "genes") %>%
            dplyr::arrange(dplyr::desc(genes), dplyr::desc(n_sets)) %>%
            dplyr::slice_head(n = input$venn_net_top) %>%
            dplyr::mutate(combo = forcats::fct_reorder(combo, genes))
          ggplot(combo, aes(x = genes, y = combo, fill = n_sets)) +
            geom_col(alpha = alp) +
            scale_fill_distiller(palette = "Blues", direction = 1, name = "# Sets") +
            labs(x = "Genes in intersection", y = "Combination",
                 title = "UpSet-style combination frequencies") +
            theme_classic()
        },
        datasetbar = {
          dsz <- df2 %>% dplyr::count(dataset, name = "n") %>%
            dplyr::arrange(dplyr::desc(n)) %>%
            dplyr::mutate(dataset = forcats::fct_reorder(dataset, n))
          ggplot(dsz, aes(x = n, y = dataset, fill = n)) +
            geom_col(alpha = alp) +
            scale_fill_distiller(palette = "Blues", direction = 1, guide = "none") +
            labs(x = "Unique genes", y = "Dataset",
                 title = "Dataset size bar plot") +
            theme_classic()
        },
        overlapheat = {
          ggplot(overlap_df, aes(x = set1, y = set2, fill = overlap)) +
            geom_tile(colour = "white", alpha = alp) +
            scale_fill_distiller(palette = "Blues", direction = 1,
                                 name = if (isTRUE(input$venn_normalize))
                                   "Jaccard" else "Overlap") +
            labs(x = "Dataset", y = "Dataset",
                 title = "Pairwise overlap heatmap") +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        },
        membership = {
          mem <- df2 %>% dplyr::count(gene, name = "n_sets") %>%
            dplyr::count(n_sets, name = "genes")
          ggplot(mem, aes(x = n_sets, y = genes, fill = genes)) +
            geom_col(alpha = alp) +
            scale_fill_distiller(palette = "Purples", direction = 1, guide = "none") +
            labs(x = "# Datasets containing gene", y = "# Genes",
                 title = "Membership size distribution") +
            theme_classic()
        },
        genefreq = {
          mem <- df2 %>% dplyr::count(gene, name = "n_sets")
          ggplot(mem, aes(x = n_sets)) +
            geom_histogram(binwidth = 1, boundary = 0.5,
                           fill = "#4c78a8", colour = "white", alpha = alp) +
            labs(x = "# Datasets containing gene", y = "Frequency",
                 title = "Gene frequency histogram across datasets") +
            theme_classic()
        },
        pairbar = {
          pb <- overlap_df %>%
            dplyr::filter(set1 != set2) %>%
            dplyr::mutate(pair = paste(set1, set2, sep = " vs ")) %>%
            dplyr::arrange(dplyr::desc(overlap)) %>%
            dplyr::slice_head(n = input$venn_net_top) %>%
            dplyr::mutate(pair = forcats::fct_reorder(pair, overlap))
          ggplot(pb, aes(x = overlap, y = pair, fill = overlap)) +
            geom_col(alpha = alp) +
            scale_fill_distiller(palette = "Blues", direction = 1, guide = "none") +
            labs(x = if (isTRUE(input$venn_normalize))
                   "Jaccard index" else "Overlap size",
                 y = "Dataset pair", title = "Top pairwise overlaps") +
            theme_classic()
        },
        datasetheat = {
          top_genes <- df2 %>% dplyr::count(gene, name = "n") %>%
            dplyr::arrange(dplyr::desc(n)) %>%
            dplyr::slice_head(n = input$venn_net_top) %>% dplyr::pull(gene)
          hd <- df2 %>%
            dplyr::filter(gene %in% top_genes) %>%
            dplyr::mutate(val = 1)
          ggplot(hd, aes(x = dataset, y = wrap_text_vec(gene, 20), fill = val)) +
            geom_tile(colour = "white", alpha = alp) +
            scale_fill_distiller(palette = "Greens", direction = 1, guide = "none") +
            labs(x = "Dataset", y = "Gene",
                 title = "Dataset-gene membership heatmap") +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  axis.text.y = element_text(size = 8))
        }
      )
      apply_labels(p, "venn", input, show_xy = FALSE)
    })
  })

  output$plot_venn <- renderPlot({
    p <- make_venn_plot()
    rv$plots[["venn"]] <- p
    p
  })
}
