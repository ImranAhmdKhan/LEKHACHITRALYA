# ============================================================
#  R/mod_sankey.R
#  Module    : Sankey / Alluvial Diagrams
#  Description : Renders flow diagrams that show connections
#                between herb fractions, bioactive molecules,
#                protein targets and their PDB classifications.
#                Integrates TCMNP Sankey/alluvial functions and
#                adds four supplementary ggplot2 chart types.
#
#  Data type   : Network data (herb–molecule–target)
#  Required columns : herb, molecule, target
#  Optional columns : Description (PDB classification)
#
#  Plot types  :
#    sankey           – TCMNP herb→molecule→target Sankey
#    alluvial         – TCMNP alluvial ribbon diagram
#    sankey_dot       – TCMNP Sankey with dot nodes
#    alluvial_dot     – TCMNP alluvial with dot annotations
#    tf_cir           – TCMNP TF circular diagram
#    dotsankey2       – TCMNP dot-Sankey variant 2
#    herbtargetbar    – Top-15 targets ranked by herb links (bar)
#    moltargetheat    – Molecule × target link-count heatmap
#    herbclassalluvial– Herb → class → target alluvial (ggalluvial)
#    molclassalluvial – Molecule → class → target alluvial (ggalluvial)
# ============================================================


# ── UI ───────────────────────────────────────────────────────

#' UI for the Sankey / Alluvial tab.
#' @return A `shinydashboard::tabItem` object.
sankeyUI <- function() {
  tabItem("sankey",
    fluidRow(
      box(width = 3, title = "Parameters", status = "success", solidHeader = TRUE,
        dataset_picker("ds_sankey"),
        hr(),
        div(class = "override-group",
          tags$h6("Column roles"),
          uiOutput("san_col_herb"),
          uiOutput("san_col_molecule"),
          uiOutput("san_col_target"),
          uiOutput("san_col_description")
        ),
        hr(),
        selectInput("sankey_type", "Diagram type",
          c("TCMNP Sankey"             = "sankey",
            "TCMNP Alluvial"           = "alluvial",
            "TCMNP Sankey Dot"         = "sankey_dot",
            "TCMNP Alluvial Dot"       = "alluvial_dot",
            "TF Circular"              = "tf_cir",
            "Dot Sankey 2"             = "dotsankey2",
            "Herb-target bar"          = "herbtargetbar",
            "Molecule \u00d7 target heatmap" = "moltargetheat",
            "Herb-class alluvial"      = "herbclassalluvial",
            "Molecule-class alluvial"  = "molclassalluvial"),
          selected = "sankey"),
        numericInput("sankey_n",       "Max rows (0 = all)", 200, 0, 5000),
        numericInput("sankey_txt_sz",  "Text size",          3,   1, 8),
        numericInput("sankey_txt_pos", "Text position",      0.5, 0, 1, step = 0.1),
        numericInput("sankey_wrap",    "Label wrap width",   18,  8, 40),
        actionButton("draw_sankey", "Draw Diagram",
                     class = "btn-success btn-lg", width = "100%")
      ),
      box(width = 9, title = "Sankey / Alluvial Visualisation",
          status = "success", solidHeader = TRUE,
        label_theme_box("sankey", show_xy = FALSE),
        withSpinner(plotOutput("plot_sankey", height = "600px"), type = 6)
      )
    )
  )
}


# ── Server ───────────────────────────────────────────────────

#' Server logic for the Sankey / Alluvial tab.
#'
#' @param input   Shiny `input` object.
#' @param output  Shiny `output` object.
#' @param session Shiny `session` object.
#' @param rv      Shared `reactiveValues`.
sankeyServer <- function(input, output, session, rv) {

  gc <- function(id, df, pats = NULL) get_col(id, df, pats, input)

  make_ds_picker("ds_sankey", "Dataset - sankey", output, input, rv)
  make_col_override("san_col_herb",        "herb",        "sel_ds_sankey",
                    c("fractions","fraction"), output, input, rv)
  make_col_override("san_col_molecule",    "molecule",    "sel_ds_sankey",
                    c("title","molecule"), output, input, rv)
  make_col_override("san_col_target",      "target",      "sel_ds_sankey",
                    c("pdb id","target"), output, input, rv)
  make_col_override("san_col_description", "Description", "sel_ds_sankey",
                    c("pdb classification"), output, input, rv)

  make_sankey_plot <- reactive({
    req(input$draw_sankey)
    isolate({
      nm <- input$sel_ds_sankey; req(nm, rv$datasets[[nm]])
      df <- rv$datasets[[nm]]
      h  <- gc("san_col_herb",        df, c("fractions","fraction","herb"))
      m  <- gc("san_col_molecule",    df, c("title","molecule"))
      tg <- gc("san_col_target",      df, c("pdb id","target"))
      ds <- gc("san_col_description", df, c("pdb classification","description"))
      df2 <- df %>%
        dplyr::rename(herb = all_of(h), molecule = all_of(m),
                      target = all_of(tg), Description = all_of(ds)) %>%
        dplyr::select(herb, molecule, target, Description) %>% dplyr::distinct()
      n        <- input$sankey_n
      wrap_w   <- input$sankey_wrap %||% 18
      sr       <- function(d, n) if (n > 0 && nrow(d) > n) d[sample(nrow(d), n), ] else d

      p <- switch(input$sankey_type,
        sankey = TCMNP::tcm_sankey(
          sr(dplyr::select(df2, herb, molecule, target), n),
          text.size     = input$sankey_txt_sz,
          text.position = input$sankey_txt_pos),
        alluvial = TCMNP::tcm_alluvial(
          sr(dplyr::select(df2, herb, molecule, target), n),
          text.size     = input$sankey_txt_sz,
          text.position = input$sankey_txt_pos),
        sankey_dot  = TCMNP::tcm_sankey_dot(sr(df2, n)),
        alluvial_dot = TCMNP::tcm_alluvial_dot(sr(df2, n)),
        tf_cir = {
          tf_df <- sr(df2, n) %>%
            dplyr::select(Description, target) %>% dplyr::distinct()
          names(tf_df) <- c("TF", "Target")
          TCMNP::tf_cirplot(tf_df)
        },
        dotsankey2 = TCMNP::dot_sankey2(sr(df2, n)),
        herbtargetbar = {
          hb <- sr(df2, n) %>%
            dplyr::count(herb, target, name = "n") %>%
            dplyr::group_by(target) %>%
            dplyr::summarise(total = sum(n), .groups = "drop") %>%
            dplyr::arrange(dplyr::desc(total)) %>%
            dplyr::slice_head(n = 15) %>%
            dplyr::right_join(sr(df2, n) %>%
                                dplyr::count(herb, target, name = "n"),
                              by = "target") %>%
            dplyr::mutate(target = forcats::fct_reorder(
              wrap_text_vec(target, wrap_w), total))
          ggplot(hb, aes(x = n, y = target, fill = herb)) +
            geom_col(alpha = 0.85) +
            labs(x = "Links", y = "Target", fill = "Herb",
                 title = "Herb-to-target link distribution") +
            theme_classic()
        },
        moltargetheat = {
          ht <- sr(df2, n) %>%
            dplyr::count(molecule, target, name = "n") %>%
            dplyr::group_by(target) %>%
            dplyr::mutate(t_total = sum(n)) %>% dplyr::ungroup()
          top_targets <- ht %>% dplyr::distinct(target, t_total) %>%
            dplyr::arrange(dplyr::desc(t_total)) %>%
            dplyr::slice_head(n = 15) %>% dplyr::pull(target)
          top_mols <- ht %>%
            dplyr::count(molecule, wt = n, name = "m_total") %>%
            dplyr::arrange(dplyr::desc(m_total)) %>%
            dplyr::slice_head(n = 20) %>% dplyr::pull(molecule)
          ht <- ht %>%
            dplyr::filter(target %in% top_targets, molecule %in% top_mols)
          ggplot(ht, aes(x = wrap_text_vec(target, wrap_w),
                         y = wrap_text_vec(molecule, wrap_w),
                         fill = n)) +
            geom_tile(colour = "white") +
            scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "Links") +
            labs(x = "Target", y = "Molecule",
                 title = "Molecule-target linkage heatmap") +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
                  axis.text.y = element_text(size = 8))
        },
        herbclassalluvial = {
          ad <- sr(df2, n) %>%
            dplyr::mutate(Description = wrap_text_vec(Description, wrap_w)) %>%
            dplyr::count(herb, Description, target, name = "Freq")
          ggplot(ad, aes(axis1 = herb, axis2 = Description, axis3 = target, y = Freq)) +
            ggalluvial::geom_alluvium(aes(fill = Description),
                                      alpha = 0.75, knot.pos = 0.35) +
            ggalluvial::geom_stratum(width = 0.25, fill = "grey92",
                                     colour = "grey50") +
            geom_text(stat = "stratum",
                      aes(label = after_stat(stratum)), size = 3) +
            scale_x_discrete(limits = c("Herb", "Class", "Target"),
                              expand = c(0.1, 0.1)) +
            labs(y = "Links", title = "Herb-class-target alluvial profile",
                 fill = "Class") +
            theme_minimal()
        },
        molclassalluvial = {
          ad <- sr(df2, n) %>%
            dplyr::mutate(
              Description = wrap_text_vec(Description, wrap_w),
              molecule    = wrap_text_vec(molecule, wrap_w)
            ) %>%
            dplyr::count(molecule, Description, target, name = "Freq")
          ggplot(ad, aes(axis1 = molecule, axis2 = Description,
                         axis3 = target, y = Freq)) +
            ggalluvial::geom_alluvium(aes(fill = Description),
                                      alpha = 0.75, knot.pos = 0.35) +
            ggalluvial::geom_stratum(width = 0.25, fill = "grey92",
                                     colour = "grey50") +
            geom_text(stat = "stratum",
                      aes(label = after_stat(stratum)), size = 2.8) +
            scale_x_discrete(limits = c("Molecule", "Class", "Target"),
                              expand = c(0.1, 0.1)) +
            labs(y = "Links",
                 title = "Molecule-class-target alluvial profile",
                 fill = "Class") +
            theme_minimal()
        }
      )
      apply_labels(p, "sankey", input, show_xy = FALSE)
    })
  })

  output$plot_sankey <- renderPlot({
    p <- make_sankey_plot()
    rv$plots[["sankey"]] <- p
    p
  })
}
