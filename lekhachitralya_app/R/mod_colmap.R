# ============================================================
#  R/mod_colmap.R
#  Module    : Column Mapping
#  Description : Allows users to assign column roles for every
#                downstream module.  Covers five data types:
#                  1. Docking data   (molecule, target, score,
#                                     classification, fraction, ROMANID)
#                  2. Network data   (herb, molecule, target, description)
#                  3. PPI data       (node1, node2, score)
#                  4. Gene-set data  (gene, dataset)
#                  5. Enrichment data(Description, Count, pvalue,
#                                     GeneRatio, BgRatio, geneID)
#                Saved mappings are stored in rv$mappings and applied
#                automatically when a dataset is loaded in a plot tab.
#
#  Data type   : All tabular input types
#  Writes to   : rv$mappings (named list, one entry per dataset)
# ============================================================


# ── UI ───────────────────────────────────────────────────────

#' UI for the Column Mapping tab.
#' @return A `shinydashboard::tabItem` object.
colmapUI <- function() {
  tabItem("colmap",
    fluidRow(
      box(width = 4, title = "Select Sheet to Map", status = "primary", solidHeader = TRUE,
        uiOutput("colmap_sheet_picker"),
        p(style = "color:#888;font-size:12px;", "Detected columns:"),
        verbatimTextOutput("colmap_cols_preview"),
        hr(),
        actionButton("apply_mapping", "Apply Mapping",
                     class = "btn-success btn-lg", width = "100%"),
        br(), br(),
        actionButton("auto_map", "Auto-detect & Apply",
                     class = "btn-info btn-lg", width = "100%"),
        br(),
        p(style = "color:#555;font-size:12px;",
          "Auto-detect uses column name patterns to assign roles.")
      ),
      box(width = 8, title = "Column Role Assignment", status = "success", solidHeader = TRUE,

        div(class = "map-group",
          h5("Docking / Score / Classification / Best Plots"),
          fluidRow(
            column(4, uiOutput("map_molecule_dock")),
            column(4, uiOutput("map_target_dock")),
            column(4, uiOutput("map_score_dock"))
          ),
          fluidRow(
            column(4, uiOutput("map_classification")),
            column(4, uiOutput("map_fraction")),
            column(4, uiOutput("map_romanid"))
          )
        ),

        div(class = "map-group",
          h5("Network / Sankey / Degree"),
          fluidRow(
            column(4, uiOutput("map_herb")),
            column(4, uiOutput("map_molecule")),
            column(4, uiOutput("map_target"))
          ),
          fluidRow(column(4, uiOutput("map_description")))
        ),

        div(class = "map-group",
          h5("PPI"),
          fluidRow(
            column(4, uiOutput("map_node1")),
            column(4, uiOutput("map_node2")),
            column(4, uiOutput("map_score"))
          )
        ),

        div(class = "map-group",
          h5("Venn / Gene-set"),
          fluidRow(
            column(4, uiOutput("map_gene")),
            column(4, uiOutput("map_dataset_col"))
          )
        ),

        div(class = "map-group",
          h5("Enrichment"),
          fluidRow(
            column(4, uiOutput("map_enrich_desc")),
            column(4, uiOutput("map_enrich_count")),
            column(4, uiOutput("map_enrich_pvalue"))
          ),
          fluidRow(
            column(4, uiOutput("map_enrich_generatio")),
            column(4, uiOutput("map_enrich_bgratio")),
            column(4, uiOutput("map_enrich_geneid"))
          )
        )
      )
    )
  )
}


# ── Server ───────────────────────────────────────────────────

#' Server logic for the Column Mapping tab.
#'
#' Renders all mapping select-inputs and handles the
#' "Apply Mapping" / "Auto-detect & Apply" buttons.
#'
#' @param input   Shiny `input` object.
#' @param output  Shiny `output` object.
#' @param session Shiny `session` object.
#' @param rv      Shared `reactiveValues` with `$datasets`, `$mappings`.
colmapServer <- function(input, output, session, rv) {

  # ── Sheet selector ───────────────────────────────────────
  output$colmap_sheet_picker <- renderUI({
    nms <- names(rv$datasets)
    req(length(nms))
    selectInput("colmap_sheet", "Sheet to map:", choices = nms)
  })

  colmap_cols <- reactive({
    req(input$colmap_sheet, rv$datasets[[input$colmap_sheet]])
    names(rv$datasets[[input$colmap_sheet]])
  })

  output$colmap_cols_preview <- renderText({
    paste(colmap_cols(), collapse = "\n")
  })

  # ── Mapping UI factory ───────────────────────────────────
  make_map_ui <- function(out_id, label, field_key, default_name = "") {
    output[[out_id]] <- renderUI({
      cols <- colmap_cols()
      nm   <- input$colmap_sheet
      cur  <- rv$mappings[[nm]]
      pre  <- if (!is.null(cur[[field_key]])) cur[[field_key]] else
        if (nzchar(default_name) && default_name %in% cols) default_name else ""
      selectInput(paste0("mp_", field_key), label,
                  choices = c("none" = "", cols), selected = pre)
    })
  }

  # Docking-data roles
  make_map_ui("map_molecule_dock",    "Molecule / Compound (Title)",  "dock_molecule",  "Title")
  make_map_ui("map_target_dock",      "Target / PDB ID",              "dock_target",    "PDB ID")
  make_map_ui("map_score_dock",       "Docking score",                "dock_score",     "docking score")
  make_map_ui("map_classification",   "PDB Classification",           "dock_classif",   "PDB CLASSIFICATION")
  make_map_ui("map_fraction",         "Fraction / Group",             "dock_fraction",  "FRACTIONS")
  make_map_ui("map_romanid",          "Roman ID",                     "dock_romanid",   "ROMANID")

  # Network-data roles
  make_map_ui("map_herb",             "herb",                         "herb",           "herb")
  make_map_ui("map_molecule",         "molecule",                     "molecule",       "molecule")
  make_map_ui("map_target",           "target",                       "target",         "target")
  make_map_ui("map_description",      "Description",                  "description",    "Description")

  # PPI roles
  make_map_ui("map_node1",            "node1 (PPI source)",           "node1",          "node1")
  make_map_ui("map_node2",            "node2 (PPI target)",           "node2",          "node2")
  make_map_ui("map_score",            "score (PPI weight)",           "score",          "score")

  # Gene-set / Venn roles
  make_map_ui("map_gene",             "gene (Venn)",                  "gene",           "gene")
  make_map_ui("map_dataset_col",      "dataset (Venn group)",         "dataset_col",    "dataset")

  # Enrichment roles
  make_map_ui("map_enrich_desc",      "Description",                  "e_desc",         "Description")
  make_map_ui("map_enrich_count",     "Count",                        "e_count",        "Count")
  make_map_ui("map_enrich_pvalue",    "pvalue",                       "e_pvalue",       "pvalue")
  make_map_ui("map_enrich_generatio", "GeneRatio",                    "e_generatio",    "GeneRatio")
  make_map_ui("map_enrich_bgratio",   "BgRatio",                      "e_bgratio",      "BgRatio")
  make_map_ui("map_enrich_geneid",    "geneID",                       "e_geneid",       "geneID")

  # ── Apply mapping button ─────────────────────────────────
  observeEvent(input$apply_mapping, {
    req(input$colmap_sheet)
    nm <- input$colmap_sheet
    mp <- list(
      dock_molecule = input$mp_dock_molecule, dock_target    = input$mp_dock_target,
      dock_score    = input$mp_dock_score,    dock_classif   = input$mp_dock_classif,
      dock_fraction = input$mp_dock_fraction, dock_romanid   = input$mp_dock_romanid,
      herb          = input$mp_herb,          molecule       = input$mp_molecule,
      target        = input$mp_target,        Description    = input$mp_description,
      node1         = input$mp_node1,         node2          = input$mp_node2,
      score         = input$mp_score,         gene           = input$mp_gene,
      dataset       = input$mp_dataset_col,   Count          = input$mp_enrich_count,
      pvalue        = input$mp_enrich_pvalue, GeneRatio      = input$mp_enrich_generatio,
      BgRatio       = input$mp_enrich_bgratio, geneID        = input$mp_enrich_geneid
    )
    mp <- mp[vapply(mp, function(v) !is.null(v) && nzchar(v), logical(1))]
    rv$mappings[[nm]] <- mp
    showNotification(paste0("Mapping saved for '", nm, "'"), type = "message")
  })

  # ── Auto-detect button ───────────────────────────────────
  observeEvent(input$auto_map, {
    req(input$colmap_sheet)
    nm <- input$colmap_sheet
    df <- rv$datasets[[nm]]
    req(df)
    ad <- auto_detect_cols(df)
    mp <- list(
      dock_molecule = ad$molecule, dock_target = ad$target,
      dock_score    = ad$score,    dock_classif = ad$description,
      dock_fraction = ad$fraction, dock_romanid = ad$romanid
    )
    mp <- mp[vapply(mp, function(v) !is.null(v) && !is.na(v) && nzchar(v), logical(1))]
    rv$mappings[[nm]] <- mp
    showNotification(
      paste0("Auto-mapped ", length(mp), " columns for '", nm, "'"),
      type = "message"
    )
  })
}
