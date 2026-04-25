# ============================================================
#  app.R  –  LekhaChitralya : TCMNP Plot Generator (v4+)
#
#  Entry point for the well-structured Shiny application.
#
#  Structure
#  ---------
#    global.R              – Libraries, constants
#    R/utils.R             – Pure data-utility functions
#    R/ui_helpers.R        – Shared UI components
#    R/server_helpers.R    – Shared server helper functions
#    R/mod_home.R          – Home tab (static guide)
#    R/mod_upload.R        – Data Upload module
#    R/mod_colmap.R        – Column Mapping module
#    R/mod_gtheme.R        – Global Theme module
#    R/mod_best.R          – Best Plots module      (docking data, 18 plots)
#    R/mod_dock.R          – Docking Heatmap module (docking data,  9 plots)
#    R/mod_scoreex.R       – Score Explorer module  (docking data, 15 plots)
#    R/mod_classif.R       – Classification module  (docking data, 14 plots)
#    R/mod_network.R       – TCM Network module     (network data)
#    R/mod_ppi.R           – PPI Network module     (PPI data)
#    R/mod_sankey.R        – Sankey / Alluvial      (network data, 10 diagrams)
#    R/mod_degree.R        – Degree Plots module    (network data, 10 plots)
#    R/mod_venn.R          – Venn / UpSet module    (gene-set data, 10 plots)
#    R/mod_enrich.R        – Enrichment module      (enrichment data, 30+ plots)
#    R/mod_export.R        – Plot Export module
#    www/custom.css        – CSS overrides
#
#  All shared reactive state (datasets, mappings, plots, global
#  theme) lives in `rv` (reactiveValues) and is passed explicitly
#  to every module server function.
#
#  Usage
#    shiny::runApp("lekhachitralya_app")
#    # or from inside the app directory:
#    shiny::runApp()
# ============================================================

# ── Load global settings and all module files ────────────────
source("global.R")
for (f in sort(list.files("R", full.names = TRUE, pattern = "\\.R$")))
  source(f)


# ── UI ───────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "green",

  # ── Header ──────────────────────────────────────────────
  dashboardHeader(
    title = tags$span(
      tags$img(
        src = "https://img.icons8.com/color/24/dna-helix.png",
        style = "margin-right:6px;vertical-align:middle;"
      ),
      "LekhaChitralya"
    )
  ),

  # ── Sidebar ─────────────────────────────────────────────
  dashboardSidebar(
    useShinyjs(),
    width = 240,
    sidebarMenu(id = "tabs",
      menuItem("\U0001f3e0 Home",
               tabName = "home",    icon = icon("home")),
      menuItem("\U0001f4c2 Data Upload",
               tabName = "upload",  icon = icon("upload")),
      menuItem("\U0001f517 Column Mapping",
               tabName = "colmap",  icon = icon("table-columns")),
      menuItem("\U0001f3a8 Global Theme",
               tabName = "gtheme",  icon = icon("palette")),
      menuItem("\U0001f3c6 Best Plots",
               tabName = "best",    icon = icon("star")),
      menuItem("\u2697\ufe0f Docking Heatmap",
               tabName = "dock",    icon = icon("fire")),
      menuItem("\U0001f4ca Score Explorer",
               tabName = "scoreex", icon = icon("chart-bar")),
      menuItem("\U0001f52c Classification",
               tabName = "classif", icon = icon("microscope")),
      menuItem("\U0001f333 TCM Network",
               tabName = "net",     icon = icon("project-diagram")),
      menuItem("\U0001f9ec PPI Network",
               tabName = "ppi",     icon = icon("circle-nodes")),
      menuItem("\U0001f30a Sankey / Alluvial",
               tabName = "sankey",  icon = icon("water")),
      menuItem("\U0001f4cf Degree Plots",
               tabName = "degree",  icon = icon("wave-square")),
      menuItem("\u26cb\ufe0f Venn / UpSet",
               tabName = "venn",    icon = icon("circle-half-stroke")),
      menuItem("\U0001f9ea Enrichment",
               tabName = "enrich",  icon = icon("dna")),
      menuItem("\U0001f4be Export",
               tabName = "export",  icon = icon("download"))
    )
  ),

  # ── Body ────────────────────────────────────────────────
  dashboardBody(
    # Custom CSS
    tags$head(
      tags$link(rel = "stylesheet", href = "custom.css"),
      tags$style(HTML("
        .main-header .logo { font-weight: bold; font-size: 16px; }
        .skin-green .main-header .logo { background-color: #1a2a3a; }
        .skin-green .main-header .navbar { background-color: #1a2a3a; }
        .skin-green .main-header .logo:hover { background-color: #233546; }
      "))
    ),

    tabItems(
      homeUI(),
      uploadUI(),
      colmapUI(),
      gthemeUI(),
      bestUI(),
      dockUI(),
      scoreexUI(),
      classifUI(),
      networkUI(),
      ppiUI(),
      sankeyUI(),
      degreeUI(),
      vennUI(),
      enrichUI(),
      exportUI()
    )
  )
)


# ── Server ───────────────────────────────────────────────────
server <- function(input, output, session) {

  # Shared reactive state
  rv <- reactiveValues(
    datasets = list(),  # name -> data.frame
    mappings = list(),  # name -> named list (canonical -> source col)
    plots    = list(),  # module_key -> ggplot object (for export)
    g_theme  = list()   # global theme defaults
  )

  # Register each module's server logic
  uploadServer(input, output, session, rv)
  colmapServer(input, output, session, rv)
  gthemeServer(input, output, session, rv)
  bestServer(  input, output, session, rv)
  dockServer(  input, output, session, rv)
  scoreexServer(input, output, session, rv)
  classifServer(input, output, session, rv)
  networkServer(input, output, session, rv)
  ppiServer(   input, output, session, rv)
  sankeyServer(input, output, session, rv)
  degreeServer(input, output, session, rv)
  vennServer(  input, output, session, rv)
  enrichServer(input, output, session, rv)
  exportServer(input, output, session, rv)
}

shinyApp(ui, server)
