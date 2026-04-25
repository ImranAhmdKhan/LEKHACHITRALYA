# ============================================================
#  R/mod_home.R
#  Module    : Home
#  Description : Informational landing page that explains the
#                expected data structure (column roles, example
#                values) and provides a quick-start guide.
#                Contains only a UI function – no server logic.
#
#  Data type   : N/A (static content)
# ============================================================

#' UI for the Home tab.
#' @return A `shinydashboard::tabItem` object.
homeUI <- function() {
  tabItem("home",
    fluidRow(
      box(width = 12, status = "success", solidHeader = TRUE,
          title = "LekhaChitralya \u2013 TCMNP Plot Generator",
          fluidRow(
            column(6,
              h4("\U0001f4cb Expected data structure"),
              tags$table(class = "table table-condensed table-bordered",
                tags$thead(tags$tr(
                  tags$th("Column"), tags$th("Role"), tags$th("Example")
                )),
                tags$tbody(
                  tags$tr(tags$td("Title"),              tags$td("Molecule CID"),       tags$td("534589")),
                  tags$tr(tags$td("PDB ID"),             tags$td("Target protein"),     tags$td("4HT2")),
                  tags$tr(tags$td("PDB CLASSIFICATION"), tags$td("Protein category"),   tags$td("LYASE/LYASE INHIBITOR")),
                  tags$tr(tags$td("docking score"),      tags$td("Affinity kcal/mol"),  tags$td("-8.868")),
                  tags$tr(tags$td("ROMANID"),            tags$td("Compound group"),     tags$td("XXVIII")),
                  tags$tr(tags$td("FRACTIONS"),          tags$td("Sample source"),      tags$td("FRACONE / EXTERNAL"))
                )
              ),
              h4("\U0001f680 Quick-start"),
              tags$ol(
                tags$li("\U0001f4c2 Upload .xlsx / .csv \u2192 Load ALL Sheets"),
                tags$li("\U0001f916 Column Mapping \u2192 Auto-detect & Apply"),
                tags$li(strong("\U0001f3c6 Best Plots"), " \u2192 choose plot type \u2192 Draw"),
                tags$li("\U0001f4be Export \u2192 PNG 300 DPI for publication")
              ),
              hr(),
              h4("\U0001f4ca Supported data types"),
              tags$ul(
                tags$li(strong("Docking data"), " \u2013 molecule, target, score, classification, fraction, ROMANID"),
                tags$li(strong("Network data"), " \u2013 herb, molecule, target"),
                tags$li(strong("PPI data"),     " \u2013 node1, node2, optional score"),
                tags$li(strong("Gene-set data"), " \u2013 gene, dataset (for Venn/UpSet)"),
                tags$li(strong("Enrichment data"), " \u2013 Description, Count, pvalue, GeneRatio, BgRatio, geneID"),
                tags$li(strong("Any tabular data"), " \u2013 use \u2728 General Plots for 82 chart types")
              )
            ),
            column(6,
              h4("\U0001f3c6 Recommended plots for docking data"),
              tags$table(class = "table table-condensed table-bordered",
                tags$thead(tags$tr(
                  tags$th("Plot"), tags$th("What it shows"), tags$th("Tab")
                )),
                tags$tbody(
                  tags$tr(tags$td(span(class = "best-badge", "\u2605"), " Lollipop"),
                          tags$td("Top binding molecules ranked"),     tags$td("\U0001f3c6 Best Plots")),
                  tags$tr(tags$td(span(class = "best-badge", "\u2605"), " Violin"),
                          tags$td("Score spread per FRACTION"),        tags$td("\U0001f3c6 Best Plots")),
                  tags$tr(tags$td(span(class = "best-badge", "\u2605"), " Bubble"),
                          tags$td("Class \u00d7 Fraction matrix"),     tags$td("\U0001f3c6 Best Plots")),
                  tags$tr(tags$td(span(class = "best-badge", "\u2605"), " Hit Rate"),
                          tags$td("% strong binders per molecule"),    tags$td("\U0001f3c6 Best Plots")),
                  tags$tr(tags$td(span(class = "best-badge", "\u2605"), " Heatmap"),
                          tags$td("Molecule \u00d7 Target matrix"),    tags$td("\u2697\ufe0f Docking")),
                  tags$tr(tags$td(span(class = "best-badge", "\u2605"), " Radar"),
                          tags$td("Molecule across protein classes"),   tags$td("\U0001f3c6 Best Plots")),
                  tags$tr(tags$td("Score dist."),
                          tags$td("Overall score histogram"),          tags$td("\U0001f4ca Score Explorer")),
                  tags$tr(tags$td("Class count"),
                          tags$td("Which protein classes targeted"),   tags$td("\U0001f52c Classification"))
                )
              ),
              hr(),
              h4("\u2728 New: General Plots & Plot Gallery"),
              p("LekhaChitralya now includes two additional tabs:"),
              tags$ul(
                tags$li(strong("\U0001f4ca General Plots"), " \u2013 86 chart types for any tabular data,",
                        " organised into 10 groups (Bar, Histogram, Box/Violin, Scatter,",
                        " Line/Area, Heatmap, Part-to-Whole, Ranking, Multivariate, Statistical)."),
                tags$li(strong("\U0001f4ca Plot Gallery"), " \u2013 A searchable catalogue of ",
                        strong("507 named chart types"), " across 20 categories,",
                        " showing which are implemented and where to find them.")
              ),
              p(em("Tip: open the Plot Gallery tab to browse and discover the right chart",
                   " for your data, then use the matching tab to generate it."))
            )
          )
      )
    )
  )
}
