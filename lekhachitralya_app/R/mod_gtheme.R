# ============================================================
#  R/mod_gtheme.R
#  Module    : Global Theme
#  Description : Lets the user set application-wide defaults for
#                ggplot2 theme, font family, base / title font
#                size, colour palette and alpha transparency.
#                Saved defaults are stored in rv$g_theme and
#                honoured as fallbacks when per-plot theme inputs
#                are blank.
#
#  Data type   : N/A (settings only)
#  Writes to   : rv$g_theme (list of theme defaults)
# ============================================================


# ── UI ───────────────────────────────────────────────────────

#' UI for the Global Theme tab.
#' @return A `shinydashboard::tabItem` object.
gthemeUI <- function() {
  tabItem("gtheme",
    fluidRow(
      box(width = 6, title = "Global Defaults", status = "warning", solidHeader = TRUE,
        selectInput("g_theme",   "Default theme",     GG_THEMES, "theme_classic"),
        selectInput("g_font",    "Font family",        FONT_FAMILIES),
        numericInput("g_base_sz",  "Base font size",  12, 6, 28),
        numericInput("g_title_sz", "Title font size", 14, 6, 32),
        hr(),
        textInput("g_xlab",   "Default X label",      ""),
        textInput("g_ylab",   "Default Y label",      ""),
        textInput("g_leglab", "Default legend title", ""),
        hr(),
        actionButton("save_gtheme", "Save Global Defaults",
                     class = "btn-success btn-lg", width = "100%")
      ),
      box(width = 6, title = "Colour Defaults", status = "warning", solidHeader = TRUE,
        selectInput("g_palette", "Default colour palette", PALETTES, "Spectral"),
        sliderInput("g_alpha",   "Default alpha",     0.2, 1, 0.9, step = 0.05),
        numericInput("g_pt_sz",  "Default point size", 3, 0.5, 10, step = 0.5),
        numericInput("g_node_sz", "Default node size", 5, 1, 20)
      )
    )
  )
}


# ── Server ───────────────────────────────────────────────────

#' Server logic for the Global Theme tab.
#'
#' @param input   Shiny `input` object.
#' @param output  Shiny `output` object.
#' @param session Shiny `session` object.
#' @param rv      Shared `reactiveValues` with `$g_theme`.
gthemeServer <- function(input, output, session, rv) {
  observeEvent(input$save_gtheme, {
    rv$g_theme <- list(
      theme   = input$g_theme,
      font    = input$g_font,
      base_sz = input$g_base_sz,
      title_sz = input$g_title_sz,
      palette = input$g_palette,
      alpha   = input$g_alpha
    )
    showNotification("Global theme saved.", type = "message")
  })
}
