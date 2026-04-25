# ============================================================
#  R/ui_helpers.R
#  Description : Reusable UI constructor functions used by
#                multiple plot modules.
#
#  Functions
#  ---------
#  dataset_picker()    Dropdown for selecting a loaded dataset
#  label_theme_box()   Collapsible box with labels + theme controls
#  apply_labels()      Apply label / theme inputs to a ggplot object
# ============================================================


#' Render a dataset-selector drop-down (+ mapping status badge).
#' Each plot tab calls this to let the user choose which loaded
#' dataset to visualise.
#'
#' @param id  The `output` slot ID that will receive the renderUI result.
#' @return A `uiOutput()` referencing the given id.
dataset_picker <- function(id) uiOutput(id)


# ── Label + Theme collapsible box ────────────────────────────

#' Build a collapsible shinydashboard box containing inputs for
#' plot title, subtitle, caption, axis labels, legend title,
#' ggplot2 theme, font family and font sizes.
#'
#' @param ns_prefix  String prefix for all input IDs (e.g. "best").
#'   Input IDs generated: `{ns_prefix}_title`, `{ns_prefix}_subtitle`,
#'   `{ns_prefix}_caption`, `{ns_prefix}_xlab`, `{ns_prefix}_ylab`,
#'   `{ns_prefix}_leglab`, `{ns_prefix}_theme`, `{ns_prefix}_font`,
#'   `{ns_prefix}_base_sz`, `{ns_prefix}_title_sz`.
#' @param show_xy  Logical – include X/Y axis label inputs (default TRUE).
#' @param show_leg Logical – include legend title input (default TRUE).
#' @return A `shinydashboard::box()` object.
label_theme_box <- function(ns_prefix, show_xy = TRUE, show_leg = TRUE) {
  box(
    width = 12, collapsible = TRUE, collapsed = TRUE,
    title = "\u270f\ufe0f Labels & Theme", status = "warning", solidHeader = TRUE,
    fluidRow(
      column(4,
        textInput(paste0(ns_prefix, "_title"),    "Plot title",       ""),
        textInput(paste0(ns_prefix, "_subtitle"),  "Subtitle",         ""),
        textInput(paste0(ns_prefix, "_caption"),   "Caption / source", "")
      ),
      column(4,
        if (show_xy) tagList(
          textInput(paste0(ns_prefix, "_xlab"), "X-axis label", ""),
          textInput(paste0(ns_prefix, "_ylab"), "Y-axis label", "")
        ),
        if (show_leg)
          textInput(paste0(ns_prefix, "_leglab"), "Legend title", "")
      ),
      column(4,
        selectInput(paste0(ns_prefix, "_theme"), "ggplot2 theme",
                    GG_THEMES, selected = "theme_classic"),
        selectInput(paste0(ns_prefix, "_font"),  "Font family", FONT_FAMILIES),
        numericInput(paste0(ns_prefix, "_base_sz"),  "Base font size", 12, 6, 28),
        numericInput(paste0(ns_prefix, "_title_sz"), "Title size",     14, 6, 32)
      )
    )
  )
}


# ── Apply labels / theme to a ggplot ────────────────────────

#' Apply the label and theme inputs created by `label_theme_box()`
#' to a ggplot object.
#'
#' @param p          A ggplot object.
#' @param ns_prefix  The same prefix passed to `label_theme_box()`.
#' @param input      Shiny `input` object (passed explicitly to avoid
#'                   capturing issues across module boundaries).
#' @param show_xy    Logical – update x/y axis labels if TRUE.
#' @param show_leg   Logical – update legend title if TRUE.
#' @return The modified ggplot object.
apply_labels <- function(p, ns_prefix, input, show_xy = TRUE, show_leg = TRUE) {
  if (!inherits(p, "gg")) return(p)
  theme_fn <- tryCatch(
    get(input[[paste0(ns_prefix, "_theme")]] %||% "theme_classic"),
    error = function(e) theme_classic
  )
  font_fam <- input[[paste0(ns_prefix, "_font")]]     %||% "sans"
  base_sz  <- input[[paste0(ns_prefix, "_base_sz")]]  %||% 12
  title_sz <- input[[paste0(ns_prefix, "_title_sz")]] %||% 14

  p <- p +
    theme_fn(base_size = base_sz, base_family = font_fam) +
    theme(
      plot.title    = element_text(size = title_sz, face = "bold"),
      plot.subtitle = element_text(size = base_sz - 1),
      plot.caption  = element_text(size = base_sz - 2, colour = "#666"),
      legend.title  = element_text(size = base_sz - 1)
    )

  lbl <- list()
  v   <- function(s) input[[paste0(ns_prefix, "_", s)]] %||% NULL
  if (!is.null(v("title")))    lbl$title    <- v("title")
  if (!is.null(v("subtitle"))) lbl$subtitle <- v("subtitle")
  if (!is.null(v("caption")))  lbl$caption  <- v("caption")
  if (show_xy) {
    if (!is.null(v("xlab"))) lbl$x <- v("xlab")
    if (!is.null(v("ylab"))) lbl$y <- v("ylab")
  }
  if (show_leg && !is.null(v("leglab")))
    lbl$fill <- lbl$colour <- v("leglab")
  if (length(lbl)) p <- p + do.call(labs, lbl)
  p
}
