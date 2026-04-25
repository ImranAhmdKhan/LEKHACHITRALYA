# ============================================================
#  R/server_helpers.R
#  Description : Shared server-side helper functions.  All
#                functions that require access to Shiny `input`
#                or the shared reactive-values object `rv` accept
#                those as explicit arguments so they can be safely
#                called from any module server function.
#
#  Functions
#  ---------
#  remap_df()            Rename columns according to a mapping list
#  get_col()             Resolve a column name via override or pattern
#  get_mapped()          Fetch a dataset and apply its saved mapping
#  apply_col_override()  Apply per-plot column-override inputs
#  need_cols()           Validate required columns are present
#  make_ds_picker()      Register renderUI for a dataset selector
#  make_col_override()   Register renderUI for a column-override selector
#  make_filter_ui()      Register renderUI for a fraction-filter widget
# ============================================================


# ── Column renaming ──────────────────────────────────────────

#' Apply a saved column-mapping list to a data frame, renaming
#' source columns to their canonical role names.
#'
#' @param df       Data frame to rename.
#' @param map_list Named list: canonical_name = source_col_name.
#' @return Data frame with renamed columns.
remap_df <- function(df, map_list) {
  for (to in names(map_list)) {
    fr <- map_list[[to]]
    if (!is.null(fr) && nzchar(fr) && fr %in% names(df) && fr != to)
      names(df)[names(df) == fr] <- to
  }
  df
}


# ── Column resolver ──────────────────────────────────────────

#' Resolve a column name, preferring a user override, then name
#' pattern matching, then the first column in the data frame.
#'
#' @param override_id  Input ID of the override selectInput.
#' @param df           Data frame.
#' @param patterns     Character vector of regex / fixed patterns to
#'                     try in order when no override is set.
#' @param input        Shiny `input` object.
#' @return A single column name (character).
get_col <- function(override_id, df, patterns = NULL, input) {
  ov <- input[[override_id]]
  if (!is.null(ov) && nzchar(ov) && ov %in% names(df)) return(ov)
  if (!is.null(patterns)) {
    nms <- tolower(names(df))
    for (p in patterns)
      if (any(grepl(p, nms, fixed = TRUE)))
        return(names(df)[grepl(p, nms, fixed = TRUE)][1])
  }
  names(df)[1]
}


# ── Dataset fetcher ──────────────────────────────────────────

#' Retrieve the dataset selected in a tab's dataset picker and
#' apply its saved column mapping (if any).
#'
#' @param suffix  Tab suffix string used to build the selector ID
#'                ("sel_ds_{suffix}").
#' @param input   Shiny `input` object.
#' @param rv      Shared `reactiveValues` with `$datasets` and
#'                `$mappings`.
#' @return The mapped data frame; calls `req()` on missing data.
get_mapped <- function(suffix, input, rv) {
  id <- paste0("sel_ds_", suffix)
  nm <- input[[id]]
  req(nm, rv$datasets[[nm]])
  df <- rv$datasets[[nm]]
  mp <- rv$mappings[[nm]]
  if (!is.null(mp)) df <- remap_df(df, mp)
  df
}


# ── Column override applier ──────────────────────────────────

#' Apply per-plot column-override selections to a data frame,
#' renaming each selected column to its expected canonical name.
#'
#' @param df    Data frame.
#' @param pairs List of two-element character vectors c(input_id, target_name).
#' @param input Shiny `input` object.
#' @return Data frame with zero or more columns renamed.
apply_col_override <- function(df, pairs, input) {
  for (pair in pairs) {
    ov  <- input[[pair[1]]]
    tgt <- pair[2]
    if (!is.null(ov) && nzchar(ov) && ov %in% names(df) && ov != tgt)
      names(df)[names(df) == ov] <- tgt
  }
  df
}


# ── Column validator ─────────────────────────────────────────

#' Stop with an informative error if required columns are absent.
#'
#' @param df   Data frame to check.
#' @param cols Character vector of required column names.
#' @return Invisibly TRUE; throws an error listing missing columns.
need_cols <- function(df, cols) {
  miss <- setdiff(cols, names(df))
  if (length(miss))
    stop("Missing columns: ", paste(miss, collapse = ", "),
         "\n-> Use Column Mapping or Column Overrides.")
  invisible(TRUE)
}


# ── Dynamic UI factories ─────────────────────────────────────

#' Register an `output` slot that renders a dataset-picker
#' selectInput for a given tab, together with a mapping-status badge.
#'
#' @param out_id  The output slot ID (e.g. "ds_best").
#' @param label   Human-readable label shown above the selectInput.
#' @param output  Shiny `output` object.
#' @param input   Shiny `input` object.
#' @param rv      Shared `reactiveValues`.
make_ds_picker <- function(out_id, label = "Dataset", output, input, rv) {
  sel_id <- gsub("^ds_", "sel_ds_", out_id)
  output[[out_id]] <- renderUI({
    nms <- names(rv$datasets)
    if (!length(nms))
      return(p(style = "color:#c00;font-size:12px;", "Load data first."))
    nm  <- input[[sel_id]]
    badge <- if (!is.null(nm) && !is.null(rv$mappings[[nm]]))
      span(style = "color:#00a65a;font-size:11px;", "Column mapping applied")
    else
      span(style = "color:#e67e22;font-size:11px;",
           "No mapping \u2013 use overrides")
    tagList(
      selectInput(sel_id, label, choices = nms,
                  selected = if (!is.null(nm) && nm %in% nms) nm else nms[1]),
      badge
    )
  })
}


#' Register an `output` slot that renders a column-override
#' selectInput pre-populated by pattern matching on the selected dataset.
#'
#' @param out_id        Output slot ID (also used as the selectInput inputId).
#' @param label         Human-readable label.
#' @param ds_sel_id     Input ID of the companion dataset selector.
#' @param auto_patterns Character vector of fixed patterns tried in order.
#' @param output        Shiny `output` object.
#' @param input         Shiny `input` object.
#' @param rv            Shared `reactiveValues`.
make_col_override <- function(out_id, label, ds_sel_id,
                              auto_patterns = NULL,
                              output, input, rv) {
  output[[out_id]] <- renderUI({
    nm   <- input[[ds_sel_id]]
    req(nm, rv$datasets[[nm]])
    cols <- names(rv$datasets[[nm]])
    pre  <- ""
    if (!is.null(auto_patterns)) {
      lc <- tolower(cols)
      for (pat in auto_patterns)
        if (any(grepl(pat, lc, fixed = TRUE))) {
          pre <- cols[grepl(pat, lc, fixed = TRUE)][1]
          break
        }
    }
    selectInput(out_id, label,
                choices  = c("(use mapping)" = "", cols),
                selected = pre)
  })
}


#' Register an `output` slot that renders a checkboxGroupInput
#' showing all unique values in the fraction column, allowing the
#' user to filter which fractions are included in a plot.
#'
#' @param out_id      Output slot / input ID.
#' @param pattern_key Fixed pattern used to find the fraction column.
#' @param ds_sel_id   Input ID of the dataset selector.
#' @param label       Widget label.
#' @param output      Shiny `output` object.
#' @param input       Shiny `input` object.
#' @param rv          Shared `reactiveValues`.
make_filter_ui <- function(out_id, pattern_key, ds_sel_id, label,
                           output, input, rv) {
  output[[out_id]] <- renderUI({
    nm  <- input[[ds_sel_id]]
    req(nm, rv$datasets[[nm]])
    df  <- rv$datasets[[nm]]
    nms <- tolower(names(df))
    col <- names(df)[grepl(pattern_key, nms)][1]
    if (is.null(col) || is.na(col)) return(NULL)
    vals <- sort(unique(as.character(df[[col]])))
    checkboxGroupInput(out_id, label,
                       choices  = vals,
                       selected = vals,
                       inline   = FALSE)
  })
}
