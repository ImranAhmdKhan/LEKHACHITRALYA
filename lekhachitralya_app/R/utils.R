# ============================================================
#  R/utils.R
#  Description : Pure (stateless) data-utility functions shared
#                across all plot modules.  None of these functions
#                access Shiny's `input`, `output`, or `rv` objects.
#
#  Functions
#  ---------
#  %||%                 NULL / empty-string coalescing operator
#  auto_detect_cols()   Guess column roles from column names
#  pivot_to_matrix()    Long data -> score matrix
#  wrap_text_vec()      Wrap a character vector for axis labels
#  parse_ratio_side()   Extract numerator or denominator of "a/b"
#  explode_links()      Expand slash-separated gene lists to rows
#  normalise_enrich_df() Add computed columns to enrichment data
#  make_degree_df()     Build a herb/molecule/target degree table
#  pairwise_overlap_df() All-pairs intersection count for sets
# ============================================================

#' NULL / empty-string coalescing operator.
`%||%` <- function(a, b) if (!is.null(a) && nzchar(a)) a else b


# ── Column auto-detection ────────────────────────────────────

#' Guess the role of each column in a docking / network data frame
#' by matching column names against known patterns.
#'
#' @param df A data frame.
#' @return Named list with slots: molecule, target, score,
#'   description, fraction, romanid.  Each slot is the matched
#'   column name or NULL when no match is found.
auto_detect_cols <- function(df) {
  nms  <- tolower(names(df))
  orig <- names(df)
  find <- function(patterns)
    orig[which(nms %in% patterns |
                 grepl(paste(patterns, collapse = "|"), nms))[1]]
  list(
    molecule    = find(c("title", "molecule", "compound", "cid", "mol_id")),
    target      = find(c("pdb id", "pdb_id", "pdbid", "target", "protein", "gene")),
    score       = find(c("docking score", "docking_score", "score", "affinity", "binding")),
    description = find(c("pdb classification", "pdb_classification", "classification",
                         "description", "category", "class", "type")),
    fraction    = find(c("fractions", "fraction", "group", "dataset", "sample")),
    romanid     = find(c("romanid", "roman_id", "roman", "id", "compound_id"))
  )
}


# ── Matrix helpers ───────────────────────────────────────────

#' Pivot a long data frame to a numeric score matrix.
#'
#' @param df       Long data frame with at least three columns.
#' @param row_col  Column name for matrix rows.
#' @param col_col  Column name for matrix columns.
#' @param val_col  Column name for values (mean used for duplicates).
#' @return A named numeric matrix (storage.mode "double").
pivot_to_matrix <- function(df, row_col, col_col, val_col) {
  wide <- df %>%
    dplyr::select(all_of(c(row_col, col_col, val_col))) %>%
    dplyr::group_by(across(all_of(c(row_col, col_col)))) %>%
    dplyr::summarise(score = mean(.data[[val_col]], na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from  = all_of(col_col),
      values_from = score,
      values_fill = 0
    )
  mat           <- as.matrix(wide[, -1, drop = FALSE])
  rownames(mat) <- wide[[row_col]]
  storage.mode(mat) <- "double"
  mat
}


# ── Label helpers ────────────────────────────────────────────

#' Wrap a character vector to at most `width` characters per line
#' for use in axis labels and plot annotations.
#'
#' @param x     Character vector.
#' @param width Maximum characters per line (default 28).
#' @return Character vector of the same length as `x`.
wrap_text_vec <- function(x, width = 28) {
  vapply(as.character(x), function(s) {
    if (is.na(s) || !nzchar(s)) return("")
    paste(strwrap(s, width = width), collapse = "\n")
  }, character(1))
}


# ── Ratio parsing ────────────────────────────────────────────

#' Extract one side of a "a/b" ratio string as a numeric vector.
#'
#' @param x    Character (or coercible) vector of ratio strings.
#' @param side Integer: 1 = numerator, 2 = denominator.
#' @return Numeric vector; NA where parsing fails.
parse_ratio_side <- function(x, side = 1L) {
  x <- as.character(x)
  vapply(x, function(v) {
    if (is.na(v) || !nzchar(v)) return(NA_real_)
    if (grepl("/", v, fixed = TRUE)) {
      sp <- strsplit(v, "/", fixed = TRUE)[[1]]
      if (length(sp) >= side) return(suppressWarnings(as.numeric(sp[[side]])))
    }
    suppressWarnings(as.numeric(v))
  }, numeric(1))
}


# ── Gene-link helpers ────────────────────────────────────────

#' Expand slash-separated gene/target lists in an enrichment result
#' into a two-column data frame of term–gene pairs.
#'
#' @param labels Character vector of term labels.
#' @param values Character vector of gene lists (e.g. "GENE1/GENE2").
#' @param sep    Separator string (default "/").
#' @return data.frame with columns `term` and `gene`.
explode_links <- function(labels, values, sep = "/") {
  parts <- Map(function(lbl, val) {
    vals <- trimws(unlist(strsplit(as.character(val), sep, fixed = TRUE)))
    vals <- vals[nzchar(vals)]
    if (!length(vals)) return(NULL)
    data.frame(term = as.character(lbl), gene = vals,
               stringsAsFactors = FALSE)
  }, labels, values)
  dplyr::bind_rows(parts) %>% dplyr::distinct()
}


# ── Enrichment normalisation ─────────────────────────────────

#' Add / impute all columns expected by the enrichment plot
#' functions (Description, ID, Count, pvalue, p.adjust,
#' GeneRatio, BgRatio, geneID, ONTOLOGY, RichFactor, neglog10p,
#' Description_wrapped, gene_ratio_num/den, bg_ratio_num/den).
#'
#' @param df A data frame that may contain a subset of the
#'   standard clusterProfiler / TCMNP enrichment columns.
#' @return Normalised data frame.
normalise_enrich_df <- function(df) {
  df <- as.data.frame(df)
  if (!nrow(df)) return(df)

  if (!"Description" %in% names(df))
    df$Description <- paste("Term", seq_len(nrow(df)))
  if (!"ID" %in% names(df))
    df$ID <- paste0("T", seq_len(nrow(df)))
  if (!"Count" %in% names(df) && "GeneRatio" %in% names(df))
    df$Count <- parse_ratio_side(df$GeneRatio, 1)
  if (!"pvalue" %in% names(df) && "p.adjust" %in% names(df))
    df$pvalue <- df$p.adjust
  if (!"p.adjust" %in% names(df) && "pvalue" %in% names(df))
    df$p.adjust <- df$pvalue
  if (!"GeneRatio" %in% names(df) && "Count" %in% names(df))
    df$GeneRatio <- paste(df$Count, pmax(df$Count, 1), sep = "/")
  if (!"BgRatio" %in% names(df) && "Count" %in% names(df)) {
    bg_total <- max(suppressWarnings(as.numeric(df$Count)), na.rm = TRUE)
    if (!is.finite(bg_total)) bg_total <- nrow(df) + 10
    bg_total <- max(bg_total,
                    max(suppressWarnings(as.numeric(df$Count)),
                        na.rm = TRUE) + 1, 10)
    df$BgRatio <- paste(df$Count, bg_total, sep = "/")
  }
  if (!"geneID" %in% names(df))
    df$geneID <- gsub("\\s+", "_", as.character(df$Description))
  if (!"ONTOLOGY" %in% names(df))
    df$ONTOLOGY <- "GO"

  df$Count   <- suppressWarnings(as.numeric(df$Count))
  df$pvalue  <- suppressWarnings(as.numeric(df$pvalue))
  df$p.adjust <- suppressWarnings(as.numeric(df$p.adjust))
  df$gene_ratio_num <- parse_ratio_side(df$GeneRatio, 1)
  df$gene_ratio_den <- parse_ratio_side(df$GeneRatio, 2)
  df$bg_ratio_num   <- parse_ratio_side(df$BgRatio,   1)
  df$bg_ratio_den   <- parse_ratio_side(df$BgRatio,   2)
  df$RichFactor     <- ifelse(df$bg_ratio_num > 0,
                              df$Count / df$bg_ratio_num, NA_real_)
  df$neglog10p      <- -log10(pmax(df$pvalue, .Machine$double.eps))
  df$Description_wrapped <- wrap_text_vec(df$Description, width = 28)
  df <- df[!is.na(df$Count) & !is.na(df$pvalue), , drop = FALSE]
  rownames(df) <- NULL
  df
}


# ── Degree helpers ───────────────────────────────────────────

#' Compute the degree (number of edges) for each node in a
#' herb-molecule-target network data frame.
#'
#' @param df Data frame with columns `herb`, `molecule`, `target`.
#' @return data.frame with columns: id, type (herb/molecule/target), degree.
make_degree_df <- function(df) {
  deg <- df %>% dplyr::mutate(value = 1)
  herb_degree     <- aggregate(deg$value, by = list(deg$herb),     length) %>%
    as.data.frame() %>% dplyr::mutate(type = "herb")
  molecule_degree <- aggregate(deg$value, by = list(deg$molecule), length) %>%
    as.data.frame() %>% dplyr::mutate(type = "molecule")
  target_degree   <- aggregate(deg$value, by = list(deg$target),   length) %>%
    as.data.frame() %>% dplyr::mutate(type = "target")
  deg2 <- rbind(herb_degree, molecule_degree, target_degree)
  colnames(deg2) <- c("id", "degree", "type")
  deg2 %>% dplyr::select(id, type, degree) %>% as.data.frame()
}


# ── Set-overlap helpers ──────────────────────────────────────

#' Build a pairwise intersection count table for all (set1, set2)
#' combinations in a long gene/item data frame.
#'
#' @param df       Data frame with at least two columns.
#' @param item_col Name of the column containing items (default "gene").
#' @param set_col  Name of the column containing set labels (default "dataset").
#' @return data.frame with columns set1, set2, overlap.
pairwise_overlap_df <- function(df, item_col = "gene", set_col = "dataset") {
  sets <- sort(unique(as.character(df[[set_col]])))
  if (!length(sets))
    return(data.frame(set1 = character(), set2 = character(), overlap = numeric()))
  set_items <- stats::setNames(lapply(sets, function(s) {
    unique(as.character(df[df[[set_col]] == s, item_col]))
  }), sets)
  grid <- expand.grid(set1 = sets, set2 = sets, stringsAsFactors = FALSE)
  grid$overlap <- vapply(seq_len(nrow(grid)), function(i) {
    length(intersect(set_items[[grid$set1[i]]], set_items[[grid$set2[i]]]))
  }, numeric(1))
  grid
}
