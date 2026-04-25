# ============================================================
#  global.R
#  Description : Loaded once at app start-up (before ui/server).
#                Installs / attaches all required packages,
#                defines app-wide constants used by every module.
# ============================================================

# ── Core Shiny stack ────────────────────────────────────────
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)

# ── Data I/O ────────────────────────────────────────────────
library(readxl)
library(DT)

# ── Data wrangling ──────────────────────────────────────────
library(dplyr)
library(tidyr)
library(tibble)

# ── Visualisation ───────────────────────────────────────────
library(ggplot2)
library(forcats)

# ── Optional / auto-installed packages ──────────────────────
safe_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}
for (p in c("TCMNP", "ggraph", "igraph", "ggalluvial")) safe_install(p)

# Optional packages used by the General Purpose Plots module and others;
# installed automatically when available.
for (p in c("ggridges", "ggrepel", "treemapify", "hexbin",
            "RColorBrewer", "stringr")) safe_install(p)

HAS_FMSB      <- requireNamespace("fmsb",       quietly = TRUE)
HAS_GGDEND    <- requireNamespace("ggdendro",   quietly = TRUE)
HAS_SCALES    <- requireNamespace("scales",     quietly = TRUE)
HAS_GGRIDGES  <- requireNamespace("ggridges",   quietly = TRUE)
HAS_GGREPEL   <- requireNamespace("ggrepel",    quietly = TRUE)
HAS_TREEMAP   <- requireNamespace("treemapify", quietly = TRUE)
if (HAS_SCALES) library(scales)

# ── App-wide constants ───────────────────────────────────────

#' Named vector of RColorBrewer palette names available throughout the app.
PALETTES <- c(
  "Spectral", "RdYlBu", "RdYlGn", "Set1", "Set2",
  "Paired",   "Blues",  "Greens", "Purples", "Oranges",
  "YlOrRd",   "PiYG",   "PRGn",   "BrBG"
)

#' ggplot2 theme function names shown in every theme selector.
GG_THEMES <- c(
  "theme_classic", "theme_bw",    "theme_minimal",
  "theme_light",   "theme_dark",  "theme_void",
  "theme_gray",    "theme_linedraw"
)

#' Font family choices available in label / theme controls.
FONT_FAMILIES <- c("sans", "serif", "mono", "Helvetica", "Times", "Courier")
