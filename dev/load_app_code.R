# Loads the app's R/ files the way shiny::runApp() does, for the dev/ scripts.
# Run the dev/ scripts from the repo folder.
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(DBI); library(RSQLite)
  library(ggplot2); library(dplyr)
})
if (!file.exists("R/mod_results.R")) setwd("..")
extra <- Sys.getenv("RES_EXTRA_R")   # optional extra file to load, e.g. a newer optimiser.R
for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) {
  if (nzchar(extra) && basename(f) == basename(extra)) next
  source(f, local = FALSE)
}
if (nzchar(extra)) source(extra, local = FALSE)
