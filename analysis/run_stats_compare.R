# Runs Person 4's statistics (spec sections 5, 6 and 8) and saves tables and
# charts. Run from the repo folder:
#   Rscript analysis/run_stats_compare.R
#
# Needs (all git-ignored, never committed):
#   data/Hostel_Laundry_Clean.csv   cleaned October survey (no names)
# For section 8, after the feedback form closes, also:
#   data/feedback_form.csv          Google Forms CSV export of the 12-question form
#   data/laundry.sqlite             the main laptop's database (published schedule
#                                   and questionnaire answers) for the app-computed
#                                   preferred day / time. Optional.
#   data/before_after_links.csv     optional, private: resp_id,resident_id for people
#                                   in both surveys (enables paired tests)
# Writes to output/: CSV tables and output/charts/*.png. Outputs contain IDs and
# numbers only, never names.

suppressPackageStartupMessages(library(ggplot2))
source("R/stats_compare.R")
source("R/stats_charts.R")
dir.create("output", showWarnings = FALSE)
fmt <- function(df) { num <- vapply(df, is.numeric, logical(1)); df[num] <- lapply(df[num], signif, 4); df }
save <- function(df, name) {
  utils::write.csv(fmt(df), file.path("output", paste0(name, ".csv")), row.names = FALSE)
  cat("  wrote output/", name, ".csv\n", sep = "")
}

d <- read_clean_survey()
cat("Survey:", nrow(d), "respondents\n")

cat("\nSection 5: group comparisons\n")
g <- compare_groups(d)
save(g$tests, "s5_group_tests"); save(g$tukey, "s5_tukey"); save(g$block_means, "s5_block_means")
print(fmt(g$tests[, c("analysis", "outcome", "comparison", "p_value", "p_holm", "effect_name", "effect")]),
      row.names = FALSE)

cat("\nSection 6: regressions\n")
dr <- drivers(d)
save(dr$coefficients, "s6_coefficients"); save(dr$fit, "s6_fit")
print(fmt(dr$fit), row.names = FALSE)

ba <- NULL; form <- NULL
if (file.exists("data/feedback_form.csv")) {
  cat("\nSection 8: before vs after\n")
  form <- read_feedback_form("data/feedback_form.csv")
  form <- form[is_real_resident(form$q1_resident_id), ]    # real participants only (R_C01-R_C20)
  cat("  feedback form:", nrow(form), "real participants\n")
  app <- NULL
  if (file.exists("data/laundry.sqlite")) {
    if (!exists("read_bookings", mode = "function")) source("R/data_load.R")
    con <- DBI::dbConnect(RSQLite::SQLite(), "data/laundry.sqlite")
    app <- app_outcomes(read_bookings(con), read_responses(con), load_config("data/config.csv"))
    DBI::dbDisconnect(con)
    save(app, "s8_app_outcomes")
    cat("  app outcomes for", nrow(app), "real participants\n")
  }
  links <- if (file.exists("data/before_after_links.csv")) utils::read.csv("data/before_after_links.csv") else NULL
  ba <- compare_before_after(d[d$block == "C", ], form, app, links)
  save(ba$tests, "s8_before_after"); save(ba$descriptives, "s8_descriptives")
  print(fmt(ba$tests[, c("analysis", "outcome", "comparison", "estimate", "ci_low", "ci_high",
                         "p_value", "effect")]), row.names = FALSE)
} else {
  cat("\nSection 8 skipped: put the form export at data/feedback_form.csv and run again.\n")
}

cat("\nCharts\n")
for (f in save_charts(d, "output/charts", ba, form)) cat("  wrote", f, "\n")
