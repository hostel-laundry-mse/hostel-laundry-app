# Checks R/stats_compare.R against the expected numbers in R_stats_spec.md
# (sections 5 and 6) and exercises section 8 on a made-up feedback form.
# Run from the repo folder:  Rscript dev/test_stats_compare.R
# Needs data/Hostel_Laundry_Clean.csv (git-ignored).
source("R/stats_compare.R")
n_ok <- 0
check <- function(desc, cond) {
  if (!isTRUE(cond)) stop("FAILED: ", desc, call. = FALSE)
  n_ok <<- n_ok + 1; cat("ok -", desc, "\n")
}
r2 <- function(x) round(x, 2)
d <- read_clean_survey()
check("54 respondents, 18 peak", nrow(d) == 54 && sum(d$peak) == 18)

# ---- Section 5 ----
g <- compare_groups(d)
t <- g$tests
row <- function(a, o, cmp = NULL) {
  x <- t[t$analysis == a & t$outcome == o, ]
  if (!is.null(cmp)) x <- x[x$comparison == cmp, ]
  x
}
for (o in list(c("Extra attempts", 0.89, 0.005, 0.82), c("No-machine share", 0.68, 0.016, 0.66),
               c("Satisfaction", 0.86, 0.006, 0.84))) {
  a <- row("ANOVA", o[1]); k <- row("Kruskal-Wallis", o[1])
  check(sprintf("ANOVA %s: p %s, eta2 %s, Kruskal p %s", o[1], o[2], o[3], o[4]),
        r2(a$p_value) == as.numeric(o[2]) && round(a$effect, 3) == as.numeric(o[3]) &&
          r2(k$p_value) == as.numeric(o[4]))
}
check("Tukey: no pair significant", all(g$tukey$p_adj > 0.05))
check("Block means extra attempts A 1.73, C 1.90, D 1.95",
      all(r2(g$block_means$mean[g$block_means$outcome == "Extra attempts"]) == c(1.73, 1.90, 1.95)))
w <- row("Welch t", "Extra attempts")
check("Welch extra attempts: p 0.86, d 0.05, CI -0.59 to 0.70",
      r2(w$p_value) == 0.86 && r2(w$effect) == 0.05 && r2(w$ci_low) == -0.59 && r2(w$ci_high) == 0.70)
check("Welch no-machine share p 0.82", r2(row("Welch t", "No-machine share")$p_value) == 0.82)
check("Welch satisfaction p 0.83", r2(row("Welch t", "Satisfaction")$p_value) == 0.83)
z <- function(cmp) row("Two-proportion z", "Abandonment", cmp)
check("Abandonment peak vs rest: z p 0.29, Fisher 0.35, CI -13 to +40 points",
      r2(z("Peak vs rest")$p_value) == 0.29 && grepl("Fisher p = 0.35", z("Peak vs rest")$note) &&
        round(100 * z("Peak vs rest")$ci_low) == -13 && round(100 * z("Peak vs rest")$ci_high) == 40)
check("Abandonment C vs D: p 0.72, Fisher 1.00",
      r2(z("Block C vs D")$p_value) == 0.72 && grepl("Fisher p = 1.00", z("Block C vs D")$note))
check("Abandonment women vs men: p 0.57, Fisher 0.76",
      r2(z("Women vs men")$p_value) == 0.57 && grepl("Fisher p = 0.76", z("Women vs men")$note))
check("Holm over exactly 9 tests, all 1.00", sum(!is.na(t$p_holm)) == 9 && all(r2(t$p_holm[!is.na(t$p_holm)]) == 1))

# ---- Section 6 ----
dr <- drivers(d)
fit <- dr$fit; co <- dr$coefficients
check("Linear: R2 0.015, n 51", round(fit$r_squared[1], 3) == 0.015 && fit$n[1] == 51)
lin <- co[co$model == "Linear (HC3)" & co$term != "(Intercept)", ]
check("Linear: no coefficient significant (HC3)", all(lin$p_value > 0.05))
check("Linear: block A -0.16 (CI -1.23 to 0.90), timetable 0.29 (p 0.54)",
      r2(lin$estimate[lin$term == "blockA"]) == -0.16 && r2(lin$ci_low[lin$term == "blockA"]) == -1.23 &&
        r2(lin$ci_high[lin$term == "blockA"]) == 0.90 && r2(lin$estimate[lin$term == "timetable_impact"]) == 0.29 &&
        r2(lin$p_value[lin$term == "timetable_impact"]) == 0.54)
check("Poisson dispersion 0.9", round(fit$dispersion[2], 1) == 0.9)
lg <- co[co$model == "Logistic (odds ratios)", ]
check("Logistic: no-machine share OR 0.08 (p 0.09), CI 0.005 to 1.46",
      r2(lg$estimate[lg$term == "no_machine_share"]) == 0.08 && r2(lg$p_value[lg$term == "no_machine_share"]) == 0.09 &&
        round(lg$ci_low[lg$term == "no_machine_share"], 3) == 0.005 && r2(lg$ci_high[lg$term == "no_machine_share"]) == 1.46)
check("Logistic: peak OR 2.10 (p 0.25)",
      r2(lg$estimate[lg$term == "peak"]) == 2.10 && r2(lg$p_value[lg$term == "peak"]) == 0.25)
check("Logistic pseudo R2 0.064", round(fit$r_squared[3], 3) == 0.064)

# ---- Section 8 on a made-up form (shape check only, numbers are fake) ----
before_C <- d[d$block == "C", ]
check("Block C baseline: n 20, pref time 6, pref day 8, give up 6, short 11",
      nrow(before_C) == 20 && sum(before_C$on_pref_time) == 6 && sum(before_C$on_pref_day) == 8 &&
        sum(before_C$abandoned) == 6 && sum(before_C$slot_gap > 0) == 11)
set.seed(1)
check("rule: 75% rounded up -> 15 of 20 (16 of 21, 18 of 24)",
      success_threshold(20) == 15 && success_threshold(21) == 16 && success_threshold(24) == 18)
check("real IDs: R_C01..R_C20 kept, SIM_C and others dropped",
      identical(is_real_resident(c("R_C01", "R_C20", " r_c07 ", "SIM_C01", "S01", "C001")),
                c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)))
n <- 23   # 20 real participants + 3 simulated rows that must be ignored
fake <- data.frame(Timestamp = "x", id = c(sprintf("R_C%02d", 1:20), sprintf("SIM_C%02d", 1:3)),
  oct = sample(c("Yes", "No"), n, TRUE), clash = sample(c("Yes", "No"), n, TRUE, c(.1, .9)),
  day = sample(c("Yes", "No"), n, TRUE, c(.8, .2)), time = sample(c("Yes", "No"), n, TRUE, c(.8, .2)),
  long = "Yes", enough = sample(c("Yes", "No, I need 1 more", "No, I need 2 or more"), n, TRUE, c(.6, .3, .1)),
  use = sample(c("Both", "Only one", "Neither", "Not sure"), n, TRUE, c(.7, .1, .05, .15)),
  trips = sample(c("0", "1", "2", "5 or more"), n, TRUE, c(.5, .3, .15, .05)),
  giveup = sample(c("Yes", "No", "Not sure"), n, TRUE, c(.1, .7, .2)),
  sat = sample(c("1 Very dissatisfied", "3", "4", "5 Very satisfied"), n, TRUE, c(.05, .15, .4, .4)),
  cmp = c(rep("Better", 9), rep("Much better", 6), rep("About the same", 4), "Worse",
          rep("Much better", 3)),
  free = "")
tmp <- tempfile(fileext = ".csv"); write.csv(fake, tmp, row.names = FALSE)
form <- read_feedback_form(tmp)
check("form read: 23 rows, columns renamed in order",
      nrow(form) == 23 && names(form)[12] == "q12_compared" && names(form)[13] == "extra_1")
app <- data.frame(resident_id = form$q1_resident_id, slots = 2, has_pref_day = TRUE, has_pref_band = TRUE,
                  day_match = rep(c(TRUE, FALSE), c(17, 6)), time_match = rep(c(TRUE, FALSE), c(16, 7)))
links <- data.frame(resp_id = before_C$resp_id[1:8], resident_id = form$q1_resident_id[1:8])
ba <- compare_before_after(before_C, form, app, links)
tt <- ba$tests
check("section 8 rows: app + form outcomes, success rule, paired", nrow(tt) == 13)
hl <- tt[tt$outcome == "Preferred time (headline)", ]
check("headline uses the 20 real only: 16/20 vs 6/20, z-test p computed",
      r2(hl$estimate) == r2(16 / 20 - 6 / 20) && hl$p_value < 0.05 && grepl("16/20", hl$note))
sr <- tt[tt$analysis == "Success rule", ]
check("success rule: 15 of 20 real better (SIM ignored) -> MET, p = P(X>=15 | 0.5)",
      sr$statistic == 15 && grepl("15 of 20; rule MET", sr$note) &&
        abs(sr$p_value - (1 - pbinom(14, 20, 0.5))) < 1e-12)
check("15 of 20 by chance under 50/50 is 2.1% (plan doc)", round(1 - pbinom(14, 20, 0.5), 3) == 0.021)
check("power of 15-of-20 rule: 0.62 / 0.80 / 0.93 at 75 / 80 / 85% (plan doc)",
      identical(round(1 - pbinom(14, 20, c(.75, .8, .85)), 2), c(0.62, 0.80, 0.93)))
sat <- tt[tt$outcome == "Satisfaction (1-5)" & tt$analysis == "Before vs after", ]
check("satisfaction: Mann-Whitney with rank-biserial r in [-1, 1]",
      grepl("Mann-Whitney", sat$test) && abs(sat$effect) <= 1)
check("rank-biserial sanity: all after > all before gives r = 1",
      st_rank_biserial(c(4, 5), c(1, 2)) == 1 && st_rank_biserial(c(1, 2), c(4, 5)) == -1)
check("paired rows use the 8 linked people", all(tt$n[tt$analysis == "Paired"] == 8))
check("descriptives: Q3, Q6, Q8 with exact CIs", nrow(ba$descriptives) == 3 && all(ba$descriptives$ci_low <= ba$descriptives$share))
check("'5 or more' read as 5, '1 Very dissatisfied' as 1",
      st_first_number("5 or more") == 5 && st_first_number("1 Very dissatisfied") == 1)
cat("\nAll", n_ok, "checks passed.\n")
