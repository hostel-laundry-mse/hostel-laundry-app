# Statistics, Person 4: sections 5, 6 and 8 of the R stats spec
# (R_stats_spec.md). Sections 1-4 and 7 are Arpith's.
#
# compare_groups(d)                 Section 5 (point 7): ANOVA, Kruskal-Wallis,
#                                   Levene, Tukey across blocks A, C, D; Welch t
#                                   + Mann-Whitney, peak vs rest; two-proportion
#                                   z + Fisher on abandonment; Holm over 9 tests.
# drivers(d)                        Section 6 (point 8): linear regression of
#                                   extra attempts (robust HC3 SEs), Poisson
#                                   check, logistic regression of abandonment.
# read_feedback_form(path)          Reads the 12-question feedback form export.
# app_outcomes(bookings, responses, config)
#                                   App-computed outcomes for real participants:
#                                   share with >= 1 slot on a preferred day and
#                                   >= 1 in their preferred time band.
# compare_before_after(before_C, form, app = NULL, links = NULL)
#                                   Section 8: Block C baseline vs the pre-pilot.
#
# Every function returns tidy data frames with the estimate, 95% CI, test,
# statistic, p-value and effect size, so tables and slides can be built from
# them directly.
#
# Inputs:
#   d        - cleaned survey, read with read_clean_survey()
#              (data/Hostel_Laundry_Clean.csv, git-ignored, no names)
#   before_C - the Block C rows of d (n = 20)
#
# Conventions (from the spec): 95% CIs; Block B (n = 3) left out of block
# comparisons and regressions; prop.test(correct = FALSE); never names, only
# resp_id / resident_id.
#
# This file only defines functions: R/ files are sourced when the app starts,
# so nothing here runs or loads packages at the top. Packages car, sandwich and
# lmtest are called with :: inside the functions.

# ---- Shared helpers ----------------------------------------------------------

PEAK_BAND <- "8 PM - 12 AM"

read_clean_survey <- function(path = "data/Hostel_Laundry_Clean.csv") {
  d <- utils::read.csv(path, stringsAsFactors = FALSE)
  d$peak <- as.integer(d$pref_time == PEAK_BAND)
  d
}

# One row of results in the shared format.
st_row <- function(section, analysis, outcome, comparison, estimate = NA,
                   ci_low = NA, ci_high = NA, test = NA, statistic = NA, df = NA,
                   p_value = NA, effect_name = NA, effect = NA, n = NA, note = NA) {
  data.frame(section = section, analysis = analysis, outcome = outcome,
             comparison = comparison, estimate = estimate, ci_low = ci_low,
             ci_high = ci_high, test = test, statistic = statistic, df = df,
             p_value = p_value, effect_name = effect_name, effect = effect, n = n,
             note = note, stringsAsFactors = FALSE)
}

# Two-proportion z-test (no continuity correction) plus Fisher's exact test.
# x, n: successes and sizes for group 1 then group 2.
st_two_prop <- function(x, n) {
  z <- suppressWarnings(stats::prop.test(x, n, correct = FALSE))
  tab <- matrix(c(x, n - x), nrow = 2)
  list(p1 = x[1] / n[1], p2 = x[2] / n[2], diff = x[1] / n[1] - x[2] / n[2],
       ci = unname(z$conf.int), z = sign(x[1] / n[1] - x[2] / n[2]) * sqrt(unname(z$statistic)),
       p = z$p.value, fisher_p = stats::fisher.test(tab)$p.value)
}

# Rank-biserial correlation for a Mann-Whitney test of a vs b:
# r = 2U / (n_a n_b) - 1, where U counts pairs with a > b (ties count half).
# Positive means a tends to be larger.
st_rank_biserial <- function(a, b) {
  a <- a[!is.na(a)]; b <- b[!is.na(b)]
  u <- sum(outer(a, b, ">")) + 0.5 * sum(outer(a, b, "=="))
  2 * u / (length(a) * length(b)) - 1
}

# ---- Section 5: group comparisons (point 7) -----------------------------------

compare_groups <- function(d) {
  if (is.null(d$peak)) d$peak <- as.integer(d$pref_time == PEAK_BAND)
  acd <- d[d$block %in% c("A", "C", "D"), ]
  acd$block <- factor(acd$block)
  outcomes <- c(extra_attempts = "Extra attempts", no_machine_share = "No-machine share",
                satisfaction = "Satisfaction")
  rows <- list(); tukey <- list(); means <- list()

  # ANOVA + Kruskal-Wallis + Levene + Tukey, blocks A, C, D
  for (y in names(outcomes)) {
    f <- stats::as.formula(paste(y, "~ block"))
    m <- stats::aov(f, data = acd)
    s <- summary(m)[[1]]
    eta2 <- s$`Sum Sq`[1] / sum(s$`Sum Sq`)
    kw <- stats::kruskal.test(f, data = acd)
    lev <- car::leveneTest(f, data = acd)
    rows[[length(rows) + 1]] <- st_row(5, "ANOVA", outcomes[[y]], "Blocks A, C, D",
      test = "One-way ANOVA", statistic = s$`F value`[1],
      df = paste(s$Df[1], s$Df[2], sep = ", "), p_value = s$`Pr(>F)`[1],
      effect_name = "eta squared", effect = eta2, n = nrow(acd),
      note = sprintf("Kruskal-Wallis p = %.2f; Levene p = %.2f",
                     kw$p.value, lev$`Pr(>F)`[1]))
    rows[[length(rows) + 1]] <- st_row(5, "Kruskal-Wallis", outcomes[[y]], "Blocks A, C, D",
      test = "Kruskal-Wallis", statistic = unname(kw$statistic), df = unname(kw$parameter),
      p_value = kw$p.value, n = nrow(acd))
    tk <- stats::TukeyHSD(m)$block
    tukey[[y]] <- data.frame(outcome = outcomes[[y]], pair = rownames(tk),
                             diff = tk[, "diff"], ci_low = tk[, "lwr"],
                             ci_high = tk[, "upr"], p_adj = tk[, "p adj"],
                             row.names = NULL)
    mn <- tapply(acd[[y]], acd$block, mean)
    means[[y]] <- data.frame(outcome = outcomes[[y]], block = names(mn),
                             mean = as.numeric(mn),
                             n = as.numeric(table(acd$block)[names(mn)]))
  }

  # Welch t + Mann-Whitney + Cohen's d, peak (8 PM - 12 AM) vs rest, all 54
  for (y in names(outcomes)) {
    a <- d[[y]][d$peak == 1]; b <- d[[y]][d$peak == 0]
    w <- stats::t.test(a, b, var.equal = FALSE)
    mw <- suppressWarnings(stats::wilcox.test(a, b))
    cd <- (mean(a) - mean(b)) / sqrt((stats::sd(a)^2 + stats::sd(b)^2) / 2)
    rows[[length(rows) + 1]] <- st_row(5, "Welch t", outcomes[[y]],
      sprintf("Peak (n = %d) vs rest (n = %d)", length(a), length(b)),
      estimate = mean(a) - mean(b), ci_low = w$conf.int[1], ci_high = w$conf.int[2],
      test = "Welch two-sample t", statistic = unname(w$statistic),
      df = unname(w$parameter), p_value = w$p.value, effect_name = "Cohen's d",
      effect = cd, n = length(a) + length(b),
      note = sprintf("Means %.2f vs %.2f; Mann-Whitney p = %.2f", mean(a), mean(b),
                     mw$p.value))
  }

  # Two-proportion z + Fisher on abandonment
  ab <- function(g1, g2, label) {
    x <- c(sum(d$abandoned[g1]), sum(d$abandoned[g2])); n <- c(sum(g1), sum(g2))
    r <- st_two_prop(x, n)
    st_row(5, "Two-proportion z", "Abandonment", label, estimate = r$diff,
           ci_low = r$ci[1], ci_high = r$ci[2], test = "Two-proportion z (no correction)",
           statistic = r$z, p_value = r$p, effect_name = "difference in proportions",
           effect = r$diff, n = sum(n),
           note = sprintf("%d/%d = %.0f%% vs %d/%d = %.0f%%; Fisher p = %.2f",
                          x[1], n[1], 100 * r$p1, x[2], n[2], 100 * r$p2, r$fisher_p))
  }
  rows[[length(rows) + 1]] <- ab(d$peak == 1, d$peak == 0, "Peak vs rest")
  rows[[length(rows) + 1]] <- ab(d$block == "C", d$block == "D", "Block C vs D")
  rows[[length(rows) + 1]] <- ab(d$female == 1, d$female == 0, "Women vs men")

  out <- do.call(rbind, rows)

  # Holm over the 9 tests: 3 ANOVA, 3 Welch (peak), 3 abandonment
  holm_idx <- which(out$analysis %in% c("ANOVA", "Welch t", "Two-proportion z"))
  out$p_holm <- NA
  out$p_holm[holm_idx] <- stats::p.adjust(out$p_value[holm_idx], method = "holm")

  list(tests = out, tukey = do.call(rbind, tukey), block_means = do.call(rbind, means))
}

# ---- Section 6: what drives difficulty (point 8) -------------------------------

drivers <- function(d) {
  if (is.null(d$peak)) d$peak <- as.integer(d$pref_time == PEAK_BAND)
  acd <- d[d$block %in% c("A", "C", "D"), ]
  acd$block <- stats::relevel(factor(acd$block), "C")
  f <- extra_attempts ~ block + peak + cycle_min + timetable_impact

  # Linear model with robust (HC3) standard errors. Tests and CIs use the
  # normal approximation (df = Inf, i.e. estimate +/- 1.96 SE), as in the
  # points 7 and 8 write-up; with df = 45 the CIs are slightly wider.
  m <- stats::lm(f, data = acd)
  V <- sandwich::vcovHC(m, type = "HC3")
  rob <- lmtest::coeftest(m, vcov. = V, df = Inf)
  ci <- lmtest::coefci(m, vcov. = V, df = Inf)
  linear <- data.frame(model = "Linear (HC3)", term = rownames(rob),
                       estimate = rob[, 1], std_error = rob[, 2], ci_low = ci[, 1],
                       ci_high = ci[, 2], statistic = rob[, 3], p_value = rob[, 4],
                       row.names = NULL)

  # Poisson check: rate ratios
  pm <- stats::glm(f, family = stats::poisson, data = acd)
  pci <- exp(stats::confint.default(pm))
  pc <- summary(pm)$coefficients
  poisson <- data.frame(model = "Poisson", term = rownames(pc),
                        estimate = exp(pc[, 1]), std_error = pc[, 2],
                        ci_low = pci[, 1], ci_high = pci[, 2], statistic = pc[, 3],
                        p_value = pc[, 4], row.names = NULL)
  dispersion <- sum(stats::residuals(pm, "pearson")^2) / stats::df.residual(pm)

  # Logistic: abandonment on no-machine share and peak, all 54
  lm2 <- stats::glm(abandoned ~ no_machine_share + peak, family = stats::binomial, data = d)
  lc <- summary(lm2)$coefficients
  lci <- exp(stats::confint.default(lm2))
  logistic <- data.frame(model = "Logistic (odds ratios)", term = rownames(lc),
                         estimate = exp(lc[, 1]), std_error = lc[, 2],
                         ci_low = lci[, 1], ci_high = lci[, 2], statistic = lc[, 3],
                         p_value = lc[, 4], row.names = NULL)
  pseudo_r2 <- 1 - lm2$deviance / lm2$null.deviance

  coefs <- rbind(linear, poisson, logistic)
  fit <- data.frame(
    model = c("Linear (HC3)", "Poisson", "Logistic (odds ratios)"),
    n = c(stats::nobs(m), stats::nobs(pm), stats::nobs(lm2)),
    r_squared = c(summary(m)$r.squared, NA, pseudo_r2),
    adj_r_squared = c(summary(m)$adj.r.squared, NA, NA),
    dispersion = c(NA, dispersion, NA),
    note = c("Block B excluded; block reference = C", "exp(coef) = rate ratios",
             "McFadden pseudo R-squared"))
  list(coefficients = coefs, fit = fit)
}

# ---- Section 8: before vs after -------------------------------------------------

# The 12-question feedback form, in order (plan doc, "Feedback form"). A Google
# Forms CSV export has a Timestamp column first, then the questions in order,
# so columns are named by position. Check the order against your form before
# running.
FORM_COLUMNS <- c("q1_resident_id", "q2_did_october_survey", "q3_clash",
                  "q4_pref_day", "q5_pref_time", "q6_long_enough", "q7_two_enough",
                  "q8_will_use", "q9_wasted_trips", "q10_give_up",
                  "q11_satisfaction", "q12_compared")

read_feedback_form <- function(path, has_timestamp = TRUE) {
  f <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (has_timestamp) f <- f[, -1, drop = FALSE]
  k <- min(ncol(f), length(FORM_COLUMNS))
  names(f)[seq_len(k)] <- FORM_COLUMNS[seq_len(k)]
  if (ncol(f) > k) names(f)[(k + 1):ncol(f)] <- paste0("extra_", seq_len(ncol(f) - k))
  f$q1_resident_id <- toupper(trimws(f$q1_resident_id))
  f
}

# Success bar: 16 of 21 (fixed Nov 1). If a different number fill the form,
# the same share (16/21 = 76%) is kept, rounded up.
success_threshold <- function(n) if (n == 21) 16 else ceiling(16 / 21 * n)

# Answers -> numbers / TRUE-FALSE, tolerant of capitalisation and wording.
st_yes <- function(x) tolower(trimws(x)) %in% c("yes", "y", "true", "1")
st_first_number <- function(x) {
  as.numeric(sub("^\\s*([0-9]+).*$", "\\1", ifelse(grepl("[0-9]", x), x, NA)))
}
st_better <- function(x) tolower(trimws(x)) %in% c("better", "much better")

# App-computed outcomes for real participants (IDs not starting with "S").
#   bookings  - read_bookings(con): the published optimiser bookings
#   responses - read_responses(con): pref_days ("Sat;Sun"), pref_band ("evening")
#   config    - load_config(): bands (band, from, to)
# A slot is "at the preferred time" if it starts inside the person's band.
app_outcomes <- function(bookings, responses, config) {
  real <- responses[!grepl("^S", responses$resident_id), ]
  b <- bookings[bookings$source == "optimiser" & bookings$person %in% real$resident_id, ]
  wd <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  b$weekday <- wd[as.POSIXlt(as.Date(b$date))$wday + 1]
  to_min <- function(x) as.numeric(substr(x, 1, 2)) * 60 + as.numeric(substr(x, 4, 5))
  per <- lapply(real$resident_id, function(id) {
    me <- real[real$resident_id == id, ]
    mine <- b[b$person == id, ]
    days <- if (is.na(me$pref_days) || me$pref_days == "") character(0)
            else strsplit(me$pref_days, ";")[[1]]
    band <- config$bands[config$bands$band %in% me$pref_band, ]
    in_band <- if (nrow(band) == 0) NA else
      any(to_min(mine$start) >= to_min(band$from[1]) & to_min(mine$start) < to_min(band$to[1]))
    data.frame(resident_id = id, slots = nrow(mine),
               has_pref_day = length(days) > 0, has_pref_band = nrow(band) > 0,
               day_match = if (length(days) == 0) NA else any(mine$weekday %in% days),
               time_match = in_band)
  })
  do.call(rbind, per)
}

# Section 8. Before = Block C baseline (n = 20). After = feedback form (and
# app outcomes if given). links (optional, private) = data.frame(resp_id,
# resident_id) for people in both, enabling paired tests.
compare_before_after <- function(before_C, form, app = NULL, links = NULL) {
  rows <- list()
  add <- function(r) rows[[length(rows) + 1]] <<- r
  nb <- nrow(before_C)

  prop_row <- function(outcome, source, x_after, n_after, x_before, label_note = NA) {
    r <- st_two_prop(c(x_after, x_before), c(n_after, nb))
    st_row(8, "Before vs after", outcome, source, estimate = r$diff,
           ci_low = r$ci[1], ci_high = r$ci[2], test = "Two-proportion z (no correction)",
           statistic = r$z, p_value = r$p, effect_name = "difference in proportions (after - before)",
           effect = r$diff, n = n_after + nb,
           note = paste0(sprintf("After %d/%d = %.0f%%; before %d/%d = %.0f%%; Fisher p = %.2f",
                                 x_after, n_after, 100 * r$p1, x_before, nb, 100 * r$p2,
                                 r$fisher_p),
                         if (!is.na(label_note)) paste0(". ", label_note) else ""))
  }
  rank_row <- function(outcome, source, after, before, label_note = NA) {
    after <- after[!is.na(after)]
    w <- suppressWarnings(stats::wilcox.test(after, before, conf.int = TRUE))
    st_row(8, "Before vs after", outcome, source,
           estimate = mean(after) - mean(before), ci_low = w$conf.int[1],
           ci_high = w$conf.int[2], test = "Mann-Whitney (Wilcoxon rank-sum)",
           statistic = unname(w$statistic), p_value = w$p.value,
           effect_name = "rank-biserial r (after vs before)",
           effect = st_rank_biserial(after, before), n = length(after) + length(before),
           note = paste0(sprintf("Means after %.2f (n = %d) vs before %.2f (n = %d); CI is for the location shift",
                                 mean(after), length(after), mean(before), length(before)),
                         if (!is.na(label_note)) paste0(". ", label_note) else ""))
  }

  # Preferred time and day: app-computed (primary) and form (secondary)
  if (!is.null(app)) {
    t_ok <- app$time_match[!is.na(app$time_match)]
    add(prop_row("Preferred time (headline)", "App: >= 1 slot in preferred band",
                 sum(t_ok), length(t_ok), sum(before_C$on_pref_time),
                 "Primary outcome; people with no preferred band left out"))
    d_ok <- app$day_match[!is.na(app$day_match)]
    add(prop_row("Preferred day", "App: >= 1 slot on a preferred day",
                 sum(d_ok), length(d_ok), sum(before_C$on_pref_day),
                 "People with no preferred day left out"))
  }
  add(prop_row("Preferred time", "Form Q5", sum(st_yes(form$q5_pref_time)),
               nrow(form), sum(before_C$on_pref_time)))
  add(prop_row("Preferred day", "Form Q4", sum(st_yes(form$q4_pref_day)),
               nrow(form), sum(before_C$on_pref_day)))

  # Satisfaction (Q11, same 1-5 scale as October)
  add(rank_row("Satisfaction (1-5)", "Form Q11", st_first_number(form$q11_satisfaction),
               before_C$satisfaction))

  # Wasted trips (Q9, expected) vs extra attempts; "5 or more" counts as 5
  add(rank_row("Wasted trips per wash day", "Form Q9 (expected)",
               st_first_number(form$q9_wasted_trips), before_C$extra_attempts,
               "After is an expectation, not measured"))

  # Give up (Q10 Yes, expected) vs abandoned
  add(prop_row("Give up a wash day", "Form Q10 'Yes' (expected)",
               sum(st_yes(form$q10_give_up)), nrow(form), sum(before_C$abandoned),
               "After is an expectation; 'Not sure' counts as not Yes"))

  # Two slots not enough (Q7 not Yes) vs slot gap > 0
  add(prop_row("Fewer washes than needed", "Form Q7 'No ...'",
               sum(!st_yes(form$q7_two_enough)), nrow(form), sum(before_C$slot_gap > 0)))

  # Success rule (Q12): better or much better
  k <- sum(st_better(form$q12_compared)); n <- nrow(form)
  sr <- if (exists("success_rule", mode = "function")) {
    tryCatch(success_rule(k, n), error = function(e) NULL)
  } else NULL
  bt <- stats::binom.test(k, n, p = 0.5, alternative = "greater")
  ci <- stats::binom.test(k, n)$conf.int
  kr <- success_threshold(n)
  add(st_row(8, "Success rule", "Rated better / much better (Q12)",
             sprintf("Rule: >= %d of %d", kr, n), estimate = k / n, ci_low = ci[1],
             ci_high = ci[2], test = "One-sided exact binomial vs 0.5",
             statistic = k, p_value = bt$p.value, effect_name = "share", effect = k / n,
             n = n, note = paste0(sprintf("%d of %d; rule %s", k, n,
                                          if (k >= kr) "MET" else "NOT met"),
                                  if (n != 21) sprintf(" (bar scaled from 16 of 21 to %d of %d)", kr, n)
                                  else "")))

  # Paired tests for people in both surveys
  paired <- NULL
  if (!is.null(links) && nrow(links) > 0) {
    m <- merge(links, before_C, by = "resp_id")
    m <- merge(m, form, by.x = "resident_id", by.y = "q1_resident_id")
    if (nrow(m) >= 2) {
      sat_after <- st_first_number(m$q11_satisfaction)
      w <- suppressWarnings(stats::wilcox.test(sat_after, m$satisfaction, paired = TRUE))
      add(st_row(8, "Paired", "Satisfaction (1-5)", sprintf("Same people (n = %d)", nrow(m)),
                 estimate = mean(sat_after - m$satisfaction), test = "Wilcoxon signed-rank",
                 statistic = unname(w$statistic), p_value = w$p.value,
                 effect_name = "mean change", effect = mean(sat_after - m$satisfaction),
                 n = nrow(m)))
      mc <- function(label, before, after) {
        tab <- table(factor(before, c(FALSE, TRUE)), factor(after, c(FALSE, TRUE)))
        t <- suppressWarnings(stats::mcnemar.test(tab))
        st_row(8, "Paired", label, sprintf("Same people (n = %d)", nrow(m)),
               estimate = mean(after) - mean(before), test = "McNemar",
               statistic = unname(t$statistic), p_value = t$p.value,
               effect_name = "change in proportion", effect = mean(after) - mean(before),
               n = nrow(m), note = sprintf("Discordant pairs: %d no->yes, %d yes->no",
                                           tab["FALSE", "TRUE"], tab["TRUE", "FALSE"]))
      }
      add(mc("Preferred time", m$on_pref_time == 1, st_yes(m$q5_pref_time)))
      add(mc("Preferred day", m$on_pref_day == 1, st_yes(m$q4_pref_day)))
      add(mc("Give up", m$abandoned == 1, st_yes(m$q10_give_up)))
      paired <- m[, c("resident_id", "resp_id")]
    }
  }

  # Descriptives the app itself depends on (no before)
  desc <- data.frame(
    question = c("Q3 slots clash with classes", "Q6 slot long enough",
                 "Q8 will use both slots"),
    yes = c(sum(st_yes(form$q3_clash)), sum(st_yes(form$q6_long_enough)),
            sum(tolower(trimws(form$q8_will_use)) == "both")),
    n = nrow(form))
  desc$share <- desc$yes / desc$n
  ci <- t(mapply(function(x, n) stats::binom.test(x, n)$conf.int, desc$yes, desc$n))
  desc$ci_low <- ci[, 1]; desc$ci_high <- ci[, 2]

  list(tests = do.call(rbind, rows), descriptives = desc,
       success_rule = sr, paired_ids = paired)
}
