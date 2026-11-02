# Charts for the slides (Person 4). Each chart_*() returns a ggplot; save them
# all with save_charts(). Uses R/stats_compare.R. Only defines functions, so it
# is safe for the app to source.
#
# Before (baseline) charts:
#   chart_baseline(d)        Block C vs all blocks on the headline measures
#   chart_attempts_fit(d)    Extra attempts: observed vs Poisson vs geometric
#   chart_utilisation()      Queue model: utilisation over 24 h vs at the peak
#   chart_block_means(d)     Extra attempts by block (means, 95% CI)
#   chart_abandon_gaps(d)    Abandonment gaps by group, with 95% CIs around 0
#   chart_success_rule(n)    Why 16 of 21 is a fair bar (chance under 50/50)
# After (pre-pilot) charts, from compare_before_after():
#   chart_before_after(ba)   Before vs after shares with 95% CIs
#   chart_satisfaction(before_C, form)  1-5 ratings, before vs after
#   chart_q12(form)          "Compared with now" answers and the 16-of-21 line

# Validated categorical palette (dataviz reference): slot 1 blue, 2 orange,
# 3 aqua; neutral inks for text; status colours are not used for series.
CH_BLUE <- "#2a78d6"; CH_ORANGE <- "#eb6834"; CH_AQUA <- "#1baf7a"
CH_INK <- "#0b0b0b"; CH_INK2 <- "#52514e"; CH_MUTED <- "#8a8984"
CH_GRID <- "#e6e5e1"; CH_SURFACE <- "#fcfcfb"; CH_NEUTRAL <- "#c9c8c2"

ch_theme <- function(base = 13) {
  ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = CH_SURFACE, colour = NA),
      panel.background = ggplot2::element_rect(fill = CH_SURFACE, colour = NA),
      panel.grid.major = ggplot2::element_line(colour = CH_GRID, linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = CH_INK2),
      axis.title = ggplot2::element_text(colour = CH_INK2, size = base - 1),
      plot.title = ggplot2::element_text(colour = CH_INK, face = "bold", size = base + 3),
      plot.subtitle = ggplot2::element_text(colour = CH_INK2, size = base - 1,
                                            margin = ggplot2::margin(b = 8)),
      plot.caption = ggplot2::element_text(colour = CH_MUTED, size = base - 3, hjust = 0),
      legend.position = "top", legend.justification = "left",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(colour = CH_INK2),
      plot.title.position = "plot", plot.caption.position = "plot",
      plot.margin = ggplot2::margin(14, 18, 10, 14))
}

ch_wilson <- function(x, n) {
  ci <- suppressWarnings(stats::prop.test(x, n, correct = FALSE)$conf.int)
  c(ci[1], ci[2])
}
ch_pct <- function(x) paste0(round(100 * x), "%")

# ---- Before ------------------------------------------------------------------

chart_baseline <- function(d) {
  measures <- list(
    "Wash at preferred time" = function(x) x$on_pref_time == 1,
    "Wash on preferred day" = function(x) x$on_pref_day == 1,
    "Give up a wash day" = function(x) x$abandoned == 1,
    "Fewer washes than needed" = function(x) x$slot_gap > 0,
    "Need at least 1 wasted trip" = function(x) x$extra_attempts >= 1,
    "Rate satisfaction 1 or 2 of 5" = function(x) x$satisfaction <= 2)
  groups <- list("Block C (n = 20)" = d[d$block == "C", ], "All blocks (n = 54)" = d)
  rows <- do.call(rbind, lapply(names(groups), function(g) {
    do.call(rbind, lapply(names(measures), function(m) {
      v <- measures[[m]](groups[[g]]); ci <- ch_wilson(sum(v), length(v))
      data.frame(group = g, measure = m, share = mean(v), lo = ci[1], hi = ci[2])
    }))
  }))
  rows$measure <- factor(rows$measure, levels = rev(names(measures)))
  rows$group <- factor(rows$group, levels = names(groups))
  rows$order <- factor(rows$group, levels = rev(names(groups)))   # first group on top
  ggplot2::ggplot(rows, ggplot2::aes(x = share, y = measure, fill = group, group = order)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(0.72), width = 0.66,
                      colour = CH_SURFACE, linewidth = 0.6) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = lo, xmax = hi),
                           position = ggplot2::position_dodge(0.72), width = 0.18,
                           colour = CH_INK2, linewidth = 0.4, orientation = "y") +
    ggplot2::geom_text(ggplot2::aes(x = hi + 0.02, label = ch_pct(share)),
                       position = ggplot2::position_dodge(0.72), hjust = 0,
                       size = 3.6, colour = CH_INK) +
    ggplot2::scale_fill_manual(values = c(CH_BLUE, CH_ORANGE)) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(round(100 * x), "%"),
                                limits = c(0, 1.08), breaks = seq(0, 1, 0.25)) +
    ggplot2::labs(title = "Only 30% of Block C washes at their preferred time",
                  subtitle = "Baseline survey, October 2026. Bars: share of residents; lines: 95% CI (Wilson)",
                  x = NULL, y = NULL,
                  caption = "Block C is the pre-pilot comparison group; all blocks give context.") +
    ch_theme()
}

chart_attempts_fit <- function(d) {
  y <- d$extra_attempts
  obs <- c(sum(y == 0), sum(y == 1), sum(y == 2), sum(y == 3), sum(y >= 4))
  n <- length(y); lam <- mean(y)
  pois <- n * c(stats::dpois(0:3, lam), 1 - stats::ppois(3, lam))
  p_busy <- lam / (1 + lam)                       # geometric with the same mean
  geo <- n * c((1 - p_busy) * p_busy^(0:3), p_busy^4)
  lab <- c("0", "1", "2", "3", "4 or more")
  df <- rbind(data.frame(k = lab, count = obs, series = "Observed"),
              data.frame(k = lab, count = pois, series = "Poisson (mean 1.85)"),
              data.frame(k = lab, count = geo, series = "Geometric (each try fails at 65%)"))
  df$k <- factor(df$k, levels = lab)
  df$series <- factor(df$series, levels = unique(df$series))
  fit_p <- function(e) stats::pchisq(sum((obs - e)^2 / e), df = 3, lower.tail = FALSE)
  ggplot2::ggplot(df[df$series == "Observed", ], ggplot2::aes(k, count)) +
    ggplot2::geom_col(fill = CH_BLUE, width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(y = 0, label = count), vjust = -0.8, size = 4,
                       colour = "white", fontface = "bold") +
    ggplot2::geom_line(data = df[df$series != "Observed", ],
                       ggplot2::aes(group = series, colour = series), linewidth = 0.9) +
    ggplot2::geom_point(data = df[df$series != "Observed", ],
                        ggplot2::aes(colour = series, shape = series), size = 3,
                        stroke = 1.2, fill = CH_SURFACE) +
    ggplot2::scale_colour_manual(values = c(CH_ORANGE, CH_AQUA)) +
    ggplot2::scale_shape_manual(values = c(21, 24)) +
    ggplot2::labs(title = "Wasted trips follow a Poisson pattern, not 'each try fails at 65%'",
                  subtitle = sprintf("Extra attempts per wash day, 54 residents. Poisson fit p = %.2f (not rejected); geometric p < 0.001 (rejected)",
                                     fit_p(pois)),
                  x = "Extra attempts (attempts minus 1)", y = "Residents",
                  caption = "Bars: observed. Lines: expected counts. Chi-square goodness of fit, 5 cells, df = 3.") +
    ch_theme()
}

# Queue model inputs from the spec (section 3): 65 min per wash, 2.43 washes
# per person per 10 days, 1/3 of demand in the 4-hour 8 PM-12 AM band.
chart_utilisation <- function(residents = c(A = 104, B = 21, C = 44, D = 84),
                              machines = c(A = 2, B = 1, C = 1, D = 2),
                              washes = 2.43, peak_share = 0.333, minutes = 65) {
  mu <- 60 / minutes
  per_day <- residents * washes / 10
  rho24 <- per_day / 24 / (machines * mu)
  rhopk <- per_day * peak_share / 4 / (machines * mu)
  df <- rbind(data.frame(block = names(residents), rho = rho24, when = "Over 24 hours"),
              data.frame(block = names(residents), rho = rhopk, when = "At the 8 PM-12 AM peak"))
  df$when <- factor(df$when, levels = c("Over 24 hours", "At the 8 PM-12 AM peak"))
  df$block <- paste0("Block ", df$block, "\n", residents[sub("Block |\n.*", "", df$block)],
                     " residents, ", machines[sub("Block |\n.*", "", df$block)],
                     ifelse(machines[sub("Block |\n.*", "", df$block)] == 1, " machine", " machines"))
  ggplot2::ggplot(df, ggplot2::aes(block, rho, fill = when)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(0.72), width = 0.66,
                      colour = CH_SURFACE, linewidth = 0.6) +
    ggplot2::geom_hline(yintercept = 1, colour = CH_INK2, linetype = "dashed", linewidth = 0.5) +
    ggplot2::annotate("text", x = 4.45, y = 1.03, label = "Fully busy (utilisation = 1)",
                      hjust = 1, vjust = 0, size = 3.4, colour = CH_INK2) +
    ggplot2::geom_label(ggplot2::aes(label = sprintf("%.2f", rho)),
                        position = ggplot2::position_dodge(0.72), vjust = -0.25, size = 3.6,
                        colour = CH_INK, fill = CH_SURFACE, label.size = 0,
                        label.padding = ggplot2::unit(0.12, "lines")) +
    ggplot2::scale_fill_manual(values = c(CH_BLUE, CH_ORANGE)) +
    ggplot2::scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, 0.25)) +
    ggplot2::labs(title = "Capacity is enough over the day; the evening peak is not",
                  subtitle = "Queue model: utilisation = arrivals / (machines x washes per machine per hour)",
                  x = NULL, y = "Utilisation",
                  caption = "Model estimates from survey answers (65 min per wash, 2.43 washes per person per 10 days, 1/3 of demand at 8 PM-12 AM); no arrival log exists.") +
    ch_theme()
}

chart_block_means <- function(d) {
  acd <- d[d$block %in% c("A", "C", "D"), ]
  rows <- do.call(rbind, lapply(c("A", "C", "D"), function(b) {
    y <- acd$extra_attempts[acd$block == b]; t <- stats::t.test(y)
    data.frame(block = sprintf("Block %s\n(n = %d)", b, length(y)), mean = mean(y),
               lo = t$conf.int[1], hi = t$conf.int[2])
  }))
  g <- compare_groups(d)$tests
  p <- g$p_value[g$analysis == "ANOVA" & g$outcome == "Extra attempts"]
  e <- g$effect[g$analysis == "ANOVA" & g$outcome == "Extra attempts"]
  ggplot2::ggplot(rows, ggplot2::aes(block, mean)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), width = 0.12,
                           colour = CH_INK2, linewidth = 0.5) +
    ggplot2::geom_point(size = 4.5, colour = CH_BLUE) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", mean)), nudge_x = 0.2,
                       size = 3.8, colour = CH_INK) +
    ggplot2::scale_y_continuous(limits = c(0, 3)) +
    ggplot2::labs(title = "Every block wastes the same number of trips",
                  subtitle = sprintf("Extra attempts per wash day, mean and 95%% CI. ANOVA p = %.2f, eta squared = %.3f; Tukey: no pair differs",
                                     p, e),
                  x = NULL, y = "Extra attempts",
                  caption = "Block B (3 respondents) left out of block comparisons.") +
    ch_theme()
}

chart_abandon_gaps <- function(d) {
  g <- compare_groups(d)$tests
  z <- g[g$analysis == "Two-proportion z", ]
  z$label <- c("Peak (8 PM-12 AM) vs rest", "Block C vs Block D", "Women vs men")
  z$label <- factor(z$label, levels = rev(z$label))
  z$txt <- sprintf("%+.0f points, p = %.2f (Holm 1.00)", 100 * z$estimate, z$p_value)
  ggplot2::ggplot(z, ggplot2::aes(estimate, label)) +
    ggplot2::geom_vline(xintercept = 0, colour = CH_INK2, linewidth = 0.5) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = ci_low, xmax = ci_high), width = 0.15,
                           colour = CH_INK2, linewidth = 0.5, orientation = "y") +
    ggplot2::geom_point(size = 4.5, colour = CH_BLUE) +
    ggplot2::geom_text(ggplot2::aes(x = ci_high + 0.03, label = txt), hjust = 0, size = 3.6,
                       colour = CH_INK) +
    ggplot2::scale_x_continuous(labels = function(x) sprintf("%+d", round(100 * x)),
                                limits = c(-0.45, 1.0), breaks = seq(-0.25, 0.5, 0.25)) +
    ggplot2::labs(title = "No group gives up detectably more often",
                  subtitle = "Difference in share who give up a wash day, with 95% CI. Every CI crosses 0",
                  x = "Difference (percentage points)", y = NULL,
                  caption = "Two-proportion z-tests (Fisher agrees). Small groups: only large gaps are detectable (about 18% power for the peak gap).") +
    ch_theme()
}

chart_success_rule <- function(n = 21, k_rule = 16) {
  df <- data.frame(k = 0:n, p = stats::dbinom(0:n, n, 0.5))
  df$zone <- ifelse(df$k >= k_rule, "Rule met", "Rule not met")
  tail <- 1 - stats::pbinom(k_rule - 1, n, 0.5)
  ggplot2::ggplot(df, ggplot2::aes(k, p, fill = zone)) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::geom_vline(xintercept = k_rule - 0.5, colour = CH_INK2, linetype = "dashed") +
    ggplot2::annotate("text", x = k_rule - 0.3, y = max(df$p) * 0.95, hjust = 0, size = 3.6,
                      colour = CH_INK,
                      label = sprintf("%d or more of %d:\n%.1f%% by chance\nif residents were split 50/50",
                                      k_rule, n, 100 * tail)) +
    ggplot2::scale_fill_manual(values = c("Rule met" = CH_BLUE, "Rule not met" = CH_NEUTRAL)) +
    ggplot2::scale_x_continuous(breaks = seq(0, n, 3)) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::labs(title = sprintf("Success rule, fixed before the pre-pilot: %d of %d rate it better", k_rule, n),
                  subtitle = "Chance of each count if the true share rating 'better' were 50% (one-sided binomial test)",
                  x = "Participants rating the schedule better or much better", y = "Probability",
                  caption = sprintf("Power of the rule: %s",
                                    paste(sprintf("%d%% true share -> %.2f", c(75, 80, 85, 90),
                                                  1 - stats::pbinom(k_rule - 1, n, c(.75, .8, .85, .9))),
                                          collapse = "; "))) +
    ch_theme() + ggplot2::theme(legend.position = "none")
}

# ---- After -------------------------------------------------------------------

chart_before_after <- function(ba) {
  t <- ba$tests[ba$tests$analysis == "Before vs after" &
                  grepl("^(App|Form Q10|Form Q7)", ba$tests$comparison), ]
  parse <- function(note, which) {
    m <- regmatches(note, regexpr(if (which == "after") "After [0-9]+/[0-9]+" else "before [0-9]+/[0-9]+", note))
    as.numeric(strsplit(sub("^[A-Za-z]+ ", "", m), "/")[[1]])
  }
  rows <- do.call(rbind, lapply(seq_len(nrow(t)), function(i) {
    a <- parse(t$note[i], "after"); b <- parse(t$note[i], "before")
    lab <- sub(" \\(headline\\)", "", t$outcome[i])
    if (grepl("expected", t$comparison[i])) lab <- paste0(lab, " (expected)")
    ca <- ch_wilson(a[1], a[2]); cb <- ch_wilson(b[1], b[2])
    rbind(data.frame(outcome = lab, when = "Before (Block C survey)", share = b[1] / b[2], lo = cb[1],
                     hi = cb[2], p = t$p_value[i], txt = sprintf("%s (%d/%d)", ch_pct(b[1] / b[2]), b[1], b[2])),
          data.frame(outcome = lab, when = "After (pre-pilot)", share = a[1] / a[2],
                     lo = ca[1], hi = ca[2], p = t$p_value[i],
                     txt = sprintf("%s (%d/%d)", ch_pct(a[1] / a[2]), a[1], a[2])))
  }))
  rows$outcome <- factor(rows$outcome, levels = rev(unique(rows$outcome)))
  rows$when <- factor(rows$when, levels = unique(rows$when))
  rows$order <- factor(rows$when, levels = rev(levels(rows$when)))  # Before on top
  ptxt <- unique(rows[, c("outcome", "p")])
  ggplot2::ggplot(rows, ggplot2::aes(share, outcome, fill = when, group = order)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(0.72), width = 0.66,
                      colour = CH_SURFACE, linewidth = 0.6) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = lo, xmax = hi), position = ggplot2::position_dodge(0.72),
                           width = 0.18, colour = CH_INK2, linewidth = 0.4, orientation = "y") +
    ggplot2::geom_text(ggplot2::aes(x = hi + 0.02, label = txt),
                       position = ggplot2::position_dodge(0.72), hjust = 0, size = 3.4, colour = CH_INK) +
    ggplot2::geom_text(data = ptxt, ggplot2::aes(x = 1.32, y = outcome, label = sprintf("p = %.3f", p)),
                       inherit.aes = FALSE, hjust = 1, size = 3.4, colour = CH_INK2) +
    ggplot2::scale_fill_manual(values = c(CH_BLUE, CH_ORANGE)) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(round(100 * x), "%"),
                                limits = c(0, 1.32), breaks = seq(0, 1, 0.25)) +
    ggplot2::labs(title = "Before vs after the pre-pilot",
                  subtitle = "Share of residents, with 95% CI (Wilson). p: two-proportion z-test",
                  x = NULL, y = NULL,
                  caption = "Preferred time and day: computed by the app from each participant's slots. 'Expected' = answers about the future, not measured.") +
    ch_theme()
}

chart_satisfaction <- function(before_C, form) {
  after <- st_first_number(form$q11_satisfaction)
  lv <- as.character(1:5)
  mk <- function(x, lab) {
    tb <- table(factor(x, levels = 1:5))
    data.frame(score = lv, share = as.numeric(tb) / sum(tb), when = lab)
  }
  df <- rbind(mk(before_C$satisfaction, sprintf("Before (Block C, n = %d)", nrow(before_C))),
              mk(after, sprintf("After (pre-pilot, n = %d)", sum(!is.na(after)))))
  df$when <- factor(df$when, levels = unique(df$when))
  ggplot2::ggplot(df, ggplot2::aes(score, share, fill = when)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(0.72), width = 0.66,
                      colour = CH_SURFACE, linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(share > 0, ch_pct(share), "")),
                       position = ggplot2::position_dodge(0.72),
                       vjust = -0.5, size = 3.4, colour = CH_INK) +
    ggplot2::scale_fill_manual(values = c(CH_BLUE, CH_ORANGE)) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"),
                                expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::scale_x_discrete(labels = c("1\nVery\ndissatisfied", "2", "3", "4", "5\nVery\nsatisfied")) +
    ggplot2::labs(title = "Satisfaction, same 1-5 question as October",
                  subtitle = sprintf("Mean %.2f before vs %.2f after", mean(before_C$satisfaction),
                                     mean(after, na.rm = TRUE)),
                  x = NULL, y = "Share of residents",
                  caption = "Mann-Whitney test (Wilcoxon signed-rank for people who answered both).") +
    ch_theme()
}

chart_q12 <- function(form, k_rule = NULL) {
  lv <- c("Much worse", "Worse", "About the same", "Better", "Much better")
  x <- trimws(form$q12_compared)
  tb <- table(factor(x, levels = lv))
  df <- data.frame(answer = factor(lv, levels = lv), count = as.numeric(tb),
                   good = lv %in% c("Better", "Much better"))
  k <- sum(df$count[df$good]); n <- sum(df$count)
  if (is.null(k_rule)) k_rule <- success_threshold(n)
  ggplot2::ggplot(df, ggplot2::aes(answer, count, fill = good)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = count), vjust = -0.5, size = 3.8, colour = CH_INK) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = CH_BLUE, `FALSE` = CH_NEUTRAL)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(title = sprintf("%d of %d rate the schedule better or much better: rule %s",
                                  k, n, if (k >= k_rule) "met" else "not met"),
                  subtitle = sprintf("Q12 'Compared with how you do laundry now, this schedule is...'. Success needs %d of %d%s",
                                     k_rule, n, if (n == 21) " (fixed Nov 1)" else " (16 of 21 scaled)"),
                  x = NULL, y = "Participants",
                  caption = sprintf("One-sided exact binomial test vs 50%%: p = %.4f; exact 95%% CI %.0f%%-%.0f%%",
                                    stats::binom.test(k, n, 0.5, "greater")$p.value,
                                    100 * stats::binom.test(k, n)$conf.int[1],
                                    100 * stats::binom.test(k, n)$conf.int[2])) +
    ch_theme() + ggplot2::theme(legend.position = "none")
}

# Saves every chart as a PNG (16:9 slide size) into `dir`.
save_charts <- function(d, dir = "output/charts", ba = NULL, form = NULL) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  sv <- function(p, name, h = 5.4) {
    ggplot2::ggsave(file.path(dir, paste0(name, ".png")), p, width = 10, height = h, dpi = 200,
                    bg = CH_SURFACE)
  }
  sv(chart_baseline(d), "b1_baseline")
  sv(chart_attempts_fit(d), "b2_attempts_poisson")
  sv(chart_utilisation(), "b3_utilisation")
  sv(chart_block_means(d), "b4_block_means")
  sv(chart_abandon_gaps(d), "b4_abandon_gaps")
  sv(chart_success_rule(), "b5_success_rule")
  if (!is.null(ba) && !is.null(form)) {
    sv(chart_before_after(ba), "a1_before_after")
    sv(chart_satisfaction(d[d$block == "C", ], form), "a2_satisfaction")
    sv(chart_q12(form), "a3_success_rule")
  }
  invisible(list.files(dir, full.names = TRUE))
}
