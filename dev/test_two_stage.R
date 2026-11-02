# Tests for the two-stage optimiser, blocked buffers and soft time preferences.
# Made-up residents only. Run from the repo root: Rscript dev/test_two_stage.R

suppressPackageStartupMessages({library(shiny); library(bslib)})
for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f)

passed <- 0
failed <- 0
ok <- function(what, cond) {
  if (isTRUE(cond)) {
    passed <<- passed + 1
    cat("ok  ", what, "\n")
  } else {
    failed <<- failed + 1
    cat("FAIL", what, "\n")
  }
}

START <- "2026-11-02"
cfg <- load_config()
# No lectures unless a test adds them.
no_lectures <- data.frame(course = "NONE", day = "Sun", start = "00:00", end = "00:30")

every_day <- function(times) paste(WEEKDAYS, times, collapse = ";")

people <- function(ids, block = "C", program = "normal", blocked = every_day("23:30-07:30"),
                   pref_days = NA_character_, pref_band = NA_character_, course = "NONE") {
  data.frame(resident_id = ids, name = paste("Test", ids), block = block,
             program = program, course = course, blocked = blocked,
             pref_days = pref_days, pref_band = pref_band)
}

solve <- function(survey, config = cfg, timetable = no_lectures) {
  fw <- suppressWarnings(build_free_windows(survey, timetable, config, START))
  run_optimiser(survey, fw, config, START, verbose = FALSE)
}

mins <- function(x) vapply(x, parse_hhmm, 1, where = "", USE.NAMES = FALSE)

sorted <- function(s) {
  s <- s[order(s$person, s$date, s$start), ]
  rownames(s) <- NULL
  s
}

cat("== Normal hours\n")
real <- people(sprintf("R_C%02d", 1:6))
res <- solve(real)
ok("everyone real gets 2 slots", all(table(res$schedule$person) == 2) &&
     nrow(res$schedule) == 12)
ok("real residents get normal hours when capacity allows",
   all(mins(res$schedule$start) >= 840 & mins(res$schedule$end) <= 1380))
ok("schedule passes the checker",
   nrow(check_schedule(res$schedule, real, cfg, no_lectures, START)) == 0)

cat("== pref_band is ignored\n")
banded <- real
banded$pref_band <- "morning"
ok("a chosen band changes nothing",
   identical(sorted(solve(banded)$schedule), sorted(res$schedule)))

cat("== Preferred day counts for less than normal hours\n")
# Normal hours are open every day here, so both preferences can be met.
fan <- people("R_C01", pref_days = "Mon;Sat")
fan_res <- solve(fan)
ok("slots are on preferred days and in normal hours",
   all(weekdays(as.Date(fan_res$schedule$date), TRUE) %in% c("Mon", "Sat")) &&
     all(mins(fan_res$schedule$start) >= 840))
# Mondays are free only in the morning, other days also in the afternoon.
trade <- people("R_C01", pref_days = "Mon;Sat",
                blocked = paste(c("Mon 12:00-23:30", "Sat 12:00-23:30"), collapse = ";"))
trade_res <- solve(trade)
ok("normal hours beat a preferred day",
   all(mins(trade_res$schedule$start) >= 840))

cat("== Odd hours are soft\n")
ok("real residents avoid odd hours when they can", all(mins(res$schedule$start) >= 420))
owl_times <- every_day("07:30-23:30")
owl <- people(c("R_C01", "S01"), blocked = owl_times)
owl_res <- solve(owl)
ok("only odd hours free: real and simulated still booked",
   all(table(owl_res$schedule$person) == 2) && nrow(owl_res$underserved) == 0)
ok("those bookings are reported as information, not problems",
   nrow(check_schedule(owl_res$schedule, owl, cfg, no_lectures, START)) == 0 &&
     sum(check_schedule_info(owl_res$schedule, owl, cfg)$kind == "odd_hours") == 4)

cat("== Blocked buffers\n")
blocked_times <- every_day("14:00-16:00")
touch <- people(sprintf("R_C%02d", 1:4), blocked = paste(every_day("23:30-07:30"), blocked_times, sep = ";"))
touch_res <- solve(touch)
starts <- mins(touch_res$schedule$start)
ends <- mins(touch_res$schedule$end)
ok("no slot ends at a blocked window's start", !any(ends == 840 | ends == 1410))
ok("no slot starts at a blocked window's end", !any(starts == 960 | starts == 450))
ok("every slot is at least 30 minutes clear",
   !any(starts < 990 & ends > 810) && all(starts >= 480) && all(ends <= 1380))

w <- widen_blocked(parse_blocked("Sun 23:30-07:30", "R_C01"), cfg)
ok("buffers run across midnight and the week's end",
   any(w$day == "Sun" & w$start == 1380) && any(w$day == "Mon" & w$end == 480))
early <- widen_blocked(parse_blocked("Tue 00:10-02:00", "R_C01"), cfg)
ok("a buffer before an early block reaches the previous day",
   any(early$day == "Mon" & early$start == 1420 & early$end == 1440))
custom <- cfg
custom$rules$blocked_buffer_before_min <- 60
custom$rules$blocked_buffer_after_min <- 0
c_w <- widen_blocked(parse_blocked("Wed 12:00-13:00", "R_C01"), custom)
ok("buffer sizes come from config", c_w$start == 660 && c_w$end == 780)

cat("== Minimum usable window\n")
# Wake 08:00, class 09:00, then blocked until the night: one hour between them.
class_tt <- data.frame(course = "X", day = WEEKDAYS, start = "09:00", end = "10:00")
gap <- people("R_C01", program = "quick", course = "X",
              blocked = paste(every_day("00:00-08:00"), every_day("10:30-24:00"), sep = ";"))
gap_res <- solve(gap, timetable = class_tt)
ok("a one-hour gap between wake-up and class yields no slot",
   nrow(gap_res$schedule) == 0 && nrow(gap_res$underserved) == 1)
wide_tt <- class_tt
wide_tt$start <- "09:30"
wide_tt$end <- "10:00"
wide_res <- solve(gap, timetable = wide_tt)
ok("a gap that fits the slot and both buffers is used",
   nrow(wide_res$schedule) == 2 && all(wide_res$schedule$start == "08:30"))

cat("== Two stages\n")
sims <- people(sprintf("S%02d", 1:8))
both <- rbind(real, sims)
with_sims <- solve(both)
ok("simulated residents are booked too", nrow(with_sims$schedule) == 28)
ok("real residents' result is identical with and without simulated ones",
   identical(sorted(with_sims$schedule[with_sims$schedule$person %in% real$resident_id, ]),
             sorted(res$schedule)))
ok("combined schedule passes the checker",
   nrow(check_schedule(with_sims$schedule, both, cfg, no_lectures, START)) == 0)
ok("per_block has one row per stage",
   identical(with_sims$status$per_block$stage[with_sims$status$per_block$block == "C"],
             c("real", "simulated")))

two_real <- people(sprintf("R_C%02d", 1:5), block = "A")
two_sims <- people(sprintf("S%02d", 11:16), block = "A")
two_alone <- solve(two_real)
two_both <- solve(rbind(two_real, two_sims))
ok("identical on a two-machine block, machines included",
   identical(sorted(two_both$schedule[two_both$schedule$person %in% two_real$resident_id, ]),
             sorted(two_alone$schedule)))
ok("two-machine result passes the checker",
   nrow(check_schedule(two_both$schedule, rbind(two_real, two_sims), cfg, no_lectures, START)) == 0)

# Everyone can only wash 13:30-14:30, so the machine has one slot a day, 10 in
# the cycle, and 7 people want 14: the real resident keeps theirs.
one_hour <- every_day("15:00-13:00")
rival <- people(c("R_C01", sprintf("S%02d", 1:6)), blocked = one_hour)
rival_res <- solve(rival)
ok("real bookings are fixed: the real resident keeps the slot",
   sum(rival_res$schedule$person == "R_C01") == 2)
ok("simulated resident can end up underserved, and it is reported",
   nrow(rival_res$underserved) > 0 &&
     all(is_simulated_id(rival_res$underserved$person)) &&
     grepl(paste(nrow(rival_res$underserved), "simulated"), rival_res$status$message) &&
     rival_res$status$code != "error")
ok("a simulated resident short of slots is not a checker problem",
   !"slot_count" %in% check_schedule(rival_res$schedule, rival, cfg, no_lectures, START)$rule)
ok("it is listed as information",
   "simulated_short" %in% check_schedule_info(rival_res$schedule, rival, cfg)$kind)
short <- res$schedule[-1, ]
ok("a real resident short of slots is still a checker problem",
   "slot_count" %in% check_schedule(short, real, cfg, no_lectures, START)$rule)

spread <- data.frame(id = 1:2, block = "A", date = as.Date(START), day_idx = 1,
                     s = c(0, 3), cells = 3, resident_id = c("R_C01", "R_C02"),
                     machine = c("A-1", "A-2"), machine_no = 1:2)
middle <- data.frame(id = 3, block = "A", date = as.Date(START), day_idx = 1, s = 2,
                     cells = 2, resident_id = "S01")
grid <- generate_grid(normalise_config(cfg), START)
dropped <- assign_around(middle, spread, grid)
ok("a simulated booking that fits no single machine is dropped", !"S01" %in% dropped$resident_id)

cat("== Checker\n")
one <- people("R_C01", blocked = "Mon 14:00-16:00")
row <- function(start, end) {
  data.frame(person = "R_C01", machine = "C-1", date = "2026-11-02", start = start, end = end)
}
rules_for <- function(start, end) check_schedule(row(start, end), one, cfg, no_lectures, START)$rule
ok("ending at a blocked start is a problem", "blocked_margin" %in% rules_for("13:00", "14:00"))
ok("starting at a blocked end is a problem", "blocked_margin" %in% rules_for("16:00", "17:00"))
ok("exactly 30 minutes clear is fine", !"blocked_margin" %in% rules_for("12:30", "13:30"))
ok("during the block is 'blocked', not both", {
  r <- rules_for("14:30", "15:30")
  "blocked" %in% r && !"blocked_margin" %in% r
})
ok("outside normal hours is not a problem", !"blocked_margin" %in% rules_for("09:00", "10:00"))
info <- check_schedule_info(row("09:00", "10:00"), one, cfg)
ok("outside normal hours is information", identical(info$kind, "outside_normal_hours"))
ok("inside normal hours gives no information",
   nrow(check_schedule_info(row("17:00", "18:00"), one, cfg)) == 0)
ok("23:00-24:00 is not normal but not odd",
   identical(check_schedule_info(row("23:00", "24:00"), one, cfg)$kind, "outside_normal_hours"))

cat("\nPASS", passed, "FAIL", failed, "\n")
if (failed > 0) quit(status = 1)
