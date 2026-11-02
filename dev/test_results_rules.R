# Automated checks for the resident pages (R/mod_results.R).
# Run from the repo folder:   Rscript dev/test_results_rules.R
# Uses data/fake_survey.csv, data/fake_schedule.csv and data/config.csv in a
# throw-away in-memory database. Each "ok" line passed; it stops at the first
# failure.
if (!file.exists("dev/load_app_code.R")) setwd("..")   # runApp() starts in dev/
source("dev/load_app_code.R")

config <- load_config("data/config.csv")
sched  <- read.csv("data/fake_schedule.csv", stringsAsFactors = FALSE)
at <- function(d, hm) res_dt(d, hm)
n_ok <- 0
check <- function(desc, cond) {
  if (!isTRUE(cond)) stop("FAILED: ", desc, call. = FALSE)
  n_ok <<- n_ok + 1; cat("ok -", desc, "\n")
}
fresh <- function() {
  con <- dbConnect(RSQLite::SQLite(), ":memory:"); db_init(con)
  add_bookings(con, sched); con          # what Publish on the Admin page does
}
ctx_at <- function(con, d, hm) res_context(con, config, at(d, hm))
bid <- function(con, p, d) {
  b <- read_bookings(con, p); b$booking_id[b$date == d & b$status %in% RES_ACTIVE][1]
}
status_of <- function(con, id) { b <- read_bookings(con); b$status[b$booking_id == id] }

# ---- 0. empty database (before anything is published) --------------------------
con <- dbConnect(RSQLite::SQLite(), ":memory:"); db_init(con)
x <- res_context(con, config, at("2026-11-03", "09:00"))
check("empty database: no crash, no cycle yet", nrow(x$bookings) == 0 && is.na(x$cycle_start))
check("empty database: Free now is empty", nrow(res_free_now(x, "R001")) == 0)
dbDisconnect(con)

# ---- 1. schedule saved once, cycle found -------------------------------------
con <- fresh()
check("published schedule is read from bookings (24 rows)", nrow(res_context(con, config, at("2026-11-03", "09:00"))$bookings) == 24)
x <- ctx_at(con, "2026-11-03", "09:00")
check("current cycle starts Mon 02 Nov", x$cycle_start == as.Date("2026-11-02"))
check("free windows built for everyone", length(unique(x$fw$resident_id)) == 12)
check("buffer: a 30-min quick wash holds the machine 60 min (30+10 -> 2 blocks)",
      with(x$bookings[x$bookings$person == "R001", ][1, ], occ_to - occ_from) == 60)

# ---- 2. check-in --------------------------------------------------------------
id <- bid(con, "R001", "2026-11-03")            # 10:00-10:30
bk <- function(x, id) x$bookings[x$bookings$booking_id == id, ]
x <- ctx_at(con, "2026-11-03", "09:59")
check("check-in closed at 09:59", !res_checkin_status(bk(x, id), x$now, x$s)$ok)
x <- ctx_at(con, "2026-11-03", "10:15")
check("check-in open at 10:15", res_checkin_status(bk(x, id), x$now, x$s)$ok)
check("someone else can't check in for R001", !res_check_in(x, id, "R002")$ok)
check("R001 checks in", res_check_in(x, id, "R001")$ok && status_of(con, id) == "checked_in")
x <- ctx_at(con, "2026-11-03", "10:16")
check("checking in twice refused", !res_check_in(x, id, "R001")$ok)
if (exists("check_in", mode = "function")) {
  check("check-in time recorded (Person 2's check_in)",
        !is.na(read_bookings(con)$checked_in_at[read_bookings(con)$booking_id == id]))
}
if (res_can_check_out()) {
  x <- ctx_at(con, "2026-11-03", "10:35")
  check("someone else can't mark R001's wash finished", !res_check_out(x, id, "R002")$ok)
  check("R001 taps Finished", res_check_out(x, id, "R001")$ok && status_of(con, id) == "finished")
  check("finished wash still counts as R001's booking that day",
        res_why_not("R001", "A-2", "2026-11-03", "15:00", x$active, x$survey, x$fw, x$s) ==
          "You already have a wash that day")
}

# ---- 2b. star rating after a finished wash -------------------------------------
if (res_can_rate() && res_can_check_out()) {
  x <- ctx_at(con, "2026-11-03", "10:40")
  later <- bid(con, "R001", "2026-11-07")
  check("a wash that isn't finished can't be rated", !res_rate(x, later, "R001", 5)$ok)
  check("someone else can't rate R001's wash", !res_rate(x, id, "R002", 5)$ok)
  check("0 stars refused", !res_rate(x, id, "R001", 0)$ok)
  check("6 stars refused", !res_rate(x, id, "R001", 6)$ok)
  check("R001 rates 4 stars with a comment", res_rate(x, id, "R001", 4, "Machine was clean")$ok)
  fb <- read_feedback(con)
  check("feedback saved: booking, person, rating, comment",
        nrow(fb) == 1 && fb$booking_id == id && fb$person == "R001" && fb$rating == 4 &&
        fb$comment == "Machine was clean")
  x <- ctx_at(con, "2026-11-03", "10:41")
  check("rating twice refused", !res_rate(x, id, "R001", 2)$ok && nrow(read_feedback(con)) == 1)
  check("page sees the rating", nrow(res_feedback_for(x, id)) == 1)
  check("stars drawn as 4 filled + 1 empty",
        grepl("(&#9733;){4}</span>.*&#9734;</span>", as.character(res_stars(4))))
}

# ---- 3. no-shows ----------------------------------------------------------------
id2 <- bid(con, "R002", "2026-11-03")           # 11:00-12:00
x <- ctx_at(con, "2026-11-03", "11:15")
check("not a no-show at start+15", status_of(con, id2) == "booked")
x <- ctx_at(con, "2026-11-03", "11:16")
check("no-show at start+16", status_of(con, id2) == "no_show")
check("checked-in (or finished) slot is not a no-show", status_of(con, id) %in% c("checked_in", "finished"))
check("no-show counted", res_no_shows(x$bookings, "R002", x$now, x$s) == 1)
check("a no-show is wasted: not on Free now", !id2 %in% res_free_now(x, "R001")$booking_id)
dbDisconnect(con)

# ---- 4. cancelling ----------------------------------------------------------------
con <- fresh()
id <- bid(con, "R003", "2026-11-10")            # 15:00-16:00, A-2
x <- ctx_at(con, "2026-11-10", "09:00")
check("6 h before: can cancel", res_cancel_status(bk(x, id), x$now, x$s)$ok)
x <- ctx_at(con, "2026-11-10", "09:01")
check("5 h 59 min before: can't cancel", !res_cancel_status(bk(x, id), x$now, x$s)$ok)
check("cancelling under 6 h refused, slot stays booked",
      !res_release(x, id, "R003")$ok && status_of(con, id) == "booked")
check("...and refused cancel isn't counted", res_cancellations(con, "R003", x$now, x$s) == 0)
x <- ctx_at(con, "2026-11-10", "15:16")
check("...so missing it becomes a no-show", status_of(con, id) == "no_show")
check("someone else can't cancel R002's slot",
      !res_release(ctx_at(con, "2026-11-03", "01:00"), bid(con, "R002", "2026-11-08"), "R001")$ok)
dbDisconnect(con)

con <- fresh()
ids <- read_bookings(con, "R005")$booking_id    # two washes, both days away
x <- ctx_at(con, "2026-11-02", "00:30")
check("cancel works", res_release(x, ids[1], "R005")$ok)
check("cancelled slot is 'released', recorded as kind = release",
      status_of(con, ids[1]) == "released" && read_changes(con, "R005")$kind == "release")
check("cancellation counted", res_cancellations(con, "R005", x$now, x$s) == 1)
for (extra in 1:3) add_change(con, "R005", ids[1], "release", at = x$now)
check("4 cancellations so far...", res_cancellations(con, "R005", x$now, x$s) == 4)
check("...and going over the limit does not block another cancel",
      res_release(x, ids[2], "R005")$ok)
check("cancellations drop out after 30 days",
      res_cancellations(con, "R005", x$now + 31 * 86400, x$s) == 0)
dbDisconnect(con)

# ---- 5. penalties ------------------------------------------------------------------
# Cycle starts 1 Dec, so the 30-day window is 1 Nov to 30 Nov.
pen_setup <- function(cancels = 0, no_shows = 0, person = "R001") {
  con <- dbConnect(RSQLite::SQLite(), ":memory:"); db_init(con)
  n <- max(cancels + no_shows, 1)
  slots <- data.frame(person = person, machine = "A-1", date = format(as.Date("2026-11-10") + seq_len(n)),
                      start = "10:00", end = "10:30")
  ids <- add_bookings(con, slots, status = "checked_in")
  for (i in seq_len(cancels)) add_change(con, person, ids[i], "release", at = "2026-11-15 09:00:00")
  for (i in seq_len(no_shows)) set_booking_status(con, ids[cancels + i], "no_show")
  con
}
lost_for <- function(cancels, no_shows) {
  con <- pen_setup(cancels, no_shows); on.exit(dbDisconnect(con))
  p <- slot_penalties(con, "2026-12-01", config, now = at("2026-12-01", "00:00"))
  sum(p$slots_lost[p$resident_id == "R001"])
}
check("3 cancellations are allowed: no slot lost", lost_for(3, 0) == 0)
check("4th cancellation loses 1 slot", lost_for(4, 0) == 1)
check("2 no-shows are allowed: no slot lost", lost_for(0, 2) == 0)
check("3rd no-show loses 1 slot", lost_for(0, 3) == 1)
check("over both limits loses both slots", lost_for(4, 3) == 2)
check("many over one limit still loses only 1", lost_for(9, 0) == 1)

con <- pen_setup(4, 3)
p <- slot_penalties(con, "2027-01-15", config, now = at("2027-01-15", "00:00"))
check("events older than 30 days don't count", nrow(p) == 0)
p <- slot_penalties(con, "2026-11-01", config, now = at("2026-11-01", "00:00"))
check("events after the cycle starts don't count", nrow(p) == 0)
dbDisconnect(con)

con <- dbConnect(RSQLite::SQLite(), ":memory:"); db_init(con)
ids <- add_bookings(con, data.frame(person = "R001", machine = "A-1",
                                    date = c("2026-11-10", "2026-11-14", "2026-11-18"),
                                    start = "10:00", end = "10:30"))
p <- slot_penalties(con, "2026-12-01", config, now = at("2026-12-01", "00:00"))
check("a missed slot nobody opened the app for still counts as a no-show",
      p$no_shows == 3 && p$slots_lost == 1)
add_change(con, "R001", ids[1], "change", at = "2026-11-15 09:00:00")
p <- slot_penalties(con, "2026-12-01", config, now = at("2026-12-01", "00:00"))
check("an old 'change' row isn't a cancellation", p$cancellations == 0)
check("no events: empty result, right columns",
      identical(names(slot_penalties(con, "2026-01-01", config)),
                c("resident_id", "cancellations", "no_shows", "slots_lost")) &&
        nrow(slot_penalties(con, "2026-01-01", config)) == 0)
dbDisconnect(con)

# ---- 8. Free now and claiming -------------------------------------------------------
con <- fresh()
id7 <- bid(con, "R007", "2026-11-07")           # Sat 14:00-15:00 C-1
x <- ctx_at(con, "2026-11-07", "08:00")
check("cancel exactly 6 h before", res_release(x, id7, "R007")$ok)
x <- ctx_at(con, "2026-11-07", "13:31")
f <- res_free_now(x, "R008")
check("it's on Free now", nrow(f) == 1 && f$booking_id == id7)
f10 <- res_free_now(x, "R010")
check("D-block resident sees 'Other block'", f10$why_not == "Other block")
check("D-block resident can't claim", !res_claim(x, id7, "R010")$ok)
check("R008 (C) claims it", res_claim(x, id7, "R008")$ok)
x <- ctx_at(con, "2026-11-07", "13:32")
check("second claim refused, slot gone from Free now",
      !res_claim(x, id7, "R009")$ok && nrow(res_free_now(x, "R009")) == 0)
b <- read_bookings(con)
check("exactly one active booking at that time on C-1",
      sum(b$machine == "C-1" & b$date == "2026-11-07" & b$start == "14:00" & b$status %in% RES_ACTIVE) == 1)
x <- ctx_at(con, "2026-11-07", "15:01")
check("not on Free now once it has started", nrow(res_free_now(x, "R009")) == 0)
dbDisconnect(con)

# ---- 9. the Shiny module, wired the way app.R does (lead-wiring) ----------------
con <- dbConnect(RSQLite::SQLite(), ":memory:"); db_init(con)
sch <- reactiveVal(NULL)                       # Admin returns NULL until Publish
testServer(mod_results_server, args = list(schedule = sch, view = "mine", con = con,
                                           current_user = NULL,
                                           now = reactive(at("2026-11-03", "10:05")),
                                           refresh_ms = 100), {
  session$flushReact()
  check("before Publish: 'No schedule published yet.'",
        grepl("No schedule published yet", output$notice$html) && !published())
  sch(sched); session$flushReact()            # e.g. a demo schedule shown on Admin
  check("an unpublished schedule from Admin is NEVER saved to the database",
        nrow(read_bookings(con)) == 0 && grepl("No schedule published yet", output$notice$html))
  add_bookings(con, sched)                     # Publish saves the bookings
  session$elapse(200); session$flushReact()
  session$setInputs(user = "R001")
  check("after Publish: notice gone, bookings read with read_bookings()",
        published() && is.null(output$notice))
  check("app.R passes no user: page shows its own sign-in dropdown",
        grepl("Signed in as", output$user_box$html))
  check("R001 sees both slots", grepl("Tue 03 Nov, 10:00-10:30", output$cards$html) &&
        grepl("Sat 07 Nov, 09:00-09:30", output$cards$html))
  check("summary shows cancellations and no-shows against their limits",
        grepl("Cancellations in the last 30 days: 0 \\(3 allowed\\)", output$summary$html) &&
          grepl("No-shows in the last 30 days: 0 \\(2 allowed\\)", output$summary$html))
  i1 <- bid(con, "R001", "2026-11-03")
  session$setInputs(action = list(type = "checkin", id = i1, nonce = 1))
  check("tapping Started checks in", grepl("Checked in", output$cards$html))
  if (res_can_check_out() && res_can_rate()) {
    check("Finished button shows after check-in", grepl(">Finished<", output$cards$html))
    session$setInputs(action = list(type = "checkout", id = i1, nonce = 11))
    check("tapping Finished marks the wash finished", status_of(con, i1) == "finished")
    check("...and the star rating pops up straight away", identical(rating_for(), i1))
    session$setInputs(do_rate = 1)
    check("Submit without stars does nothing", nrow(read_feedback(con)) == 0)
    session$setInputs(stars = "5", comment = "Quick and easy", do_rate = 2)
    check("submitting 5 stars saves the rating",
          nrow(read_feedback(con)) == 1 && read_feedback(con)$rating == 5)
    check("card shows 'You rated 5/5' and the comment",
          grepl("You rated 5/5", output$cards$html) && grepl("Quick and easy", output$cards$html))
  }
  i2 <- bid(con, "R001", "2026-11-07")
  session$setInputs(action = list(type = "cant", id = i2, nonce = 2))
  check("Can't make it asks about that slot", identical(pending(), i2))
  session$setInputs(do_release = 1)
  check("Cancelling from the dialog works", status_of(con, i2) == "released" &&
        grepl("last 30 days: 1 \\(3 allowed\\)", output$summary$html))
  sch(NULL); session$flushReact()              # new app session: Admin reactive is NULL again
  check("Admin back to NULL (e.g. app restarted): published slots still show",
        published() && grepl("Sat 07 Nov|Tue 03 Nov", output$cards$html))
})
dbDisconnect(con)

con <- dbConnect(RSQLite::SQLite(), ":memory:"); db_init(con)
testServer(mod_results_server, args = list(view = "all", con = con,
                                           current_user = reactive("R007"),
                                           now = reactive(at("2026-11-07", "13:00")),
                                           refresh_ms = 100), {
  session$flushReact()
  check("Schedule page before Publish: 'No schedule published yet.'",
        grepl("No schedule published yet", output$notice$html))
  add_bookings(con, sched); session$elapse(200); session$flushReact()
  session$setInputs(date = "2026-11-03", which_block = "all")
  check("Schedule page after Publish: notice gone", is.null(output$notice))
  check("no dropdown when app.R passes the user", is.null(output$user_box))
  check("Schedule grid draws", !is.null(output$grid$src))
  session$setInputs(date = "2026-11-11", which_block = "mine")
  check("a day with no bookings in my block still draws the empty grid", !is.null(output$grid$src))
  check("Free now renders", grepl("Nothing free|Claim", output$free_now$html))
  d <- read.csv(output$download)
  check("download has this cycle's bookings", nrow(d) == 24)
})
dbDisconnect(con)
cat("\nAll", n_ok, "checks passed.\n")
