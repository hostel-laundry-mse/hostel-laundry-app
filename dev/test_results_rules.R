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
using_p1 <- exists("pick_from_queue", mode = "function")
cat("Person 1's helpers loaded:", using_p1, "\n\n")

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
check("no-show counted", res_no_shows(x$bookings, "R002") == 1)
dbDisconnect(con)

# ---- 4. change / release windows ----------------------------------------------
con <- fresh()
id <- bid(con, "R003", "2026-11-10")            # 15:00-16:00, A-2
x <- ctx_at(con, "2026-11-10", "09:00")
check("6 h before: can move or release", res_change_status(bk(x, id), x$now, x$s, 3)$mode == "change")
x <- ctx_at(con, "2026-11-10", "09:01")
check("5 h 59 min before: release only", res_change_status(bk(x, id), x$now, x$s, 3)$mode == "release")
x <- ctx_at(con, "2026-11-10", "15:00")
check("after it starts: nothing", !res_change_status(bk(x, id), x$now, x$s, 3)$ok)
x <- ctx_at(con, "2026-11-04", "08:00")
check("no changes left: nothing", !res_change_status(bk(x, id), x$now, x$s, 0)$ok)
x <- ctx_at(con, "2026-11-10", "10:00")
check("moving under 6 h refused",
      !res_move(x, id, "R003", data.frame(person = "R003", machine = "A-2",
                date = "2026-11-10", start = "19:00", end = "20:00"))$ok)

# ---- 5. alternatives obey every rule -------------------------------------------
dbDisconnect(con); con <- fresh()     # fresh: section 4 moved the clock forward
id <- bid(con, "R003", "2026-11-10")
x <- ctx_at(con, "2026-11-04", "08:00")
alts <- res_alternatives(x, bk(x, id), n = 50)
check("alternatives found", nrow(alts) > 0)
others <- x$active[x$active$booking_id != id, ]
ok_rule <- vapply(seq_len(nrow(alts)), function(i)
  res_why_not("R003", alts$machine[i], alts$date[i], alts$start[i], others, x$survey, x$fw, x$s) == "",
  logical(1))
check("every alternative passes block, free time, machine+buffer, reserve, own-date rules", all(ok_rule))
check("no alternative on R003's other wash date (Nov 5)", !any(alts$date == "2026-11-05"))
check("no alternative overlapping the original time (Nov 10, 15:00-16:00), on any machine",
      !any(alts$date == "2026-11-10" & res_min(alts$start) < 960 & res_min(alts$end) > 900))
check("all alternatives on A machines, in the future, inside the cycle",
      all(grepl("^A-", alts$machine)) && all(res_dt(alts$date, alts$start) > x$now) &&
      all(res_in_cycle(alts$date, x$cycle_start, x$s)))
check("all on the 30-min grid from 07:00 and length = program (60)",
      all((res_min(alts$start) - 420) %% 30 == 0) && all(res_min(alts$end) - res_min(alts$start) == 60))
fb <- res_find_alternatives(bk(x, id), x$active, x$survey, x$fw, x$s, x$cycle_start, x$now, 50)
check("stand-in finder also obeys the rules", all(vapply(seq_len(nrow(fb)), function(i)
  res_why_not("R003", fb$machine[i], fb$date[i], fb$start[i], others, x$survey, x$fw, x$s) == "",
  logical(1))))

# ---- 6. moving --------------------------------------------------------------------
r <- res_move(x, id, "R003", alts[1, ])
check("move succeeds", r$ok)
b <- read_bookings(con, "R003")
check("old row marked moved, new row source = change",
      status_of(con, id) == "moved" && sum(b$source == "change" & b$status == "booked") == 1)
check("move counted as a change (kind = change)",
      nrow(read_changes(con, "R003")) == 1 && read_changes(con, "R003")$kind == "change")
check("optimiser row keeps its planned date (gap rule for next cycle)",
      any(b$source == "optimiser" & b$date == "2026-11-10"))
taken <- data.frame(person = "R003", machine = "A-1", date = "2026-11-03", start = "11:00", end = "12:00")
newid <- max(b$booking_id)
x <- ctx_at(con, "2026-11-04", "08:00")
check("moving onto someone's slot refused", !res_move(x, newid, "R003", taken)$ok)
check("refused move uses no change", res_changes_used(con, "R003", x$now, x$s) == 1)

# 3 changes max, release counts, resets after 30 days
for (k in 1:2) {
  x <- ctx_at(con, "2026-11-04", "08:00")
  cur <- read_bookings(con, "R003"); cur <- cur[cur$status == "booked" & cur$date != "2026-11-05", ]
  if (k == 1) { a <- res_alternatives(x, cur[1, ]); check("second move", res_move(x, cur$booking_id[1], "R003", a[1, ])$ok) }
  else check("release counts too", res_release(x, cur$booking_id[1], "R003")$ok)
}
x <- ctx_at(con, "2026-11-04", "08:00")
check("3 changes used", res_changes_used(con, "R003", x$now, x$s) == 3)
cur <- read_bookings(con, "R003"); cur <- cur[cur$status == "booked", ]
check("4th change refused", !res_release(x, cur$booking_id[1], "R003")$ok)
check("changes come back after 30 days",
      res_changes_left(con, "R003", at("2026-12-04", "08:01"), x$s) == 3)
dbDisconnect(con)

# ---- 7. queue ---------------------------------------------------------------------
con <- fresh()
x <- ctx_at(con, "2026-11-02", "08:00")
check("R008 holding 2 unused slots can't join yet", res_queue_status(x, "R008")$state == "not_yet")
check("joining refused", !res_join_queue(x, "R008")$ok)
for (d in c("2026-11-04", "2026-11-09")) set_booking_status(con, bid(con, "R008", d), "checked_in")
if (res_can_check_out()) check_out(con, bid(con, "R008", "2026-11-09"), at = at("2026-11-09", "13:40"))
x <- ctx_at(con, "2026-11-02", "08:00")
check("R008 after using both can join", res_join_queue(x, "R008")$ok)
check("R008 waiting, number 1", res_queue_status(ctx_at(con, "2026-11-02", "08:01"), "R008")$position == 1)

# R009 (C, normal) releases Sat 08 Nov 15:00 days ahead -> R008 (C, quick) gets it
id9 <- bid(con, "R009", "2026-11-08")
x <- ctx_at(con, "2026-11-02", "09:00")
r <- res_release(x, id9, "R009")
b <- read_bookings(con)
got <- b[b$source == "queue", ]
check("released slot went straight to R008 in the queue", r$ok && nrow(got) == 1 && got$person == "R008" &&
      got$machine == "C-1" && got$date == "2026-11-08" && got$start == "15:00")
check("R008 gets their own program length (quick = 30 min)", got$end == "15:30")
check("R008's queue request is served", read_queue(con)$status[read_queue(con)$person == "R008"] == "served")
check("released row stays 'released' (history kept)", status_of(con, id9) == "released")

# queue order: fewer than 2 first
x <- ctx_at(con, "2026-11-02", "10:00")
check("R009 (now holds 1) can join", res_join_queue(x, "R009")$ok)
add_to_queue(con, "R007", requested_at = at("2026-11-02", "09:30"))   # earlier, holds 2
x <- ctx_at(con, "2026-11-02", "10:01")
check("queue order: R009 (holds 1) ahead of R007 (holds 2, asked earlier)",
      res_queue_status(x, "R009")$position == 1 && res_queue_status(x, "R007")$position == 2)
q <- read_queue(con, "waiting")
# a C-1 slot both R009 and R007 can take (found from their real free time)
both <- NULL
for (d in format(as.Date("2026-11-03") + 0:7)) for (m in seq(x$s$open, x$s$close - 90, 30)) {
  st <- res_hhmm(m)
  if (res_why_not("R009", "C-1", d, st, x$active, x$survey, x$fw, x$s) == "" &&
      res_why_not("R007", "C-1", d, st, x$active, x$survey, x$fw, x$s) == "") {
    both <- data.frame(machine = "C-1", date = d, start = st); break
  }
  if (!is.null(both)) break
}
check("found a slot both R009 and R007 are free for", !is.null(both))
pk <- res_pick_from_queue(both, q, x$active, x$survey, x$fw, x$s, x$cycle_start, x$now)
check("stand-in picker chooses R009 first", identical(pk$person, "R009"))
check("blocked time respected: R009 not eligible Fri 06 Nov 09:00 (blocked Fri 07-12)",
      res_why_not("R009", "C-1", "2026-11-06", "09:00", x$active, x$survey, x$fw, x$s) == "Not in your free time")
check("D resident never gets a C slot",
      res_why_not("R010", "C-1", "2026-11-06", "09:00", x$active, x$survey, x$fw, x$s) == "Other block")
check("leaving the queue works", res_leave_queue(x, "R007")$ok &&
      !"R007" %in% read_queue(con, "waiting")$person)
dbDisconnect(con)

# ---- 8. Free now and claiming -------------------------------------------------------
con <- fresh()
id7 <- bid(con, "R007", "2026-11-07")           # Sat 14:00-15:00 C-1
x <- ctx_at(con, "2026-11-07", "13:30")
check("release 30 min before (allowed: release only)", res_release(x, id7, "R007")$ok)
x <- ctx_at(con, "2026-11-07", "13:31")
f <- res_free_now(x, "R008")
check("it's on Free now, marked starting soon", nrow(f) == 1 && f$booking_id == id7 && f$soon)
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
  check("summary shows 3 changes left", grepl("Changes left: 3 of 3", output$summary$html))
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
  check("Can't make it offers alternatives", is.data.frame(options()) && nrow(options()) > 0)
  session$setInputs(alt = "1", do_move = 1)
  check("Move from the dialog works", status_of(con, i2) == "moved" &&
        grepl("Changes left: 2 of 3", output$summary$html))
  check("queue box shown", grepl("queue|used both", output$queue_box$html))
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
