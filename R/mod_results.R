# Resident pages (Person 4): "My slots" and "Schedule".
#
# mod_results_ui(id, view = c("mine", "all"))
#   Takes: module id, which page to show.
#   Returns: UI for the My slots page ("mine") or the Schedule page ("all").
#
# mod_results_server(id, schedule = NULL, view = c("mine", "all"), con = NULL,
#                    current_user = NULL, now = NULL, cycle_start = NULL,
#                    refresh_ms = 3000)
#   Takes: id           - module id
#          schedule     - reactive from mod_admin_server(): the published
#                         schedule, or NULL until Publish. Used only as a
#                         signal to refresh. Bookings are always read from the
#                         database with read_bookings(con), which is where
#                         Publish saves them, so slots still show after the
#                         app restarts (when the Admin reactive is NULL again).
#          view         - "mine" or "all"
#          con          - DBI connection from db_connect(). NULL opens one.
#          current_user - reactive resident_id of the logged-in resident. NULL
#                         shows a "Signed in as" dropdown on the page instead.
#          now          - reactive clock (POSIXct). NULL means Sys.time().
#          cycle_start  - reactive first date of the current cycle. NULL works
#                         it out from the optimiser bookings in the database.
#          refresh_ms   - how often to look for changes made by other people.
#   Returns: nothing (output only).
#
# My slots ("mine"): the resident's bookings this cycle; "Started" check-in
#   (from slot start to checkin_grace_min after); "Can't make it" (move to
#   another slot up to change_cutoff_hours before, or release it any time
#   before it starts; both count towards max_changes per changes_window_days);
#   changes left and no-shows; join or leave the extra-slot queue.
# Schedule ("all"): every machine by date, coloured by status, and the
#   "Free now" list of released slots anyone eligible can claim.
#
# All rules follow CLAUDE.md "Scheduling rules" and "Changes, check-in and
# the queue". Rule values come from data/config.csv via load_config().
#
# Uses from other files (all optional except the R/data_load.R helpers):
#   R/data_load.R  load_config, load_survey, load_timetable,
#                  build_free_windows, db_connect, read_responses,
#                  read_bookings, add_bookings, set_booking_status,
#                  read_changes, add_change, read_queue, add_to_queue,
#                  set_queue_status
#   R/optimiser.R  find_alternative_slots, pick_from_queue. Until they exist,
#                  res_find_alternatives() and res_pick_from_queue() below
#                  are used: they apply the same rules.
#   R/mod_questionnaire.R  PATH_SURVEY, PATH_TIMETABLE, PATH_CONFIG (the
#                  files the Admin page uses), if defined.
#
# Until the database has any bookings, both pages say "No schedule published
# yet." Nothing here ever writes the Admin page's schedule to the database:
# only Publish (R/mod_admin.R) does that.
#
# Every function and constant here starts with res_ / RES_ so nothing clashes
# with the other files, which are all sourced into the same environment.
#
# Choices made here that the team may want to confirm:
#   - Joining the extra-slot queue: allowed once you have used (checked into)
#     both slots this cycle, or if you hold fewer than 2 (e.g. released one).
#   - A moved or released slot is freed machine time: it is offered to the
#     queue, otherwise it shows under "Free now".
#   - After 3 no-shows nothing happens yet (open decision 1); the count is
#     shown.

# ---- Settings ----------------------------------------------------------------

RES_DEFAULTS <- list(checkin_grace_min = 15, change_cutoff_hours = 6,
                     max_changes = 3, changes_window_days = 30,
                     queue_min_lead_min = 60, reserve_share = 0,
                     buffer_min = 10, grid_min = 30, cycle_days = 10,
                     slots_per_cycle = 2)
RES_ACTIVE <- c("booked", "checked_in")       # statuses that hold the machine
RES_FREED  <- c("released", "moved")          # statuses that free it again

res_path <- function(name, fallback) {
  if (exists(name)) get(name) else fallback
}

# Settings this page needs, read from load_config()'s list.
res_settings <- function(config) {
  r <- config$rules
  get_num <- function(x, default) {
    if (is.null(x) || length(x) == 0 || is.na(x[[1]]) || x[[1]] == "") default
    else as.numeric(x[[1]])
  }
  s <- lapply(names(RES_DEFAULTS), function(k) get_num(r[[k]], RES_DEFAULTS[[k]]))
  names(s) <- names(RES_DEFAULTS)
  s$buffer_min <- get_num(config$hours$buffer_min, RES_DEFAULTS$buffer_min)
  s$open  <- res_min(config$hours$open)
  s$close <- res_min(config$hours$close)
  s$days  <- config$hours$days
  s$programs <- setNames(as.numeric(config$programs$duration_min),
                         config$programs$program)
  s$machines <- res_machine_list(config)
  s
}

# All working machines: data.frame(machine, block).
res_machine_list <- function(config) {
  m <- config$machines
  out <- do.call(rbind, lapply(seq_len(nrow(m)), function(i) {
    data.frame(machine = paste0(m$block[i], "-", seq_len(m$n_machines[i])),
               block = m$block[i])
  }))
  out[!out$machine %in% config$out_of_order, ]
}

# ---- Time helpers ------------------------------------------------------------

res_min <- function(x) {                       # "20:30" -> 1230; "24:00" -> 1440
  x <- as.character(x)
  as.numeric(substr(x, 1, 2)) * 60 + as.numeric(substr(x, 4, 5))
}
res_hhmm <- function(m) sprintf("%02d:%02d", as.integer(m %/% 60), as.integer(m %% 60))

# Clock time of a date + "HH:MM", in the computer's time zone (as optimiser.R).
res_dt <- function(date, hhmm) {
  if (length(date) == 0) return(as.POSIXct(character(0)))
  as.POSIXct(paste(format(as.Date(date)), "00:00")) + res_min(hhmm) * 60
}
res_day <- function(date) format(as.Date(date), "%a %d %b")
res_today <- function(now) as.Date(format(now, "%Y-%m-%d"))
res_label <- function(date, start, end, machine) {
  sprintf("%s, %s-%s, machine %s", res_day(date), start, end, machine)
}

# ---- Reading state -----------------------------------------------------------

res_block_of <- function(machine) sub("-.*$", "", machine)

# The survey this page uses: questionnaire answers in the database, plus the
# survey file the Admin page plans from for anyone who hasn't answered.
res_survey <- function(con) {
  file <- tryCatch(load_survey(res_path("PATH_SURVEY", "data/fake_survey.csv")),
                   error = function(e) NULL)
  db <- tryCatch(read_responses(con), error = function(e) NULL)
  if (is.null(db) || nrow(db) == 0) return(file)
  if (is.null(file)) return(db)
  cols <- intersect(names(file), names(db))
  rbind(db[, cols], file[!file$resident_id %in% db$resident_id, cols])
}

# Bookings with clock times and the machine time they hold (buffer included).
res_bookings <- function(con, s) {
  b <- read_bookings(con)
  b$block <- res_block_of(b$machine)
  b$start_dt <- res_dt(b$date, b$start)
  b$end_dt <- res_dt(b$date, b$end)
  dur <- res_min(b$end) - res_min(b$start)
  b$occ_from <- res_min(b$start)
  b$occ_to <- b$occ_from + ceiling((dur + s$buffer_min) / s$grid_min) * s$grid_min
  b
}

# Start of the current cycle: optimiser bookings are grouped into runs of
# cycle_days from their first date; the latest run that has started by today.
res_cycle_start <- function(bookings, now, s) {
  d <- sort(unique(as.Date(bookings$date[bookings$source == "optimiser"])))
  if (length(d) == 0) return(NA)
  starts <- d[1]
  for (x in as.list(d)) {
    if (x >= max(starts) + s$cycle_days) starts <- c(starts, x)
  }
  started <- starts[starts <= res_today(now)]
  if (length(started) > 0) max(started) else min(starts)
}

res_in_cycle <- function(dates, cycle_start, s) {
  d <- as.Date(dates)
  d >= cycle_start & d < cycle_start + s$cycle_days
}

# ---- Housekeeping --------------------------------------------------------------

# Booked slots nobody checked into within the grace time become no-shows.
res_expire_no_shows <- function(con, now, s) {
  b <- res_bookings(con, s)
  late <- b$booking_id[b$status == "booked" &
                         now > b$start_dt + s$checkin_grace_min * 60]
  for (id in late) set_booking_status(con, id, "no_show")
  length(late)
}

# ---- Counts ----------------------------------------------------------------------

res_changes_used <- function(con, person, now, s) {
  ch <- read_changes(con, person)
  if (nrow(ch) == 0) return(0)
  at <- as.POSIXct(ch$at)
  sum(at > now - s$changes_window_days * 86400)
}
res_changes_left <- function(con, person, now, s) {
  max(0, s$max_changes - res_changes_used(con, person, now, s))
}
res_no_shows <- function(b, person) sum(b$person == person & b$status == "no_show")

# ---- Rule checks: each returns list(ok, why) -------------------------------------

res_checkin_status <- function(bk, now, s) {
  closes <- bk$start_dt + s$checkin_grace_min * 60
  if (bk$status != "booked") return(list(ok = FALSE, why = ""))
  if (now < bk$start_dt) return(list(ok = FALSE, why = paste("Check-in opens at", bk$start)))
  if (now > closes) return(list(ok = FALSE, why = "Check-in has closed"))
  list(ok = TRUE, why = paste("Tap Started before", format(closes, "%H:%M")))
}

# What "Can't make it" allows: "change" (move or release), "release" (only
# release: under change_cutoff_hours before), or "none".
res_change_status <- function(bk, now, s, changes_left) {
  if (bk$status != "booked") return(list(ok = FALSE, mode = "none", why = ""))
  if (now >= bk$start_dt) return(list(ok = FALSE, mode = "none", why = "Slot has started"))
  if (changes_left <= 0) {
    return(list(ok = FALSE, mode = "none",
                why = sprintf("No changes left (%g per %g days)", s$max_changes,
                              s$changes_window_days)))
  }
  if (now > bk$start_dt - s$change_cutoff_hours * 3600) {
    return(list(ok = TRUE, mode = "release",
                why = sprintf("Under %g h to go: you can release it but not move it",
                              s$change_cutoff_hours)))
  }
  list(ok = TRUE, mode = "change", why = "")
}

# Is machine time [from, to) free on that machine and date? (minutes)
res_machine_free <- function(active, machine, date, from, to) {
  o <- active[active$machine == machine & active$date == format(as.Date(date)), ]
  all(o$occ_to <= from | o$occ_from >= to)
}

# Does adding `minutes` keep the block's emergency reserve free that date?
res_reserve_ok <- function(active, block, date, minutes, s) {
  n <- sum(s$machines$block == block)
  cap <- floor((1 - s$reserve_share) * n * (s$close - s$open))
  used <- sum((active$occ_to - active$occ_from)[active$block == block &
                                                  active$date == format(as.Date(date))])
  used + minutes <= cap
}

# Is the whole wash [from, to) inside one of the person's free windows?
res_in_free_time <- function(fw, person, date, from, to) {
  w <- fw[fw$resident_id == person & fw$date == format(as.Date(date)), ]
  nrow(w) > 0 && any(res_min(w$from) <= from & res_min(w$to) >= to)
}

# Can `person` take a wash starting at `start` on this machine and date?
# Returns "" if yes, otherwise the reason.
res_why_not <- function(person, machine, date, start, active, survey, fw, s) {
  me <- survey[survey$resident_id == person, ]
  if (nrow(me) == 0) return("Not in the survey")
  if (res_block_of(machine) != me$block[1]) return("Other block")
  dur <- s$programs[me$program[1]]
  if (is.na(dur)) return("Unknown wash program")
  from <- res_min(start)
  occ_to <- from + ceiling((dur + s$buffer_min) / s$grid_min) * s$grid_min
  if (occ_to > s$close) return("Runs past closing time")
  if (any(active$person == person & active$date == format(as.Date(date)))) {
    return("You already have a wash that day")
  }
  if (!res_in_free_time(fw, person, date, from, from + dur)) return("Not in your free time")
  if (!res_machine_free(active, machine, date, from, occ_to)) return("Too short for your program")
  if (!res_reserve_ok(active, res_block_of(machine), date, occ_to - from, s)) {
    return("Would use the emergency reserve")
  }
  ""
}

# ---- Alternatives and queue (stand-ins until R/optimiser.R has them) -------------

# Same rules as optimiser.R's find_alternative_slots(): own block, working
# machine, opening hours, whole wash in free time, machine free with buffer,
# emergency reserve, a date without their other bookings, starts after now.
# The 4-5 day gap does not apply to a change.
res_find_alternatives <- function(booking, active, survey, fw, s, cycle_start, now, n = 5) {
  who <- booking$person
  others <- active[!(active$booking_id %in% booking$booking_id), ]
  dates <- cycle_start + seq_len(s$cycle_days) - 1
  dates <- dates[format(dates, "%a") %in% s$days]
  starts <- seq(s$open, s$close - s$grid_min, by = s$grid_min)
  out <- list()
  for (d in as.list(dates)) for (st in starts) for (m in s$machines$machine) {
    if (res_dt(d, res_hhmm(st)) <= now) next
    if (m == booking$machine && format(d) == booking$date && res_hhmm(st) == booking$start) next
    if (res_why_not(who, m, d, res_hhmm(st), others, survey, fw, s) != "") next
    dur <- s$programs[survey$program[survey$resident_id == who][1]]
    out[[length(out) + 1]] <- data.frame(person = who, machine = m, date = format(d),
                                         start = res_hhmm(st), end = res_hhmm(st + dur))
  }
  if (length(out) == 0) {
    return(data.frame(person = character(), machine = character(), date = character(),
                      start = character(), end = character()))
  }
  out <- do.call(rbind, out)
  dist <- abs(as.numeric(as.Date(out$date) - as.Date(booking$date))) * 10000 +
    abs(res_min(out$start) - res_min(booking$start))
  out <- out[order(dist), ]
  out <- out[!duplicated(paste(out$date, out$start)), ]   # one machine per time
  rownames(out) <- NULL
  head(out, n)
}

# Same rules and order as optimiser.R's pick_from_queue().
res_pick_from_queue <- function(released, queue, active, survey, fw, s, cycle_start, now) {
  lead <- as.numeric(difftime(res_dt(released$date, released$start), now, units = "mins"))
  if (lead < s$queue_min_lead_min) {
    return(list(person = NA_character_, booking = NULL, reason = "starts too soon"))
  }
  cyc <- active[res_in_cycle(active$date, cycle_start, s), ]
  have <- vapply(queue$person, function(p) sum(cyc$person == p), numeric(1))
  queue <- queue[order(have >= s$slots_per_cycle, queue$requested_at, queue$person), ]
  for (p in queue$person) {
    if (res_why_not(p, released$machine, released$date, released$start,
                    active, survey, fw, s) != "") next
    dur <- s$programs[survey$program[survey$resident_id == p][1]]
    booking <- data.frame(person = p, machine = released$machine, date = released$date,
                          start = released$start,
                          end = res_hhmm(res_min(released$start) + dur))
    return(list(person = p, booking = booking, reason = "first eligible person in the queue"))
  }
  list(person = NA_character_, booking = NULL, reason = "nobody in the queue fits")
}

# Use Person 1's helpers when R/optimiser.R has them. Someone who can't make
# a time doesn't want the same time on another machine, so alternatives that
# overlap the original slot on the same date are left out.
res_alternatives <- function(ctx, booking, n = 5) {
  alt <- NULL
  if (exists("find_alternative_slots", mode = "function")) {
    alt <- tryCatch(
      find_alternative_slots(booking[, c("person", "machine", "date", "start", "end")],
                             ctx$active[, c("person", "machine", "date", "start", "end")],
                             ctx$survey, ctx$fw, ctx$config, ctx$cycle_start,
                             now = ctx$now, n = n + 20),
      error = function(e) NULL)
  }
  if (!is.data.frame(alt)) {
    alt <- res_find_alternatives(booking, ctx$active, ctx$survey, ctx$fw, ctx$s,
                                 ctx$cycle_start, ctx$now, n + 20)
  }
  overlaps <- alt$date == booking$date &
    res_min(alt$start) < res_min(booking$end) & res_min(alt$end) > res_min(booking$start)
  alt <- alt[!overlaps, , drop = FALSE]
  rownames(alt) <- NULL
  head(alt, n)
}

res_pick <- function(ctx, released, queue) {
  if (exists("pick_from_queue", mode = "function")) {
    others <- ctx$active
    pick <- tryCatch(
      pick_from_queue(released[, c("machine", "date", "start")], queue,
                      others[, c("person", "machine", "date", "start", "end")],
                      ctx$survey, ctx$fw, ctx$config, ctx$cycle_start, now = ctx$now),
      error = function(e) NULL)
    if (is.list(pick)) return(pick)
  }
  res_pick_from_queue(released, queue, ctx$active, ctx$survey, ctx$fw, ctx$s,
                      ctx$cycle_start, ctx$now)
}

# ---- Context: everything one action needs, read fresh --------------------------

# fw_cache: environment used to keep free windows between calls (they only
# change when the survey or cycle changes).
res_context <- function(con, config, now, cycle_start = NULL, fw_cache = NULL) {
  s <- res_settings(config)
  res_expire_no_shows(con, now, s)
  b <- res_bookings(con, s)
  cs <- if (is.null(cycle_start)) res_cycle_start(b, now, s) else as.Date(cycle_start)
  survey <- res_survey(con)
  key <- paste(format(cs), res_digest_survey(survey))
  if (!is.null(fw_cache) && identical(fw_cache$key, key)) {
    fw <- fw_cache$fw
  } else {
    tt <- tryCatch(load_timetable(res_path("PATH_TIMETABLE", "data/timetable.csv")),
                   error = function(e) NULL)
    fw <- if (is.na(cs) || is.null(survey) || is.null(tt)) {
      data.frame(resident_id = character(), date = character(), from = character(),
                 to = character())
    } else suppressWarnings(build_free_windows(survey, tt, config, cs))
    if (!is.null(fw_cache)) { fw_cache$key <- key; fw_cache$fw <- fw }
  }
  list(con = con, config = config, s = s, now = now, cycle_start = cs,
       bookings = b, active = b[b$status %in% RES_ACTIVE, ], survey = survey, fw = fw)
}
res_digest_survey <- function(survey) {
  if (is.null(survey)) return("none")
  paste(nrow(survey), sum(nchar(do.call(paste, c(survey, sep = "|"))), na.rm = TRUE))
}

# ---- Actions: each returns list(ok, msg) -------------------------------------

res_my_booking <- function(ctx, booking_id, person) {
  ctx$bookings[ctx$bookings$booking_id == booking_id & ctx$bookings$person == person, ]
}

res_check_in <- function(ctx, booking_id, person) {
  bk <- res_my_booking(ctx, booking_id, person)
  if (nrow(bk) != 1) return(list(ok = FALSE, msg = "Booking not found."))
  st <- res_checkin_status(bk, ctx$now, ctx$s)
  if (!st$ok) return(list(ok = FALSE, msg = st$why))
  set_booking_status(ctx$con, booking_id, "checked_in")
  list(ok = TRUE, msg = "Checked in. Happy washing!")
}

# Freed machine time goes to the first eligible person in the queue.
res_offer_to_queue <- function(ctx, freed) {
  q <- read_queue(ctx$con, "waiting")
  if (nrow(q) == 0) return(NA_character_)
  pick <- res_pick(ctx, freed, q)
  if (is.na(pick$person)) return(NA_character_)
  add_bookings(ctx$con, pick$booking, source = "queue")
  set_queue_status(ctx$con, pick$person, "served")
  pick$person
}

res_release <- function(ctx, booking_id, person) {
  bk <- res_my_booking(ctx, booking_id, person)
  if (nrow(bk) != 1) return(list(ok = FALSE, msg = "Booking not found."))
  st <- res_change_status(bk, ctx$now, ctx$s, res_changes_left(ctx$con, person, ctx$now, ctx$s))
  if (!st$ok) return(list(ok = FALSE, msg = st$why))
  set_booking_status(ctx$con, booking_id, "released")
  add_change(ctx$con, person, booking_id, "release", at = ctx$now)
  ctx$active <- ctx$active[ctx$active$booking_id != booking_id, ]
  to <- res_offer_to_queue(ctx, bk)
  list(ok = TRUE, msg = if (is.na(to)) "Slot released. Others can now claim it."
                        else "Slot released and given to someone in the queue.")
}

res_move <- function(ctx, booking_id, person, new_slot) {
  bk <- res_my_booking(ctx, booking_id, person)
  if (nrow(bk) != 1) return(list(ok = FALSE, msg = "Booking not found."))
  st <- res_change_status(bk, ctx$now, ctx$s, res_changes_left(ctx$con, person, ctx$now, ctx$s))
  if (!st$ok || st$mode != "change") {
    return(list(ok = FALSE, msg = if (st$why != "") st$why else "This slot can't be moved."))
  }
  others <- ctx$active[ctx$active$booking_id != booking_id, ]
  why <- res_why_not(person, new_slot$machine, new_slot$date, new_slot$start,
                     others, ctx$survey, ctx$fw, ctx$s)
  if (why != "") return(list(ok = FALSE, msg = paste("That slot is no longer possible:", why)))
  if (res_dt(new_slot$date, new_slot$start) <= ctx$now) {
    return(list(ok = FALSE, msg = "That slot has already started."))
  }
  add_bookings(ctx$con, new_slot[, c("person", "machine", "date", "start", "end")],
               source = "change")
  set_booking_status(ctx$con, booking_id, "moved")
  add_change(ctx$con, person, booking_id, "change", at = ctx$now)
  ctx$active <- res_bookings(ctx$con, ctx$s)
  ctx$active <- ctx$active[ctx$active$status %in% RES_ACTIVE, ]
  res_offer_to_queue(ctx, bk)
  list(ok = TRUE, msg = paste("Moved to", res_label(new_slot$date, new_slot$start,
                                                    new_slot$end, new_slot$machine)))
}

# Released or moved slots that start in the future and whose machine time is
# still free, with why_not for `person` ("" = they can claim it).
res_free_now <- function(ctx, person) {
  b <- ctx$bookings
  f <- b[b$status %in% RES_FREED & b$start_dt > ctx$now, ]
  if (nrow(f) == 0) return(f)
  still <- vapply(seq_len(nrow(f)), function(i) {
    res_machine_free(ctx$active, f$machine[i], f$date[i], f$occ_from[i], f$occ_from[i] + 1)
  }, logical(1))
  f <- f[still, ]
  f <- f[!duplicated(paste(f$machine, f$date, f$start)), ]
  if (nrow(f) == 0) return(f)
  f <- f[order(f$start_dt), ]
  f$why_not <- vapply(seq_len(nrow(f)), function(i) {
    if (is.null(person)) return("Sign in first")
    res_why_not(person, f$machine[i], f$date[i], f$start[i], ctx$active,
                ctx$survey, ctx$fw, ctx$s)
  }, character(1))
  f$soon <- f$start_dt <= ctx$now + ctx$s$queue_min_lead_min * 60
  f
}

res_claim <- function(ctx, booking_id, person) {
  f <- res_free_now(ctx, person)
  slot <- f[f$booking_id == booking_id, ]
  if (nrow(slot) != 1) return(list(ok = FALSE, msg = "That slot is no longer free."))
  if (slot$why_not != "") return(list(ok = FALSE, msg = slot$why_not))
  dur <- ctx$s$programs[ctx$survey$program[ctx$survey$resident_id == person][1]]
  new <- data.frame(person = person, machine = slot$machine, date = slot$date,
                    start = slot$start, end = res_hhmm(res_min(slot$start) + dur))
  add_bookings(ctx$con, new, source = "free_now")
  if (person %in% read_queue(ctx$con, "waiting")$person) {
    set_queue_status(ctx$con, person, "served")
  }
  list(ok = TRUE, msg = sprintf("It's yours: %s.", res_label(new$date, new$start, new$end,
                                                             new$machine)))
}

# Queue: "waiting" (with position), "can_join" or "not_yet" (with why).
res_queue_status <- function(ctx, person) {
  q <- read_queue(ctx$con, "waiting")
  cyc <- ctx$bookings[ctx$bookings$person == person &
                        res_in_cycle(ctx$bookings$date, ctx$cycle_start, ctx$s), ]
  if (person %in% q$person) {
    blk <- ctx$survey$block[ctx$survey$resident_id == person][1]
    same <- q[q$person %in% ctx$survey$resident_id[ctx$survey$block == blk], ]
    cycb <- ctx$active[res_in_cycle(ctx$active$date, ctx$cycle_start, ctx$s), ]
    have <- vapply(same$person, function(p) sum(cycb$person == p), numeric(1))
    same <- same[order(have >= ctx$s$slots_per_cycle, same$requested_at, same$person), ]
    return(list(state = "waiting", position = which(same$person == person)))
  }
  held <- sum(cyc$status %in% c(RES_ACTIVE, "no_show"))
  used <- sum(cyc$status == "checked_in")
  if (held < ctx$s$slots_per_cycle || used >= ctx$s$slots_per_cycle) {
    return(list(state = "can_join"))
  }
  list(state = "not_yet", why = "You can join once you've used both of your slots this cycle.")
}

res_join_queue <- function(ctx, person) {
  st <- res_queue_status(ctx, person)
  if (st$state != "can_join") {
    return(list(ok = FALSE, msg = if (is.null(st$why)) "Already in the queue." else st$why))
  }
  add_to_queue(ctx$con, person, requested_at = ctx$now)
  # Freed slots more than queue_min_lead_min away go to the queue now.
  f <- res_free_now(ctx, person)
  got <- FALSE
  for (i in seq_len(nrow(f))) {
    if (f$soon[i]) next
    ctx$active <- { b <- res_bookings(ctx$con, ctx$s); b[b$status %in% RES_ACTIVE, ] }
    if (identical(res_offer_to_queue(ctx, f[i, ]), person)) { got <- TRUE; break }
  }
  list(ok = TRUE, msg = if (got) "You've been given a released slot. See My slots."
                        else "You're in the queue for an extra slot.")
}

res_leave_queue <- function(ctx, person) {
  set_queue_status(ctx$con, person, "withdrawn")
  list(ok = TRUE, msg = "You've left the queue.")
}

# ---- UI ----------------------------------------------------------------------

RES_STATUS_LABEL <- c(booked = "Booked", checked_in = "Checked in",
                      no_show = "No-show", released = "Free", moved = "Free")
RES_STATUS_COLOUR <- c(booked = "#2c7fb8", checked_in = "#198754",
                       no_show = "#dc3545", released = "#adb5bd", moved = "#adb5bd")

res_css <- tags$style(HTML("
  .res-card { border:1px solid var(--bs-border-color,#dee2e6); border-radius:8px;
              padding:12px 14px; margin-bottom:10px; max-width:680px; }
  .res-card h5 { margin:0 0 4px 0; }
  .res-pill { display:inline-block; color:#fff; border-radius:10px; padding:1px 9px;
              font-size:.8em; margin-left:6px; vertical-align:middle; }
  .res-hint { color:var(--bs-secondary-color,#6c757d); font-size:.9em; margin-left:6px; }
  .res-card .btn { margin-top:8px; margin-right:6px; }
  .res-info { background:var(--bs-tertiary-bg,#f1f5f9); border-radius:8px;
              padding:10px 14px; margin-bottom:14px; max-width:680px; }
"))

mod_results_ui <- function(id, view = c("mine", "all")) {
  view <- match.arg(view)
  ns <- NS(id)
  user_box <- uiOutput(ns("user_box"))
  if (view == "mine") {
    card(
      fill = FALSE,
      card_header("My slots"),
      card_body(
        fillable = FALSE,
        res_css, user_box,
        uiOutput(ns("notice")),
        conditionalPanel(
          "output.published", ns = ns,
          uiOutput(ns("summary")),
          uiOutput(ns("cards")),
          h5("Extra slot"),
          uiOutput(ns("queue_box")))
      )
    )
  } else {
    card(
      fill = FALSE,
      card_header("Schedule"),
      card_body(
      fillable = FALSE,
      res_css, user_box,
      uiOutput(ns("notice")),
      conditionalPanel(
      "output.published", ns = ns,
      h5("Free now"),
      p(class = "res-hint", style = "margin-left:0",
        "Released slots anyone in that block can claim, first come first served."),
      uiOutput(ns("free_now")),
      hr(),
      div(style = "display:flex; gap:32px; flex-wrap:wrap; align-items:flex-end",
          selectInput(ns("date"), "Date", choices = NULL),
          radioButtons(ns("which_block"), "Show",
                       c("My block" = "mine", "All blocks" = "all"), inline = TRUE)),
      div(style = "margin-top:8px", plotOutput(ns("grid"), height = "640px")),
      div(downloadButton(ns("download"), "Download this cycle (CSV)")))
      )
    )
  }
}

# ---- Server -------------------------------------------------------------------

mod_results_server <- function(id, schedule = NULL, view = c("mine", "all"),
                               con = NULL, current_user = NULL, now = NULL,
                               cycle_start = NULL, refresh_ms = 3000) {
  view <- match.arg(view)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    if (is.null(con)) {
      con <- db_connect()
      session$onSessionEnded(function() DBI::dbDisconnect(con))
    }
    config <- load_config(res_path("PATH_CONFIG", "data/config.csv"))
    fw_cache <- new.env()

    # Clock: ticks every 30 s so buttons open and close on time.
    clock <- reactive({
      invalidateLater(30000)
      if (is.null(now)) Sys.time() else now()
    })

    # Notices changes made by anyone (other pages, other residents).
    db_state <- reactivePoll(refresh_ms, session, function() {
      DBI::dbGetQuery(con, "SELECT
        (SELECT COUNT(*) || ':' || COALESCE(SUM(booking_id * length(status)), 0) FROM bookings)
        || '|' || (SELECT COUNT(*) FROM changes)
        || '|' || (SELECT COUNT(*) || ':' || COALESCE(SUM(length(status)), 0) FROM queue) AS v")$v
    }, function() TRUE)
    bump <- reactiveVal(0)

    # Publish on the Admin page saves the bookings; refresh straight away
    # instead of waiting for the next poll. The schedule itself is not used.
    if (is.function(schedule)) {
      observeEvent(schedule(), bump(isolate(bump()) + 1), ignoreNULL = FALSE)
    }

    ctx <- reactive({
      db_state(); bump()
      cs <- if (is.null(cycle_start)) NULL else cycle_start()
      res_context(con, config, clock(), cs, fw_cache)
    })

    # Has anything been published? Both pages show a notice until it has.
    published <- reactive(nrow(ctx()$bookings) > 0)
    output$published <- reactive(published())
    outputOptions(output, "published", suspendWhenHidden = FALSE)
    output$notice <- renderUI({
      if (published()) return(NULL)
      div(class = "res-info", strong("No schedule published yet."), br(),
          "Your slots will appear here once the admin publishes this cycle's schedule.")
    })

    # Who is signed in: from app.R if given, otherwise a dropdown here.
    output$user_box <- renderUI({
      if (!is.null(current_user)) return(NULL)
      sv <- ctx()$survey
      ids <- sort(unique(c(sv$resident_id, ctx()$bookings$person)))
      lab <- vapply(ids, function(i) {
        nm <- sv$name[sv$resident_id == i]
        if (length(nm) && !is.na(nm[1])) paste0(i, " (", nm[1], ")") else i
      }, character(1))
      sel <- isolate(input$user)
      div(style = "max-width:320px",
          selectInput(ns("user"), "Signed in as", choices = setNames(ids, lab),
                      selected = if (!is.null(sel) && sel %in% ids) sel else ids[1]))
    })
    me <- reactive({
      u <- if (!is.null(current_user)) current_user() else input$user
      req(u)
      u
    })

    act <- function(label, type, id = 0, enabled = TRUE, class = "btn-outline-secondary") {
      js <- sprintf("Shiny.setInputValue('%s', {type: '%s', id: %s, nonce: Math.random()}, {priority: 'event'})",
                    ns("action"), type, id)
      tags$button(class = paste("btn btn-sm", class), onclick = js,
                  disabled = if (!enabled) NA else NULL, label)
    }
    done <- function(r) {
      showNotification(r$msg, type = if (isTRUE(r$ok)) "message" else "error")
      bump(bump() + 1)
    }

    # ---- My slots -----------------------------------------------------------
    output$summary <- renderUI({
      x <- ctx(); p <- me()
      if (is.na(x$cycle_start)) return(NULL)
      div(class = "res-info",
          strong(sprintf("Cycle: %s to %s", res_day(x$cycle_start),
                         res_day(x$cycle_start + x$s$cycle_days - 1))), br(),
          sprintf("Changes left: %g of %g (each counts for %g days).",
                  res_changes_left(con, p, x$now, x$s), x$s$max_changes,
                  x$s$changes_window_days), br(),
          sprintf("No-shows so far: %d.", res_no_shows(x$bookings, p)))
    })

    output$cards <- renderUI({
      x <- ctx(); p <- me()
      if (is.na(x$cycle_start)) return(NULL)
      mine <- x$bookings[x$bookings$person == p &
                           res_in_cycle(x$bookings$date, x$cycle_start, x$s) &
                           x$bookings$status %in% c(RES_ACTIVE, "no_show"), ]
      if (nrow(mine) == 0) return(div(class = "res-card", "You have no slots in this cycle."))
      mine <- mine[order(mine$start_dt), ]
      left <- res_changes_left(con, p, x$now, x$s)
      lapply(seq_len(nrow(mine)), function(i) {
        bk <- mine[i, ]
        ci <- res_checkin_status(bk, x$now, x$s)
        ch <- res_change_status(bk, x$now, x$s, left)
        extra <- if (bk$source %in% c("queue", "free_now")) " (extra slot)" else
                 if (bk$source == "change") " (changed)" else ""
        div(class = "res-card",
            h5(sprintf("%s, %s-%s", res_day(bk$date), bk$start, bk$end),
               span(class = "res-pill", style = paste0("background:", RES_STATUS_COLOUR[[bk$status]]),
                    RES_STATUS_LABEL[[bk$status]])),
            div(paste0("Machine ", bk$machine, extra)),
            if (bk$status == "booked") tagList(
              act("Started", "checkin", bk$booking_id, ci$ok, "btn-success"),
              act("Can't make it", "cant", bk$booking_id, ch$ok),
              span(class = "res-hint", if (ci$ok) ci$why else if (ch$why != "") ch$why else ci$why)
            ))
      })
    })

    output$queue_box <- renderUI({
      x <- ctx(); p <- me()
      if (is.na(x$cycle_start)) return(NULL)
      st <- res_queue_status(x, p)
      switch(st$state,
        waiting = div(class = "res-card",
                      sprintf("You're in the queue (number %d in your block). ", st$position),
                      "A released slot that fits will appear in My slots.", br(),
                      act("Leave queue", "leave_queue")),
        can_join = div(class = "res-card",
                       "Need another wash? Join the queue and you'll get the next released slot that fits.",
                       br(), act("Join queue", "join_queue", class = "btn-primary")),
        div(class = "res-card", st$why))
    })

    # ---- "Can't make it" ----------------------------------------------------
    pending <- reactiveVal(NULL)
    options <- reactiveVal(NULL)

    open_dialog <- function(booking_id) {
      x <- ctx(); p <- me()
      bk <- res_my_booking(x, booking_id, p)
      req(nrow(bk) == 1)
      st <- res_change_status(bk, x$now, x$s, res_changes_left(con, p, x$now, x$s))
      if (!st$ok) { done(list(ok = FALSE, msg = st$why)); return() }
      alts <- if (st$mode == "change") res_alternatives(x, bk) else NULL
      pending(booking_id); options(alts)
      have_alts <- is.data.frame(alts) && nrow(alts) > 0
      choices <- if (have_alts) setNames(as.character(seq_len(nrow(alts))),
                                         res_label(alts$date, alts$start, alts$end, alts$machine))
      showModal(modalDialog(
        title = sprintf("Can't make %s, %s-%s?", res_day(bk$date), bk$start, bk$end),
        p(sprintf("Moving or releasing uses 1 of your %g remaining changes.",
                  res_changes_left(con, p, x$now, x$s))),
        if (st$mode == "release") p(em(st$why))
        else if (have_alts) radioButtons(ns("alt"), "Move to:", choices = choices, width = "100%")
        else p(em("No other slot fits your free time right now. You can still release it.")),
        p(class = "res-hint", style = "margin-left:0",
          "A released slot goes to someone in the queue, or to the Free now list."),
        footer = tagList(
          modalButton("Keep my slot"),
          actionButton(ns("do_release"), "Release it", class = "btn-outline-danger"),
          if (have_alts) actionButton(ns("do_move"), "Move", class = "btn-primary")
        ),
        easyClose = TRUE))
    }

    observeEvent(input$action, {
      a <- input$action; x <- ctx(); p <- me()
      switch(a$type,
        checkin     = done(res_check_in(x, a$id, p)),
        cant        = open_dialog(a$id),
        claim       = done(res_claim(x, a$id, p)),
        join_queue  = done(res_join_queue(x, p)),
        leave_queue = done(res_leave_queue(x, p)))
    })
    observeEvent(input$do_move, {
      alts <- options(); i <- as.integer(input$alt)
      req(pending(), is.data.frame(alts), length(i) == 1, i <= nrow(alts))
      removeModal()
      done(res_move(ctx(), pending(), me(), alts[i, ]))
    })
    observeEvent(input$do_release, {
      req(pending())
      removeModal()
      done(res_release(ctx(), pending(), me()))
    })

    # ---- Schedule -----------------------------------------------------------
    output$free_now <- renderUI({
      x <- ctx()
      f <- res_free_now(x, me())
      if (nrow(f) == 0) return(p(em("Nothing free right now.")))
      lapply(seq_len(nrow(f)), function(i) {
        div(class = "res-card",
            strong(res_label(f$date[i], f$start[i], f$end[i], f$machine[i])),
            if (f$soon[i]) span(class = "res-pill", style = "background:#fd7e14", "starting soon"),
            br(),
            act("Claim it", "claim", f$booking_id[i], f$why_not[i] == "", "btn-success"),
            span(class = "res-hint", f$why_not[i]))
      })
    })

    observe({
      x <- ctx()
      req(!is.na(x$cycle_start))
      days <- format(x$cycle_start + seq_len(x$s$cycle_days) - 1)
      today <- format(res_today(x$now))
      cur <- isolate(input$date)
      sel <- if (!is.null(cur) && cur %in% days) cur else if (today %in% days) today else days[1]
      updateSelectInput(session, "date", choices = setNames(days, res_day(days)), selected = sel)
    })

    output$grid <- renderPlot({
      req(input$date)
      x <- ctx(); p <- me()
      b <- x$bookings
      s <- b[b$date == input$date & b$status %in% c(RES_ACTIVE, "no_show", RES_FREED), ]
      my_block <- x$survey$block[x$survey$resident_id == p][1]
      machines <- x$s$machines
      if (input$which_block == "mine" && !is.na(my_block)) {
        machines <- machines[machines$block == my_block, ]
      }
      s <- s[s$machine %in% machines$machine, ]
      # a freed slot someone has since taken is hidden behind the new booking
      s <- s[!(s$status %in% RES_FREED & vapply(seq_len(nrow(s)), function(i) {
        !res_machine_free(x$active, s$machine[i], s$date[i], s$occ_from[i], s$occ_from[i] + 1)
      }, logical(1))), ]
      lv <- machines$machine
      validate(need(length(lv) > 0, "No machines to show."))
      s$x <- match(s$machine, lv)
      s$y0 <- res_min(s$start); s$y1 <- res_min(s$end)
      s$who <- ifelse(s$person == p, "You", s$person)
      s$who[s$status %in% RES_FREED] <- "free"
      s$state <- factor(RES_STATUS_LABEL[s$status], levels = unique(RES_STATUS_LABEL))
      cols <- setNames(RES_STATUS_COLOUR[c("booked", "checked_in", "no_show", "released")],
                       unique(RES_STATUS_LABEL))
      lo <- x$s$open; hi <- x$s$close
      ggplot(s) +
        geom_rect(aes(xmin = x - 0.42, xmax = x + 0.42, ymin = -y1, ymax = -y0, fill = state),
                  colour = ifelse(s$person == p, "black", "white"), linewidth = 0.6) +
        geom_text(aes(x = x, y = -(y0 + y1) / 2, label = who), size = 3.3,
                  colour = "white", fontface = "bold") +
        scale_fill_manual(values = cols, drop = FALSE, name = NULL) +
        scale_x_continuous(breaks = seq_along(lv), labels = lv,
                           limits = c(0.5, length(lv) + 0.5)) +
        # time runs down the page: minutes are plotted as negative numbers
        scale_y_continuous(breaks = -seq(lo, hi, 60), labels = res_hhmm(seq(lo, hi, 60)),
                           limits = c(-hi, -lo)) +
        labs(x = "Machine", y = NULL, title = res_day(input$date),
             subtitle = if (nrow(s) == 0) "No bookings on this date" else NULL) +
        theme_minimal(base_size = 14) +
        theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
              legend.position = "top")
    })

    output$download <- downloadHandler(
      filename = function() paste0("laundry_cycle_", format(ctx()$cycle_start), ".csv"),
      content = function(file) {
        x <- ctx()
        b <- x$bookings[res_in_cycle(x$bookings$date, x$cycle_start, x$s) &
                          x$bookings$status %in% c(RES_ACTIVE, "no_show"), ]
        b <- b[order(b$date, b$machine, b$start), ]
        utils::write.csv(b[, c("person", "machine", "date", "start", "end", "status", "source")],
                         file, row.names = FALSE)
      })
  })
}
