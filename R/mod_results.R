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
#   (from slot start to checkin_grace_min after); "Can't make it" cancels the
#   slot up to cancel_cutoff_hours before it starts, making it a free slot;
#   "Finished" after checking in (check_out()); then a 1-5 star rating with an
#   optional comment (add_feedback()), which can be skipped and given later;
#   cancellations and no-shows in the last penalty_window_days, and any slots
#   lost this cycle.
# Schedule ("all"): every machine by date, coloured by status, and the
#   "Free now" list of cancelled slots anyone eligible can claim. A missed slot
#   is a no-show and is not offered to anyone.
#
# All rules follow CLAUDE.md "Scheduling rules" and "Cancellations, no-shows
# and free slots". Rule values come from data/config.csv via load_config().
#
# Uses from other files (all optional except the R/data_load.R helpers):
#   R/data_load.R  load_config, load_survey, load_timetable,
#                  build_free_windows, db_connect, read_responses,
#                  read_bookings, add_bookings, set_booking_status,
#                  read_changes, add_change, slot_penalties
#   R/mod_questionnaire.R  PATH_SURVEY, PATH_TIMETABLE, PATH_CONFIG (the
#                  files the Admin page uses), if defined.
#
# Until the database has any bookings, both pages say "No schedule published
# yet." Nothing here ever writes the Admin page's schedule to the database:
# only Publish (R/mod_admin.R) does that.
#
# Every function and constant here starts with res_ / RES_ so nothing clashes
# with the other files, which are all sourced into the same environment.

# ---- Settings ----------------------------------------------------------------

RES_DEFAULTS <- list(checkin_grace_min = 15, cancel_cutoff_hours = 6,
                     max_cancellations = 3, max_no_shows = 2,
                     penalty_window_days = 30, reserve_share = 0,
                     buffer_min = 10, grid_min = 30, cycle_days = 10)
RES_ACTIVE <- c("booked", "checked_in", "finished")  # hold the machine / count as a booking
RES_FREED  <- "released"                             # free for anyone to claim

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

# Cancellations and no-shows in the last penalty_window_days.
res_cancellations <- function(con, person, now, s) {
  ch <- read_changes(con, person)
  ch <- ch[ch$kind == "release", ]
  if (nrow(ch) == 0) return(0)
  sum(as.POSIXct(ch$at) > now - s$penalty_window_days * 86400)
}
res_no_shows <- function(b, person, now, s) {
  sum(b$person == person & b$status == "no_show" &
        b$start_dt > now - s$penalty_window_days * 86400)
}
# Slots this person lost in the given cycle.
res_slots_lost <- function(con, config, person, cycle_start, now) {
  p <- slot_penalties(con, cycle_start, config, now)
  sum(p$slots_lost[p$resident_id == person])
}

# ---- Rule checks: each returns list(ok, why) -------------------------------------

res_checkin_status <- function(bk, now, s) {
  closes <- bk$start_dt + s$checkin_grace_min * 60
  if (bk$status != "booked") return(list(ok = FALSE, why = ""))
  if (now < bk$start_dt) return(list(ok = FALSE, why = paste("Check-in opens at", bk$start)))
  if (now > closes) return(list(ok = FALSE, why = "Check-in has closed"))
  list(ok = TRUE, why = paste("Tap Started before", format(closes, "%H:%M")))
}

# Can "Can't make it" cancel this slot? Only up to cancel_cutoff_hours before
# it starts. After that, not checking in makes it a no-show.
res_cancel_status <- function(bk, now, s) {
  if (bk$status != "booked") return(list(ok = FALSE, why = ""))
  if (now >= bk$start_dt) return(list(ok = FALSE, why = "Slot has started"))
  if (now > bk$start_dt - s$cancel_cutoff_hours * 3600) {
    return(list(ok = FALSE,
                why = sprintf("Under %g h to go: it can't be cancelled, and missing it counts as a no-show",
                              s$cancel_cutoff_hours)))
  }
  list(ok = TRUE, why = "")
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
       bookings = b, active = b[b$status %in% RES_ACTIVE, ], survey = survey, fw = fw,
       feedback = res_read_feedback(con))
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
  # Person 2's check_in() also records the time; fall back if it isn't there.
  if (exists("check_in", mode = "function")) {
    r <- tryCatch(check_in(ctx$con, booking_id, at = ctx$now, config = ctx$config),
                  error = function(e) e)
    if (inherits(r, "error")) return(list(ok = FALSE, msg = conditionMessage(r)))
  } else {
    set_booking_status(ctx$con, booking_id, "checked_in")
  }
  list(ok = TRUE, msg = "Checked in. Happy washing!")
}

# "Finished": Person 2's check_out() marks the wash finished (needs R/data_load.R
# with check_out(); the button is hidden without it).
res_can_check_out <- function() exists("check_out", mode = "function")
res_check_out <- function(ctx, booking_id, person) {
  bk <- res_my_booking(ctx, booking_id, person)
  if (nrow(bk) != 1) return(list(ok = FALSE, msg = "Booking not found."))
  if (bk$status != "checked_in") return(list(ok = FALSE, msg = "Check in first."))
  if (!res_can_check_out()) return(list(ok = FALSE, msg = "Not available yet."))
  r <- tryCatch(check_out(ctx$con, booking_id, at = ctx$now), error = function(e) e)
  if (inherits(r, "error")) return(list(ok = FALSE, msg = conditionMessage(r)))
  list(ok = TRUE, msg = "Marked as finished. Thanks for freeing the machine!")
}

# ---- Star rating after a wash (Person 2's add_feedback / read_feedback) ------

res_can_rate <- function() {
  exists("add_feedback", mode = "function") && exists("read_feedback", mode = "function")
}
res_read_feedback <- function(con) {
  empty <- data.frame(feedback_id = integer(), booking_id = integer(), person = character(),
                      rating = integer(), comment = character(), at = character())
  if (!res_can_rate()) return(empty)
  tryCatch(read_feedback(con), error = function(e) empty)
}
# The feedback row for a booking, or zero rows.
res_feedback_for <- function(ctx, booking_id) {
  ctx$feedback[ctx$feedback$booking_id == booking_id, , drop = FALSE]
}
# Five stars as HTML, the first n filled.
res_stars <- function(n) {
  n <- as.integer(n)
  HTML(paste0('<span style="color:#f5a623;font-size:1.15em;letter-spacing:1px">',
              strrep("&#9733;", n), "</span>",
              '<span style="color:#ced4da;font-size:1.15em;letter-spacing:1px">',
              strrep("&#9734;", 5 - n), "</span>"))
}
res_rate <- function(ctx, booking_id, person, rating, comment = NA) {
  bk <- res_my_booking(ctx, booking_id, person)
  if (nrow(bk) != 1) return(list(ok = FALSE, msg = "Booking not found."))
  if (!res_can_rate()) return(list(ok = FALSE, msg = "Ratings are not available yet."))
  if (bk$status != "finished") return(list(ok = FALSE, msg = "You can rate a wash once it's finished."))
  if (nrow(res_feedback_for(ctx, booking_id)) > 0) {
    return(list(ok = FALSE, msg = "You've already rated this wash."))
  }
  r <- tryCatch(add_feedback(ctx$con, booking_id, rating, comment, at = ctx$now),
                error = function(e) e)
  if (inherits(r, "error")) return(list(ok = FALSE, msg = conditionMessage(r)))
  list(ok = TRUE, msg = "Thanks for your feedback!")
}

res_release <- function(ctx, booking_id, person) {
  bk <- res_my_booking(ctx, booking_id, person)
  if (nrow(bk) != 1) return(list(ok = FALSE, msg = "Booking not found."))
  st <- res_cancel_status(bk, ctx$now, ctx$s)
  if (!st$ok) return(list(ok = FALSE, msg = st$why))
  set_booking_status(ctx$con, booking_id, "released")
  add_change(ctx$con, person, booking_id, "release", at = ctx$now)
  list(ok = TRUE, msg = "Slot cancelled. It's now a free slot anyone can claim.")
}

# Cancelled slots that start in the future and whose machine time is still
# free, with why_not for `person` ("" = they can claim it).
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
  list(ok = TRUE, msg = sprintf("It's yours: %s.", res_label(new$date, new$start, new$end,
                                                             new$machine)))
}

# ---- UI ----------------------------------------------------------------------

RES_STATUS_LABEL <- c(booked = "Booked", checked_in = "Checked in",
                      finished = "Finished", no_show = "No-show",
                      released = "Free")
RES_STATUS_COLOUR <- c(booked = "#2c7fb8", checked_in = "#198754",
                       finished = "#6f42c1", no_show = "#dc3545",
                       released = "#adb5bd")

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
          uiOutput(ns("cards")))
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
        "Cancelled slots anyone in that block can claim, first come first served."),
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
        || '|' || (SELECT COUNT(*) FROM changes) AS v")$v
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
      lost <- res_slots_lost(con, config, p, x$cycle_start, x$now)
      div(class = "res-info",
          strong(sprintf("Cycle: %s to %s", res_day(x$cycle_start),
                         res_day(x$cycle_start + x$s$cycle_days - 1))), br(),
          sprintf("Cancellations in the last %g days: %g (%g allowed).",
                  x$s$penalty_window_days, res_cancellations(con, p, x$now, x$s),
                  x$s$max_cancellations), br(),
          sprintf("No-shows in the last %g days: %g (%g allowed).",
                  x$s$penalty_window_days, res_no_shows(x$bookings, p, x$now, x$s),
                  x$s$max_no_shows), br(),
          "Going over either limit costs you one slot in the next cycle.",
          if (lost > 0) tagList(br(), strong(sprintf(
            "You lost %g slot(s) this cycle for going over a limit.", lost))))
    })

    output$cards <- renderUI({
      x <- ctx(); p <- me()
      if (is.na(x$cycle_start)) return(NULL)
      mine <- x$bookings[x$bookings$person == p &
                           res_in_cycle(x$bookings$date, x$cycle_start, x$s) &
                           x$bookings$status %in% c(RES_ACTIVE, "no_show"), ]
      if (nrow(mine) == 0) return(div(class = "res-card", "You have no slots in this cycle."))
      mine <- mine[order(mine$start_dt), ]
      lapply(seq_len(nrow(mine)), function(i) {
        bk <- mine[i, ]
        ci <- res_checkin_status(bk, x$now, x$s)
        ch <- res_cancel_status(bk, x$now, x$s)
        extra <- if (bk$source == "free_now") " (free slot)" else ""
        div(class = "res-card",
            h5(sprintf("%s, %s-%s", res_day(bk$date), bk$start, bk$end),
               span(class = "res-pill", style = paste0("background:", RES_STATUS_COLOUR[[bk$status]]),
                    RES_STATUS_LABEL[[bk$status]])),
            div(paste0("Machine ", bk$machine, extra)),
            if (bk$status == "booked") tagList(
              act("Started", "checkin", bk$booking_id, ci$ok, "btn-success"),
              act("Can't make it", "cant", bk$booking_id, ch$ok),
              span(class = "res-hint", if (ci$ok) ci$why else if (ch$why != "") ch$why else ci$why)
            ),
            if (bk$status == "checked_in" && res_can_check_out()) tagList(
              act("Finished", "checkout", bk$booking_id, TRUE, "btn-primary"),
              span(class = "res-hint", "Tap when your wash is done")
            ),
            if (bk$status == "finished" && res_can_rate()) {
              fb <- res_feedback_for(x, bk$booking_id)
              if (nrow(fb) > 0) {
                div(style = "margin-top:6px", res_stars(fb$rating[1]),
                    span(class = "res-hint", sprintf("You rated %d/5", as.integer(fb$rating[1]))),
                    if (!is.na(fb$comment[1]) && nzchar(fb$comment[1]))
                      div(class = "res-hint", style = "margin-left:0", em(HTML(paste0("&ldquo;", htmltools::htmlEscape(fb$comment[1]), "&rdquo;")))))
              } else {
                tagList(act("Rate this wash", "rate", bk$booking_id, TRUE, "btn-outline-warning"),
                        span(class = "res-hint", "How was it? 1-5 stars"))
              }
            })
      })
    })

    # ---- "Can't make it" ----------------------------------------------------
    pending <- reactiveVal(NULL)

    open_dialog <- function(booking_id) {
      x <- ctx(); p <- me()
      bk <- res_my_booking(x, booking_id, p)
      req(nrow(bk) == 1)
      st <- res_cancel_status(bk, x$now, x$s)
      if (!st$ok) { done(list(ok = FALSE, msg = st$why)); return() }
      pending(booking_id)
      showModal(modalDialog(
        title = sprintf("Can't make %s, %s-%s?", res_day(bk$date), bk$start, bk$end),
        p(sprintf("This will be cancellation %g in the last %g days (%g allowed). Going over the limit costs you one slot in the next cycle.",
                  res_cancellations(con, p, x$now, x$s) + 1, x$s$penalty_window_days,
                  x$s$max_cancellations)),
        p(class = "res-hint", style = "margin-left:0",
          "The slot goes on the Free now list, where anyone can claim it."),
        footer = tagList(
          modalButton("Keep my slot"),
          actionButton(ns("do_release"), "Cancel this slot", class = "btn-outline-danger")
        ),
        easyClose = TRUE))
    }

    # ---- Star rating ----------------------------------------------------------
    rating_for <- reactiveVal(NULL)
    open_rating <- function(booking_id) {
      x <- ctx(); bk <- res_my_booking(x, booking_id, me())
      if (nrow(bk) != 1 || !res_can_rate()) return()
      rating_for(booking_id)
      showModal(modalDialog(
        title = "How was your wash?",
        p(sprintf("%s, %s-%s on machine %s", res_day(bk$date), bk$start, bk$end, bk$machine)),
        radioButtons(ns("stars"), NULL, width = "100%", selected = character(0),
                     choiceNames = lapply(1:5, function(n) span(res_stars(n), " ", n)),
                     choiceValues = as.character(1:5)),
        textAreaInput(ns("comment"), "Comment (optional)", width = "100%", rows = 3,
                      placeholder = "Anything we should know? e.g. machine was dirty, waited for it"),
        footer = tagList(modalButton("Skip"),
                         actionButton(ns("do_rate"), "Submit", class = "btn-primary")),
        easyClose = TRUE))
    }
    observeEvent(input$do_rate, {
      if (is.null(input$stars) || !nzchar(input$stars)) {
        showNotification("Pick 1 to 5 stars first.", type = "warning")
        return()
      }
      req(rating_for())
      removeModal()
      done(res_rate(ctx(), rating_for(), me(), as.integer(input$stars), input$comment))
      rating_for(NULL)
    })

    observeEvent(input$action, {
      a <- input$action; x <- ctx(); p <- me()
      switch(a$type,
        checkin     = done(res_check_in(x, a$id, p)),
        checkout    = {
          r <- res_check_out(x, a$id, p)
          done(r)
          if (isTRUE(r$ok)) open_rating(a$id)       # ask for stars straight away
        },
        rate        = open_rating(a$id),
        cant        = open_dialog(a$id),
        claim       = done(res_claim(x, a$id, p)))
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
      cols <- setNames(RES_STATUS_COLOUR[c("booked", "checked_in", "finished", "no_show",
                                           "released")], unique(RES_STATUS_LABEL))
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
