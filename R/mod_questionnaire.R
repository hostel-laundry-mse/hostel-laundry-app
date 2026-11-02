# Questionnaire module: a resident enters their @mse.ac.in email, which must be
# in the residents table (the participant list), then their course, wash
# program, blocked times and preferences, and agrees to the Terms of Use. A
# "Demo login (testing only)" option lets a sample resident be used instead.
#
# mod_questionnaire_ui(id)
#   Takes: module id.
#   Returns: UI for the Questionnaire page.
#
# mod_questionnaire_server(id, con)
#   Takes: module id, con - DBI connection from db_connect().
#   Returns: reactive giving the latest submitted response as a one-row
#            data.frame in the survey format (see CLAUDE.md).
#
# Row saved (one per resident, a resubmission replaces the earlier row), the
# survey format in CLAUDE.md:
#   resident_id, name, block, program, course, blocked, pref_days,
#   pref_band
# pref_band is not asked any more and is always "No preference".
# `blocked` is `;`-separated `Day HH:MM-HH:MM`. An end before the start runs
# past midnight (Mon 23:00-07:00) and counts towards the day it starts on; an
# end of 24:00 means midnight.
#
# Identity. The email is looked up with find_resident_by_email(); resident_id,
# name and block come from that residents row and cannot be edited. An email
# that is not listed cannot sign in. Agreeing to the Terms is recorded with
# record_consent() (version TERMS_VERSION) before the answers are saved with
# write_responses(); if the consent cannot be recorded the answers are not
# saved. The demo login uses a sample resident from the fake survey: its answers
# are saved but no consent is recorded, since it is not in residents.

# ---- File paths (the only place they are written) ----------------------------
PATH_SURVEY <- "data/fake_survey.csv"
PATH_TIMETABLE <- "data/timetable.csv"
PATH_CONFIG <- "data/config.csv"
PATH_LAST_PLANNED <- "data/fake_last_planned.csv"
PATH_FAKE_SCHEDULE <- "data/fake_schedule.csv"

SURVEY_COLUMNS <- c("resident_id", "name", "block", "program",
                    "course", "blocked", "pref_days", "pref_band")

EMAIL_DOMAIN <- "mse.ac.in"

# Change this when the terms change: everyone then has to agree again.
TERMS_VERSION <- "2026-10-31-v1"

CONSENT_LABEL <- "I have read and agree to the Terms of Use and the data policy."

# Terms of Use, shown in a modal. Each section: title, optional intro, and
# either bullets or a numbered list.
TERMS <- list(
  list(title = "1. How slots are allocated", bullets = c(
    "The app tries to match your preferred days and times, but they are not guaranteed. The schedule is built from everyone's timetables, availability and preferences, so some requests can't be met.",
    "Each resident gets up to 2 slots per 10-day cycle, with a gap of 4–5 days between them. A quick wash is 30 minutes and a normal wash is 60 minutes.",
    "Access is by block: Block D for boys, Blocks A–C for girls.",
    "A small share of slots is kept free for walk-ins. Extra slots are given only after everyone's two slots have been used.")),
  list(title = "2. Your responsibilities", bullets = c(
    "Give accurate lecture, blocked-time and preference details. Wrong details can make the schedule unsuitable for you.",
    "Check in within 15 minutes of your slot's start. After that, the slot is released to others.",
    "Use only your own slots. Don't swap or sell them.")),
  list(title = "3. Cancelling or changing a slot", bullets = c(
    "You can cancel or change a slot up to 6 hours before it starts.",
    "You can do this 3 times in any 30 days. Changes and cancellations count together.",
    "A cancelled slot is released and goes to the priority queue.")),
  list(title = "4. Cancellation vs no-show", bullets = c(
    "Cancellation: you tell the app at least 6 hours before your slot. It doesn't count against you and is not a no-show. It does use one of your 3 changes for the 30 days.",
    "No-show: you don't check in within 15 minutes of your slot's start, or you cancel less than 6 hours before. It moves you further back in the queue (see section 5).",
    "A resident can have at most 3 no-shows. After your 3rd no-show, your allowance for the following month is reduced by 2 slots.")),
  list(title = "5. Priority Queue for free slots",
       intro = "A slot can become free at short notice, a few hours before it starts or right at the start, for example because someone cancelled or didn't check in. It goes to the first person in the queue by this order. If two people are in the same group, whoever asked first goes first.",
       numbered = c(
    "You cancelled your slot and haven't had your 1st slot in this 10-day cycle.",
    "You haven't had your 2nd slot in this cycle.",
    "You had 1 no-show and haven't had your 1st slot.",
    "You had 1 no-show and haven't had your 2nd slot.",
    "You asked for an extra slot after using both of your slots.",
    "You had 2 no-shows and haven't had your 1st slot.",
    "You had 2 no-shows and haven't had your 2nd slot.",
    "You had 3 no-shows and haven't had your 1st slot.",
    "You had 3 no-shows and haven't had your 2nd slot.")),
  list(title = "6. Walk-in (emergency) slots",
       intro = "Some slots, usually at times when most residents are in class such as mornings, are kept free and not booked in advance. Use them for urgent washes. They are open to anyone in your block's allowed group."),
  list(title = "7. Your data", bullets = c(
    "We collect your name, block, course, blocked times and preferences only to build and evaluate the laundry schedule, as part of a statistics course project.",
    "Only the project team and the course instructor can see it. It is not shared elsewhere, and results are reported in aggregate without identifying you.",
    "Taking part is voluntary. You can ask for your answers to be removed at any time.")),
  list(title = "8. Data Policy", bullets = c(
    "Data will be deleted at the end of each academic year.")),
  list(title = "9. Other", bullets = c(
    "This is a pilot. The schedule may change if machines are unavailable or rules are updated, and the team will tell residents when that happens."))
)

terms_ui <- function() {
  tagList(lapply(TERMS, function(sec) {
    tagList(
      h6(class = "mt-3", sec$title),
      if (!is.null(sec$intro)) p(sec$intro),
      if (!is.null(sec$bullets)) tags$ul(lapply(sec$bullets, tags$li)),
      if (!is.null(sec$numbered)) tags$ol(lapply(sec$numbered, tags$li))
    )
  }))
}

# ---- Fallbacks, used only if a file above fails to load ----------------------
FALLBACK_DAYS <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
FALLBACK_BLOCKS <- c("A", "B", "C", "D")
FALLBACK_COURSES <- c("DS", "FE", "GE", "AE", "AQF", "EE", "MBA")
FALLBACK_PROGRAMS <- c(quick = 30, normal = 60)
# The time band is no longer asked (the optimiser does not use it). The saved
# row keeps the pref_band column, always set to this value.
NO_BAND <- "No preference"
FALLBACK_MAX_BLOCKED_HOURS <- 10
# Daily blocked-time limit on a day the course has no class (on days with a
# class it is max_blocked_hours from the config, 10 by default).
NO_CLASS_LIMIT_HOURS <- 12
FALLBACK_RESIDENTS <- data.frame(
  resident_id = c("R001", "R002"), name = c("Kiran Okafor", "Noor Mehta"),
  block = c("A", "A"),
  program = c("quick", "normal"), course = c("DS", "AE"),
  blocked = c("Mon 23:00-07:00", ""), pref_days = c("Sat", ""),
  pref_band = c("morning", ""), stringsAsFactors = FALSE
)

# ---- Reading files -----------------------------------------------------------

# Everything as character, empty cells stay "". NULL if the file fails to load.
read_csv_chr <- function(path) {
  tryCatch(
    as.data.frame(readr::read_csv(path, col_types = readr::cols(.default = "c"),
                                  na = character(), show_col_types = FALSE)),
    error = function(e) NULL, warning = function(w) NULL)
}

# named character vector key -> value for one config section
config_section <- function(tab, section) {
  rows <- tab[tab$section == section, , drop = FALSE]
  setNames(rows$value, rows$key)
}

# What the forms need from data/config.csv, with fallbacks if it fails to load.
q_config <- function(path = PATH_CONFIG) {
  tab <- read_csv_chr(path)
  ok <- is.data.frame(tab) && all(c("section", "key", "value") %in% names(tab))
  if (!ok) {
    return(list(from_file = FALSE, blocks = FALLBACK_BLOCKS,
                programs = FALLBACK_PROGRAMS,
                days = FALLBACK_DAYS,
                max_blocked_hours = FALLBACK_MAX_BLOCKED_HOURS))
  }
  or <- function(x, default) if (length(x)) x else default
  rules <- config_section(tab, "rules")
  hours <- config_section(tab, "hours")
  programs <- config_section(tab, "program")
  list(
    from_file = TRUE,
    blocks = or(names(config_section(tab, "machines")), FALLBACK_BLOCKS),
    programs = or(setNames(as.numeric(programs), names(programs)),
                  FALLBACK_PROGRAMS),
    days = if ("days" %in% names(hours)) {
      strsplit(hours[["days"]], ";", fixed = TRUE)[[1]]
    } else FALLBACK_DAYS,
    max_blocked_hours = if ("max_blocked_hours" %in% names(rules)) {
      as.numeric(rules[["max_blocked_hours"]])
    } else FALLBACK_MAX_BLOCKED_HOURS
  )
}

# Unique courses in the timetable.
q_courses <- function(path = PATH_TIMETABLE) {
  t <- read_csv_chr(path)
  if (is.data.frame(t) && "course" %in% names(t) && nrow(t) > 0) {
    return(unique(t$course))
  }
  FALLBACK_COURSES
}

# Weekdays on which `course` has a lecture in the timetable (columns course,
# day, start, end; day is Mon ... Sun). NULL if the file does not load or the
# course is not in it.
class_days <- function(course, path = PATH_TIMETABLE) {
  t <- read_csv_chr(path)
  if (!is.data.frame(t) || !all(c("course", "day") %in% names(t))) return(NULL)
  if (length(course) != 1 || is.na(course) || !course %in% t$course) return(NULL)
  unique(t$day[t$course == course])
}

# The blocked hours allowed per day for a course: `class_limit` on days with a
# class, `free_limit` on days without. Class time itself does not count towards
# the blocked hours. A course that is not found gets `class_limit` on every day.
# Returns a named numeric vector (one entry per day) whose "class_days"
# attribute lists the class days; the attribute is missing for an unknown course.
day_limits <- function(course, days = FALLBACK_DAYS,
                       class_limit = FALLBACK_MAX_BLOCKED_HOURS,
                       free_limit = NO_CLASS_LIMIT_HOURS, path = PATH_TIMETABLE) {
  cd <- class_days(course, path)
  if (is.null(cd)) return(setNames(rep(class_limit, length(days)), days))
  structure(setNames(ifelse(days %in% cd, class_limit, free_limit), days),
            class_days = cd)
}

# The limit that applies on `day`: `limit` is one number for every day, or a
# named vector from day_limits().
limit_on <- function(limit, day) {
  if (!is.null(names(limit)) && day %in% names(limit)) return(limit[[day]])
  if (length(limit) == 1 && is.null(names(limit))) return(limit)
  FALLBACK_MAX_BLOCKED_HOURS
}

# Words for why a day has its limit, for the messages.
limit_reason <- function(limit, day) {
  cd <- attr(limit, "class_days")
  if (is.null(cd)) return("per day")
  if (day %in% cd) "on days with classes" else "on days without classes"
}

# "Mon (11 h, limit 10 h on days with classes)" for each day in `over`.
over_text <- function(over, limit) {
  paste0(names(over), " (", round(over, 1), " h, limit ", vapply(
    names(over), function(d) limit_on(limit, d), 1), " h ", vapply(
    names(over), function(d) limit_reason(limit, d), ""), ")", collapse = ", ")
}

# Blocks for the Block dropdown: the machines keys of data/config.csv; if the
# config lists none, the blocks in the residents table; the fallback only if
# both are empty.
q_blocks <- function(con = NULL, path = PATH_CONFIG) {
  tab <- read_csv_chr(path)
  if (is.data.frame(tab) && all(c("section", "key") %in% names(tab))) {
    keys <- unique(tab$key[tab$section == "machines"])
    if (length(keys) > 0) return(keys)
  }
  if (!is.null(con)) {
    r <- tryCatch(read_residents(con), error = function(e) NULL)
    b <- if (is.data.frame(r)) unique(r$block[!is.na(r$block) & nzchar(r$block)])
    if (length(b) > 0) return(sort(b))
  }
  FALLBACK_BLOCKS
}

# Sample residents (survey format).
q_residents <- function(path = PATH_SURVEY) {
  s <- read_csv_chr(path)
  if (is.data.frame(s) && nrow(s) > 0 && "resident_id" %in% names(s)) return(s)
  FALLBACK_RESIDENTS
}

# ---- Pure helpers for blocked windows (tested by test_blocked_helpers()) -----
# A window is a row of data.frame(day, start, end), minutes after midnight of
# its start day. `end` runs past 1440 when the window crosses midnight, so the
# length is always end - start.

hhmm_to_min <- function(x) {
  p <- strsplit(x, ":", fixed = TRUE)
  vapply(p, function(v) as.integer(v[1]) * 60L + as.integer(v[2]), integer(1))
}

min_to_hhmm <- function(m) sprintf("%02d:%02d", m %/% 60L, m %% 60L)

empty_windows <- function() {
  data.frame(day = character(), start = integer(), end = integer(),
             stringsAsFactors = FALSE)
}

# "Mon 23:00-07:00;Wed 22:00-24:00" -> data.frame(day, start, end)
parse_windows <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) {
    return(empty_windows())
  }
  parts <- trimws(strsplit(x, ";", fixed = TRUE)[[1]])
  parts <- parts[grepl("^[A-Za-z]{3} \\d{1,2}:\\d{2}-\\d{1,2}:\\d{2}$", parts)]
  if (length(parts) == 0) return(empty_windows())
  times <- strsplit(sub("^\\S+ ", "", parts), "-", fixed = TRUE)
  start <- hhmm_to_min(vapply(times, `[`, "", 1))
  end <- hhmm_to_min(vapply(times, `[`, "", 2))
  end <- ifelse(end <= start, end + 1440L, end)  # crosses midnight
  data.frame(day = sub(" .*$", "", parts), start = start, end = end,
             stringsAsFactors = FALSE)
}

# Back to the string the data uses: an end past midnight is written as the
# clock time on the next day, an end exactly at midnight as 24:00.
format_windows <- function(w, days = FALLBACK_DAYS) {
  if (nrow(w) == 0) return("")
  w <- w[order(match(w$day, days), w$start), ]
  end <- ifelse(w$end > 1440L, w$end - 1440L, w$end)
  paste0(w$day, " ", min_to_hhmm(w$start), "-", min_to_hhmm(end),
         collapse = ";")
}

# Merge overlapping or touching windows that start on the same day.
merge_windows <- function(w, days = FALLBACK_DAYS) {
  if (nrow(w) == 0) return(empty_windows())
  out <- list()
  for (d in unique(w$day)) {
    x <- w[w$day == d, ]
    x <- x[order(x$start), ]
    cur_s <- x$start[1]
    cur_e <- x$end[1]
    for (i in seq_len(nrow(x))[-1]) {
      if (x$start[i] <= cur_e) {
        cur_e <- max(cur_e, x$end[i])
      } else {
        out[[length(out) + 1]] <- data.frame(day = d, start = cur_s, end = cur_e)
        cur_s <- x$start[i]
        cur_e <- x$end[i]
      }
    }
    out[[length(out) + 1]] <- data.frame(day = d, start = cur_s, end = cur_e)
  }
  res <- do.call(rbind, out)
  res <- res[order(match(res$day, days), res$start), ]
  rownames(res) <- NULL
  res
}

# Named numeric vector: merged blocked hours per start day, for days that
# have any.
hours_per_day <- function(w) {
  m <- merge_windows(w)
  if (nrow(m) == 0) return(setNames(numeric(), character()))
  tot <- tapply((m$end - m$start) / 60, m$day, sum)
  tot[unique(m$day)]
}

# `limit` is one number for every day (the default), or a named vector with a
# limit per day (see day_limits()).
days_over_limit <- function(w, limit = FALLBACK_MAX_BLOCKED_HOURS) {
  h <- hours_per_day(w)
  lim <- vapply(names(h), function(d) limit_on(limit, d), 1)
  h[h > lim]
}

test_blocked_helpers <- function() {
  # parse and format as the data writes them
  w <- parse_windows("Mon 23:00-07:00;Wed 22:00-24:00;Sun 00:00-10:00")
  stopifnot(identical(w$end, c(1860L, 1440L, 600L)))        # 07:00 next day
  stopifnot(format_windows(w) == "Mon 23:00-07:00;Wed 22:00-24:00;Sun 00:00-10:00")
  stopifnot(format_windows(parse_windows("Tue 08:00-10:00;Mon 00:00-07:30")) ==
              "Mon 00:00-07:30;Tue 08:00-10:00")
  stopifnot(nrow(parse_windows("")) == 0, nrow(parse_windows(NA)) == 0)
  # hours, crossing midnight counts to the start day
  h <- hours_per_day(w)
  stopifnot(h[["Mon"]] == 8, h[["Wed"]] == 2, h[["Sun"]] == 10)
  stopifnot(length(days_over_limit(w)) == 0)                 # exactly 10 is fine
  # merging overlaps, including across midnight
  m <- merge_windows(parse_windows("Mon 23:00-07:00;Mon 23:30-24:00"))
  stopifnot(nrow(m) == 1, format_windows(m) == "Mon 23:00-07:00")  # contained
  m <- merge_windows(parse_windows("Mon 23:00-07:00;Mon 06:00-09:00"))
  stopifnot(nrow(m) == 2)  # 06:00-09:00 is Monday morning, a different time
  m <- merge_windows(parse_windows("Mon 22:00-23:30;Mon 23:00-07:00"))
  stopifnot(format_windows(m) == "Mon 22:00-07:00",
            hours_per_day(m)[["Mon"]] == 9)
  stopifnot(nrow(merge_windows(parse_windows("Mon 08:00-09:00;Mon 10:00-11:00"))) == 2)
  # over the limit
  over <- days_over_limit(parse_windows("Mon 22:00-09:00;Tue 08:00-10:00"))
  stopifnot(identical(names(over), "Mon"), over[["Mon"]] == 11)
  over <- days_over_limit(parse_windows("Fri 20:00-24:00;Fri 00:00-07:00"), 10)
  stopifnot(over[["Fri"]] == 11)                             # separate windows add up
  invisible(TRUE)
}

# One blocked window from the "Add window" row. Windows on the same day that
# overlap or touch are merged into one; separate ones are all kept. A window
# is refused (nothing added, a message returned) if From equals To, or if it
# would take that day over its blocked-hours limit (`limit`: one number, or the
# per-day vector from day_limits()). Returns list(windows, message)
# with message NULL when added.
add_window <- function(windows, day, from, to, limit = FALLBACK_MAX_BLOCKED_HOURS,
                       days = FALLBACK_DAYS) {
  s <- hhmm_to_min(from)
  e <- hhmm_to_min(to)
  if (is.na(s) || is.na(e) || !day %in% days) {
    return(list(windows = windows, message = "Please choose a day and times."))
  }
  if (e == s) {
    return(list(windows = windows, message = "'To' must differ from 'From'."))
  }
  if (e < s) e <- e + 1440L  # runs past midnight
  all <- merge_windows(rbind(windows, data.frame(day = day, start = s, end = e,
                                                 stringsAsFactors = FALSE)), days)
  hours <- hours_per_day(all)[[day]]
  lim <- limit_on(limit, day)
  if (hours > lim) {
    return(list(windows = windows, message = paste0(
      "That window was not added: ", day, " would have ", round(hours, 1),
      " h blocked; the limit is ", lim, " h ", limit_reason(limit, day), ".")))
  }
  list(windows = all, message = NULL)
}

# Sleep: one blocked window per chosen day (`days`) with the chosen times. `to`
# earlier than `from` is the normal overnight case (it counts towards the start
# day, like any window past midnight). Windows from earlier sleep (`previous`:
# a c(start, end) pair in minutes for all `days`, or a data.frame of day, start,
# end with each day's own earlier sleep) are replaced on those days only, and an
# identical window already there is not added twice. Returns list(windows, pair,
# sleep), sleep being the data.frame of the windows for `days`; stops if
# from == to.
add_sleep <- function(windows, from, to, days = FALLBACK_DAYS, previous = NULL) {
  s <- hhmm_to_min(from)
  e <- hhmm_to_min(to)
  if (e == s) stop("Sleep 'To' must differ from 'From'.", call. = FALSE)
  if (e < s) e <- e + 1440L
  key <- function(w) paste(w$day, w$start, w$end)
  if (!is.null(previous)) {
    prev <- if (is.data.frame(previous)) previous else
      data.frame(day = days, start = previous[1], end = previous[2])
    prev <- prev[prev$day %in% days, , drop = FALSE]
    windows <- windows[!key(windows) %in% key(prev), , drop = FALSE]
  }
  new <- data.frame(day = days, start = s, end = e, stringsAsFactors = FALSE)
  windows <- rbind(windows, new[!key(new) %in% key(windows), , drop = FALSE])
  rownames(windows) <- NULL
  list(windows = windows, pair = c(s, e), sleep = new)
}

test_sleep_windows <- function() {
  days <- FALLBACK_DAYS
  # default 23:00 to 07:00: one overnight window per day, same as parsing it
  r <- add_sleep(empty_windows(), "23:00", "07:00", days)
  stopifnot(nrow(r$windows) == 7, identical(r$windows$day, days),
            all(r$windows$start == 1380L), all(r$windows$end == 1860L))
  stopifnot(identical(format_windows(r$windows, days),
                      paste0(days, " 23:00-07:00", collapse = ";")))
  stopifnot(identical(r$windows, add_sleep(empty_windows(), "23:00", "07:00", days)$windows))
  stopifnot(all(hours_per_day(r$windows) == 8), length(days_over_limit(r$windows)) == 0)
  # a custom pair on the same day (not overnight)
  r2 <- add_sleep(empty_windows(), "01:00", "06:30", days)
  stopifnot(nrow(r2$windows) == 7, all(r2$windows$start == 60L),
            all(r2$windows$end == 390L), all(hours_per_day(r2$windows) == 5.5))
  # an overnight pair counts to its start day, as for any window
  r3 <- add_sleep(empty_windows(), "22:00", "09:00", days)
  stopifnot(all(r3$windows$end == 1980L), all(hours_per_day(r3$windows) == 11),
            identical(names(days_over_limit(r3$windows)), days))   # over 10 h: warning
  # adding twice: no duplicates, and a new pair replaces the old one
  again <- add_sleep(r$windows, "23:00", "07:00", days, previous = r$pair)
  stopifnot(nrow(again$windows) == 7)
  same_no_prev <- add_sleep(r$windows, "23:00", "07:00", days)   # pair unknown
  stopifnot(nrow(same_no_prev$windows) == 7)
  changed <- add_sleep(r$windows, "00:00", "08:00", days, previous = r$pair)
  stopifnot(nrow(changed$windows) == 7, all(changed$windows$start == 0L),
            all(changed$windows$end == 480L))
  # other windows survive, including one removed-and-kept sleep day left alone
  other <- parse_windows("Mon 09:00-10:30")
  mixed <- add_sleep(rbind(other, r$windows[-3, ]), "23:30", "06:30", days,
                     previous = r$pair)
  stopifnot(nrow(mixed$windows) == 8, "Mon 09:00-10:30" %in%
              strsplit(format_windows(mixed$windows, days), ";")[[1]])
  # a subset of days, with each day's own earlier sleep replaced separately
  wk <- add_sleep(empty_windows(), "22:30", "06:30", days[1:5])
  we <- add_sleep(wk$windows, "00:00", "09:00", days[6:7])
  stopifnot(nrow(we$windows) == 7, identical(we$sleep$day, c("Sat", "Sun")))
  tracked <- rbind(wk$sleep, we$sleep)
  fri <- add_sleep(we$windows, "23:30", "07:30", "Fri", previous = tracked)
  stopifnot(nrow(fri$windows) == 7, fri$windows$start[fri$windows$day == "Fri"] == 1410L,
            sum(fri$windows$start == 1350L) == 4, sum(fri$windows$start == 0L) == 2)
  # bad pair
  stopifnot(inherits(tryCatch(add_sleep(empty_windows(), "07:00", "07:00", days),
                              error = function(e) e), "error"))
  # the saved string parses with the existing helpers, with the same windows
  txt <- format_windows(r3$windows, days)
  back <- parse_windows(txt)
  stopifnot(identical(back$end, r3$windows$end), identical(format_windows(back, days), txt))
  # through the form: dropdown values, the button twice, an individual removal,
  # then Save: the blocked string in the row parses back
  shiny::testServer(mod_questionnaire_server, args = list(con = NULL), {
    session$setInputs(demo = TRUE, user = "R001", login = 1)
    blocked(empty_windows())
    session$setInputs(sleep_days = FALLBACK_DAYS, sleep_from = "23:00",
                      sleep_to = "07:00", b_sleep = 1)
    stopifnot(nrow(blocked()) == 7)
    session$setInputs(sleep_from = "22:30", sleep_to = "06:30", b_sleep = 2)
    stopifnot(nrow(blocked()) == 7, all(blocked()$start == 1350L))   # replaced
    session$setInputs(b_sel = "3", b_remove = 1)                      # remove Wed only
    stopifnot(nrow(blocked()) == 6, !"Wed" %in% blocked()$day)
    session$setInputs(block = "A", course = "DS", program = "quick",
                      pref_days = "Sat", terms = TRUE, save = 1)
    saved <- parse_windows(last_response()$blocked)
    stopifnot(nrow(saved) == 6, all(saved$start == 1350L), all(saved$end == 1830L))
    session$setInputs(sleep_from = "23:00", sleep_to = "07:00", b_sleep = 3)
    stopifnot(nrow(blocked()) == 7, all(blocked()$start == 1380L))    # Wed is back
    # "same for each day" unticked: one Sleep day at a time, each with its own times
    session$setInputs(b_clear = 1)
    session$setInputs(sleep_same = FALSE, sleep_day = "Mon", sleep_from = "22:30",
                      sleep_to = "06:30", b_sleep = 4)
    stopifnot(nrow(blocked()) == 1, blocked()$day == "Mon")
    session$setInputs(sleep_day = "Sat", sleep_from = "00:00", sleep_to = "09:00",
                      b_sleep = 5)
    session$setInputs(sleep_day = "Tue", sleep_from = "22:30", sleep_to = "06:30",
                      b_sleep = 6)
    w <- blocked()
    stopifnot(nrow(w) == 3, w$start[w$day == "Sat"] == 0L,
              w$end[w$day == "Sat"] == 540L, w$start[w$day == "Mon"] == 1350L)
    # change only Monday: replaced, the other days keep their own times
    session$setInputs(sleep_day = "Mon", sleep_from = "23:30", sleep_to = "07:30",
                      b_sleep = 7)
    w <- blocked()
    stopifnot(nrow(w) == 3, w$start[w$day == "Mon"] == 1410L,
              w$start[w$day == "Tue"] == 1350L, w$start[w$day == "Sat"] == 0L)
    # ticked again: all seven days at once, replacing the earlier sleep
    session$setInputs(sleep_same = TRUE, sleep_from = "23:00", sleep_to = "07:00",
                      b_sleep = 8)
    stopifnot(nrow(blocked()) == 7, all(blocked()$start == 1380L))
  })
  invisible(TRUE)
}

test_add_window <- function() {
  days <- FALLBACK_DAYS
  txt <- function(w) format_windows(w, days)
  # two separate windows on the same day are both kept
  r1 <- add_window(empty_windows(), "Mon", "09:00", "11:00")
  r2 <- add_window(r1$windows, "Mon", "14:00", "16:30")
  stopifnot(is.null(r1$message), is.null(r2$message), nrow(r2$windows) == 2,
            txt(r2$windows) == "Mon 09:00-11:00;Mon 14:00-16:30")
  # overlapping windows on the same day merge into one
  r3 <- add_window(r2$windows, "Mon", "10:00", "15:00")
  stopifnot(is.null(r3$message), nrow(r3$windows) == 1,
            txt(r3$windows) == "Mon 09:00-16:30")
  # adjacent (touching) windows merge too; another day is left alone
  r4 <- add_window(r1$windows, "Mon", "11:00", "12:00")
  r5 <- add_window(r4$windows, "Tue", "09:00", "10:00")
  stopifnot(txt(r4$windows) == "Mon 09:00-12:00", nrow(r5$windows) == 2)
  # a window overlapping sleep is merged with it, not dropped
  sl <- add_sleep(empty_windows(), "23:00", "07:00", days)$windows
  r6 <- add_window(sl, "Mon", "22:00", "23:30")
  stopifnot(is.null(r6$message), nrow(r6$windows) == 7,
            "Mon 22:00-07:00" %in% strsplit(txt(r6$windows), ";")[[1]])
  # refused: over the daily limit (sleep 8 h + 3 h), From = To; windows unchanged
  r7 <- add_window(sl, "Wed", "09:00", "12:00", limit = 10)
  stopifnot(identical(r7$windows, sl), grepl("not added", r7$message),
            grepl("Wed", r7$message), grepl("11 h", r7$message))
  r8 <- add_window(sl, "Wed", "09:00", "10:00", limit = 10)   # 9 h: fine
  stopifnot(is.null(r8$message))
  r9 <- add_window(r1$windows, "Mon", "09:00", "09:00")
  stopifnot(identical(r9$windows, r1$windows), grepl("must differ", r9$message))
  # the saved row has overlapping windows merged
  row <- build_response_row("R001", "K", "A", "quick", "DS",
                            parse_windows("Mon 09:00-11:00;Mon 10:00-12:00"), "Sat")
  stopifnot(row$blocked == "Mon 09:00-12:00")
  # through the form
  shiny::testServer(mod_questionnaire_server, args = list(con = NULL), {
    session$setInputs(demo = TRUE, user = "R001", login = 1)
    blocked(empty_windows())
    session$setInputs(b_day = "Mon", b_start = "09:00", b_end = "11:00", b_add = 1)
    session$setInputs(b_day = "Mon", b_start = "14:00", b_end = "16:30", b_add = 2)
    stopifnot(nrow(blocked()) == 2, is.null(output$err_b_add$html))
    session$setInputs(block = "A", course = "DS", program = "quick",
                      pref_days = "Sat", terms = TRUE, save = 1)
    stopifnot(identical(last_response()$blocked, "Mon 09:00-11:00;Mon 14:00-16:30"))
    # overlapping: merged into one
    session$setInputs(b_start = "10:00", b_end = "15:00", b_add = 3)
    stopifnot(nrow(blocked()) == 1, txt(blocked()) == "Mon 09:00-16:30")
    # refused: 7.5 h + 3 h is over 10 h; message under the row, nothing added
    session$setInputs(b_start = "17:00", b_end = "20:00", b_add = 4)
    stopifnot(nrow(blocked()) == 1, grepl("text-danger", output$err_b_add$html),
              grepl("not added", output$err_b_add$html))
    # a later valid window clears the message
    session$setInputs(b_start = "17:00", b_end = "18:00", b_add = 5)
    stopifnot(nrow(blocked()) == 2, is.null(output$err_b_add$html))
  })
  invisible(TRUE)
}

# Per-course daily limits: 10 h on a day with a class, 12 h on a day without.
test_day_limits <- function() {
  days <- FALLBACK_DAYS
  tt <- tempfile(fileext = ".csv")
  writeLines(c("course,day,start,end", "XA,Mon,09:00,10:30", "XA,Mon,14:00,15:00",
               "XA,Wed,09:00,10:30"), tt)
  # the real timetable's columns are what class_days() reads
  stopifnot(all(c("course", "day", "start", "end") %in% names(read_csv_chr(PATH_TIMETABLE))))
  stopifnot(setequal(class_days("XA", tt), c("Mon", "Wed")), is.null(class_days("ZZ", tt)),
            is.null(class_days("XA", "no/such/file.csv")))
  lim <- day_limits("XA", days, 10, 12, tt)
  stopifnot(identical(names(lim), days), lim[["Mon"]] == 10, lim[["Wed"]] == 10,
            lim[["Tue"]] == 12, lim[["Sun"]] == 12)
  # unknown course (or none chosen yet): 10 h on every day
  stopifnot(all(day_limits("ZZ", days, 10, 12, tt) == 10),
            all(day_limits("", days, 10, 12, tt) == 10),
            all(day_limits(NULL, days, 10, 12, tt) == 10))
  # days_over_limit with a per-day limit; a plain number still means every day
  w <- parse_windows("Mon 08:00-18:30;Tue 08:00-20:30;Thu 08:00-20:00")
  stopifnot(identical(names(days_over_limit(w, lim)), c("Mon", "Tue")),   # 10.5, 12.5
            identical(names(days_over_limit(w)), c("Mon", "Tue", "Thu")), # 10 h for all
            length(days_over_limit(w, 13)) == 0)
  # add_window: class day (Mon) allows 10 h but refuses 10.5 h
  a1 <- add_window(empty_windows(), "Mon", "08:00", "18:00", lim, days)
  a2 <- add_window(empty_windows(), "Mon", "08:00", "18:30", lim, days)
  stopifnot(is.null(a1$message), !is.null(a2$message), nrow(a2$windows) == 0)
  # no-class day (Tue) allows 12 h but refuses 12.5 h, naming day, hours and limit
  b1 <- add_window(empty_windows(), "Tue", "08:00", "20:00", lim, days)
  b2 <- add_window(empty_windows(), "Tue", "08:00", "20:30", lim, days)
  stopifnot(is.null(b1$message), !is.null(b2$message),
            identical(b2$message, paste0("That window was not added: Tue would have ",
              "12.5 h blocked; the limit is 12 h on days without classes.")))
  stopifnot(grepl("on days with classes", a2$message), grepl("Mon would have 10.5 h", a2$message))
  # sleep (8 h) plus a 2 h window is fine on a class day, a 2.5 h window is refused
  sl <- add_sleep(empty_windows(), "23:00", "07:00", days)$windows
  stopifnot(is.null(add_window(sl, "Mon", "09:00", "11:00", lim, days)$message),
            !is.null(add_window(sl, "Mon", "09:00", "11:30", lim, days)$message))
  # the same 4 h window: refused on a class day (12 h), accepted on a free day
  stopifnot(!is.null(add_window(sl, "Mon", "09:00", "13:00", lim, days)$message),
            is.null(add_window(sl, "Tue", "09:00", "13:00", lim, days)$message))
  # Save is blocked while a day is over its limit
  ma <- function(blocked, limits) names(missing_answers(
    "B", "B", "XA", "Sat", blocked, FALSE, TRUE, "B", c("XA"), days, limits))
  stopifnot(length(ma(w, 13)) == 0, identical(ma(w, lim), "blocked"),
            length(ma(parse_windows("Mon 08:00-18:00"), lim)) == 0)
  # through the form (real timetable: DS has classes Mon-Fri, none on Sat/Sun;
  # FE has classes on Tue, Thu, Fri and Sat)
  shiny::testServer(mod_questionnaire_server, args = list(con = NULL), {
    session$setInputs(demo = TRUE, user = "R001", login = 1)
    blocked(empty_windows())
    session$setInputs(course = "DS", b_day = "Sat", b_start = "08:00", b_end = "19:00",
                      b_add = 1)                          # 11 h on a no-class day
    stopifnot(nrow(blocked()) == 1, is.null(output$err_b_add$html))
    session$setInputs(b_end = "20:30", b_add = 2)         # 12.5 h: refused
    stopifnot(grepl("Sat would have 12.5 h blocked; the limit is 12 h on days without classes",
                    output$err_b_add$html), nrow(blocked()) == 1)
    session$setInputs(b_day = "Mon", b_start = "08:00", b_end = "18:30", b_add = 3)
    stopifnot(grepl("Mon would have 10.5 h blocked; the limit is 10 h on days with classes",
                    output$err_b_add$html))
    # changing the course re-checks the windows: FE has a class on Sat, so 11 h is over
    stopifnot(is.null(output$b_warning$html))
    session$setInputs(course = "FE")
    stopifnot(grepl("Sat \\(11 h, limit 10 h on days with classes\\)", output$b_warning$html))
    session$setInputs(block = "A", program = "quick", pref_days = "Sat", terms = TRUE,
                      save = 1)
    stopifnot(is.null(last_response()), grepl("Too much blocked time", output$save_result$html))
    # back to a course with no Saturday class: fine again, and it saves
    session$setInputs(course = "DS")
    stopifnot(is.null(output$b_warning$html))
    session$setInputs(save = 2)
    stopifnot(!is.null(last_response()), last_response()$blocked == "Sat 08:00-19:00")
    # an unknown course uses 10 h on every day
    session$setInputs(course = "ZZ")
    stopifnot(grepl("Sat \\(11 h, limit 10 h per day\\)", output$b_warning$html))
  })
  invisible(TRUE)
}

# One-row survey-format data.frame built from form values.
build_response_row <- function(resident_id, name, block, program, course,
                               blocked, pref_days, days = FALLBACK_DAYS) {
  data.frame(resident_id = resident_id, name = name, block = block,
             program = program, course = course,
             blocked = format_windows(merge_windows(blocked, days), days),
             pref_days = paste(pref_days, collapse = ";"),
             pref_band = NO_BAND,
             stringsAsFactors = FALSE)[, SURVEY_COLUMNS]
}

test_response_row <- function() {
  row <- build_response_row("R001", "Kiran Okafor", "A", "quick", "DS",
                            parse_windows("Mon 23:00-07:00"), c("Sat", "Sun"))
  # exactly the columns of CLAUDE.md "Survey data format", in order, no room
  stopifnot(identical(names(row), c("resident_id", "name", "block", "program",
                                    "course", "blocked", "pref_days",
                                    "pref_band")))
  stopifnot(nrow(row) == 1, row$blocked == "Mon 23:00-07:00",
            row$pref_days == "Sat;Sun")
  # the time band is not asked: the saved value is always "No preference"
  stopifnot(identical(row$pref_band, "No preference"), identical(NO_BAND, "No preference"))
  # and the same as the header of the sample survey file
  survey <- read_csv_chr(PATH_SURVEY)
  if (is.data.frame(survey)) stopifnot(identical(names(row), names(survey)))
  invisible(TRUE)
}

# Saves one row with write_responses(), which replaces that resident's earlier
# row. With con = NULL (no database) nothing is written.
# Returns list(saved, row).
save_response <- function(con, row) {
  if (is.null(con)) return(list(saved = FALSE, row = row))
  write_responses(con, row)
  list(saved = TRUE, row = row)
}

# The resident's saved row from the database (survey format), or NULL if they
# have none or there is no database.
saved_response <- function(con, resident_id) {
  if (is.null(con)) return(NULL)
  rows <- tryCatch(read_responses(con), error = function(e) NULL)
  if (is.null(rows)) return(NULL)
  row <- rows[rows$resident_id == resident_id, , drop = FALSE]
  if (nrow(row) == 0) NULL else row
}

# Records the consent first, then saves the answers. If the consent cannot be
# recorded nothing is saved. consent = FALSE (demo login) skips the consent.
# Returns list(ok, saved, stage, message): stage is "consent" or "save" on a
# failure; saved is FALSE when there is no database (a preview only).
submit_response <- function(con, row, consent = TRUE) {
  if (consent && !is.null(con)) {
    r <- tryCatch(record_consent(con, row$resident_id, TERMS_VERSION),
                  error = function(e) e)
    if (inherits(r, "error")) {
      return(list(ok = FALSE, stage = "consent", message = conditionMessage(r)))
    }
  }
  res <- tryCatch(save_response(con, row), error = function(e) e)
  if (inherits(res, "error")) {
    return(list(ok = FALSE, stage = "save", message = conditionMessage(res)))
  }
  list(ok = TRUE, saved = res$saved)
}

# ---- Email -------------------------------------------------------------------

normalize_email <- function(x) tolower(trimws(x))

# Format check only: one address ending in @mse.ac.in, any case. Spaces around
# the address are ignored; spaces inside it are not allowed.
is_valid_email <- function(x) {
  if (length(x) != 1 || is.na(x)) return(FALSE)
  grepl(paste0("^[^@[:space:]]+@", gsub(".", "\\.", EMAIL_DOMAIN, fixed = TRUE), "$"),
        normalize_email(x))
}

test_email_format <- function() {
  ok <- c("aisha.bagga@mse.ac.in", "A.Bagga@MSE.AC.IN", "  a@mse.ac.in  ")
  bad <- c("a@gmail.com", "a@mse.ac.in.evil.com", "a@xmse.ac.in", "a b@mse.ac.in",
           "a@ mse.ac.in", "@mse.ac.in", "a@@mse.ac.in", "a@mse.ac.in;b@mse.ac.in",
           "", "   ", NA, "mse.ac.in", "a")
  stopifnot(all(vapply(ok, is_valid_email, NA)))
  stopifnot(!any(vapply(bad, is_valid_email, NA)))
  stopifnot(normalize_email("  A.Bagga@MSE.ac.in ") == "a.bagga@mse.ac.in")
  invisible(TRUE)
}

resident_choices <- function(residents) {
  setNames(residents$resident_id, paste(residents$resident_id, residents$name))
}

time_choices <- function(from = "00:00", to = "24:00", step = 30) {
  min_to_hhmm(seq(hhmm_to_min(from), hhmm_to_min(to), by = step))
}

# ---- Required answers --------------------------------------------------------

# Every question is required. Returns a named list with one message per missing
# answer (names: block, course, pref_days, blocked, terms); empty
# when everything is complete; no day may be over its blocked-hours limit
# (`limits`, see day_limits()). The block must be one of `blocks` and equal to
# the block in the signed-in resident's row (`resident_block`). Blocked times need at least one window or the
# explicit "I have no blocked times" tick. Used for the red messages, for the
# Save button and again on Save, so a skipped answer is never written.
missing_answers <- function(block, resident_block, course, pref_days,
                            blocked, no_blocked, terms,
                            blocks = FALLBACK_BLOCKS, courses = q_courses(),
                            days = FALLBACK_DAYS,
                            limits = FALLBACK_MAX_BLOCKED_HOURS) {
  one <- function(x) length(x) == 1 && !is.na(x)
  out <- list()
  if (!(one(block) && block %in% blocks)) {
    out$block <- "Please choose your block."
  } else if (length(resident_block) == 1 && !identical(block, resident_block)) {
    out$block <- paste0("Your block in the participant list is ", resident_block,
                        ". Please select ", resident_block, ".")
  }
  if (!(one(course) && course %in% courses)) {
    out$course <- "Please choose your course."
  }
  if (length(pref_days) == 0 || !all(pref_days %in% days)) {
    out$pref_days <- "Please tick at least one preferred day."
  }
  if (nrow(blocked) == 0 && !isTRUE(no_blocked)) {
    out$blocked <- paste("Add at least one blocked-time window, or tick",
                         "\"I have no blocked times\".")
  }
  over <- days_over_limit(blocked, limits)
  if (length(over) > 0) {
    out$blocked <- paste0("Too much blocked time, so this cannot be saved yet: ",
                          over_text(over, limits), ". Shorten or remove windows.")
  }
  if (!isTRUE(terms)) {
    out$terms <- "You must agree to the Terms of Use and the data policy."
  }
  out
}

test_required_fields <- function() {
  w <- parse_windows("Mon 23:00-07:00")
  none <- empty_windows()
  courses <- c("DS", "IMA")
  miss <- function(course = "DS", pref_days = "Sat", blocked = w, no_blocked = FALSE, terms = TRUE,
                   block = "B", resident_block = "B") {
    names(missing_answers(block, resident_block, course, pref_days,
                          blocked, no_blocked, terms, c("A", "B"), courses,
                          FALLBACK_DAYS))
  }
  msg <- function(...) {
    missing_answers(..., blocked = w, no_blocked = FALSE, terms = TRUE,
                    course = "DS", pref_days = "Sat",
                    blocks = c("A", "B"), courses = courses,
                    days = FALLBACK_DAYS)$block
  }
  # block: empty, unknown, or not the resident's own block; a match is fine
  stopifnot(identical(miss(block = ""), "block"),
            identical(miss(block = NULL), "block"),
            identical(miss(block = "Z"), "block"),
            identical(miss(block = "A"), "block"),
            length(miss(block = "A", resident_block = "A")) == 0)
  stopifnot(identical(msg(block = "", resident_block = "B"), "Please choose your block."),
            identical(msg(block = "A", resident_block = "B"),
                      "Your block in the participant list is B. Please select B."))
  # all complete: windows, or the explicit "no blocked times" tick
  stopifnot(length(miss()) == 0, length(miss(blocked = none, no_blocked = TRUE)) == 0)
  # each missing field on its own gives exactly its own message
  stopifnot(identical(miss(course = ""), "course"),
            identical(miss(course = NULL), "course"),
            identical(miss(course = "XX"), "course"),
            identical(miss(pref_days = NULL), "pref_days"),
            identical(miss(pref_days = character(0)), "pref_days"),
            identical(miss(pref_days = "Funday"), "pref_days"),
            identical(miss(terms = FALSE), "terms"),
            identical(miss(terms = NULL), "terms"))
  # blocked times: neither windows nor the tick is missing; the tick alone is enough
  stopifnot(identical(miss(blocked = none), "blocked"),
            identical(miss(blocked = none, no_blocked = FALSE), "blocked"),
            identical(miss(blocked = none, no_blocked = NULL), "blocked"),
            length(miss(blocked = none, no_blocked = TRUE)) == 0)
  # everything missing: all of them are reported
  stopifnot(identical(
    miss(block = "", course = "", pref_days = NULL, blocked = none, terms = FALSE),
    c("block", "course", "pref_days", "blocked", "terms")))
  invisible(TRUE)
}

test_q_blocks <- function() {
  none <- tempfile(fileext = ".csv")
  writeLines(c("section,key,value", "program,quick,30"), none)
  stopifnot(identical(q_blocks(NULL), q_blocks(NULL, PATH_CONFIG)),
            all(c("A", "B", "C", "D") %in% q_blocks(NULL)))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  db_init(con)
  write_residents(con, data.frame(resident_id = c("X1", "X2"), name = c("a", "b"),
                                  email = c("a@mse.ac.in", "b@mse.ac.in"),
                                  block = c("C", "C")))
  # config lists no blocks: the residents table; with no residents, the fallback
  stopifnot(identical(q_blocks(con, none), "C"),
            identical(q_blocks(NULL, none), FALLBACK_BLOCKS))
  invisible(TRUE)
}

# Server-level checks: sign-in against residents, the terms blocking submit, the
# consent flow, and the demo login. Needs shiny, bslib, DBI and RSQLite, and
# R/data_load.R, sourced first.
test_save_flow <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  db_init(con)
  write_residents(con, data.frame(
    resident_id = c("P001", "P002"), name = c("Priya Test", "Dev Test"),
    email = c("test.person@mse.ac.in", "other@mse.ac.in"), block = c("B", "D")))
  form <- list(block = "B", course = "DS", program = "quick",
               pref_days = "Sat", no_blocked = TRUE)
  consents <- function() DBI::dbGetQuery(con, "SELECT * FROM consents")
  shiny::testServer(mod_questionnaire_server, args = list(con = con), {
    # not signed in: nothing is saved
    do.call(session$setInputs, c(form, list(terms = TRUE, save = 1)))
    stopifnot(is.null(last_response()), nrow(read_responses(con)) == 0)
    # wrong domain, and a well-formed email that is not in residents
    session$setInputs(email = "someone@gmail.com", email_go = 1)
    stopifnot(is.null(current_user()), grepl("mse.ac.in", output$email_msg$html))
    session$setInputs(email = "nobody@mse.ac.in", email_go = 2)
    stopifnot(is.null(current_user()), grepl("participant list", output$email_msg$html))
    # listed email (any case, spaces around): id, name and block from residents
    session$setInputs(email = "  Test.Person@MSE.AC.IN ", email_go = 3)
    stopifnot(identical(current_user(), "P001"),
              grepl("Priya Test", output$identity$html))
    # terms not ticked: no consent, no answers
    do.call(session$setInputs, c(form, list(terms = FALSE, save = 2)))
    stopifnot(is.null(last_response()), nrow(read_responses(con)) == 0,
              nrow(consents()) == 0, grepl("Terms of Use", output$save_result$html))
    # ticked: consent recorded with the version, then answers saved
    session$setInputs(terms = TRUE, save = 3)
    stopifnot(has_consented(con, "P001", TERMS_VERSION),
              identical(consents()$terms_version, TERMS_VERSION))
    r <- read_responses(con)
    stopifnot(nrow(r) == 1, identical(names(r), SURVEY_COLUMNS),
              r$resident_id == "P001", r$name == "Priya Test", r$block == "B")
    # saving again replaces the answers and keeps one consent row
    session$setInputs(program = "normal", save = 4)
    r <- read_responses(con)
    stopifnot(nrow(r) == 1, r$program == "normal", nrow(consents()) == 1)
    # consent cannot be recorded (resident removed from the list): not saved
    DBI::dbExecute(con, "DELETE FROM residents WHERE resident_id = 'P001'")
    DBI::dbExecute(con, "DELETE FROM consents")
    session$setInputs(program = "quick", save = 5)
    stopifnot(read_responses(con)$program == "normal", nrow(consents()) == 0,
              grepl("not saved", output$save_result$html))
    # another resident, in block D
    session$setInputs(email = "other@mse.ac.in", email_go = 4)
    stopifnot(identical(current_user(), "P002"))
    session$setInputs(block = "D", terms = TRUE, save = 6)
    r <- read_responses(con)
    stopifnot(nrow(r) == 2, r$block[r$resident_id == "P002"] == "D")
    # demo login: a sample resident still saves a valid row; no consent (not listed)
    session$setInputs(demo = TRUE, user = "R001", login = 1)
    session$setInputs(block = "A", terms = TRUE, save = 7)
    r <- read_responses(con)
    stopifnot(nrow(r) == 3, identical(names(r), SURVEY_COLUMNS),
              r$name[r$resident_id == "R001"] == residents$name[residents$resident_id == "R001"],
              !has_consented(con, "R001", TERMS_VERSION))
  })
  # every answer is required: a skipped one is never saved and shows a red message
  con2 <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  db_init(con2)
  write_residents(con2, data.frame(resident_id = "P001", name = "Priya Test",
                                   email = "test.person@mse.ac.in", block = "B"))
  full <- list(block = "B", course = "DS", program = "quick",
               pref_days = "Sat", no_blocked = TRUE, terms = TRUE)
  shiny::testServer(mod_questionnaire_server, args = list(con = con2), {
    session$setInputs(email = "test.person@mse.ac.in", email_go = 1)
    n <- 0
    try_save <- function(...) {
      n <<- n + 1
      do.call(session$setInputs, c(modifyList(full, list(...)), list(save = n)))
    }
    saved <- function() nrow(read_responses(con2))
    blank <- function(x) is.na(x) || x == ""   # read_responses() gives NA for empty
    # nothing answered yet: every field shows its message
    session$setInputs(block = "", course = "", pref_days = NULL,
                      no_blocked = FALSE, terms = FALSE)
    for (f in c("block", "course", "pref_days", "blocked", "terms")) {
      stopifnot(grepl("text-danger", output[[paste0("err_", f)]]$html))
    }
    # block: empty, then not the block in the participant list (B), then matching
    try_save(block = "")
    stopifnot(saved() == 0, grepl("Please choose your block.", output$save_result$html))
    try_save(block = "A")
    stopifnot(saved() == 0, nrow(DBI::dbGetQuery(con2, "SELECT * FROM consents")) == 0,
              grepl("Your block in the participant list is B. Please select B.",
                    output$save_result$html),
              grepl("Please select B", output$err_block$html))
    # each other field missing on its own: not saved, its own message
    try_save(course = "")
    stopifnot(saved() == 0, grepl("course", output$save_result$html))
    try_save(pref_days = character(0))
    stopifnot(saved() == 0, grepl("preferred day", output$save_result$html))
    try_save(no_blocked = FALSE)
    stopifnot(saved() == 0, grepl("blocked-time", output$save_result$html))
    try_save(terms = FALSE)
    stopifnot(saved() == 0, grepl("Terms of Use", output$save_result$html),
              nrow(DBI::dbGetQuery(con2, "SELECT * FROM consents")) == 0)
    # all complete (with "I have no blocked times"): saved in the survey format
    try_save()
    r <- read_responses(con2)
    stopifnot(nrow(r) == 1, identical(names(r), SURVEY_COLUMNS), r$block == "B",
              blank(r$blocked), r$pref_band == "No preference", r$pref_days == "Sat",
              r$course == "DS", is.null(output$err_course$html) ||
                !grepl("text-danger", output$err_course$html))
    # the time band is never asked: a saved row has "No preference", also when
    # the form is saved again
    try_save(program = "normal")
    stopifnot(identical(read_responses(con2)$pref_band, "No preference"))
    # a window instead of the tick is also complete, and is saved
    session$setInputs(b_day = "Mon", b_start = "09:00", b_end = "10:00", b_add = 1)
    try_save(no_blocked = FALSE)
    stopifnot(read_responses(con2)$blocked == "Mon 09:00-10:00")
  })
  # no database: email sign-in is refused, the demo login previews the row
  shiny::testServer(mod_questionnaire_server, args = list(con = NULL), {
    session$setInputs(email = "test.person@mse.ac.in", email_go = 1)
    stopifnot(is.null(current_user()), grepl("No database", output$email_msg$html))
    session$setInputs(demo = TRUE, user = "R001", login = 1)
    do.call(session$setInputs, c(modifyList(form, list(block = "A")),
                                 list(terms = TRUE, save = 1)))
    stopifnot(!is.null(last_response()), grepl("would be saved", output$save_result$html))
  })
  invisible(TRUE)
}

# ---- Module ------------------------------------------------------------------

mod_questionnaire_ui <- function(id) {
  ns <- NS(id)
  cfg <- q_config()
  prog_labels <- setNames(names(cfg$programs),
                          paste0(tools::toTitleCase(names(cfg$programs)), " (",
                                 cfg$programs, " min)"))
  times <- time_choices()
  div(
    class = "p-2",
    tags$style(HTML("
      .q-days .shiny-options-group { display: flex; flex-wrap: wrap; gap: .25rem 1.25rem; }
      .q-days .form-check-inline, .q-days .checkbox-inline { margin: 0; }
      .q-days.shiny-input-container { width: auto; }
      /* Questionnaire card: one wrapper, so the card's own flex gap does not add
         space for empty outputs; even .75rem spacing between its blocks. */
      .q-form > * { margin: 0; }
      .q-form > * + * { margin-top: .75rem; }
      .q-form > :empty:not(hr) { display: none; }
      .q-form > hr { margin: .75rem 0; }
      .q-form .shiny-input-container { margin-bottom: 0; }
      .q-form .checkbox, .q-form .checkbox-inline,
      .q-form .form-check, .q-form .form-check-inline { margin: 0; }
      .q-form .shiny-html-output > * + * { margin-top: .5rem; }
      .q-form p, .q-form h6 { margin-bottom: 0; }
      .q-form h6 + p { margin-top: .25rem; }
      /* rows of fields: wrap on narrow windows, buttons level with the selects */
      .q-hide-empty:empty { display: none; }
      .q-row .shiny-input-container { margin-bottom: 0; }
      .q-row { display: flex; flex-wrap: wrap; gap: .75rem; align-items: flex-end; }
      .q-row .form-control, .q-row .form-select, .q-row .btn { height: 2.5rem; }
      .q-row .btn { padding-top: 0; padding-bottom: 0; }
    ")),
    layout_columns(
      fill = FALSE, col_widths = c(12, 12),
      card(
        fill = FALSE,
        card_header("Sign in"),
        div(class = "q-row",
            textInput(ns("email"), "Your @mse.ac.in email",
                      placeholder = paste0("name@", EMAIL_DOMAIN), width = "20rem"),
            actionButton(ns("email_go"), "Continue")),
        uiOutput(ns("email_msg"), class = "q-hide-empty"),
        checkboxInput(ns("demo"), "Demo login (testing only)", value = FALSE),
        conditionalPanel(
          "input.demo", ns = ns,
          div(class = "d-flex flex-wrap gap-3 align-items-end",
              selectInput(ns("user"), "Sample resident",
                          choices = resident_choices(q_residents()),
                          selectize = FALSE, width = "16rem"),
              div(class = "mb-3", actionButton(ns("login"), "Use sample resident")))),
        textOutput(ns("who"))
      ),
      conditionalPanel(
        "output.signed_in", ns = ns,
        card(
          fill = FALSE,
          card_header("Questionnaire"),
          div(class = "q-form",
          uiOutput(ns("identity")),
          div(class = "q-row", style = "align-items: flex-start;",
              div(selectInput(ns("block"), "Block",
                              choices = c("Choose your block" = "", q_blocks()),
                              selectize = FALSE, width = "13rem"),
                  uiOutput(ns("err_block"))),
              div(selectInput(ns("course"), "Course",
                              choices = c("Choose your course" = "", q_courses()),
                              selectize = FALSE, width = "12rem"),
                  uiOutput(ns("err_course"))),
              div(selectInput(ns("program"), "Program", choices = prog_labels,
                              selectize = FALSE, width = "11rem"))),
          div(class = "q-days",
              checkboxGroupInput(ns("pref_days"), "Preferred days",
                                 choices = cfg$days, inline = TRUE)),
          uiOutput(ns("err_pref_days")),
          tags$hr(),
          h6("Blocked times (when you are definitely unavailable, including sleep)"),
          p(class = "text-muted small",
            "A window can run past midnight: choose a 'To' earlier than 'From'."),
          div(class = "q-row",
              selectInput(ns("b_day"), "Day", choices = cfg$days,
                          selectize = FALSE, width = "7rem"),
              selectInput(ns("b_start"), "From", choices = times[-length(times)],
                          selected = "09:00", selectize = FALSE, width = "7rem"),
              selectInput(ns("b_end"), "To", choices = times[-1],
                          selected = "17:00", selectize = FALSE, width = "7rem"),
              actionButton(ns("b_add"), "Add window")),
          uiOutput(ns("err_b_add")),
          div(class = "q-row",
              conditionalPanel(
                "!input.sleep_same", ns = ns,
                selectInput(ns("sleep_day"), "Sleep day", choices = cfg$days,
                            selectize = FALSE, width = "7rem")),
              selectInput(ns("sleep_from"), "Sleep from", choices = times[-length(times)],
                          selected = "23:00", selectize = FALSE, width = "8rem"),
              selectInput(ns("sleep_to"), "Sleep to", choices = times[-1],
                          selected = "07:00", selectize = FALSE, width = "8rem"),
              actionButton(ns("b_sleep"), "Add sleep to every day",
                           class = "btn-outline-secondary")),
          checkboxInput(ns("sleep_same"), "Sleep cycle same for each day",
                        value = TRUE),
          uiOutput(ns("b_list")),
          checkboxInput(ns("no_blocked"), "I have no blocked times", value = FALSE),
          uiOutput(ns("err_blocked")),
          uiOutput(ns("b_warning")),
          tags$hr(),
          div(class = "d-flex flex-wrap column-gap-3 align-items-baseline",
              checkboxInput(ns("terms"), CONSENT_LABEL, value = FALSE, width = "auto"),
              actionLink(ns("show_terms"), "Read the Terms of Use")),
          uiOutput(ns("err_terms")),
          actionButton(ns("save"), "Save response", class = "btn-primary",
                       disabled = TRUE),
          uiOutput(ns("save_result"))
          )
        )
      )
    )
  )
}

mod_questionnaire_server <- function(id, con = NULL) {
  moduleServer(id, function(input, output, session) {
    cfg <- q_config()
    blocks <- q_blocks(con)  # config machines, else the residents table
    residents <- q_residents()  # sample residents, for the demo login
    resident <- reactiveVal(NULL)  # one row: resident_id, name, email, block
    current_user <- reactiveVal(NULL)  # resident_id
    login_kind <- reactiveVal(NULL)    # "email" or "demo"
    blocked <- reactiveVal(empty_windows())
    last_response <- reactiveVal(NULL)
    save_msg <- reactiveVal(NULL)
    email_msg <- reactiveVal(NULL)
    prefill_from <- reactiveVal("none")

    # The dropdown is built from the config; if that lists no blocks, from residents.
    if (!identical(blocks, q_blocks(NULL))) {
      updateSelectInput(session, "block",
                        choices = c("Choose your block" = "", blocks), selected = "")
    }

    output$signed_in <- reactive(!is.null(current_user()))
    outputOptions(output, "signed_in", suspendWhenHidden = FALSE)

    output$who <- renderText({
      if (is.null(current_user())) return("Not signed in.")
      paste0("Signed in as ", resident()$name, " (", current_user(), ")",
             if (identical(login_kind(), "demo")) ", demo login", ".",
             if (prefill_from() == "saved") " Form filled from your saved response.")
    })

    # The name comes from the residents row and is not editable. The block is
    # chosen below and must match the block in that row.
    output$identity <- renderUI({
      req(resident())
      div(
          div(class = "text-muted small", "Name"), strong(resident()$name))
    })

    get_col <- function(row, col, default = "") {
      if (col %in% names(row) && !is.na(row[[col]])) row[[col]] else default
    }

    # Fills the form from a one-row survey-format data.frame.
    fill_form <- function(r) {
      if (get_col(r, "course") %in% q_courses()) {
        updateSelectInput(session, "course", selected = r$course)
      }
      if (get_col(r, "program") %in% names(cfg$programs)) {
        updateSelectInput(session, "program", selected = r$program)
      }
      pd <- strsplit(get_col(r, "pref_days"), ";", fixed = TRUE)[[1]]
      updateCheckboxGroupInput(session, "pref_days", selected = pd)
      w <- parse_windows(get_col(r, "blocked"))
      blocked(w)
      updateCheckboxInput(session, "no_blocked", value = nrow(w) == 0)
    }

    sign_out <- function() {
      resident(NULL)
      current_user(NULL)
      login_kind(NULL)
      prefill_from("none")
    }
    email_error <- function(msg) email_msg(div(class = "text-danger small mb-2", msg))

    observeEvent(input$email_go, {
      save_msg(NULL)
      if (!is_valid_email(input$email)) {
        sign_out()
        return(email_error(paste0("Please use your ", EMAIL_DOMAIN,
                                  " email address, for example name@", EMAIL_DOMAIN, ".")))
      }
      if (is.null(con)) {
        sign_out()
        return(email_error("No database connection, so sign-in is not available."))
      }
      found <- tryCatch(find_resident_by_email(con, input$email),
                        error = function(e) NULL)
      if (is.null(found) || nrow(found) == 0) {
        sign_out()
        return(email_error(paste("This email is not on the participant list.",
                                 "Please check the address or contact the project team.")))
      }
      if (!found$block[1] %in% blocks) {
        sign_out()
        return(email_error(paste0("Your block (", found$block[1],
                                  ") is not set up. Please contact the project team.")))
      }
      email_msg(NULL)
      resident(found[1, , drop = FALSE])
      current_user(found$resident_id[1])
      updateSelectInput(session, "block", selected = "")  # chosen again each time
      login_kind("email")
      r <- saved_response(con, found$resident_id[1])
      prefill_from(if (is.null(r)) "none" else "saved")
      if (!is.null(r)) fill_form(r)
      # Already agreed to this version: the box starts ticked.
      agreed <- tryCatch(has_consented(con, found$resident_id[1], TERMS_VERSION),
                         error = function(e) FALSE)
      updateCheckboxInput(session, "terms", value = isTRUE(agreed))
    })
    output$email_msg <- renderUI(email_msg())

    # Demo login (testing only): a sample resident from the fake survey.
    observeEvent(input$login, {
      s <- residents[residents$resident_id == input$user, , drop = FALSE]
      if (nrow(s) == 0 || !get_col(s, "block") %in% blocks) {
        sign_out()
        return(email_error("That sample resident's block is not set up."))
      }
      email_msg(NULL)
      save_msg(NULL)
      resident(data.frame(resident_id = s$resident_id, name = s$name,
                          email = NA_character_, block = s$block,
                          stringsAsFactors = FALSE))
      current_user(s$resident_id)
      updateSelectInput(session, "block", selected = "")
      login_kind("demo")
      r <- saved_response(con, input$user)
      prefill_from(if (is.null(r)) "sample" else "saved")
      fill_form(if (is.null(r)) s else r)
    })

    observeEvent(input$show_terms, {
      showModal(modalDialog(
        title = paste0("Terms of Use (version ", TERMS_VERSION, ")"),
        terms_ui(), size = "l", easyClose = TRUE, footer = modalButton("Close")))
    })

    # Every question is required: one red message under each missing answer,
    # and Save stays disabled until none is left. Save checks again on the server.
    # Blocked hours allowed per day for the chosen course: 10 h on days with a
    # class, 12 h on days without (class time itself is not counted).
    limits <- reactive(day_limits(input$course, cfg$days, cfg$max_blocked_hours))
    unanswered <- reactive({
      missing_answers(input$block, resident()$block, input$course, input$pref_days,
                      blocked(), input$no_blocked, input$terms,
                      blocks, q_courses(), cfg$days, limits())
    })
    field_error <- function(name) {
      renderUI({
        msg <- unanswered()[[name]]
        if (!is.null(msg)) div(class = "text-danger small mb-2", msg)
      })
    }
    output$err_block <- field_error("block")
    output$err_course <- field_error("course")
    output$err_pref_days <- field_error("pref_days")
    output$err_blocked <- field_error("blocked")
    output$err_terms <- field_error("terms")
    observe({
      shiny::updateActionButton(session, "save", disabled = length(unanswered()) > 0)
    })

    # Add window: several windows on one day are kept, overlapping or touching
    # ones are merged, and a refused window says why under the row.
    b_add_msg <- reactiveVal(NULL)
    observeEvent(input$b_add, {
      r <- add_window(blocked(), input$b_day, input$b_start, input$b_end,
                      limits(), cfg$days)
      b_add_msg(r$message)
      if (!is.null(r$message)) return()
      blocked(r$windows)
      updateCheckboxInput(session, "no_blocked", value = FALSE)
    })
    output$err_b_add <- renderUI({
      if (!is.null(b_add_msg())) div(class = "text-danger small", b_add_msg())
    })

    # Sleep day / from / to, like the blocked-time row. "Sleep cycle same for
    # each day" (ticked by default) adds the times to all days at once; unticked,
    # the chosen Sleep day gets its own times. Adding again replaces the sleep
    # added before on those days only, so there are no duplicates. Sleep loaded
    # from a saved response is not known, so only an identical window is not
    # doubled; remove the old windows below if the times changed.
    sleep_added <- reactiveVal(empty_windows())
    sleep_same <- reactive(!identical(input$sleep_same, FALSE))
    sleep_days <- reactive({
      if (sleep_same()) cfg$days else cfg$days[cfg$days %in% input$sleep_day]
    })
    observe({
      updateActionButton(session, "b_sleep", label = if (sleep_same()) {
        "Add sleep to every day"
      } else paste("Add sleep to", if (length(sleep_days())) sleep_days() else "day"))
    })
    observeEvent(input$b_sleep, {
      if (length(sleep_days()) == 0) return()
      r <- tryCatch(add_sleep(blocked(), input$sleep_from, input$sleep_to,
                              sleep_days(), sleep_added()),
                    error = function(e) e)
      if (inherits(r, "error")) {
        showNotification(conditionMessage(r), type = "error")
        return()
      }
      blocked(r$windows)
      old <- sleep_added()
      sleep_added(rbind(old[!old$day %in% sleep_days(), , drop = FALSE], r$sleep))
      updateCheckboxInput(session, "no_blocked", value = FALSE)
    })

    # Every window is listed so that any one can be removed.
    output$b_list <- renderUI({
      w <- blocked()
      if (nrow(w) == 0) return(p(class = "text-muted", "No blocked times yet."))
      shown <- order(match(w$day, cfg$days), w$start)
      labels <- vapply(shown, function(i) format_windows(w[i, ], cfg$days), "")
      tagList(
        p(strong("Blocked (merged): "),
          format_windows(merge_windows(w, cfg$days), cfg$days)),
        div(class = "q-days",
            checkboxGroupInput(session$ns("b_sel"), "Tick windows to remove",
                               choices = setNames(shown, labels), inline = TRUE)),
        actionButton(session$ns("b_remove"), "Remove ticked",
                     class = "btn-sm btn-outline-secondary"),
        actionButton(session$ns("b_clear"), "Clear all",
                     class = "btn-sm btn-outline-danger")
      )
    })
    observeEvent(input$b_remove, {
      drop <- as.integer(input$b_sel)
      if (length(drop) == 0) return()
      w <- blocked()
      blocked(w[-drop, , drop = FALSE])
    })
    observeEvent(input$b_clear, {
      b_add_msg(NULL)
      blocked(empty_windows())
      sleep_added(empty_windows())
    })

    # Re-checked whenever the windows or the course change.
    output$b_warning <- renderUI({
      over <- days_over_limit(blocked(), limits())
      if (length(over) == 0) return(NULL)
      div(class = "alert alert-warning mt-2",
          strong("Warning: "),
          paste0("over the daily blocked-time limit on ", over_text(over, limits()),
                 ". Shorten or remove those windows; the response cannot be ",
                 "saved until every day is within its limit."))
    })

    observeEvent(input$save, {
      refuse <- function(msg) {
        save_msg(div(class = "alert alert-danger mt-2", msg))
        showNotification(msg, type = "error")
      }
      if (is.null(current_user())) {
        return(refuse(paste0("Please enter your ", EMAIL_DOMAIN,
                             " email and press Continue first.")))
      }
      todo <- unanswered()
      if (length(todo) > 0) {
        return(refuse(paste(unlist(todo), collapse = " ")))
      }
      demo <- identical(login_kind(), "demo")
      r <- resident()
      row <- build_response_row(r$resident_id, r$name, input$block, input$program,
                                input$course, blocked(), input$pref_days,
                                cfg$days)
      res <- submit_response(con, row, consent = !demo)
      if (!res$ok) {
        return(refuse(if (res$stage == "consent") {
          paste("Your agreement could not be recorded, so your answers were not saved:",
                res$message)
        } else {
          paste("Your agreement was recorded but your answers could not be saved:",
                res$message)
        }))
      }
      last_response(row)
      save_msg(if (res$saved) {
        div(class = "alert alert-success mt-2", "Saved.",
            if (demo) " (Demo login: no agreement was recorded.)")
      } else {
        tagList(
          div(class = "alert alert-info mt-2",
              "Database not ready: this is what would be saved."),
          tags$pre(paste(capture.output(print(t(row))), collapse = "\n"))
        )
      })
    })
    output$save_result <- renderUI(save_msg())

    # Outputs that are empty are hidden by the CSS above (.q-form > :empty), and
    # Shiny does not draw a hidden output, so it would never appear. Keep
    # drawing them.
    for (o in c("identity", "email_msg", "err_block", "err_course", "err_pref_days",
                "err_blocked", "err_terms", "err_b_add", "b_list", "b_warning",
                "save_result")) {
      outputOptions(output, o, suspendWhenHidden = FALSE)
    }

    last_response
  })
}
