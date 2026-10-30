# Data loading and storage.
#
# load_survey(path)
#   Takes: path to a survey CSV (see CLAUDE.md "Survey data format").
#   Returns: data.frame with one row per resident, columns resident_id, name,
#            block, program, course, blocked, pref_days, pref_band.
#            Empty blocked, pref_days and pref_band are NA, and the pref
#            columns are NA if the file does not have them.
#
# load_config(path)
#   Takes: path to config.csv (columns section, key, value).
#   Returns: list(machines     = data.frame(block, n_machines),
#                 programs     = data.frame(program, duration_min),
#                 hours        = list(open, close, days, buffer_min),
#                 rules        = named list of the rules,* rows (numbers),
#                 out_of_order = character vector of machine ids,
#                 bands        = data.frame(band, from, to)).
#            hours$days is a character vector and times are "HH:MM".
#            hours$buffer_min is left out if the file does not set it.
#
# load_timetable(path)
#   Takes: path to a timetable CSV (see CLAUDE.md "Timetable format").
#   Returns: data.frame with one row per weekly lecture, columns course, day,
#            start, end.
#
# build_free_windows(survey, timetable, config, cycle_start)
#   Takes: survey      - data.frame from load_survey()
#          timetable   - data.frame from load_timetable()
#          config      - list from load_config()
#          cycle_start - first date of the cycle (Date or "YYYY-MM-DD")
#   Returns: data.frame(resident_id, date, from, to), all character, one row
#            per free stretch of time (see CLAUDE.md "Free windows format").
#            Sorted by resident and time. A person or date with no free time
#            has no rows. Warns about a course with no lectures in the
#            timetable, since its residents count as free at all those times.
#
# db_connect(path)
#   Takes: path to an SQLite file, created if it does not exist.
#   Returns: a DBI connection (RSQLite) with the tables below ready to use.
#
# db_init(con)
#   Takes: con - connection from db_connect().
#   Creates any missing table: bookings, changes, queue (see CLAUDE.md
#   "Database tables") and responses (the columns save_response() in
#   R/mod_questionnaire.R writes). Existing tables and rows are kept.
#   Returns: con, invisibly.
#
# read_responses(con)
#   Returns: data.frame in the same form as load_survey(), empty text as NA.
# write_responses(con, rows)
#   Takes: rows - data.frame with the survey columns. Each row replaces that
#          resident's earlier row, all rows or none.
#
# read_bookings(con, person = NULL)
#   Returns: data.frame(booking_id, person, machine, date, start, end, status,
#            source), everyone's unless person is given.
# add_bookings(con, schedule, source = "optimiser", status = "booked")
#   Takes: schedule - data.frame(person, machine, date, start, end).
#   Returns: the new booking_ids, in row order. All rows or none are added.
# set_booking_status(con, booking_id, status)
#   Stops if there is no such booking.
#
# read_changes(con, person = NULL)
#   Returns: data.frame(person, booking_id, at, kind).
# add_change(con, person, booking_id, kind, at = Sys.time())
#   Takes: kind - "change" or "release". booking_id must exist.
#
# read_queue(con, status = NULL)
#   Returns: data.frame(person, requested_at, status), earliest request first.
# add_to_queue(con, person, requested_at = Sys.time())
#   Does nothing if the person is already waiting.
# set_queue_status(con, person, status)
#   Sets the person's waiting request to "served" or "withdrawn". Stops if
#   they have none.
#
# clear_cycle_plan(con, cycle_start, cycle_days = 10)
#   Deletes the cycle's optimiser bookings, so a new plan can replace them.
#   Stops and deletes nothing if the cycle has been used: any booking in it
#   that is not source = optimiser and status = booked, or that has a row in
#   changes. The table has no published flag, so a published cycle nobody has
#   used yet is cleared too; the caller must not call this for one.
#   Returns: the number of bookings deleted.
#
# last_planned(con, cycle_start)
#   Takes: cycle_start - first date of the cycle (Date or "YYYY-MM-DD").
#   Returns: data.frame(resident_id, date), each person's last booking with
#            source = optimiser (any status) dated before cycle_start, as
#            run_optimiser() takes. People with none have no row.

WEEKDAYS <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

load_survey <- function(path = "data/fake_survey.csv") {
  survey <- read_data_csv(path, c("resident_id", "name", "block", "program",
                                  "course", "blocked"))
  for (col in c("pref_days", "pref_band")) {
    if (!col %in% names(survey)) survey[[col]] <- NA_character_
  }
  survey <- survey[, c("resident_id", "name", "block", "program",
                       "course", "blocked", "pref_days", "pref_band")]
  require_values(survey, c("resident_id", "block", "program", "course"), path)

  repeats <- unique(survey$resident_id[duplicated(survey$resident_id)])
  if (length(repeats) > 0) {
    stop(path, ": resident_id must be unique, repeated: ",
         paste(repeats, collapse = ", "), call. = FALSE)
  }

  for (i in seq_len(nrow(survey))) {
    parse_blocked(survey$blocked[i], survey$resident_id[i])
  }
  survey
}

load_config <- function(path = "data/config.csv") {
  tab <- read_data_csv(path, c("section", "key", "value"))
  require_values(tab, c("section", "key"), path)
  rows <- function(s) tab[tab$section == s, , drop = FALSE]

  as_numbers <- function(r, what) {
    x <- suppressWarnings(as.numeric(r$value))
    if (anyNA(x)) {
      stop(path, ": ", what, " value must be a number, check: ",
           paste(r$key[is.na(x)], collapse = ", "), call. = FALSE)
    }
    x
  }

  machines <- rows("machines")
  programs <- rows("program")
  if (nrow(machines) == 0) stop(path, ": no machines rows", call. = FALSE)
  if (nrow(programs) == 0) stop(path, ": no program rows", call. = FALSE)

  h <- rows("hours")
  h <- setNames(as.list(h$value), h$key)
  missing_hours <- setdiff(c("open", "close", "days"), names(h))
  if (length(missing_hours) > 0) {
    stop(path, ": missing hours row(s): ",
         paste(missing_hours, collapse = ", "), call. = FALSE)
  }
  days <- strsplit(h$days, ";")[[1]]
  if (!all(days %in% WEEKDAYS)) {
    stop(path, ": hours,days must use ", paste(WEEKDAYS, collapse = " "),
         ", got: ", h$days, call. = FALSE)
  }
  open_min <- parse_hhmm(h$open, paste0(path, ": hours,open"))
  close_min <- parse_hhmm(h$close, paste0(path, ": hours,close"))
  if (close_min <= open_min) {
    stop(path, ": hours,close must be after hours,open", call. = FALSE)
  }

  rules <- rows("rules")
  bands <- rows("band")
  band_ok <- grepl("^[0-9]{2}:[0-9]{2}-[0-9]{2}:[0-9]{2}$", bands$value)
  if (!all(band_ok)) {
    stop(path, ": band value must be HH:MM-HH:MM, check: ",
         paste(bands$key[!band_ok], collapse = ", "), call. = FALSE)
  }

  hours <- list(open = h$open, close = h$close, days = days)
  if (!is.null(h$buffer_min)) {
    hours$buffer_min <- as_numbers(
      data.frame(key = "buffer_min", value = h$buffer_min), "hours")
  }

  list(
    machines = data.frame(block = machines$key,
                          n_machines = as.integer(as_numbers(machines, "machines"))),
    programs = data.frame(program = programs$key,
                          duration_min = as_numbers(programs, "program")),
    hours = hours,
    rules = setNames(as.list(as_numbers(rules, "rules")), rules$key),
    out_of_order = rows("out_of_order")$key,
    bands = data.frame(band = bands$key,
                       from = sub("-.*", "", bands$value),
                       to = sub(".*-", "", bands$value))
  )
}

load_timetable <- function(path = "data/fake_timetable.csv") {
  timetable <- read_data_csv(path, c("course", "day", "start", "end"))
  timetable <- timetable[, c("course", "day", "start", "end")]
  require_values(timetable, names(timetable), path)

  for (i in seq_len(nrow(timetable))) {
    where <- paste0(path, " row ", i, " (", timetable$course[i], ")")
    if (!timetable$day[i] %in% WEEKDAYS) {
      stop(where, ": day must be one of ", paste(WEEKDAYS, collapse = " "),
           ", got '", timetable$day[i], "'", call. = FALSE)
    }
    if (parse_hhmm(timetable$end[i], where) <=
        parse_hhmm(timetable$start[i], where)) {
      stop(where, ": lecture end must be after its start", call. = FALSE)
    }
  }
  timetable
}

build_free_windows <- function(survey, timetable, config, cycle_start) {
  start_date <- parse_cycle_start(cycle_start)

  rule <- function(name, default) {
    x <- config$rules[[name]]
    if (is.null(x)) default else x
  }
  dates <- start_date + seq_len(rule("cycle_days", 10)) - 1
  dates <- dates[date_weekday(dates) %in% config$hours$days]
  margin <- rule("lecture_margin_min", 30)
  open_min <- parse_hhmm(config$hours$open, "hours,open")
  close_min <- parse_hhmm(config$hours$close, "hours,close")

  no_lectures <- setdiff(unique(survey$course), timetable$course)
  if (length(no_lectures) > 0) {
    warning("No lectures in the timetable for course(s): ",
            paste(no_lectures, collapse = ", "), call. = FALSE)
  }

  # Weekly busy times per course, in minutes after midnight of their weekday.
  lectures <- data.frame(
    course = timetable$course,
    day = timetable$day,
    start = vapply(timetable$start, parse_hhmm, numeric(1),
                   where = "timetable", USE.NAMES = FALSE) - margin,
    end = vapply(timetable$end, parse_hhmm, numeric(1),
                 where = "timetable", USE.NAMES = FALSE) + margin
  )

  windows <- lapply(seq_len(nrow(survey)), function(i) {
    blocked <- parse_blocked(survey$blocked[i], survey$resident_id[i])
    own <- lectures[lectures$course == survey$course[i], ]
    busy <- rbind(own[, c("day", "start", "end")], blocked)

    per_date <- lapply(dates, function(d) {
      today <- busy[busy$day == date_weekday(d), ]
      free <- subtract_busy(open_min, close_min, today$start, today$end)
      data.frame(resident_id = rep(survey$resident_id[i], nrow(free)),
                 date = rep(format(d), nrow(free)),
                 from = format_hhmm(free$from),
                 to = format_hhmm(free$to))
    })
    do.call(rbind, per_date)
  })

  out <- do.call(rbind, c(
    list(data.frame(resident_id = character(), date = character(),
                    from = character(), to = character())),
    windows
  ))
  rownames(out) <- NULL
  out
}

db_connect <- function(path = "data/laundry.sqlite") {
  if (!dir.exists(dirname(path))) {
    stop("Folder for the database does not exist: ", dirname(path), call. = FALSE)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  # Each app session has its own connection, so wait for a lock, don't fail.
  DBI::dbGetQuery(con, "PRAGMA busy_timeout = 5000")
  db_init(con)
  con
}

db_init <- function(con) {
  statements <- c(
    "CREATE TABLE IF NOT EXISTS responses (
       resident_id TEXT PRIMARY KEY,
       name TEXT, block TEXT, program TEXT, course TEXT,
       blocked TEXT, pref_days TEXT, pref_band TEXT)",
    "CREATE TABLE IF NOT EXISTS bookings (
       booking_id INTEGER PRIMARY KEY AUTOINCREMENT,
       person TEXT NOT NULL, machine TEXT NOT NULL, date TEXT NOT NULL,
       start TEXT NOT NULL, \"end\" TEXT NOT NULL,
       status TEXT NOT NULL CHECK (status IN
         ('booked', 'checked_in', 'released', 'no_show', 'moved')),
       source TEXT NOT NULL CHECK (source IN
         ('optimiser', 'change', 'queue', 'free_now')))",
    "CREATE TABLE IF NOT EXISTS changes (
       person TEXT NOT NULL,
       booking_id INTEGER NOT NULL REFERENCES bookings (booking_id),
       at TEXT NOT NULL,
       kind TEXT NOT NULL CHECK (kind IN ('change', 'release')))",
    "CREATE TABLE IF NOT EXISTS queue (
       person TEXT NOT NULL, requested_at TEXT NOT NULL,
       status TEXT NOT NULL CHECK (status IN
         ('waiting', 'served', 'withdrawn')))",
    "CREATE UNIQUE INDEX IF NOT EXISTS queue_one_waiting
       ON queue (person) WHERE status = 'waiting'"
  )
  for (statement in statements) DBI::dbExecute(con, statement)
  invisible(con)
}

read_responses <- function(con) {
  rows <- DBI::dbGetQuery(con, "SELECT resident_id, name, block, program, course,
    blocked, pref_days, pref_band FROM responses ORDER BY resident_id")
  rows[rows == ""] <- NA_character_
  rows
}

write_responses <- function(con, rows) {
  cols <- c("resident_id", "name", "block", "program", "course", "blocked",
            "pref_days", "pref_band")
  require_columns(rows, cols, "write_responses()")
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "DELETE FROM responses WHERE resident_id = ?",
                   params = list(rows$resident_id))
    DBI::dbAppendTable(con, "responses", rows[, cols])
  })
  invisible(TRUE)
}

read_bookings <- function(con, person = NULL) {
  read_rows(con, "SELECT booking_id, person, machine, date, start, \"end\",
    status, source FROM bookings", "person", person, "date, start, booking_id")
}

add_bookings <- function(con, schedule, source = "optimiser", status = "booked") {
  require_columns(schedule, c("person", "machine", "date", "start", "end"),
                  "add_bookings()")
  DBI::dbWithTransaction(con, {
    vapply(seq_len(nrow(schedule)), function(i) {
      DBI::dbExecute(
        con,
        "INSERT INTO bookings (person, machine, date, start, \"end\", status, source)
         VALUES (?, ?, ?, ?, ?, ?, ?)",
        params = list(schedule$person[i], schedule$machine[i],
                      as.character(schedule$date[i]), schedule$start[i],
                      schedule$end[i], status, source))
      as.integer(DBI::dbGetQuery(con, "SELECT last_insert_rowid()")[[1]])
    }, integer(1))
  })
}

set_booking_status <- function(con, booking_id, status) {
  updated <- DBI::dbExecute(con, "UPDATE bookings SET status = ? WHERE booking_id = ?",
                            params = list(status, booking_id))
  if (updated == 0) stop("No booking with id ", booking_id, call. = FALSE)
  invisible(TRUE)
}

read_changes <- function(con, person = NULL) {
  read_rows(con, "SELECT person, booking_id, at, kind FROM changes", "person",
            person, "at")
}

add_change <- function(con, person, booking_id, kind, at = Sys.time()) {
  DBI::dbExecute(con, "INSERT INTO changes (person, booking_id, at, kind)
                       VALUES (?, ?, ?, ?)",
                 params = list(person, booking_id, time_text(at), kind))
  invisible(TRUE)
}

read_queue <- function(con, status = NULL) {
  read_rows(con, "SELECT person, requested_at, status FROM queue", "status",
            status, "requested_at")
}

add_to_queue <- function(con, person, requested_at = Sys.time()) {
  DBI::dbExecute(con, "INSERT OR IGNORE INTO queue (person, requested_at, status)
                       VALUES (?, ?, 'waiting')",
                 params = list(person, time_text(requested_at)))
  invisible(TRUE)
}

set_queue_status <- function(con, person, status) {
  updated <- DBI::dbExecute(
    con, "UPDATE queue SET status = ? WHERE person = ? AND status = 'waiting'",
    params = list(status, person))
  if (updated == 0) stop(person, " is not waiting in the queue", call. = FALSE)
  invisible(TRUE)
}

last_planned <- function(con, cycle_start) {
  DBI::dbGetQuery(
    con,
    "SELECT person AS resident_id, MAX(date) AS date FROM bookings
     WHERE source = 'optimiser' AND date < ?
     GROUP BY person ORDER BY person",
    params = list(format(parse_cycle_start(cycle_start))))
}

clear_cycle_plan <- function(con, cycle_start, cycle_days = 10) {
  first <- parse_cycle_start(cycle_start)
  dates <- list(format(first), format(first + cycle_days - 1))
  DBI::dbWithTransaction(con, {
    used <- DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) FROM bookings b
       WHERE date BETWEEN ? AND ?
         AND (source <> 'optimiser' OR status <> 'booked' OR EXISTS
              (SELECT 1 FROM changes c WHERE c.booking_id = b.booking_id))",
      params = dates)[[1]]
    if (used > 0) {
      stop("Cycle starting ", format(first), " is already in use (", used,
           " booking(s) changed, checked in or added since planning), ",
           "so its plan cannot be cleared", call. = FALSE)
    }
    DBI::dbExecute(con, "DELETE FROM bookings WHERE date BETWEEN ? AND ?",
                   params = dates)
  })
}

# Runs a SELECT, filtered on one column if value is given.
read_rows <- function(con, select, col, value, order_by) {
  if (is.null(value)) {
    return(DBI::dbGetQuery(con, paste(select, "ORDER BY", order_by)))
  }
  DBI::dbGetQuery(con, paste(select, "WHERE", col, "= ? ORDER BY", order_by),
                  params = list(value))
}

require_columns <- function(data, cols, where) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop(where, ": missing column(s): ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
}

time_text <- function(x) {
  if (inherits(x, "POSIXt")) format(x, "%Y-%m-%d %H:%M:%S") else as.character(x)
}

parse_cycle_start <- function(cycle_start) {
  start_date <- tryCatch(as.Date(cycle_start), error = function(e) as.Date(NA))
  if (length(start_date) != 1 || is.na(start_date)) {
    stop("cycle_start must be one date, YYYY-MM-DD, got: ",
         paste(cycle_start, collapse = ", "), call. = FALSE)
  }
  start_date
}

# Reads a CSV with every column as character and empty cells as NA.
read_data_csv <- function(path, required) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  data <- as.data.frame(readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = "",
    show_col_types = FALSE
  ))
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop(path, ": missing column(s): ", paste(missing_cols, collapse = ", "),
         ". Found: ", paste(names(data), collapse = ", "), call. = FALSE)
  }
  data
}

require_values <- function(data, cols, path) {
  for (col in cols) {
    empty <- which(is.na(data[[col]]))
    if (length(empty) > 0) {
      stop(path, ": empty ", col, " in row(s): ",
           paste(empty, collapse = ", "), call. = FALSE)
    }
  }
}

# English weekday of a date, whatever the computer's language is.
date_weekday <- function(dates) {
  WEEKDAYS[(as.integer(format(dates, "%u")))]
}

# "HH:MM" to minutes after midnight, 00:00 to 24:00.
parse_hhmm <- function(x, where) {
  ok <- !is.na(x) && grepl("^[0-9]{2}:[0-9]{2}$", x)
  if (ok) {
    h <- as.integer(substr(x, 1, 2))
    m <- as.integer(substr(x, 4, 5))
    ok <- m < 60 && (h < 24 || (h == 24 && m == 0))
  }
  if (!ok) stop(where, ": time must be HH:MM, got '", x, "'", call. = FALSE)
  h * 60 + m
}

format_hhmm <- function(x) {
  sprintf("%02d:%02d", as.integer(x %/% 60), as.integer(x %% 60))
}

# One person's blocked text to data.frame(day, start, end) in minutes after
# midnight of that weekday. A block crossing midnight becomes two rows: the
# part on its own day and the part on the next day (Sun runs into Mon).
parse_blocked <- function(blocked, resident_id) {
  out <- data.frame(day = character(), start = numeric(), end = numeric())
  if (is.na(blocked) || trimws(blocked) == "") return(out)

  for (item in trimws(strsplit(blocked, ";")[[1]])) {
    where <- paste0("Resident ", resident_id, " blocked time '", item, "'")
    if (!grepl("^[A-Za-z]+ [^-]+-[^-]+$", item)) {
      stop(where, ": must look like Mon 23:00-07:00", call. = FALSE)
    }
    day <- sub(" .*", "", item)
    if (!day %in% WEEKDAYS) {
      stop(where, ": day must be one of ", paste(WEEKDAYS, collapse = " "),
           call. = FALSE)
    }
    times <- strsplit(sub("^[^ ]+ ", "", item), "-")[[1]]
    start <- parse_hhmm(times[1], where)
    end <- parse_hhmm(times[2], where)
    if (start == 1440 || start == end) {
      stop(where, ": start must be before 24:00 and differ from the end",
           call. = FALSE)
    }

    if (end > start) {
      out[nrow(out) + 1, ] <- list(day, start, end)
    } else {
      next_day <- WEEKDAYS[match(day, WEEKDAYS) %% 7 + 1]
      out[nrow(out) + 1, ] <- list(day, start, 1440)
      if (end > 0) out[nrow(out) + 1, ] <- list(next_day, 0, end)
    }
  }
  out
}

# The parts of open_min to close_min not covered by any busy period.
subtract_busy <- function(open_min, close_min, busy_start, busy_end) {
  free <- data.frame(from = numeric(), to = numeric())
  at <- open_min
  for (i in order(busy_start)) {
    if (busy_start[i] > at && at < close_min) {
      free[nrow(free) + 1, ] <- list(at, min(busy_start[i], close_min))
    }
    at <- max(at, busy_end[i])
  }
  if (at < close_min) free[nrow(free) + 1, ] <- list(at, close_min)
  free
}
