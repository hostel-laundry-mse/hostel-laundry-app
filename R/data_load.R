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
#   Creates any missing table: bookings, changes, queue, residents, consents,
#   feedback (see CLAUDE.md "Database tables") and responses (the columns
#   save_response() in R/mod_questionnaire.R writes). An older bookings table
#   is rebuilt to add checked_in_at, finished_at and status finished, keeping
#   its rows and booking_ids. Existing tables and rows are kept.
#   Returns: con, invisibly.
#
# load_participants(path)
#   Takes: path to a CSV with columns resident_id, name, email, block.
#   Returns: data.frame with those columns, emails in lower case. Stops unless
#            every email ends in @mse.ac.in and IDs and emails are unique.
# write_residents(con, df)
#   Replaces the whole login list with df, checked as above: anyone left out
#   can no longer log in. Their consents, bookings and feedback are kept.
# read_residents(con)
#   Returns: data.frame(resident_id, name, email, block).
# find_resident_by_email(con, email)
#   Returns: that person's row, any letter case, or zero rows if not listed.
#
# record_consent(con, resident_id, terms_version, at = Sys.time())
#   Stops if the resident is not listed. Agreeing again to the same version
#   keeps the first agreed_at.
# has_consented(con, resident_id, terms_version)
#   Returns: TRUE or FALSE for exactly that version.
#
# check_in(con, booking_id, at = Sys.time(), config = load_config())
#   Booking must be booked and at within slot start to start +
#   checkin_grace_min, both included (default 15). Sets status checked_in and
#   checked_in_at. Times are this computer's local time. Stops otherwise.
# check_out(con, booking_id, at = Sys.time())
#   Booking must be checked_in, and at not before checked_in_at. Sets status
#   finished and finished_at. Stops otherwise.
#
# add_feedback(con, booking_id, rating, comment = NA, at = Sys.time())
#   Takes: rating - whole number 1 to 5; comment - optional text.
#   Booking must be finished, and have no feedback yet. person comes from the
#   booking. Returns: the new feedback_id.
# read_feedback(con)
#   Returns: data.frame(feedback_id, booking_id, person, rating, comment, at).
#
# export_responses(con, path)
#   Writes responses, residents and consents to one .rds file to share between
#   laptops. Contains names and emails, so do not commit it. Returns: path,
#   invisibly.
# import_responses(con, paths, prefer = NULL)
#   Takes: paths  - one or more files from export_responses()
#          prefer - who wins a conflict: a path from paths, or "database" for
#                   this database's own rows. Several means first listed wins.
#   Merges them into this database, all or nothing. A resident with different
#   answers (or different residents details) in this database and the files
#   stops the import with an error of class import_conflict, listing them in
#   its $conflicts, unless prefer names a source that has them. Identical
#   duplicates are fine and importing again changes nothing. Consents keep the
#   earliest agreed_at. Bookings are never exported or imported. Residents in
#   this database but not in the files are kept.
#   Returns: list(responses, residents, consents), each counts of added,
#            updated and unchanged, resolved (conflicts settled by prefer) and
#            responses_not_in_residents (resident_ids).
#
# read_responses(con)
#   Returns: data.frame in the same form as load_survey(), empty text as NA.
# write_responses(con, rows)
#   Takes: rows - data.frame with the survey columns. Each row replaces that
#          resident's earlier row, all rows or none.
#
# read_bookings(con, person = NULL)
#   Returns: data.frame(booking_id, person, machine, date, start, end, status,
#            source, checked_in_at, finished_at), everyone's unless person is
#            given.
# add_bookings(con, schedule, source = "optimiser", status = "booked")
#   Takes: schedule - data.frame(person, machine, date, start, end).
#   Returns: the new booking_ids, in row order. All rows or none are added.
# set_booking_status(con, booking_id, status)
#   Stops if there is no such booking. Status finished is refused: use
#   check_out().
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
    bookings_table_sql("bookings"),
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
       ON queue (person) WHERE status = 'waiting'",
    "CREATE TABLE IF NOT EXISTS residents (
       resident_id TEXT PRIMARY KEY,
       name TEXT NOT NULL,
       email TEXT NOT NULL UNIQUE COLLATE NOCASE,
       block TEXT NOT NULL)",
    "CREATE TABLE IF NOT EXISTS consents (
       resident_id TEXT NOT NULL, agreed_at TEXT NOT NULL,
       terms_version TEXT NOT NULL,
       PRIMARY KEY (resident_id, terms_version))",
    "CREATE TABLE IF NOT EXISTS feedback (
       feedback_id INTEGER PRIMARY KEY AUTOINCREMENT,
       booking_id INTEGER NOT NULL UNIQUE REFERENCES bookings (booking_id),
       person TEXT NOT NULL,
       rating INTEGER NOT NULL CHECK (rating BETWEEN 1 AND 5),
       comment TEXT,
       at TEXT NOT NULL)"
  )
  for (statement in statements) DBI::dbExecute(con, statement)
  upgrade_bookings_table(con)
  invisible(con)
}

bookings_table_sql <- function(name) {
  paste0(
    "CREATE TABLE IF NOT EXISTS ", name, " (
       booking_id INTEGER PRIMARY KEY AUTOINCREMENT,
       person TEXT NOT NULL, machine TEXT NOT NULL, date TEXT NOT NULL,
       start TEXT NOT NULL, \"end\" TEXT NOT NULL,
       status TEXT NOT NULL CHECK (status IN
         ('booked', 'checked_in', 'released', 'no_show', 'moved', 'finished')),
       source TEXT NOT NULL CHECK (source IN
         ('optimiser', 'change', 'queue', 'free_now')),
       checked_in_at TEXT,
       finished_at TEXT)"
  )
}

bookings_table_is_current <- function(con) {
  sql <- DBI::dbGetQuery(
    con, "SELECT sql FROM sqlite_master WHERE type = 'table' AND name = 'bookings'")$sql
  grepl("'finished'", sql, fixed = TRUE) &&
    all(c("checked_in_at", "finished_at") %in% DBI::dbListFields(con, "bookings"))
}

# SQLite cannot change a CHECK, so an older bookings table is copied into a new
# one. Foreign keys are off during the swap so changes and feedback keep their
# rows, and the ID counter is kept so an old booking_id is never reused.
upgrade_bookings_table <- function(con) {
  if (bookings_table_is_current(con)) return(invisible(FALSE))

  foreign_keys <- DBI::dbGetQuery(con, "PRAGMA foreign_keys")[[1]]
  DBI::dbExecute(con, "PRAGMA foreign_keys = OFF")
  on.exit(DBI::dbExecute(con, paste("PRAGMA foreign_keys =", foreign_keys)))

  DBI::dbExecute(con, "BEGIN IMMEDIATE")
  tryCatch({
    # Another session may have upgraded it while this one waited for the lock.
    if (!bookings_table_is_current(con)) {
      counter <- DBI::dbGetQuery(
        con, "SELECT seq FROM sqlite_sequence WHERE name = 'bookings'")$seq
      DBI::dbExecute(con, bookings_table_sql("bookings_upgrade"))
      cols <- DBI::dbQuoteIdentifier(con, DBI::dbListFields(con, "bookings"))
      cols <- paste(cols, collapse = ", ")
      DBI::dbExecute(con, paste0("INSERT INTO bookings_upgrade (", cols, ") SELECT ",
                                 cols, " FROM bookings"))
      DBI::dbExecute(con, "DROP TABLE bookings")
      DBI::dbExecute(con, "ALTER TABLE bookings_upgrade RENAME TO bookings")
      DBI::dbExecute(con, "DELETE FROM sqlite_sequence WHERE name = 'bookings'")
      if (length(counter) > 0) {
        DBI::dbExecute(con, "INSERT INTO sqlite_sequence (name, seq) VALUES ('bookings', ?)",
                       params = list(counter))
      }
    }
    DBI::dbExecute(con, "COMMIT")
  }, error = function(e) {
    DBI::dbExecute(con, "ROLLBACK")
    stop("Could not upgrade the bookings table, nothing changed: ",
         conditionMessage(e), call. = FALSE)
  })
  invisible(TRUE)
}

load_participants <- function(path = "data/participants.csv") {
  cols <- c("resident_id", "name", "email", "block")
  participants <- read_data_csv(path, cols)
  check_residents(participants[, cols], path)
}

# Trims and lower-cases, then stops on anything not fit to be a login list.
check_residents <- function(df, where) {
  cols <- c("resident_id", "name", "email", "block")
  require_columns(df, cols, where)
  df <- df[, cols]
  df[] <- lapply(df, function(x) trimws(as.character(x)))
  df$email <- tolower(df$email)
  if (nrow(df) == 0) stop(where, ": no residents", call. = FALSE)
  df[!is.na(df) & df == ""] <- NA_character_
  require_values(df, cols, where)

  bad <- which(!grepl("^[^@[:space:]]+@mse\\.ac\\.in$", df$email))
  if (length(bad) > 0) {
    stop(where, ": email must end in @mse.ac.in, check row(s) ",
         paste0(bad, " (", df$email[bad], ")", collapse = ", "), call. = FALSE)
  }
  for (col in c("resident_id", "email")) {
    repeats <- unique(df[[col]][duplicated(df[[col]])])
    if (length(repeats) > 0) {
      stop(where, ": ", col, " must be unique, repeated: ",
           paste(repeats, collapse = ", "), call. = FALSE)
    }
  }
  df
}

write_residents <- function(con, df) {
  df <- check_residents(df, "write_residents()")
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "DELETE FROM residents")
    DBI::dbAppendTable(con, "residents", df)
  })
  invisible(TRUE)
}

read_residents <- function(con) {
  DBI::dbGetQuery(con, "SELECT resident_id, name, email, block FROM residents
                        ORDER BY resident_id")
}

find_resident_by_email <- function(con, email) {
  DBI::dbGetQuery(con, "SELECT resident_id, name, email, block FROM residents
                        WHERE email = ?",
                  params = list(tolower(trimws(email))))
}

record_consent <- function(con, resident_id, terms_version, at = Sys.time()) {
  listed <- DBI::dbGetQuery(con, "SELECT COUNT(*) FROM residents WHERE resident_id = ?",
                            params = list(resident_id))[[1]]
  if (listed == 0) stop("Resident ", resident_id, " is not listed", call. = FALSE)
  DBI::dbExecute(con, "INSERT OR IGNORE INTO consents (resident_id, agreed_at, terms_version)
                       VALUES (?, ?, ?)",
                 params = list(resident_id, time_text(at), terms_version))
  invisible(TRUE)
}

has_consented <- function(con, resident_id, terms_version) {
  DBI::dbGetQuery(con, "SELECT COUNT(*) FROM consents
                        WHERE resident_id = ? AND terms_version = ?",
                  params = list(resident_id, terms_version))[[1]] > 0
}

check_in <- function(con, booking_id, at = Sys.time(), config = load_config()) {
  at <- as_local_time(at)
  booking <- booking_row(con, booking_id)
  if (booking$status != "booked") {
    stop("Booking ", booking_id, " cannot be checked in: its status is ",
         booking$status, call. = FALSE)
  }
  grace <- config$rules$checkin_grace_min
  if (is.null(grace)) grace <- 15
  opens <- as_local_time(paste(booking$date, booking$start))
  closes <- opens + grace * 60
  if (at < opens || at > closes) {
    stop("Booking ", booking_id, " can only be checked in from ",
         format(opens, "%Y-%m-%d %H:%M"), " to ", format(closes, "%H:%M"),
         call. = FALSE)
  }
  DBI::dbExecute(con, "UPDATE bookings SET status = 'checked_in', checked_in_at = ?
                       WHERE booking_id = ?",
                 params = list(time_text(at), booking_id))
  invisible(TRUE)
}

check_out <- function(con, booking_id, at = Sys.time()) {
  at <- as_local_time(at)
  booking <- booking_row(con, booking_id)
  if (booking$status != "checked_in") {
    stop("Booking ", booking_id, " cannot be checked out: its status is ",
         booking$status, call. = FALSE)
  }
  # Bookings checked in through set_booking_status() have no time to compare.
  if (!is.na(booking$checked_in_at) && at < as_local_time(booking$checked_in_at)) {
    stop("Booking ", booking_id, " cannot be checked out before its check-in at ",
         booking$checked_in_at, call. = FALSE)
  }
  DBI::dbExecute(con, "UPDATE bookings SET status = 'finished', finished_at = ?
                       WHERE booking_id = ?",
                 params = list(time_text(at), booking_id))
  invisible(TRUE)
}

add_feedback <- function(con, booking_id, rating, comment = NA, at = Sys.time()) {
  booking <- booking_row(con, booking_id)
  if (booking$status != "finished") {
    stop("Booking ", booking_id, " has no feedback yet because it is not finished: ",
         "its status is ", booking$status, call. = FALSE)
  }
  number <- suppressWarnings(as.numeric(rating))
  if (length(number) != 1 || is.na(number) || number != round(number) ||
      number < 1 || number > 5) {
    stop("Rating must be a whole number from 1 to 5, got: ",
         paste(rating, collapse = ", "), call. = FALSE)
  }
  given <- DBI::dbGetQuery(con, "SELECT COUNT(*) FROM feedback WHERE booking_id = ?",
                           params = list(booking_id))[[1]]
  if (given > 0) {
    stop("Booking ", booking_id, " already has feedback", call. = FALSE)
  }
  if (is.null(comment) || length(comment) == 0 || is.na(comment) ||
      trimws(comment) == "") {
    comment <- NA_character_
  }
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "INSERT INTO feedback (booking_id, person, rating, comment, at)
                         VALUES (?, ?, ?, ?, ?)",
                   params = list(booking_id, booking$person, as.integer(number),
                                 as.character(comment), time_text(at)))
    as.integer(DBI::dbGetQuery(con, "SELECT last_insert_rowid()")[[1]])
  })
}

read_feedback <- function(con) {
  DBI::dbGetQuery(con, "SELECT feedback_id, booking_id, person, rating, comment, at
                        FROM feedback ORDER BY feedback_id")
}

booking_row <- function(con, booking_id) {
  booking <- DBI::dbGetQuery(con, "SELECT * FROM bookings WHERE booking_id = ?",
                             params = list(booking_id))
  if (nrow(booking) == 0) stop("No booking with id ", booking_id, call. = FALSE)
  booking
}

# POSIXct from a time or "YYYY-MM-DD HH:MM[:SS]" text, in local time.
as_local_time <- function(x) {
  if (inherits(x, "POSIXt")) return(as.POSIXct(x))
  time <- as.POSIXct(as.character(x), format = "%Y-%m-%d %H:%M:%S")
  if (is.na(time)) time <- as.POSIXct(as.character(x), format = "%Y-%m-%d %H:%M")
  if (is.na(time)) stop("Cannot read the time '", x, "'", call. = FALSE)
  time
}

export_tables <- function(con) {
  list(responses = read_responses(con),
       residents = read_residents(con),
       consents = DBI::dbGetQuery(
         con, "SELECT resident_id, agreed_at, terms_version FROM consents"))
}

export_responses <- function(con, path) {
  if (!grepl("\\.rds$", path, ignore.case = TRUE)) {
    stop("Export file must end in .rds, got: ", path, call. = FALSE)
  }
  saveRDS(c(list(format = "hostel_laundry_export", exported_at = time_text(Sys.time())),
            export_tables(con)), path)
  invisible(path)
}

# Only the three shared tables are taken from a file, whatever else it holds.
read_export <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  data <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(data) || !identical(data$format, "hostel_laundry_export")) {
    stop(path, " is not a file from export_responses()", call. = FALSE)
  }
  required <- list(
    responses = c("resident_id", "name", "block", "program", "course", "blocked",
                  "pref_days", "pref_band"),
    residents = c("resident_id", "name", "email", "block"),
    consents = c("resident_id", "agreed_at", "terms_version")
  )
  lapply(setNames(names(required), names(required)), function(table) {
    require_columns(data[[table]], required[[table]], paste0(path, ", ", table))
    data[[table]][, required[[table]], drop = FALSE]
  })
}

# Finds one winning row per resident across sources, a named list of
# data.frames. Returns list(rows, conflicts, resolved).
merge_import_rows <- function(sources, table, cols, prefer) {
  all_rows <- do.call(rbind, lapply(names(sources), function(label) {
    rows <- sources[[label]]
    if (nrow(rows) == 0) return(NULL)
    cbind(rows, source = label, stringsAsFactors = FALSE)
  }))
  no_rows <- list(rows = sources[[1]][0, cols, drop = FALSE], conflicts = NULL,
                  resolved = NULL)
  if (is.null(all_rows)) return(no_rows)
  all_rows[cols][is.na(all_rows[cols])] <- ""

  winners <- list()
  conflicts <- list()
  resolved <- list()
  for (group in split(all_rows, all_rows$resident_id)) {
    differing <- cols[vapply(cols, function(col) length(unique(group[[col]])) > 1, TRUE)]
    if (length(differing) == 0) {
      winners[[length(winners) + 1]] <- group[1, cols]
      next
    }
    report <- data.frame(table = table, resident_id = group$resident_id[1],
                         columns = paste(differing, collapse = ", "),
                         sources = paste(group$source, collapse = " | "))
    chosen <- intersect(prefer, group$source)
    if (length(chosen) == 0) {
      conflicts[[length(conflicts) + 1]] <- report
    } else {
      winners[[length(winners) + 1]] <- group[group$source == chosen[1], cols][1, ]
      report$winner <- chosen[1]
      resolved[[length(resolved) + 1]] <- report
    }
  }
  rows <- do.call(rbind, winners)
  if (is.null(rows)) rows <- no_rows$rows
  rows[rows == ""] <- NA_character_
  list(rows = rows, conflicts = do.call(rbind, conflicts),
       resolved = do.call(rbind, resolved))
}

# Compares winners with this database. Returns list(new, changed, counts).
import_changes <- function(rows, current, cols) {
  key <- function(df) {
    df[is.na(df)] <- ""
    do.call(paste, c(df[cols], sep = "\r"))
  }
  is_new <- !rows$resident_id %in% current$resident_id
  is_changed <- !is_new & !key(rows) %in% key(current)
  list(rows = rows[is_new | is_changed, cols, drop = FALSE],
       counts = c(added = sum(is_new), updated = sum(is_changed),
                  unchanged = sum(!is_new & !is_changed)))
}

import_responses <- function(con, paths, prefer = NULL) {
  if (length(paths) == 0) stop("No files to import", call. = FALSE)
  real_path <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  paths <- paths[!duplicated(real_path(paths))]
  files <- setNames(lapply(paths, read_export), paths)

  prefer <- as.character(prefer)
  unknown <- prefer[prefer != "database" & !real_path(prefer) %in% real_path(paths)]
  if (length(unknown) > 0) {
    stop("prefer must be one of the files being imported or \"database\", got: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  labels <- paths[match(real_path(prefer), real_path(paths))]
  prefer <- ifelse(prefer == "database", "database", labels)

  current <- export_tables(con)
  from <- function(table) {
    c(list(database = current[[table]]), lapply(files, `[[`, table))
  }
  response_cols <- names(current$responses)
  resident_cols <- names(current$residents)
  responses <- merge_import_rows(from("responses"), "responses", response_cols, prefer)
  residents <- merge_import_rows(from("residents"), "residents", resident_cols, prefer)

  conflicts <- rbind(responses$conflicts, residents$conflicts)
  if (!is.null(conflicts)) {
    lines <- paste0("  ", conflicts$table, " ", conflicts$resident_id, ": ",
                    conflicts$columns, " differ between ", conflicts$sources)
    stop(structure(
      class = c("import_conflict", "error", "condition"),
      list(message = paste0(
        "Different answers for the same resident, nothing imported:\n",
        paste(lines, collapse = "\n"),
        "\nChoose who wins with prefer = \"<path>\" or prefer = \"database\"."),
        call = NULL, conflicts = conflicts)
    ))
  }

  merged <- residents$rows
  if (nrow(merged) > 0) {
    repeats <- unique(merged$email[duplicated(merged$email)])
    if (length(repeats) > 0) {
      stop("Nothing imported: the same email is on different resident IDs: ",
           paste(repeats, collapse = ", "), call. = FALSE)
    }
  }

  new_responses <- import_changes(responses$rows, current$responses, response_cols)
  new_residents <- import_changes(residents$rows, current$residents, resident_cols)

  consents <- do.call(rbind, from("consents"))
  bad_times <- consents$agreed_at[
    !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$", consents$agreed_at)]
  if (length(bad_times) > 0) {
    stop("Nothing imported: cannot read consent time(s): ",
         paste(unique(bad_times), collapse = ", "), call. = FALSE)
  }
  earliest <- consents[order(consents$agreed_at), ]
  earliest <- earliest[!duplicated(earliest[, c("resident_id", "terms_version")]), ]
  now_held <- merge(earliest, current$consents, by = c("resident_id", "terms_version"),
                    all.x = TRUE, suffixes = c("", "_now"))
  consent_new <- now_held[is.na(now_held$agreed_at_now), ]
  consent_earlier <- now_held[!is.na(now_held$agreed_at_now) &
                                now_held$agreed_at < now_held$agreed_at_now, ]

  DBI::dbWithTransaction(con, {
    for (id in new_responses$rows$resident_id) {
      DBI::dbExecute(con, "DELETE FROM responses WHERE resident_id = ?", params = list(id))
    }
    if (nrow(new_responses$rows) > 0) {
      DBI::dbAppendTable(con, "responses", new_responses$rows)
    }
    for (id in new_residents$rows$resident_id) {
      DBI::dbExecute(con, "DELETE FROM residents WHERE resident_id = ?", params = list(id))
    }
    if (nrow(new_residents$rows) > 0) {
      DBI::dbAppendTable(con, "residents", new_residents$rows)
    }
    if (nrow(consent_new) > 0) {
      DBI::dbAppendTable(con, "consents",
                         consent_new[, c("resident_id", "agreed_at", "terms_version")])
    }
    for (i in seq_len(nrow(consent_earlier))) {
      DBI::dbExecute(
        con, "UPDATE consents SET agreed_at = ? WHERE resident_id = ? AND terms_version = ?",
        params = list(consent_earlier$agreed_at[i], consent_earlier$resident_id[i],
                      consent_earlier$terms_version[i]))
    }
  })

  held <- read_residents(con)$resident_id
  list(
    responses = new_responses$counts,
    residents = new_residents$counts,
    consents = c(added = nrow(consent_new), updated = nrow(consent_earlier),
                 unchanged = nrow(now_held) - nrow(consent_new) - nrow(consent_earlier)),
    resolved = rbind(responses$resolved, residents$resolved),
    responses_not_in_residents = setdiff(read_responses(con)$resident_id, held)
  )
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
    status, source, checked_in_at, finished_at FROM bookings", "person", person,
    "date, start, booking_id")
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
  if (identical(status, "finished")) {
    stop("Use check_out() to finish booking ", booking_id, call. = FALSE)
  }
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
