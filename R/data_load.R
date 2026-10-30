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
#   Takes: path to an SQLite file.
#   Returns: a DBI connection (RSQLite) used to store responses and schedules.

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
  open_min <- hhmm_to_min(h$open, paste0(path, ": hours,open"))
  close_min <- hhmm_to_min(h$close, paste0(path, ": hours,close"))
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
    if (hhmm_to_min(timetable$end[i], where) <=
        hhmm_to_min(timetable$start[i], where)) {
      stop(where, ": lecture end must be after its start", call. = FALSE)
    }
  }
  timetable
}

build_free_windows <- function(survey, timetable, config, cycle_start) {
  start_date <- tryCatch(as.Date(cycle_start), error = function(e) as.Date(NA))
  if (length(start_date) != 1 || is.na(start_date)) {
    stop("cycle_start must be one date, YYYY-MM-DD, got: ",
         paste(cycle_start, collapse = ", "), call. = FALSE)
  }

  rule <- function(name, default) {
    x <- config$rules[[name]]
    if (is.null(x)) default else x
  }
  dates <- start_date + seq_len(rule("cycle_days", 10)) - 1
  dates <- dates[date_weekday(dates) %in% config$hours$days]
  margin <- rule("lecture_margin_min", 30)
  open_min <- hhmm_to_min(config$hours$open, "hours,open")
  close_min <- hhmm_to_min(config$hours$close, "hours,close")

  no_lectures <- setdiff(unique(survey$course), timetable$course)
  if (length(no_lectures) > 0) {
    warning("No lectures in the timetable for course(s): ",
            paste(no_lectures, collapse = ", "), call. = FALSE)
  }

  # Weekly busy times per course, in minutes after midnight of their weekday.
  lectures <- data.frame(
    course = timetable$course,
    day = timetable$day,
    start = vapply(timetable$start, hhmm_to_min, numeric(1),
                   where = "timetable", USE.NAMES = FALSE) - margin,
    end = vapply(timetable$end, hhmm_to_min, numeric(1),
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
                 from = min_to_hhmm(free$from),
                 to = min_to_hhmm(free$to))
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

db_connect <- function(path = "laundry.sqlite") {
  NULL
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
hhmm_to_min <- function(x, where) {
  ok <- !is.na(x) && grepl("^[0-9]{2}:[0-9]{2}$", x)
  if (ok) {
    h <- as.integer(substr(x, 1, 2))
    m <- as.integer(substr(x, 4, 5))
    ok <- m < 60 && (h < 24 || (h == 24 && m == 0))
  }
  if (!ok) stop(where, ": time must be HH:MM, got '", x, "'", call. = FALSE)
  h * 60 + m
}

min_to_hhmm <- function(x) {
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
    start <- hhmm_to_min(times[1], where)
    end <- hhmm_to_min(times[2], where)
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
