# Simulated Block C residents for the pre-pilot. Files in R/ are sourced when
# the app starts, so this file only defines functions: nothing runs or is
# written until simulate_blockC() is called.
#
# simulate_blockC(source_path, out_path, n, seed, blocked_from, con)
#   Takes: source_path  - the cleaned survey CSV
#          out_path     - where to write the survey-format CSV
#          n            - number of residents, IDs C001, C002, ...
#          seed         - passed to set.seed()
#          blocked_from - "simulated", or "responses" to sample blocked strings
#                         from block C rows of read_responses(con)
#          con          - connection from db_connect(), for "responses" only
#   Returns: the data.frame written, invisibly, in the survey format (see
#            CLAUDE.md). Each person copies course, program, pref_time and
#            preferred days together from one sampled respondent. Pool: block C,
#            year Masters Y1, course in BLOCKC_COURSES.
#   Stops if a pref_time or course cannot be mapped, or a row fails
#   build_response_row() or load_survey().
#
# Blocked strings go through parse_windows, merge_windows, days_over_limit and
# build_response_row from R/mod_questionnaire.R, so they are saved as a form
# submission would save them.

BLOCKC_COURSES <- c("DS", "MBA", "FE", "AQF", "EE", "AE", "GE", "IMA")

BLOCKC_BANDS <- c("12 PM - 4 PM" = "afternoon", "4 PM - 8 PM" = "evening",
                  "8 PM - 12 AM" = "evening", "12 AM - 4 AM" = "night")

BLOCKC_FIRST_NAMES <- c(
  "Ananya", "Bhavna", "Chitra", "Deepa", "Esha", "Farah", "Gauri", "Hina",
  "Isha", "Jaya", "Kavya", "Lata", "Meera", "Nisha", "Oviya", "Priya", "Qadira",
  "Ritu", "Sana", "Tara", "Uma", "Vidya", "Wafa", "Yamini", "Zoya"
)

BLOCKC_LAST_NAMES <- c("Rao", "Iyer", "Sen", "Das", "Nair")

simulate_blockC <- function(source_path = "data/Hostel_Laundry_Clean.csv",
                            out_path = "data/fake_blockC.csv", n = 25,
                            seed = 2026,
                            blocked_from = c("simulated", "responses"),
                            con = NULL) {
  blocked_from <- match.arg(blocked_from)
  respondents <- blockc_respondents(source_path)
  real_blocked <- if (blocked_from == "responses") blockc_real_blocked(con)

  set.seed(seed)
  picked <- respondents[sample.int(nrow(respondents), n, replace = TRUE), ]

  rows <- lapply(seq_len(n), function(i) {
    p <- picked[i, ]
    band <- blockc_band(p$pref_time)
    windows <- if (blocked_from == "simulated") {
      blockc_blocked_windows(band)
    } else {
      parse_windows(real_blocked[sample.int(length(real_blocked), 1)])
    }
    merged <- merge_windows(windows)
    if (length(days_over_limit(merged)) > 0) {
      stop("Blocked times over the limit for resident ", i, call. = FALSE)
    }
    build_response_row(
      resident_id = sprintf("C%03d", i),
      name = paste(BLOCKC_FIRST_NAMES[(i - 1) %% length(BLOCKC_FIRST_NAMES) + 1],
                   BLOCKC_LAST_NAMES[(i - 1) %% length(BLOCKC_LAST_NAMES) + 1]),
      block = "C",
      program = if (p$wash_program == "Quick Wash") "quick" else "normal",
      course = p$course,
      blocked = merged,
      pref_days = WEEKDAYS[unlist(p[paste0("day_", tolower(WEEKDAYS))]) == 1],
      pref_band = band
    )
  })
  residents <- do.call(rbind, rows)

  readr::write_csv(residents, out_path, na = "")
  load_survey(out_path)
  invisible(residents)
}

blockc_respondents <- function(path) {
  data <- read_data_csv(path, c("course", "year", "block", "wash_program",
                                "pref_time", paste0("day_", tolower(WEEKDAYS))))
  pool <- data[data$block == "C" & data$year == "Masters Y1" &
                 data$course %in% BLOCKC_COURSES, ]
  if (nrow(pool) == 0) stop(path, ": no respondents to sample from", call. = FALSE)
  unmapped <- setdiff(pool$pref_time, names(BLOCKC_BANDS))
  if (length(unmapped) > 0) {
    stop(path, ": no band for pref_time: ", paste(unmapped, collapse = ", "),
         call. = FALSE)
  }
  pool
}

blockc_band <- function(pref_time) {
  unname(BLOCKC_BANDS[pref_time])
}

# Blocked strings of block C's real responses, empty ones included.
blockc_real_blocked <- function(con) {
  if (is.null(con)) {
    stop("blocked_from = \"responses\" needs con from db_connect()", call. = FALSE)
  }
  responses <- read_responses(con)
  responses <- responses[responses$block == "C", ]
  if (nrow(responses) == 0) {
    stop("No block C responses in the database yet", call. = FALSE)
  }
  ifelse(is.na(responses$blocked), "", responses$blocked)
}

# One person's week as data.frame(day, start, end), minutes after midnight of
# day, on 30-minute steps: the same sleep block every night, plus 0-2 personal
# blocks that do not touch sleep, each other or the previous night's sleep.
blockc_blocked_windows <- function(band) {
  bedtime <- blockc_pick(switch(
    band,
    night = c(1620, 1650, 1680),    # 03:00 to 04:00, so part of the night is free
    evening = c(1440, 1470, 1500),  # 00:00 to 01:00
    c(1350, 1380, 1410)             # 22:30 to 23:30
  ))
  sleep_length <- blockc_pick(c(420, 450, 480))
  windows <- do.call(rbind, lapply(seq_along(WEEKDAYS), function(d) {
    # A bedtime after midnight belongs to the next day, as the form writes it.
    after_midnight <- bedtime >= 1440
    data.frame(day = WEEKDAYS[(d - 1 + after_midnight) %% 7 + 1],
               start = bedtime - 1440L * after_midnight,
               end = bedtime - 1440L * after_midnight + sleep_length,
               stringsAsFactors = FALSE)
  }))

  for (k in seq_len(blockc_pick(0:2))) {
    repeat {
      length_min <- blockc_pick(c(60, 90, 120))
      block <- data.frame(day = blockc_pick(WEEKDAYS),
                          start = blockc_pick(seq(420, 1320 - length_min, by = 30)),
                          stringsAsFactors = FALSE)
      block$end <- block$start + length_min
      if (blockc_block_fits(block, windows)) break
    }
    windows <- rbind(windows, block)
  }
  windows
}

# TRUE if block leaves a gap to every window, counting a window that crosses
# midnight on the next day too, and keeps its day within the blocked limit.
blockc_block_fits <- function(block, windows) {
  week <- 7 * 1440
  to_week <- function(w) (match(w$day, WEEKDAYS) - 1) * 1440 + w$start
  starts <- to_week(windows)
  ends <- starts + windows$end - windows$start
  new_start <- to_week(block)
  new_end <- new_start + block$end - block$start
  clash <- vapply(c(-week, 0, week), function(shift) {
    any(new_start <= ends + shift & starts + shift <= new_end)
  }, logical(1))
  !any(clash) &&
    length(days_over_limit(rbind(windows, block))) == 0
}

# sample() treats a single number as 1:n, so always sample positions.
blockc_pick <- function(x) {
  x[sample.int(length(x), 1)]
}
