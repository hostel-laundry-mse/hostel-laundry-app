# Questionnaire module: a resident enters their @mse.ac.in email, then their
# name, block, course, wash program, blocked times and preferences, and agrees
# to the Terms of Use. A "Demo login (testing only)" option lets a sample
# resident be used instead.
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
# `blocked` is `;`-separated `Day HH:MM-HH:MM`. An end before the start runs
# past midnight (Mon 23:00-07:00) and counts towards the day it starts on; an
# end of 24:00 means midnight.
#
# Identity. The resident signs in with an email ending in @mse.ac.in (format
# check only, no password). Until the database has an email column
# (SAVE_EXTRA_COLUMNS is FALSE) the lowercased email is used as resident_id.
# Once the columns exist, flip SAVE_EXTRA_COLUMNS to TRUE: the first save then
# generates resident_id "R_" + block + a 2-digit number (R_C01), numbered per
# block from the highest existing number + 1, found again by email afterwards
# and never changed; email, consent and consent_at are saved as extra columns.
# The demo login keeps the sample resident_id (R001...).

# ---- File paths (the only place they are written) ----------------------------
PATH_SURVEY <- "data/fake_survey.csv"
PATH_TIMETABLE <- "data/timetable.csv"
PATH_CONFIG <- "data/config.csv"
PATH_LAST_PLANNED <- "data/fake_last_planned.csv"
PATH_FAKE_SCHEDULE <- "data/fake_schedule.csv"

# Block rule: each block is single-gender. Gender is only used to limit the
# block choices; it is not saved in the row.
BLOCK_GENDER <- c(A = "Female", B = "Female", C = "Female", D = "Male")
GENDERS <- c("Female", "Male")

SURVEY_COLUMNS <- c("resident_id", "name", "block", "program",
                    "course", "blocked", "pref_days", "pref_band")

# TODO: set to TRUE once the responses table has email, consent and
# consent_at columns (Person 2, data_load.R) and write_responses() accepts them.
SAVE_EXTRA_COLUMNS <- FALSE
EXTRA_COLUMNS <- c("email", "consent", "consent_at")
EMAIL_DOMAIN <- "mse.ac.in"

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
  list(title = "8. Other", bullets = c(
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
FALLBACK_BANDS <- c("morning", "afternoon", "evening")
FALLBACK_MAX_BLOCKED_HOURS <- 10
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
                programs = FALLBACK_PROGRAMS, bands = FALLBACK_BANDS,
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
    bands = or(names(config_section(tab, "band")), FALLBACK_BANDS),
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

days_over_limit <- function(w, limit = FALLBACK_MAX_BLOCKED_HOURS) {
  h <- hours_per_day(w)
  h[h > limit]
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

# One-row survey-format data.frame built from form values. With `email` given
# (SAVE_EXTRA_COLUMNS) the columns email, consent and consent_at are appended.
build_response_row <- function(resident_id, name, block, program, course,
                               blocked, pref_days, pref_band = "",
                               days = FALLBACK_DAYS, email = NULL,
                               consent_at = NULL) {
  row <- data.frame(resident_id = resident_id, name = name, block = block,
                    program = program, course = course,
                    blocked = format_windows(blocked, days),
                    pref_days = paste(pref_days, collapse = ";"),
                    pref_band = if (is.null(pref_band)) "" else pref_band,
                    stringsAsFactors = FALSE)
  if (!is.null(email)) {
    row$email <- email
    row$consent <- 1L
    row$consent_at <- if (is.null(consent_at)) "" else consent_at
    return(row[, c(SURVEY_COLUMNS, EXTRA_COLUMNS)])
  }
  row[, SURVEY_COLUMNS]
}

test_response_row <- function() {
  row <- build_response_row("R001", "Kiran Okafor", "A", "quick", "DS",
                            parse_windows("Mon 23:00-07:00"), c("Sat", "Sun"),
                            "evening")
  # exactly the columns of CLAUDE.md "Survey data format", in order, no room
  stopifnot(identical(names(row), c("resident_id", "name", "block", "program",
                                    "course", "blocked", "pref_days",
                                    "pref_band")))
  stopifnot(nrow(row) == 1, row$blocked == "Mon 23:00-07:00",
            row$pref_days == "Sat;Sun")
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
# have none or there is no database. `key` is the resident_id, or the email
# when the responses have an email column.
saved_response <- function(con, key) {
  if (is.null(con)) return(NULL)
  rows <- tryCatch(read_responses(con), error = function(e) NULL)
  if (is.null(rows)) return(NULL)
  hit <- if ("email" %in% names(rows)) rows$email == key else FALSE
  row <- rows[rows$resident_id == key | hit, , drop = FALSE]
  if (nrow(row) == 0) NULL else row
}

# ---- Email and resident id ---------------------------------------------------

normalize_email <- function(x) tolower(trimws(x))

# Format check only: one address ending in @mse.ac.in, any case. Spaces around
# the address are ignored; spaces inside it are not allowed.
is_valid_email <- function(x) {
  if (length(x) != 1 || is.na(x)) return(FALSE)
  grepl(paste0("^[^@[:space:]]+@", gsub(".", "\\.", EMAIL_DOMAIN, fixed = TRUE), "$"),
        normalize_email(x))
}

# "R_" + block + 2-digit number: the highest existing number for that block
# + 1 (not a row count, so deleted rows never cause a clash).
next_resident_id <- function(block, existing_ids) {
  m <- regmatches(existing_ids,
                  regexec(paste0("^R_", block, "([0-9]+)$"), existing_ids))
  nums <- suppressWarnings(as.integer(vapply(m, function(v) {
    if (length(v) == 2) v[2] else NA_character_
  }, "")))
  sprintf("R_%s%02d", block, max(c(0L, nums), na.rm = TRUE) + 1L)
}

# The resident_id to save. Until the email column exists the lowercased email
# is the id. After that an email already in `rows` keeps its id; a new one gets
# the next id for the block. `rows` is read_responses() (or NULL).
resolve_resident_id <- function(email, block, rows, extra = SAVE_EXTRA_COLUMNS) {
  email <- normalize_email(email)
  if (!extra) return(email)
  if (is.data.frame(rows) && "email" %in% names(rows)) {
    hit <- rows$resident_id[!is.na(rows$email) & rows$email == email]
    if (length(hit) > 0) return(hit[1])
  }
  next_resident_id(block, if (is.data.frame(rows)) rows$resident_id else character())
}

test_email_and_id <- function() {
  ok <- c("aisha.bagga@mse.ac.in", "A.Bagga@MSE.AC.IN", "  a@mse.ac.in  ")
  bad <- c("a@gmail.com", "a@mse.ac.in.evil.com", "a@xmse.ac.in", "a b@mse.ac.in",
           "a@ mse.ac.in", "@mse.ac.in", "a@@mse.ac.in", "a@mse.ac.in;b@mse.ac.in",
           "", "   ", NA, "mse.ac.in", "a")
  stopifnot(all(vapply(ok, is_valid_email, NA)))
  stopifnot(!any(vapply(bad, is_valid_email, NA)))
  stopifnot(normalize_email("  A.Bagga@MSE.ac.in ") == "a.bagga@mse.ac.in")
  # generated ids: highest number + 1 per block, not a row count
  stopifnot(next_resident_id("C", c("R_C01", "R_C03", "R_A07", "R001", "x")) == "R_C04")
  stopifnot(next_resident_id("D", c("R_C01")) == "R_D01")
  stopifnot(next_resident_id("A", character()) == "R_A01")
  stopifnot(next_resident_id("B", c("R_B09")) == "R_B10",
            next_resident_id("B", c("R_B99")) == "R_B100")
  stopifnot(next_resident_id("C", c("R_C01", "R_CC02", "R_C1x")) == "R_C02")
  # until the columns exist the email is the id
  stopifnot(resolve_resident_id("A@Mse.ac.in", "C", NULL, FALSE) == "a@mse.ac.in")
  # with the columns: first save generates, later saves keep the id
  rows <- data.frame(resident_id = c("R_C01", "R_C02"),
                     email = c("x@mse.ac.in", "y@mse.ac.in"))
  stopifnot(resolve_resident_id("z@mse.ac.in", "C", rows, TRUE) == "R_C03")
  stopifnot(resolve_resident_id("Y@mse.ac.in", "A", rows, TRUE) == "R_C02")  # block changed
  stopifnot(resolve_resident_id("z@mse.ac.in", "D", NULL, TRUE) == "R_D01")
  invisible(TRUE)
}

resident_choices <- function(residents) {
  setNames(residents$resident_id, paste(residents$resident_id, residents$name))
}

# Blocks a gender may choose, in the order of `blocks` (default: all known).
blocks_for_gender <- function(gender, blocks = names(BLOCK_GENDER)) {
  blocks[!is.na(BLOCK_GENDER[blocks]) & BLOCK_GENDER[blocks] == gender]
}

block_allowed <- function(gender, block) {
  g <- unname(BLOCK_GENDER[block])
  !is.na(g) & g == gender
}

# Gender implied by a block (NA for an unknown block).
gender_for_block <- function(block) unname(BLOCK_GENDER[block])

test_block_rule <- function() {
  stopifnot(identical(blocks_for_gender("Male"), "D"))
  stopifnot(identical(blocks_for_gender("Female"), c("A", "B", "C")))
  stopifnot(identical(blocks_for_gender("Female", c("C", "D")), "C"))  # keeps order
  stopifnot(block_allowed("Male", "D"), block_allowed("Female", "A"),
            block_allowed("Female", "B"), block_allowed("Female", "C"))
  stopifnot(!block_allowed("Male", "A"), !block_allowed("Male", "C"),
            !block_allowed("Female", "D"), !block_allowed("Male", "Z"))
  stopifnot(identical(gender_for_block("D"), "Male"),
            identical(gender_for_block("B"), "Female"),
            is.na(gender_for_block("Z")))
  # every configured block and every sample resident's block has a gender
  stopifnot(all(q_config()$blocks %in% names(BLOCK_GENDER)))
  stopifnot(all(q_residents()$block %in% names(BLOCK_GENDER)))
  invisible(TRUE)
}

time_choices <- function(from = "00:00", to = "24:00", step = 30) {
  min_to_hhmm(seq(hhmm_to_min(from), hhmm_to_min(to), by = step))
}

# Server-level checks: the terms block submit, the demo login saves a valid
# row, and the email login saves under the lowercased email. Needs shiny,
# bslib, DBI and RSQLite, and R/data_load.R, sourced first.
test_save_flow <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  db_init(con)
  form <- list(name = "Test Person", gender = "Female", block = "B",
               course = "DS", program = "quick", pref_band = "", pref_days = "Sat")
  shiny::testServer(mod_questionnaire_server, args = list(con = con), {
    # not signed in
    do.call(session$setInputs, c(form, list(terms = TRUE, save = 1)))
    stopifnot(is.null(last_response()), nrow(read_responses(con)) == 0)
    # bad email: not signed in, message shown
    session$setInputs(email = "someone@gmail.com", email_go = 1)
    stopifnot(is.null(current_user()), grepl("mse.ac.in", output$email_msg$html))
    # good email (any case, spaces around)
    session$setInputs(email = "  Test.Person@MSE.AC.IN ", email_go = 2)
    stopifnot(identical(current_user(), "test.person@mse.ac.in"))
    # terms not ticked: submit is refused and nothing is saved
    session$setInputs(terms = FALSE, save = 2)
    stopifnot(is.null(last_response()), nrow(read_responses(con)) == 0,
              grepl("Terms of Use", output$save_result$html))
    # ticked: saved under the lowercased email (SAVE_EXTRA_COLUMNS is FALSE)
    session$setInputs(terms = TRUE, save = 3)
    r <- read_responses(con)
    stopifnot(nrow(r) == 1, r$resident_id == "test.person@mse.ac.in",
              identical(names(r), SURVEY_COLUMNS))
    # demo login: a sample resident still saves a valid survey-format row
    session$setInputs(demo = TRUE, user = "R001", login = 1)
    do.call(session$setInputs, modifyList(form, list(name = "", block = "A", terms = TRUE, save = 4)))
    r <- read_responses(con)
    stopifnot(nrow(r) == 2, "R001" %in% r$resident_id,
              identical(names(r), SURVEY_COLUMNS),
              r$name[r$resident_id == "R001"] == residents$name[residents$resident_id == "R001"],
              r$block[r$resident_id == "R001"] == "A")
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
      .q-days.shiny-input-container { margin-bottom: .5rem; width: auto; }
    ")),
    layout_columns(
      fill = FALSE, col_widths = c(12, 12),
      card(
        fill = FALSE,
        card_header("Sign in"),
        div(class = "d-flex flex-wrap gap-3 align-items-start",
            textInput(ns("email"), "Your @mse.ac.in email",
                      placeholder = paste0("name@", EMAIL_DOMAIN), width = "20rem"),
            div(style = "margin-top: 32px;",
                actionButton(ns("email_go"), "Continue"))),
        uiOutput(ns("email_msg")),
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
          div(class = "d-flex flex-wrap gap-3 align-items-start",
              textInput(ns("name"), "Name", width = "12rem"),
              selectInput(ns("gender"), "Gender", choices = GENDERS,
                          selectize = FALSE, width = "8rem"),
              selectInput(ns("block"), "Block",
                          choices = blocks_for_gender(GENDERS[1], cfg$blocks),
                          selectize = FALSE, width = "8rem"),
              selectInput(ns("course"), "Course", choices = q_courses(),
                          selectize = FALSE, width = "10rem"),
              selectInput(ns("program"), "Program", choices = prog_labels,
                          selectize = FALSE, width = "11rem"),
              selectInput(ns("pref_band"), "Time band (optional)",
                          choices = c("No preference" = "", cfg$bands),
                          selectize = FALSE, width = "12rem")),
          div(class = "q-days",
              checkboxGroupInput(ns("pref_days"), "Preferred days (optional)",
                                 choices = cfg$days, inline = TRUE)),
          tags$hr(class = "my-2"),
          h6("Blocked times (when you are definitely unavailable, including sleep)"),
          p(class = "text-muted small mb-2",
            "A window can run past midnight: choose a 'To' earlier than 'From'."),
          div(class = "d-flex flex-wrap gap-3 align-items-end",
              selectInput(ns("b_day"), "Day", choices = cfg$days,
                          selectize = FALSE, width = "7rem"),
              selectInput(ns("b_start"), "From", choices = times[-length(times)],
                          selected = "09:00", selectize = FALSE, width = "7rem"),
              selectInput(ns("b_end"), "To", choices = times[-1],
                          selected = "17:00", selectize = FALSE, width = "7rem"),
              div(class = "mb-3", actionButton(ns("b_add"), "Add window")),
              div(class = "mb-3",
                  actionButton(ns("b_sleep"), "Add sleep 23:00-07:00 to every day",
                               class = "btn-outline-secondary"))),
          uiOutput(ns("b_list")),
          uiOutput(ns("b_warning")),
          tags$hr(class = "my-2"),
          div(class = "d-flex flex-wrap column-gap-3 align-items-baseline",
              checkboxInput(ns("terms"), CONSENT_LABEL, value = FALSE, width = "auto"),
              actionLink(ns("show_terms"), "Read the Terms of Use")),
          actionButton(ns("save"), "Save response", class = "btn-primary",
                       disabled = NA),
          uiOutput(ns("save_result"))
        )
      )
    )
  )
}

mod_questionnaire_server <- function(id, con = NULL) {
  moduleServer(id, function(input, output, session) {
    cfg <- q_config()
    residents <- q_residents()
    current_user <- reactiveVal(NULL)  # normalised email, or sample resident_id
    login_kind <- reactiveVal(NULL)    # "email" or "demo"
    blocked <- reactiveVal(empty_windows())
    last_response <- reactiveVal(NULL)
    save_msg <- reactiveVal(NULL)
    email_msg <- reactiveVal(NULL)
    prefill_from <- reactiveVal("none")

    output$signed_in <- reactive(!is.null(current_user()))
    outputOptions(output, "signed_in", suspendWhenHidden = FALSE)

    user_row <- reactive({
      req(identical(login_kind(), "demo"))
      residents[residents$resident_id == current_user(), , drop = FALSE]
    })

    output$who <- renderText({
      if (is.null(current_user())) return("Not signed in.")
      paste0("Signed in as ", current_user(),
             if (identical(login_kind(), "demo")) " (demo login)", ".",
             if (prefill_from() == "saved") " Form filled from your saved response.")
    })

    get_col <- function(row, col, default = "") {
      if (col %in% names(row) && !is.na(row[[col]])) row[[col]] else default
    }

    # Gender decides which blocks can be chosen.
    observeEvent(input$gender, {
      choices <- blocks_for_gender(input$gender, cfg$blocks)
      keep <- isolate(input$block)
      updateSelectInput(session, "block", choices = choices,
                        selected = if (keep %in% choices) keep else choices[1])
    })

    # Fills the form from a one-row survey-format data.frame.
    fill_form <- function(r) {
      updateTextInput(session, "name", value = get_col(r, "name"))
      block <- get_col(r, "block")
      if (block %in% cfg$blocks && !is.na(gender_for_block(block))) {
        gender <- gender_for_block(block)  # gender is derived from the block
        updateSelectInput(session, "gender", selected = gender)
        updateSelectInput(session, "block",
                          choices = blocks_for_gender(gender, cfg$blocks),
                          selected = block)
      }
      if (get_col(r, "course") %in% q_courses()) {
        updateSelectInput(session, "course", selected = r$course)
      }
      if (get_col(r, "program") %in% names(cfg$programs)) {
        updateSelectInput(session, "program", selected = r$program)
      }
      updateSelectInput(session, "pref_band",
                        selected = get_col(r, "pref_band"))
      pd <- strsplit(get_col(r, "pref_days"), ";", fixed = TRUE)[[1]]
      updateCheckboxGroupInput(session, "pref_days", selected = pd)
      blocked(parse_windows(get_col(r, "blocked")))
    }

    observeEvent(input$email_go, {
      if (!is_valid_email(input$email)) {
        email_msg(div(class = "text-danger small mb-2",
                      paste0("Please use your ", EMAIL_DOMAIN, " email address, ",
                             "for example name@", EMAIL_DOMAIN, ".")))
        return()
      }
      email_msg(NULL)
      save_msg(NULL)
      key <- normalize_email(input$email)
      current_user(key)
      login_kind("email")
      r <- saved_response(con, key)
      prefill_from(if (is.null(r)) "none" else "saved")
      if (!is.null(r)) fill_form(r)
    })
    output$email_msg <- renderUI(email_msg())

    # Demo login (testing only): a sample resident from the fake survey.
    observeEvent(input$login, {
      current_user(input$user)
      login_kind("demo")
      email_msg(NULL)
      save_msg(NULL)
      r <- saved_response(con, input$user)
      prefill_from(if (is.null(r)) "sample" else "saved")
      if (is.null(r)) {
        r <- residents[residents$resident_id == input$user, , drop = FALSE]
      }
      fill_form(r)
    })

    observeEvent(input$show_terms, {
      showModal(modalDialog(title = "Terms of Use", terms_ui(), size = "l",
                            easyClose = TRUE, footer = modalButton("Close")))
    })

    # Save is only enabled once the terms are ticked.
    observe({
      shiny::updateActionButton(session, "save", disabled = !isTRUE(input$terms))
    })

    observeEvent(input$b_add, {
      s <- hhmm_to_min(input$b_start)
      e <- hhmm_to_min(input$b_end)
      if (e == s) {
        showNotification("'To' must differ from 'From'.", type = "error")
        return()
      }
      if (e < s) e <- e + 1440L  # runs past midnight
      blocked(rbind(blocked(), data.frame(day = input$b_day, start = s, end = e,
                                          stringsAsFactors = FALSE)))
    })

    observeEvent(input$b_sleep, {
      sleep <- parse_windows(paste0(cfg$days, " 23:00-07:00", collapse = ";"))
      blocked(rbind(blocked(), sleep))
    })

    output$b_list <- renderUI({
      m <- merge_windows(blocked(), cfg$days)
      if (nrow(m) == 0) return(p(class = "text-muted", "No blocked times yet."))
      tagList(
        p(strong("Blocked (merged): "), format_windows(m, cfg$days)),
        actionButton(session$ns("b_clear"), "Clear all",
                     class = "btn-sm btn-outline-danger")
      )
    })
    observeEvent(input$b_clear, blocked(empty_windows()))

    output$b_warning <- renderUI({
      limit <- cfg$max_blocked_hours
      over <- days_over_limit(blocked(), limit)
      if (length(over) == 0) return(NULL)
      div(class = "alert alert-warning mt-2",
          strong("Warning: "),
          paste0(names(over), " (", round(over, 1), " h)", collapse = ", "),
          paste0(" blocked for more than ", limit, " hours. You can still ",
                 "save, but you will have little free time to wash."))
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
      if (!isTRUE(input$terms)) {
        return(refuse(paste("You must agree to the Terms of Use and the data",
                            "policy before submitting.")))
      }
      demo <- identical(login_kind(), "demo")
      name <- trimws(input$name)
      if (!nzchar(name) && demo) name <- user_row()$name
      if (!nzchar(name)) return(refuse("Please enter your name."))
      if (!block_allowed(input$gender, input$block)) {
        return(refuse(paste0(
          "Block ", input$block, " is not available for ", input$gender, ". ",
          input$gender, " residents can choose block ",
          paste(blocks_for_gender(input$gender), collapse = ", "), ".")))
      }

      resident_id <- if (demo) current_user() else {
        resolve_resident_id(current_user(), input$block,
                            tryCatch(read_responses(con), error = function(e) NULL))
      }
      row <- build_response_row(
        resident_id, name, input$block, input$program, input$course, blocked(),
        input$pref_days, input$pref_band, cfg$days,
        email = if (SAVE_EXTRA_COLUMNS) { if (demo) "" else current_user() },
        consent_at = if (SAVE_EXTRA_COLUMNS) format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      res <- tryCatch(save_response(con, row), error = function(e) e)
      if (inherits(res, "error")) {
        save_msg(div(class = "alert alert-danger mt-2",
                     "Could not save: ", conditionMessage(res)))
        return()
      }
      last_response(row)
      save_msg(if (res$saved) {
        div(class = "alert alert-success mt-2", "Saved.")
      } else {
        tagList(
          div(class = "alert alert-info mt-2",
              "Database not ready: this is what would be saved."),
          tags$pre(paste(capture.output(print(t(row))), collapse = "\n"))
        )
      })
    })
    output$save_result <- renderUI(save_msg())

    last_response
  })
}
