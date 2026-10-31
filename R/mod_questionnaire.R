# Questionnaire module: a sample resident logs in, then enters their block,
# course, wash program, blocked times and preferences.
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

# ---- File paths (the only place they are written) ----------------------------
PATH_SURVEY <- "data/fake_survey.csv"
PATH_TIMETABLE <- "data/fake_timetable.csv"  # CLAUDE.md: data/timetable.csv
PATH_CONFIG <- "data/config.csv"
PATH_LAST_PLANNED <- "data/fake_last_planned.csv"
PATH_FAKE_SCHEDULE <- "data/fake_schedule.csv"

# Block rule: each block is single-gender. Gender is only used to limit the
# block choices; it is not saved in the row.
BLOCK_GENDER <- c(A = "Female", B = "Female", C = "Female", D = "Male")
GENDERS <- c("Female", "Male")

SURVEY_COLUMNS <- c("resident_id", "name", "block", "program",
                    "course", "blocked", "pref_days", "pref_band")

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

# One-row survey-format data.frame built from form values.
build_response_row <- function(resident_id, name, block, program, course,
                               blocked, pref_days, pref_band = "",
                               days = FALLBACK_DAYS) {
  data.frame(resident_id = resident_id, name = name, block = block,
             program = program, course = course,
             blocked = format_windows(blocked, days),
             pref_days = paste(pref_days, collapse = ";"),
             pref_band = if (is.null(pref_band)) "" else pref_band,
             stringsAsFactors = FALSE)[, SURVEY_COLUMNS]
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

# Saves one row, replacing that resident's earlier row. With con = NULL (the
# database is not ready) nothing is written. Returns list(saved, row).
# TODO: table name "responses" is a guess until the database tables exist.
save_response <- function(con, row) {
  if (is.null(con)) return(list(saved = FALSE, row = row))
  if (DBI::dbExistsTable(con, "responses")) {
    DBI::dbExecute(con, "DELETE FROM responses WHERE resident_id = ?",
                   params = list(row$resident_id))
  }
  DBI::dbWriteTable(con, "responses", row, append = TRUE)
  list(saved = TRUE, row = row)
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
    layout_columns(
      fill = FALSE, col_widths = c(12, 12),
      card(
        fill = FALSE,
        card_header("Log in (demo)"),
        div(class = "d-flex flex-wrap gap-3 align-items-end",
            selectInput(ns("user"), "Sample resident",
                        choices = resident_choices(q_residents()),
                        selectize = FALSE, width = "16rem"),
            div(class = "mb-3", actionButton(ns("login"), "Log in"))),
        textOutput(ns("who"))
      ),
      card(
        fill = FALSE,
        card_header("Questionnaire"),
        div(class = "d-flex flex-wrap gap-3",
            selectInput(ns("gender"), "Gender", choices = GENDERS,
                        selectize = FALSE, width = "8rem"),
            selectInput(ns("block"), "Block",
                        choices = blocks_for_gender(GENDERS[1], cfg$blocks),
                        selectize = FALSE, width = "8rem"),
            selectInput(ns("course"), "Course", choices = q_courses(),
                        selectize = FALSE, width = "10rem"),
            selectInput(ns("program"), "Program", choices = prog_labels,
                        selectize = FALSE, width = "12rem"),
            selectInput(ns("pref_band"), "Preferred time band (optional)",
                        choices = c("No preference" = "", cfg$bands),
                        selectize = FALSE, width = "14rem")),
        checkboxGroupInput(ns("pref_days"), "Preferred days (optional)",
                           choices = cfg$days, inline = TRUE),
        tags$hr(),
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
        tags$hr(),
        actionButton(ns("save"), "Save response", class = "btn-primary"),
        uiOutput(ns("save_result"))
      )
    )
  )
}

mod_questionnaire_server <- function(id, con = NULL) {
  moduleServer(id, function(input, output, session) {
    cfg <- q_config()
    residents <- q_residents()
    current_user <- reactiveVal(NULL)
    blocked <- reactiveVal(empty_windows())
    last_response <- reactiveVal(NULL)
    save_msg <- reactiveVal(NULL)

    user_row <- reactive({
      req(current_user())
      residents[residents$resident_id == current_user(), , drop = FALSE]
    })

    output$who <- renderText({
      if (is.null(current_user())) "Not logged in."
      else paste("Logged in as", current_user(), "-", user_row()$name)
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

    observeEvent(input$login, {
      current_user(input$user)
      save_msg(NULL)
      r <- residents[residents$resident_id == input$user, , drop = FALSE]
      block <- get_col(r, "block")
      if (block %in% cfg$blocks && !is.na(gender_for_block(block))) {
        gender <- gender_for_block(block)  # sample residents: gender from block
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
      if (is.null(current_user())) {
        showNotification("Please log in first.", type = "error")
        return()
      }
      if (!block_allowed(input$gender, input$block)) {
        msg <- paste0("Block ", input$block, " is not available for ",
                      input$gender, ". ", input$gender, " residents can choose ",
                      "block ", paste(blocks_for_gender(input$gender), collapse = ", "),
                      ".")
        save_msg(div(class = "alert alert-danger mt-2", msg))
        showNotification(msg, type = "error")
        return()
      }
      row <- build_response_row(current_user(), user_row()$name, input$block,
                                input$program, input$course,
                                blocked(), input$pref_days, input$pref_band,
                                cfg$days)
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
