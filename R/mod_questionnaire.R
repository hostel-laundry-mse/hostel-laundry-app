# Questionnaire module: a sample resident logs in, then enters their block,
# course, wash program, blocked times and preferred days.
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
# Row saved (one per resident, a resubmission replaces the earlier row):
#   resident_id, name, block, course, program, blocked, pref_days, pref_band
# `blocked` uses the same `;`-separated `Day HH:MM-HH:MM` string as
# `availability` in CLAUDE.md, but lists times the resident is NOT free.
# TODO: CLAUDE.md does not define `blocked` or `course` yet; confirm the format.

DAYS <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
BLOCKED_LIMIT_HOURS <- 10

# ---- Fallbacks, used while load_config() / load_survey() are stubs ----------
# TODO: remove each fallback once the real loader returns the value.
FALLBACK_BLOCKS <- c("A", "B", "C", "D")
FALLBACK_COURSES <- c("DS", "FE", "GE", "AE", "AQF", "EE", "MBA")
FALLBACK_PROGRAMS <- c(quick = 30, normal = 60)
FALLBACK_BANDS <- c("morning", "afternoon", "evening")  # TODO: from config
FALLBACK_RESIDENTS <- data.frame(
  resident_id = c("R001", "R002", "R003"),
  name = c("Aarav Okafor", "Meera Rao", "Tara Singh"),
  block = c("A", "C", "D"),
  course = c("DS", "MBA", "EE"),
  program = c("quick", "normal", "quick"),
  blocked = "",
  pref_days = c("Sat;Sun", "", "Fri"),
  pref_band = c("evening", "", "morning"),
  stringsAsFactors = FALSE
)

# ---- Pure helpers (tested by test_blocked_helpers()) ------------------------

hhmm_to_min <- function(x) {
  p <- strsplit(x, ":", fixed = TRUE)
  vapply(p, function(v) as.integer(v[1]) * 60L + as.integer(v[2]), integer(1))
}

min_to_hhmm <- function(m) sprintf("%02d:%02d", m %/% 60L, m %% 60L)

empty_windows <- function() {
  data.frame(day = character(), start = integer(), end = integer(),
             stringsAsFactors = FALSE)
}

# "Mon 00:00-07:00;Tue 23:00-24:00" -> data.frame(day, start, end) in minutes
parse_windows <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) {
    return(empty_windows())
  }
  parts <- trimws(strsplit(x, ";", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  day <- sub(" .*$", "", parts)
  times <- strsplit(sub("^\\S+ ", "", parts), "-", fixed = TRUE)
  data.frame(day = day,
             start = hhmm_to_min(vapply(times, `[`, "", 1)),
             end = hhmm_to_min(vapply(times, `[`, "", 2)),
             stringsAsFactors = FALSE)
}

format_windows <- function(w) {
  if (nrow(w) == 0) return("")
  w <- w[order(match(w$day, DAYS), w$start), ]
  paste0(w$day, " ", min_to_hhmm(w$start), "-", min_to_hhmm(w$end),
         collapse = ";")
}

# Merge overlapping or touching windows within each day.
merge_windows <- function(w) {
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
  res <- res[order(match(res$day, DAYS), res$start), ]
  rownames(res) <- NULL
  res
}

# Named numeric vector: merged blocked hours for each day that has any.
hours_per_day <- function(w) {
  m <- merge_windows(w)
  if (nrow(m) == 0) return(setNames(numeric(), character()))
  tapply((m$end - m$start) / 60, factor(m$day, levels = DAYS), sum)[
    unique(m$day)]
}

days_over_limit <- function(w, limit = BLOCKED_LIMIT_HOURS) {
  h <- hours_per_day(w)
  h[h > limit]
}

test_blocked_helpers <- function() {
  w <- data.frame(day = c("Mon", "Mon", "Mon", "Tue"),
                  start = hhmm_to_min(c("00:00", "06:00", "09:00", "08:00")),
                  end = hhmm_to_min(c("07:00", "08:00", "10:00", "10:00")),
                  stringsAsFactors = FALSE)
  m <- merge_windows(w)
  stopifnot(nrow(m) == 3)                              # overlap merged
  stopifnot(identical(m$end[1], hhmm_to_min("08:00"))) # 00:00-08:00
  stopifnot(hours_per_day(w)[["Mon"]] == 9, hours_per_day(w)[["Tue"]] == 2)
  stopifnot(length(days_over_limit(w)) == 0)
  big <- rbind(w, data.frame(day = "Mon", start = hhmm_to_min("20:00"),
                             end = hhmm_to_min("24:00")))
  stopifnot(identical(names(days_over_limit(big)), "Mon"))  # 13h > 10h
  stopifnot(format_windows(parse_windows("Tue 08:00-10:00;Mon 00:00-07:30")) ==
              "Mon 00:00-07:30;Tue 08:00-10:00")
  stopifnot(nrow(parse_windows("")) == 0)
  invisible(TRUE)
}

# One-row survey-format data.frame built from form values.
build_response_row <- function(resident_id, name, block, course, program,
                               blocked, pref_days, pref_band = "") {
  data.frame(resident_id = resident_id, name = name, block = block,
             course = course, program = program,
             blocked = format_windows(blocked),
             pref_days = paste(pref_days, collapse = ";"),
             pref_band = if (is.null(pref_band)) "" else pref_band,
             stringsAsFactors = FALSE)
}

# Saves one row, replacing that resident's earlier row. With con = NULL (the
# database is not ready) nothing is written. Returns list(saved, row).
# TODO: table name "responses" is a guess until Person 2 creates the tables.
save_response <- function(con, row) {
  if (is.null(con)) return(list(saved = FALSE, row = row))
  if (DBI::dbExistsTable(con, "responses")) {
    DBI::dbExecute(con, "DELETE FROM responses WHERE resident_id = ?",
                   params = list(row$resident_id))
  }
  DBI::dbWriteTable(con, "responses", row, append = TRUE)
  list(saved = TRUE, row = row)
}

# ---- Option sources ----------------------------------------------------------

q_residents <- function() {
  s <- tryCatch(load_survey(), error = function(e) NULL)
  if (!is.data.frame(s) || nrow(s) == 0) {
    s <- tryCatch(readr::read_csv("data/fake_survey.csv", col_types = readr::cols(.default = "c"),
                                  show_col_types = FALSE),
                  error = function(e) NULL)
  }
  if (is.data.frame(s) && nrow(s) > 0) return(as.data.frame(s))
  FALLBACK_RESIDENTS  # TODO: fake_survey.csv has no rows yet
}

q_blocks <- function() {
  cfg <- tryCatch(load_config(), error = function(e) NULL)
  b <- if (is.list(cfg)) cfg$machines$block else NULL
  if (length(b)) b else FALLBACK_BLOCKS
}

q_courses <- function(path = "data/timetable.csv") {
  if (file.exists(path)) {
    t <- tryCatch(readr::read_csv(path, show_col_types = FALSE), error = function(e) NULL)
    if (is.data.frame(t) && "course" %in% names(t) && nrow(t) > 0) {
      return(sort(unique(t$course)))
    }
  }
  FALLBACK_COURSES  # TODO: data/timetable.csv does not exist yet
}

# named vector program key -> minutes
q_programs <- function() {
  cfg <- tryCatch(load_config(), error = function(e) NULL)
  p <- if (is.list(cfg)) cfg$programs else NULL
  if (is.data.frame(p) && nrow(p) > 0) {
    return(setNames(p$duration_min, p$program))
  }
  FALLBACK_PROGRAMS
}

# band keys from config; the form adds "no preference" itself
q_bands <- function() {
  cfg <- tryCatch(load_config(), error = function(e) NULL)
  b <- if (is.list(cfg)) names(cfg$bands) else NULL
  if (length(b)) b else FALLBACK_BANDS
}

q_opening <- function() {
  cfg <- tryCatch(load_config(), error = function(e) NULL)
  h <- if (is.list(cfg)) cfg$hours else NULL
  # TODO: machines are usable 24 h; time pickers use the full day until the
  # config loader works. Opening hours are read from config once it does.
  list(open = if (!is.null(h$open)) h$open else "00:00",
       close = if (!is.null(h$close)) h$close else "24:00")
}

time_choices <- function(from = "00:00", to = "24:00", step = 30) {
  min_to_hhmm(seq(hhmm_to_min(from), hhmm_to_min(to), by = step))
}

# ---- Module ------------------------------------------------------------------

mod_questionnaire_ui <- function(id) {
  ns <- NS(id)
  programs <- q_programs()
  prog_labels <- setNames(names(programs),
                          paste0(tools::toTitleCase(names(programs)), " (",
                                 programs, " min)"))
  times <- time_choices()
  layout_columns(
    col_widths = c(12, 12),
    card(
      card_header("Log in (demo)"),
      layout_columns(
        col_widths = c(6, 3),
        selectInput(ns("user"), "Sample resident", choices = NULL),
        div(style = "margin-top: 32px;", actionButton(ns("login"), "Log in"))
      ),
      textOutput(ns("who"))
    ),
    card(
      card_header("Questionnaire"),
      selectInput(ns("block"), "Block", choices = q_blocks()),
      selectInput(ns("course"), "Course", choices = q_courses()),
      selectInput(ns("program"), "Program", choices = prog_labels),
      checkboxGroupInput(ns("pref_days"), "Preferred days (optional)",
                         choices = DAYS, inline = TRUE),
      selectInput(ns("pref_band"), "Preferred time band (optional)",
                  choices = c("No preference" = "", q_bands())),
      tags$hr(),
      h6("Blocked times (when you are definitely unavailable, including sleep)"),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        selectInput(ns("b_day"), "Day", choices = DAYS),
        selectInput(ns("b_start"), "From", choices = times[-length(times)],
                    selected = "09:00"),
        selectInput(ns("b_end"), "To", choices = times[-1], selected = "17:00"),
        div(style = "margin-top: 32px;",
            actionButton(ns("b_add"), "Add window"))
      ),
      actionButton(ns("b_sleep"), "Add sleep 23:00-07:00 to every day",
                   class = "btn-sm btn-outline-secondary"),
      uiOutput(ns("b_list")),
      uiOutput(ns("b_warning")),
      tags$hr(),
      actionButton(ns("save"), "Save response", class = "btn-primary"),
      uiOutput(ns("save_result"))
    )
  )
}

mod_questionnaire_server <- function(id, con = NULL) {
  moduleServer(id, function(input, output, session) {
    residents <- q_residents()
    current_user <- reactiveVal(NULL)
    blocked <- reactiveVal(empty_windows())
    last_response <- reactiveVal(NULL)
    save_msg <- reactiveVal(NULL)

    updateSelectInput(session, "user", choices = setNames(
      residents$resident_id, paste(residents$resident_id, residents$name)))

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

    observeEvent(input$login, {
      current_user(input$user)
      save_msg(NULL)
      r <- residents[residents$resident_id == input$user, , drop = FALSE]
      if (get_col(r, "block") %in% q_blocks()) {
        updateSelectInput(session, "block", selected = r$block)
      }
      if (get_col(r, "course") %in% q_courses()) {
        updateSelectInput(session, "course", selected = r$course)
      }
      if (get_col(r, "program") %in% q_programs() |
          get_col(r, "program") %in% names(q_programs())) {
        updateSelectInput(session, "program", selected = r$program)
      }
      pd <- strsplit(get_col(r, "pref_days"), ";", fixed = TRUE)[[1]]
      updateCheckboxGroupInput(session, "pref_days", selected = pd)
      updateSelectInput(session, "pref_band",
                        selected = get_col(r, "pref_band"))
      blocked(parse_windows(get_col(r, "blocked")))
    })

    observeEvent(input$b_add, {
      s <- hhmm_to_min(input$b_start)
      e <- hhmm_to_min(input$b_end)
      if (e <= s) {
        showNotification("'To' must be after 'From'.", type = "error")
        return()
      }
      blocked(rbind(blocked(), data.frame(day = input$b_day, start = s, end = e,
                                          stringsAsFactors = FALSE)))
    })

    observeEvent(input$b_sleep, {
      sleep <- rbind(
        data.frame(day = DAYS, start = 0L, end = hhmm_to_min("07:00")),
        data.frame(day = DAYS, start = hhmm_to_min("23:00"), end = 1440L))
      blocked(rbind(blocked(), sleep))
    })

    output$b_list <- renderUI({
      m <- merge_windows(blocked())
      if (nrow(m) == 0) return(p(class = "text-muted", "No blocked times yet."))
      tagList(
        p(strong("Blocked (merged): "), format_windows(m)),
        actionButton(session$ns("b_clear"), "Clear all",
                     class = "btn-sm btn-outline-danger")
      )
    })
    observeEvent(input$b_clear, blocked(empty_windows()))

    output$b_warning <- renderUI({
      over <- days_over_limit(blocked())
      if (length(over) == 0) return(NULL)
      div(class = "alert alert-warning mt-2",
          strong("Warning: "),
          paste0(names(over), " (", round(over, 1), " h)", collapse = ", "),
          paste0(" blocked for more than ", BLOCKED_LIMIT_HOURS,
                 " hours. You can still save, but you will have little free time to wash."))
    })

    observeEvent(input$save, {
      if (is.null(current_user())) {
        showNotification("Please log in first.", type = "error")
        return()
      }
      row <- build_response_row(current_user(), user_row()$name, input$block,
                                input$course, input$program, blocked(),
                                input$pref_days, input$pref_band)
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
