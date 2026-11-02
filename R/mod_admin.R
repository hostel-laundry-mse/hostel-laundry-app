# Admin module: read the residents' answers, run the pipeline (free windows,
# optimiser, checker) for a cycle, publish the schedule to the database.
#
# mod_admin_ui(id)
#   Takes: module id.
#   Returns: UI for the Admin page.
#
# mod_admin_server(id, con)
#   Takes: module id, con - DBI connection from db_connect().
#   Returns: reactive giving the published schedule data.frame
#            (person, machine, date, start, end), or NULL until Publish has
#            succeeded. NULL again after a new Generate. A demo or stand-in
#            schedule is never returned.
#
# Generate reads the answers with read_responses(con). With no answers it
# refuses ("No responses yet"). People over the cancellation or no-show limit
# (slot_penalties()) get fewer slots, and the page lists them. The "Use demo
# data" box instead uses data/fake_survey.csv and data/fake_schedule.csv, checked against
# load_survey(PATH_SURVEY); a demo schedule can never be published.
# If the optimiser is missing or fails (status code "error", or no bookings),
# the failure message is shown and the stand-in fake schedule is displayed,
# also not publishable.
# Publish runs clear_cycle_plan() then add_bookings(). If the cycle already has
# bookings it asks for a confirmation click first. The published flag is set
# only after add_bookings() succeeds, and the schedule stays in memory so a
# failed publish can be retried.

DEFAULT_CYCLE_START <- as.Date("2026-11-02")

# ---- Pipeline ----------------------------------------------------------------

text_or <- function(x, default = "") {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) default else x[1]
}

# Runs the pipeline for one cycle.
#   survey         - answers (survey format); NULL for demo data
#   last_planned_df - data.frame(resident_id, date) or NULL
#   demo           - use the fake survey and fake schedule, skip the optimiser
#   notify         - called with a progress message before each step
#   slots_lost     - data.frame from slot_penalties(), or NULL
# Returns list(schedule, source, steps, problems, warnings, message,
#              cycle_start, cycle_days). source is "optimiser" (publishable),
# "demo" or "fallback" (stand-in fake schedule, not publishable), or "none".
# `steps` is a data.frame(step, state, note); state is done, failed, missing
# or skipped.
run_pipeline <- function(cycle_start, survey, last_planned_df = NULL,
                         demo = FALSE, notify = function(msg) NULL,
                         slots_lost = NULL) {
  steps <- data.frame(step = character(), state = character(),
                      note = character(), stringsAsFactors = FALSE)
  add <- function(step, state, note = NULL) {
    steps[nrow(steps) + 1, ] <<- list(step, state, text_or(note))
  }
  warns <- character()
  cycle_days <- 10
  finish <- function(schedule = NULL, source = "none", problems = NULL,
                     message = NULL) {
    list(schedule = schedule, source = source, steps = steps,
         problems = problems, warnings = warns, message = message,
         cycle_start = cycle_start, cycle_days = cycle_days)
  }
  step <- function(name, value) {  # `value` is evaluated here, errors caught
    r <- tryCatch(value, error = function(e) e)
    failed <- inherits(r, "error")
    add(name, if (failed) "failed" else "done",
        if (failed) conditionMessage(r))
    r
  }

  notify("Loading config and timetable...")
  config <- step("load_config", load_config(PATH_CONFIG))
  if (inherits(config, "error")) {
    return(finish(message = paste("Could not load the config:",
                                  conditionMessage(config))))
  }
  timetable <- step("load_timetable", load_timetable(PATH_TIMETABLE))
  if (inherits(timetable, "error")) {
    return(finish(message = paste("Could not load the timetable:",
                                  conditionMessage(timetable))))
  }
  cycle_days <- text_or(config$rules$cycle_days, 10)

  # Checks a stand-in schedule against the fake survey (not the database).
  stand_in <- function(source, reason = NULL) {
    schedule <- read_csv_chr(PATH_FAKE_SCHEDULE)
    if (!is.data.frame(schedule)) {
      return(finish(message = paste("Could not read", PATH_FAKE_SCHEDULE)))
    }
    fake_survey <- step("load_survey (fake)", load_survey(PATH_SURVEY))
    problems <- if (inherits(fake_survey, "error")) NULL else {
      check(schedule, fake_survey, FALSE)
    }
    finish(schedule, source, problems, reason)
  }

  check <- function(schedule, check_survey, real) {
    if (!exists("check_schedule", mode = "function")) {
      add("check_schedule", "missing", "check_schedule is not merged yet")
      return(NULL)
    }
    notify("Checking the schedule...")
    p <- tryCatch(
      check_schedule(schedule, check_survey, config, timetable,
                     if (real) cycle_start, if (real) last_planned_df,
                     if (real) slots_lost),
      error = function(e) e)
    if (inherits(p, "error")) {
      add("check_schedule", "failed", conditionMessage(p))
      return(NULL)
    }
    add("check_schedule", "done", paste(nrow(p), "problem(s)"))
    p
  }

  if (demo) {
    add("run_optimiser", "skipped", "demo data: fake survey and fake schedule")
    return(stand_in("demo"))
  }

  notify("Building free windows...")
  free <- withCallingHandlers(
    step("build_free_windows",
         build_free_windows(survey, timetable, config, cycle_start)),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  if (inherits(free, "error")) {
    return(stand_in("fallback", paste("Free windows failed:",
                                      conditionMessage(free))))
  }

  if (!exists("run_optimiser", mode = "function")) {
    add("run_optimiser", "missing", "run_optimiser is not merged yet")
    return(stand_in("fallback", "run_optimiser is not available yet."))
  }
  notify("Running the optimiser...")
  res <- tryCatch(
    run_optimiser(survey, free, config, cycle_start, last_planned_df,
                  verbose = FALSE, slots_lost = slots_lost),
    error = function(e) e)
  if (inherits(res, "error")) {
    add("run_optimiser", "failed", conditionMessage(res))
    return(stand_in("fallback", paste("The optimiser failed:",
                                      conditionMessage(res))))
  }
  sched <- res$schedule
  if (identical(res$status$code, "error") || !is.data.frame(sched) ||
      nrow(sched) == 0) {
    reason <- text_or(res$status$message, "The optimiser produced no bookings.")
    add("run_optimiser", "failed", reason)
    return(stand_in("fallback", reason))
  }
  add("run_optimiser", "done", paste0(
    text_or(res$status$message),
    if (is.data.frame(res$underserved)) {
      paste0(" Underserved: ", nrow(res$underserved), ".")
    }))
  finish(sched, "optimiser", check(sched, survey, TRUE))
}

# ---- Publishing --------------------------------------------------------------

# What the module returns: the schedule only once it has been published from
# the optimiser. Demo and stand-in schedules are never published.
released_schedule <- function(published, source, schedule) {
  if (isTRUE(published) && identical(source, "optimiser")) schedule else NULL
}

test_released_schedule <- function() {
  sched <- data.frame(person = "R001", machine = "A-1", date = "2026-11-03",
                      start = "09:00", end = "09:30")
  stopifnot(identical(released_schedule(TRUE, "optimiser", sched), sched))
  stopifnot(is.null(released_schedule(FALSE, "optimiser", sched)))  # not yet published
  stopifnot(is.null(released_schedule(TRUE, "demo", sched)))
  stopifnot(is.null(released_schedule(TRUE, "fallback", sched)))
  stopifnot(is.null(released_schedule(TRUE, NULL, sched)))
  invisible(TRUE)
}

# The schedule for display: a `name` column (from read_residents()) right after
# `person`, blank for an id that is not in residents. Only the table on this
# page uses it; the schedule itself, and what Publish saves, stay unchanged.
with_names <- function(schedule, residents) {
  name <- if (is.data.frame(residents)) {
    residents$name[match(schedule$person, residents$resident_id)]
  } else {
    rep(NA_character_, nrow(schedule))
  }
  name[is.na(name)] <- ""
  out <- schedule
  out$name <- name
  out[, c("person", "name", setdiff(names(schedule), "person")), drop = FALSE]
}

test_schedule_names <- function() {
  sched <- data.frame(person = c("P001", "X999", "P002"),
                      machine = c("A-1", "A-2", "B-1"),
                      date = "2026-11-03", start = "09:00", end = "09:30")
  residents <- data.frame(resident_id = c("P002", "P001"),
                          name = c("Dev Test", "Priya Test"),
                          email = c("d@mse.ac.in", "p@mse.ac.in"),
                          block = c("D", "B"))
  out <- with_names(sched, residents)
  stopifnot(identical(names(out), c("person", "name", "machine", "date", "start", "end")))
  stopifnot(identical(out$name, c("Priya Test", "", "Dev Test")))  # unknown id: blank
  stopifnot(identical(out$person, sched$person), nrow(out) == nrow(sched))
  # the schedule itself is not changed
  stopifnot(identical(names(sched), c("person", "machine", "date", "start", "end")))
  # no residents available: every name blank
  stopifnot(all(with_names(sched, NULL)$name == ""))
  stopifnot(nrow(with_names(sched[0, ], residents)) == 0)
  invisible(TRUE)
}

# Existing bookings dated inside the cycle.
cycle_bookings <- function(con, cycle_start, cycle_days = 10) {
  first <- as.Date(cycle_start)
  b <- read_bookings(con)
  b[b$date >= format(first) & b$date <= format(first + cycle_days - 1), ,
    drop = FALSE]
}

# clear_cycle_plan() then add_bookings(). Returns list(ok, stage, message,
# cleared, added). stage is "clear" or "add" for a failure: a "clear" failure
# changes nothing; an "add" failure leaves the cycle empty (add_bookings adds
# all rows or none), so the caller keeps the schedule and the admin retries.
publish_plan <- function(con, schedule, cycle_start, cycle_days = 10) {
  cleared <- tryCatch(clear_cycle_plan(con, cycle_start, cycle_days),
                      error = function(e) e)
  if (inherits(cleared, "error")) {
    return(list(ok = FALSE, stage = "clear", message = conditionMessage(cleared)))
  }
  added <- tryCatch(add_bookings(con, schedule, "optimiser", "booked"),
                    error = function(e) e)
  if (inherits(added, "error")) {
    return(list(ok = FALSE, stage = "add", cleared = cleared,
                message = conditionMessage(added)))
  }
  list(ok = TRUE, cleared = cleared, added = length(added))
}

# Run from the R console after source("R/data_load.R"), like the tests in
# R/mod_questionnaire.R.
test_publish_plan <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  db_init(con)
  sched <- data.frame(person = c("R001", "R002"), machine = c("A-1", "A-2"),
                      date = c("2026-11-03", "2026-11-04"),
                      start = c("09:00", "10:00"), end = c("09:30", "11:00"))
  stopifnot(nrow(cycle_bookings(con, "2026-11-02", 10)) == 0)
  r <- publish_plan(con, sched, "2026-11-02", 10)
  stopifnot(r$ok, r$added == 2, nrow(cycle_bookings(con, "2026-11-02", 10)) == 2)
  # publishing again replaces the plan, it does not duplicate it
  r <- publish_plan(con, sched, "2026-11-02", 10)
  stopifnot(r$ok, r$cleared == 2, nrow(read_bookings(con)) == 2)
  # a used cycle is refused and left untouched
  set_booking_status(con, read_bookings(con)$booking_id[1], "checked_in")
  r <- publish_plan(con, sched, "2026-11-02", 10)
  stopifnot(!r$ok, r$stage == "clear", nrow(read_bookings(con)) == 2)
  # a failure after clearing is reported as an "add" failure
  con2 <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  db_init(con2)
  publish_plan(con2, sched, "2026-11-02", 10)
  r <- publish_plan(con2, sched[, c("person", "machine", "date")],
                    "2026-11-02", 10)
  stopifnot(!r$ok, r$stage == "add", nrow(read_bookings(con2)) == 0)
  invisible(TRUE)
}

# ---- Module ------------------------------------------------------------------

mod_admin_ui <- function(id) {
  ns <- NS(id)
  tab <- function(title, out) nav_panel(title, uiOutput(ns(out)))
  div(
    class = "p-2",
    layout_columns(
      fill = FALSE, col_widths = c(12, 12),
      card(
        fill = FALSE,
        card_header("Admin"),
        div(class = "d-flex flex-wrap gap-3 align-items-end",
            dateInput(ns("cycle_start"), "Cycle start date",
                      value = DEFAULT_CYCLE_START,
                      format = "yyyy-mm-dd", width = "12rem"),
            div(class = "mb-3",
                actionButton(ns("generate"), "Generate schedule",
                             class = "btn-primary")),
            div(class = "mb-3",
                actionButton(ns("publish"), "Publish", disabled = NA)),
            div(class = "mb-3", textOutput(ns("published_state")))),
        div(class = "form-text mb-2", "Generating a schedule takes about 3 minutes."),
        checkboxInput(ns("use_demo"),
                      "Use demo data (fake survey and fake schedule, cannot be published)",
                      value = FALSE),
        uiOutput(ns("status")),
        uiOutput(ns("schedule_ui"))
      ),
      navset_card_tab(
        title = "Activity",
        tab("Queue", "queue"),
        tab("Cancellations", "changes"),
        tab("No-shows", "noshows")
      )
    )
  )
}

mod_admin_server <- function(id, con = NULL) {
  moduleServer(id, function(input, output, session) {
    schedule <- reactiveVal(NULL)
    generated <- reactiveVal(NULL)  # list(cycle_start, cycle_days, source)
    published <- reactiveVal(FALSE)
    published_msg <- reactiveVal(NULL)
    status <- reactiveVal(NULL)

    alert <- function(class, ...) div(class = paste("alert mt-3", class), ...)

    observeEvent(input$generate, {
      published(FALSE)
      published_msg(NULL)
      demo <- isTRUE(input$use_demo)
      survey <- NULL
      last_planned_df <- NULL
      penalties <- NULL
      penalty_error <- NULL
      if (!demo) {
        survey <- tryCatch(read_responses(con), error = function(e) NULL)
        if (is.null(survey) || nrow(survey) == 0) {
          schedule(NULL)
          generated(NULL)
          status(alert("alert-danger", strong("No responses yet. "),
                       if (is.null(con)) "There is no database connection. ",
                       "Residents need to submit the questionnaire first. ",
                       "To try the page without them, tick 'Use demo data'."))
          return()
        }
        last_planned_df <- tryCatch(last_planned(con, input$cycle_start),
                                    error = function(e) NULL)
        penalties <- tryCatch(
          slot_penalties(con, input$cycle_start, load_config(PATH_CONFIG)),
          error = function(e) {
            penalty_error <<- conditionMessage(e)
            NULL
          })
      }
      penalised <- if (is.data.frame(penalties)) {
        penalties[penalties$slots_lost > 0, , drop = FALSE]
      }

      res <- withProgress(
        message = "Generating schedule...", value = 0.1,
        run_pipeline(input$cycle_start, survey, last_planned_df, demo,
                     notify = function(msg) setProgress(message = msg),
                     slots_lost = penalised))
      schedule(res$schedule)
      generated(if (is.null(res$schedule)) NULL else
        list(cycle_start = as.Date(input$cycle_start),
             cycle_days = res$cycle_days, source = res$source))

      badge <- function(state) {
        cls <- c(done = "text-bg-success", failed = "text-bg-danger",
                 missing = "text-bg-secondary",
                 skipped = "text-bg-secondary")[[state]]
        span(class = paste("badge", cls), state)
      }
      rows <- lapply(seq_len(nrow(res$steps)), function(i) {
        tags$tr(tags$td(code(res$steps$step[i])),
                tags$td(badge(res$steps$state[i])),
                tags$td(class = "small", res$steps$note[i]))
      })
      status(tagList(
        switch(res$source,
          optimiser = alert("alert-success", "Schedule from the optimiser."),
          demo = alert("alert-warning", strong("Demo data. "),
                       "Fake survey and fake schedule; this cannot be published."),
          fallback = alert("alert-danger", strong("Optimiser did not produce a schedule. "),
                           res$message, " Showing the stand-in fake schedule; ",
                           "this cannot be published."),
          alert("alert-danger", res$message)),
        if (!is.null(penalty_error)) {
          alert("alert-danger", strong("Penalties could not be worked out, so nobody lost a slot: "),
                penalty_error)
        },
        if (has_rows(penalised)) {
          alert("alert-warning", strong("Slots lost this cycle:"),
                tags$ul(lapply(seq_len(nrow(penalised)), function(i) {
                  tags$li(sprintf("%s loses %d (%d cancellations, %d no-shows)",
                                  penalised$resident_id[i], penalised$slots_lost[i],
                                  penalised$cancellations[i], penalised$no_shows[i]))
                })))
        },
        if (length(res$warnings)) {
          alert("alert-warning", strong("Warnings while building free windows:"),
                tags$ul(lapply(res$warnings, tags$li)))
        },
        if (!is.null(res$problems)) {
          alert(if (nrow(res$problems) == 0) "alert-success" else "alert-danger",
                if (nrow(res$problems) == 0) "Checker: no problems found."
                else paste("Checker found", nrow(res$problems), "problem(s)."))
        },
        tags$table(class = "table table-sm w-auto",
                   tags$thead(tags$tr(tags$th("Step"), tags$th("State"),
                                      tags$th("Note"))),
                   tags$tbody(rows))
      ))
    })

    # Publish is only enabled for a schedule from the optimiser.
    observe({
      g <- generated()
      shiny::updateActionButton(session, "publish",
                                disabled = !identical(g$source, "optimiser"))
    })

    do_publish <- function() {
      g <- generated()
      res <- publish_plan(con, schedule(), g$cycle_start, g$cycle_days)
      if (res$ok) {
        published(TRUE)
        published_msg(paste0("published, ", res$added, " booking(s) saved"))
        showNotification(paste("Published", res$added, "booking(s)."),
                         type = "message")
      } else if (res$stage == "clear") {
        showNotification(paste("Not published, nothing was changed:", res$message),
                         type = "error", duration = NULL)
      } else {
        showNotification(
          paste0("The old plan was removed but the new one could not be saved: ",
                 res$message, " The schedule is kept; click Publish to retry."),
          type = "error", duration = NULL)
      }
    }

    observeEvent(input$publish, {
      g <- generated()
      if (is.null(schedule()) || !identical(g$source, "optimiser")) {
        showNotification("Only a schedule from the optimiser can be published.",
                         type = "error")
        return()
      }
      if (is.null(con)) {
        showNotification("No database connection.", type = "error")
        return()
      }
      existing <- cycle_bookings(con, g$cycle_start, g$cycle_days)
      if (nrow(existing) > 0) {
        showModal(modalDialog(
          title = "Replace the existing plan?",
          paste0("This cycle (", format(g$cycle_start), " to ",
                 format(g$cycle_start + g$cycle_days - 1), ") already has ",
                 nrow(existing), " booking(s). Publishing replaces them with ",
                 "the new plan. If any were cancelled, checked in or claimed ",
                 "as free slots, the replacement is refused."),
          footer = tagList(modalButton("Cancel"),
                           actionButton(session$ns("confirm_publish"),
                                        "Replace plan", class = "btn-danger"))))
        return()
      }
      do_publish()
    })
    observeEvent(input$confirm_publish, {
      removeModal()
      do_publish()
    })

    output$published_state <- renderText({
      if (published()) paste("Status:", published_msg()) else "Status: not published"
    })
    output$status <- renderUI(status())
    output$schedule_ui <- renderUI({
      req(schedule())
      DT::DTOutput(session$ns("schedule_table"))
    })
    output$schedule_table <- DT::renderDT({
      req(schedule())
      residents <- if (is.null(con)) NULL else {
        tryCatch(read_residents(con), error = function(e) NULL)
      }
      DT::datatable(with_names(schedule(), residents), rownames = FALSE,
                    fillContainer = FALSE)
    })

    # Activity tabs read the database tables; they refresh after a publish.
    no_data <- p(class = "text-muted", "No data yet.")
    has_rows <- function(d) is.data.frame(d) && nrow(d) > 0
    activity <- function(read) {
      reactive({
        published()
        if (is.null(con)) NULL else tryCatch(read(con), error = function(e) NULL)
      })
    }
    queue_df <- activity(read_queue)
    changes_df <- activity(read_changes)
    noshows_df <- activity(function(con) {
      b <- read_bookings(con)
      b[b$status == "no_show", , drop = FALSE]
    })
    output$queue <- renderUI(
      if (has_rows(queue_df())) tableOutput(session$ns("queue_tbl")) else no_data)
    output$queue_tbl <- renderTable(queue_df())
    output$changes <- renderUI(
      if (has_rows(changes_df())) tableOutput(session$ns("changes_tbl")) else no_data)
    output$changes_tbl <- renderTable(changes_df())
    output$noshows <- renderUI(
      if (has_rows(noshows_df())) tableOutput(session$ns("noshows_tbl")) else no_data)
    output$noshows_tbl <- renderTable(noshows_df())

    reactive(released_schedule(published(), generated()$source, schedule()))
  })
}
