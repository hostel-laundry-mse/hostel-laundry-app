# Admin module: pick the cycle start, run the pipeline (free windows, optimiser,
# checker), publish the schedule.
#
# mod_admin_ui(id)
#   Takes: module id.
#   Returns: UI for the Admin page.
#
# mod_admin_server(id, con)
#   Takes: module id, con - DBI connection from db_connect().
#   Returns: reactive giving the current schedule data.frame
#            (person, machine, date, start, end), or NULL before one is run.
#
# Generate runs, in order: load_survey / load_config / load_timetable,
# build_free_windows, run_optimiser, check_schedule. Each step is reported as
# done, stub (defined but returns nothing yet) or missing (not defined yet).
# If the optimiser did not produce a schedule, data/fake_schedule.csv (path in
# R/mod_questionnaire.R) is shown instead and the page says so.
# Publish sets a reactiveVal held inside the module.
# TODO: the returned reactive is not gated on "published" because the return
# value is fixed by the contract above.

DEFAULT_CYCLE_START <- as.Date("2026-11-02")

# ---- Pipeline ----------------------------------------------------------------

# "missing" if the function is not defined, "stub" if it returned nothing
# usable, otherwise "done". `fun` is looked up by name when called.
step_state <- function(fun_name, value, usable = is.data.frame) {
  if (!exists(fun_name, mode = "function")) return("missing")
  if (usable(value)) "done" else "stub"
}

call_if_defined <- function(fun_name, ...) {
  if (!exists(fun_name, mode = "function")) return(NULL)
  tryCatch(get(fun_name, mode = "function")(...), error = function(e) {
    structure(list(), error = conditionMessage(e))
  })
}

# Runs the pipeline. Returns list(schedule, source, steps, problems, status).
# `steps` is a data.frame(step, state, note) for display.
run_pipeline <- function(cycle_start) {
  steps <- data.frame(step = character(), state = character(),
                      note = character(), stringsAsFactors = FALSE)
  add <- function(step, state, note = NULL) {
    if (is.null(note)) note <- ""
    steps[nrow(steps) + 1, ] <<- list(step, state, note)
  }
  usable_list <- function(x) is.list(x) && !is.data.frame(x) && length(x) > 0

  survey <- call_if_defined("load_survey", PATH_SURVEY)
  state <- step_state("load_survey", survey)
  if (state != "done") {
    survey <- read_csv_chr(PATH_SURVEY)
    add("load_survey", state, paste("read", PATH_SURVEY, "directly"))
  } else add("load_survey", state)

  config <- call_if_defined("load_config", PATH_CONFIG)
  state <- step_state("load_config", config, usable_list)
  if (state != "done") {
    config <- read_csv_chr(PATH_CONFIG)  # the optimiser also takes the raw table
    add("load_config", state, paste("passing", PATH_CONFIG, "as a table"))
  } else add("load_config", state)

  timetable <- call_if_defined("load_timetable", PATH_TIMETABLE)
  state <- step_state("load_timetable", timetable)
  if (state != "done") {
    timetable <- read_csv_chr(PATH_TIMETABLE)
    add("load_timetable", state, paste("read", PATH_TIMETABLE, "directly"))
  } else add("load_timetable", state)

  free <- call_if_defined("build_free_windows", survey, timetable, config,
                          cycle_start)
  state <- step_state("build_free_windows", free)
  add("build_free_windows", state,
      if (state != "done") "optimiser cannot run without free windows")

  schedule <- NULL
  optimiser_note <- NULL
  if (state == "done") {
    res <- call_if_defined("run_optimiser", survey, free, config, cycle_start,
                           read_csv_chr(PATH_LAST_PLANNED), verbose = FALSE)
    if (is.list(res) && is.data.frame(res$schedule)) {
      schedule <- res$schedule
      optimiser_note <- paste0(res$status$message,
                               if (is.data.frame(res$underserved)) {
                                 paste0(" Underserved: ", nrow(res$underserved), ".")
                               })
    }
  }
  opt_state <- if (!exists("run_optimiser", mode = "function")) "missing"
               else if (is.data.frame(schedule)) "done" else "stub"
  add("run_optimiser", opt_state, optimiser_note)

  source_label <- "optimiser"
  if (!is.data.frame(schedule)) {
    schedule <- read_csv_chr(PATH_FAKE_SCHEDULE)
    source_label <- "fake"
  }

  problems <- call_if_defined("check_schedule", schedule, survey, config)
  state <- step_state("check_schedule", problems)
  add("check_schedule", state,
      if (state == "done") paste(nrow(problems), "problem(s)"))

  list(schedule = schedule, source = source_label, steps = steps,
       problems = if (is.data.frame(problems)) problems else NULL)
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
            div(class = "mb-3", actionButton(ns("publish"), "Publish")),
            div(class = "mb-3", textOutput(ns("published_state")))),
        uiOutput(ns("status")),
        uiOutput(ns("schedule_ui"))
      ),
      navset_card_tab(
        title = "Activity",
        tab("Queue", "queue"),
        tab("Changes", "changes"),
        tab("No-shows", "noshows")
      )
    )
  )
}

mod_admin_server <- function(id, con = NULL) {
  moduleServer(id, function(input, output, session) {
    schedule <- reactiveVal(NULL)
    published <- reactiveVal(FALSE)
    status <- reactiveVal(NULL)

    observeEvent(input$generate, {
      published(FALSE)
      res <- run_pipeline(input$cycle_start)
      schedule(res$schedule)

      badge <- function(state) {
        cls <- c(done = "text-bg-success", stub = "text-bg-warning",
                 missing = "text-bg-secondary")[[state]]
        span(class = paste("badge", cls), state)
      }
      rows <- lapply(seq_len(nrow(res$steps)), function(i) {
        tags$tr(tags$td(code(res$steps$step[i])),
                tags$td(badge(res$steps$state[i])),
                tags$td(class = "small", res$steps$note[i]))
      })
      status(tagList(
        if (res$source == "fake") {
          div(class = "alert alert-warning mt-3",
              "The optimiser did not produce a schedule, so this is the ",
              "stand-in ", code(PATH_FAKE_SCHEDULE), ".")
        } else {
          div(class = "alert alert-success mt-3", "Schedule from the optimiser.")
        },
        if (!is.null(res$problems)) {
          div(class = if (nrow(res$problems) == 0) "alert alert-success"
                      else "alert alert-danger",
              if (nrow(res$problems) == 0) "Checker: no problems found."
              else paste("Checker found", nrow(res$problems), "problem(s)."))
        },
        tags$table(class = "table table-sm w-auto",
                   tags$thead(tags$tr(tags$th("Step"), tags$th("State"),
                                      tags$th("Note"))),
                   tags$tbody(rows))
      ))
    })

    observeEvent(input$publish, {
      if (is.null(schedule())) {
        showNotification("Generate a schedule first.", type = "error")
        return()
      }
      published(TRUE)
    })

    output$published_state <- renderText({
      if (published()) "Status: published" else "Status: not published"
    })
    output$status <- renderUI(status())
    output$schedule_ui <- renderUI({
      req(schedule())
      DT::DTOutput(session$ns("schedule_table"))
    })
    output$schedule_table <- DT::renderDT({
      req(schedule())
      DT::datatable(schedule(), rownames = FALSE, fillContainer = FALSE)
    })

    # TODO: Queue and Changes show the `queue` and `changes` tables once they
    # exist. No-shows are bookings with status "no_show" in `bookings`.
    no_data <- p(class = "text-muted", "No data yet.")
    table_ui <- function(table, output_id) {
      if (!is.null(con) && DBI::dbExistsTable(con, table)) {
        tableOutput(session$ns(output_id))
      } else no_data
    }
    output$queue <- renderUI(table_ui("queue", "queue_tbl"))
    output$queue_tbl <- renderTable(DBI::dbReadTable(con, "queue"))
    output$changes <- renderUI(table_ui("changes", "changes_tbl"))
    output$changes_tbl <- renderTable(DBI::dbReadTable(con, "changes"))
    output$noshows <- renderUI({
      if (!is.null(con) && DBI::dbExistsTable(con, "bookings")) {
        tableOutput(session$ns("noshows_tbl"))
      } else no_data
    })
    output$noshows_tbl <- renderTable(DBI::dbGetQuery(
      con, "SELECT * FROM bookings WHERE status = 'no_show'"))

    schedule
  })
}
