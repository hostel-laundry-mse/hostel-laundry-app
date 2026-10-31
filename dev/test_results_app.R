# Demo of the resident pages with a pretend clock, for testing and presenting.
# Run from the repo folder:
#   R -e "shiny::runApp('dev/test_results_app.R')"
#
# Uses its own throw-away database in memory, filled from
# data/fake_schedule.csv, so the real data/laundry.sqlite is never touched.
# Washes that ended before the pretend time are marked as used.
#
# Things to try:
#   - R001 at Tue 03 Nov 10:05: "Started" is open for the 10:00 slot.
#   - R001's Sat 07 Nov slot: "Can't make it" -> move it or release it.
#   - Within 6 hours of a slot only "Release it" is offered.
#   - Release a slot, then sign in as someone else in that block and look at
#     Schedule -> Free now (or join the queue first and get it automatically).

if (!file.exists("dev/load_app_code.R")) setwd("..")   # runApp() starts in dev/
source("dev/load_app_code.R")
REPO <- getwd()   # Shiny switches back to dev/ for each visitor; we step back

sched <- read.csv("data/fake_schedule.csv", stringsAsFactors = FALSE)
people <- sort(unique(sched$person))
days <- format(as.Date("2026-11-02") + 0:9)

seed <- function(con, now) {
  for (t in c("changes", "queue", "bookings")) DBI::dbExecute(con, paste("DELETE FROM", t))
  res_sync_schedule(con, sched)
  b <- read_bookings(con)
  used <- b$booking_id[res_dt(b$date, b$end) < now]
  for (id in used) set_booking_status(con, id, "checked_in")
}

ui <- page_navbar(
  title = "Hostel Laundry (demo clock)",
  theme = bs_theme(version = 5),
  header = div(class = "container-fluid", style = "padding:10px 16px",
    div(style = "display:flex; gap:24px; flex-wrap:wrap; align-items:flex-end",
      selectInput("user", "Signed in as", people, selected = "R001", width = "200px"),
      selectInput("day", "Pretend date", setNames(days, res_day(days)),
                  selected = "2026-11-03", width = "200px"),
      textInput("time", "Pretend time (HH:MM)", "10:05", width = "160px"),
      div(style = "margin-bottom:16px", actionButton("reset", "Reset demo data")))),
  nav_panel("My slots", mod_results_ui("my_slots", view = "mine")),
  nav_panel("Schedule", mod_results_ui("schedule", view = "all"))
)

server <- function(input, output, session) {
  setwd(REPO)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  db_init(con)
  session$onSessionEnded(function() DBI::dbDisconnect(con))
  now <- reactive({
    req(grepl("^[0-2][0-9]:[0-5][0-9]$", input$time))
    res_dt(input$day, input$time)
  })
  seed(con, isolate(now()))
  observeEvent(input$reset, { seed(con, now()); session$reload() })
  user <- reactive(input$user)
  mod_results_server("my_slots", view = "mine", con = con, current_user = user, now = now)
  mod_results_server("schedule", view = "all", con = con, current_user = user, now = now)
}

shinyApp(ui, server)
