# Admin module: load survey/config, run the optimiser and checker, publish
# the schedule.
#
# mod_admin_ui(id)
#   Takes: module id.
#   Returns: UI for the Admin page.
#
# mod_admin_server(id, con)
#   Takes: module id, con - DBI connection from db_connect().
#   Returns: reactive giving the current schedule data.frame
#            (person, machine, day, start, end), or NULL before one is run.

mod_admin_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header("Admin"),
    p("Placeholder: data upload, optimiser run and schedule checks will go here.")
  )
}

mod_admin_server <- function(id, con = NULL) {
  moduleServer(id, function(input, output, session) {
    reactive(NULL)
  })
}
