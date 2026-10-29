# Questionnaire module: residents enter their block, room, program, loads per
# week and availability windows.
#
# mod_questionnaire_ui(id)
#   Takes: module id.
#   Returns: UI for the Questionnaire page.
#
# mod_questionnaire_server(id, con)
#   Takes: module id, con - DBI connection from db_connect().
#   Returns: reactive giving the latest submitted response as a one-row
#            data.frame in the survey format (see CLAUDE.md).

mod_questionnaire_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header("Questionnaire"),
    p("Placeholder: the laundry preferences form will go here.")
  )
}

mod_questionnaire_server <- function(id, con = NULL) {
  moduleServer(id, function(input, output, session) {
    reactive(NULL)
  })
}
