# Results module: shows a schedule, either one resident's slots ("mine") or
# the full schedule for all machines ("all").
#
# mod_results_ui(id, view = c("mine", "all"))
#   Takes: module id, which view to show.
#   Returns: UI for the My slots page ("mine") or the Schedule page ("all").
#
# mod_results_server(id, schedule, view = c("mine", "all"))
#   Takes: module id, schedule - reactive data.frame (person, machine, day,
#          start, end), which view to show.
#   Returns: nothing (output only).

mod_results_ui <- function(id, view = c("mine", "all")) {
  view <- match.arg(view)
  ns <- NS(id)
  card(
    card_header(if (view == "mine") "My slots" else "Schedule"),
    p(if (view == "mine") {
      "Placeholder: your booked laundry slots will appear here."
    } else {
      "Placeholder: the full machine schedule will appear here."
    })
  )
}

mod_results_server <- function(id, schedule, view = c("mine", "all")) {
  view <- match.arg(view)
  moduleServer(id, function(input, output, session) {
    invisible(NULL)
  })
}
