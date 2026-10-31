# Hostel laundry slot scheduler: joins the modules in R/ into one app.
# Files in R/ are sourced automatically by shiny::runApp().

library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(DT)
library(ggplot2)
library(readr)
library(dplyr)

ui <- page_navbar(
  title = "Hostel Laundry",
  theme = bs_theme(version = 5),
  nav_panel("Questionnaire", mod_questionnaire_ui("questionnaire")),
  nav_panel("My slots", mod_results_ui("my_slots", view = "mine")),
  nav_panel("Schedule", mod_results_ui("schedule", view = "all")),
  nav_panel("Admin", mod_admin_ui("admin"))
)

server <- function(input, output, session) {
  con <- db_connect()
  session$onSessionEnded(function() DBI::dbDisconnect(con))

  # No login yet: current_user is NULL, so the resident pages show their own
  # "Signed in as" dropdown. Pass a reactive resident_id here once login exists.
  current_user <- NULL

  mod_questionnaire_server("questionnaire", con)
  schedule <- mod_admin_server("admin", con)
  mod_results_server("my_slots", schedule, view = "mine", con = con,
                     current_user = current_user)
  mod_results_server("schedule", schedule, view = "all", con = con,
                     current_user = current_user)
}

shinyApp(ui, server)
