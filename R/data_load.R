# Data loading and storage.
#
# load_survey(path)
#   Takes: path to a survey CSV (see CLAUDE.md "Survey data format").
#   Returns: data.frame with one row per resident, columns resident_id, name,
#            block, room, program, loads_per_week, availability.
#
# load_config(path)
#   Takes: path to config.csv (columns section, key, value).
#   Returns: list(machines = data.frame(block, n_machines),
#                 programs = data.frame(program, duration_min),
#                 hours    = list(open, close, days, slot_step_min)).
#
# db_connect(path)
#   Takes: path to an SQLite file.
#   Returns: a DBI connection (RSQLite) used to store responses and schedules.

load_survey <- function(path = "data/fake_survey.csv") {
  NULL
}

load_config <- function(path = "data/config.csv") {
  NULL
}

db_connect <- function(path = "laundry.sqlite") {
  NULL
}
