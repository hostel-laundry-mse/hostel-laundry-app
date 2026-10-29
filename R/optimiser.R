# Schedule optimiser (ompr + GLPK).
#
# optimise_schedule(survey, config)
#   Takes: survey  - data.frame from load_survey()
#          config  - list from load_config()
#   Returns: schedule data.frame with columns person, machine, day, start, end
#            (see CLAUDE.md "Schedule format"). One row per assigned load.

optimise_schedule <- function(survey, config) {
  NULL
}
