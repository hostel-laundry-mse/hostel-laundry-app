# Schedule checker: validates a schedule against the survey and config.
#
# check_schedule(schedule, survey, config)
#   Takes: schedule - data.frame (person, machine, day, start, end)
#          survey   - data.frame from load_survey()
#          config   - list from load_config()
#   Returns: data.frame of problems with columns rule, person, machine, day,
#            message. Zero rows means the schedule is valid. Rules to check:
#            machine double-booking, outside opening hours, outside a person's
#            availability, wrong block, wrong program duration, wrong number
#            of loads per person.

check_schedule <- function(schedule, survey, config) {
  NULL
}
