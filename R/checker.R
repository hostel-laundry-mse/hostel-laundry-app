# Schedule checker: validates a schedule against the rules in CLAUDE.md.
#
# check_schedule(schedule, survey, config, timetable = load_timetable(),
#                cycle_start = NULL, last_planned = NULL, slots_lost = NULL)
#   Takes: schedule     - data.frame (person, machine, date, start, end)
#          survey       - data.frame from load_survey()
#          config       - list from load_config()
#          timetable    - data.frame from load_timetable(). The default is the
#                         fake timetable, so pass the real one for real data.
#          cycle_start  - first date of the cycle (Date or "YYYY-MM-DD").
#                         NULL means the earliest date in the schedule, which
#                         is later than the true start if nobody washes on
#                         day 1, so pass it whenever it is known.
#          last_planned - optional data.frame(resident_id, date) from
#                         last_planned(). NULL skips the cross-cycle rules.
#          slots_lost   - optional data.frame(resident_id, slots_lost) from
#                         slot_penalties(): those people are expected to have
#                         that many fewer bookings.
#   Returns: data.frame of problems, one row per problem, ordered by row, with
#            columns rule, person, machine, date, row, message. Zero rows means
#            the schedule is valid. row is the schedule row number; for a rule
#            about two bookings it is the later one, and the message names the
#            other. machine, date and row are NA for slot_count.
#   Rules:   invalid_row (date or time unreadable; that row's other checks are
#            skipped), unknown_person, wrong_block, no_such_machine,
#            out_of_order, hours, grid, duration, out_of_cycle, lecture, margin
#            (inside a lecture's margin but not the lecture), blocked,
#            double_booking, buffer_overlap, slot_count, gap_short, gap_long,
#            second_before_day6, cross_too_soon, cross_too_late.
#   Not checked: the emergency reserve, peak target, check-ins, changes and the
#            10-hour blocked limit.

check_schedule <- function(schedule, survey, config, timetable = load_timetable(),
                           cycle_start = NULL, last_planned = NULL,
                           slots_lost = NULL) {
  require_columns(schedule, c("person", "machine", "date", "start", "end"),
                  "check_schedule()")
  rule <- function(name, default) {
    x <- config$rules[[name]]
    if (is.null(x)) default else x
  }
  grid_min <- rule("grid_min", 30)
  cycle_days <- rule("cycle_days", 10)
  gap_min <- rule("gap_min_days", 4)
  gap_max <- rule("gap_max_days", 5)
  margin <- rule("lecture_margin_min", 30)
  slots <- rule("slots_per_cycle", 2)
  second_min_day <- rule("second_slot_min_day", cycle_days - gap_max + 1)
  buffer <- if (is.null(config$hours$buffer_min)) 10 else config$hours$buffer_min
  open_min <- parse_hhmm(config$hours$open, "hours,open")
  close_min <- parse_hhmm(config$hours$close, "hours,close")

  bookings <- data.frame(
    row = seq_len(nrow(schedule)),
    person = as.character(schedule$person),
    machine = as.character(schedule$machine),
    date = as.character(schedule$date),
    start = as.character(schedule$start),
    end = as.character(schedule$end)
  )
  bookings$day <- as.Date(bookings$date, format = "%Y-%m-%d")
  bookings$start_min <- checker_minutes(bookings$start)
  bookings$end_min <- checker_minutes(bookings$end)
  readable <- !is.na(bookings$day) & !is.na(bookings$start_min) &
    !is.na(bookings$end_min)
  # An end at or before the start runs past midnight, so past closing time.
  past_midnight <- readable & bookings$end_min <= bookings$start_min
  bookings$end_min[past_midnight] <- bookings$end_min[past_midnight] + 1440
  # The time the machine is held: the wash plus buffer, up to a whole block.
  bookings$held_until <- bookings$start_min + grid_min *
    ceiling((bookings$end_min - bookings$start_min + buffer) / grid_min)

  first_day <- if (!is.null(cycle_start)) {
    parse_cycle_start(cycle_start)
  } else if (any(readable)) {
    min(bookings$day[readable])
  } else {
    as.Date(NA)
  }

  found <- list()
  add <- function(rule, b, message) {
    found[[length(found) + 1]] <<- data.frame(
      rule = rule, person = b$person, machine = b$machine, date = b$date,
      row = b$row, message = message
    )
  }
  add_person <- function(rule, person, message) {
    found[[length(found) + 1]] <<- data.frame(
      rule = rule, person = person, machine = NA_character_,
      date = NA_character_, row = NA_integer_, message = message
    )
  }

  for (i in which(!readable)) {
    b <- bookings[i, ]
    add("invalid_row", b, paste0("Row ", b$row, ": unreadable date or time (",
                                 b$date, " ", b$start, "-", b$end, ")"))
  }

  for (i in which(readable)) {
    b <- bookings[i, ]
    resident <- survey[match(b$person, survey$resident_id), ]
    known <- !is.na(resident$resident_id)
    label <- paste0(b$person, " ", b$date, " ", b$start, "-", b$end)
    weekday <- date_weekday(b$day)

    if (!known) add("unknown_person", b, paste0(b$person, " is not in the survey"))

    machine_block <- sub("-[0-9]+$", "", b$machine)
    machine_number <- suppressWarnings(as.integer(sub("^.*-", "", b$machine)))
    block_row <- match(machine_block, config$machines$block)
    machine_exists <- !is.na(block_row) && !is.na(machine_number) &&
      machine_number >= 1 && machine_number <= config$machines$n_machines[block_row]
    if (known && machine_block != resident$block) {
      add("wrong_block", b, paste0(b$person, " lives in block ", resident$block,
                                   " but is on machine ", b$machine))
    } else if (!machine_exists) {
      add("no_such_machine", b, paste0("Machine ", b$machine, " does not exist"))
    } else if (b$machine %in% config$out_of_order) {
      add("out_of_order", b, paste0("Machine ", b$machine, " is out of order"))
    }

    if (!weekday %in% config$hours$days) {
      add("hours", b, paste0(label, ": the laundry is closed on ", weekday))
    } else if (b$start_min < open_min || b$end_min > close_min) {
      add("hours", b, paste0(label, " is outside opening hours ",
                             config$hours$open, "-", config$hours$close))
    } else if (b$held_until > close_min) {
      # As the optimiser and resident pages: the buffer must fit before closing.
      add("hours", b, paste0(label, " holds the machine until ",
                             format_hhmm(b$held_until %% 1440), " with its buffer, past closing at ",
                             config$hours$close))
    }

    if ((b$start_min - open_min) %% grid_min != 0) {
      add("grid", b, paste0(label, " does not start on a ", grid_min,
                            "-minute block"))
    }

    if (is.na(first_day) || b$day < first_day ||
        b$day > first_day + cycle_days - 1) {
      add("out_of_cycle", b, paste0(label, " is outside the cycle starting ",
                                    format(first_day)))
    }

    if (!known) next

    duration <- config$programs$duration_min[
      match(resident$program, config$programs$program)]
    if (is.na(duration)) {
      add("duration", b, paste0("Program '", resident$program,
                                "' is not in config"))
    } else if (b$end_min - b$start_min != duration) {
      add("duration", b, paste0(label, " lasts ", b$end_min - b$start_min,
                                " min but ", resident$program, " takes ",
                                duration))
    }

    lectures <- timetable[timetable$course == resident$course &
                            timetable$day == weekday, ]
    lecture_start <- checker_minutes(lectures$start)
    lecture_end <- checker_minutes(lectures$end)
    if (any(b$start_min < lecture_end & lecture_start < b$end_min)) {
      add("lecture", b, paste0(label, " is during a ", resident$course,
                               " lecture"))
    } else if (any(b$start_min < lecture_end + margin &
                   lecture_start - margin < b$end_min)) {
      add("margin", b, paste0(label, " is within ", margin, " minutes of a ",
                              resident$course, " lecture"))
    }

    blocked <- parse_blocked(resident$blocked, resident$resident_id)
    blocked <- blocked[blocked$day == weekday, ]
    if (any(b$start_min < blocked$end & blocked$start < b$end_min)) {
      add("blocked", b, paste0(label, " is during ", b$person,
                               "'s blocked time"))
    }
  }

  # Two bookings on one machine and date.
  usable <- bookings[readable, ]
  for (same in split(usable, paste(usable$machine, usable$date))) {
    pairs <- if (nrow(same) > 1) utils::combn(nrow(same), 2) else matrix(0, 2, 0)
    for (k in seq_len(ncol(pairs))) {
      a <- same[pairs[1, k], ]
      b <- same[pairs[2, k], ]
      washes_overlap <- a$start_min < b$end_min && b$start_min < a$end_min
      held_overlap <- a$start_min < b$held_until && b$start_min < a$held_until
      with_other <- paste0(b$machine, " on ", b$date, ": row ", a$row, " (",
                           a$person, " ", a$start, "-", a$end, ") and row ",
                           b$row, " (", b$person, " ", b$start, "-", b$end, ")")
      if (washes_overlap) {
        add("double_booking", b, paste(with_other, "overlap"))
      } else if (held_overlap) {
        add("buffer_overlap", b, paste(with_other, "overlap once the",
                                       buffer, "minute buffer is included"))
      }
    }
  }

  # Each person's bookings together.
  for (person in survey$resident_id) {
    n_booked <- sum(bookings$person == person)
    lost <- if (is.null(slots_lost)) 0 else {
      sum(slots_lost$slots_lost[slots_lost$resident_id == person])
    }
    allowed <- max(slots - lost, 0)
    if (n_booked != allowed) {
      add_person("slot_count", person, paste0(person, " has ", n_booked,
                                              " booking(s), not ", allowed))
    }

    own <- usable[usable$person == person, ]
    own <- own[order(own$day, own$start_min), ]
    if (nrow(own) == 0) next

    for (k in seq_len(nrow(own))[-1]) {
      gap <- as.numeric(own$day[k] - own$day[k - 1])
      between <- paste0(person, "'s bookings on ", own$date[k - 1], " and ",
                        own$date[k], " are ", gap, " day(s) apart")
      if (gap < gap_min) {
        add("gap_short", own[k, ], paste0(between, ", under ", gap_min))
      } else if (gap > gap_max) {
        add("gap_long", own[k, ], paste0(between, ", over ", gap_max))
      }
    }

    if (nrow(own) >= 2 && !is.na(first_day)) {
      second_day <- as.numeric(own$day[2] - first_day) + 1
      if (second_day < second_min_day) {
        add("second_before_day6", own[2, ],
            paste0(person, "'s second booking is on day ", second_day,
                   ", before day ", second_min_day))
      }
    }

    if (!is.null(last_planned)) {
      previous <- as.Date(last_planned$date[last_planned$resident_id == person])
      if (length(previous) > 0) {
        previous <- max(previous)
        gap <- as.numeric(own$day[1] - previous)
        between <- paste0(person, "'s first booking is ", gap,
                          " day(s) after the last cycle's ", format(previous))
        if (gap < gap_min) {
          add("cross_too_soon", own[1, ], paste0(between, ", under ", gap_min))
        } else if (gap > gap_max && previous + gap_max >= first_day) {
          # Skipped when the 4-5 day window ended before this cycle began, as
          # after a penalty: no date in this cycle could meet it.
          add("cross_too_late", own[1, ], paste0(between, ", over ", gap_max))
        }
      }
    }
  }

  empty <- data.frame(rule = character(), person = character(),
                      machine = character(), date = character(),
                      row = integer(), message = character())
  problems <- do.call(rbind, c(list(empty), found))
  problems <- problems[order(problems$row, problems$person, problems$rule), ]
  rownames(problems) <- NULL
  problems
}

# "HH:MM" to minutes after midnight, NA if it can't be read.
checker_minutes <- function(x) {
  vapply(x, function(one) {
    tryCatch(as.numeric(parse_hhmm(one, "")), error = function(e) NA_real_)
  }, numeric(1), USE.NAMES = FALSE)
}
