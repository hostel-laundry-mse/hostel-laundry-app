# Schedule optimiser and slot rules (ompr + GLPK).
#
# run_optimiser(survey, free_windows, config, cycle_start, last_planned = NULL,
#               weights = default_weights(), time_limit = 50, verbose = TRUE,
#               slots_lost = NULL)
#   Takes: survey       - data.frame in the survey format (CLAUDE.md)
#          free_windows - data.frame(resident_id, date, from, to) from
#                         build_free_windows(): when each person is free
#          config       - the list from load_config() or the raw
#                         data/config.csv table (columns section, key, value)
#          cycle_start  - first date of the 10-day cycle (Date or "YYYY-MM-DD")
#          last_planned - optional data.frame(resident_id, date): each
#                         person's last optimiser-planned booking date in the
#                         previous cycle, for the 4-5 day gap across cycles
#          weights      - objective weights, see default_weights()
#          time_limit   - seconds GLPK may spend on each hostel block
#          slots_lost   - optional data.frame(resident_id, slots_lost) from
#                         slot_penalties(): how many of their 2 slots each
#                         person loses this cycle. Someone losing 2 gets none.
#   Returns: list(
#     schedule    = data.frame(person, machine, date, start, end), the
#                   schedule format in CLAUDE.md, one row per booking,
#     underserved = data.frame(person, name, block, slots_cap, slots_given,
#                   reason), everyone who got fewer bookings than their
#                   allowance (slots_cap, which already takes off slots_lost),
#     status      = list(code, message, per_block), code is "optimal",
#                   "not proven optimal" (a block hit the time limit or
#                   failed) or "error",
#     diagnostics = list(candidates, variables, constraints, build_s,
#                   solve_s, objective, utilisation, gap_relaxed, penalised,
#                   per_block), penalised is data.frame(resident_id, slots_cap)
#                   of everyone with a reduced allowance. status$per_block has
#                   a stage column, "real" or "simulated", and a block with
#                   simulated residents has one row for each. The status message
#                   counts how many underserved people are simulated.
#
# optimise_schedule(survey, free_windows, config, cycle_start, last_planned)
#   Same inputs. Returns only the schedule data.frame.
#
# How the model works
#   Time: each open date of the cycle is cut into blocks of grid_min
#   minutes. A booking starts on a block and holds the machine for
#   ceiling((program + buffer) / grid_min) blocks.
#   Candidates: every allowed (person, date, start block). Only allowed
#   combinations get a decision variable, so block access, opening hours and
#   free time (lectures, blocked times) are enforced by build_candidates()
#   and never appear as constraints.
#   Model: x[c] = 1 if candidate c is booked. It decides who washes when and
#   limits how many working machines of the block are busy at once.
#   Machines in a block are identical, so specific machines are given out
#   afterwards by assign_machines(), which always succeeds when that limit
#   holds. Hostel blocks share no machines, so each block is its own model.
#   Two stages: in each block the real residents are solved alone, exactly as if
#   nobody simulated (is_simulated_id()) existed. The simulated residents are
#   then solved around those bookings, which never move, so they may end up
#   underserved. That is reported, not an error.
#
# The config list from load_config() is read with these names (all but
# machines, programs and hours$open/close/days are optional):
#   machines     data.frame(block, n_machines)
#   programs     data.frame(program, duration_min)
#   hours        list(open, close, days, buffer_min, normal_start, normal_end,
#                odd_start, odd_end)
#   rules        named list of the rules,* rows in config.csv
#   out_of_order character vector of machine ids, e.g. "C-2"

library(dplyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

# Agreed rule: everyone gets 2 bookings per cycle, less any slots lost to
# penalties. The gap constraints in build_model() are written for at most 2.
SLOTS_PER_CYCLE <- 2L

# Preference points for a wash inside normal hours. A preferred weekday is worth
# 1, so the time of day counts for more than the day.
NORMAL_HOURS_POINTS <- 2L

# Objective weights.
#   pref:   value of one preference point (scores are 1 to 4, less the odd-hours
#           penalty).
#   peak:   cost of one booked machine-minute above peak_target in an hour.
#   assign: value of one booking. NULL means "set automatically" to
#           spread * pref * A_max + peak * A_max * longest booking minutes + 1,
#           where A_max is the most bookings the stage could get and spread is
#           the highest score less the lowest, counting 0 as a score because a
#           booking with a negative score must still beat no booking. Swapping
#           bookings around can raise total preference by at most spread per
#           booking and lower total overload by at most the booked minutes,
#           so with this value one extra booking always beats any preference
#           or peak gain: nobody loses a booking to improve someone else's
#           preferences.
default_weights <- function() {
  list(assign = NULL, pref = 1, peak = 0.02)
}

optimise_schedule <- function(survey, free_windows, config, cycle_start,
                              last_planned = NULL) {
  run_optimiser(survey, free_windows, config, cycle_start, last_planned,
                verbose = FALSE)$schedule
}

run_optimiser <- function(survey, free_windows, config, cycle_start,
                          last_planned = NULL, weights = default_weights(),
                          time_limit = 50, verbose = TRUE, slots_lost = NULL) {
  result <- tryCatch(
    run_optimiser_unsafe(survey, free_windows, config, cycle_start,
                         last_planned, weights, time_limit, slots_lost),
    error = function(e) {
      list(
        schedule = empty_schedule(),
        underserved = data.frame(),
        status = list(
          code = "error",
          message = paste("The optimiser could not run:", conditionMessage(e)),
          per_block = data.frame()
        ),
        diagnostics = list()
      )
    }
  )
  if (verbose) print_diagnostics(result)
  result
}

run_optimiser_unsafe <- function(survey, free_windows, config, cycle_start,
                                 last_planned, weights, time_limit,
                                 slots_lost = NULL) {
  cfg <- normalise_config(config)
  grid <- generate_grid(cfg, cycle_start)
  people <- prepare_people(survey, cfg, grid, last_planned, slots_lost)
  candidates <- build_candidates(people, free_windows, cfg, grid)
  people <- mark_gap_relaxed(people, candidates, cfg)

  # Solve each hostel block separately: blocks share no machines.
  booked_real <- list()
  booked_sim <- list()
  per_block <- list()
  for (b in cfg$machines$block) {
    cand_b <- filter(candidates, block == b)
    people_b <- filter(people, block == b, valid)
    sim_cand <- is_simulated_id(cand_b$resident_id)
    sim_people <- is_simulated_id(people_b$resident_id)
    real <- solve_block(cand_b[!sim_cand, ], people_b[!sim_people, ], cfg, grid,
                        weights, time_limit)
    booked_real[[b]] <- real$booked
    per_block[[length(per_block) + 1]] <- data.frame(block = b, stage = "real", real$info)
    if (any(sim_people)) {
      sim <- solve_block(cand_b[sim_cand, ], people_b[sim_people, ], cfg, grid,
                         weights, time_limit, fixed = real$booked)
      booked_sim[[b]] <- sim$booked
      per_block[[length(per_block) + 1]] <- data.frame(block = b, stage = "simulated", sim$info)
    }
  }
  per_block <- bind_rows(per_block)
  booked <- bind_rows(candidates[0, ], unname(booked_real))
  booked <- assign_machines(booked, grid)
  booked <- assign_around(bind_rows(candidates[0, ], unname(booked_sim)), booked, grid)

  schedule <- extract_schedule(rep(1, nrow(booked)), booked)
  underserved <- find_underserved(booked, candidates, people, cfg)

  not_optimal <- unique(per_block$block[!per_block$status %in% c("optimal", "no candidates")])
  code <- if (length(not_optimal) == 0) "optimal" else "not proven optimal"
  message <- paste0(
    if (code == "optimal") "Optimal schedule found: " else
      paste0("Block(s) ", paste(not_optimal, collapse = ", "),
             " were not solved to optimality (see per_block). "),
    nrow(schedule), " bookings for ", n_distinct(schedule$person), " of ",
    nrow(people), " people; ", nrow(underserved),
    " people got fewer than their allowance (",
    sum(is_simulated_id(underserved$person)), " simulated); ",
    sum(people$cap < SLOTS_PER_CYCLE), " penalised."
  )

  list(
    schedule = schedule,
    underserved = underserved,
    status = list(code = code, message = message, per_block = per_block),
    diagnostics = list(
      candidates = nrow(candidates),
      variables = sum(per_block$variables),
      constraints = sum(per_block$constraints),
      build_s = sum(per_block$build_s),
      solve_s = sum(per_block$solve_s),
      objective = sum(per_block$objective, na.rm = TRUE),
      utilisation = utilisation(booked, cfg, grid),
      gap_relaxed = people$resident_id[people$gap_relaxed],
      penalised = as.data.frame(select(filter(people, cap < SLOTS_PER_CYCLE),
                                       resident_id, slots_cap = cap)),
      per_block = per_block
    )
  )
}

# ---------------------------------------------------------------------------
# Inputs
# ---------------------------------------------------------------------------

# Converts either config form into one flat list with times in minutes after
# midnight. Optional settings get their defaults here.
normalise_config <- function(config) {
  if (is.data.frame(config)) {
    tab <- mutate(config, across(everything(), as.character))
    get <- function(s) filter(tab, section == s)
    m <- get("machines")
    p <- get("program")
    h <- get("hours")
    h <- setNames(as.list(h$value), h$key)
    r <- get("rules")
    config <- list(
      machines = data.frame(block = m$key, n_machines = as.integer(m$value)),
      programs = data.frame(program = p$key, duration_min = as.numeric(p$value)),
      hours = list(open = h$open, close = h$close,
                   days = strsplit(h$days, ";")[[1]],
                   buffer_min = h$buffer_min, normal_start = h$normal_start,
                   normal_end = h$normal_end, odd_start = h$odd_start,
                   odd_end = h$odd_end),
      rules = setNames(as.list(r$value), r$key),
      out_of_order = get("out_of_order")$key
    )
  }

  opt <- function(x, default) {
    if (is.null(x) || length(x) == 0 || is.na(x[[1]]) || x[[1]] == "") default
    else as.numeric(x[[1]])
  }
  hm_or <- function(x, default) hm_to_min(if (is.null(x)) default else x)
  rules <- config$rules
  days <- config$hours$days
  if (length(days) == 1) days <- strsplit(days, ";")[[1]]

  list(
    machines = mutate(config$machines, block = as.character(block),
                      n_machines = as.integer(n_machines)),
    programs = config$programs,
    open = hm_to_min(config$hours$open),
    close = hm_to_min(config$hours$close),
    days = days,
    buffer_min = opt(config$hours$buffer_min, 10),
    grid_min = opt(rules$grid_min, 30),
    cycle_days = opt(rules$cycle_days, 10),
    gap_min = opt(rules$gap_min_days, 4),
    gap_max = opt(rules$gap_max_days, 5),
    reserve_share = opt(rules$reserve_share, 0),
    peak_target = opt(rules$peak_target, NA),
    normal_start = hm_or(config$hours$normal_start, "14:00"),
    normal_end = hm_or(config$hours$normal_end, "23:00"),
    odd_start = hm_or(config$hours$odd_start, "00:00"),
    odd_end = hm_or(config$hours$odd_end, "07:00"),
    odd_penalty_real = opt(rules$odd_penalty_real, 8),
    odd_penalty_simulated = opt(rules$odd_penalty_simulated, 2),
    out_of_order = as.character(config$out_of_order)
  )
}

# Adds program duration, booking length in blocks, the cap and the previous
# cycle's last planned day (as a day number of this cycle, so 0 or less) to
# each person. The cap is 2 less any slots lost to penalties. People whose
# block or program is not in config are marked not valid.
prepare_people <- function(survey, cfg, grid, last_planned = NULL, slots_lost = NULL) {
  if (!"pref_days" %in% names(survey)) survey$pref_days <- NA_character_
  lost <- data.frame(resident_id = character(), lost = numeric())
  if (!is.null(slots_lost) && nrow(slots_lost) > 0) {
    lost <- transmute(slots_lost, resident_id = as.character(resident_id),
                      lost = as.numeric(slots_lost))
  }
  last <- data.frame(resident_id = character(), last_idx = numeric())
  if (!is.null(last_planned) && nrow(last_planned) > 0) {
    last <- last_planned |>
      transmute(resident_id = as.character(resident_id),
                last_idx = as.numeric(as.Date(date) - grid$start) + 1) |>
      group_by(resident_id) |>
      summarise(last_idx = max(last_idx), .groups = "drop")
  }
  survey |>
    mutate(across(c(resident_id, block, program), as.character)) |>
    distinct(resident_id, .keep_all = TRUE) |>
    left_join(cfg$programs, by = "program") |>
    left_join(last, by = "resident_id") |>
    left_join(lost, by = "resident_id") |>
    mutate(
      cells = grid$cells_needed(duration_min),
      cap = pmax(SLOTS_PER_CYCLE - coalesce(lost, 0), 0),
      gap_relaxed = FALSE,
      valid = block %in% cfg$machines$block & !is.na(duration_min),
      problem = case_when(
        !block %in% cfg$machines$block ~ paste0("block '", block, "' is not in config"),
        is.na(duration_min) ~ paste0("program '", program, "' is not in config"),
        TRUE ~ NA_character_
      )
    ) |>
    arrange(resident_id)
}

# ---------------------------------------------------------------------------
# Time structure
# ---------------------------------------------------------------------------

# The only place the time structure is defined. The cycle runs cycle_days
# dates from cycle_start; dates whose weekday is not an open day are left
# out. Each date is cut into n_cells blocks of grid_min minutes from opening
# time. Machines and their numbers come only from config. Returns:
#   start        cycle_start as a Date
#   dates        one row per open date: date, day_idx (1 = cycle_start),
#                weekday
#   machines     one row per machine: machine, block, machine_no, in_order
#   working      number of working machines per block (named vector)
#   n_cells      blocks per date
#   cell_start   function(cell) -> minutes after midnight (blocks are 0-based)
#   cells_needed function(duration) -> blocks a booking of that program
#                occupies, buffer included
generate_grid <- function(cfg, cycle_start) {
  g <- cfg$grid_min
  start <- as.Date(cycle_start)
  all_dates <- start + seq_len(cfg$cycle_days) - 1
  dates <- data.frame(
    date = all_dates,
    day_idx = seq_along(all_dates),
    weekday = weekday_name(all_dates)
  ) |>
    filter(weekday %in% cfg$days)
  machines <- cfg$machines |>
    rowwise() |>
    reframe(block = block, machine_no = seq_len(n_machines)) |>
    mutate(
      machine = paste0(block, "-", machine_no),
      in_order = !machine %in% cfg$out_of_order
    )
  working <- vapply(cfg$machines$block, function(b) {
    sum(machines$in_order & machines$block == b)
  }, integer(1))

  list(
    start = start,
    dates = dates,
    machines = machines,
    working = working,
    n_cells = floor((cfg$close - cfg$open) / g),
    cell_start = function(cell) cfg$open + cell * g,
    cells_needed = function(duration) ceiling((duration + cfg$buffer_min) / g)
  )
}

# ---------------------------------------------------------------------------
# Candidates
# ---------------------------------------------------------------------------

# Every allowed (person, date, start block). A combination is allowed only if:
#   - the person's block has at least one working machine (machines are
#     only ever taken from the person's own block, which also enforces
#     gender access, and out-of-order machines are never counted or given
#     out),
#   - the date is an open date of the cycle and the whole booking, buffer
#     included, fits inside opening hours,
#   - the wash itself (start to start + program duration) fits inside one of
#     the person's free windows, which already exclude lectures and blocked
#     times, each with its buffers. The buffer after a wash is machine time,
#     not the person's, so it may run past the end of the window,
#   - it is at least gap_min days after the person's last planned booking of
#     the previous cycle.
# Preference score: 1, plus 1 on a preferred weekday, plus NORMAL_HOURS_POINTS if
# the whole wash is inside normal hours, less the odd-hours penalty (real or
# simulated) if any of the wash overlaps odd hours.
build_candidates <- function(people, free_windows, cfg, grid) {
  ok <- filter(people, valid, cap > 0, grid$working[block] > 0)
  windows <- free_windows |>
    transmute(
      resident_id = as.character(resident_id),
      date = as.Date(date),
      from = pmax(hm_to_min(from), cfg$open),
      to = pmin(hm_to_min(to), cfg$close)
    ) |>
    inner_join(grid$dates, by = "date") |>
    inner_join(select(ok, resident_id, duration_min, cells, last_idx),
               by = "resident_id") |>
    filter(is.na(last_idx) | day_idx >= last_idx + cfg$gap_min) |>
    mutate(
      s_min = pmax(0, ceiling((from - cfg$open) / cfg$grid_min)),
      s_max = pmin(grid$n_cells - cells,
                   floor((to - duration_min - cfg$open) / cfg$grid_min))
    ) |>
    filter(s_min <= s_max)

  starts <- windows |>
    reframe(
      resident_id = rep(resident_id, s_max - s_min + 1),
      date = rep(date, s_max - s_min + 1),
      s = sequence(s_max - s_min + 1, from = s_min)
    ) |>
    distinct()

  ok |>
    select(resident_id, block, duration_min, cells, pref_days) |>
    inner_join(starts, by = "resident_id") |>
    left_join(grid$dates, by = "date") |>
    mutate(
      start_min = grid$cell_start(s),
      end_min = start_min + duration_min,
      normal = start_min >= cfg$normal_start & end_min <= cfg$normal_end,
      odd = start_min < cfg$odd_end & cfg$odd_start < end_min,
      odd_penalty = ifelse(is_simulated_id(resident_id),
                           cfg$odd_penalty_simulated, cfg$odd_penalty_real),
      score = 1L + in_day_list(weekday, pref_days) + NORMAL_HOURS_POINTS * normal -
        odd_penalty * odd
    ) |>
    arrange(block, resident_id, day_idx, s) |>
    mutate(id = row_number()) |>
    select(id, resident_id, block, date, day_idx, s, cells, start_min,
           end_min, score)
}

# A person with a previous-cycle booking must have their first booking
# gap_min to gap_max days after it. If their free time allows no booking in
# that window at all, the upper limit is dropped for them (they can still be
# booked later rather than not at all); they are listed in
# diagnostics$gap_relaxed.
mark_gap_relaxed <- function(people, candidates, cfg) {
  has_first <- candidates |>
    left_join(select(people, resident_id, last_idx), by = "resident_id") |>
    filter(!is.na(last_idx), day_idx <= last_idx + cfg$gap_max) |>
    distinct(resident_id)
  people |>
    mutate(gap_relaxed = valid & !is.na(last_idx) &
             !resident_id %in% has_first$resident_id &
             resident_id %in% candidates$resident_id)
}

# TRUE if a person's booked day numbers obey the gap rules: consecutive
# bookings gap_min to gap_max days apart, at most one booking before the
# last gap_max days of the cycle (so the next cycle can follow on), and the
# first booking gap_min to gap_max days after the previous cycle's last
# planned booking (only the lower limit if the person is gap_relaxed).
days_ok <- function(days, last_idx, relaxed, cfg) {
  days <- sort(days)
  gaps <- diff(days)
  if (any(gaps < cfg$gap_min | gaps > cfg$gap_max)) return(FALSE)
  if (sum(days <= cfg$cycle_days - cfg$gap_max) > 1) return(FALSE)
  if (!is.na(last_idx) && length(days) > 0) {
    if (days[1] - last_idx < cfg$gap_min) return(FALSE)
    if (!relaxed && days[1] - last_idx > cfg$gap_max) return(FALSE)
  }
  TRUE
}

# Days from..to, or none when from > to (unlike from:to, never counts down).
day_range <- function(from, to) {
  if (from > to) integer() else seq(from, to)
}

# TRUE where weekday[i] is one of the ;-separated days in lists[i].
in_day_list <- function(weekday, lists) {
  vapply(seq_along(weekday), function(i) {
    !is.na(lists[i]) && weekday[i] %in% strsplit(lists[i], ";")[[1]]
  }, logical(1))
}

# Machines busy per date and block of time, from bookings already fixed.
fixed_load <- function(fixed, grid) {
  if (is.null(fixed) || nrow(fixed) == 0) {
    return(data.frame(date = as.Date(character()), cell = numeric(),
                      hour = numeric(), busy = integer()))
  }
  count(occupied_cells(fixed, grid), date, cell, hour, name = "busy")
}

# Expands each booking or candidate to the blocks it occupies (buffer
# included).
occupied_cells <- function(cand, grid) {
  cand |>
    reframe(
      id = rep(id, cells),
      block = rep(block, cells),
      date = rep(date, cells),
      cell = sequence(cells, from = s)
    ) |>
    mutate(hour = grid$cell_start(cell) %/% 60)
}

# ---------------------------------------------------------------------------
# Model
# ---------------------------------------------------------------------------

# Builds the ompr model for one hostel block. Constraint rows are first
# built as sparse lists (candidate positions and coefficients per row), then
# added in one quantified add_constraint call. relax = TRUE makes x
# continuous in [0, 1], used only to get an upper bound for the gap. fixed holds
# bookings that are already made in this block: they take up machines, reserve
# and peak room but are not decided here.
build_model <- function(cand, people, cfg, grid, weights, relax = FALSE, fixed = NULL) {
  n <- nrow(cand)
  cand$k <- seq_len(n)
  g <- cfg$grid_min
  n_working <- unname(grid$working[cand$block[1]])
  occ <- occupied_cells(cand, grid) |>
    left_join(select(cand, id, k), by = "id")
  load <- fixed_load(fixed, grid)

  rows <- list(ids = list(), coef = list(), rhs = numeric())
  add_row <- function(rows, ids, coef, rhs) {
    rows$ids[[length(rows$ids) + 1]] <- ids
    rows$coef[[length(rows$coef) + 1]] <- coef
    rows$rhs <- c(rows$rhs, rhs)
    rows
  }

  # MACHINE CAPACITY: at any moment, the number of bookings running in this
  # block (each counted from its start until the end of its buffer) is at
  # most the number of working machines in the block. Bookings are time
  # intervals, so checking the blocks where some booking starts is enough, and
  # a fixed booking counts as starting in every block it holds. Rows that cannot
  # be exceeded are skipped.
  starts <- distinct(bind_rows(distinct(cand, date, cell = s), select(load, date, cell)))
  capacity <- occ |>
    semi_join(starts, by = c("date", "cell")) |>
    group_by(date, cell) |>
    summarise(ids = list(sort(k)), .groups = "drop") |>
    left_join(select(load, date, cell, busy), by = c("date", "cell")) |>
    mutate(room = n_working - coalesce(busy, 0L)) |>
    filter(lengths(ids) > room) |>
    arrange(room)
  capacity <- capacity[!duplicated(vapply(capacity$ids, paste, "", collapse = ",")), ]
  for (i in seq_len(nrow(capacity))) {
    rows <- add_row(rows, capacity$ids[[i]], rep(1, length(capacity$ids[[i]])), capacity$room[i])
  }

  # Per-person rules. y_d below means "number of this person's bookings on
  # day d", the sum of their x on that day.
  last_days <- seq_len(cfg$cycle_days)
  early_days <- last_days[last_days <= cfg$cycle_days - cfg$gap_max]
  for (p in split(cand, cand$resident_id)) {
    person <- people[people$resident_id == p$resident_id[1], ]
    on <- function(days) p$k[p$day_idx %in% days]

    # SLOT CAP: each person gets at most 2 bookings in the cycle, or 1 if they
    # lost a slot to a penalty.
    if (nrow(p) > person$cap) rows <- add_row(rows, p$k, rep(1, nrow(p)), person$cap)

    # MINIMUM GAP: in any gap_min consecutive days a person has at most one
    # booking, so two bookings are at least gap_min days apart. This also
    # means at most one booking per day.
    for (d in seq_len(cfg$cycle_days)) {
      ids <- on(d:(d + cfg$gap_min - 1))
      if (length(ids) > 1) rows <- add_row(rows, ids, rep(1, length(ids)), 1)
    }

    # MAXIMUM GAP: a booking on day d rules out any booking more than
    # gap_max days later.
    for (d in seq_len(cfg$cycle_days)) {
      ids <- c(on(d), on(day_range(d + cfg$gap_max + 1, cfg$cycle_days)))
      if (length(on(d)) > 0 && length(ids) > length(on(d))) {
        rows <- add_row(rows, ids, rep(1, length(ids)), 1)
      }
    }

    # NEXT CYCLE STAYS POSSIBLE: at most one booking in the days before the
    # last gap_max days of the cycle, so the second booking is late enough
    # for the next cycle's first booking to be 4-5 days after it.
    ids <- on(early_days)
    if (length(ids) > 1) rows <- add_row(rows, ids, rep(1, length(ids)), 1)

    # GAP ACROSS CYCLES: if the person had a booking last cycle, any booking
    # later than gap_max days after it needs a first booking within
    # gap_min..gap_max days after it (bookings too close were already
    # removed as candidates). Written as: late bookings <= first bookings.
    if (!is.na(person$last_idx) && !person$gap_relaxed) {
      first <- on(seq(person$last_idx + cfg$gap_min, person$last_idx + cfg$gap_max))
      late <- on(day_range(person$last_idx + cfg$gap_max + 1, cfg$cycle_days))
      if (length(late) > 0) {
        rows <- add_row(rows, c(late, first),
                        c(rep(1, length(late)), rep(-1, length(first))), 0)
      }
    }
  }

  # EMERGENCY RESERVE: in this hostel block, on each date, booked
  # machine-minutes (buffer included) may use at most (1 - reserve_share) of
  # the working machine-minutes, so some machine time is always kept free.
  reserve_cap <- floor((1 - cfg$reserve_share) * n_working * grid$n_cells * g)
  reserve <- cand |>
    group_by(date) |>
    summarise(ids = list(k), coef = list(cells * g), .groups = "drop") |>
    left_join(summarise(group_by(load, date), minutes = sum(busy) * g), by = "date") |>
    mutate(room = reserve_cap - coalesce(minutes, 0)) |>
    filter(vapply(coef, sum, 0) > room)
  for (i in seq_len(nrow(reserve))) {
    rows <- add_row(rows, reserve$ids[[i]], reserve$coef[[i]], reserve$room[i])
  }

  # PEAK OVERLOAD: for each clock hour of each date, booked machine-minutes
  # in this block above peak_target are counted in o[r] and penalised in the
  # objective. Hours that can never exceed the target are skipped.
  peak <- data.frame()
  if (!is.na(cfg$peak_target)) {
    peak <- occ |>
      group_by(date, hour, k) |>
      summarise(minutes = n() * g, .groups = "drop") |>
      group_by(date, hour) |>
      summarise(ids = list(k), coef = list(minutes), .groups = "drop") |>
      left_join(summarise(group_by(load, date, hour), fixed_min = sum(busy) * g,
                          .groups = "drop"), by = c("date", "hour")) |>
      mutate(room = cfg$peak_target - coalesce(fixed_min, 0)) |>
      filter(vapply(coef, sum, 0) > room)
  }
  n_peak <- nrow(peak)

  # Objective weights; see default_weights().
  a_max <- sum(people$cap)
  assign_w <- weights$assign
  auto_w <- (max(cand$score, 0) - min(cand$score, 0)) * weights$pref * a_max +
    weights$peak * a_max * max(cand$cells) * g + 1
  if (is.null(assign_w)) {
    assign_w <- auto_w
  } else if (assign_w < auto_w) {
    warning("weights$assign is below ", auto_w,
            "; the solver may trade bookings for preferences.")
  }
  obj <- assign_w + weights$pref * cand$score

  ids <- rows$ids
  coefs <- rows$coef
  rhs <- rows$rhs
  model <- MIPModel()
  model <- if (relax) {
    add_variable(model, x[c], c = seq_len(n), type = "continuous", lb = 0, ub = 1)
  } else {
    add_variable(model, x[c], c = seq_len(n), type = "binary")
  }
  if (n_peak > 0) {
    model <- model |>
      add_variable(o[r], r = seq_len(n_peak), type = "continuous", lb = 0) |>
      set_objective(sum_over(obj[c] * x[c], c = seq_len(n)) -
                      weights$peak * sum_over(o[r], r = seq_len(n_peak)), "max")
  } else {
    model <- set_objective(model, sum_over(obj[c] * x[c], c = seq_len(n)), "max")
  }
  if (length(ids) > 0) {
    model <- add_constraint(
      model,
      sum_over(coefs[[r]][j] * x[ids[[r]][j]], j = seq_along(ids[[r]])) <= rhs[r],
      r = seq_along(ids)
    )
  }
  if (n_peak > 0) {
    pk_ids <- peak$ids
    pk_coef <- peak$coef
    target <- peak$room
    model <- add_constraint(
      model,
      sum_over(pk_coef[[r]][j] * x[pk_ids[[r]][j]], j = seq_along(pk_ids[[r]])) -
        o[r] <= target[r],
      r = seq_len(n_peak)
    )
  }

  list(
    model = model,
    n_variables = n + n_peak,
    n_constraints = length(ids) + n_peak
  )
}

# Builds and solves one hostel block. Never throws. Outcomes, in
# info$status:
#   "optimal"                         GLPK proved the best schedule (gap 0)
#   "time limit, not proven optimal"  GLPK's best schedule so far is kept
#   "time limit, greedy fallback"     GLPK found none in time; the block is
#                                     filled by greedy_fill() instead
#   "error: ..." / "solver status"    same greedy fallback
# gap = (upper bound - objective) / upper bound, where the upper bound comes
# from solving the model with x relaxed to [0, 1]. GLPK through ROI cannot
# report its own bound or take a gap tolerance. fixed holds bookings already
# made in this block, see build_model(); candidates they leave no machine for are
# dropped first.
solve_block <- function(cand, people, cfg, grid, weights, time_limit, fixed = NULL) {
  load <- fixed_load(fixed, grid)
  if (nrow(load) > 0 && nrow(cand) > 0) {
    full <- filter(load, busy >= grid$working[[cand$block[1]]])
    cand <- anti_join(cand, semi_join(occupied_cells(cand, grid), full,
                                      by = c("date", "cell")), by = "id")
  }
  info <- data.frame(
    status = "no candidates", candidates = nrow(cand), variables = 0,
    constraints = 0, build_s = 0, solve_s = 0, objective = NA_real_,
    gap = NA_real_
  )
  if (nrow(cand) == 0) return(list(booked = cand, info = info))

  fallback <- function(info, why) {
    info$status <- why
    around <- if (is.null(fixed)) cand[0, ] else fixed
    filled <- greedy_fill(around, cand, people, cfg, grid)
    list(booked = filter(filled, resident_id %in% people$resident_id), info = info)
  }

  tryCatch({
    t0 <- Sys.time()
    built <- build_model(cand, people, cfg, grid, weights, fixed = fixed)
    t1 <- Sys.time()
    sol <- solve_model(built$model, with_ROI(
      solver = "glpk", control = list(tm_limit = time_limit * 1000)
    ))
    t2 <- Sys.time()
    info$variables <- built$n_variables
    info$constraints <- built$n_constraints
    info$build_s <- as.numeric(t1 - t0, units = "secs")
    info$solve_s <- as.numeric(t2 - t1, units = "secs")

    symbol <- sol$additional_solver_output$ROI$status$msg$symbol
    if (is.null(symbol)) symbol <- solver_status(sol)
    if (symbol %in% c("GLP_OPT", "GLP_FEAS")) {
      xs <- get_solution(sol, x[c]) |> arrange(c)
      info$objective <- objective_value(sol)
      if (symbol == "GLP_OPT") {
        info$status <- "optimal"
        info$gap <- 0
      } else {
        info$status <- "time limit, not proven optimal"
        info$gap <- relaxed_gap(cand, people, cfg, grid, weights, info$objective, fixed)
      }
      list(booked = cand[xs$value > 0.5, ], info = info)
    } else {
      timed_out <- info$solve_s >= 0.95 * time_limit
      fallback(info, if (timed_out) "time limit, greedy fallback" else
        paste0("solver status ", symbol, ", greedy fallback"))
    }
  }, error = function(e) {
    fallback(info, paste("error:", conditionMessage(e), "- greedy fallback"))
  })
}

# Relative gap between an objective value and the relaxed (LP) optimum.
relaxed_gap <- function(cand, people, cfg, grid, weights, objective, fixed = NULL) {
  tryCatch({
    lp <- build_model(cand, people, cfg, grid, weights, relax = TRUE, fixed = fixed)
    bound <- objective_value(solve_model(lp$model, with_ROI("glpk")))
    (bound - objective) / abs(bound)
  }, error = function(e) NA_real_)
}

# Fallback when GLPK returns no schedule. Adds bookings one at a time, one
# per person per round in resident_id order, until nobody can get another.
# Each booking must obey the same hard rules as the model: it is a
# candidate, the block never has more bookings running than working
# machines in any block of time (buffer included), the person's days still
# pass days_ok(), and the date's emergency reserve stays free. Among allowed
# candidates the best preference score wins, then earliest day and start.
greedy_fill <- function(booked, candidates, people, cfg, grid) {
  g <- cfg$grid_min
  key <- function(b, d) paste(b, d)
  running <- list()
  reserve_cap <- floor((1 - cfg$reserve_share) * grid$working * grid$n_cells * g)
  used <- numeric()
  person_days <- split(booked$day_idx, booked$resident_id)
  add_booking <- function(b) {
    k <- key(b$block, b$date)
    if (is.null(running[[k]])) running[[k]] <<- integer(grid$n_cells)
    running[[k]][b$s + seq_len(b$cells)] <<- running[[k]][b$s + seq_len(b$cells)] + 1L
    used[k] <<- (if (is.na(used[k])) 0 else used[k]) + b$cells * g
  }
  for (i in seq_len(nrow(booked))) add_booking(booked[i, ])

  cand_by_person <- split(candidates, candidates$resident_id)
  todo <- people |>
    filter(resident_id %in% names(cand_by_person)) |>
    arrange(resident_id)
  added <- list()
  repeat {
    progress <- FALSE
    for (i in seq_len(nrow(todo))) {
      p <- todo[i, ]
      have <- person_days[[p$resident_id]]
      if (length(have) >= p$cap) next
      opts <- cand_by_person[[p$resident_id]]
      ok_day <- vapply(opts$day_idx, function(d) {
        days_ok(c(have, d), p$last_idx, p$gap_relaxed, cfg)
      }, logical(1))
      opts <- opts[ok_day, ]
      if (nrow(opts) == 0) next
      free <- mapply(function(b, d, s, n) {
        v <- running[[key(b, d)]]
        is.null(v) || all(v[s + seq_len(n)] < grid$working[[b]])
      }, opts$block, as.character(opts$date), opts$s, opts$cells)
      fits <- coalesce(used[key(opts$block, opts$date)], 0) + opts$cells * g <=
        reserve_cap[opts$block]
      opts <- opts[free & fits, ]
      if (nrow(opts) == 0) next
      pick <- opts |> arrange(desc(score), day_idx, s) |> slice(1)
      add_booking(pick)
      person_days[[p$resident_id]] <- c(have, pick$day_idx)
      added[[length(added) + 1]] <- pick
      progress <- TRUE
    }
    if (!progress) break
  }
  bind_rows(booked, bind_rows(added))
}

# ---------------------------------------------------------------------------
# Output
# ---------------------------------------------------------------------------

# Gives each booking a specific working machine of its block. Bookings on a
# date are taken in start order and each goes to the lowest-numbered machine
# that is free from its start block (the previous booking's buffer has
# ended). Because the model never lets more bookings run at once than there
# are working machines, this always finds a machine.
assign_machines <- function(booked, grid) {
  if (nrow(booked) == 0) return(mutate(booked, machine = character(), machine_no = integer()))
  working <- filter(grid$machines, in_order)
  booked <- arrange(booked, block, day_idx, s, resident_id)
  booked$machine <- NA_character_
  booked$machine_no <- NA_integer_
  for (grp in split(seq_len(nrow(booked)), paste(booked$block, booked$date))) {
    ms <- filter(working, block == booked$block[grp[1]])
    free_from <- rep(0, nrow(ms))
    for (i in grp) {
      j <- which(free_from <= booked$s[i])[1]
      if (is.na(j)) stop("no free machine for ", booked$resident_id[i],
                         " on ", booked$date[i], "; capacity rule was broken")
      booked$machine[i] <- ms$machine[j]
      booked$machine_no[i] <- ms$machine_no[j]
      free_from[j] <- booked$s[i] + booked$cells[i]
    }
  }
  booked
}

# Gives the simulated residents' bookings a working machine around bookings
# already placed, which keep theirs. Each goes to the lowest-numbered machine
# with no overlap. With two or more machines the placed bookings can leave no
# single machine free even though the model's count of busy machines held; such
# a booking is dropped, so the person shows up as underserved.
assign_around <- function(booked, placed, grid) {
  if (nrow(booked) == 0) return(placed)
  working <- filter(grid$machines, in_order)
  booked <- arrange(booked, block, day_idx, s, resident_id)
  booked$machine <- NA_character_
  booked$machine_no <- NA_integer_
  for (i in seq_len(nrow(booked))) {
    ms <- filter(working, block == booked$block[i])
    for (j in seq_len(nrow(ms))) {
      same <- placed[placed$machine == ms$machine[j] & placed$date == booked$date[i], ]
      if (all(same$s + same$cells <= booked$s[i] | same$s >= booked$s[i] + booked$cells[i])) {
        booked$machine[i] <- ms$machine[j]
        booked$machine_no[i] <- ms$machine_no[j]
        placed <- bind_rows(placed, booked[i, ])
        break
      }
    }
  }
  bind_rows(placed, booked[0, ])
}

# Turns a 0/1 solution vector (one value per row of candidates, which must
# already have machines from assign_machines()) into the schedule format:
# person, machine, date, start, end. end = start + program duration; the
# buffer is not part of the person's slot.
extract_schedule <- function(solution, candidates) {
  candidates[solution > 0.5, ] |>
    arrange(date, start_min, machine_no, machine) |>
    transmute(
      person = resident_id,
      machine = machine,
      date = format(date),
      start = min_to_hm(start_min),
      end = min_to_hm(end_min)
    ) |>
    as.data.frame()
}

empty_schedule <- function() {
  data.frame(person = character(), machine = character(), date = character(),
             start = character(), end = character())
}

# Everyone with fewer than 2 bookings, and why: their block or program is
# unknown, their free time fits no wash, the gap rules allow no two of
# their free days, or capacity was full.
find_underserved <- function(booked, candidates, people, cfg) {
  free_days <- split(candidates$day_idx, candidates$resident_id)
  most_possible <- vapply(people$resident_id, function(p) {
    days <- unique(free_days[[p]])
    if (length(days) == 0) return(0L)
    person <- people[people$resident_id == p, ]
    pairs <- utils::combn(c(days, NA), 2)
    two <- any(apply(pairs, 2, function(d) {
      !anyNA(d) && days_ok(d, person$last_idx, person$gap_relaxed, cfg)
    }))
    if (two) 2L else 1L
  }, integer(1))
  people |>
    mutate(most_possible = most_possible) |>
    left_join(count(booked, resident_id, name = "slots_given"), by = "resident_id") |>
    mutate(slots_given = coalesce(slots_given, 0L)) |>
    filter(slots_given < cap) |>
    transmute(
      person = resident_id,
      name = name,
      block = block,
      slots_cap = cap,
      slots_given = slots_given,
      reason = case_when(
        !is.na(problem) ~ problem,
        most_possible == 0 ~ "no free time that fits a wash in this cycle",
        most_possible < cap ~ "free time allows no two days 4-5 days apart",
        TRUE ~ "capacity full"
      )
    ) |>
    as.data.frame()
}

# Booked machine-minutes (buffer included) divided by working
# machine-minutes over the open dates, overall and per block.
utilisation <- function(booked, cfg, grid) {
  avail <- data.frame(block = names(grid$working), machines = unname(grid$working)) |>
    filter(machines > 0) |>
    mutate(available = machines * grid$n_cells * cfg$grid_min * nrow(grid$dates))
  used <- booked |>
    group_by(block) |>
    summarise(booked = sum(cells * cfg$grid_min), .groups = "drop")
  per_block <- avail |>
    left_join(used, by = "block") |>
    mutate(booked = coalesce(booked, 0), share = booked / available) |>
    select(block, booked, available, share)
  list(
    overall = sum(per_block$booked) / sum(per_block$available),
    per_block = as.data.frame(per_block)
  )
}

print_diagnostics <- function(result) {
  cat("Optimiser status:", result$status$code, "\n ", result$status$message, "\n")
  d <- result$diagnostics
  if (length(d) == 0) return(invisible())
  cat(sprintf(
    "  candidates %d | variables %d | constraints %d | build %.1fs | solve %.1fs | objective %.1f\n",
    d$candidates, d$variables, d$constraints, d$build_s, d$solve_s, d$objective
  ))
  u <- d$utilisation
  cat(sprintf("  utilisation %.1f%% overall; per block: %s\n", 100 * u$overall,
              paste0(u$per_block$block, " ", sprintf("%.1f%%", 100 * u$per_block$share),
                     collapse = ", ")))
  if (length(d$gap_relaxed) > 0) {
    cat("  previous-cycle gap relaxed for:", paste(d$gap_relaxed, collapse = ", "), "\n")
  }
  invisible()
}

# ---------------------------------------------------------------------------
# Time helpers
# ---------------------------------------------------------------------------

hm_to_min <- function(x) {
  x <- as.character(x)
  as.numeric(substr(x, 1, 2)) * 60 + as.numeric(substr(x, 4, 5))
}

min_to_hm <- function(m) {
  sprintf("%02d:%02d", as.integer(m %/% 60), as.integer(m %% 60))
}

# Weekday names as used in config and the survey, independent of locale.
weekday_name <- function(date) {
  c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[as.POSIXlt(date)$wday + 1]
}
