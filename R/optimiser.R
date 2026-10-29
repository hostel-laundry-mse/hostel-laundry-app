# Schedule optimiser (ompr + GLPK).
#
# run_optimiser(survey, config, weights = default_weights(), time_limit = 50,
#               verbose = TRUE)
#   Takes: survey  - data.frame in the survey format (CLAUDE.md), e.g. from
#                    load_survey()
#          config  - either the list from load_config() or the raw
#                    data/config.csv table (columns section, key, value)
#          weights - objective weights, see default_weights()
#   Returns: list(
#     schedule    = data.frame(person, machine, day, start, end), the
#                   schedule format in CLAUDE.md, one row per booking,
#     underserved = data.frame(person, name, block, slots_cap, slots_given,
#                   reason), everyone who got fewer than min(2,
#                   loads_per_week) bookings,
#     status      = list(code, message, per_block), code is "optimal",
#                   "partial" (a block failed) or "error",
#     diagnostics = list(candidates, variables, constraints, build_s,
#                   solve_s, objective, utilisation, per_block))
#
# optimise_schedule(survey, config)
#   Takes: survey, config as above.
#   Returns: only the schedule data.frame (person, machine, day, start, end).
#
# How the model works
#   Time: each open day is cut into grid cells of grid_min minutes. A booking
#   starts on a cell and occupies ceiling((program + buffer) / grid_min)
#   cells, so short programs take less machine time than long ones.
#   Candidates: every (person, machine, day, start cell) that is allowed.
#   Only allowed combinations get a decision variable, so block access,
#   out-of-order machines, opening hours and timetable clashes are enforced
#   by build_candidates() and never appear as constraints.
#   Model: x[c] = 1 if candidate c is booked. Constraints and objective are
#   in build_model(). Hostel blocks share no machines, so each block is
#   solved as its own model and the schedules are combined.
#   Second pass: assign_extra_slots() gives people who need more than 2
#   washes extra bookings in leftover machine time.
#
# The config list from load_config() is read with these names (all but
# machines, programs and hours$open/close/days are optional):
#   machines     data.frame(block, n_machines)
#   programs     data.frame(program, duration_min)
#   hours        list(open, close, days, buffer_min)
#   rules        list(grid_min, reserve_share, peak_target)
#   out_of_order character vector of machine ids, e.g. "C-2"
#   bands        data.frame(band, from, to), times as "HH:MM"

library(dplyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

# Objective weights.
#   pref:   value of one preference point (scores are 1, 2 or 3).
#   peak:   cost of one booked machine-minute above peak_target in an hour.
#   assign: value of one booking. NULL means "set automatically" to
#           2 * pref * A_max + peak * A_max * longest booking minutes + 1,
#           where A_max is the most bookings the block could get. Swapping
#           bookings around can raise total preference by at most 2 per
#           booking and lower total overload by at most the booked minutes,
#           so with this value one extra booking always beats any preference
#           or peak gain: nobody loses a booking to improve someone else's
#           preferences.
default_weights <- function() {
  list(assign = NULL, pref = 1, peak = 0.02)
}

optimise_schedule <- function(survey, config) {
  run_optimiser(survey, config, verbose = FALSE)$schedule
}

run_optimiser <- function(survey, config, weights = default_weights(),
                          time_limit = 50, verbose = TRUE) {
  result <- tryCatch(
    run_optimiser_unsafe(survey, config, weights, time_limit),
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

run_optimiser_unsafe <- function(survey, config, weights, time_limit) {
  cfg <- normalise_config(config)
  grid <- generate_grid(cfg)
  people <- prepare_people(survey, cfg, grid)
  candidates <- build_candidates(people, cfg, grid)

  # Solve each hostel block separately: blocks share no machines.
  booked <- list()
  per_block <- list()
  for (b in cfg$machines$block) {
    cand_b <- filter(candidates, block == b)
    people_b <- filter(people, block == b, valid)
    res <- solve_block(cand_b, people_b, cfg, grid, weights, time_limit)
    booked[[b]] <- res$booked
    per_block[[b]] <- data.frame(block = b, res$info)
  }
  booked <- bind_rows(booked)
  per_block <- bind_rows(per_block)
  if (nrow(booked) == 0) booked <- candidates[0, ]
  booked$pass <- rep("main", nrow(booked))

  util_main <- utilisation(booked, cfg, grid)
  booked <- assign_extra_slots(booked, candidates, people, cfg, grid)

  schedule <- extract_schedule(rep(1, nrow(booked)), booked)
  underserved <- find_underserved(booked, candidates, people)

  failed <- per_block$block[!per_block$status %in% c("optimal", "no candidates")]
  code <- if (length(failed) == 0) "optimal" else "partial"
  message <- paste0(
    if (code == "optimal") "Optimal schedule found: " else
      paste0("Solver failed for block(s) ", paste(failed, collapse = ", "),
             ", which got no main bookings. "),
    nrow(schedule), " bookings for ", n_distinct(schedule$person), " of ",
    nrow(people), " people (", sum(booked$pass == "extra"),
    " from the extra-slot pass); ", nrow(underserved),
    " people got fewer than their cap."
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
      utilisation = list(
        main = util_main,
        final = utilisation(booked, cfg, grid)
      ),
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
    bands <- get("band")
    config <- list(
      machines = data.frame(block = m$key, n_machines = as.integer(m$value)),
      programs = data.frame(program = p$key, duration_min = as.numeric(p$value)),
      hours = list(open = h$open, close = h$close,
                   days = strsplit(h$days, ";")[[1]],
                   buffer_min = h$buffer_min),
      rules = setNames(as.list(r$value), r$key),
      out_of_order = get("out_of_order")$key,
      bands = data.frame(
        band = bands$key,
        from = sub("-.*", "", bands$value),
        to = sub(".*-", "", bands$value)
      )
    )
  }

  opt <- function(x, default) {
    if (is.null(x) || length(x) == 0 || is.na(x[[1]]) || x[[1]] == "") default
    else as.numeric(x[[1]])
  }
  days <- config$hours$days
  if (length(days) == 1) days <- strsplit(days, ";")[[1]]
  bands <- config$bands
  if (is.null(bands) || nrow(bands) == 0) {
    bands <- data.frame(band = character(), from = numeric(), to = numeric())
  } else {
    bands <- mutate(bands, from = hm_to_min(from), to = hm_to_min(to))
  }

  list(
    machines = config$machines,
    programs = config$programs,
    open = hm_to_min(config$hours$open),
    close = hm_to_min(config$hours$close),
    days = days,
    buffer_min = opt(config$hours$buffer_min, 10),
    grid_min = opt(config$rules$grid_min, 15),
    reserve_share = opt(config$rules$reserve_share, 0),
    peak_target = opt(config$rules$peak_target, NA),
    out_of_order = as.character(config$out_of_order),
    bands = bands
  )
}

# Adds program duration, booking length in cells and slot cap to each person,
# and marks people whose block or program is not in config as not valid.
prepare_people <- function(survey, cfg, grid) {
  if (!"pref_days" %in% names(survey)) survey$pref_days <- NA_character_
  if (!"pref_band" %in% names(survey)) survey$pref_band <- NA_character_
  survey |>
    mutate(across(c(resident_id, block, program), as.character)) |>
    distinct(resident_id, .keep_all = TRUE) |>
    left_join(cfg$programs, by = "program") |>
    mutate(
      cells = grid$cells_needed(duration_min),
      cap = pmin(2L, as.integer(loads_per_week)),
      valid = block %in% cfg$machines$block & !is.na(duration_min),
      problem = case_when(
        !block %in% cfg$machines$block ~ paste0("block '", block, "' is not in config"),
        is.na(duration_min) ~ paste0("program '", program, "' is not in config"),
        TRUE ~ NA_character_
      )
    ) |>
    arrange(resident_id)
}

# Splits "Mon 07:00-09:00;Fri 09:00-12:00" into one row per free window.
parse_availability <- function(people) {
  parts <- strsplit(as.character(people$availability), ";")
  windows <- data.frame(
    resident_id = rep(people$resident_id, lengths(parts)),
    text = trimws(unlist(parts))
  )
  windows |>
    filter(grepl("^[A-Za-z]{3} [0-9]{2}:[0-9]{2}-[0-9]{2}:[0-9]{2}$", text)) |>
    mutate(
      day = substr(text, 1, 3),
      from = hm_to_min(substr(text, 5, 9)),
      to = hm_to_min(substr(text, 11, 15))
    ) |>
    select(resident_id, day, from, to)
}

# ---------------------------------------------------------------------------
# Time structure
# ---------------------------------------------------------------------------

# The only place the time structure is defined. Each open day is cut into
# n_cells cells of grid_min minutes, starting at opening time. Returns:
#   cells        one row per machine, day and cell: machine, block, day,
#                cell (0-based), cell_start (minutes), hour (clock hour the
#                cell starts in), in_order
#   machines     one row per machine: machine, block, machine_no, in_order
#   n_cells      cells per day
#   cell_start   function(cell) -> minutes after midnight
#   cells_needed function(duration) -> cells a booking of that program
#                occupies, buffer included
generate_grid <- function(cfg) {
  g <- cfg$grid_min
  n_cells <- floor((cfg$close - cfg$open) / g)
  machines <- cfg$machines |>
    rowwise() |>
    reframe(block = block, machine_no = seq_len(n_machines)) |>
    mutate(
      machine = paste0(block, "-", machine_no),
      in_order = !machine %in% cfg$out_of_order
    )
  cells <- expand.grid(
    cell = seq_len(n_cells) - 1L,
    day = cfg$days,
    machine = machines$machine,
    stringsAsFactors = FALSE
  ) |>
    left_join(machines, by = "machine") |>
    mutate(
      cell_start = cfg$open + cell * g,
      hour = cell_start %/% 60
    ) |>
    select(machine, block, day, cell, cell_start, hour, in_order)

  list(
    cells = cells,
    machines = machines,
    n_cells = n_cells,
    cell_start = function(cell) cfg$open + cell * g,
    cells_needed = function(duration) ceiling((duration + cfg$buffer_min) / g)
  )
}

# ---------------------------------------------------------------------------
# Candidates
# ---------------------------------------------------------------------------

# Every allowed (person, machine, day, start cell). A combination is allowed
# only if:
#   - the machine is in the person's own block (this also enforces gender
#     access, since blocks are single-gender),
#   - the machine is not out of order,
#   - the day is an open day and the whole booking, buffer included, fits
#     inside opening hours,
#   - the wash itself (start to start + program duration) fits inside one of
#     the person's free windows. The buffer is machine time, not the
#     person's, so it may run past the end of the window.
# Preference score: 3 = preferred day and start in preferred band,
# 2 = one of the two, 1 = neither but the person is free.
build_candidates <- function(people, cfg, grid) {
  ok <- filter(people, valid)
  windows <- parse_availability(ok) |>
    filter(day %in% cfg$days) |>
    inner_join(select(ok, resident_id, duration_min, cells), by = "resident_id") |>
    mutate(
      s_min = pmax(0, ceiling((from - cfg$open) / cfg$grid_min)),
      s_max = pmin(grid$n_cells - cells,
                   floor((to - duration_min - cfg$open) / cfg$grid_min))
    ) |>
    filter(s_min <= s_max)

  starts <- windows |>
    reframe(
      resident_id = rep(resident_id, s_max - s_min + 1),
      day = rep(day, s_max - s_min + 1),
      s = sequence(s_max - s_min + 1, from = s_min)
    ) |>
    distinct()

  working <- filter(grid$machines, in_order) |> select(machine, block, machine_no)

  ok |>
    select(resident_id, block, duration_min, cells, pref_days, pref_band) |>
    inner_join(starts, by = "resident_id") |>
    inner_join(working, by = "block", relationship = "many-to-many") |>
    left_join(rename(cfg$bands, band_from = from, band_to = to),
              by = c("pref_band" = "band")) |>
    mutate(
      start_min = grid$cell_start(s),
      end_min = start_min + duration_min,
      day_idx = match(day, cfg$days),
      pref_day = in_day_list(day, pref_days),
      pref_time = !is.na(band_from) & start_min >= band_from & start_min < band_to,
      score = 1L + pref_day + pref_time
    ) |>
    arrange(block, resident_id, day_idx, s, machine_no) |>
    mutate(id = row_number()) |>
    select(id, resident_id, block, machine, machine_no, day, day_idx, s,
           cells, start_min, end_min, score)
}

# TRUE where day[i] is one of the ;-separated days in lists[i].
in_day_list <- function(day, lists) {
  vapply(seq_along(day), function(i) {
    !is.na(lists[i]) && day[i] %in% strsplit(lists[i], ";")[[1]]
  }, logical(1))
}

# Expands each candidate to the cells it occupies (buffer included).
occupied_cells <- function(cand, grid) {
  cand |>
    reframe(
      id = rep(id, cells),
      machine = rep(machine, cells),
      day = rep(day, cells),
      cell = sequence(cells, from = s)
    ) |>
    mutate(hour = grid$cell_start(cell) %/% 60)
}

# ---------------------------------------------------------------------------
# Model
# ---------------------------------------------------------------------------

# Builds the constraint rows as sparse lists (candidate ids and coefficients
# per row) and then the ompr model for one hostel block.
build_model <- function(cand, people, cfg, grid, weights) {
  n <- nrow(cand)
  cand$k <- seq_len(n)
  occ <- occupied_cells(cand, grid) |>
    left_join(select(cand, id, k), by = "id")
  g <- cfg$grid_min

  # Rows of the form sum(x[ids]) <= rhs.
  rows <- list()

  # NO OVERLAP: on each machine, at any moment, at most one booking
  # (including its buffer) can be running. Bookings are intervals, so it is
  # enough to check the cells where some booking starts.
  starts <- distinct(cand, machine, day, cell = s)
  no_overlap <- occ |>
    semi_join(starts, by = c("machine", "day", "cell")) |>
    group_by(machine, day, cell) |>
    summarise(ids = list(sort(k)), .groups = "drop") |>
    filter(lengths(ids) > 1)
  no_overlap <- no_overlap[!duplicated(vapply(no_overlap$ids, paste, "", collapse = ",")), ]
  rows$no_overlap <- list(ids = no_overlap$ids, rhs = rep(1, nrow(no_overlap)))

  # SLOT CAP: each person gets at most min(2, loads_per_week) bookings in
  # this main solve.
  cap <- cand |>
    left_join(select(people, resident_id, cap), by = "resident_id") |>
    group_by(resident_id, cap) |>
    summarise(ids = list(k), .groups = "drop") |>
    filter(lengths(ids) > cap)
  rows$cap <- list(ids = cap$ids, rhs = cap$cap)

  # DIFFERENT DAYS: a person's bookings must be on different days, so each
  # person has at most one booking per day. Not needed when the cap is 1.
  per_day <- cand |>
    left_join(select(people, resident_id, cap), by = "resident_id") |>
    filter(cap > 1) |>
    group_by(resident_id, day) |>
    summarise(ids = list(k), .groups = "drop") |>
    filter(lengths(ids) > 1)
  rows$per_day <- list(ids = per_day$ids, rhs = rep(1, nrow(per_day)))

  # EMERGENCY RESERVE: in this hostel block, on each day, booked
  # machine-minutes (buffer included) may use at most (1 - reserve_share) of
  # the working machine-minutes, so some machine time is always kept free.
  n_working <- sum(grid$machines$in_order & grid$machines$block == cand$block[1])
  reserve_cap <- floor((1 - cfg$reserve_share) * n_working * grid$n_cells * g)
  reserve <- cand |>
    group_by(day) |>
    summarise(ids = list(k), coef = list(cells * g), .groups = "drop") |>
    filter(vapply(coef, sum, 0) > reserve_cap)

  # PEAK OVERLOAD: for each clock hour of each day, booked machine-minutes
  # above peak_target are counted in o[r] and penalised in the objective.
  # Hours that can never exceed the target are skipped.
  peak <- data.frame()
  if (!is.na(cfg$peak_target)) {
    peak <- occ |>
      group_by(day, hour, k) |>
      summarise(minutes = n() * g, .groups = "drop") |>
      group_by(day, hour) |>
      summarise(ids = list(k), coef = list(minutes), .groups = "drop") |>
      filter(vapply(coef, sum, 0) > cfg$peak_target)
  }
  n_peak <- nrow(peak)

  # Objective weights; see default_weights().
  a_max <- sum(people$cap)
  assign_w <- weights$assign
  auto_w <- 2 * weights$pref * a_max +
    weights$peak * a_max * max(cand$cells) * g + 1
  if (is.null(assign_w)) assign_w <- auto_w
  else if (assign_w < auto_w)
    warning("weights$assign is below ", auto_w,
            "; the solver may trade bookings for preferences.")
  obj <- assign_w + weights$pref * cand$score

  # Flatten all <= rows of x only into one list for a single add_constraint.
  ids <- c(rows$no_overlap$ids, rows$cap$ids, rows$per_day$ids, reserve$ids)
  coefs <- c(lapply(rows$no_overlap$ids, function(i) rep(1, length(i))),
             lapply(rows$cap$ids, function(i) rep(1, length(i))),
             lapply(rows$per_day$ids, function(i) rep(1, length(i))),
             reserve$coef)
  rhs <- c(rows$no_overlap$rhs, rows$cap$rhs, rows$per_day$rhs,
           rep(reserve_cap, nrow(reserve)))

  model <- MIPModel() |>
    add_variable(x[c], c = seq_len(n), type = "binary")
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
    target <- cfg$peak_target
    model <- add_constraint(
      model,
      sum_over(pk_coef[[r]][j] * x[pk_ids[[r]][j]], j = seq_along(pk_ids[[r]])) -
        o[r] <= target,
      r = seq_len(n_peak)
    )
  }

  list(
    model = model,
    n_variables = n + n_peak,
    n_constraints = length(ids) + n_peak
  )
}

# Builds and solves one hostel block. Never throws: a failure returns no
# bookings and the reason in info$status.
solve_block <- function(cand, people, cfg, grid, weights, time_limit) {
  info <- data.frame(
    status = "no candidates", candidates = nrow(cand), variables = 0,
    constraints = 0, build_s = 0, solve_s = 0, objective = NA_real_
  )
  if (nrow(cand) == 0) return(list(booked = cand, info = info))

  out <- tryCatch({
    t0 <- Sys.time()
    built <- build_model(cand, people, cfg, grid, weights)
    t1 <- Sys.time()
    sol <- solve_model(built$model, with_ROI(
      solver = "glpk", control = list(tm_limit = time_limit * 1000)
    ))
    t2 <- Sys.time()
    info$variables <- built$n_variables
    info$constraints <- built$n_constraints
    info$build_s <- as.numeric(t1 - t0, units = "secs")
    info$solve_s <- as.numeric(t2 - t1, units = "secs")
    if (solver_status(sol) == "success") {
      info$status <- "optimal"
      info$objective <- objective_value(sol)
      xs <- get_solution(sol, x[c]) |> arrange(c)
      list(booked = cand[xs$value > 0.5, ], info = info)
    } else {
      info$status <- paste("solver status:", solver_status(sol))
      list(booked = cand[0, ], info = info)
    }
  }, error = function(e) {
    info$status <- paste("error:", conditionMessage(e))
    list(booked = cand[0, ], info = info)
  })
  out
}

# ---------------------------------------------------------------------------
# Output
# ---------------------------------------------------------------------------

# Turns a 0/1 solution vector (one value per candidate row) into the
# schedule format: person, machine, day, start, end. end = start + program
# duration; the buffer is not part of the person's slot.
extract_schedule <- function(solution, candidates) {
  candidates[solution > 0.5, ] |>
    arrange(day_idx, start_min, machine_no, machine) |>
    transmute(
      person = resident_id,
      machine = machine,
      day = day,
      start = min_to_hm(start_min),
      end = min_to_hm(end_min)
    ) |>
    as.data.frame()
}

empty_schedule <- function() {
  data.frame(person = character(), machine = character(), day = character(),
             start = character(), end = character())
}

# Second pass, no solver. People whose loads_per_week is more than what they
# got are given extra bookings, one at a time, highest remaining need first
# (ties by resident_id). Each extra booking must obey the same hard rules:
# a candidate (so own block, working machine, inside opening hours and the
# person's free time), no overlap with any booking on that machine (buffer
# included), not on a day the person already has a booking, and the block's
# emergency reserve stays free. Among allowed candidates the best preference
# score wins, then least added peak overload, then earliest day, start and
# machine.
assign_extra_slots <- function(booked, candidates, people, cfg, grid) {
  g <- cfg$grid_min
  need <- people |>
    filter(valid) |>
    select(resident_id, loads_per_week) |>
    left_join(count(booked, resident_id, name = "got"), by = "resident_id") |>
    mutate(got = coalesce(got, 0L), left = loads_per_week - got) |>
    filter(left > 0)
  if (nrow(need) == 0 || nrow(candidates) == 0) return(booked)

  # Machine occupancy: one logical vector of cells per machine and day.
  busy <- list()
  key <- function(m, d) paste(m, d)
  for (i in seq_len(nrow(booked))) {
    b <- booked[i, ]
    k <- key(b$machine, b$day)
    if (is.null(busy[[k]])) busy[[k]] <- logical(grid$n_cells)
    busy[[k]][b$s + seq_len(b$cells)] <- TRUE
  }
  # Booked machine-minutes per block and day, and per block, day and hour.
  n_working <- grid$machines |> filter(in_order) |> count(block, name = "n")
  reserve_cap <- setNames(
    floor((1 - cfg$reserve_share) * n_working$n * grid$n_cells * g),
    n_working$block
  )
  used <- booked |> group_by(block, day) |> summarise(m = sum(cells * g), .groups = "drop")
  used <- setNames(used$m, paste(used$block, used$day))
  hour_used <- occupied_cells(booked, grid) |>
    left_join(select(booked, id, block), by = "id") |>
    count(block, day, hour, name = "m") |>
    mutate(m = m * g)
  hour_used <- setNames(hour_used$m, paste(hour_used$block, hour_used$day, hour_used$hour))
  booked_days <- split(booked$day, booked$resident_id)

  cand_by_person <- split(candidates, candidates$resident_id)
  extra <- list()
  repeat {
    progress <- FALSE
    need <- arrange(need, desc(left), resident_id)
    for (i in seq_len(nrow(need))) {
      if (need$left[i] <= 0) next
      p <- need$resident_id[i]
      opts <- cand_by_person[[p]]
      if (is.null(opts)) next
      opts <- opts[!opts$day %in% booked_days[[p]], ]
      if (nrow(opts) == 0) next
      free <- mapply(function(m, d, s, n) {
        v <- busy[[key(m, d)]]
        is.null(v) || !any(v[s + seq_len(n)])
      }, opts$machine, opts$day, opts$s, opts$cells)
      fits <- coalesce(used[paste(opts$block, opts$day)], 0) + opts$cells * g <=
        reserve_cap[opts$block]
      opts <- opts[free & fits, ]
      if (nrow(opts) == 0) next
      opts$added_peak <- if (is.na(cfg$peak_target)) 0 else
        vapply(seq_len(nrow(opts)), function(j) {
          o <- opts[j, ]
          hrs <- grid$cell_start(o$s + seq_len(o$cells) - 1) %/% 60
          add <- table(hrs) * g
          cur <- coalesce(hour_used[paste(o$block, o$day, names(add))], 0)
          sum(pmax(0, cur + add - cfg$peak_target) - pmax(0, cur - cfg$peak_target))
        }, 0)
      pick <- opts |> arrange(desc(score), added_peak, day_idx, s, machine_no) |> slice(1)

      k <- key(pick$machine, pick$day)
      if (is.null(busy[[k]])) busy[[k]] <- logical(grid$n_cells)
      busy[[k]][pick$s + seq_len(pick$cells)] <- TRUE
      bk <- paste(pick$block, pick$day)
      used[bk] <- coalesce(used[bk], 0) + pick$cells * g
      hrs <- table(grid$cell_start(pick$s + seq_len(pick$cells) - 1) %/% 60) * g
      hk <- paste(pick$block, pick$day, names(hrs))
      hour_used[hk] <- coalesce(hour_used[hk], 0) + as.numeric(hrs)
      booked_days[[p]] <- c(booked_days[[p]], pick$day)
      extra[[length(extra) + 1]] <- select(pick, -added_peak)
      need$left[i] <- need$left[i] - 1
      progress <- TRUE
    }
    if (!progress) break
  }
  if (length(extra) == 0) return(booked)
  bind_rows(booked, mutate(bind_rows(extra), pass = "extra"))
}

# Everyone with fewer bookings than their cap, min(2, loads_per_week), and
# why: their block or program is unknown, their free time matches a working
# machine on fewer days than their cap, or capacity was full.
find_underserved <- function(booked, candidates, people) {
  cand_days <- candidates |>
    group_by(resident_id) |>
    summarise(free_days = n_distinct(day), .groups = "drop")
  people |>
    left_join(count(booked, resident_id, name = "slots_given"), by = "resident_id") |>
    left_join(cand_days, by = "resident_id") |>
    mutate(
      slots_given = coalesce(slots_given, 0L),
      free_days = coalesce(free_days, 0L)
    ) |>
    filter(slots_given < cap) |>
    transmute(
      person = resident_id,
      name = name,
      block = block,
      slots_cap = cap,
      slots_given = slots_given,
      reason = case_when(
        !is.na(problem) ~ problem,
        free_days == 0 ~ "no free time that matches any machine",
        free_days < cap ~ paste0("free time matches a machine on only ",
                                 free_days, " day(s)"),
        TRUE ~ "capacity full"
      )
    ) |>
    as.data.frame()
}

# Booked machine-minutes (buffer included) divided by working
# machine-minutes, overall and per block.
utilisation <- function(booked, cfg, grid) {
  avail <- grid$machines |>
    filter(in_order) |>
    count(block, name = "machines") |>
    mutate(available = machines * grid$n_cells * cfg$grid_min * length(cfg$days))
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
  u <- d$utilisation$final
  cat(sprintf("  utilisation %.1f%% overall; per block: %s\n", 100 * u$overall,
              paste0(u$per_block$block, " ", sprintf("%.1f%%", 100 * u$per_block$share),
                     collapse = ", ")))
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
