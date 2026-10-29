# Hostel Laundry Slot Scheduler

Shiny app for a hostel: residents fill in a laundry questionnaire, an admin
runs an optimiser that assigns each resident's loads to washing machines in
their block, and residents see their slots.

## Stack

- **shiny** + **bslib**: UI, `page_navbar` with pages Questionnaire, My slots, Schedule, Admin
- **ompr** + **ompr.roi** + **ROI.plugin.glpk**: the scheduling MILP, solved with GLPK
- **DBI** + **RSQLite**: storing responses and schedules (`*.sqlite`, git-ignored)
- **DT**, **ggplot2**: schedule tables and charts
- **readr**, **dplyr**: reading CSVs and data wrangling
- **renv**: package versions locked in `renv.lock`. After clone, run `renv::restore()`.

Run with `shiny::runApp()`. Files in `R/` are sourced automatically.

## File ownership

Four people work on this repo, each on one piece. **A session edits only its
own person's files.** If a change is needed in another file, describe it to the
user instead of editing it. Change a function's inputs or outputs only by
agreement, since other files depend on the contracts written at the top of
each file.

| Owner | Piece | Files |
|---|---|---|
| Lead | App wiring | `app.R`, `CLAUDE.md`, `README.md`, `.gitignore`, `renv.lock` |
| Person 1 | Optimiser (ompr model) | `R/optimiser.R` |
| Person 2 | Constraint checker + data loading | `R/checker.R`, `R/data_load.R`, `data/config.csv`, `data/fake_survey.csv`, `data/fake_schedule.csv`, `data/fake_schedule_bad.csv` |
| Person 3 | Shiny questionnaire + input pages | `R/mod_questionnaire.R`, `R/mod_admin.R` |
| Person 4 | Results page (schedule table, charts) | `R/mod_results.R` (My slots and Schedule pages) |

What each file does:

| File | Contents |
|---|---|
| `app.R` | Joins modules into the navbar app; shared library calls |
| `R/data_load.R` | `load_survey()`, `load_config()`, `db_connect()` |
| `R/optimiser.R` | `run_optimiser()`: survey + config to list(schedule, underserved, status, diagnostics); `optimise_schedule()`: wrapper returning only the schedule |
| `R/checker.R` | `check_schedule()`: validates a schedule, returns problems |
| `R/mod_questionnaire.R` | Questionnaire page module |
| `R/mod_admin.R` | Admin page module; runs optimiser and checker, returns the current schedule |
| `R/mod_results.R` | My slots and Schedule pages (`view = "mine"` / `"all"`) |
| `data/config.csv` | Machines, programs, opening hours |
| `data/fake_survey.csv` | Made-up residents (survey format) |
| `data/fake_schedule.csv` | A valid schedule (schedule format) |
| `data/fake_schedule_bad.csv` | A schedule that breaks the checker's rules |

## Workflow

- Work on your own branch (e.g. `optimiser`, `checker`, `questionnaire`, `results`), never directly on `main`.
- Merge into `main` through a pull request.
- `renv.lock` is shared: if you need a new package, ask the lead to add it so only one person changes the lockfile at a time.
- Until the optimiser works, build and test against `data/fake_schedule.csv`.

## Survey data format

`data/fake_survey.csv`, one row per resident:

| Column | Type | Example |
|---|---|---|
| `resident_id` | string, unique | `R001` |
| `name` | string | `Aarav Okafor` |
| `block` | string, matches a `machines` key in config | `B` |
| `room` | string | `B146` |
| `program` | string, matches a `program` key in config | `cotton` |
| `loads_per_week` | integer, 1 to 3 | `1` |
| `availability` | `;`-separated `Day HH:MM-HH:MM` windows when the person is free | `Mon 07:00-09:00;Fri 09:00-12:00` |
| `pref_days` | optional, `;`-separated days | `Sat;Sun` |
| `pref_band` | optional, a `band` key from config | `evening` |

Days are `Mon Tue Wed Thu Fri Sat Sun`. Times are 24-hour `HH:MM`.
A booking is valid only if the whole wash (start to end) fits inside one of
the person's `availability` windows. If `pref_days` / `pref_band` are missing
or empty, the person has no preference.

## Config format

`data/config.csv` has columns `section,key,value`:

- `machines,<block>,<count>`: number of machines in each block
- `program,<name>,<minutes>`: wash program durations
- `hours,open|close,HH:MM`: laundry room opening hours
- `hours,days,Mon;Tue;...`: days the laundry is open
- `hours,buffer_min,<minutes>`: machine time reserved after each wash (default 10)
- `rules,grid_min,<minutes>`: time grid step; bookings start on it and occupy whole grid blocks (default 15)
- `rules,reserve_share,<0-1>`: share of machine-minutes per hostel block per day kept free as emergency reserve (default 0)
- `rules,peak_target,<minutes>`: booked machine-minutes per hostel block per clock hour above which the optimiser applies a penalty (default: no penalty)
- `out_of_order,<machine id>,1`: machine that must not be used, e.g. `out_of_order,C-2,1` (default: none)
- `band,<name>,HH:MM-HH:MM`: time bands for `pref_band`, e.g. `band,evening,17:00-23:00`

All rows except `machines`, `program` and `hours,open|close|days` are optional.

## Schedule format

The optimiser returns, and the checker and results pages take, a data.frame
with one row per assigned load:

| Column | Type | Example |
|---|---|---|
| `person` | `resident_id` from the survey | `R001` |
| `machine` | `<block>-<number>` | `B-2` |
| `day` | `Mon` ... `Sun` | `Fri` |
| `start` | `HH:MM` | `09:30` |
| `end` | `HH:MM`, start + program duration | `11:00` |
