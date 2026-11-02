# Hostel Laundry Slot Scheduler

Shiny app for a hostel. Residents fill in a questionnaire (their course,
times they can't wash, wash program). The app works out when each person is
free from public course timetables, and an optimiser books every resident
into a washing machine in their own block for each 10-day cycle. Residents
see their slots, check in when they start, and can cancel a slot, which makes
it a free slot anyone can claim.

## Stack

- **shiny** + **bslib**: UI, `page_navbar` with pages Questionnaire, My slots, Schedule, Admin
- **ompr** + **ompr.roi** + **ROI.plugin.glpk**: the scheduling model, solved with GLPK.
  Solver choice is under review for speed (HiGHS is the alternative); do not change it without the lead.
- **DBI** + **RSQLite**: storing responses, bookings, cancellations and check-ins (`*.sqlite`, git-ignored)
- **DT**, **ggplot2**: schedule tables and charts
- **readr**, **dplyr**: reading CSVs and data wrangling
- **renv**: package versions locked in `renv.lock`. After clone, run `renv::restore()`.

Run with `shiny::runApp()`. Files in `R/` are sourced automatically.
The app assumes the laptops it runs on are set to India time (IST): slot
times, check-in and the cancellation cutoff all use the computer's clock.
The laundry machines run 24 hours (00:00-24:00); residents' sleep is handled by their own blocked times; the last normal wash starts 22:30 and the last quick wash 23:00 so the buffer ends by midnight.

## File ownership

Four people work on this repo, each on one piece. **A session edits only its
own person's files.** If a change is needed in another file, describe it to the
user instead of editing it. Change a function's inputs or outputs only by
agreement, since other files depend on the contracts written at the top of
each file.

| Owner | Piece | Files |
|---|---|---|
| Lead | App wiring, shared docs, packages | `app.R`, `CLAUDE.md`, `README.md`, `.gitignore`, `renv.lock` |
| Person 1 | Optimiser and slot rules | `R/optimiser.R` |
| Person 2 | Data, free time, database, checker | `R/data_load.R`, `R/checker.R`, `R/simulate_blockC.R`, `data/config.csv`, `data/timetable.csv`, `data/fake_timetable.csv`, `data/fake_survey.csv`, `data/fake_schedule.csv`, `data/fake_schedule_bad.csv`, `data/fake_last_planned.csv`, `data/participants.csv` (git-ignored) |
| Person 3 | Questionnaire and admin pages | `R/mod_questionnaire.R`, `R/mod_admin.R` |
| Person 4 | Resident pages after the schedule is out | `R/mod_results.R` (My slots and Schedule pages), `dev/` (test scripts for these pages) |

What each file does:

| File | Contents |
|---|---|
| `app.R` | Joins modules into the navbar app; shared library calls |
| `R/data_load.R` | `load_survey()`, `load_config()`, `load_timetable()`, `build_free_windows()` (free time per person per date), `db_connect()` and database read/write helpers for the tables below; `slot_penalties()`: who loses slots for going over the cancellation or no-show limit |
| `R/optimiser.R` | `run_optimiser()`: free windows + survey + config + each person's last booking date from the previous cycle to list(schedule, underserved, status, diagnostics); `run_optimiser()` also takes the slots each person lost to penalties; `optimise_schedule()`: wrapper returning only the schedule |
| `R/checker.R` | `check_schedule()`: validates a schedule against every rule below, returns problems |
| `R/simulate_blockC.R` | `simulate_blockC()`: simulated block C residents (survey format) for the pre-pilot; defines functions only, nothing runs on app start |
| `R/mod_questionnaire.R` | Questionnaire page: block, program, course, blocked times (with the 10-hour check), preferences |
| `R/mod_admin.R` | Admin page: pick cycle start date, run optimiser and checker, publish; see who lost slots; view cancellations and no-shows. `mod_admin_server()` returns only the published schedule (NULL until Publish), since the resident pages save whatever it returns as real bookings |
| `R/mod_results.R` | My slots page: own bookings, check-in button, "can't make it" cancels a slot, cancellations and no-shows against their limits. Schedule page: all machines by date, "Free now" list |
| `data/config.csv` | Machines, programs, opening hours, rules |
| `data/timetable.csv` | Weekly lecture timetable per course (public data): the real one, used for the pre-pilot |
| `data/fake_timetable.csv` | Small made-up timetable, for tests |
| `data/participants.csv` | Real names and emails of the people allowed to log in, loaded into `residents`. Git-ignored: never commit it |
| `data/fake_survey.csv` | Made-up residents (survey format) |
| `data/fake_schedule.csv` | A valid schedule (schedule format) |
| `data/fake_schedule_bad.csv` | A schedule that breaks the checker's rules |
| `data/fake_last_planned.csv` | Made-up last booking dates from a previous cycle, for tests |

## Workflow

- Work on your own branch (e.g. `optimiser`, `checker`, `questionnaire`, `results`), never directly on `main`.
- Merge into `main` through a pull request.
- `renv.lock` is shared: if you need a new package, ask the lead to add it so only one person changes the lockfile at a time.
- Until the optimiser works, build and test against `data/fake_schedule.csv`.

## Scheduling rules

**Cycle.** Schedules cover 10 days from a cycle start date the admin picks.
Everyone gets 2 bookings per cycle, less any slots lost to penalties. Extra
washes come only from free slots.

**Gap between washes.** A person's consecutive bookings are 4 or 5 days
apart (by date). This also applies across cycles: the first booking of a
cycle is 4-5 days after the person's last booking of the previous cycle.
To keep the next cycle possible, the second booking falls on day 6 or later
of the cycle. A person with no booking in the previous cycle has no
constraint on their first booking. The gap is measured from the dates the
optimiser planned, even if the person cancelled the slot or did not show up;
free slots they claimed do not count. If the 4-5 day window has already passed
when a cycle starts, as it can after a penalty, the upper limit is dropped for
that person's first booking.

**Time blocks.** Each day is cut into 30-minute blocks from opening time. A
booking starts on a block and holds the machine for
ceiling((program duration + buffer) / 30) blocks. The person's slot is start
to start + program duration; the buffer is machine time only.

**Free time.** For each person and date:
opening hours minus lectures of their course (including 30 minutes before
and after each lecture) minus their personal blocked times. A booking is
valid only if the whole wash (start to end) fits inside free time.

**Blocked times.** Each person may block at most 10 hours per day in total,
sleep included. A block that crosses midnight is written on the day it
starts (e.g. `Mon 23:30-07:30` runs into Tuesday) and counts towards that day.

**Other rules.**
- Only machines in the person's own block (blocks are single-gender).
- Out-of-order machines are never used.
- A machine runs at most one booking at a time, buffer included.
- Emergency reserve: on each date, each block keeps at least `reserve_share` of its machine time unbooked.
- Busy hours above `peak_target` are avoided where possible.
- Preferred days are honoured where possible; getting everyone a booking always comes first. There is no time-of-day preference: `pref_band` is ignored.
- If someone's free time leaves no valid booking, they are listed as underserved with the reason.

## Cancellations, no-shows and free slots

**Cancelling.** The person taps "Can't make it" up to 6 hours before the slot
starts. Slots cannot be moved to another time. Less than 6 hours before, the
slot cannot be cancelled: not checking in makes it a no-show.

**Check-in.** The person taps "Started" between the slot start and 15
minutes after. No check-in by then: the booking is marked a no-show and the
slot goes to waste. Nobody else can take it.

**Free slots.** A cancelled slot goes on the Schedule page's "Free now" list,
where anyone eligible can claim it, first come first served. Eligible means:
same block, their program fits in the machine time, it is inside their free
time, and they have no other booking that day. There is no queue and no
priority order, and someone can claim a slot whatever their count this cycle.

**Penalties.** In the 30 days before a cycle starts, a person may have 3
cancellations and 2 no-shows. Going over the cancellation limit loses 1 slot
in that cycle, and going over the no-show limit loses 1 slot, so going over
both loses both slots. `slot_penalties()` works this out when the admin
generates a schedule, and the admin must pass it to `run_optimiser()` and
`check_schedule()`. A person with one slot still keeps the 4-5 day gap from
their last planned booking.

## Survey data format

`data/fake_survey.csv`, one row per resident:

| Column | Type | Example |
|---|---|---|
| `resident_id` | string, unique | `R001` |
| `name` | string | `Aarav Okafor` |
| `block` | string, matches a `machines` key in config | `B` |
| `program` | string, matches a `program` key in config | `cotton` |
| `course` | string, matches a `course` in the timetable | `BTECH-CSE-Y2` |
| `blocked` | `;`-separated weekly `Day HH:MM-HH:MM` times the person can't wash, sleep included, at most 10 h per day; an end before the start means it runs past midnight | `Mon 23:30-07:30;Sat 07:00-09:00` |
| `pref_days` | optional, `;`-separated weekdays | `Sat;Sun` |
| `pref_band` | optional, a `band` key from config | `evening` |

Days are `Mon Tue Wed Thu Fri Sat Sun`. Times are 24-hour `HH:MM`.
Everyone in a course is assumed to have that course's full timetable.
If `pref_days` / `pref_band` are missing or empty, the person has no preference.

## Timetable format

`data/timetable.csv`, one row per weekly lecture:

| Column | Type | Example |
|---|---|---|
| `course` | string | `BTECH-CSE-Y2` |
| `day` | `Mon` ... `Sun` | `Tue` |
| `start` | `HH:MM` | `09:00` |
| `end` | `HH:MM` | `10:30` |

## Free windows format

`build_free_windows(survey, timetable, config, cycle_start)` returns, and the
optimiser takes, one row per free stretch of time:

| Column | Type | Example |
|---|---|---|
| `resident_id` | from the survey | `R001` |
| `date` | `YYYY-MM-DD`, within the cycle | `2026-11-03` |
| `from` | `HH:MM` | `14:00` |
| `to` | `HH:MM` | `17:30` |

## Config format

`data/config.csv` has columns `section,key,value`:

- `machines,<block>,<count>`: number of machines in each block
- `program,<name>,<minutes>`: wash program durations
- `hours,open|close,HH:MM`: laundry room opening hours
- `hours,days,Mon;Tue;...`: days the laundry is open
- `hours,buffer_min,<minutes>`: machine time reserved after each wash (default 10)
- `rules,grid_min,<minutes>`: time block length (default 30)
- `rules,cycle_days,<days>`: cycle length (default 10)
- `rules,slots_per_cycle,<n>`: bookings per person per cycle (default 2; the optimiser supports only 2)
- `rules,second_slot_min_day,<day>`: earliest cycle day for the second booking (default cycle_days - gap_max_days + 1, i.e. 6)
- `rules,gap_min_days,<days>` and `rules,gap_max_days,<days>`: days between a person's consecutive bookings (default 4 and 5)
- `rules,lecture_margin_min,<minutes>`: unavailable time before and after each lecture (default 30)
- `rules,max_blocked_hours,<hours>`: personal blocked time allowed per day, sleep included (default 10)
- `rules,cancel_cutoff_hours,<hours>`: latest a slot can be cancelled before it starts (default 6)
- `rules,max_cancellations,<n>` and `rules,max_no_shows,<n>`: how many are allowed before a slot is lost (default 3 and 2)
- `rules,penalty_window_days,<days>`: how far back they are counted from the cycle start (default 30)
- `rules,checkin_grace_min,<minutes>`: time after slot start to check in (default 15)
- `rules,reserve_share,<0-1>`: share of machine time per block per date kept free as emergency reserve (default 0)
- `rules,peak_target,<minutes>`: booked machine-minutes per block per clock hour above which the optimiser applies a penalty (default: no penalty)
- `out_of_order,<machine id>,1`: machine that must not be used, e.g. `out_of_order,C-2,1` (default: none)
- `band,<name>,HH:MM-HH:MM`: time bands for `pref_band`, e.g. `band,evening,17:00-24:00`

All rows except `machines`, `program` and `hours,open|close|days` are optional.

## Schedule format

The optimiser returns, and the checker and results pages take, a data.frame
with one row per booking:

| Column | Type | Example |
|---|---|---|
| `person` | `resident_id` from the survey | `R001` |
| `machine` | `<block>-<number>` | `B-2` |
| `date` | `YYYY-MM-DD` | `2026-11-03` |
| `start` | `HH:MM` | `09:30` |
| `end` | `HH:MM`, start + program duration | `11:00` |

## Database tables

SQLite, created and accessed through helpers in `R/data_load.R`:

- `bookings`: `booking_id, person, machine, date, start, end, status, source, checked_in_at, finished_at`.
  `checked_in_at` and `finished_at` are local time, `YYYY-MM-DD HH:MM:SS`,
  empty until set. `status` is `booked`, `checked_in`, `released`,
  `no_show`, `moved` or `finished`. Use `check_in()` and `check_out()` to set
  them; `set_booking_status()` refuses `finished`.
  `source` is `optimiser` or `free_now`. Rows with `source = optimiser` (any
  status) always hold the planned dates used for the next cycle's gap. The
  `moved` status and the `change` and `queue` sources only appear in old data.
- `changes`: `person, booking_id, at, kind`. `kind` is `release` for a
  cancellation, counted for the limit (`change` only in old data).
- `queue`: old data only, no longer written. Remove it with the Admin page's Queue tab.
- `residents`: `resident_id, name, email, block`. The people allowed to log
  in, loaded from `data/participants.csv` (git-ignored, emails must end in
  `@mse.ac.in`). `write_residents()` replaces the whole list; someone removed
  loses login but keeps their consents, bookings and feedback.
- `consents`: `resident_id, agreed_at, terms_version`. One row per resident
  and version, keeping the first `agreed_at`.
- `feedback`: `feedback_id, booking_id, person, rating, comment, at`. Rating
  is 1 to 5, only for a `finished` booking, at most one per booking.

`db_init()` adds new tables and upgrades an older `bookings` table without
losing data. `responses` keeps exactly the columns the questionnaire writes.

**Sharing between laptops.** Answers are collected on several laptops, each
with its own database. `export_responses(con, path)` writes responses,
residents and consents to one `.rds` file (real names and emails, do not
commit). `import_responses(con, paths, prefer = NULL)` merges files into
this database, all or nothing: different answers for the same resident stop
the import and list them, until `prefer` names the file (or `"database"`)
that wins. Consents keep the earliest `agreed_at`. Bookings are never
shared: the schedule is made once, after merging, on one laptop.
