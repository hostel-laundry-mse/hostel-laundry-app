# Hostel Laundry Slot Scheduler

A Shiny app that books every hostel resident into a washing machine twice
per 10-day cycle, 4-5 days apart, around their lectures and blocked times,
using an integer program (ompr + GLPK). Residents can check in, change or
release a slot, and queue for slots others release.

## Requirements

- R 4.6 or later
- The packages listed in `renv.lock` (installed by renv, see below)

## Setup

Open R in the project folder. renv activates automatically via `.Rprofile`.
Then restore the packages:

```r
renv::restore()
```

## Run the app

From R in the project folder:

```r
shiny::runApp()
```

Or from a terminal:

```sh
Rscript -e "shiny::runApp(launch.browser = TRUE)"
```

## Pages

- **Questionnaire**: residents enter block, program, course, blocked times and preferences.
- **My slots**: a resident's slots, check-in, "can't make it" (change or release), extra-slot queue.
- **Schedule**: the full schedule for every machine by date, and the "Free now" list.
- **Admin**: pick the cycle start, run the optimiser and checker, publish; see queue, changes and no-shows.

## Data

The laundry machines run 24 hours (00:00-24:00); residents' sleep is handled by their own blocked times; the last normal wash starts 22:30 and the last quick wash 23:00 so the buffer ends by midnight.

- `data/config.csv`: machines per block, programs and durations, opening hours, scheduling rules.
- `data/timetable.csv`: weekly lecture timetable per course.
- `data/fake_survey.csv`: made-up residents for development.
- `data/fake_schedule.csv`: a valid schedule for testing the results pages and checker.
- `data/fake_schedule_bad.csv`: a schedule that breaks the rules, for testing the checker.

Other files in `data/` and any `*.sqlite` database are git-ignored.

## Working on this repo

See [CLAUDE.md](CLAUDE.md) for who owns which file and the branch workflow.
