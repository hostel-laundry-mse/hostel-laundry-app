# Hostel Laundry Slot Scheduler

A Shiny app that collects residents' laundry preferences and builds a weekly
washing-machine schedule with an integer program (ompr + GLPK).

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

- **Questionnaire**: residents enter block, room, program, loads per week and availability.
- **My slots**: a resident's assigned slots.
- **Schedule**: the full schedule for every machine.
- **Admin**: load data, run the optimiser and checker, publish the schedule.

## Data

- `data/config.csv`: machines per block, programs and durations, opening hours.
- `data/fake_survey.csv`: made-up residents for development.
- `data/fake_schedule.csv`: a valid schedule for testing the results pages and checker.
- `data/fake_schedule_bad.csv`: a schedule that breaks the rules, for testing the checker.

Other files in `data/` and any `*.sqlite` database are git-ignored.

## Working on this repo

See [CLAUDE.md](CLAUDE.md) for who owns which file and the branch workflow.
