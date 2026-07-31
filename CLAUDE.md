# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

An R research project modelling the invasion front of cane toads (Rhinella marina) in Western Australia.
Bayesian occupancy/detection models (JAGS, via `rjags`; one legacy model uses `nimble`) are fit to
nocturnal time-to-detection survey data and to a separate fixed-effort visual-count dataset (the
"Clulow" data), to estimate the current location of the invasion front and how it has moved year over
year. There is no package structure (no `DESCRIPTION`, no `renv`/`packrat` lockfile) — this is a
collection of analysis scripts run interactively/top-to-bottom from an RStudio project
(`invasion-front-monitoring.Rproj`).

## Running the analysis

There is no build/test/lint tooling. Scripts are run directly in R (typically from RStudio), e.g.:

```r
source("src/time-to-detection-invasion-front_JAGS.R")
```

Every analysis script starts with `source("src/a-load-data.R")`, which must succeed first — it needs an
external `DATA_PATH` environment variable pointing at a directory containing the raw survey data (see
below). There's no test suite; correctness is checked by inspecting `gelman.diag()` / trace and density
plots and the output plots written to `out/`.

### Data dependency (`DATA_PATH`)

`src/a-load-data.R` reads raw data from outside the repo (data files are git-ignored: `dat/`, `admin/`,
`out/`, `*.pdf`, `*.html`). It resolves the data directory as:

- Windows: `$DATA_PATH/invasion-front-monitoring`
- otherwise: `$DATA_PATH/Toads/invasion-front-monitoring`

Expected inputs under that directory: `clulow-data/cane_toad_pr_ab_surveys.xlsx`,
`clulow-data/2023_Honours_allSurvey_siteLevel_cleaned.xlsx`,
`clulow-data/2024_Rmar24_visual-surveys_cleaned.xlsx`, and
`invasion-front-reconnaissance-data.xlsx`. The 2025 visual-survey data is instead fetched live from a
Fulcrum share URL (geojson) at run time — network access is required.

Some model scripts also write fitted parameters back out to `$DATA_PATH` (not into `out/`), e.g.
`invasion-front-parameters.Rdata`, `invasion-front-parameters-multi-year.Rdata`.

### Sibling repo dependency

`src/annual-forecast/annual-forecast.R` and `annual-forecast-data-prep.R` source files and load data from
a **sibling repository** at `../spread-model` (relative to this project directory) — e.g.
`../spread-model/src/pprocess_functions.R` and `../spread-model/dat/Posteriors.RData`. These scripts will
not run unless that repo is checked out alongside this one, and `annual-forecast.R` temporarily `setwd()`s
into it while running simulations before switching back.

## Architecture

**Data loading is centralized.** `src/a-load-data.R` is the single entry point every other script sources
first. It builds two key data frames, both `sf` objects with lon/lat geometry (WGS84):

- `df` — the "main" survey dataset: Fulcrum nocturnal/interview surveys (2025) merged with reconnaissance
  survey data from Ben & Tim (2023-4), plus a few synthetic non-surveyed points added only to fill out an
  alpha hull. Key columns: `toad.present`, `person.minutes`, `p.m.positive` (person-minutes to detection,
  NA if not detected), `survey_type` (`nocturnal`/`interview`), `water_available`, `year`.
- `df.clulow` — the separate Clulow et al. dataset (2022-2024), with both presence/absence-only records
  and a subset with actual encounter counts (`total.count`). Some records lack counts entirely.

Both frames get saved to `out/merged-visual-surveys.RData` and get mapped by `src/map-toad-presence.R`
(sourced automatically at the end of `a-load-data.R`), which produces both a static ggplot map
(`out/map-toad-presence.pdf`) and an interactive leaflet map showing presence/absence by year and dataset.

**Model progression.** The `src/*_JAGS.R` scripts (plus matching `.txt` JAGS model definitions in
`src/model-files/`) form a progression of increasingly complete models, all built around the same core
idea described in `ms/model-descriptions.Rmd`:

1. `time-to-detection_JAGS.R` — single-year, no spatial structure: just estimates encounter rate `lambda`
   and overall occupancy probability `p.occ` from nocturnal TTD data.
2. `time-to-detection-invasion_JAGS.R` — adds space and time: fits a linear invasion front
   (`y = a*x + b`, with a `beta` term for a temporal trend) to nocturnal data only.
3. `time-to-detection-invasion-front_JAGS.R` — front-of-current-year model: uses the latest year's data
   plus any historical presence records to fit a single front line, and renders it over a satellite basemap
   (`out/current-front.pdf`, `out/front-location.pdf`).
4. `time-to-detection-multi-year_JAGS.R` — the most complete model: a **shared slope `a`** across years
   with a **year-specific intercept `b[k]`**, jointly fit to nocturnal TTD data *and* the Clulow count/PA
   data (count data uses a Poisson likelihood, PA-only data uses the same Bernoulli/TTD-style likelihood).
   Produces `out/multi-year-front.pdf` and posterior front-movement-between-years plots
   (`out/front-movement-posteriors.pdf`), derived from `delta[k] = (b[k+1]-b[k]) / sqrt(1+a^2)`.

Occupancy in these spatial models is a **deterministic step function** of perpendicular distance from the
front line (`step()` in JAGS) — a site is either on the toad side of the front or not; detection
probability is what's stochastic. All survey types share a single encounter-rate parameter `lambda`,
which is what links the disparate data sources together. Coordinates are always projected to Albers
equal-area (EPSG:3577), centred on the data's mean, and usually scaled to km before being passed into JAGS
— get the same `mean.coord`/scale transform right when adding new spatial data or the model will not line
up with existing code that maps posterior samples back to lon/lat (see the `pred_to_sf()` helpers).

`src/simtest.R` is a standalone simulation-recovery check (using `AHMbook::simOccttd`) for the simplest
TTD model — useful as a sanity check when modifying `time-to-detection_JAGS.txt`, not part of the main
pipeline.

`ms/model-descriptions.Rmd` is the authoritative mathematical description of the current (multi-year)
model — read it before changing model structure, priors, or likelihoods.

## Conventions

- Scripts use base-R assignment sparingly; almost everything is written in tidyverse/`|>` pipe style.
- Column naming is inconsistent by data source (dots in `df`/`df.clulow`-derived names, underscores in raw
  Fulcrum columns) — expect to see both `person.minutes`-style and `search_time_mins`-style names in the
  same script, and don't assume renaming to one convention has happened.
- Projected/centred coordinates consistently use the `X.c`/`Y.c` naming convention across scripts.
