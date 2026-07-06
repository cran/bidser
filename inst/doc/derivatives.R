params <-
list(family = "red", preset = "homage")

## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## ----albers-classes, echo=FALSE, results='asis'-------------------------------
cat(sprintf(
  paste0(
    '<script>document.addEventListener("DOMContentLoaded",function(){',
    'document.body.classList.remove("palette-red","palette-lapis","palette-ochre","palette-teal","palette-green","palette-violet","preset-homage","preset-study","preset-structural","preset-adobe","preset-midnight");',
    'document.body.classList.add("palette-%s","preset-%s");',
    '});</script>'
  ),
  params$family,
  params$preset
))

## ----theme-setup, include = FALSE---------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE) &&
    requireNamespace("albersdown", quietly = TRUE)) {
  ggplot2::theme_set(
    albersdown::theme_albers(family = params$family, preset = params$preset)
  )
}

suppressPackageStartupMessages({
  library(bidser)
  library(dplyr)
  library(tidyr)
})

## ----build-fixture, include = FALSE-------------------------------------------
deriv_fixture <- tempfile("bidser-deriv-")
dir.create(deriv_fixture, recursive = TRUE)

readr::write_tsv(
  tibble::tibble(participant_id = "sub-01"),
  file.path(deriv_fixture, "participants.tsv")
)

jsonlite::write_json(
  list(Name = "DerivativeFixture", BIDSVersion = "1.8.0"),
  file.path(deriv_fixture, "dataset_description.json"),
  auto_unbox = TRUE
)

dir.create(file.path(deriv_fixture, "sub-01", "func"), recursive = TRUE)
file.create(file.path(
  deriv_fixture, "sub-01", "func", "sub-01_task-rest_run-01_bold.nii.gz"
))
readr::write_tsv(
  tibble::tibble(onset = 0, duration = 1, trial_type = "go"),
  file.path(
    deriv_fixture, "sub-01", "func", "sub-01_task-rest_run-01_events.tsv"
  )
)

fmriprep_root <- file.path(deriv_fixture, "derivatives", "fmriprep")
dir.create(file.path(fmriprep_root, "sub-01", "func"), recursive = TRUE)
jsonlite::write_json(
  list(Name = "fmriprep", BIDSVersion = "1.8.0", DatasetType = "derivative"),
  file.path(fmriprep_root, "dataset_description.json"),
  auto_unbox = TRUE
)
file.create(file.path(
  fmriprep_root,
  "sub-01", "func",
  "sub-01_task-rest_run-01_space-MNI_desc-preproc_bold.nii.gz"
))
readr::write_tsv(
  tibble::tibble(
    CSF = c(0.10, 0.20, 0.30, 0.20),
    WhiteMatter = c(0.40, 0.50, 0.60, 0.50),
    GlobalSignal = c(1.00, 1.10, 1.20, 1.10),
    FramewiseDisplacement = c(0.01, 0.02, 0.03, 0.02)
  ),
  file.path(
    fmriprep_root,
    "sub-01", "func",
    "sub-01_task-rest_run-01_desc-confounds_timeseries.tsv"
  )
)

qsiprep_root <- file.path(deriv_fixture, "derivatives", "qsiprep")
dir.create(file.path(qsiprep_root, "sub-01", "anat"), recursive = TRUE)
jsonlite::write_json(
  list(Name = "qsiprep", BIDSVersion = "1.8.0", DatasetType = "derivative"),
  file.path(qsiprep_root, "dataset_description.json"),
  auto_unbox = TRUE
)
file.create(file.path(
  qsiprep_root, "sub-01", "anat", "sub-01_space-MNI_desc-preproc_T1w.nii.gz"
))

proj <- bids_project(deriv_fixture, derivatives = "auto")

## ----overview-plot, fig.width=9, fig.height=7---------------------------------
plot_bids(proj, interactive = FALSE)

## ----list-pipelines-----------------------------------------------------------
pipes <- derivative_pipelines(proj)
pipes

stopifnot(
  nrow(pipes) == 2L,
  all(c("fmriprep", "qsiprep") %in% pipes$pipeline)
)

## ----query-preproc-run--------------------------------------------------------
prep_bold <- query_files(
  proj,
  regex = "bold\\.nii\\.gz$",
  scope = "derivatives",
  pipeline = "fmriprep",
  desc = "preproc",
  return = "tibble"
)

prep_bold[, c("path", "scope", "pipeline")]

stopifnot(
  nrow(prep_bold) == 1L,
  identical(prep_bold$scope[[1]], "derivatives"),
  identical(prep_bold$pipeline[[1]], "fmriprep")
)

## ----read-confounds-----------------------------------------------------------
confounds_flat <- read_confounds(
  proj,
  subid = "01",
  task = "rest",
  nest = FALSE
)

confounds_flat

stopifnot(
  nrow(confounds_flat) == 4L,
  all(is.finite(confounds_flat$FramewiseDisplacement)),
  max(confounds_flat$FramewiseDisplacement) < 0.05
)

## ----plot-confounds-----------------------------------------------------------
confounds_pca <- read_confounds(
  proj,
  subid = "01",
  task = "rest",
  npcs = 2
)

plot(confounds_pca, view = "aggregate")

## ----variables-coverage-------------------------------------------------------
run_variables <- variables_table(
  proj,
  scope = "all",
  pipeline = "fmriprep"
)

run_variables[, c(".subid", ".task", ".run", "n_scans", "n_events", "n_confound_rows")]

stopifnot(
  run_variables$n_scans[[1]] == 1L,
  run_variables$n_events[[1]] == 1L,
  run_variables$n_confound_rows[[1]] == 4L
)

## ----print-report-------------------------------------------------------------
report <- bids_report(
  proj,
  scope = "all",
  pipeline = "fmriprep"
)

report

## ----cleanup, include = FALSE-------------------------------------------------
unlink(deriv_fixture, recursive = TRUE, force = TRUE)

