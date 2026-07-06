params <-
list(family = "red", preset = "homage")

## ----setup, include = FALSE---------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("albersdown", quietly = TRUE)) ggplot2::theme_set(albersdown::theme_albers(family = params$family, preset = params$preset))
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 7,
  fig.height = 4
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

## ----load-packages------------------------------------------------------------
library(bidser)
library(dplyr)
library(tidyr)
library(tibble)

## ----build-mock, include = FALSE----------------------------------------------
temp_dir <- tempfile("bids_confounds_vignette_")
dir.create(temp_dir)

# participants.tsv
readr::write_tsv(
  tibble(participant_id = c("sub-01", "sub-02"), age = c(25L, 30L), sex = c("M", "F")),
  file.path(temp_dir, "participants.tsv")
)

# dataset_description.json
jsonlite::write_json(
  list(Name = "ConfoundDemo", BIDSVersion = "1.7.0"),
  file.path(temp_dir, "dataset_description.json"),
  auto_unbox = TRUE
)

# Create raw + derivative structure
set.seed(42)
n_vols <- 100

for (sub in c("01", "02")) {
  for (task in c("rest", "nback")) {
    # --- raw ---
    func_dir <- file.path(temp_dir, paste0("sub-", sub), "func")
    dir.create(func_dir, recursive = TRUE, showWarnings = FALSE)

    bold_name <- sprintf("sub-%s_task-%s_run-01_bold.nii.gz", sub, task)
    file.create(file.path(func_dir, bold_name))

    events_name <- sprintf("sub-%s_task-%s_run-01_events.tsv", sub, task)
    n_trials <- if (task == "nback") 40 else 0
    if (n_trials > 0) {
      ev <- tibble(
        onset = sort(runif(n_trials, 0, 290)),
        duration = rep(2, n_trials),
        trial_type = sample(c("0back", "2back"), n_trials, replace = TRUE),
        response_time = round(rnorm(n_trials, 0.8, 0.15), 3)
      )
      readr::write_tsv(ev, file.path(func_dir, events_name))
    } else {
      readr::write_tsv(
        tibble(onset = numeric(0), duration = numeric(0), trial_type = character(0)),
        file.path(func_dir, events_name)
      )
    }

    # --- derivatives/fmriprep ---
    deriv_func <- file.path(temp_dir, "derivatives", "fmriprep",
                            paste0("sub-", sub), "func")
    dir.create(deriv_func, recursive = TRUE, showWarnings = FALSE)

    preproc_name <- sprintf(
      "sub-%s_task-%s_run-01_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz",
      sub, task
    )
    file.create(file.path(deriv_func, preproc_name))

    conf_name <- sprintf(
      "sub-%s_task-%s_run-01_desc-confounds_timeseries.tsv",
      sub, task
    )
    conf <- tibble(
      csf = rnorm(n_vols), white_matter = rnorm(n_vols), global_signal = rnorm(n_vols),
      framewise_displacement = abs(rnorm(n_vols, 0.2, 0.1)),
      trans_x = cumsum(rnorm(n_vols, 0, 0.02)),
      trans_y = cumsum(rnorm(n_vols, 0, 0.02)),
      trans_z = cumsum(rnorm(n_vols, 0, 0.02)),
      rot_x   = cumsum(rnorm(n_vols, 0, 0.001)),
      rot_y   = cumsum(rnorm(n_vols, 0, 0.001)),
      rot_z   = cumsum(rnorm(n_vols, 0, 0.001)),
      trans_x_derivative1 = c(NA, diff(cumsum(rnorm(n_vols, 0, 0.02)))),
      trans_y_derivative1 = c(NA, diff(cumsum(rnorm(n_vols, 0, 0.02)))),
      trans_z_derivative1 = c(NA, diff(cumsum(rnorm(n_vols, 0, 0.02)))),
      rot_x_derivative1   = c(NA, diff(cumsum(rnorm(n_vols, 0, 0.001)))),
      rot_y_derivative1   = c(NA, diff(cumsum(rnorm(n_vols, 0, 0.001)))),
      rot_z_derivative1   = c(NA, diff(cumsum(rnorm(n_vols, 0, 0.001)))),
      trans_x_power2 = trans_x^2, trans_y_power2 = trans_y^2, trans_z_power2 = trans_z^2,
      rot_x_power2 = rot_x^2, rot_y_power2 = rot_y^2, rot_z_power2 = rot_z^2,
      trans_x_derivative1_power2 = trans_x_derivative1^2,
      trans_y_derivative1_power2 = trans_y_derivative1^2,
      trans_z_derivative1_power2 = trans_z_derivative1^2,
      rot_x_derivative1_power2 = rot_x_derivative1^2,
      rot_y_derivative1_power2 = rot_y_derivative1^2,
      rot_z_derivative1_power2 = rot_z_derivative1^2,
      a_comp_cor_00 = rnorm(n_vols), a_comp_cor_01 = rnorm(n_vols),
      a_comp_cor_02 = rnorm(n_vols), a_comp_cor_03 = rnorm(n_vols),
      a_comp_cor_04 = rnorm(n_vols), a_comp_cor_05 = rnorm(n_vols),
      t_comp_cor_00 = rnorm(n_vols), t_comp_cor_01 = rnorm(n_vols),
      t_comp_cor_02 = rnorm(n_vols),
      cosine_00 = cos(seq(0, pi, length.out = n_vols)),
      cosine_01 = cos(seq(0, 2 * pi, length.out = n_vols)),
      cosine_02 = cos(seq(0, 3 * pi, length.out = n_vols))
    )
    readr::write_tsv(conf, file.path(deriv_func, conf_name))
  }
}

# Also write dataset_description.json for fmriprep derivative
deriv_root <- file.path(temp_dir, "derivatives", "fmriprep")
jsonlite::write_json(
  list(Name = "fMRIPrep", BIDSVersion = "1.7.0",
       GeneratedBy = list(list(Name = "fMRIPrep", Version = "21.0.0"))),
  file.path(deriv_root, "dataset_description.json"),
  auto_unbox = TRUE
)

## ----load-project-------------------------------------------------------------
proj <- bids_project(temp_dir, fmriprep = TRUE)
proj

## ----list-sets----------------------------------------------------------------
list_confound_sets()

## ----show-motion-sets---------------------------------------------------------
# 6 rigid-body motion parameters
confound_set("motion6")

# Friston 24-parameter expansion (motion + derivatives + squares)
length(confound_set("motion24"))

## ----read-motion6-------------------------------------------------------------
conf_nested <- read_confounds(proj, cvars = confound_set("motion6"))
conf_nested

## ----unnest-motion------------------------------------------------------------
conf_nested |>
  unnest(data) |>
  select(participant_id, task, run, trans_x, rot_x) |>
  head()

## ----wildcard-confounds-------------------------------------------------------
# All CompCor components
compcor_conf <- read_confounds(
  proj,
  subid = "01", task = "rest",
  cvars = c("a_comp_cor_*", "t_comp_cor_*")
)
names(compcor_conf$data[[1]])

## ----flat-confounds-----------------------------------------------------------
conf_flat <- read_confounds(
  proj,
  cvars = confound_set("motion6"),
  nest = FALSE
)
dim(conf_flat)
head(conf_flat)

## ----show-strategies----------------------------------------------------------
list_confound_strategies()

## ----pca-strategy-------------------------------------------------------------
strat <- confound_strategy("pcabasic80")
conf_pca <- read_confounds(proj, subid = "01", task = "rest", cvars = strat)
names(conf_pca$data[[1]])

## ----custom-strategy----------------------------------------------------------
my_strat <- confound_strategy(
  pca_vars = c(confound_set("motion24"), confound_set("acompcor")),
  raw_vars = c("framewise_displacement", confound_set("cosine")),
  npcs = 5
)

conf_custom <- read_confounds(proj, subid = "01", task = "nback", cvars = my_strat)
names(conf_custom$data[[1]])

## ----read-events--------------------------------------------------------------
events <- read_events(proj, task = "nback")
events

## ----unnest-events------------------------------------------------------------
trials <- events |>
  unnest(data) |>
  select(.subid, .task, .run, onset, duration, trial_type, response_time)

head(trials)

## ----trial-summary------------------------------------------------------------
trials |>
  group_by(.subid, trial_type) |>
  summarise(
    n_trials = n(),
    mean_rt = mean(response_time, na.rm = TRUE),
    .groups = "drop"
  )

## ----load-all-events----------------------------------------------------------
all_events <- load_all_events(proj, task = "nback")
nrow(all_events)

## ----variables-table----------------------------------------------------------
vars <- variables_table(proj)
vars |> select(.subid, .task, .run, any_of(c("n_scans", "n_events", "n_confound_rows")))

## ----variables-events-only----------------------------------------------------
vars_events <- variables_table(proj, task = "nback", include = "events")
vars_events |> select(.subid, .task, .run, n_events)

## ----per-run-loop-------------------------------------------------------------
# Use the nback-only table which has both events and confounds columns
vars_nback <- variables_table(proj, task = "nback")

vars_nback |>
  filter(n_events > 0, n_confound_rows > 0) |>
  rowwise() |>
  mutate(
    n_conditions = length(unique(events$trial_type)),
    mean_fd = mean(confounds$framewise_displacement, na.rm = TRUE)
  ) |>
  ungroup() |>
  select(.subid, .task, n_conditions, mean_fd)

## ----bids-report--------------------------------------------------------------
report <- bids_report(proj)
report

## ----report-data--------------------------------------------------------------
rdata <- bids_report_data(proj)
rdata$summary
rdata$run_coverage

## ----coverage-check-----------------------------------------------------------
rdata$run_coverage |>
  mutate(
    has_scans = n_scans > 0,
    has_events = if ("n_events" %in% names(rdata$run_coverage)) n_events > 0 else FALSE,
    has_confounds = n_confound_rows > 0
  )

## ----full-pipeline------------------------------------------------------------
analysis_data <- variables_table(proj, task = "nback") |>
  filter(n_events > 0, n_confound_rows > 0) |>
  rowwise() |>
  mutate(
    n_trials = nrow(events),
    n_vols = nrow(confounds)
  ) |>
  ungroup() |>
  select(.subid, .task, .run, n_trials, n_vols)

analysis_data

## ----cleanup, include = FALSE-------------------------------------------------
unlink(temp_dir, recursive = TRUE)

