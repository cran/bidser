params <-
list(family = "red", preset = "homage")

## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## ----theme-setup, include = FALSE---------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE) &&
    requireNamespace("albersdown", quietly = TRUE)) {
  ggplot2::theme_set(
    albersdown::theme_albers(family = params$family, preset = params$preset)
  )
}
suppressPackageStartupMessages({
  library(bidser)
  library(tibble)
  library(dplyr)
  library(tidyr)
  library(gluedown)
})

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

## ----setup, include = FALSE---------------------------------------------------
ds001_path <- tryCatch(
  get_example_bids_dataset("ds001"),
  error = function(e) NULL
)
if (is.null(ds001_path)) {
  knitr::knit_exit("Example dataset not available.")
}
proj <- bids_project(ds001_path)

## -----------------------------------------------------------------------------
proj

## -----------------------------------------------------------------------------
# Check if the dataset has multiple sessions per subject
sessions(proj)

# Get all participant IDs
participants(proj)

# What tasks are included?
tasks(proj)

# Get a summary of the dataset
bids_summary(proj)

## -----------------------------------------------------------------------------
# Find all anatomical T1-weighted images
t1w_files <- query_files(proj, regex = "T1w\\.nii", full_path = FALSE)
head(t1w_files)

# Find all functional BOLD scans
bold_files <- func_scans(proj, full_path = FALSE)
head(bold_files)

## -----------------------------------------------------------------------------
# Get functional scans for specific subjects
sub01_scans <- func_scans(proj, subid = "01")
sub02_scans <- func_scans(proj, subid = "02")

cat("Subject 01:", length(sub01_scans), "scans\n")
cat("Subject 02:", length(sub02_scans), "scans\n")

# Filter by task (ds001 only has one task, but this shows the syntax)
task_scans <- func_scans(proj, task = "balloonanalogrisktask")
cat("Balloon task:", length(task_scans), "scans total\n")

# Combine filters: specific subject AND task
sub01_task_scans <- func_scans(proj, subid = "01", task = "balloonanalogrisktask")
cat("Subject 01, balloon task:", length(sub01_task_scans), "scans\n")

## -----------------------------------------------------------------------------
# Get scans for subjects 01, 02, and 03
first_three_scans <- func_scans(proj, subid = "0[123]")
cat("First 3 subjects:", length(first_three_scans), "scans total\n")

# Get scans for all subjects (equivalent to default)
all_scans <- func_scans(proj, subid = ".*")
cat("All subjects:", length(all_scans), "scans total\n")

## -----------------------------------------------------------------------------
# Find all event files
event_file_paths <- event_files(proj)
cat("Found", length(event_file_paths), "event files\n")

# Read event data into a nested data frame
events_data <- read_events(proj)
events_data

## -----------------------------------------------------------------------------
# Unnest events for subject 01
first_subject_events <- events_data %>%
  filter(.subid == "01") %>%
  unnest(cols = c(data))

head(first_subject_events)
names(first_subject_events)

## -----------------------------------------------------------------------------
# How many trials per subject?
trial_counts <- events_data %>%
  unnest(cols = c(data)) %>%
  group_by(.subid) %>%
  summarise(n_trials = n(), .groups = "drop")

trial_counts

## -----------------------------------------------------------------------------
# Read the task-level sidecar directly
direct_sidecars <- read_sidecar(
  proj,
  task = "balloonanalogrisktask",
  inherit = FALSE
)

nrow(direct_sidecars)
direct_sidecars %>% select(any_of(c("RepetitionTime", "TaskName")))

## -----------------------------------------------------------------------------
# Resolve effective metadata for a specific BOLD file
resolved_meta <- get_metadata(proj, bold_files[[1]], inherit = TRUE)

names(resolved_meta)
resolved_meta$RepetitionTime

## -----------------------------------------------------------------------------
# Create a subject-specific interface for subject 01
subject_01 <- bids_subject(proj, "01")

# Get all functional scans for this subject
sub01_scans <- subject_01$scans()
cat("Subject 01:", length(sub01_scans), "functional scans\n")

# Get event files for this subject
sub01_events <- subject_01$events()
cat("Subject 01:", length(sub01_events), "event files\n")

# Read event data for this subject
sub01_event_data <- subject_01$events()
sub01_event_data

## -----------------------------------------------------------------------------
subjects_to_analyze <- c("01", "02", "03")

for (subj_id in subjects_to_analyze) {
  subj <- bids_subject(proj, subj_id)
  scans <- subj$scans()
  events <- subj$events()
  cat(sprintf("Subject %s: %d scans, %d event files\n",
              subj_id, length(scans), length(events)))
}

## -----------------------------------------------------------------------------
subject_trial_summary <- lapply(participants(proj)[1:3], function(subj_id) {
  subj <- bids_subject(proj, subj_id)
  event_data <- subj$events()
  n_trials <- if (nrow(event_data) > 0) {
    event_data %>% unnest(cols = c(data)) %>% nrow()
  } else {
    0
  }
  tibble(subject = subj_id, n_trials = n_trials, n_scans = length(subj$scans()))
}) %>% bind_rows()

subject_trial_summary

## -----------------------------------------------------------------------------
# Exact entity matching -- reproducible, no regex surprises
exact_bold <- query_files(
  proj,
  regex = "bold\\.nii\\.gz$",
  subid = "01",
  task = "balloonanalogrisktask",
  match_mode = "exact"
)
cat("Exact-match BOLD files:", length(exact_bold), "\n")

# Regex entity matching -- select multiple values with patterns
regex_bold <- query_files(
  proj,
  regex = "bold\\.nii\\.gz$",
  subid = "0[1-3]",
  task = "balloon.*",
  match_mode = "regex"
)
cat("Regex-match BOLD files:", length(regex_bold), "\n")

# Glob matching -- shell-style wildcards
glob_bold <- query_files(
  proj,
  regex = "bold\\.nii\\.gz$",
  subid = "0*",
  match_mode = "glob"
)
cat("Glob-match BOLD files:", length(glob_bold), "\n")

## -----------------------------------------------------------------------------
# Require the queried entity to actually exist on returned files
task_annotated <- query_files(
  proj,
  regex = "\\.nii\\.gz$",
  task = ".*",
  require_entity = TRUE,
  scope = "raw"
)
cat("Files with an explicit task entity:", length(task_annotated), "\n")

# Filter by extension and datatype directly
json_files <- query_files(proj, extension = "\\.json$")
cat("JSON files:", length(json_files), "\n")

func_niftis <- query_files(proj, datatype = "func", extension = "\\.nii\\.gz$")
cat("Functional NIfTIs:", length(func_niftis), "\n")

## -----------------------------------------------------------------------------
# Return a tibble instead of paths -- includes all parsed BIDS entities
bold_tbl <- query_files(
  proj,
  regex = "bold\\.nii\\.gz$",
  subid = "0[1-3]",
  return = "tibble"
)
bold_tbl |> select(path, subid, task, run)

## ----derivatives-query, eval = FALSE------------------------------------------
# deriv_path <- get_example_bids_dataset("ds000001-fmriprep")
# proj_deriv <- bids_project(deriv_path)
# 
# # Search only derivatives from a specific pipeline
# prep_bold <- query_files(
#   proj_deriv,
#   regex = "bold\\.nii\\.gz$",
#   desc = "preproc",
#   scope = "derivatives",
#   pipeline = "fmriprep",
#   match_mode = "exact"
# )
# 
# # Or use the convenience wrapper
# deriv_bold <- derivative_files(proj_deriv, pipeline = "fmriprep",
#                                regex = "bold\\.nii\\.gz$")
# 
# # Search everywhere and get a tibble with scope/pipeline columns
# all_bold <- query_files(
#   proj_deriv,
#   regex = "bold\\.nii\\.gz$",
#   scope = "all",
#   return = "tibble"
# )

## ----permissive-project, eval = FALSE-----------------------------------------
# proj_relaxed <- bids_project(
#   "/path/to/bids",
#   strict_participants = FALSE
# )
# 
# # Check where participant IDs came from
# participants(proj_relaxed, as_tibble = TRUE)
# 
# # See which derivative pipelines were discovered
# derivative_pipelines(proj_relaxed)

## ----variables-report, eval = FALSE-------------------------------------------
# vars <- variables_table(
#   proj_deriv,
#   scope = "all",
#   pipeline = "fmriprep"
# )
# 
# vars[, c(".subid", ".task", ".run", "n_scans", "n_events", "n_confound_rows")]
# 
# report <- bids_report(proj_deriv, scope = "all", pipeline = "fmriprep")
# report

## -----------------------------------------------------------------------------
full_paths <- func_scans(proj, subid = "01", full_path = TRUE)
full_paths

all(file.exists(full_paths))

## ----derivatives, eval = FALSE------------------------------------------------
# deriv_path <- get_example_bids_dataset("ds000001-fmriprep")
# proj_deriv <- bids_project(deriv_path)
# 
# # See which pipelines were discovered
# derivative_pipelines(proj_deriv)
# 
# # Query preprocessed BOLD scans
# preproc <- query_files(
#   proj_deriv,
#   regex = "bold\\.nii\\.gz$",
#   desc = "preproc",
#   scope = "derivatives",
#   pipeline = "fmriprep",
#   return = "tibble"
# )
# head(preproc$path)
# 
# # Read confound regressors
# conf <- read_confounds(proj_deriv, subid = "01")

## ----cleanup, include=FALSE---------------------------------------------------
# Example datasets are cached by get_example_bids_dataset(); leave them in place.

