params <-
list(family = "red")

## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## ----theme-setup, include = FALSE---------------------------------------------
suppressPackageStartupMessages({
  library(bidser)
  library(tibble)
  library(dplyr)
  library(tidyr)
  library(gluedown)
})

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
t1w_files <- search_files(proj, regex = "T1w\\.nii", full_path = FALSE)
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
# Find all JSON sidecar files
json_files <- search_files(proj, regex = "\\.json$")
cat("Found", length(json_files), "JSON files\n")

# Find files for specific runs
run1_files <- search_files(proj, regex = "bold", run = "01")
cat("Found", length(run1_files), "files from run 01\n")

# Complex pattern matching: T1w files for subjects 01-05
t1w_subset <- search_files(proj, regex = "T1w", subid = "0[1-5]")
cat("Found", length(t1w_subset), "T1w files for subjects 01-05\n")

## -----------------------------------------------------------------------------
# Get full paths to functional scans for analysis
full_paths <- func_scans(proj, subid = "01", full_path = TRUE)
full_paths

# Check that files actually exist
all(file.exists(full_paths))

## ----derivatives, eval = FALSE------------------------------------------------
# # Download an fMRIPrep example dataset
# deriv_path <- get_example_bids_dataset("ds000001-fmriprep")
# proj_deriv <- bids_project(deriv_path, fmriprep = TRUE)
# 
# proj_deriv
# 
# # Convenience functions for derivative files, e.g. preprocessed scans:
# pscans <- preproc_scans(proj_deriv)
# head(as.character(pscans))
# 
# # Read confound files
# conf <- read_confounds(proj_deriv, subid = "01")

## ----cleanup, include=FALSE---------------------------------------------------
if (exists("ds001_path")) unlink(ds001_path, recursive = TRUE)
if (exists("deriv_path")) unlink(deriv_path, recursive = TRUE)

