## Load pre-exclusion combined trial-per-row data, subset columns
## Filter to main, target-present trials.
load_task_all <- function(proc_dir) {
  aah_task_all <- readr::read_tsv(
    here(proc_dir, "combined", "combined_task.tsv"),
    col_types = cols(
      subject = col_character(),
      time_elapsed_ms = col_double(),
      blocknum = col_double(),
      block_type = col_character(),
      block_response = col_character(),
      trialnum = col_double(),
      target = col_character(),
      level = col_character(),
      field = col_character(),
      target_present = col_character(),
      response = col_character(),
      correct = col_double(),
      rt = col_double(),
    )
  ) |>
    filter(block_type == "main" & target_present == "yes") |>
    select(subject,
           block_response,
           target,
           level,
           field,
           correct,
           rt)
  return(aah_task_all)
}

## Load pre-exclusion summary data, subset columns, and recode "ehi_total" to "ehi."
## Depends on: lib/load_process/load_process.R
load_summary_all <- function(proc_dir) {
  aah_summary_all <- load_summary(proc_dir) |>
    select(
      subject,
      first_block,
      ehi_total,
      age,
      country,
      sex,
      education,
      race,
      hispanic_ethnicity,
      rt_overall,
      duration_s,
      task_experience_response,
      task_experience_other_response,
      open_ended_feedback_response,
      exclude_many_gos,
      exclude_low_acc,
      exclude_low_rt,
      exclude_high_rt
    ) |> rename(ehi = ehi_total)
}