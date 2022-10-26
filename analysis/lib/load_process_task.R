library(readr)
requireNamespace("cli")


## Given an input folder, load and process all task data
load_and_process <- function(input_dir, proc_dir, data_type) {
  input_files <- get_input_paths(input_dir, data_type)
  n_input_files <- length(input_files)
  for (j in (c(1: n_input_files))) {
    cli::cli_text("{.strong {j}/{n_input_files} ({data_type} data)...}")
    input_path <- input_files[j]
    data_raw <- load_raw(input_path, data_type)
    data_tests(data_raw, data_type)
    data_recoded <- recode_raw(data_raw, data_type)
    data_cleaned <- clean_recoded(data_recoded, data_type)
    save_cleaned(data_cleaned, proc_dir, data_type)
  }
  cli::cli_progress_step(msg = glue("Loading input file {j}/{n_input_files}..."),
                         msg_done = "Loaded {j}/{n_input_files} {data_type} data files from input directory.")
}

## First, get a list of all input files.
## (All files in the data type's input directory with extension .iqdat)
get_input_paths <- function(input_dir, data_type) {
  if (data_type == "task") {
    data_subdir <- here(input_dir, "raw")
  }
  cli::cli_alert_info(glue("Getting {data_type} data filepaths from input directory:"))
  cli::cli_bullets(c(" " = glue("{data_subdir}")))
  cli::cli_progress_step(
    msg = glue("Getting {data_type} data filepaths from input directory..."),
    msg_done = "Got {n_input_files} {data_type} data filepaths from input directory."
  )
  input_files <- list.files(path = data_subdir,
                            pattern = "*.iqdat",
                            full.names = TRUE)
  
  if (rlang::is_empty(input_files)) {
    cli::cli_abort(c(
      "{.var input_files} is empty",
      "x" = str_c("No input .iqdat files found at: ", data_subdir)
    ))
  } else {
    n_input_files <- length(input_files)
    return(input_files)
  }
}

## Then, loop through these files.
## For each file: load, recode, clean, and save.
load_raw <- function(input_path, data_type) {
  if (is.na(input_path)) {
    cli::cli_alert_danger("Input path is empty (NA)")
    cli::cli_abort(c(
      "{.var input_path} is NA",
      "x" = str_c("No valid .iqdat file path was provided")
    ))
  } else {
    if (data_type == "task") {
      subject_id <- "?"
      cli::cli_alert_info(glue("Loading input file: {input_path}"))
      data_raw <- readr::read_tsv(
        input_path,
        col_types = cols(
          build = col_character(),
          computer.platform = col_character(),
          date = col_date(format = ""),
          time = col_time(format = ""),
          timestamp = col_time(format = ""),
          time_elapsed_ms = col_double(),
          subject = col_double(),
          group = col_double(),
          session = col_double(),
          blockcode = col_character(),
          blocknum = col_double(),
          trialcode = col_character(),
          trialnum = col_double(),
          stimulus_left = col_character(),
          stimulus_right = col_character(),
          stim_index = col_double(),
          target_present = col_character(),
          response = col_character(),
          correct = col_double(),
          latency = col_double()
        )
      )
      subject_id <- data_raw$subject %>% first() %>% as.character()
      ## Status message is down here so it can display subject ID on finish
      cli::cli_progress_step(
        msg = glue("Loading input file..."),
        msg_done = glue("Loaded input file (subject {subject_id}).")
      )
      
    }
    
    return(data_raw)
  }
}

data_tests <- function(data_raw, data_type) {
  # cli::cli_alert_warning("No data quality tests have been written")
  if (data_type == "task") {
    return()
  }
  #### Tests for raw task data file
  #TODO
  #message("Testing that data file contains what we expect...")
  
  ## Confirm:
  ## When "target present", exactly one of the two stimuli contains "target"
  
  #message("Testing: When 'target present', exactly one of the two stimuli #contains 'target'")
  #warning("Failed")
  #message("Passed")
  
  
  ## When "target absent", both stimuli contain "distractor"
  
  ## When "target present" and response is nonzero, marked "correct"
  ## When "target present" and response is zero, marked "incorrect"
  ## When "target absent" and response is nonzero, marked "incorrect"
  ## When "target absent" and response is zero, marked "correct"
  
  #### Test for demographics
  #TODO
  
  #### Tests for EHI
  #TODO
  
  #### Tests for end questions
  #TODO
  #message("No data integrity tests")
}

recode_raw <- function(data_raw, data_type) {
  cli::cli_progress_step(msg = glue("Recoding data..."),
                         msg_done = glue("Recoded data."))
  
  if (data_type == "task") {
    ## Recode responses
    data_proc <- data_raw %>%
      rename(response_raw = response)
    
    data_proc <- data_proc %>% mutate(
      response_chr = case_when(
        response_raw == 0 ~ "",
        response_raw == 44 ~ "z",
        response_raw == 53 ~ "slash"
      ),
      response = case_when(
        response_raw == 0 ~ "absent",
        response_raw %in% c(44, 53) ~ "present"
      ),
      response_log = case_when(response == "absent" ~ 0,
                               response == "present" ~ 1)
    )
    
    ## Recode reaction time
    data_proc <- data_proc %>% rename(rt = latency)
  }
  return(data_proc)
}

clean_recoded <- function(data_recoded, data_type) {
  cli::cli_progress_step(msg = glue("Cleaning data..."),
                         msg_done = glue("Cleaned data."))
  
  if (data_type == "task") {
    ## Remove unused columns
    data_cleaned <- data_recoded %>%
      select(
        subject,
        time_elapsed_ms,
        blocknum,
        trialcode,
        trialnum,
        target_present,
        response,
        response_log,
        correct,
        rt
      ) %>%
      ## Remove non-trial rows (leaving only experiment rows)
      filter(trialcode %in% c("practice_slash", "practice_z", "main_slash", "main_z"))
    
    ## Recode blocks
    data_cleaned <- data_cleaned %>%
      rename(block = trialcode) %>%
      ## Recode trialnum (because every other trial was an
      ## "advance" trial, which we don't care about)
      mutate(trialnum = (trialnum / 2) + .5) %>%
      ## Recode blocknum (because instructions count as blocks)
      mutate(blocknum = (blocknum / 2) - 1) %>%
      mutate(subject = as.character(subject))
    
    ## Add field for block type (main or practice)
    data_cleaned <- data_cleaned %>% mutate(block_type = case_when(
      str_detect(block, "main") ~ "main",
      str_detect(block, "practice") ~ "practice"
    ))
    
  }
  return(data_cleaned)
}

save_cleaned <- function(data_cleaned, proc_dir, data_type) {
  subject_id <- data_cleaned$subject %>% first() %>% as.character()
  cli::cli_progress_step(
    msg = glue("Saving processed data..."),
    msg_done = glue("Saved processed data (subject {subject_id}).")
  )
  if (data_type == "task") {
    file_name <- str_c(subject_id, "_task.csv")
    save_path <- here::here(proc_dir, "individual", "task", file_name)
  }
  cli::cli_alert_info(glue("Saving processed {data_type} data to:"))
  cli::cli_bullets(c(" " = glue("{save_path}")))
  write_csv(data_cleaned, save_path)
}