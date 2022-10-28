library(readr)
requireNamespace("here")
requireNamespace("cli")

source(here::here("lib", "util.R"))
source(here::here("lib", "load_process", "code_target_level_field.R"))
source(here::here("lib", "load_process", "data_tests.R"))

####***********************************************************************####
#### PROCESS INDIVIDUAL-LEVEL DATA
## Given an input folder, load and process all task data (main loop)
load_and_process <- function(input_dir, proc_dir, data_type) {
  input_files <- get_input_paths(input_dir, data_type)
  n_input_files <- length(input_files)
  for (j in (c(1:n_input_files))) {
    cli::cli_text("{.strong {j}/{n_input_files} ({data_type} data)...}")
    input_path <- input_files[j]
    data_raw <- load_raw(input_path, data_type)
    data_recoded <- recode_raw(data_raw, data_type)
    data_cleaned <- clean_recoded(data_recoded, data_type)
    data_tests(data_cleaned, data_type)
    save_cleaned(data_cleaned, proc_dir, data_type)
  }
  cli::cli_progress_step(msg = glue("Loading input file {j}/{n_input_files}..."),
                         msg_done = "Loaded {j}/{n_input_files} {data_type} data files from input directory.")
}

## First, get a list of all input files.
## (All files in the data type's input directory with extension .iqdat)
get_input_paths <- function(input_dir, data_type, pattern = "*.iqdat") {
    if (data_type == "task") {
      data_subdir <- here(input_dir, "task")
    } else if (data_type == "ehi") {
      data_subdir <- here(input_dir, "survey", "ehi_short")
    }
    cli::cli_alert_info(glue("Getting {data_type} data filepaths from input directory:"))
    cli::cli_bullets(c(" " = glue("{data_subdir}")))
    cli::cli_progress_step(
      msg = glue("Getting {data_type} data filepaths from input directory..."),
      msg_done = "Got {n_input_files} {data_type} data filepaths from input directory."
    )
    input_files <- list.files(path = data_subdir,
                              pattern = pattern,
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
    subject_id <- "?"
    cli::cli_alert_info(glue("Loading input file: {input_path}"))
    
    if (data_type == "task") {
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
      
    } else if (data_type == "ehi") {
      data_raw <- readr::read_tsv(
        input_path,
        col_types = cols(
          date = col_date(format = ""),
          time = col_time(format = ""),
          group = col_double(),
          subject = col_character(),
          session = col_double(),
          build = col_character(),
          ehi_i1_writing_response = col_character(),
          ehi_i1_writing_latency = col_double(),
          ehi_i2_throwing_response = col_character(),
          ehi_i2_throwing_latency = col_double(),
          ehi_i3_toothbrush_response = col_character(),
          ehi_i3_toothbrush_latency = col_double(),
          ehi_i4_spoon_response = col_character(),
          ehi_i4_spoon_latency = col_double(),
        ),
      )
    }
    
    subject_id <-
      data_raw$subject %>% first() %>% as.character()
    ## Status message is down here so it can display subject ID on finish
    cli::cli_progress_step(
      msg = glue("Loading input file..."),
      msg_done = glue("Loaded input file (subject {subject_id}).")
    )
    return(data_raw)
  }
}

recode_raw <- function(data_raw, data_type) {
  cli::cli_progress_step(msg = glue("Recoding data..."),
                         msg_done = glue("Recoded data."))
  
  if (data_type == "task") {
    ## TODO: rename output of this function "data_recoded", instead of "data_proc"
    ## Code target, level, field
    data_proc <- data_raw %>% code_target_level_field()
    
    ## Recode responses
    data_proc <- data_proc %>%
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
      )
    )
    
    
    
    ## Recode reaction time
    data_proc <- data_proc %>% rename(rt = latency)
    
  } else if (data_type == "ehi") {
    data_proc <- data_raw %>%
      select(-ends_with("latency"),
             -date,
             -time,
             -group,
             -session,
             -build) %>%
      rename_with(trim_end, ends_with("response")) %>%
      mutate(across(
        starts_with("ehi"),
        ~ recode(
          .,
          `Always right` = 25,
          `Usually right` = 12.5,
          `Both equally` = 0,
          `Usually left` = -12.5,
          `Always left` = -25,
        )
      ))
  }
  
  return(data_proc)
}

clean_recoded <- function(data_recoded, data_type) {
  cli::cli_progress_step(msg = glue("Cleaning data..."),
                         msg_done = glue("Cleaned data."))
  
  if (data_type == "task") {
    ## Remove unused columns
    data_cleaned <- data_recoded %>%

      ## Make subject a string, instead of  a number
      mutate(subject = as.character(subject)) %>%
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
      
      ## Add field for block type (main or practice)
      mutate(block_type = case_when(
        str_detect(block, "main") ~ "main",
        str_detect(block, "practice") ~ "practice"
      )) %>%
      
      ## Add field for block response (z or slash)
      mutate(
        block_response = recode(
          block,
          main_z = "z",
          main_slash = "slash",
          practice_z = "z",
          practice_slash = "slash"
        )
      ) %>% 
      select(
        subject,
        time_elapsed_ms,
        blocknum,
        block_type,
        block_response,
        trialnum,
        target,
        level,
        field,
        target_present,
        response,
        correct,
        rt
      )
    
  } else if (data_type == "ehi") {
    data_cleaned <- data_recoded %>%
      mutate(ehi_total = sum(across(starts_with("ehi"))))
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
    file_name <- str_c(subject_id, "_task.tsv")
    save_path <- here::here(proc_dir, "individual",
                            "task", file_name)
  } else if (data_type == "ehi") {
    file_name <- str_c(subject_id, "_ehi.tsv")
    save_path <- here::here(proc_dir, "individual",
                            "survey", "ehi_short", file_name)
  }
  cli::cli_alert_info(glue("Saving processed {data_type} data to:"))
  cli::cli_bullets(c(" " = glue("{save_path}")))
  write_tsv(data_cleaned, save_path)
}


####***********************************************************************####
#### LOAD AND SUMMARIZE PROCESSED INDIVIDUAL DATA
## Load and summarize an individual's processed data (main loop)
load_and_summarize_proc <- function(input_dir, output_dir, data_type) {
  summary_proc <- tibble(subject = as.character(),
                         acc_slash = as.numeric(),
                         acc_z = as.numeric())
  input_files <- get_input_paths(input_dir, data_type = "task",
                                 pattern = "*.tsv")
  
  n_input_files <- length(input_files)
  for (j in (c(1:n_input_files))) {
    cli::cli_text("{.strong {j}/{n_input_files} ({data_type} data)...}")
    input_path <- input_files[j]
    input_path
    data_proc <- load_proc(input_path, data_type = "task")
    ind_summary <- summarize_ind(data_proc)
    summary_proc <- summary_proc %>% add_row(ind_summary)
  }
  summary_proc
  file_name <- str_c("summary.tsv")
  save_path <- here::here(output_dir, file_name)
  cli::cli_alert_info(glue("Saving summary {data_type} data to:"))
  cli::cli_bullets(c(" " = glue("{save_path}")))
  write_tsv(summary_proc, save_path)
}

## Load an individual's processed data
load_proc <- function(input_path, data_type) {
  if (is.na(input_path)) {
    cli::cli_alert_danger("Input path is empty (NA)")
    cli::cli_abort(c(
      "{.var input_path} is NA",
      "x" = str_c("No valid .iqdat file path was provided")
    ))
  } else {
    subject_id <- "?"
    cli::cli_alert_info(glue("Loading input file: {input_path}"))
    
    if (data_type == "task") {
      data_raw <- readr::read_tsv(
        input_path,
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
      )
      
    } else if (data_type == "ehi") {
      data_raw <- readr::read_tsv(input_path)
    }
    
    subject_id <-
      data_raw$subject %>% first() %>% as.character()
    ## Status message is down here so it can display subject ID on finish
    cli::cli_progress_step(
      msg = glue("Loading input file..."),
      msg_done = glue("Loaded input file (subject {subject_id}).")
    )
    return(data_raw)
  }
}

## Summarize an individual's processed data
summarize_ind <- function(data_proc, data_type = "task") {
  ## Calculate percent correct for each block,
  ## separating present and absent trials
  data_proc <- data_proc %>% 
      mutate(response_log = case_when(response == "absent" ~ 0,
                               response == "present" ~ 1)
      )
  
  response_counts_block_pa <- data_proc %>%
    group_by(block_response, block_type, target_present, subject) %>%
    summarize(
      total_responses = n(),
      n_present_resp = sum(response_log),
      n_absent_resp = total_responses - n_present_resp,
      n_correct = sum(correct),
      percent_correct = 100 * (n_correct / total_responses)
    )
  
  ## Calculate percent correct for each block,
  ## collapsing present and absent trials
  response_counts_by_block <- data_proc %>%
    group_by(block_response, block_type, subject) %>%
    summarize(
      total_responses = n(),
      n_present_resp = sum(response_log),
      n_absent_resp = total_responses - n_present_resp,
      n_correct = sum(correct),
      percent_correct = 100 * (n_correct / total_responses)
    )
  
  proc_summary <- response_counts_by_block %>%
    ungroup() %>%
    filter(block_type == "main") %>%
    select(subject, block_response, percent_correct) %>%
    pivot_wider(
      names_from = block_response,
      names_prefix = "acc_",
      values_from = percent_correct
    )
  
  return(proc_summary)
  
  ## TODO. calculate 
  ## (1) median rt by condition (level x field), for target-present trials
  ##     to do this, need to code global and local target present!!
  ##     this should be calculated for the processed, 
  ##     individual-level data.
  ## (1) accuracy by condition (level x field), for target-present trials
  ## (1) accuracy by condition (level x field), for target-absent trials
  ## (1) accuracy by condition (level x field), for all trials
  ## (1) which block came first (add a column "first_block" (z, slash))
  #data_proc
}