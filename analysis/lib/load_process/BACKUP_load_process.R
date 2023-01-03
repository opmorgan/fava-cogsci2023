library(readr)
requireNamespace("here")
requireNamespace("cli")

source(here::here("lib", "util.R"))
source(here::here("lib", "load_process", "code_target_level_field.R"))
source(here::here("lib", "load_process", "data_tests.R"))

####***********************************************************************####
#### LOAD AND PROCESS RAW INDIVIDUAL-LEVEL DATA
#### (1_process.Rmd)
## Given an input folder, load and process all task data (main loop)
process_raw <- function(input_dir, proc_dir, data_type) {
  input_files <- get_input_paths(input_dir, data_type)
  n_input_files <- length(input_files)
  for (j in (c(1:n_input_files))) {
    cli::cli_text("{.strong {j}/{n_input_files} (Processing {data_type} data)...}")
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
get_input_paths <-
  function(input_dir, data_type, pattern = "*.iqdat") {
    if (data_type == "task") {
      data_subdir <- here(input_dir, "task")
    } else if (data_type == "ehi") {
      data_subdir <- here(input_dir, "survey", "ehi_short")
    } else if (data_type == "demographics") {
      data_subdir <- here(input_dir, "survey", "demographics")
    } else if (data_type == "end") {
      data_subdir <- here(input_dir, "survey", "end_questions")
    }
    
    n_input_files <- "?"
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
          subject = col_character(),
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
    } else if (data_type == "demographics") {
      data_raw <- readr::read_tsv(
        input_path,
        col_types = cols(
          date = col_date(format = ""),
          time = col_time(format = ""),
          group = col_double(),
          subject = col_character(),
          session = col_double(),
          build = col_character(),
          age_response = col_double(),
          age_latency = col_double(),
          country_response = col_character(),
          country_latency = col_double(),
          sex_response = col_character(),
          sex_latency = col_double(),
          sex_other_response = col_character(),
          sex_other_latency = col_double(),
          education_response = col_character(),
          education_latency = col_double(),
          educationother_response = col_character(),
          educationother_latency = col_double(),
          raceoption1_response = col_character(),
          raceoption1_latency = col_double(),
          raceoption2_response = col_character(),
          raceoption2_latency = col_double(),
          raceoption3_response = col_character(),
          raceoption3_latency = col_double(),
          raceoption4_response = col_character(),
          raceoption4_latency = col_double(),
          raceoption5_response = col_character(),
          raceoption5_latency = col_double(),
          raceoption6_response = col_character(),
          raceoption6_latency = col_double(),
          raceother_response = col_character(),
          raceother_latency = col_double(),
          race_other_response = col_character(),
          race_other_latency = col_double(),
          ethnicity_response = col_character(),
          ethnicity_latency = col_double()
        )
      )
    } else if (data_type == "end") {
      data_raw <- readr::read_tsv(
        input_path,
        col_types = cols(
          date = col_date(format = ""),
          time = col_time(format = ""),
          group = col_double(),
          subject = col_character(),
          session = col_double(),
          build = col_character(),
          task_experience_response = col_character(),
          task_experience_latency = col_double(),
          task_experience_other_response = col_character(),
          task_experience_other_latency = col_double(),
          open_ended_feedback_response = col_character(),
          open_ended_feedback_latency = col_double()
        )
      )
    }
    
    subject_id <-
      data_raw$subject |> first() |> as.character()
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
    data_proc <- data_raw |> code_target_level_field()
    
    ## Recode responses
    data_proc <- data_proc |>
      rename(response_raw = response)
    
    data_proc <- data_proc |> mutate(
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
    
    
    
    ## Rename reaction time ("latency" -> "rt")
    data_proc <- data_proc |> rename(rt = latency)
    
  } else if (data_type == "ehi") {
    data_proc <- data_raw |>
      select(-ends_with("latency"),
             -date,
             -time,
             -group,-session,-build) |>
      rename_with(trim_end, ends_with("response")) |>
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
  } else if (data_type == "demographics") {
    data_proc <- data_raw |>
      ## remove all latency measures, and trim columns
      select(-ends_with("latency"),-date,-time,-group,-session,-build) |>
      ## remove "response" suffix
      rename_with(trim_end, ends_with("response"))
    
    #### Recode sex so that "not listed" shows full response.
    data_proc <- data_proc |> mutate(
      sex = case_when(
        sex == "Male" ~ "Male",
        sex == "Female" ~ "Female",
        sex == "Not listed:" ~ str_c("Not listed: ", sex_other),
        TRUE ~ sex
      )
    )
    
    
    #### Recode race
    ## Put all race responses in a vector
    race_answers <-
      data_proc |> select(starts_with("race")) |> as.character()
    ## If there is only one, code race as that race
    ## If there is more than one, code as "multiple races"
    answer_indices <- which(race_answers != "NA")
    if (length(answer_indices) == 1) {
      race_recoded <- race_answers[[answer_indices]]
    } else if (length(answer_indices > 1)) {
      race_recoded <- "Multiple"
    }
    
    data_proc <- data_proc |>
      select(-starts_with("race")) |>
      mutate(race = race_recoded) |>
      select(subject, age, country, sex, education, race, ethnicity) |>
      rename(hispanic_ethnicity = ethnicity) |>
      mutate(race = case_when(
        race == "Some other race (please describe)" ~ "Other",
        TRUE ~ race
      ))
    
    #### Recode education
    ## Code education as number of years (keep as character type)
    data_proc <- data_proc |>
      mutate(
        education = case_when(
          education == "Doctoral or professional degree (~21+ years of education)" ~ "21",
          education == "Master's degree (~18 years)" ~ "18",
          education == "Bachelor's degree (~16 years)" ~ "16",
          education == "Associate's degree (~14 years)" ~ "14",
          education == "Postsecondary non-degree award (~14 years)" ~ "14",
          education == "Some college, no degree (~12-16 years)" ~ "12",
          education == "High school diploma or equivalent (~12 years)" ~ "12",
          education == "Some high school, no degree (~8-12 years)" ~ "8",
          education == "Elementary or Middle school (~5-8 years)" ~ "5",
          education == "Less than Elementary or Middle school (<5 years)" ~ "0",
          education == "Something else (please specify):" ~ "Other",
          TRUE ~ "NA"
        )
      )
    
  } else if (data_type == "end") {
    data_proc <- data_raw
  }
  
  return(data_proc)
}

clean_recoded <- function(data_recoded, data_type) {
  cli::cli_progress_step(msg = glue("Cleaning data..."),
                         msg_done = glue("Cleaned data."))
  
  if (data_type == "task") {
    ## Remove unused columns
    data_cleaned <- data_recoded |>
      
      ## Make subject a string, instead of  a number
      mutate(subject = as.character(subject)) |>
      ## Remove non-trial rows (leaving only experiment rows)
      filter(trialcode %in% c("practice_slash", "practice_z", "main_slash", "main_z"))
    
    ## Recode blocks
    data_cleaned <- data_cleaned |>
      rename(block = trialcode) |>
      ## Recode trialnum (because every other trial was an
      ## "advance" trial, which we don't care about)
      mutate(trialnum = (trialnum / 2) + .5) |>
      ## Recode blocknum (because instructions count as blocks)
      mutate(blocknum = (blocknum / 2) - 1.5) |>
      
      ## Add field for block type (main or practice)
      mutate(block_type = case_when(
        str_detect(block, "main") ~ "main",
        str_detect(block, "practice") ~ "practice"
      )) |>
      
      ## Add field for block response (z or slash)
      mutate(
        block_response = recode(
          block,
          main_z = "z",
          main_slash = "slash",
          practice_z = "z",
          practice_slash = "slash"
        )
      ) |>
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
    data_cleaned <- data_recoded |>
      mutate(ehi_total = sum(across(starts_with("ehi")))) |>
      select(subject,
             starts_with("ehi"))
  } else if (data_type == "demographics") {
    data_cleaned <- data_recoded
  } else if (data_type == "end") {
    data_cleaned <-
      data_recoded |> select(
        subject,
        task_experience_response,
        task_experience_other_response,
        open_ended_feedback_response
      )
  }
  
  return(data_cleaned)
}

save_cleaned <- function(data_cleaned, proc_dir, data_type) {
  subject_id <- data_cleaned$subject |> first() |> as.character()
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
  } else if (data_type == "demographics") {
    file_name <- str_c(subject_id, "_demographics.tsv")
    save_path <- here::here(proc_dir,
                            "individual",
                            "survey",
                            "demographics",
                            file_name)
  } else if (data_type == "end") {
    file_name <- str_c(subject_id, "_end.tsv")
    save_path <- here::here(proc_dir,
                            "individual",
                            "survey",
                            "end_questions",
                            file_name)
  }
  
  cli::cli_alert_info(glue("Saving processed {data_type} data to:"))
  cli::cli_bullets(c(" " = glue("{save_path}")))
  write_tsv(data_cleaned, save_path)
}


####***********************************************************************####
#### LOAD AND SUMMARIZE PROCESSED INDIVIDUAL DATA
#### (1_process.Rmd)
## Load and summarize an individual's processed data (wide) (main loop)
load_and_summarize_proc <-
  function(input_dir, output_dir, data_type) {
    if (data_type == "task") {
      group_summary <- tibble(
        subject = as.character(),
        first_block = as.character(),
        acc_slash = as.numeric(),
        acc_z = as.numeric(),
        acc_absent = as.numeric(),
        acc_present = as.numeric(),
        acc_global_LVF = as.numeric(),
        acc_global_RVF = as.numeric(),
        acc_local_LVF = as.numeric(),
        acc_local_RVF = as.numeric(),
        rt_global_LVF = as.numeric(),
        rt_global_RVF = as.numeric(),
        rt_local_LVF = as.numeric(),
        rt_local_RVF = as.numeric(),
        rt_overall = as.numeric(),
        duration_s = as.numeric(),
        exclude_many_gos = as.logical(),
        exclude_low_acc = as.logical(),
        exclude_low_rt = as.logical(),
        exclude_high_rt = as.logical(),
        exclude_long_duration = as.logical(),
        exclude = as.logical()
      )
    } else if (data_type == "ehi") {
      group_summary <- tibble(
        subject = as.character(),
        ehi_i1_writing = as.numeric(),
        ehi_i2_throwing = as.numeric(),
        ehi_i3_toothbrush = as.numeric(),
        ehi_i4_spoon = as.numeric(),
        ehi_total = as.numeric()
      )
    } else if (data_type == "demographics") {
      group_summary <- tibble(
        subject = as.character(),
        age = as.numeric(),
        country = as.character(),
        sex = as.character(),
        education = as.character(),
        race = as.character(),
        hispanic_ethnicity = as.character()
      )
    } else if (data_type == "end") {
      group_summary <- tibble(
        subject = as.character(),
        task_experience_response = as.character(),
        task_experience_other_response = as.character(),
        open_ended_feedback_response = as.character()
      )
    }
    
    input_files <- get_input_paths(input_dir, data_type = data_type,
                                   pattern = "*.tsv")
    n_input_files <- length(input_files)
    for (j in (c(1:n_input_files))) {
      cli::cli_text("{.strong {j}/{n_input_files} (Summarizing {data_type} data)...}")
      input_path <- input_files[j]
      input_path
      ind_proc <- load_proc(input_path, data_type = data_type)
      ind_summary <- summarize_ind(ind_proc, data_type = data_type)
      group_summary <- group_summary |> add_row(ind_summary)
    }
    file_name <- str_c("summary_", data_type, ".tsv")
    save_path <- here::here(output_dir, file_name)
    cli::cli_alert_info(glue("Saving summary {data_type} data to:"))
    cli::cli_bullets(c(" " = glue("{save_path}")))
    write_tsv(group_summary, save_path)
    return(group_summary)
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
      data_raw <- readr::read_tsv(
        input_path,
        col_types =
          cols(
            subject = col_character(),
            ehi_i1_writing = col_double(),
            ehi_i2_throwing = col_double(),
            ehi_i3_toothbrush = col_double(),
            ehi_i4_spoon = col_double(),
            ehi_total = col_double()
          )
      )
    } else if (data_type == "demographics") {
      data_raw <- readr::read_tsv(
        input_path,
        col_types =
          cols(
            subject = col_character(),
            age = col_double(),
            country = col_character(),
            sex = col_character(),
            education = col_character(),
            race = col_character(),
            hispanic_ethnicity = col_character()
          )
      )
    } else if (data_type == "end") {
      data_raw <- readr::read_tsv(
        input_path,
        col_types =
          cols(
            subject = col_character(),
            task_experience_response = col_character(),
            task_experience_other_response = col_character(),
            open_ended_feedback_response = col_character()
          )
      )
    }
    
    subject_id <-
      data_raw$subject |> first() |> as.character()
    ## Status message is down here so it can display subject ID on finish
    cli::cli_progress_step(
      msg = glue("Loading input file..."),
      msg_done = glue("Loaded input file (Subject {subject_id}).")
    )
    return(data_raw)
  }
}

## Summarize an individual's processed data, and record any exclusions
summarize_ind <- function(ind_proc, data_type = "task") {
  subject_id <-
    ind_proc$subject |> first() |> as.character()
  cli::cli_progress_step(
    msg = glue("Summarizing individual data..."),
    msg_done = glue("Summarized individual data (Subject {subject_id}).")
  )
  
  if (data_type == "task") {
    #### Accuracy
    ## Calculate percent correct for each block,
    ## separating present and absent trials
    ind_proc <- ind_proc |>
      mutate(response_log = case_when(response == "absent" ~ 0,
                                      response == "present" ~ 1)) |>
      filter(block_type == "main")
    
    response_counts_pa <- ind_proc |>
      group_by(target_present, subject) |>
      summarize(
        total_responses = n(),
        n_present_resp = sum(response_log),
        n_absent_resp = total_responses - n_present_resp,
        n_correct = sum(correct),
        percent_correct = 100 * (n_correct / total_responses)
      )
    
    acc_pa <- response_counts_pa |>
      ungroup() |>
      mutate(target_present = target_present |> recode("no" = "absent", yes = "present")) |>
      select(subject, target_present, percent_correct) |>
      pivot_wider(
        names_from = c(target_present),
        names_prefix = "acc_",
        values_from = percent_correct
      )
    
    ind_summary <- acc_pa
    
    ## Calculate percent correct for each block,
    ## collapsing present and absent trials
    response_counts_block <- ind_proc |>
      group_by(block_response, subject) |>
      summarize(
        total_responses = n(),
        n_present_resp = sum(response_log),
        n_absent_resp = total_responses - n_present_resp,
        n_correct = sum(correct),
        percent_correct = 100 * (n_correct / total_responses)
      )
    
    acc_all <- response_counts_block |>
      ungroup() |>
      select(subject, block_response, percent_correct) |>
      pivot_wider(
        names_from = block_response,
        names_prefix = "acc_",
        values_from = percent_correct
      )
    
    ind_summary <- left_join(acc_all, ind_summary)
    
    first_block_var <- ind_proc |>
      slice(1) |>
      (\(.) .[["block_response"]])()
    
    ## Record which block was first (z or slash)
    ind_summary <- ind_summary |>
      mutate(first_block = first_block_var) |>
      select(subject, first_block, everything())
    
    ## Calculate accuracy by condition (level x field)
    response_counts_condition <- ind_proc |>
      filter(target_present == "yes") |>
      group_by(level, field, subject) |>
      summarize(
        total_responses = n(),
        n_present_resp = sum(response_log),
        n_absent_resp = total_responses - n_present_resp,
        n_correct = sum(correct),
        percent_correct = 100 * (n_correct / total_responses)
      )
    
    acc_condition <- response_counts_condition |>
      ungroup() |>
      select(subject, level, field, percent_correct) |>
      pivot_wider(
        names_from = c(level, field),
        names_prefix = "acc_",
        values_from = percent_correct
      )
    
    ind_summary <- left_join(ind_summary, acc_condition)
    
    #### Reaction time
    ## Calculate median RT by condition
    rt_condition <- ind_proc |>
      filter(target_present == "yes") |>
      group_by(level, field, subject) |>
      summarize(rt = median(rt)) |>
      pivot_wider(
        names_from = c(level, field),
        names_prefix = "rt_",
        values_from = rt
      )
    
    ind_summary <- left_join(ind_summary, rt_condition)
    
    ## Calculate overall median RT
    rt_overall <- ind_proc |>
      filter(target_present == "yes") |>
      group_by(subject) |>
      summarize(rt_overall = median(rt))
    
    ind_summary <- left_join(ind_summary, rt_overall)
    
    ## Calculate overall duration
    ind_summary <- ind_summary |>
      mutate(duration_s = last(ind_proc$time_elapsed_ms) / 1000)
    
    #### Exclusions
    ## Responded "go" almost every time?
    too_many_gos <- 0
    for (block_response_var in response_counts_block$block_response) {
      n_gos <- response_counts_block |>
        filter(block_response == block_response_var) |>
        (\(.) .[["n_present_resp"]])()
      if (n_gos >= 78) {
        too_many_gos <- 1
      }
    }
    ind_summary <- ind_summary |>
      mutate(exclude_many_gos = too_many_gos)
    
    ## Accuracy at 60% or lower on any main block?
    low_acc <- 0
    if ((acc_all$acc_slash < 60) | (acc_all$acc_z < 60)) {
      low_acc <- 1
    }
    ind_summary <- ind_summary |>
      mutate(exclude_low_acc = low_acc)
    
    ## Median RT over 1500 or under 200?
    low_rt <- 0
    high_rt <- 0
    if (ind_summary$rt_overall < 200) {
      low_rt <- 1
    }
    if (ind_summary$rt_overall > 1500) {
      high_rt <- 1
    }
    ind_summary <- ind_summary |>
      mutate(exclude_low_rt = low_rt,
             exclude_high_rt = high_rt)
    
    
    ## Took longer than 45 minutes to do the task?
    ## (45*60 seconds = 2700 seconds)
    long_duration <- 0
    if (ind_summary$duration_s > 2700) {
      long_duration <- 1
    }
    ind_summary <- ind_summary |>
      mutate(exclude_long_duration = long_duration)
    
    
    ind_summary <- ind_summary |>
      mutate(
        exclude = case_when(
          exclude_many_gos == 1 ~ 1,
          exclude_low_acc == 1 ~ 1,
          exclude_low_rt == 1 ~ 1,
          exclude_high_rt == 1 ~ 1,
          TRUE ~ 0,
        )
      )
  } else if (data_type == "ehi") {
    ind_summary <- ind_proc
  } else if (data_type == "demographics") {
    ind_summary <- ind_proc
  } else if (data_type == "end") {
    ind_summary <- ind_proc
  }
  
  return(ind_summary)
  
}

## Load and combine individuals' processed data (long)
load_and_combine_proc <-
  function(input_dir, output_dir, data_type = "task") {
    if (data_type == "task") {
      group_proc <- tibble(
        subject = as.character(),
        time_elapsed_ms = as.numeric(),
        blocknum = as.numeric(),
        block_type = as.character(),
        block_response = as.character(),
        trialnum = as.numeric(),
        target = as.character(),
        level = as.character(),
        field = as.character(),
        target_present = as.character(),
        response = as.character(),
        correct = as.numeric(),
        rt = as.numeric(),
      )
    } else if (data_type == "ehi") {
      group_proc <- tibble(
        subject = as.character(),
        ehi_i1_writing = as.numeric(),
        ehi_i2_throwing = as.numeric(),
        ehi_i3_toothbrush = as.numeric(),
        ehi_i4_spoon = as.numeric(),
        ehi_total = as.numeric()
      )
    } else if (data_type == "demographics") {
      group_proc <- tibble(
        subject = as.character(),
        age = as.numeric(),
        country = as.character(),
        sex = as.character(),
        education = as.character(),
        race = as.character(),
        hispanic_ethnicity = as.character()
      )
    } else if (data_type == "end") {
      group_proc <- tibble(
        subject = as.character(),
        task_experience_response = as.character(),
        task_experience_other_response = as.character(),
        open_ended_feedback_response = as.character()
      )
    }
    
    input_files <- get_input_paths(input_dir = input_dir,
                                   data_type = data_type,
                                   pattern = "*.tsv")
    
    n_input_files <- length(input_files)
    for (j in (c(1:n_input_files))) {
      cli::cli_text("{.strong {j}/{n_input_files} (Loading processed {data_type} data)...}")
      input_path <-  input_files[j]
      ind_proc <- load_proc(input_path, data_type)
      group_proc <- group_proc |> add_row(ind_proc)
    }
    
    file_name <- str_c("combined_", data_type, ".tsv")
    save_path <- here::here(output_dir, file_name)
    cli::cli_alert_info("{.strong Saving combined {data_type} data to:}")
    cli::cli_bullets(c(" " = glue("{save_path}")))
    write_tsv(group_proc, save_path)
    return(group_proc)
  }


####***********************************************************************####
#### LOAD SUMMARIZED GROUP DATA (PRE-EXCLUSION CALCULATIONS)
#### (1_process.Rmd, 2_exclude.Rmd)
load_summary <- function(proc_dir) {
  input_path = here(proc_dir, "summary", "summary.tsv")
  
  cli::cli_alert_info("Loading summary data (pre-exclusion calculation) from:")
  cli::cli_bullets(c(" " = "{input_path}"))
  
  summary <- readr::read_tsv(
    input_path,
    col_types = cols(
      subject = col_character(),
      first_block = col_character(),
      acc_slash = col_double(),
      acc_z = col_double(),
      acc_absent = col_double(),
      acc_present = col_double(),
      acc_global_LVF = col_double(),
      acc_global_RVF = col_double(),
      acc_local_LVF = col_double(),
      acc_local_RVF = col_double(),
      rt_global_LVF = col_double(),
      rt_global_RVF = col_double(),
      rt_local_LVF = col_double(),
      rt_local_RVF = col_double(),
      rt_overall = col_double(),
      duration_s = col_double(),
      exclude_many_gos = col_double(),
      exclude_low_acc = col_double(),
      exclude_low_rt = col_double(),
      exclude_high_rt = col_double(),
      exclude = col_double(),
      ehi_i1_writing = col_double(),
      ehi_i2_throwing = col_double(),
      ehi_i3_toothbrush = col_double(),
      ehi_i4_spoon = col_double(),
      ehi_total = col_double(),
      age = col_double(),
      country = col_character(),
      sex = col_character(),
      education = col_double(),
      race = col_character(),
      hispanic_ethnicity = col_character(),
      task_experience_response = col_character(),
      task_experience_other_response = col_character(),
      open_ended_feedback_response = col_character()
    )
  )
  return(summary)
}


####***********************************************************************####
#### LOAD DATA NEEDED TO CALCULATE EXCLUSIONS
#### (2_exclude.Rmd)
####
#### LOAD PROCESSED INDIVIDUAL TASK DATA (PRE-EXCLUSION CALCULATIONS)
#### & SUBSET COLUMNS TO THOSE NEEDED FOR ANALYSIS
#### [old] & SUBSET TO "main", "present"
#### [old] (The relevant data for behavioral exclusions; RT/accuracy analyses)
#### (2_exclude.Rmd)
load_task_all <- function(proc_dir) {
  input_path <- here(proc_dir, "combined", "combined_task.tsv")
  
  cli::cli_alert_info("Loading task data (pre-exclusion calculation) from:")
  cli::cli_bullets(c(" " = "{input_path}"))
  
  aah_task_all <- readr::read_tsv(
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
  ) |>
    select(
      subject,
      block_type,
      block_response,
      target_present,
      target,
      level,
      field,
      correct,
      rt
    )
  return(aah_task_all)
}

#### LOAD SUMMARIZED GROUP DATA (PRE-EXCLUSION CALCULATIONS)
#### & SUBSET to columns used to calculate exclusions
#### Recode "ehi_total" to "ehi" (for analysis)
#### (2_exclude.Rmd)
## Depends on: lib/load_process/load_process.R/load_summary()
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

####***********************************************************************####
#### LOAD TASK DATA (POST-EXCLUSION CALCULATION) FOR ANALYSIS
#### (3_analysis.Rmd, ...)
#### aah_long.tsv is "the data": all trials (including practice & absent),
#### all subjects, and all exclusion information.
#### This data must be filtered to "main" blocks, "present" trials,
#### and included subjects (exclude == 0) for analyses.
load_aah_long <- function(proc_dir) {
  input_path <- here(proc_dir, "aah_long.tsv")
  
  cli::cli_alert_info("Loading long task data (all subjects  and all trials, including practice and target absent) from:")
  cli::cli_bullets(c(" " = "{input_path}", " " = "-> aah_long"))
  
  aah_long <- readr::read_tsv(
    input_path,
    col_types = cols(
      subject = col_character(),
      block_response = col_character(),
      target = col_character(),
      level = col_character(),
      field = col_character(),
      correct = col_logical(),
      rt = col_double(),
      first_block = col_character(),
      ehi = col_double(),
      age = col_double(),
      country = col_character(),
      sex = col_character(),
      education = col_double(),
      race = col_character(),
      hispanic_ethnicity = col_character(),
      rt_overall = col_double(),
      duration_s = col_double(),
      task_experience_response = col_character(),
      task_experience_other_response = col_logical(),
      open_ended_feedback_response = col_character(),
      exclude_many_gos = col_logical(),
      exclude_low_acc = col_logical(),
      exclude_low_rt = col_logical(),
      exclude_high_rt = col_logical(),
      exclude_country = col_logical(),
      exclude_age = col_logical(),
      exclude_done_before = col_logical(),
      exclude_no_ehi = col_logical(),
      exclude = col_logical()
    )
  )
  return(aah_long)
}
#### Filter aah_long ("the data") to: "main" blocks, "present" trials,
#### and included subjects (exclude == 0) for analyses.
filter_aah_long_for_analysis <- function(aah_long) {
  
  cli::cli_alert_info("Filtering long task data for analysis (no excluded subjects; experimental trials (not practice) only; target present only).")
  cli::cli_bullets(c(" " = "aah_long -> aah"))
  ## For analysis, filter aah_long to:
  ## (1) no excluded subjects,
  ## (2) "main" phase only (filtering out "practice")
  ## (3) target-present trials only (for RT, accuracy by level)
  aah <- aah_long |> 
    filter(exclude == 0) |>
    filter(block_type == "main" & target_present == "yes") |>
    select(-starts_with("exclude")) |>
    ## Recode "level" for nicer printing.
    mutate(level = recode(level, global = "Global", local = "Local"))
  return(aah)
}

#### LOAD SUMMARY DATA (POST-EXCLUSION CALCULATION) FOR ANALYSIS
#### (3_analysis.Rmd)
load_aah_summary <- function(proc_dir) {
  input_path <- here(proc_dir, "aah_summary.tsv")
  
  cli::cli_alert_info("Loading summary data (all subjects) from:")
  cli::cli_bullets(c(" " = "{input_path}", " " = "-> aah_summary"))
  
  aah_summary <- readr::read_tsv(
    input_path,
    col_types = cols(
      subject = col_character(),
      first_block = col_character(),
      ehi = col_double(),
      age = col_double(),
      country = col_character(),
      sex = col_character(),
      education = col_double(),
      race = col_character(),
      hispanic_ethnicity = col_character(),
      rt_overall = col_double(),
      duration_s = col_double(),
      task_experience_response = col_character(),
      task_experience_other_response = col_logical(),
      open_ended_feedback_response = col_character(),
      exclude_many_gos = col_double(),
      exclude_low_acc = col_double(),
      exclude_low_rt = col_double(),
      exclude_high_rt = col_double(),
      exclude_country = col_double(),
      exclude_age = col_double(),
      exclude_done_before = col_double(),
      exclude_no_ehi = col_double(),
      exclude = col_double()
    )
  )
  return(aah_summary)
}
#### Filter summary data for analysis (no excluded subjects)
filter_aah_summary_for_analysis <- function(aah_summary_all) {
  cli::cli_alert_info("Filtering summary data for analysis (no excluded subjects).")
  cli::cli_bullets(c(" " = "aah_summary_all -> aah_summary"))
  aah_summary <- aah_summary_all |>
    filter(exclude == 0) |>
    select(-starts_with("exclude"))
  return(aah_summary)
}