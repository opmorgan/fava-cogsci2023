data_tests <- function(data_cleaned, data_type) {
  if (data_type == "task") {
    cli::cli_progress_step(
      msg = glue(
        "Testing that data file includes the expected number of each trial type..."
      ),
      msg_done = "Tested that data file includes the expected number of each trial type."
    )
    subject_id <-
      data_cleaned$subject |> first() |> as.character()
    ## Test that:
    ## (1) There are 160 trials  where block_type = "main"
    main_trials <- data_cleaned |> filter(block_type == "main")
    n_main_trials <- main_trials |>  dim() |> (\(.) .[[1]])()
    expected_n <- 160
    if (n_main_trials != expected_n) {
      cli::cli_abort(c(
        "{.var n_main_trials} is equal to {n_main_trials}",
        "x" = str_c(
          "The number of main trials is not equal to {expected_n} (Subject: {subject_id})"
        )
      ))
    }
    
    ## (1) Of these 160 trials, 80 have block = "main_z"; 80, "main_slash"
    for (block_response_var in c("z", "slash")) {
      main_block_trials <- main_trials |>
        filter(block_response == block_response_var)
      n_main_block_trials <- main_block_trials |> dim() |> (\(.) .[[1]])()
      expected_n <- 80
      if (n_main_block_trials != expected_n) {
        cli::cli_abort(c(
          "{.var n_main_block_trials} is equal to {n_main_block_trials}",
          "x" = str_c(
            "The number of main block trials ({block_response_var}) is not equal to {expected_n} (Subject: {subject_id})"
          )
        ))
      }
    }
    
    ## (1) In each block of 80 trials, 80% (64/80) of trials are target-present (they contain a circle or a square), and 20% (16/80) of trials are target-absent
    for (block_response_var in c("z", "slash")) {
      main_block_trials <- main_trials |>
        filter(block_response == block_response_var)
      main_block_present_trials <- main_block_trials |>
        filter(target %in% c("square", "circle"))
      n_main_block_present_trials <-
        main_block_present_trials |> dim() |> (\(.) .[[1]])()
      expected_n <- 64
      if (n_main_block_present_trials != expected_n) {
        cli::cli_abort(
          c(
            "{.var n_main_block_present_trials} is equal to {n_main_block_present_trials}",
            "x" = str_c(
              "The number of main block trials ({block_response_var}) with target present is not equal to {expected_n} (Subject: {subject_id})"
            )
          )
        )
      }
    }
    
    ## (1) In each block of 80 trials, 32 have global targets; 32, local.
    for (block_response_var in c("z", "slash")) {
      main_block_trials <- main_trials |>
        filter(block_response == block_response_var)
      for (level_var in c("global", "local")) {
        main_block_level_trials <- main_block_trials |>
          filter(level == level_var)
        n_main_block_level_trials <-
          main_block_level_trials |> dim() |> (\(.) .[[1]])()
        expected_n <- 32
        if (n_main_block_level_trials != expected_n) {
          cli::cli_abort(
            c(
              "{.var n_main_block_level_trials} is equal to {n_main_block_level_trials}",
              "x" = str_c(
                "The number of main block trials ({block_response_var}) with {level_var} targets is not equal to {expected_n} (Subject: {subject_id})"
              )
            )
          )
        }
      }
    }
    
    
    ## (1) Of the 32 global targets, 16 are LVF and 16 RVF; of the 32 local, 16 are LVF/RVF.
    for (block_response_var in c("z", "slash")) {
      main_block_trials <- main_trials |>
        filter(block_response == block_response_var)
      for (level_var in c("global", "local")) {
        main_block_level_trials <- main_block_trials |>
          filter(level == level_var)
        n_main_block_level_trials <-
          main_block_level_trials |> dim() |> (\(.) .[[1]])()
        
        for (field_var in c("LVF", "RVF")) {
          main_block_level_field_trials <- main_block_level_trials |>
            filter(field == field_var)
          
          n_main_block_level_field_trials <-
            main_block_level_field_trials |>
            dim() |> (\(.) .[[1]])()
          
          expected_n <- 16
          if (n_main_block_level_field_trials != expected_n) {
            cli::cli_abort(
              c(
                "{.var n_main_block_level_field_trials} is equal to {n_main_block_level_field_trials}",
                "x" = str_c(
                  "The number of main block trials ({block_response_var}) with {level_var} targets in the {field_var} is not equal to {expected_n} (Subject: {subject_id})"
                )
              )
            )
          }
        }
      }
    }
    
    cli::cli_progress_step(
      msg = glue(
        "Testing that correct/incorrect responses are coded as expected..."
      ),
      msg_done = "Tested that correct/incorrect responses are coded as expected."
    )
    
    ## (1) When "target" is anything other than absent, the column "target_present" has a "yes"; if not, a "no"
    test_vector <- data_cleaned |>
      mutate(
        correctly_matches = case_when(
          (target %in% c("circle", "square")) & target_present == "yes" ~ 1,
          (target == "absent") & target_present == "no" ~ 1,
          (target %in% c("circle", "square")) &
            target_present == "no" ~ 0,
          (target == "absent") & target_present == "yes" ~ 0,
          TRUE ~ 0
        )
      ) |> (\(.) .[["correctly_matches"]])()
    n_mismatches <- sum(test_vector == 0)
    if (n_mismatches >= 1) {
      cli::cli_abort(
        c(
          "{.var n_mismatches} is equal to {n_mismatches}, but it should be equal to zero.",
          "x" = str_c(
            "In {n_mismatches} cases, a trial is marked as 'present' when no circle or square was recorded, or as 'absent' when a circle or square was recorded (Subject: {subject_id})"
          )
        )
      )
    }
    
    ## (1)When a target is present and response is "present,"
    ## the trial is marked as correct.
    test_vector <- data_cleaned |>
      mutate(correctly_matches = case_when(
        (target_present == "yes")
        & (response == "present")
        & (correct) == 0 ~ 0,
        TRUE ~ 1
      )) |> (\(.) .[["correctly_matches"]])()
    n_mismatches <- sum(test_vector == 0)
    if (n_mismatches >= 1) {
      cli::cli_abort(
        c(
          "{.var n_mismatches} is equal to {n_mismatches}, but it should be equal to zero.",
          "x" = str_c(
            "In {n_mismatches} cases, a trial is marked as incorrect when a target was present and response was 'present' (Subject: {subject_id})"
          )
        )
      )
    }
    
    ## (1)When a target is present and response is "absent,"
    ## the trial is marked as incorrect.
    test_vector <- data_cleaned |>
      mutate(correctly_matches = case_when(
        (target_present == "yes")
        & (response == "absent")
        & (correct) == 1 ~ 0,
        TRUE ~ 1
      )) |> (\(.) .[["correctly_matches"]])()
    n_mismatches <- sum(test_vector == 0)
    if (n_mismatches >= 1) {
      cli::cli_abort(
        c(
          "{.var n_mismatches} is equal to {n_mismatches}, but it should be equal to zero.",
          "x" = str_c(
            "In {n_mismatches} cases, a trial is marked as correct when a target was present and response was 'absent' (Subject: {subject_id})"
          )
        )
      )
    }
    
    ## (1)When no target is present and response is "absent,"
    ## the trial is marked as correct.
    test_vector <- data_cleaned |>
      mutate(correctly_matches = case_when(
        (target_present == "no")
        & (response == "absent")
        & (correct) == 0 ~ 0,
        TRUE ~ 1
      )) |> (\(.) .[["correctly_matches"]])()
    n_mismatches <- sum(test_vector == 0)
    if (n_mismatches >= 1) {
      cli::cli_abort(
        c(
          "{.var n_mismatches} is equal to {n_mismatches}, but it should be equal to zero.",
          "x" = str_c(
            "In {n_mismatches} cases, a trial is marked as incorrect when no target was present and response was 'absent' (Subject: {subject_id})"
          )
        )
      )
    }
    
    ## (1)When no target is present and response is "present,"
    ## the trial is marked as incorrect.
    test_vector <- data_cleaned |>
      mutate(correctly_matches = case_when(
        (target_present == "no")
        & (response == "present")
        & (correct) == 1 ~ 0,
        TRUE ~ 1
      )) |> (\(.) .[["correctly_matches"]])()
    n_mismatches <- sum(test_vector == 0)
    if (n_mismatches >= 1) {
      cli::cli_abort(
        c(
          "{.var n_mismatches} is equal to {n_mismatches}, but it should be equal to zero.",
          "x" = str_c(
            "In {n_mismatches} cases, a trial is marked as correct when no target was present and response was 'present' (Subject: {subject_id})"
          )
        )
      )
    }
  } else if (data_type == "ehi") {
    return()
  } else if (data_type == "demographics") {
    return()
  } else if (data_type == "end") {
    return()
  } else
    return()
  
  #### TODO: Test for demographics
  
  #### TODO: Tests for EHI
  
  #### TODO: Tests for end questions
}