## FIELD BY LEVEL BY SHAPE
model_emm_FLS <- function(manual_cache_dir,
                          data_label,
                          data,
                          use_cached_model = FALSE) {
  model_path <-
    here(manual_cache_dir, str_c("FLS_model_", data_label, ".rds"))
  emm_path <-
    here(manual_cache_dir, str_c("FLS_emm_", data_label, ".rds"))
  
  if (use_cached_model == FALSE) {
    ## Create model & emmeans object
    model <- lmer(rt ~ field * level * shape + (1 | subject),
                  data = data)
    emm <- emmeans(model, ~ field * level * shape)
    
    ## Manually cache model
    cli_progress_step(msg = "Caching model: {model_path}",
                      msg_done = "Created and cached model: {model_path}")
    saveRDS(model, model_path)
    cli_progress_step(msg = "Caching emm object: {emm_path}",
                      msg_done = "Created and cached emm_object: {emm_path}")
    saveRDS(emm, emm_path)
    
  } else if (use_cached_model == TRUE) {
    ## Load cached model & emmeans object
    cli_progress_step(msg = "Loading model: {model_path}",
                      msg_done = "Loaded model: {model_path}")
    model <- readRDS(model_path)
    cli_progress_step(msg = "Loading emm object: {emm_path}",
                      msg_done = "Loaded emm object: {emm_path}")
    emm <- readRDS(emm_path)
  }
  
  model_emm <- list()
  model_emm$model <- model
  model_emm$emm <- emm
  
  return(model_emm)
}

anova_FLS <- function(use_cached_anova = FALSE,
                      manual_cache_dir,
                      FLS_model,
                      data_label = data_label,
                      title = "") {
  anova_path <-
    here(manual_cache_dir, str_c("FLS_anova_", data_label, ".rds"))
  
  if (use_cached_anova == FALSE) {
    FLS_model_no_interaction <-
      update(FLS_model, . ~ . - field:level:shape)
    interaction_anova <-
      interaction_stats(FLS_model, FLS_model_no_interaction)
    
    ## Cache anova
    cli_progress_step(msg = "Caching anova: {anova_path}",
                      msg_done = "Created and cached anova: {anova_path}")
    saveRDS(interaction_anova, anova_path)
    
  } else if (use_cached_anova == TRUE) {
    ## Load cached anova
    cli_progress_step(msg = "Loading anova: {anova_path}",
                      msg_done = "Loaded anova: {anova_path}")
    interaction_anova <- readRDS(anova_path)
  }
  
  cli_progress_step(msg = "Printing anova table: {anova_path}",
                    msg_done = "Printed anova table: {anova_path}")
  interaction_anova |>
    as_tibble() |>
    rename(p.value = `Pr(>Chisq)`) |>
    format_p.value() |>
    pretty_table() |>
    tab_header(title = title,
               subtitle = "ANOVA: compare models with vs. without interaction term") |>
    tab_footnote(footnote = "F-test (two-sided? https://daniellakens.blogspot.com/2016/04/one-sided-f-tests-and-halving-p-values.html)",
                 locations = cells_column_labels(columns = p.value))
}



emmtest_FLS <- function(FLS_emm, title = "title") {
  FLS_emmtest <- FLS_emm |>
    contrast(interaction = c("consec")) |>
    summary(infer = T)
  
  FLS_emmtest |>
    as_tibble() |>
    format_p.value() |>
    pretty_table() |>
    tab_header(title = title,
               subtitle = "Compare effect estimate to zero with emmeans()") |>
    tab_footnote(footnote = "A positive number means LVF global bias is stronger for squares (as observed in pilot)",
                 locations = cells_column_labels(columns = estimate)) |>
    tab_footnote(footnote = "Two-sided",
                 locations = cells_column_labels(columns = p.value)) |>
    tab_footnote(footnote = "Confidence level: 95%",
                 locations = cells_column_labels(columns = ends_with("CL"))) |>
    tab_footnote(footnote = "Z-approximation",
                 locations = cells_column_labels(columns = df))
}



emmtest_FL_S <- function(use_cached_model = FALSE,
                         manual_cache_dir,
                         FLS_model,
                         data_label,
                         title = "title") {
  emmtest_path <-
    here(manual_cache_dir,
         str_c("FL_S_emmtest_", data_label, ".rds"))
  
  if (use_cached_model == FALSE) {
    FLS_emmtest <- FLS_model |>
      emmeans(~ field * level | shape) |>
      contrast(interaction = c("consec")) |>
      summary(infer = T)
    
    ## Cache emm test object
    cli_progress_step(msg = "Caching emm test object: {emmtest_path}",
                      msg_done = "Created and cached anova: {emmtest_path}")
    saveRDS(FLS_emmtest, emmtest_path)
  } else if (use_cached_model == TRUE) {
    ## Load cached emm test
    cli_progress_step(msg = "Loading emm test object: {emmtest_path}",
                      msg_done = "Loaded emm test object: {emmtest_path}")
    FLS_emmtest <- readRDS(emmtest_path)
    
  }
  
  FLS_emmtest |>
    as_tibble() |>
    format_p.value() |>
    pretty_table() |>
    tab_header(title = title,
               subtitle = "Compare effect estimate to zero with emmeans()") |>
    tab_footnote(footnote = "A positive number means global bias (faster RT for global)",
                 locations = cells_column_labels(columns = estimate)) |>
    tab_footnote(footnote = "Two-sided",
                 locations = cells_column_labels(columns = p.value)) |>
    tab_footnote(footnote = "Confidence level: 95%",
                 locations = cells_column_labels(columns = ends_with("CL"))) |>
    tab_footnote(footnote = "Z-approximation",
                 locations = cells_column_labels(columns = df))
  
}



emmtest_GB_F_S <- function(FLS_emm, data_label, title = "") {
  FLS_emm |>
    contrast("revpairwise") |>
    summary(infer = T,
            adjust = "none",
            rows = c(3, 4)) |>
    as_tibble() |>
    filter(
      contrast %in%
        c(
          "LVF Local Circle - LVF Global Circle",
          "RVF Local Circle - RVF Global Circle",
          "LVF Local Square - LVF Global Square",
          "RVF Local Square - RVF Global Square"
        )
    ) |>
    format_p.value() |>
    pretty_table() |>
    tab_header(title = title) |>
    tab_footnote(footnote = "A positive number means global bias (faster RT for global)",
                 locations = cells_column_labels(columns = estimate)) |>
    tab_footnote(footnote = "Confidence level: 95%",
                 locations = cells_column_labels(columns = ends_with("CL"))) |>
    tab_footnote(footnote = "Two-sided, uncorrected",
                 locations = cells_column_labels(columns = p.value)) |>
    tab_footnote(footnote = "Z-approximation",
                 locations = cells_column_labels(columns = df))
}



emmtest_F_L_S <- function(FLS_emm, data_label, title = "") {
  FLS_emm |> summary() |>
    as_tibble() |>
    arrange(desc(field)) |>
    pretty_table() |>
    tab_header(title = title) |>
    tab_footnote(footnote = "Confidence level: 95%",
                 locations = cells_column_labels(columns = ends_with("CL"))) |>
    tab_footnote(footnote = "Z-approximation",
                 locations = cells_column_labels(columns = df))
}


## FIELD BY LEVEL BY SHAPE BY HANDEDNESS (BINNED)
model_emm_FLSH <- function(manual_cache_dir,
                          data_label,
                          data,
                          use_cached_model = FALSE) {
  model_path <-
    here(manual_cache_dir, str_c("FLSH_model_", data_label, ".rds"))
  emm_path <-
    here(manual_cache_dir, str_c("FLSH_emm_", data_label, ".rds"))
  
  if (use_cached_model == FALSE) {
    ## Create model & emmeans object
    model <- lmer(rt ~ field * level * handedness * shape + (1 | subject),
                  data = data)
    emm <- emmeans(model, ~ field * level * handedness * shape)
    
    ## Manually cache model
    cli_progress_step(msg = "Caching model: {model_path}",
                      msg_done = "Created and cached model: {model_path}")
    saveRDS(model, model_path)
    cli_progress_step(msg = "Caching emm object: {emm_path}",
                      msg_done = "Created and cached emm_object: {emm_path}")
    saveRDS(emm, emm_path)
    
  } else if (use_cached_model == TRUE) {
    ## Load cached model & emmeans object
    cli_progress_step(msg = "Loading model: {model_path}",
                      msg_done = "Loaded model: {model_path}")
    model <- readRDS(model_path)
    cli_progress_step(msg = "Loading emm object: {emm_path}",
                      msg_done = "Loaded emm object: {emm_path}")
    emm <- readRDS(emm_path)
  }
  
  model_emm <- list()
  model_emm$model <- model
  model_emm$emm <- emm
  
  return(model_emm)
}

anova_FLSH <- function(use_cached_anova = FALSE,
                      manual_cache_dir,
                      FLSH_model,
                      data_label = data_label,
                      title = "") {
  anova_path <-
    here(manual_cache_dir, str_c("FLSH_anova_", data_label, ".rds"))
  
  if (use_cached_anova == FALSE) {
    FLSH_model_no_interaction <-
      update(FLSH_model, . ~ . - field:level:handedness:shape)
    interaction_anova <-
      interaction_stats(FLSH_model, FLSH_model_no_interaction)
    
    ## Cache anova
    cli_progress_step(msg = "Caching anova: {anova_path}",
                      msg_done = "Created and cached anova: {anova_path}")
    saveRDS(interaction_anova, anova_path)
    
  } else if (use_cached_anova == TRUE) {
    ## Load cached anova
    cli_progress_step(msg = "Loading anova: {anova_path}",
                      msg_done = "Loaded anova: {anova_path}")
    interaction_anova <- readRDS(anova_path)
  }
  
  cli_progress_step(msg = "Printing anova table: {anova_path}",
                    msg_done = "Printed anova table: {anova_path}")
  interaction_anova |>
    as_tibble() |>
    rename(p.value = `Pr(>Chisq)`) |>
    format_p.value() |>
    pretty_table() |>
    tab_header(title = title,
               subtitle = "ANOVA: compare models with vs. without interaction term") |>
    tab_footnote(footnote = "F-test (two-sided? https://daniellakens.blogspot.com/2016/04/one-sided-f-tests-and-halving-p-values.html)",
                 locations = cells_column_labels(columns = p.value))
}


emmtest_FLSH <- function(FLSH_emm, title = "") {
  FLSH_emmtest <- FLSH_emm |>
    contrast(interaction = c("consec")) |>
    summary(infer = T)
  
  FLSH_emmtest |>
    as_tibble() |>
    format_p.value() |>
    pretty_table() |>
    tab_header(title = title,
               subtitle = "Compare effect estimate to zero with emmeans()") |>
    tab_footnote(footnote = "A positive number means the interaction of field by level by handedness is stronger for squares",
                 locations = cells_column_labels(columns = estimate)) |>
    tab_footnote(footnote = "Two-sided",
                 locations = cells_column_labels(columns = p.value)) |>
    tab_footnote(footnote = "Confidence level: 95%",
                 locations = cells_column_labels(columns = ends_with("CL"))) |>
    tab_footnote(footnote = "Z-approximation",
                 locations = cells_column_labels(columns = df))
}



emmtest_FLH_S <- function(use_cached_model = FALSE,
                         manual_cache_dir,
                         FLSH_model,
                         data_label,
                         title = "title") {
  emmtest_path <-
    here(manual_cache_dir,
         str_c("FLH_S_emmtest_", data_label, ".rds"))
  
  if (use_cached_model == FALSE) {
    FLH_S_emmtest <- FLSH_model |>
      emmeans(~ field * level * handedness | shape) |>
      contrast(interaction = c("consec")) |>
      summary(infer = T)
    
    ## Cache emm test object
    cli_progress_step(msg = "Caching emm test object: {emmtest_path}",
                      msg_done = "Created and cached anova: {emmtest_path}")
    saveRDS(FLH_S_emmtest, emmtest_path)
  } else if (use_cached_model == TRUE) {
    ## Load cached emm test
    cli_progress_step(msg = "Loading emm test object: {emmtest_path}",
                      msg_done = "Loaded emm test object: {emmtest_path}")
    FLH_S_emmtest <- readRDS(emmtest_path)
    
  }
  
  FLH_S_emmtest |>
    as_tibble() |>
    format_p.value() |>
    pretty_table() |>
    tab_header(title = title,
               subtitle = "Compare effect estimate to zero with emmeans()") |>
    tab_footnote(footnote = "A positive number means more LVF global bias for right handers",
                 locations = cells_column_labels(columns = estimate)) |>
    tab_footnote(footnote = "Two-sided",
                 locations = cells_column_labels(columns = p.value)) |>
    tab_footnote(footnote = "Confidence level: 95%",
                 locations = cells_column_labels(columns = ends_with("CL"))) |>
    tab_footnote(footnote = "Z-approximation",
                 locations = cells_column_labels(columns = df))
  
}

emmtest_FLS_H <- function(use_cached_model = FALSE,
                         manual_cache_dir,
                         FLSH_model,
                         data_label,
                         title = "title") {
  emmtest_path <-
    here(manual_cache_dir,
         str_c("FLS_H_emmtest_", data_label, ".rds"))
  
  if (use_cached_model == FALSE) {
    FLS_H_emmtest <- FLSH_model |>
      emmeans(~ field * level * shape | handedness) |>
      contrast(interaction = c("consec")) |>
      summary(infer = T)
    
    ## Cache emm test object
    cli_progress_step(msg = "Caching emm test object: {emmtest_path}",
                      msg_done = "Created and cached anova: {emmtest_path}")
    saveRDS(FLS_H_emmtest, emmtest_path)
  } else if (use_cached_model == TRUE) {
    ## Load cached emm test
    cli_progress_step(msg = "Loading emm test object: {emmtest_path}",
                      msg_done = "Loaded emm test object: {emmtest_path}")
    FLS_H_emmtest <- readRDS(emmtest_path)
    
  }
  
  FLS_H_emmtest |>
    as_tibble() |>
    format_p.value() |>
    pretty_table() |>
    tab_header(title = title,
               subtitle = "Compare effect estimate to zero with emmeans()") |>
    tab_footnote(footnote = "A positive number means the interaction of field by level by handedness is stronger for squares",
                 locations = cells_column_labels(columns = estimate)) |>
    tab_footnote(footnote = "Two-sided",
                 locations = cells_column_labels(columns = p.value)) |>
    tab_footnote(footnote = "Confidence level: 95%",
                 locations = cells_column_labels(columns = ends_with("CL"))) |>
    tab_footnote(footnote = "Z-approximation",
                 locations = cells_column_labels(columns = df))
  
}

emmtest_FL_H_S <- function(use_cached_model = FALSE,
                         manual_cache_dir,
                         FLSH_model,
                         data_label,
                         title = "title") {
  emmtest_path <-
    here(manual_cache_dir,
         str_c("FL_H_S_emmtest_", data_label, ".rds"))
  
  if (use_cached_model == FALSE) {
    FL_H_S_emmtest <- FLSH_model |>
      emmeans(~ field * level | handedness | shape ) |>
      contrast(interaction = c("consec")) |>
      summary(infer = T)
    
    ## Cache emm test object
    cli_progress_step(msg = "Caching emm test object: {emmtest_path}",
                      msg_done = "Created and cached anova: {emmtest_path}")
    saveRDS(FL_H_S_emmtest, emmtest_path)
  } else if (use_cached_model == TRUE) {
    ## Load cached emm test
    cli_progress_step(msg = "Loading emm test object: {emmtest_path}",
                      msg_done = "Loaded emm test object: {emmtest_path}")
    FL_H_S_emmtest <- readRDS(emmtest_path)
    
  }
  
  
  FL_H_S_emmtest |>
    as_tibble() |>
    format_p.value() |>
    pretty_table() |>
    tab_header(title = title,
               subtitle = "Compare effect estimate to zero with emmeans()") |>
    tab_footnote(footnote = "A positive number means LVF global bias",
                 locations = cells_column_labels(columns = estimate)) |>
    tab_footnote(footnote = "Two-sided",
                 locations = cells_column_labels(columns = p.value)) |>
    tab_footnote(footnote = "Confidence level: 95%",
                 locations = cells_column_labels(columns = ends_with("CL"))) |>
    tab_footnote(footnote = "Z-approximation",
                 locations = cells_column_labels(columns = df))
  
}



## FIELD BY LEVEL BY SHAPE BY HANDEDNESS (continuous)
model_emm_FLSehi <- function(manual_cache_dir,
                           data_label,
                           data,
                           use_cached_model = FALSE) {
  model_path <-
    here(manual_cache_dir, str_c("FLSehi_model_", data_label, ".rds"))
  emm_path <-
    here(manual_cache_dir, str_c("FLSehi_emm_", data_label, ".rds"))
  
  if (use_cached_model == FALSE) {
    ## Create model & emmeans object
    model <- lmer(rt ~ field * level * ehi * shape + (1 | subject),
                  data = data)
    emm <- emmeans(model, ~ field * level * ehi * shape)
    
    ## Manually cache model
    cli_progress_step(msg = "Caching model: {model_path}",
                      msg_done = "Created and cached model: {model_path}")
    saveRDS(model, model_path)
    cli_progress_step(msg = "Caching emm object: {emm_path}",
                      msg_done = "Created and cached emm_object: {emm_path}")
    saveRDS(emm, emm_path)
    
  } else if (use_cached_model == TRUE) {
    ## Load cached model & emmeans object
    cli_progress_step(msg = "Loading model: {model_path}",
                      msg_done = "Loaded model: {model_path}")
    model <- readRDS(model_path)
    cli_progress_step(msg = "Loading emm object: {emm_path}",
                      msg_done = "Loaded emm object: {emm_path}")
    emm <- readRDS(emm_path)
  }
  
  model_emm <- list()
  model_emm$model <- model
  model_emm$emm <- emm
  
  return(model_emm)
}

anova_FLSehi <- function(use_cached_anova = FALSE,
                       manual_cache_dir,
                       FLSehi_model,
                       data_label = data_label,
                       title = "") {
  anova_path <-
    here(manual_cache_dir, str_c("FLSehi_anova_", data_label, ".rds"))
  
  if (use_cached_anova == FALSE) {
    FLSehi_model_no_interaction <-
      update(FLSehi_model, . ~ . - field:level:ehi:shape)
    interaction_anova <-
      interaction_stats(FLSehi_model, FLSehi_model_no_interaction)
    
    ## Cache anova
    cli_progress_step(msg = "Caching anova: {anova_path}",
                      msg_done = "Created and cached anova: {anova_path}")
    saveRDS(interaction_anova, anova_path)
    
  } else if (use_cached_anova == TRUE) {
    ## Load cached anova
    cli_progress_step(msg = "Loading anova: {anova_path}",
                      msg_done = "Loaded anova: {anova_path}")
    interaction_anova <- readRDS(anova_path)
  }
  
  cli_progress_step(msg = "Printing anova table: {anova_path}",
                    msg_done = "Printed anova table: {anova_path}")
  interaction_anova |>
    as_tibble() |>
    rename(p.value = `Pr(>Chisq)`) |>
    format_p.value() |>
    pretty_table() |>
    tab_header(title = title,
               subtitle = "ANOVA: compare models with vs. without interaction term") |>
    tab_footnote(footnote = "F-test (two-sided? https://daniellakens.blogspot.com/2016/04/one-sided-f-tests-and-halving-p-values.html)",
                 locations = cells_column_labels(columns = p.value))
}

emmtest_FLSehi <- function(use_cached_model = FALSE,
                           manual_cache_dir,
                           FLSehi_model,
                           data_label,
                           title = "") {
  
  emt_path <-
    here(manual_cache_dir, str_c("FLSehi_emt_", data_label, ".rds"))
  
  
  if (use_cached_model == FALSE) {
    ## This code estimates the effect of EHI on RT for each field*level*shape:
    emt <-
      emtrends(FLSehi_model, pairwise ~ field * level * shape, var = "ehi")
    
    ## Manually cache emtrends object
    cli_progress_step(msg = "Caching emtrends object: {emt_path}",
                      msg_done = "Created and cached emtrends object: {emt_path}")
    saveRDS(emt, emt_path)
    
  } else if (use_cached_model == TRUE) {
    ## Load cached emtrends object
    cli_progress_step(msg = "Loading emtrends object: {emt_path}",
                      msg_done = "Created and cached emtrends object: {emt_path}")
    cli_progress_step(msg = "Loading emtrends object: {emt_path}",
                      msg_done = "Loaded emtrends object: {emt_path}")
    emt <- readRDS(emt_path)
  }
    
  emt$emtrends |>
    contrast(interaction = c("consec")) |>
    summary(infer = T) |>
    as_tibble() |>
    format_p.value() |>
    pretty_table() |>
    tab_header(title = title,
               subtitle = "Compare effect estimate to zero with emmeans()") |>
    tab_footnote(footnote = "A positive number means the interaction of field by level by EHI is stronger for squares",
                 locations = cells_column_labels(columns = estimate)) |>
    tab_footnote(footnote = "Two-sided",
                 locations = cells_column_labels(columns = p.value)) |>
    tab_footnote(footnote = "Confidence level: 95%",
                 locations = cells_column_labels(columns = ends_with("CL"))) |>
    tab_footnote(footnote = "Z-approximation",
                 locations = cells_column_labels(columns = df))
  
}
  

emmtest_FLehi_S <- function(use_cached_model = FALSE,
                           manual_cache_dir,
                           FLSehi_model,
                           data_label,
                           title = "") {
  
  emt_path <-
    here(manual_cache_dir, str_c("FLehi_S_emt_", data_label, ".rds"))
  
  
  if (use_cached_model == FALSE) {
    emt <-
      emtrends(FLSehi_model, pairwise ~ field * level |
                 shape, var = "ehi")
    
    ## Manually cache emtrends object
    cli_progress_step(msg = "Caching emtrends object: {emt_path}",
                      msg_done = "Created and cached emtrends object: {emt_path}")
    saveRDS(emt, emt_path)
    
  } else if (use_cached_model == TRUE) {
    ## Load cached emtrends object
    cli_progress_step(msg = "Loading emtrends object: {emt_path}",
                      msg_done = "Created and cached emtrends object: {emt_path}")
    cli_progress_step(msg = "Loading emtrends object: {emt_path}",
                      msg_done = "Loaded emtrends object: {emt_path}")
    emt <- readRDS(emt_path)
  }
  
  
  emt$emtrends |>
    contrast(interaction = c("consec"), var = "ehi") |>
    summary(infer = T) |>
    as_tibble() |>
    format_p.value() |>
    pretty_table() |>
    tab_header(title = title,
               subtitle = "") |>
    tab_footnote(footnote = "A positive number means more global bias for right handers, in ms per EHI unit (-100 to 100)",
                 locations = cells_column_labels(columns = estimate)) |>
    tab_footnote(footnote = "Two-sided",
                 locations = cells_column_labels(columns = p.value)) |>
    tab_footnote(footnote = "Confidence level: 95%",
                 locations = cells_column_labels(columns = ends_with("CL"))) |>
    tab_footnote(footnote = "Z-approximation",
                 locations = cells_column_labels(columns = df))
  
}

pull_ehi_FLS <- function(use_cached_model = F,
                          manual_cache_dir,
                          FLSehi_model,
                          data_label,
                          title = "") {
  ref_path <-
    here(manual_cache_dir, str_c("FLehi_S_ref_", data_label, ".rds"))
  
  
  if (use_cached_model == FALSE) {
    ref <- ref_grid(FLSehi_model, at = list(ehi = c(-100, 0, 100)))
    
    ## Manually cache refrends object
    cli_progress_step(msg = "Caching ref object: {ref_path}",
                      msg_done = "Created and cached ref object: {ref_path}")
    saveRDS(ref, ref_path)
    
  } else if (use_cached_model == TRUE) {
    ## Load cached refrends object
    cli_progress_step(msg = "Loading ref object: {ref_path}",
                      msg_done = "Created and cached ref object: {ref_path}")
    cli_progress_step(msg = "Loading ref object: {ref_path}",
                      msg_done = "Loaded ref object: {ref_path}")
    ref <- readRDS(ref_path)
  }
  
  ref |>
    contrast(interaction = c("consec"), by = "ehi") |>
    summary(infer = T, type = "response") |>
    as_tibble() |>
    format_p.value() |>
    pretty_table() |>
    tab_header(title = title) |>
    tab_footnote(footnote = "Strong left hander: -100; Mixed hander: 0; Strong right hander: +100",
                 locations = cells_column_labels(columns = ehi)) |>
    tab_footnote(footnote = "Estimated LVF global bias (ms); a positive number means LVF global bias",
                 locations = cells_column_labels(columns = estimate))
}


pull_ehi_FL_S <- function(use_cached_model = F,
                          manual_cache_dir,
                          FLSehi_model,
                          data_label,
                          title = "") {
  
  ref_path <-
    here(manual_cache_dir, str_c("FLehi_S_ref_", data_label, ".rds"))
  
  
  if (use_cached_model == FALSE) {
ref <- ref_grid(FLSehi_model, at = list(ehi = c(-100, 0, 100)))
    
    ## Manually cache refrends object
    cli_progress_step(msg = "Caching ref object: {ref_path}",
                      msg_done = "Created and cached ref object: {ref_path}")
    saveRDS(ref, ref_path)
    
  } else if (use_cached_model == TRUE) {
    ## Load cached refrends object
    cli_progress_step(msg = "Loading ref object: {ref_path}",
                      msg_done = "Created and cached ref object: {ref_path}")
    cli_progress_step(msg = "Loading ref object: {ref_path}",
                      msg_done = "Loaded ref object: {ref_path}")
    ref <- readRDS(ref_path)
  }

ref |> contrast(interaction = "consec", by = c("shape", "ehi")) |> 
  summary(infer = T) |> 
  as_tibble() |> 
  arrange(shape, ehi) |> 
  format_p.value() |> 
  pretty_table() |> 
  tab_header(title = title) |> 
  tab_footnote(footnote = "Estimated LVF global bias (ms); a positive number means LVF global bias",
               locations = cells_column_labels(columns = estimate)) |> 
    tab_footnote(footnote = "Two-sided",
                 locations = cells_column_labels(columns = p.value)) |>
    tab_footnote(footnote = "Confidence level: 95%",
                 locations = cells_column_labels(columns = ends_with("CL"))) |>
    tab_footnote(footnote = "Z-approximation",
                 locations = cells_column_labels(columns = df))
  
}
