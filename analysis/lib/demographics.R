## Functions to create a one-row demographics table with summary info
report_mean_sd <- function(numeric_vector) {
  return(str_c(
    round(mean(numeric_vector, na.rm = T),2),
    " (", round(sd(numeric_vector, na.rm = T), 2), ")"))
}

report_country <- function(country_char_vector) {
  n_us <- sum(country_char_vector == "US")
  n_not_us <- sum(country_char_vector != "US")
  return(str_c(n_us, "/", n_not_us))
}

report_sex <- function(sex_char_vector) {
  n_male <- sum(sex_char_vector == "Male")
  n_female <- sum(sex_char_vector == "Female")
  n_other <- sum(!(sex_char_vector %in% c("Male", "Female")))
  if (n_other == 0) {
    return(str_c(n_male, "/", n_female))
  } else if (n_other >= 1) {
    return(str_c(n_male, "/", n_female, "/", n_other))
  }
}

demo_summary_table <- function(aah_summary) {
demo_table <- aah_summary |>
  summarize(N = n(),
            `Age (years)` = report_mean_sd(age),
            `Education (years)`= report_mean_sd(education),
            `Sex (M/F/O)` = report_sex(sex),
            EHI = report_mean_sd(ehi_total)
  )
return(demo_table)
}

demo_country_table <- function(aah_summary){
  aah_summary |>
  group_by(country) |>
  summarize(n = n()) |>
  rename(Country = country) |> 
  arrange(-n)
}

demo_race_table <- function(aah_summary){
aah_summary |>
  group_by(race) |>
  summarize(n = n()) |>
  rename(Race = race) |> 
  arrange(-n)
}


demo_ethnicity_table <- function(aah_summary){
  aah_summary |>
  group_by(hispanic_ethnicity) |>
  summarize(n = n()) |>
  rename(`Hispanic ethnicity` = hispanic_ethnicity) |> 
  arrange(-n)
}

demo_xp_table <- function(aah_summary){
  aah_summary |>
  group_by(task_experience_response, task_experience_other_response) |>
  summarize(n = n()) |>
  rename(`Have you done this task before?` = task_experience_response,
         Explanation = task_experience_other_response) |> 
  arrange(-n)
}
