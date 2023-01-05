trim_end <- function(string_end) {
  string <- sub("_[^_]+$", "", string_end)
  return(string)
}

## Function to make pretty table that rounds to 2 digits and stays in place
library(gt)
pretty_table <- function(table, title = NULL, digits = 3,
                         groupname_col = NULL
                         ) {
    gt(table, groupname_col = groupname_col) |> 
      tab_header(title = title) |> 
      sub_missing(columns = everything(), missing_text = "-") |> 
      fmt_number(columns = where(is.numeric),
                 drop_trailing_zeros = T,
                 decimals = digits) |> 
   tab_style(
      #Apply new style to all column headers
     locations = cells_column_labels(columns = everything()),
     style     = list(
       #Give a thick border below
       # cell_borders(sides = "bottom", weight = px(2)),
       #Make text bold
       cell_text(weight = "bold")
     )
   )
}


## Format p-values in a table
format_p.value <- function(tbl) {
  tbl  %>%
    ## display p as "<.001" if it is less than 0.001; or else, round.
    ## Diaply p as "<.0001 if less that .0001; or else, round.
    mutate(p.value = case_when(
      (p.value < 0.0001) ~ "<.0001",
      (0.0001 <= p.value & p.value < 0.001) ~
        as.character(p.value %>% round(4)),
      (0.001 <= p.value & p.value < 0.01) ~
        as.character(p.value %>% round(3)),
      (0.01 <= p.value) ~
        as.character(p.value  %>% round(3))
    )) |> 
    ## Remove leading zero
    mutate(p.value = p.value |> str_remove("^0+"))
}


## Compare two models with anova()
interaction_stats <-
  function(model_with_interaction,
           model_with_no_interaction) {
    return(anova(model_with_interaction, model_with_no_interaction))
  }
