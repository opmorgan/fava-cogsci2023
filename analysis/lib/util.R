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
      fmt_missing(columns = everything(), missing_text = "-") |> 
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