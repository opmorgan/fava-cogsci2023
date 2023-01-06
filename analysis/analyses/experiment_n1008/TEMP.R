```{r}
## Estimate LVF Global bias for EHI -100
pull_LVF_global_bias <- function(X_tbl) {
  X_LVF_est <-
    X_tbl |> slice(1) |> pull(odds.ratio)
  
  X_RVF_est <-
    X_tbl |> slice(2) |> pull(odds.ratio)
  
  X_LVF_global_bias <- (X_LVF_est - X_RVF_est)
  return(X_LVF_global_bias)
}

L_LVF_global_bias <- pull_LVF_global_bias(L_tbl)
L_LVF_global_bias |>
  as_tibble() |>
  rename(LVF_global_bias = value) |>
  pretty_table() |> 
  tab_header(title = "Estimated LVF Global Bias for EHI of -100 (strong left hander)") 
```
<br>
  ```{r}
R_tbl |> 
  format_p.value() |> 
  pretty_table() |> 
  tab_header(title = "Estimated global bias by field, for EHI of +100 (strong right hander)") |> 
  tab_footnote(footnote = "Estimated global bias (ms)",
               locations = cells_column_labels(columns = odds.ratio))
## This shows the pairwise differences like RVF Global - RVF Local
## when ehi = +100.
```
<br>
  ```{r}
R_LVF_global_bias <- pull_LVF_global_bias(R_tbl)
R_LVF_global_bias |>
  as_tibble() |>
  rename(LVF_global_bias = value) |>
  pretty_table() |> 
  tab_header(title = "Estimated LVF Global Bias for EHI of +100 (strong right hander)")

## Odds for LVF global bias for right handers: 0.98 (logodds: -0.0202)
## Odds for LVF global bias for strong left handers: 1.138 (logodds: 0.13)
```
