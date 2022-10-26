trim_end <- function(string_end) {
  string <- sub("_[^_]+$", "", string_end)
  return(string)
}