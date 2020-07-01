`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

data_frame <- function(...){
  data.frame(..., stringsAsFactors = FALSE)
}
  
read_sql <- function(filename) {
  paste(readLines(filename), collapse = "\n")
}

read_csv <- function(...) {
  utils::read.csv(..., stringsAsFactors = FALSE)
}

merge_by_common_cols <- function(d1, d2, ...){
  merge(d1, d2, by = intersect(names(d1), names(d2)), ...)
}

not_is_finite <- function(x) {
  is.numeric(x) & !is.finite(x)
}

set_as_na <- function(x) {
  x <- NA
}

'%!in%' <- function(x,y)!('%in%'(x,y))

##
grepv <- function(patterns, value) {
  stopifnot(length(patterns) >= 1)
  j <- rep(0, length(value))
  
  for(i in patterns) {
    j <- j + grepl(i, value)
  }
  
  v <- j == length(patterns)
  
  if(!all(v)) {
    ("not sure yet what to return when not all patterns exits.")
  }
  return(v)
}

assert_has_columns <- function(d, cols_must_have){
  stopifnot(all(cols_must_have %in% names(d)))
}
