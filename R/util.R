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
    stop("not all patterns exit in value.")
  }
  return(v)
}

squote <- function(x){
sprintf("'%s'", x)
}

sql_in <- function(items, text_item = TRUE) {
  items <- paste(if (text_item) squote(items) else items,
                 collapse= ", ")
  sprintf("(%s)", items)
}

system_file <- function(...) {
  system.file(..., mustWork = TRUE, package = "vimpact")
}

vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

assert_has_columns <- function(data, col_names,
                               name = deparse(substitute(data))) {
  missing_names <- setdiff(col_names, colnames(data))
  if (length(missing_names) > 0) {
    stop(sprintf("Required column names %s are missing from %s",
                 paste(missing_names, collapse = ", "),
                 name))
  }
  invisible(TRUE)
}

assert_allowed_values <- function(data, column, values) {
  present_values <- unique(data[[column]])
  additional_values <- setdiff(present_values, values)
  if (length(additional_values) > 0) {
    stop(sprintf("Column '%s' contains values %s. Allowed values are %s.",
                 column,
                 paste(sprintf("'%s'", additional_values), collapse = ", "),
                 paste(sprintf("'%s'", values), collapse = ", ")))
  }
  invisible(TRUE)
}
