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

assert_has_columns <- function(d, cols_must_have){
  stopifnot(all(cols_must_have %in% names(d)))
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

assert_col_names <- function(data, col_names,
                             name = deparse(substitute(data))) {
  missing_names <- setdiff(col_names, colnames(data))
  if (length(missing_names) > 0) {
    stop(sprintf("Required column names %s are missing from %s",
                 paste(missing_names, collapse = ", "),
                 name))
  }
  invisible(TRUE)
}

assert_group_counts <- function(data1, data2, group_names,
                                data1_name = deparse(substitute(data1)),
                                data2_name = deparse(substitute(data2))) {
  group_count_1 <- aggregate(data1, group_names, NROW)
  if (nrow(data1) != nrow(data2)) {
    text <- vcapply(seq_len(group_names), function(col) {
      group_name <- group_names[col]
      data1_count <- length(unique(data1[[col]]))
      data2_count <- length(unique(data2[[col]]))
      sprintf("%s: %s has %s groups and %s has %s groups",
              group_name, data1_name, data1_count, data2_name, data2_count)
    })
    text <- c("Datasets have different numbers of groups", text)
    stop(paste(text, collapse = "\n  *"))
  }
  invisible(TRUE)
}
