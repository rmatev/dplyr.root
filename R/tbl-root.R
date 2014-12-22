#' Create a ROOT tbl
#'


#' @export
same_src.tbl_root <- function(x, y) {
  if (!inherits(y, "tbl_root")) return(FALSE)
  same_src(x$src, y$src)
}

#' @export
tbl_vars.tbl_root <- function(x) {
  names(x$vars)
}

#' @export
groups.tbl_root <- function(x) {
  NULL
}

# Grouping methods -------------------------------------------------------------

#' @export
ungroup.tbl_root <- function(x, ...) {
  x
}

#' @export
group_size.tbl_root <- function(x) {
  stop('grouping not implemented for ROOT tables', call. = FALSE)
}

#' @export
n_groups.tbl_root <- function(x) {
  if (is.null(groups(x))) return(1L)
  
  stop('grouping not implemented for ROOT tables', call. = FALSE)
}

# Standard data frame methods --------------------------------------------------

#' @export
as.data.frame.tbl_root <- function(x, row.names = NULL, optional = NULL, ..., n = 100000L) {
  collect.tbl_root(x, n = n, ...)
}

#' @export
print.tbl_root <- function(x, ..., n = NULL, width = NULL) {
  cat("Source: root file ", dim_desc(x), "\n", sep = "")
  cat("\n")
  trunc_mat(x, n = n, width = width)
}

#' @export
dimnames.tbl_root <- function(x) {
  list(NULL, tbl_vars.tbl_root(x))
}

#' @export
dim.tbl_root <- function(x) {
  if (length(x$selection) == 0) {
    n <- RootTreeToR::nEntries(x$tree)
  } else {
    n <- NA
  }
  
  p <- length(x$vars)
  c(n, p)
}

#' @export
head.tbl_root <- function(x, n = 6L, ...) {
  assert_that(length(n) == 1, n > 0L)
  
  as.data.frame(x, n = as.integer(n))
}

#' @export
tail.tbl_root <- function(x, n = 6L, ...) {
  stop("tail is not supported by ROOT tables", call. = FALSE)
}

# Verbs ------------------------------------------------------------------------

#' @export
filter_.tbl_root <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  input <- partial_eval(dots, .data)

  evaluated <- lapply(input, function(expr) {
    env <- root_env(expr, .data$vars)
    eval(expr, envir = env)
  })
  
  .data$selection = c(.data$selection, evaluated)
  .data
}

#' @export
arrange_.tbl_root <- function(.data, ..., .dots) {
  stop('arrange is not supported by ROOT tables', call. = FALSE)
}

#' @export
select_.tbl_root <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- select_vars_(tbl_vars(.data), dots,
                       include = as.character(groups(.data)))
  
  .data$vars <- setNames(.data$vars[vars], names(vars))
  .data
}

#' @export
rename_.tbl_root <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- rename_vars_(tbl_vars(.data), dots)
  
  .data$vars <- setNames(.data$vars[vars], names(vars))
  .data
}

#' @export
summarise_.tbl_root <- function(.data, ..., .dots) {
  stop('summarise is not supported by ROOT tables', call. = FALSE)
#   .data <- collect(.data)
#   summarise_(.data, ..., .dots)
}

#' @export
mutate_.tbl_root <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  input <- partial_eval(dots, .data)
  
  for (i in seq_along(input)) {
    expr <- input[[i]]
    env <- root_env(expr, .data$vars)
    evaluated <- eval(expr, envir = env)
    .data$vars <- c(.data$vars, setNames(list(evaluated), names(input)[[i]]))
  }
  
  .data
}

#' @export
group_by_.tbl_root <- function(.data, ..., .dots, add = FALSE) {
  stop('group_by is not supported by ROOT tables', call. = FALSE)
}

# Copying ----------------------------------------------------------------------

#' @export
collapse.tbl_root <- function(x, ...) {
  x
}

#' @export
compute.tbl_root <- function(x, ...) {
  x
}

#' @export
collect.tbl_root <- function(x, n = NULL, ...) {
  vars <- translate_root_q(x$vars, x, env = NULL)
  selections <- translate_root_q(x$selection, x, env = NULL)
  selection <- if (length(selections) > 0)
    paste0('(', selections, ')', collapse=' && ')
  else
    ''
  if (is.null(n) || n < 0) n <- 0L
  
  data <- RootTreeToR::toR(x$tree, vars, selection, nEntries=nEntries(x$tree), maxSize=n)
  names(data)[1:length(vars)] <- names(vars)
  
  grouped_df(data, groups(x))
}