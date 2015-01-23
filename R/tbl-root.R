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
  print(trunc_mat(x, n = n, width = width))
  invisible(x)
}

#' @export
dimnames.tbl_root <- function(x) {
  list(NULL, tbl_vars.tbl_root(x))
}

#' @export
dim.tbl_root <- function(x) {
  if (length(x$selection) == 0) {
    n <- RootTreeToR::nEntries(if (is.null(x$elist)) x$tree else x$elist)
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
  if (length(x$selection) == 0)
    return(x)
  
  selections <- translate_root_q(x$selection, x, env = NULL)
  selection <- if (length(selections) > 0) paste0('(', selections, ')', collapse=' && ') else ''
  
  name <- paste('entrylist', sub('\\.', '_', sprintf('%.6f', Sys.time())), paste(sample(letters, 8), collapse=''), sep='_')
  st <- system.time({
    elist <- RootTreeToR::makeEventList(x$tree, name=name, selection, nEntries=1000000000, entryList=T)
  })
  if (st[3] > 1.0) {
    message(sprintf('selection was collapsed in %.1f s (user %.1f s, sys %.1f s)', st[3], st[1], st[2]))
  }
  
  x[c('elist', 'selection')] <- list(elist, NULL)
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
  selection <- if (length(selections) > 0) paste0('(', selections, ')', collapse=' && ') else ''
  if (is.null(n) || n < 0) n <- 0L
  
  pattern <- paste(names(RootTreeToR::getNames(x$tree)), collapse='|')
  needed_vars <- lapply(c(vars, selections), function(x) regmatches(x, gregexpr(pattern, x)))
  needed_vars <- unique(unlist(needed_vars))
  
  if (!is.null(x$elist))
    narrowWithEntryList(x$tree, x$elist)  # TODO chain might be shared between tables, which makes concurent execution impossible
  
  if (n > 0 && n * length(vars) <= getOption('max.print')) {
    initial_size <- n
    st1 <- 0
  } else {
    st1 <- system.time({
      n_selected <- RootTreeToR::nEntries(x$tree, selection)
    })
    initial_size <- if (n == 0) n_selected else min(n_selected, n)
  }

  st2 <- system.time({
    data <- RootTreeToR::toR(x$tree,
                             vars, selection,
                             nEntries=1000000000, # TODO
                             initialSize=max(initial_size, 1),
                             maxSize=n,
                             activate=needed_vars
                             )
  })
  names(data)[1:length(vars)] <- names(vars)
  
  if (!is.null(x$elist))
    clearEntryList(x$tree)

  data <- grouped_df(data, groups(x))
  
  st <- st1 + st2
  if (st[3] > 1.0) {
    message(sprintf('data was retrieved in %.1f s (user %.1f s, sys %.1f s)', st[3], st[1], st[2]))
  }
  
  data
}
