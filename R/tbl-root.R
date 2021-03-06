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
group_vars.tbl_root <- function(x) {
  NULL
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
#   if (!is.null(n))
#     n <- min(n, as.integer(0.9*getOption('max.print')/length(x$vars)))
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
filter.tbl_root <- function(.data, ..., .dots) {
  dots <- quos(...)
  input <- dbplyr:::partial_eval_dots(dots, vars = tbl_vars(.data))

  evaluated <- lapply(input, function(expr) {
    env <- root_env(expr, .data$vars)
    eval(get_expr(expr), envir = env)
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
mutate.tbl_root <- function(.data, ..., .dots = list()) {
  dots <- quos(..., .named = TRUE)
  input <- dbplyr:::partial_eval_dots(dots, vars = tbl_vars(.data))
  
  for (i in seq_along(input)) {
    expr <- input[[i]]
    env <- root_env(expr, .data$vars)
    evaluated <- eval(get_expr(expr), envir = env)
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
  
  name <- paste('entrylist',
                sub('\\.', '_', sprintf('%.6f', Sys.time())),
                paste(sample(letters, 8), collapse=''), sep='_')
  st <- system.time({
    elist <- RootTreeToR::makeEventList(x$tree, name=name, selection,
                                        nEntries=1000000000, entryList=T)
  })
  if (st[3] > 1.0) {
    message(sprintf('selection was collapsed in %.1f s (user %.1f s, sys %.1f s)',
                    st[3], st[1], st[2]))
  }
  
  x[c('elist', 'selection')] <- list(elist, NULL)
  x
}

#' @export
compute.tbl_root <- function(x, ...) {
  x
}

#' @export
collect.tbl_root <- function(x, n = NULL, protect = is.null(n), hint = NA, ...) {
  vars <- translate_root_q(x$vars, x, env = NULL)
  selections <- translate_root_q(x$selection, x, env = NULL)
  selection <- if (length(selections) > 0) paste0('(', selections, ')', collapse=' && ') else ''
  
  pattern <- paste(names(RootTreeToR::getNames(x$tree)), collapse='|')
  needed_vars <- lapply(c(vars, selections), function(x) regmatches(x, gregexpr(pattern, x)))
  needed_vars <- unique(unlist(needed_vars))
  
  if (!is.null(x$elist)) {
    RootTreeToR::narrowWithEntryList(x$tree, x$elist)
    # TODO chain might be shared between tables, which makes concurent execution impossible
  }
  
  st1 <- 0
  if (!is.null(hint) && is.finite(hint) && hint > 0) {
    initial_size <- hint
  } else if (!is.null(n)) {
    initial_size <- n
  } else {
    n_selected <- nrow(x)
    if (!is.finite(n_selected)) {
      st1 <- system.time({
        n_selected <- RootTreeToR::nEntries(x$tree, selection)
      })
      if (st1[3] > 1.0) {
        message(sprintf('number of rows was determined in %.1f s (user %.1f s, sys %.1f s)',
                        st1[3], st1[1], st1[2]))
      }
    }
    initial_size <- n_selected
  }

  # TODO do not assume all columns are doubles
  mem_estimate <- 8 * as.numeric(initial_size) * length(vars)
  if (mem_estimate > 2*2^30) {
    warning(sprintf('Collected data will amount to about %.1f GB', mem_estimate / 2^30))
    if (protect) stop('If this is intended, use collect(..., protect = F)', call.=F)
  }
  
  st2 <- system.time({
    data <- RootTreeToR::toR(x$tree,
                             vars, selection,
                             nEntries=1000000000, # TODO
                             initialSize=max(initial_size, 1),
                             maxSize=(if (is.null(n) || n < 0) 0L else n),
                             activate=needed_vars,
                             ...
                             )
  })
  names(data)[1:length(vars)] <- names(vars)
  # TODO there is a bug in RootTreeToR::toR, workaround follows
  if (!length(data[[1]])) {
    attr(data, 'row.names') <- integer(0)
  }
  
  if (!is.null(x$elist))
    RootTreeToR::clearEntryList(x$tree)

  data <- as_tibble(data, groups(x))
  
  st <- st1 + st2
  if (st[3] > 1.0) {
    message(sprintf('data (%.0f MB) was retrieved in %.1f s (user %.1f s, sys %.1f s)',
                    object.size(data)/1024^2, st[3], st[1], st[2]))
  }
  
  data
}
