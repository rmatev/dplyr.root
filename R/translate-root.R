#' Translate an expression to ROOT's TTree::Draw syntax.
#'

#' @export
translate_root <- function(..., tbl = NULL, env = parent.frame(), variant = NULL,
                          window = FALSE) {
  translate_root_q(dplyr:::dots(...), tbl = tbl, env = env)
}

#' @export
#' @rdname translate_root
translate_root_q <- function(expr, tbl = NULL, env = parent.frame()) {
  stopifnot(is.null(tbl) || inherits(tbl, "tbl_root"))
  if (is.null(expr)) return(NULL)
  
  # If environment not null, and tbl supplied, partially evaluate input
  if (!is.null(env) && !is.null(tbl)) {
    expr <- dbplyr::partial_eval(expr, tbl, env)
  }

  pieces <- lapply(expr, function(x) {
    if (is.atomic(x)) return(escape_rootexpr(x))
    
    env <- root_env(x)
    eval(x, envir = env)
  })
  
  unlist(pieces)
}

root_env <- function(expr, vars = NULL) {
  default_env <- new.env(parent = emptyenv())
  
  # Known R -> ROOT functions
  special_calls <- dbplyr:::copy_env(root_scalar, parent = default_env)
  
  # Existing symbols in expression
  names <- dbplyr:::all_names(expr)
  name_env <- dbplyr:::ceply(names, function(x) escape_rootexpr(rootexpr_ident(x)), parent = special_calls)
  
  # Existing variables
  context_env <- if (length(vars) == 0) {
    new.env(parent = name_env)
  } else {
    l <- lapply(vars, function(x) {
      stopifnot(is.rootexpr(x))
      rootexpr(paste0('(', escape_rootexpr(x), ')'))
    })
    list2env(l, parent = name_env)
  }
  
  # Known ROOT expressions
  symbol_env <- dbplyr:::copy_env(root_symbols, parent = context_env)
  symbol_env
}
