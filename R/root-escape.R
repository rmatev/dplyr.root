#' ROOT escaping.
#'
#' These functions are critical when writing functions that translate R
#' functions to ROOT functions. Typically a conversion function should escape
#' all it's inputs and return an sql object.
#'
#' @export
rootexpr <- function(x) {
  structure(x, class = c("rootexpr", "character"))
}

#' @export
rootexpr_ident <- function(x) {
  if (is.null(x)) return()
  if (is.ident(x)) return(x)
  
  structure(x, class = c("ident", "rootexpr", "character"))
}

setOldClass(c("rootexpr", "character"))
setOldClass(c("ident", "rootexpr", "character"))

#' @export
is.rootexpr <- function(x) inherits(x, "rootexpr")

#' @export
print.rootexpr <- function(x, ...) cat(format(x, ...), sep = "\n")
#' @export
format.rootexpr <- function(x, ...) paste0("<ROOT expression> ", x)

#' @export
escape_rootexpr <- function(x) {
  if (length(x) > 1)
    stop('Vectors not supported in ROOT expressions.', call. = FALSE)
  UseMethod("escape_rootexpr")
}

#' @export
escape_rootexpr.rootexpr <- function(x) {
  x
}

#' @export
escape_rootexpr.logical <- function(x) {
  rootexpr(as.character(as.integer(x)))
}

#' @export
escape_rootexpr.factor <- function(x) {
  escape_rootexpr.character(as.character(x))
}

#' @export
escape_rootexpr.character <- function(x) {
  y <- paste0('"', as.character(x), '"')
  rootexpr(y)
}

#' @export
escape_rootexpr.double <- function(x) {
  rootexpr(as.character(x))
}

#' @export
escape_rootexpr.integer <- function(x) {
  rootexpr(as.character(x))
}
