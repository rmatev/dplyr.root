#' Connect to a root file.
#'
#' Use \code{src_root} to connect to an existing root file,
#' and \code{tbl} to connect to tables within that database.
#' If you are running a local sqliteql database, leave all parameters set as
#' their defaults to connect. If you're connecting to a remote database,
#' ask your database administrator for the values of these variables.
#'
#' @param path Path to SQLite database
#' @param create if \code{FALSE}, \code{path} must already exist. If
#'   \code{TRUE}, will create a new SQlite3 database at \code{path}.
#' @param src a sqlite src created with \code{src_sqlite}.
#' @param from Either a string giving the name of table in database, or
#'   \code{\link{sql}} described a derived table or compound join.
#' @param ... Included for compatibility with the generic, but otherwise
#'   ignored.
#' @export
#' 
src_root <- function(path) {
  if (!requireNamespace("RootTreeToR", quietly = TRUE)) {
    stop("RootTreeToR package required to work with root files", call. = FALSE)
  }
  
  if (!file.exists(path)) {
    stop("Path does not exist", call. = FALSE)
  }
  
  src('root', path = path)
}

#' @export
src_desc.src_root <- function(x) {
  paste0("root file at ", x$path)
}

#' @export
src_tbls.src_root <- function(x) {
  '<src_tbls.src_root() not yet implemented>'
}

#' @export
format.src_root <- function(x, ...) {
  paste0("src:  ", src_desc(x), "\n", dplyr:::wrap("tbls: ", paste0(sort(src_tbls(x)), collapse = ", ")))
}


#' @export
#' @rdname src_root
tbl.src_root <- function(src, tree_name, ...) {
  tree <- RootTreeToR::openRootChain(tree=tree_name, files=src$path, verbose=F)
    
  nms <- names(RootTreeToR::getNames(tree))
  vars <- setNames(lapply(nms, rootexpr_ident), nms)
  make_tbl(c('roottree', 'root'),
           tree = tree,
           src = src,
           tree_name = tree_name,
           vars = vars,
           selection = NULL,
           elist = NULL
           )
}

#' @export
#' @rdname src_root
tbl_rootchain <- function(tbls) {
  files <- sapply(tbls, function(tbl) paste0(tbl$src$path, '/', tbl$tree_name))
  tree <- RootTreeToR::openRootChain(tree='', files=files, verbose=F)

  nms <- names(RootTreeToR::getNames(tree))
  vars <- setNames(lapply(nms, rootexpr_ident), nms)
  make_tbl(c('rootchain', 'root'),
           tree = tree,
           vars = vars,
           selection = NULL,
           elist = NULL
           )
}
