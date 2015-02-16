#' Connect to a ROOT file.
#'
#' Use \code{src_root} to open an existing ROOT file,
#' and \code{tbl} to open trees within that ROOT file.
#' Use \code{tbl_rootchain} to open a "chain", i.e. bind
#' multiple trees from one or many files.
#'
#' @param path Path to ROOT file.
#' @param ... Included for compatibility with the generic, but otherwise
#'   ignored.
#' @param src A ROOT src created with \code{src_root}.
#' @param tree_name Name (and path) to the tree in the ROOT file.
#'   Vector is recycled if used in \code{tbl_rootchain}.
#' @param tbls Either a list of ROOT trees created with \code{tbl}, or
#'   a character vector of file names.
#'
#' @export
src_root <- function(path, ...) {
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
tbl_rootchain <- function(tbls, tree_name) {
  if (is.list(tbls)) {
    chain_desc <- data.frame(stringsAsFactors = F,
      file = sapply(tbls, function(tbl) tbl$src$path),
      tree_name = sapply(tbls, function(tbl) tbl$tree_name),
    )
  } else if (is.character(tbls)) {
    chain_desc <- data.frame(stringsAsFactors = F, file = tbls, tree_name)
  } else {
    stop('Argument tbls must be list of tables or vector of filenames.', call.=F)
  }
  
  # Workaround for ROOT bug https://root.cern.ch/phpBB3/viewtopic.php?f=3&t=19131
  # https://sft.its.cern.ch/jira/browse/ROOT-7036
  if (anyDuplicated(chain_desc$file)) {
    new_names <- with(chain_desc, gsub(.Platform$file.sep, '_', file.path(file, tree_name)))
    chain_desc$symlink <- tempfile(paste0(new_names, '_'), fileext='.root')
    res <- with(chain_desc, file.symlink(file, symlink))
    if (!all(res)) stop('Could not create symlinks for some files', call.=F)
    files <- with(chain_desc, file.path(symlink, tree_name))
    # TODO how to delete these symlinks?
  } else {
    files <- with(chain_desc, file.path(file, tree_name))
  }
  
  tree <- RootTreeToR::openRootChain(tree='', files=files, verbose=F)
  
  nms <- names(RootTreeToR::getNames(tree))
  vars <- setNames(lapply(nms, rootexpr_ident), nms)
  make_tbl(c('rootchain', 'root'),
           tree = tree,
           chain_desc = chain_desc,
           vars = vars,
           selection = NULL,
           elist = NULL
           )
}
