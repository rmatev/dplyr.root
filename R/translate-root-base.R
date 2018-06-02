
rootexpr_prefix <- function (f, n = NULL) {
  assert_that(is.string(f))
  function(..., na.rm) {
    if (!missing(na.rm)) {
      stop("na.rm does not work in ROOT", call. = FALSE)
    }
    args <- list(...)
    if (!is.null(n) && length(args) != n) {
      stop("Invalid number of args to ROOT expression function ", f, ". Expecting ", 
           n, call. = FALSE)
    }
    if (any(names2(args) != "")) {
      warning("Named arguments ignored for ROOT expression ", f, call. = FALSE)
    }
    args <- lapply(args, escape_rootexpr)
    x <- list(rootexpr(f), '(', paste0(args, collapse=', '), ')')
    rootexpr(do.call(paste0, x))
  }
}

rootexpr_infix <- function(f) {
  assert_that(is.string(f))
  function(x, y) {
    rootexpr(paste0(escape_rootexpr(x), " ", f, " ", escape_rootexpr(y)))
  }
}

root_symbols <- dbplyr::sql_translator(
  pi     = rootexpr('pi')
)

root_scalar <- dbplyr::sql_translator(
  `+`    = rootexpr_infix("+"),
  `*`    = rootexpr_infix("*"),
  `/`    = rootexpr_infix("/"),
  `%%`   = rootexpr_prefix("fmod", 2),
  `^`    = rootexpr_infix("^"),
  `-`    = function(x, y = NULL) {
    if (is.null(y)) {
      rootexpr(paste0(" - ", escape_rootexpr(x)))
    } else {
      rootexpr(paste0(escape_rootexpr(x), " - ", escape_rootexpr(y)))
    }
  },
  
  `!=`    = rootexpr_infix("!="),
  `==`    = rootexpr_infix("=="),
  `<`     = rootexpr_infix("<"),
  `<=`    = rootexpr_infix("<="),
  `>`     = rootexpr_infix(">"),
  `>=`    = rootexpr_infix(">="),
  
  `!`     = rootexpr_prefix("!"),
  `&`     = rootexpr_infix("&&"),
  `&&`    = rootexpr_infix("&&"),
  `|`     = rootexpr_infix("||"),
  `||`    = rootexpr_infix("||"),
  xor     = function(x, y) {
    rootexpr(sprintf("((%1$s) || (%2$s)) && !((%1$s) && (%2$s))", escape_rootexpr(x), escape_rootexpr(y)))
  },
  `%in%`  = function(x, table) {
    table <- unique(table)
    if (length(table) == 0) return(rootexpr("0"))
    x <- escape_rootexpr(x)
    table <- lapply(table, escape_rootexpr)
    expr <- paste(sprintf("(%s == %s)", x, table), collapse = ' || ')
    rootexpr(sprintf("(%s)", expr))
  },
  
#   bitwNot(a)
#   bitwAnd(a, b)
#   bitwOr(a, b)
#   bitwXor(a, b)
#   
#   bitwShiftL(a, n)
#   bitwShiftR(a, n)
    
  abs     = rootexpr_prefix("abs", 1),
  acos    = rootexpr_prefix("acos", 1),
  acosh   = rootexpr_prefix("acosh", 1),
  asin    = rootexpr_prefix("asin", 1),
  asinh   = rootexpr_prefix("asinh", 1),
  atan    = rootexpr_prefix("atan", 1),
  atan2   = rootexpr_prefix("atan2", 2),
  atanh   = rootexpr_prefix("atanh", 1),
#   ceil    = rootexpr_prefix("ceil", 1),
#   ceiling = rootexpr_prefix("ceil", 1),
  cos     = rootexpr_prefix("cos", 1),
  cosh    = rootexpr_prefix("cosh", 1),
#   cot     = rootexpr_prefix("cot", 1),
#   coth    = rootexpr_prefix("coth", 1),
  exp     = rootexpr_prefix("exp", 1),
#   floor   = rootexpr_prefix("floor", 1),
  fmod    = rootexpr_prefix("fmod", 2),
  log     = rootexpr_prefix("log", 1),
  log10   = rootexpr_prefix("log10", 1),
  pmin    = rootexpr_prefix("min", 2),
  pmax    = rootexpr_prefix("max", 2),
#   round   = rootexpr_prefix("round", 2),
  sign    = rootexpr_prefix("sign", 1),
  sin     = rootexpr_prefix("sin", 1),
  sinh    = rootexpr_prefix("sinh", 1),
  sqrt    = rootexpr_prefix("sqrt", 1),
  tan     = rootexpr_prefix("tan", 1),
  trunc   = rootexpr_prefix("int", 1),
  
#   tolower = rootexpr_prefix("lower", 1),
#   toupper = rootexpr_prefix("upper", 1),
#   nchar   = rootexpr_prefix("length", 1),
  
  `if` = function(cond, if_true, if_false) {
    rootexpr(sprintf("(%s)?(%s):(%s)", escape_rootexpr(cond), escape_rootexpr(if_true), escape_rootexpr(if_false)))
  },
  
  rootexpr = function(...) rootexpr(...),
  `(` = function(x) {
    rootexpr(sprintf("(%s)", escape_rootexpr(x)))
  },
  `{` = function(x) {
    rootexpr(sprintf("(%s)", escape_rootexpr(x)))
  },
  
#   as.numeric = function(x) build_sql("CAST(", x, " AS NUMERIC)"),
  as.integer = rootexpr_prefix("int", 1),
#   as.character = function(x) build_sql("CAST(", x, " AS TEXT)"),
  
#   c = function(...) escape(c(...)),
#   `:` = function(from, to) escape(from:to),

  between = function(x, left, right) {
    rootexpr(sprintf("(%1$s <= %2$s) && (%2$s < %3$s)", left, x, right))
  }

#   between = function(x, left, right) {
#     build_sql(x, " BETWEEN ", left, " AND ", right)
#   }
)
