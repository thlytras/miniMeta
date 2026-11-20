#' Parse arguments from a comma-separated list
#'
#' Read a comma-separated list of arguments (as a character string),
#' parse them, and return as a named R list. This function is used in
#' miniMeta to parse arguments for forest.meta() when given as a string.
#'
#' The string is parsed but never evaluated as arbitrary R code: only
#' literal values are accepted (numbers, strings, \code{TRUE}/\code{FALSE},
#' \code{NA}, \code{NULL}, \code{Inf}, \code{NaN}), optionally combined with
#' \code{c()} or \code{list()}, and unary minus/plus on numbers. Anything else
#' (function calls, variables, other operators) is rejected.
#'
#' @param x A character vector (of length one) containing the arguments.
#'     All should be named.
#'
#' @return A named list of arguments, or an object of class "try-error" on failure.
#'
#' @examples
#' parseArguments('col.diamond="red", sm="RR", common=FALSE')
#'
#' @export
parseArguments <- function(x) {
  fail <- function(msg) {
    res <- try(stop(msg, call.=FALSE), silent=TRUE)
    attr(res, "condition")$call <- call('getArgs')
    res
  }
  if (is.null(x) || length(x)==0) return(list())
  x <- paste(x, collapse=" ")
  if (!nzchar(trimws(x))) return(list())

  # Parse (do NOT evaluate). str2lang() requires exactly one expression,
  # so input such as 'a=1), b=system("id")' is a syntax error here.
  ex <- try(str2lang(sprintf("getArgs(%s)", x)), silent=TRUE)
  if (inherits(ex, "try-error")) return(ex)
  if (!is.call(ex) || !identical(ex[[1]], as.name("getArgs"))) {
    return(fail("Invalid argument list"))
  }
  args <- as.list(ex)[-1]

  if (length(args)>0 && (is.null(names(args)) || any(names(args)==""))) {
    return(fail("All provided arguments should be named"))
  }

  okSymbols <- c("Inf", "NaN", "NA_integer_", "NA_real_", "NA_character_", "T", "F")
  isLiteral <- function(e) {
    if (is.null(e)) return(TRUE)
    if (is.atomic(e)) return(length(e)==1)
    if (is.name(e)) return(as.character(e) %in% okSymbols)
    if (!is.call(e) || !is.name(e[[1]])) return(FALSE)
    fn <- as.character(e[[1]])
    a <- as.list(e)[-1]
    if (fn %in% c("c", "list")) return(all(vapply(a, isLiteral, logical(1))))
    if (fn %in% c("-", "+") && length(a)==1) {
      return(is.numeric(a[[1]]) || (is.name(a[[1]]) && as.character(a[[1]]) %in% c("Inf", "NaN")))
    }
    if (fn == "(" && length(a)==1) return(isLiteral(a[[1]]))
    FALSE
  }
  bad <- names(args)[!vapply(args, isLiteral, logical(1))]
  if (length(bad)>0) {
    return(fail(sprintf(
      "Only literal values are allowed (numbers, strings, TRUE/FALSE, NA, NULL, c(...)); not accepted: %s",
      paste(bad, collapse=", "))))
  }

  # Safe to evaluate now: only literals, c(), list(), unary +/- and '(' remain,
  # and they are looked up in the base environment.
  try(lapply(args, function(e) if (is.null(e)) NULL else eval(e, envir=baseenv())), silent=TRUE)
}
