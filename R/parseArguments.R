#' Parse arguments from a comma-separated list
#'
#' Read a comma-separated list of arguments (as a character string), 
#' parse them, and return as a named R list. This function is used in
#' miniMeta to parse arguments for forest.meta() when given as a string.
#'
#' @param x A character vector (of length one) containing the arguments.
#'     All should be named.
#'
#' @return A named list of arguments, or an object of class "try-error" on failure.
#'
#' @examples
#' parseArguments('col.diamond="red", sm="RR", comb.fixed=FALSE')
#'
#' @export
parseArguments <- function(x) {
  getArgs <- function(...) return(list(...))
  x <- gsub(";|\n", "", x)
  res <- try(eval(parse(text=sprintf("getArgs(%s)", x))), silent=TRUE)
  if (class(res)=="try-error") {
    return(res)
  }
  if (length(res)>0 && (is.null(names(res)) || (sum(names(res)=="")>0))) {
    res <- try(stop("All provided arguments should be named"), silent=TRUE)
    attr(res, "condition")$call <- call('getArgs')
    return(res)
  }
  return(res)
}

