#' Forest plot for miniMeta objects
#'
#' Draws a forest plot for a miniMeta object using the options 
#' stored in the object
#'
#' @param x An object of class \code{miniMeta}
#' @param ... Further arguments passed to or from other methods
#'
#' @return NULL
#'
#' @examples
#' forest(example_miniMeta_obs)
#' 
#' @export
forest.miniMeta <- function(x, ...) {
  if (!is.miniMeta(x)) stop("`x` should be a miniMeta object")
  do.call(forest, c(list(x=x$meta), plotOptions(x)))
  invisible()
}
