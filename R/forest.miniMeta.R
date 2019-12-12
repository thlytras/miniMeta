#' Forest plot for miniMeta objects
#'
#' Draws a forest plot for a miniMeta object using the options 
#' stored in the object
#'
#' @param x An object of class \code{miniMeta}
#'
#' @return NULL
#'
#' @export
forest.miniMeta <- function(x) {
  if (!is.miniMeta(x)) stop("`x` should be a miniMeta object")
  do.call(forest.meta, c(list(x=x$meta), plotOptions(x)))
  invisible()
}
