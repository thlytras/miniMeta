#' Get forest plot options from miniMeta object
#'
#' This function returns the forest plot options stored in a miniMeta 
#' object, as a named list of arguments, for further processing.
#' This allows finer control than directly plotting using the 
#' \code{\link{forest.miniMeta}} method. See the example below.
#'
#' @param x An object of class \code{miniMeta}
#' 
#' @return A named list of arguments corresponding to the arguments of 
#' \code{\link[meta]{forest.meta}}.
#' 
#' @importFrom methods formalArgs
#' 
#' @examples 
#' \donttest{
#' # Extract the plot options from the miniMeta object
#' plot_opts <- plotOptions(example_miniMeta_obs)
#' # Call directly the forest.meta method, with all plot options
#' do.call(forest, c(x=list(example_miniMeta_obs$meta), plot_opts))
#' 
#' # Equivalently, call the forest.miniMeta method directly
#' forest(example_miniMeta_obs)
#' }
#'
#' @export
plotOptions <- function(x) {
  if (!is.miniMeta(x)) stop("`x` should be a miniMeta object")
  res <- x$plotOptions
  oMat <- c("printI2" = "print.I2", "printQ" = "print.Q", 
    "printPval" = "print.pval.Q", "printTau2" = "print.tau2",
    "diamCol" = "col.diamond", "barCol" = "col.study", "sqCol" = "col.square"
    )
  names(res)[names(res) %in% names(oMat)] <- unname(oMat[names(res)[names(res) %in% names(oMat)]])
  res$leftcols <- "studlab"
  if (is.miniMeta.rct(x)) {
    if (res$inclAbsNum) res$leftcols <- c(res$leftcols, 
        "event.e", "n.e", "event.c", "n.c")
    res$inclAbsNum <- NULL
  }
  res$rightcols <- c("effect", "ci")
  if (res$showWeights) {
    if (x$meta$comb.fixed) res$rightcols <- c(res$rightcols, "w.fixed")
    if (x$meta$comb.random) res$rightcols <- c(res$rightcols, "w.random")
  }
  res$text.fixed <- "Fixed-effects model"
  res$text.random <- "Random-effects model"
  res$col.diamond.lines <- res$col.diamond
  res$fontsize <- res$pointsize
  res$plotwidth <- sprintf("%.2fcm", 8*res$pointsize/12)
  res$colgap <- sprintf("%.2fmm", 2*res$pointsize/12)
  res$fileType <- res$width <- res$height <- res$pointsize <- res$res <- NULL
  adv <- parseArguments(res$advParInput)
  res$advParInput <- NULL
  forest_args <- formalArgs(meta::forest.meta)
  forest_args <- forest_args[!(forest_args %in% 
      c("...", "x", "comb.random", "comb.fixed", "layout", "new"))]
  if (class(adv)!="try-error" && length(adv)>0) {
    adv <- adv[names(adv) %in% forest_args]
    res <- rev(c(res,adv))
    res <- rev(res[!duplicated(names(res))])
  }
  res <- res[names(res) %in% forest_args]
  res <- res[forest_args[forest_args %in% names(res)]]
  return(res)
}
