#' Get analytical options from miniMeta object
#'
#' This function returns the analytical options stored in a miniMeta object,
#' as a named list of arguments, for further processing.
#'
#' @param x An object of class \code{miniMeta}
#' @param data If \code{TRUE}, the data associated with the meta-analysis
#' are included in the return value. This allows you for example to run:
#' \code{do.call(metabin, analysisOptions(x, data=TRUE))} or
#' \code{do.call(metagen, analysisOptions(x, data=TRUE))} 
#' (depending on the contents of x), in order to re-run the meta-analysis.
#'
#'
#' @return A named list of arguments corresponding to the arguments of 
#' \code{\link[meta]{metagen}} or \code{\link[meta]{metabin}}.
#'
#' @export
analysisOptions <- function(x, data=FALSE) {
  if (!is.miniMeta(x)) stop("`x` should be a miniMeta object")
  res <- x$analysisOptions
  oMat <- c("combFixed" = "comb.fixed", "combRandom" = "comb.random", 
    "methodTau" = "method.tau")
  names(res)[names(res) %in% names(oMat)] <- unname(oMat[names(res)[names(res) %in% names(oMat)]])
  if (("incr" %in% names(res)) && res$incr != "TACC") res$incr <- as.numeric(res$incr)
  if (data) {
    if (is.miniMeta.rct(x)) {
      res$event.e <- m$meta$event.e
      res$n.e <- m$meta$n.e
      res$event.c <- m$meta$event.c
      res$n.c <- m$meta$n.c
    } else {
      res$TE <- m$meta$TE
      res$seTE <- m$meta$seTE
    }
    grp <- trimws(as.character(m$data$group)); grp[grp==""] <- NA
    if (sum(is.na(grp))==0 & length(unique(grp))>1) {
      res$byVar <- factor(grp)
    }
  }
  return(res)
}
