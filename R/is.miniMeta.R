#' Is this a miniMeta object?
#'
#' This function checks whether this is a valid miniMeta object
#'
#' @param x An object of class \code{miniMeta}
#'
#' @return \code{TRUE} if it is a valid miniMeta object, \code{FALSE} if it is not.
#'
#' @import meta
#' 
#' @examples
#' is.miniMeta(example_miniMeta_obs) # returns TRUE
#' is.miniMeta(example_miniMeta_rct) # returns TRUE
#'
#' @export
is.miniMeta <- function(x) {
  res <- c(
    inherits(x, "list"),
    c("data", "meta", "analysisOptions", "plotOptions") %in% names(x)
  )
  return(sum(!res)==0)
}


#' Is this a miniMeta object for RCTs?
#'
#' This function checks whether this is a valid miniMeta object holding a
#' a meta-analysis of Randomized Controlled Trials (RCTs).
#'
#' @param x An object of class \code{miniMeta}
#'
#' @return \code{TRUE} if it is a valid miniMeta object holding a meta-analysis 
#' of Randomized Controlled Trials (RCTs), \code{FALSE} if it is not.
#'
#' @import meta
#' 
#' @examples
#' is.miniMeta.rct(example_miniMeta_obs) # returns FALSE
#' is.miniMeta.rct(example_miniMeta_rct) # returns TRUE
#'
#' @export
is.miniMeta.rct <- function(x) {
  if (!is.miniMeta(x)) return(FALSE)
  res <- c(inherits(x$meta, "metabin"))
  return(sum(!res)==0)
}


#' Is this a miniMeta object for observational studies?
#'
#' This function checks whether this is a valid miniMeta object holding a
#' a meta-analysis of observational studies.
#'
#' @param x An object of class \code{miniMeta}
#'
#' @return \code{TRUE} if it is a valid miniMeta object holding a meta-analysis 
#' of observational studies, \code{FALSE} if it is not.
#'
#' @import meta
#' 
#' @examples
#' is.miniMeta.obs(example_miniMeta_obs) # returns TRUE
#' is.miniMeta.obs(example_miniMeta_rct) # returns FALSE
#'
#' @export
is.miniMeta.obs <- function(x) {
  if (!is.miniMeta(x)) return(FALSE)
  res <- c(inherits(x$meta, "metagen"))
  return(sum(!res)==0)
}
