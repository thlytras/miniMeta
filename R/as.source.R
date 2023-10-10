#' Return miniMeta analysis as source code
#'
#' Returns an entire miniMeta analysis in an R source code format. 
#' This provides a basis for further processing the results exported 
#' from miniMeta, using R code, in order to perform more elaborate or
#' more specific analyses.
#'
#' @param x An object of class \code{miniMeta}
#'
#' @return A character vector of length one, containing R code that 
#' loads the data, runs the meta-analysis, and plots a forest plot. 
#' You can save this in a text file using \code{\link[base]{writeLines}}.
#'
#' @examples
#' fname <- tempfile("my_analysis", fileext = ".R")
#' fname
#' # Writes the miniMeta analysis to an R script
#' writeLines(as.source(example_miniMeta_rct), fname)
#'
#' @export
as.source <- function(x) {
  if (!is.miniMeta(x)) stop("`x` should be a miniMeta object")
  aOpts <- analysisOptions(x)
  fOpts <- plotOptions(x)
  con <- textConnection("res", "w", local=TRUE)
  dput(x$data, con)
  close(con)
  res <- c("dat <- ", res, "\n")
  if (is.miniMeta.rct(x)) {
    res <- c(res, "m <- metabin(e.e, n.e, e.c, n.c, studlab=Study, data=dat,")
  } else {
    res <- c(res, sprintf("m <- metagen(%s, seTE, studlab=Study, data=dat,",
        if (aOpts$sm %in% c("RR","OR")) "log(TE)" else "TE"))    
  }
  aOpts <- deparse(aOpts)
  aOpts[1] <- gsub("^list\\(", "    ", aOpts[1])
  res <- c(res, aOpts, "\n")
  res <- c(res, "forest(m, ")
  fOpts <- deparse(fOpts)
  fOpts[1] <- gsub("^list\\(", "    ", fOpts[1])
  res <- c(res, fOpts, "\n")
  res <- c(res, with(x$plotOptions, 
    sprintf("funnel(m, studlab=%s, pos.studlab=%s, col=\"%s\", bg=\"%s\")\n",
      showStudlab, posStudlab, ptCol, ptCol)))
  if (is.miniMeta.rct(x)) {
    res <- c(res, with(x$plotOptions, 
      sprintf("labbe(m, studlab=%s, col=\"%s\", bg=\"%s\")\n",
        showStudlab, ptCol, ptCol)))
  }
  res <- paste(res, collapse="\n")
  return(res)
}

