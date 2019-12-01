
except <- function(x, y) {
  x[!(x %in% y)]
}


checkRCTValidity <- function(rctDAT) {
  msg <- list()
  if (nrow(rctDAT)==0) {
    msg <- c(msg, "Empty data -- cannot perform meta-analysis")
  } else {
    if (sum(is.na(rctDAT[,2:5]))>0) msg <- c(msg, "Blank cells not allowed.")
    a <- with(rctDAT, which(e.e>n.e | e.c>n.c))
    if (length(a)>0) msg <- c(msg, paste("Number of events cannot be higher than the number randomized.\n   Check studies: '", paste(rctDAT[a,1], collapse="', '"), "'.", sep=""))
    a <- with(rctDAT, which(n.e==0 | n.c==0))
    if (length(a)>0) msg <- c(msg, paste("Number randomized cannot be zero.\n   Check studies: '", paste(rctDAT[a,1], collapse="', '"), "'.", sep=""))  
  }
  res <- TRUE
  if (length(msg)>0) {
    res <- FALSE
    attr(res, "msg") <- msg
  }
  return(res)
}


gradeRR <- function(dat, m) {
  ef <- exp(with(m, c(TE.random, lower.random, upper.random)))
  rd <- sum(dat[,3])*1000/sum(dat[,4]) * (ef-1)
  rd[3] <- min(rd[3], 1000*(1-sum(dat[,3])/sum(dat[,4])))
  sg <- function(x) sprintf("%.0f %s", abs(x), c("fewer", "more")[as.integer(x>=0)+1])
  a <- c(sprintf("%s/%s (%.1f%%)", sum(dat[,3]), sum(dat[,4]), sum(dat[,3])*100/sum(dat[,4])),
    sprintf("%s/%s (%.1f%%)", sum(dat[,1]), sum(dat[,2]), sum(dat[,1])*100/sum(dat[,2])),
    sprintf("RR, %.2f (%.2f – %.2f)", ef[1], ef[2], ef[3]),
    sprintf("%s per 1000", round(sum(dat[,3])*1000/sum(dat[,4]))),
    sprintf("%s per 1000 (from %s to %s)", sg(rd[1]), sg(rd[2]), sg(rd[3])))
  a <- data.frame("Results"=a)
  rownames(a) <- c("Event rate (control)", "Event rate (intervention)", "Relative effect", "Risk with control", "RD with intervention")
  a
}



readAdvParameters <- function(x) {
  getArgs <- function(...) return(list(...))
  x <- gsub(";|\n", "", x)
  pars <- try(eval(parse(text=sprintf("getArgs(%s)", x))), silent=TRUE)
  if (class(pars)=="try-error") {
    return(pars)
  }
  if (length(pars)>0 && (is.null(names(pars)) || (sum(names(pars)=="")>0))) {
    pars <- try(stop("All provided arguments should be named"), silent=TRUE)
    attr(pars, "condition")$call <- call('getArgs')
    return(pars)
  }
  return(pars)
}


