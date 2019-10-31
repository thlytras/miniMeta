checkRCTValidity <- function(rctDAT) {
  msg <- list()
  if (nrow(rctDAT)==0) {
    msg <- c(msg, "Empty data -- cannot perform meta-analysis")
  } else {
    if (sum(is.na(rctDAT[,-1]))>0) msg <- c(msg, "Blank cells not allowed.")
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


getNonEmptyDFrows <- function(dat, ignore.studlab=FALSE) {
  if (ignore.studlab) {
    apply(dat[,-1], 1, function(x) !sum(is.na(unlist(x)))==4)
  } else {
    apply(dat, 1, function(x) (sum(is.na(unlist(x))) + sum(unlist(x)=="", na.rm=TRUE))<5)
  }
}


