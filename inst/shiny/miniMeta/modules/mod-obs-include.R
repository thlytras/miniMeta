
except <- function(x, y) {
  x[!(x %in% y)]
}


gradeObs <- function(m, bR) {
  lim <- function(x) {
    x[x<0] <- 0
    x[x>1000] <- 1000
    x
  }
  ef <- with(m, c(TE.random, lower.random, upper.random))
  uR <- bR*10
  sg <- function(x) sprintf("%.0f %s", abs(x), c("fewer", "more")[as.integer(x>=0)+1])
  if (m$sm=="RR") {
    eR <- lim(uR * exp(ef))
    a <- sprintf("RR, %.2f (%.2f – %.2f)", exp(ef[1]), exp(ef[2]), exp(ef[3]))
  } else if (m$sm=="OR") {
    ORtoP <- function(o) o / (o+1)
    eR <- lim(ORtoP(uR/(1000-uR) * exp(ef))*1000)
    a <- sprintf("OR, %.2f (%.2f – %.2f)", exp(ef[1]), exp(ef[2]), exp(ef[3]))
  } else if (m$sm=="RD") {
    eR <- lim(uR + ef*1000)
    a <- sprintf("RD, %.2f (%.2f – %.2f)", ef[1], ef[2], ef[3])
  } else { # Arcsine risk difference
    eR <- (sin(asin(sqrt(uR/1000)) + ef)^2)*1000
    a <- sprintf("ASD, %.2f (%.2f – %.2f)", ef[1], ef[2], ef[3])
  }
  rdI <- eR-uR; rdI[2:3] <- rdI[2:3][order(abs(rdI[2:3]))]
  a <- c(a, 
        sprintf("%.0f per 1000", uR),
        sprintf("%.0f per 1000", eR[1]),
        sprintf("%.0f per 1000", uR),
        sprintf("%s per 1000 (from %s to %s)", sg(rdI[1]), sg(rdI[2]), sg(rdI[3]))
    )[c(2,3,1,4,5)]
  a <- data.frame("Results"=a)
  rownames(a) <- c("Event rate (unexposed)", "Event rate (exposed)", "Relative effect", "Risk in unexposed", "RD with exposure")
  a
}

