
#' my.plot.bcea
#'
#' Cost-effectiveness plane plot.
#' Borrowed ideas from the \code{BCEA} package.
#'
#' @param dat1 (e,c) pairs
#' @param dat2 (e,c) pairs
#' @param dat3 (e,c) pairs
#' @param dat4 (e,c) pairs
#' @param wtp Willingness-to-pay, may be a vector i.e. multiple lines
#' @param intlabels Intervention labels to use in the legend
#' @param contour Include ellipses?
#' @param LEVELS 0-1 confidence intervals for ellipse(s)
#' @param SCALEdays x-axis QALYs in days or years
#' @param SCALEcosts should cost be per person
#' @param labelLong Full point description or not
#' @param N original sample size
#'
#' @return plot
#'
#' @export
my.plot.bcea <- function(dat1,
                         dat2 = NA,
                         dat3 = NA,
                         dat4 = NA,
                         wtp = 20000,
                         intlabels,
                         contour = FALSE,
                         LEVELS = 0.95,
                         # XLIM=c(-15,5), YLIM=c(-120,50),
                         XLIM = c(-20,20),
                         YLIM = c(-100000,50000),
                         wtpNEG = NA,
                         SCALEdays = TRUE,
                         SCALEcosts = TRUE,
                         labelLong = TRUE,
                         TITLE = "",
                         N = 1){

  require(MASS)
  require(car)

  daysinyear <- 365.25

  if(is.na(dat4)){
    dat4 <- dat3
    COLS <-  c("grey", "darkgrey", "black", NA)
    PCH <- c(16, 15, 17, NA)
    LTY <- c(1,2,3,0)
  }else{
    COLS <- c("black", "black", "grey", "grey")
    PCH <- c(16, 0, 16, 0)
    LTY <- c(1,2,1,2)
  }

  dat1e <- dat1$e[,1]-dat1$e[,2]
  dat2e <- dat2$e[,1]-dat2$e[,2]
  dat3e <- dat3$e[,1]-dat3$e[,2]
  dat4e <- dat4$e[,1]-dat4$e[,2]

  dat1c <- dat1$c[,2]-dat1$c[,1]
  dat2c <- dat2$c[,2]-dat2$c[,1]
  dat3c <- dat3$c[,2]-dat3$c[,1]
  dat4c <- dat4$c[,2]-dat4$c[,1]

  bins <- 20
  z1 <- kde2d(dat1e, dat1c, n=bins)
  z2 <- kde2d(dat2e, dat2c, n=bins)
  z3 <- kde2d(dat3e, dat3c, n=bins)
  z4 <- kde2d(dat4e, dat4c, n=bins)

  par(mar=c(5.1,5.1,4.1,2.1))

  if(!SCALEdays){
    dat1e <- dat1e/daysinyear*N
    dat2e <- dat2e/daysinyear*N
    dat3e <- dat3e/daysinyear*N
    dat4e <- dat4e/daysinyear*N
    XLIM <- XLIM/daysinyear*N
    XLAB <- "Incremental QALYs"
  }else{
    XLAB <- "Incremental QALYs (days)"}

  if(!SCALEcosts){
    dat1c <- dat1c*N
    dat2c <- dat2c*N
    dat3c <- dat3c*N
    dat4c <- dat4c*N
    # YLIM <- YLIM*N/2
  }else{}

  plot(1, type="n", ylim=YLIM, xlim=XLIM,
       xlab=XLAB, ylab="Incremental costs (GBP)",
       col="black", pch=16,
       cex.axis=1.5, cex.lab=2, cex.main=1.5, main=TITLE)

  abline(v=0, col="darkgrey")
  abline(h=0, col="darkgrey")

  if(!is.na(wtpNEG)) segments(0,0, 0, 100+YLIM[1]*2, lwd=2)

  if(labelLong){labtext <- paste(intlabels[1], "vs", intlabels[-1])
  }else{
    labtext <- intlabels
  }


  legend.pos <- c(XLIM[1], YLIM[2])

  if(!contour){
    points(dat1e, dat1c, col=COLS[1], pch=PCH[1])
    points(dat2e, dat2c, col=COLS[2], pch=PCH[2])
    points(dat3e, dat3c, col=COLS[3], pch=PCH[3])
    points(dat4e, dat4c, col=COLS[4], pch=PCH[4])
    legend(x=legend.pos[1],y=legend.pos[2], legend=labtext, col=COLS[1:length(labtext)], pch=PCH[1:length(labtext)], bty = "n")#, y.intersp = 0.8, x.intersp = 0.1, text.width = 0.1)

  }else{
    dataEllipse(dat1e, dat1c, plot.points=FALSE, center.cex=0, col=COLS[1], lwd=2, lty=LTY[1], levels=LEVELS)
    dataEllipse(dat2e, dat2c, plot.points=FALSE, center.cex=0, col=COLS[2], lwd=2, lty=LTY[2], levels=LEVELS)
    dataEllipse(dat3e, dat3c, plot.points=FALSE, center.cex=0, col=COLS[3], lwd=2, lty=LTY[3], levels=LEVELS)
    dataEllipse(dat4e, dat4c, plot.points=FALSE, center.cex=0, col=COLS[4], lwd=2, lty=LTY[4], levels=LEVELS)
    ## better fitted but not 95%
    # contour(z1, drawlabels=FALSE, nlevels=2, col="black", lwd=2, lty=1, add=TRUE)
    # contour(z2, drawlabels=FALSE, nlevels=2, col="black", lwd=2, lty=2, add=TRUE)
    # contour(z3, drawlabels=FALSE, nlevels=2, col="grey", lwd=2, lty=1, add=TRUE)
    # contour(z4, drawlabels=FALSE, nlevels=2, col="grey", lwd=2, lty=2, add=TRUE)

    legend(x=legend.pos[1],y=legend.pos[2], legend=labtext, col=COLS[1:length(labtext)], lwd=c(2,2,2,2), lty=LTY[1:length(labtext)], bty = "n")
  }

  points(median(dat1e), median(dat1c), col="red", pch=16)
  points(median(dat2e), median(dat2c), col="red", pch=16)
  points(median(dat3e), median(dat3c), col="red", pch=16)
  points(median(dat4e), median(dat4c), col="red", pch=16)

  mapply(abline, a=0, b=wtp, col=c("red","blue"),  lwd=c(1,3))

}


#' My Version of the Cost-Effectiveness Acceptability Curve Plot
#'
#' Borrowed ideas from the \code{BCEA} package.
#'
#' @param dat1 Output from IDEAdectree.simple
#' @param dat2 Output from IDEAdectree.simple
#' @param dat3 Output from IDEAdectree.simple
#' @param dat4 Output from IDEAdectree.simple
#' @param intlabels Intervention labels to use in the legend
#' @param wtpNEG If no health detriment is ok. Asymptotic as wtp tends to infinity.
#' @param labelLong (TRUE)
#' @param TITLE
#' @param SCALEdays (TRUE)
#' @param SCALEcosts (TRUE)
#' @param N Original sample size (number of patients)
#' @param CI Use Binomial 95% confidence intervals, capture some model uncertainty due to simulation size
#'

#' @return plot
#'
#' @export
my.plot.ceac <-
  function(dat1,
           dat2,
           dat3,
           dat4 = NA,
           intlabels,
           wtpNEG = NA,
           labelLong = TRUE,
           TITLE = "",
           SCALEdays = TRUE,
           SCALEcosts = TRUE,
           N = 1,
           CI = FALSE) {

    stopifnot(nrow(dat1$e) == nrow(dat2$e), nrow(dat1$e) == nrow(dat3$e))

    Nrealisation <- nrow(dat1$e)

    if (is.na(dat4)) {
      dat4 <- dat3
      COLS <-  c("grey", "darkgrey", "black", NA)
      PCH <- c(16, 15, 17, NA)
      LTY <- c(1, 2, 3, 0)
    } else{
      COLS <- c("black", "black", "grey", "grey")
      PCH <- c(16, 0, 16, 0)
      LTY <- c(1, 2, 1, 2)
    }

    daysinyear <- 365

  dat1e.diff <- dat1$e[,1]-dat1$e[,2]
  dat1c.diff <- dat1$c[,2]-dat1$c[,1]

  dat2e.diff <- dat2$e[,1]-dat2$e[,2]
  dat2c.diff <- dat2$c[,2]-dat2$c[,1]

  dat3e.diff <- dat3$e[,1]-dat3$e[,2]
  dat3c.diff <- dat3$c[,2]-dat3$c[,1]

  dat4e.diff <- dat4$e[,1]-dat4$e[,2]
  dat4c.diff <- dat4$c[,2]-dat4$c[,1]

  if(!SCALEdays){
    dat1e.diff <- dat1e.diff/daysinyear*N
    dat2e.diff <- dat2e.diff/daysinyear*N
    dat3e.diff <- dat3e.diff/daysinyear*N
    dat4e.diff <- dat4e.diff/daysinyear*N
  }else{}

  if(!SCALEcosts){
    dat1c.diff <- dat1c.diff*N
    dat2c.diff <- dat2c.diff*N
    dat3c.diff <- dat3c.diff*N
    dat4c.diff <- dat4c.diff*N
  }else{}

  ##TODO##
  ## bootstrap sample
  # bootsize <- 10
  # dat1c.diff.BOOT <- dat1c.diff.BOOT <- dat1c.diff.BOOT <- dat1c.diff.BOOT <- matrix(NA, nrow=bootsize, ncol=length(dat1c.diff))
  # dat1c.diff.BOOT <- dat1c.diff.BOOT <- dat1c.diff.BOOT <- dat1c.diff.BOOT <- matrix(NA, nrow=bootsize, ncol=length(dat1c.diff))
  # for(i in 1:bootsize){
  #
  #   sboot <- sample(1:1000, replace = TRUE)
  #   dat1c.diff.BOOT[i,] <- dat1c.diff[sboot]
  #   dat1e.diff.BOOT[i,] <- dat1e.diff[sboot]
  #   dat2c.diff.BOOT[i,] <- dat2c.diff[sboot]
  #   dat2e.diff.BOOT[i,] <- dat2e.diff[sboot]
  #   dat3c.diff.BOOT[i,] <- dat3c.diff[sboot]
  #   dat3e.diff.BOOT[i,] <- dat3e.diff[sboot]
  #   dat4c.diff.BOOT[i,] <- dat4c.diff[sboot]
  #   dat4e.diff.BOOT[i,] <- dat4e.diff[sboot]
  # }

  timescale <- ifelse(SCALEdays, 365, 1)
  j <- 1
  dat1.ceac <- dat2.ceac <- dat3.ceac <- dat4.ceac <- NA
  XMAX <- 50000
  xvals <- seq(0,XMAX, by=100)

  for (i in xvals){
    WTP <- i/timescale

    if(is.na(wtpNEG)){
      dat1.ceac[j] <- sum(dat1c.diff<WTP*dat1e.diff)/Nrealisation
      dat2.ceac[j] <- sum(dat2c.diff<WTP*dat2e.diff)/Nrealisation
      dat3.ceac[j] <- sum(dat3c.diff<WTP*dat3e.diff)/Nrealisation
      dat4.ceac[j] <- sum(dat4c.diff<WTP*dat4e.diff)/Nrealisation
    }else{
      dat1.ceac[j] <- sum(dat1c.diff<WTP*dat1e.diff & dat1e.diff>=0)/Nrealisation
      dat2.ceac[j] <- sum(dat2c.diff<WTP*dat2e.diff & dat2e.diff>=0)/Nrealisation
      dat3.ceac[j] <- sum(dat3c.diff<WTP*dat3e.diff & dat3e.diff>=0)/Nrealisation
      dat4.ceac[j] <- sum(dat4c.diff<WTP*dat4e.diff & dat4e.diff>=0)/Nrealisation
    }
    j <- j+1
  }

  # print(c(dat1.ceac[200], dat2.ceac[200], dat3.ceac[200], dat4.ceac[200]))

  if(labelLong){labtext <- paste(intlabels[1], "vs", intlabels[-1])
  }else{
    labtext <- intlabels
  }

  ## Binomial confidence intervals
  dat1.caec.CI.upper <- dat1.ceac + 1.96*sqrt(Nrealisation*dat1.ceac*(1-dat1.ceac))/Nrealisation
  dat2.caec.CI.upper <- dat2.ceac + 1.96*sqrt(Nrealisation*dat2.ceac*(1-dat2.ceac))/Nrealisation
  dat3.caec.CI.upper <- dat3.ceac + 1.96*sqrt(Nrealisation*dat3.ceac*(1-dat3.ceac))/Nrealisation
  dat4.caec.CI.upper <- dat4.ceac + 1.96*sqrt(Nrealisation*dat4.ceac*(1-dat4.ceac))/Nrealisation

  dat1.caec.CI.lower <- dat1.ceac - 1.96*sqrt(Nrealisation*dat1.ceac*(1-dat1.ceac))/Nrealisation
  dat2.caec.CI.lower <- dat2.ceac - 1.96*sqrt(Nrealisation*dat2.ceac*(1-dat2.ceac))/Nrealisation
  dat3.caec.CI.lower <- dat3.ceac - 1.96*sqrt(Nrealisation*dat3.ceac*(1-dat3.ceac))/Nrealisation
  dat4.caec.CI.lower <- dat4.ceac - 1.96*sqrt(Nrealisation*dat4.ceac*(1-dat4.ceac))/Nrealisation

  par(mar=c(5.1,5.1,4.1,2.1))

  plot(1, type="n", ylim=c(0,1), xlim=c(0,XMAX),
       ylab="Probability of cost effectiveness", xlab="Maximum willingness to pay (GBP per QALY)",
       cex.axis = 1.5, cex.lab=1.5, cex.main=1.5, main=TITLE)

  # lines(x=xvals, y=dat1.ceac, type="o", col=COLS[1], pch=PCH[1])
  # lines(x=xvals, y=dat2.ceac, type="o", col=COLS[2], pch=PCH[2])
  # lines(x=xvals, y=dat3.ceac, type="o", col=COLS[3], pch=PCH[3])
  # lines(x=xvals, y=dat4.ceac, type="o", col=COLS[4], pch=PCH[4])
  #
  # legend(30000,1, labtext, col=COLS[1:length(labtext)], pch=PCH[1:length(labtext)], bty = "n")#, y.intersp = 0.8, x.intersp = 0.1, text.width = 0.1)

  if(CI){ #error bounds
    alpha <- 0.3
    CIcols <- c(rgb(1, 0, 0, alpha), rgb(0, 1, 0, alpha), rgb(0, 0, 1, alpha), rgb(0, 1, 1, alpha))
    polygon(c(xvals, rev(xvals)), c(dat1.caec.CI.upper, rev(dat1.caec.CI.lower)), col=CIcols[1], border=NA)
    polygon(c(xvals, rev(xvals)), c(dat2.caec.CI.upper, rev(dat2.caec.CI.lower)), col=CIcols[2], border=NA)
    polygon(c(xvals, rev(xvals)), c(dat3.caec.CI.upper, rev(dat3.caec.CI.lower)), col=CIcols[3], border=NA)
    polygon(c(xvals, rev(xvals)), c(dat4.caec.CI.upper, rev(dat4.caec.CI.lower)), col=CIcols[4], border=NA)
    # legend(35000,1, labtext, fill=CIcols, bty = "n")
    legend(25000,1, labtext, fill=CIcols, bty = "n")
  }else{
    lines(x=xvals, y=dat1.ceac, type="l", lwd=2, col=COLS[1], lty=LTY[1])
    lines(x=xvals, y=dat2.ceac, type="l", lwd=2, col=COLS[2], lty=LTY[2])
    lines(x=xvals, y=dat3.ceac, type="l", lwd=2, col=COLS[3], lty=LTY[3])
    lines(x=xvals, y=dat4.ceac, type="l", lwd=2, col=COLS[4], lty=LTY[4])
    # legend(35000,1, labtext, col=COLS[1:length(labtext)], lty=LTY[1:length(labtext)], bty = "n")#, y.intersp = 0.8, x.intersp = 0.1, text.width = 0.1)
    legend(25000,1, labtext, col=COLS[1:length(labtext)], lty=LTY[1:length(labtext)], bty = "n")#, y.intersp = 0.8, x.intersp = 0.1, text.width = 0.1)
  }
  mapply(abline, v=c(20000,30000), col=c("red","blue"),  lwd=c(1,3))

}
