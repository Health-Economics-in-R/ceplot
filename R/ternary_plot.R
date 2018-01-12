#!/usr/bin/env Rscript
#--------------------------------------
# Example Ternary plot for Nathan Green
#
# Date:   13th December 2017
# Author: Nicholas Hamilton
# Email:  n.hamilton@unsw.edu.au
#--------------------------------------

#Load Data
# df = read.csv("C:/Users/ngreen1/Google Drive/R code/ggtern/tern_testdata.csv", header = TRUE)

##Common Theme for Both
theme_common = function(){
  list(
    theme_showarrows(),
    theme_legend_position('tl'),
    labs(Tarrow = "A = Start Complete",   T = 'A',
         Larrow = "B = N-Start",          L = "B",
         Rarrow = "C = Start N-Complete", R = 'C')
  )
}


# Using Heatmap:
#    Inspired By: https://stackoverflow.com/questions/26221236/ternary-heatmap-in-r
# The Binning Statistic
mystat <- function(dfStat, minT=0, maxT=1, minR=0, maxR=1, minL=0, maxL=1) {

  ret <- dfStat
  ret <- with(ret, ret[minT <= startcomplete  & startcomplete  < maxT, , drop = F])
  ret <- with(ret, ret[minL <= nstart         & nstart         < maxL, , drop = F])
  ret <- with(ret, ret[minR <= startncomplete & startncomplete < maxR, , drop = F])

  if (is.na(nrow(ret))) {
    ret <- 0
  } else {
    ret <- mean(ret$INMB)
  }
  ret
}

#Process the data
prepareDF = function(data, inc = 0.2, discardZero = FALSE){

    #   When plot_corner is FALSE, corner_cutoff determines where to stop plotting
    corner_cutoff = 1

    #   When plot_corner is FALSE, corner_number toggles display of obervations in the corners
    #   This only has an effect when text==FALSE
    corner_numbers = TRUE
    cnt <- 1
    points <- data.frame()

    for (z in seq(0,1,inc)) {
      x <- 1 - z
      y <- 0
      while (x > 0) {
        points <- rbind(points, c(cnt, x, y, z))
        x <- round(x - inc, digits = 2)
        y <- round(y + inc, digits = 2)
        cnt <- cnt + 1
      }
      points <- rbind(points, c(cnt, x, y, z))
      cnt <- cnt + 1
    }

    colnames(points) = c("IDPoint","T","L","R")

    polygons <- data.frame()
    cnt <- 1

    #   Normal triangles
    for (p in points$IDPoint) {
      if (is.element(p, points$IDPoint[points$T == 0])) {
        next
      } else {
        pL <- points$L[points$IDPoint == p]
        pT <- points$T[points$IDPoint == p]
        pR <- points$R[points$IDPoint == p]
        polygons <- rbind(polygons,
                          c(cnt,p),
                          c(cnt,points$IDPoint[abs(points$L - pL) < inc/2 & abs(points$R - pR - inc) < inc/2]),
                          c(cnt,points$IDPoint[abs(points$L - pL - inc) < inc/2 & abs(points$R - pR) < inc/2]))
        cnt <- cnt + 1
      }
    }

    # Upside down triangles
    for (p in points$IDPoint) {
      if (!is.element(p, points$IDPoint[points$T == 0])) {
        if (!is.element(p, points$IDPoint[points$L == 0])) {
          pL <- points$L[points$IDPoint == p]
          pT <- points$T[points$IDPoint == p]
          pR <- points$R[points$IDPoint == p]
          polygons <- rbind(polygons,
                            c(cnt,p),
                            c(cnt,points$IDPoint[abs(points$T-pT) < inc/2 & abs(points$R-pR-inc) < inc/2]),
                            c(cnt,points$IDPoint[abs(points$L-pL) < inc/2 & abs(points$R-pR-inc) < inc/2]))
          cnt <- cnt + 1
        }
      }
    }

    #   IMPORTANT FOR CORRECT ORDERING.
    polygons$PointOrder <- 1:nrow(polygons)
    colnames(polygons) = c("IDLabel","IDPoint","PointOrder")

    df.tr <- merge(polygons, points)

    Labs = ddply(df.tr, "IDLabel", function(x){c(c(mean(x$T), mean(x$L), mean(x$R)))})
    colnames(Labs) = c("Label","T","L","R")

    if (FALSE) {
       triangles <- ggtern(data = df.tr, aes(L,T,R)) +
                       geom_polygon(aes(group = IDLabel), color = "black", alpha = 0.25) +
                       geom_text(data = Labs,aes(label = Label), size = 4, color = "black") +
                       theme_bw()
            print(triangles)
    }

    bins <- ddply(df.tr, .(IDLabel), summarize,
                  maxT = max(T),
                  maxL = max(L),
                  maxR = max(R),
                  minT = min(T),
                  minL = min(L),
                  minR = min(R))

    stat <- ddply(bins, .(IDLabel), here(summarize), N = mystat(dfStat = df, minT, maxT, minR, maxR, minL, maxL))
    stat$N[is.na(stat$N)] = 0
    df <- join(df.tr, stat, by = "IDLabel")

    if (discardZero)
      df = df[which(df$N != 0.0),]
    df = df[with(df, order(PointOrder)), ]

    Labs = ddply(df,.(IDLabel,N), function(x){c(c(mean(x$T),mean(x$L),mean(x$R)))})
    colnames(Labs) = c("Label","N","T","L","R")

    list(data   = df,
         labels = Labs)
}

