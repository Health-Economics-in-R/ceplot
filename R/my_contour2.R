
#' my_contour2
#'
#' @param res_bcea
#' @param wtp
#' @param xlim
#' @param ylim
#' @param comparison
#' @param graph
#' @param CONTOUR_PC
#' @param facet (TRUE/FALSE)
#' @param ...
#'
#' @rdname ce_planes
#' @return
#' @export
#'
#' @examples
my_contour2 <- function(...) UseMethod('my_contour2')


#' @rdname ce_planes
#'
#' @export
#'
#' @method my_contour2 bcea
my_contour2.bcea <- function(res_bcea,
                             wtp = 20000,
                             xlim = NULL,
                             ylim = NULL,
                             comparison = NULL,
                             withpoints = FALSE,
                             graph = c("base", "ggplot2"),
                             CONTOUR_PC = "5%",
                             facet = FALSE, ...) {
  options(scipen = 10)
  exArgs <- list(...)
  if (!exists("xlab", where = exArgs)) {
    xlab <- "Effectiveness differential"
  }
  else {
    xlab <- exArgs$xlab
  }
  if (!exists("ylab", where = exArgs)) {
    ylab <- "Cost differential"
  }
  else {
    ylab <- exArgs$ylab
  }
  if (!exists("title", where = exArgs)) {
    title <- paste("Cost effectiveness plane \n", res_bcea$interventions[res_bcea$ref],
                   " vs ", res_bcea$interventions[res_bcea$comp], sep = "")
  }
  else {
    title <- exArgs$title
  }

  base.graphics <- ifelse(isTRUE(pmatch(graph, c("base", "ggplot2")) ==
                                   2), FALSE, TRUE)
  if (base.graphics) {
    ps.options(encoding = "CP1250")
    pdf.options(encoding = "CP1250")
    if (is.null(comparison)) {
      message("the first available comparison will be selected.
              To plot multiple comparisons together please use the ggplot2 version.
              Please see ?contour2 for additional details.")
      comparison <- 1
    }
    if (res_bcea$n.comparisons > 1) {
      if (!exists("title", where = exArgs)) {
        title <- paste("Cost effectiveness plane contour plot \n",
                       res_bcea$interventions[res_bcea$ref], " vs ", res_bcea$interventions[res_bcea$comp[comparison]],
                       sep = "")
      }
      else {
        title <- exArgs$title
      }
      res_bcea$delta.e <- res_bcea$delta.e[, comparison]
      res_bcea$delta.c <- res_bcea$delta.c[, comparison]
      res_bcea$comp <- res_bcea$comp[comparison]
      res_bcea$ICER <- res_bcea$ICER[comparison]
    }
    m.e <- range(res_bcea$delta.e)[1]
    M.e <- range(res_bcea$delta.e)[2]
    m.c <- range(res_bcea$delta.c)[1]
    M.c <- range(res_bcea$delta.c)[2]
    step <- (M.e - m.e)/10
    m.e <- ifelse(m.e < 0, m.e, -m.e)
    m.c <- ifelse(m.c < 0, m.c, -m.c)
    x.pt <- 0.95 * m.e
    y.pt <- ifelse(x.pt * wtp < m.c, m.c, x.pt * wtp)
    xx <- seq(100 * m.c/wtp, 100 * M.c/wtp, step)
    yy <- xx * wtp
    xx[1] <- ifelse(min(xx) < m.e, xx[1], 2 * m.e)
    yy[1] <- ifelse(min(yy) < m.c, yy[1], 2 * m.c)
    xx[length(xx)] <- ifelse(xx[length(xx)] < M.e, 1.5 *
                               M.e, xx[length(xx)])
    if (!is.null(xlim)) {
      m.e <- xlim[1]
      M.e <- xlim[2]
    }
    if (!is.null(ylim)) {
      m.c <- ylim[1]
      M.c <- ylim[2]
    }

    plot(xx, yy, col = "white",
         xlim = c(m.e, M.e), ylim = c(m.c, M.c),
         xlab = xlab, ylab = ylab,
         main = title, axes = F)

    polygon(c(min(xx), seq(min(xx), max(xx), step), max(xx)),
            c(min(yy), wtp * seq(min(xx), max(xx), step), min(yy)),
            col = "grey95", border = "black")
    axis(1)
    axis(2)
    box()
    points(x = res_bcea$delta.e,
           y = res_bcea$delta.c,
           pch = 20,
           cex = 0.35,
           col = "grey55")
    abline(h = 0, col = "dark grey")
    abline(v = 0, col = "dark grey")
    text(x = M.e, y = M.c,
         labels = paste("•", " ICER=", format(res_bcea$ICER, digits = 6,
                                              nsmall = 2), sep = ""),
         cex = 0.95, pos = 2, col = "red")
    points(x = mean(res_bcea$delta.e),
           y = mean(res_bcea$delta.c),
           pch = 20,
           col = "red",
           cex = 1)
    t1 <- paste("k==", format(wtp, digits = 3, nsmall = 2,
                              scientific = F), sep = "")
    text(x = x.pt, y = y.pt,
         labels = parse(text = t1), cex = 0.8, pos = 4)
    requireNamespace("MASS")
    nlevels <- 4
    scale <- 0.5
    density <- MASS::kde2d(x = res_bcea$delta.e,
                           y = res_bcea$delta.c,
                           n = 300,
                           h = c(sd(res_bcea$delta.e)/scale, sd(res_bcea$delta.c)/scale))
    m.c <- range(res_bcea$delta.c)[1]
    M.c <- range(res_bcea$delta.c)[2]
    m.e <- range(res_bcea$delta.e)[1]
    M.e <- range(res_bcea$delta.e)[2]

    par(new = TRUE)
    contour(x = density$x, y = density$y, z = density$z,
            add = TRUE, levels = c(0.05, 0.95),
            nlevels = nlevels, drawlabels = FALSE, lwd = 1.5)
    return(invisible(NULL))
  }
  else {
    if (!isTRUE(requireNamespace("ggplot2", quietly = TRUE) &
                requireNamespace("grid", quietly = TRUE))) {
      message("falling back to base graphics\n")
      contour2(he = res_bcea,
               comparison = comparison,
               xlim = xlim,
               levels = c(0.05, 0.95),
               ylim = ylim,
               wtp = wtp,
               graph = "base")
      return(invisible(NULL))
    }
    scale = 0.5
    nlevels = 5
    requireNamespace("MASS")
    z <- e <- NA_real_
    if (res_bcea$n.comparisons == 1) {
      density <- with(res_bcea, MASS::kde2d(x = delta.e,
                                            y = delta.c,
                                            n = 300,
                                            h = c(sd(delta.e)/scale, sd(delta.c)/scale)))
      density <- data.frame(expand.grid(e = density$x,
                                        c = density$y),
                            z = as.vector(density$z))

      kd <- with(res_bcea, ks::kde(x = cbind(delta.e, delta.c),
                                   compute.cont = TRUE))

      contour_1 <-
        with(kd, contourLines(x = eval.points[[1]],
                              y = eval.points[[2]],
                              z = estimate,
                              levels = cont[CONTOUR_PC])[[1]]) %>%
        data.frame()

      contour <- ceplane.plot(he = res_bcea,
                              wtp = wtp,
                              graph = "ggplot2",
                              ...) +
        # ggplot2::geom_contour(ggplot2::aes(z = z,
        #                                    x = e,
        #                                    y = c),
        #                       data = density,
        #                       colour = "black",
        #                       bins = nlevels) +
        geom_path(aes(x, y),
                  data = contour_1)
    }
    if (res_bcea$n.comparisons > 1 & is.null(comparison)) {

      densitydf <- data.frame()
      contour_df <- data.frame()

      for (i in 1:res_bcea$n.comparisons) {

        #this is the main section...
        #

        density <- with(res_bcea, MASS::kde2d(x = delta.e[, i],
                                              y = delta.c[, i],
                                              n = 300,
                                              h = c(sd(delta.e[, i])/scale,
                                                    sd(delta.c[, i])/scale)))
        densitydf <- rbind(densitydf, cbind(expand.grid(density$x, density$y),
                                            as.vector(density$z)))
        kd <- with(res_bcea, ks::kde(x = cbind(delta.e[, i], delta.c[, i]),
                                     compute.cont = TRUE))

        contour_s <-
          with(kd, contourLines(x = eval.points[[1]],
                                y = eval.points[[2]],
                                z = estimate,
                                levels = cont[CONTOUR_PC])[[1]]) %>%
          data.frame(s = as.character(i))

        contour_df <- rbind(contour_df, contour_s)
      }
      names(densitydf) <- c("e", "c", "z")
      densitydf <- cbind(densitydf,
                         comparison = as.factor(sort(rep(1:res_bcea$n.comparisons,
                                                         dim(densitydf)[1]/res_bcea$n.comparisons))))

      contour <-
        my_ceplane.plot(he = res_bcea,
                        wtp = wtp,
                        graph = "ggplot2",
                        withpoints = withpoints, ...) +
        # ggplot2::geom_contour(data = densitydf,
        #                       ggplot2::aes(x = e,
        #                                    y = c,
        #                                    z = z,
        #                                    colour = comparison,
        #                                    linetype = comparison),
        #                       bins = nlevels) +
        #                       # breaks = 0:3) +
        scale_colour_manual(values = rainbow(3)) +
        geom_path(aes(x, y, colour = s,
                      linetype = s),
                  data = contour_df)  #+
        #facet_wrap(. ~ comparison) ##TODO:repeats contours
    }
    if (res_bcea$n.comparisons > 1 & !is.null(comparison)) {
      res_bcea$comp <- res_bcea$comp[comparison]
      res_bcea$delta.e <- res_bcea$delta.e[, comparison]
      res_bcea$delta.c <- res_bcea$delta.c[, comparison]
      res_bcea$n.comparators = length(comparison) + 1
      res_bcea$n.comparisons = length(comparison)
      res_bcea$interventions = res_bcea$interventions[sort(c(res_bcea$ref,
                                                 res_bcea$comp))]
      res_bcea$ICER = res_bcea$ICER[comparison]
      res_bcea$ib = res_bcea$ib[, , comparison]
      res_bcea$eib = res_bcea$eib[, comparison]
      res_bcea$U = res_bcea$U[, , sort(c(res_bcea$ref, comparison + 1))]
      res_bcea$ceac = res_bcea$ceac[, comparison]
      res_bcea$ref = rank(c(res_bcea$ref, res_bcea$comp))[1]
      res_bcea$comp = rank(c(res_bcea$ref, res_bcea$comp))[-1]
      res_bcea$mod <- TRUE

      return(contour2(he = res_bcea,
                      wtp = wtp,
                      xlim = xlim,
                      ylim = ylim,
                      comparison = NULL,
                      graph = "ggplot2", ...))
    }

    contour <- contour + ggplot2::coord_cartesian(xlim = xlim,
                                                  ylim = ylim)
    return(contour)
  }
}

