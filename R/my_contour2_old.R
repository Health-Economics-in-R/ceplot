
#' my_contour2
#'
#' @param he
#' @param wtp
#' @param xlim
#' @param ylim
#' @param comparison
#' @param graph
#' @param CONTOUR_PC
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
my_contour2 <- function(he,
                        wtp = 20000,
                        xlim = NULL, ylim = NULL,
                        comparison = NULL,
                        withpoints = FALSE,
                        graph = c("base", "ggplot2"),
                        CONTOUR_PC = "5%", ...) {
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
    title <- paste("Cost effectiveness plane \n", he$interventions[he$ref],
                   " vs ", he$interventions[he$comp], sep = "")
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
      message("The first available comparison will be selected. To plot multiple comparisons together please use the ggplot2 version. Please see ?contour2 for additional details.")
      comparison <- 1
    }
    if (he$n.comparisons > 1) {
      if (!exists("title", where = exArgs)) {
        title <- paste("Cost effectiveness plane contour plot \n",
                       he$interventions[he$ref], " vs ", he$interventions[he$comp[comparison]],
                       sep = "")
      }
      else {
        title <- exArgs$title
      }
      he$delta.e <- he$delta.e[, comparison]
      he$delta.c <- he$delta.c[, comparison]
      he$comp <- he$comp[comparison]
      he$ICER <- he$ICER[comparison]
    }
    m.e <- range(he$delta.e)[1]
    M.e <- range(he$delta.e)[2]
    m.c <- range(he$delta.c)[1]
    M.c <- range(he$delta.c)[2]
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
    plot(xx, yy, col = "white", xlim = c(m.e, M.e), ylim = c(m.c, M.c),
         xlab = xlab, ylab = ylab, main = title, axes = F)

    polygon(c(min(xx), seq(min(xx), max(xx), step), max(xx)),
            c(min(yy), wtp * seq(min(xx), max(xx), step), min(yy)),
            col = "grey95", border = "black")
    axis(1)
    axis(2)
    box()
    points(he$delta.e, he$delta.c, pch = 20, cex = 0.35,
           col = "grey55")
    abline(h = 0, col = "dark grey")
    abline(v = 0, col = "dark grey")
    text(M.e, M.c, paste("•", " ICER=", format(he$ICER, digits = 6,
                                               nsmall = 2), sep = ""), cex = 0.95, pos = 2, col = "red")
    points(mean(he$delta.e), mean(he$delta.c), pch = 20,
           col = "red", cex = 1)
    t1 <- paste("k==", format(wtp, digits = 3, nsmall = 2,
                              scientific = F), sep = "")
    text(x.pt, y.pt, parse(text = t1), cex = 0.8, pos = 4)
    requireNamespace("MASS")
    nlevels <- 4
    scale <- 0.5
    density <- MASS::kde2d(he$delta.e, he$delta.c, n = 300,
                           h = c(sd(he$delta.e)/scale, sd(he$delta.c)/scale))
    m.c <- range(he$delta.c)[1]
    M.c <- range(he$delta.c)[2]
    m.e <- range(he$delta.e)[1]
    M.e <- range(he$delta.e)[2]

    par(new = TRUE)
    contour(density$x, density$y, density$z, add = TRUE, levels = c(0.05, 0.95),
            nlevels = nlevels, drawlabels = FALSE, lwd = 1.5)
    return(invisible(NULL))
  }
  else {
    if (!isTRUE(requireNamespace("ggplot2", quietly = TRUE) &
                requireNamespace("grid", quietly = TRUE))) {
      message("falling back to base graphics\n")
      contour2(he, comparison = comparison, xlim = xlim, levels = c(0.05, 0.95),
               ylim = ylim, wtp = wtp, graph = "base")
      return(invisible(NULL))
    }
    scale = 0.5
    nlevels = 5
    requireNamespace("MASS")
    z <- e <- NA_real_
    if (he$n.comparisons == 1) {
      density <- with(he, MASS::kde2d(delta.e, delta.c,
                                      n = 300, h = c(sd(delta.e)/scale, sd(delta.c)/scale)))
      density <- data.frame(expand.grid(e = density$x,
                                        c = density$y), z = as.vector(density$z))
      contour <- ceplane.plot(he, wtp = wtp, graph = "ggplot2",
                              ...) + ggplot2::geom_contour(ggplot2::aes(z = z,
                                                                        x = e, y = c), data = density, colour = "black",
                                                           bins = nlevels)
    }
    if (he$n.comparisons > 1 & is.null(comparison)) {

      densitydf <- data.frame()
      contour_df <- data.frame()

      for (i in 1:he$n.comparisons) {
        density <- with(he, MASS::kde2d(delta.e[, i],
                                        delta.c[, i], n = 300, h = c(sd(delta.e[, i])/scale,
                                                                     sd(delta.c[, i])/scale)))
        densitydf <- rbind(densitydf, cbind(expand.grid(density$x,
                                                        density$y), as.vector(density$z)))
        kd <- with(he, ks::kde(cbind(delta.e[, i],delta.c[, i]), compute.cont = TRUE))

        contour_s <-
          with(kd, contourLines(x = eval.points[[1]], y = eval.points[[2]],
                                z = estimate, levels = cont[CONTOUR_PC])[[1]]) %>%
          data.frame(s = as.character(i))

        contour_df <- rbind(contour_df, contour_s)
      }
      names(densitydf) <- c("e", "c", "z")
      densitydf <- cbind(densitydf, comparison = as.factor(sort(rep(1:he$n.comparisons,
                                                                    dim(densitydf)[1]/he$n.comparisons))))
      contour <-
        my_ceplane.plot(he, wtp = wtp, graph = "ggplot2", withpoints = withpoints, ...) +
        # ggplot2::geom_contour(data = densitydf,
        #                       ggplot2::aes(x = e, y = c, z = z, colour = comparison),
        #                       bins = nlevels, linetype = 1)
        geom_path(aes(x, y, colour = s), data = contour_df)
    }
    if (he$n.comparisons > 1 & !is.null(comparison)) {
      he$comp <- he$comp[comparison]
      he$delta.e <- he$delta.e[, comparison]
      he$delta.c <- he$delta.c[, comparison]
      he$n.comparators = length(comparison) + 1
      he$n.comparisons = length(comparison)
      he$interventions = he$interventions[sort(c(he$ref,
                                                 he$comp))]
      he$ICER = he$ICER[comparison]
      he$ib = he$ib[, , comparison]
      he$eib = he$eib[, comparison]
      he$U = he$U[, , sort(c(he$ref, comparison + 1))]
      he$ceac = he$ceac[, comparison]
      he$ref = rank(c(he$ref, he$comp))[1]
      he$comp = rank(c(he$ref, he$comp))[-1]
      he$mod <- TRUE
      return(contour2(he, wtp = wtp, xlim = xlim, ylim = ylim,
                      comparison = NULL, graph = "ggplot2", ...))
    }
    contour <- contour + ggplot2::coord_cartesian(xlim = xlim,
                                                  ylim = ylim)
    return(contour)
  }
}

