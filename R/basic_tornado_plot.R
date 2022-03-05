
#' A Simple Tornado Plot in Base R
#'
#' Provide middle value and standard errors.
#' Assumes symmetrical.
#'
#' https://www.r-statistics.com/2010/07/visualization-of-regression-coefficients-in-r/
#'
#' @param cf Central values
#' @param se Standard errors
#' @param df
#' @param level
#' @param parm
#' @param labels
#' @param xlab
#' @param ylab
#' @param xlim
#' @param ylim
#' @param las
#' @param lwd
#' @param lty
#' @param pch
#' @param col
#' @param length
#' @param angle
#' @param code
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' tornado_plot(cf = c(a=2, b=3), se = c(1,2), labels = TRUE)
#'
tornado_plot <- function(cf,
                         se,
                         df = NULL,
                         level = 0.95,
                         parm = NULL,
                         labels = FALSE,
                         xlab = "Coefficient confidence intervals",
                         ylab = "",
                         xlim = NULL,
                         ylim = NULL,
                         las = 1,
                         lwd = 1,
                         lty = c(1, 2),
                         pch = 19,
                         col = 1,
                         length = 0,
                         angle = 30,
                         code = 3, ...) {

  if (is.null(parm)) parm <- seq_along(cf)
  if (is.numeric(parm) | is.logical(parm)) parm <- names(cf)[parm]
  if (is.character(parm)) parm <- which(names(cf) %in% parm)

  # cf <- cf[parm]
  # se <- se[parm]
  k <- length(cf)

  ci_U <- cf + se
  ci_L <- cf - se

  lwd <- rep(lwd, length.out = 2)
  lty <- rep(lty, length.out = 2)
  pch <- rep(pch, length.out = k)
  col <- rep(col, length.out = k)

  if (is.null(xlim)) xlim <- range(c(0, max(ci_U), min(ci_L)))
  if (is.null(ylim)) ylim <- c(1 - 0.05 * k, 1.05 * k)

  if (isTRUE(labels)) labels <- names(cf)
  if (identical(labels, FALSE)) labels <- ""
  labels <- rep(labels, length.out = k)

  plot(0, 0, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab,
       axes = FALSE, type = "n", las = las, ...)
  arrows(ci_U, 1:k, ci_L, 1:k, lty = lty[1], lwd = lwd[1], col = col,
         length = length, angle = angle, code = code)
  points(cf, 1:k, pch = pch, col = col)
  # abline(v = 0, lty = lty[2], lwd = lwd[2])
  axis(1)
  axis(2, at = 1:k, labels = labels, las = las)
  box()
}

