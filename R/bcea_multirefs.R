
# @param ref Indices of pairs from left to right

bcea_multirefs <- function(e, c,
                           ref = seq_len(ncol(e)),
                           Kmax = 50000,
                           wtp = NULL){

  if (ncol(e) %% 2 != 0) stop("odd number of columns in e")
  if (ncol(c) %% 2 != 0) stop("odd number of columns in c")

  n.sim <- dim(e)[1]
  n.comparisons <- dim(e)[2]/2

  interventions <- paste("intervention", 1:n.comparisons)

  odd_cols  <- seq(1, ncol(e), 2)
  even_cols <- seq(2, ncol(e), 2)

  comp <- odd_cols

  # this is the main change to the BCEA version
  delta.e <- e[ ,odd_cols] - e[ ,even_cols]
  delta.c <- c[ ,odd_cols] - c[ ,even_cols]

  ICER <- colMeans(delta.c)/colMeans(delta.e)

  if (!exists("Kmax")) {
    Kmax <- 50000
  }

  if (!is.null(wtp)) {
    wtp <- sort(unique(wtp))
    npoints <- length(wtp) - 1
    Kmax <- max(wtp)
    step <- NA
    k <- wtp
    K <- npoints + 1
  } else {
    npoints <- 500
    step <- Kmax/npoints
    k <- seq(0, Kmax, step)
    K <- length(k)
  }

  if (n.comparisons == 1) {
    ib <- scale(k %*% t(delta.e), delta.c, scale = FALSE)
    ceac <- rowMeans(ib > 0)

    eib <- rowMeans(ib)
    best <- rep(ref, K)
    best[which(eib < 0)] <- comp
    check <- c(0, diff(best))
    kstar <- k[check != 0]
  }

  if (n.comparisons > 1) {
    ib <- array(rep(delta.e, K) * rep(k, each = n.sim * n.comparisons) -
                  as.vector(delta.c), dim = c(n.sim, n.comparisons,
                                              K))
    ib <- aperm(ib, c(3, 1, 2))
    ceac <- apply(ib > 0, c(1, 3), mean)
  }

  if (n.comparisons > 1) {
    eib <- apply(ib, 3, function(x) apply(x, 1, mean))

    if (is.null(dim(eib))) {
      tmp <- min(eib)
      tmp2 <- which.min(eib)
    }
    else {
      tmp <- apply(eib, 1, min)
      tmp2 <- apply(eib, 1, which.min)
    }

    best <- ifelse(tmp > 0, ref, comp[tmp2])
    check <- c(0, diff(best))
    kstar <- k[check != 0]
  }

  U <- array(rep(e, K) * rep(k, each = n.sim * n.comparisons) -
               as.vector(c), dim = c(n.sim, n.comparisons, K))

  U <- aperm(U, c(1, 3, 2))

  rowMax <- function(x) {
    do.call(pmax, as.data.frame(x))
  }

  Ustar <- vi <- ol <- matrix(NA, n.sim, K)

  for (i in 1:K) {
    Ustar[, i] <- rowMax(U[, i, ])

    vi[, i] <- Ustar[, i] - max(apply(U[, i, ], 2, mean))
  }

  evi <- colMeans(ol)

  he <- list(n.sim = n.sim,
             n.comparisons = n.comparisons,
             n.comparators = n.comparisons,
             delta.e = delta.e,
             delta.c = delta.c,
             ICER = ICER,
             Kmax = Kmax,
             k = k,
             ceac = ceac,
             ib = ib,
             eib = eib, kstar = kstar, best = best, U = U, vi = vi,
             Ustar = Ustar, ol = ol, evi = evi,
             interventions = interventions,
             ref = ref, comp = comp, step = step,
             e = e, c = c)

  class(he) <- "bcea"

  return(he)
}
