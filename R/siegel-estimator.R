#' @export
siegel_estimator <- function(xx, yy) { # TODO add option for randomized or not
  n <- length(xx)
  if (n <= 1e3) {
    slope <- median_estimator(xx, yy)
  } else {
    mdn_idx <- ceiling((n - 1) / 2) # low median
    lst <- siegel_interval(xx, yy)
    L   <- lst[[1]]
    C_inside <- lst[[2]]
    slopes      <- median_slopes(xx, yy, C_inside, mdn_idx)
    slope_ranks <- rank(slopes, ties.method = "first") + length(L)
    slope       <- mean(slopes[which(slope_ranks %in% c(k_c, k_c + (n %% 2 == 0) * 1))])
  }
  intercept <- median_intercept(xx, yy, slope)
  return(c("Intercept" = intercept, "Slope" = slope))
}
