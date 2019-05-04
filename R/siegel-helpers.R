get_inf_intersect <- function(xx, yy, decreasing = FALSE) {
  if (isTRUE(decreasing)) {
    slope <- rank(xx, ties.method = "max")
    slope <- length(xx) + 1 - slope
  } else {
    slope <- rank(xx, ties.method = "min")
  }
  slope_order <- order(slope)
  slope <- slope[slope_order]
  slope_rle   <- rle(slope)
  
  slope_cumidx <- c(1, lag(cumsum(slope_rle$lengths))[-1] + 1)
  for (i in seq(slope_rle$lengths)) {
    if (slope_rle$lengths[i] > 1) {
      height <- yy[slope_order[seq(slope_cumidx[i], slope_cumidx[i] + slope_rle$lengths[i] - 1)]]
      height <- rank(height)
      if (isTRUE(decreasing)) {
        height <- length(height) + 1 - height
      }
      idx <- seq(slope_cumidx[i], slope_cumidx[i] + slope_rle$lengths[i] - 1)
      slope[idx] <- slope[idx] + height - 1
    }
  }
  slope[order(slope_order)]
}