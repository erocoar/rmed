siegel_interval <- function(xx, yy) {
  n <- length(xx)
  low <- ((n - 1) %% 2 == 0) * 1
  
  tol <- 1e-5
  L_i <- numeric(n)
  R_i <- numeric(n)
  C_i <- rep(n - 1, n)
  
  L <- c()
  R <- c()
  C <- seq(n)
  C_inside <- C
  
  k_i <- ceiling((n - 1) / 2)
  k_c <- ceiling(n / 2)
  interval <- c(-Inf, Inf)
  b <- 0.5
  r <- ceiling(n^b)
  
  # while(sum(C_i[C_inside]) > n) {
  while(length(C_inside) > ceiling(log(n))) {
    c_samp  <- sample(C_inside, r, replace = TRUE)
    
    # 2. for each sampled line, calculate median intersection abscissa
    if (all(is.infinite(interval))) {
      c_medians <- sapply(c_samp, function(x) {
        abscissas <- sample(C[-x], r, replace = FALSE)
        slp <- (yy[x] - yy[abscissas]) / (xx[x] - xx[abscissas])
        slp[which(rank(slp, ties.method = "first") == ceiling((sqrt(n) * k_i) / n))]
      })
    } else {
      intersect_lo <- if (is.infinite(interval[1])) get_inf_intersect(xx, yy, TRUE) else (interval[1] + tol) * xx - yy 
      intersect_hi <- if (is.infinite(interval[2])) get_inf_intersect(xx, yy, FALSE) else (interval[2] + tol) * xx - yy 
      
      intersect_lo_rank <- rank(intersect_lo, ties.method = "first")
      intersect_hi_rank <- rank(intersect_hi, ties.method = "first")
      intersect_lo_order <- order(intersect_lo_rank)
      
      inv_pairs <- get_inv_pairs(intersect_hi_rank[intersect_lo_order], intersect_hi_rank[c_samp])
      inv_pairs <- intersect_lo_order[order(intersect_hi_rank[intersect_lo_order])[inv_pairs]]
      mat <- matrix(inv_pairs, ncol = 2, byrow = TRUE)
      c_medians <- sapply(c_samp, function(x) {
        vec <- as.vector(mat[mat[, 1] == x | mat[, 2] == x, ])
        vec <- vec[vec != x]
        samp <- sample(vec, r, replace = TRUE)
        ints <- (yy[x] - yy[samp]) / (xx[x] - xx[samp])
        ints[is.infinite(ints)] <- Inf
        ints[which(rank(ints, ties.method = "first") == ceiling((sqrt(n) * ceiling((n - 1) / 2)) / n))]
      })
    }
    
    # 3. compute new interval
    k <- k_c - length(L)
    C_len <- length(C_inside)
    k_lo <- max(1, floor( (r * k) / C_len - (3 * sqrt(r)) / 2 ))
    k_hi <- min(r, floor( (r * k) / C_len + (3 * sqrt(r)) / 2 ))
    
    ranks <- rank(c_medians, ties.method = "first")
    theta_lo_new  <- max(c_medians[ranks == k_lo], interval[1])
    theta_hi_new  <- min(c_medians[ranks == k_hi], interval[2])
    
    # 4. for each line inside C count the nr of intersections in the intervals
    intersect_lo <- (theta_lo_new + tol) * xx - yy
    intersect_hi <- (theta_hi_new + tol) * xx - yy
    
    intersect_lo_rank  <- rank(intersect_lo, ties.method = "first")
    intersect_hi_rank  <- rank(intersect_hi, ties.method = "first")
    intersect_lo_order <- order(intersect_lo_rank)
    
    C_i_new <- count_inversion_involvement(intersect_hi_rank[intersect_lo_order])[intersect_hi_rank]
    
    intersect_hi <- (theta_lo_new + tol) * xx - yy
    intersect_lo <- if (is.infinite(interval[1])) get_inf_intersect(xx, yy, TRUE) else (interval[1] + tol) * xx - yy 
    
    intersect_lo_rank <- rank(intersect_lo, ties.method = "first")
    intersect_hi_rank <- rank(intersect_hi, ties.method = "first")
    intersect_lo_order<- order(intersect_lo_rank)
    
    L_i_new <- count_inversion_involvement(intersect_hi_rank[intersect_lo_order])[intersect_hi_rank]
    
    R_i_new <- n - 1 - C_i_new - L_i_new - R_i - L_i
    
    if (sum(R_i_new + R_i >= k_i + low) >= k_c) { # used to be k_c
      # center actually right interval theta_hi_new, theta_hi
      L_i <- C_i_new + L_i_new + L_i
      C_i <- R_i_new
      L   <- which(L_i >= k_i)
      R   <- which(R_i >= k_i + low)
      C_inside <- setdiff(C_inside, c(L, R))
      interval <- c(theta_hi_new, interval[2])
    } else if (sum(L_i_new + L_i >= k_i) >= k_c) {
      # center actually in theta_lo, theta_lo_new
      R_i <- C_i_new + R_i_new + R_i
      C_i <- L_i_new
      R   <- which(R_i >= k_i + low)
      L   <- which(L_i >= k_i)
      C_inside <- setdiff(C_inside, c(L, R))
      interval <- c(interval[1], theta_lo_new)
    } else {
      # center in C
      L_i <- L_i + L_i_new
      R_i <- R_i + R_i_new
      C_i <- C_i_new
      L   <- which(L_i >= k_i)
      R   <- which(R_i >= k_i + low)
      C_inside <- setdiff(C_inside, c(L, R))
      C_inside <- setdiff(C_inside, which(L_i == (n - 1) / 2 & R_i == (n - 1) / 2))
      interval <- c(theta_lo_new, theta_hi_new)
    }
  }
  
  list(L, C_inside)
}
