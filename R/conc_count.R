#' @export
count_inversion_involvement <- function(x) {
  get_conc_count(x)
}

#' @export
count_inversions <- function(x) {
  sum(get_conc_count(x)) / 2
}

#' @export
get_dc_count <- function(x) {
  total <- sum(get_conc_count(x)) / 2
  list("C" = choose(length(x), 2) - total, "D" = total)
}

#' @export
kendall_distance <- function(x, y) {
 count_inversions(x[order(y)]) 
}

