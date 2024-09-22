#' Convert coordinate of from DDMM.mmmm to decimal degrees
#'
#' @param x Longitude or latitude numeric vector
#'
#' @return A vector
#' @export
#'
convert_coord <- function(x) {

  # It's simpler to do the arithmetic on positive numbers, we'll add the signs
  #  back in at the end.
  sgns   <- sign(x)
  x <- abs(x)

  # grab the MM.MMMM bit, which is always <100. '%%' is modular arithmetic.
  mm <- x %% 100

  # grab the DD bit. Divide by 100 because of the MM.MMMM bit.
  dd <- (x - mm)/100

  # convert to decimal degrees, don't forget to add the signs back!
  out <- (dd+mm/60) * sgns
  return(out)
}
