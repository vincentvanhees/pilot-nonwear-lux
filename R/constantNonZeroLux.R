
#' Classify periods with constant (low variantion) and non-zero Lux 
#'
#' @description Classify periods with constant (low variantion) and non-zero Lux 
#'
#' @param x Tibble with columns min_per_hour, relvar_hour, and sd_diff_per_hour
#' @param minimum_relval_per_hour Numeric with the minimum relative variance in lux per hour
#'
#' @return Numeric vector with with length equal to x with classification  \cr
#' (1 = condition met, 0 = condition not met)
#'
#' @export
constantNonZeroLux = function(x, minimum_relval_per_hour) {

  # Time windows with consistently a non-zero Lux but hardly any lux variation for at 
  # least an hour or variance in the derivative of the lux.
  classification = rep(0, nrow(x))
  # Not every sensor is able to measure zero, so we first identify the minimum value from the data
  # but this should not be higher than 1 Lux.
  minimum_value = pmin(min(x$min_per_hour, na.rm = TRUE), 1)
  
  condition_met = which(x$min_per_hour > minimum_value &
                          (x$relvar_hour < minimum_relval_per_hour |
                             x$sd_diff_per_hour < minimum_relval_per_hour))
  if (length(condition_met) > 0) {
    classification[condition_met] = 1
  }
  return(classification)
}