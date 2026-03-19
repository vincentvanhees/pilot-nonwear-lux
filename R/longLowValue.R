#' Classify long period with low value
#'
#' @description Classify long period with low value
#'
#' @param x Numeric vector with lux values
#' @param x_threshold Numeric, threshold to defined low
#' @param window_size_hours Numeric, rolling window size in hours as passed on to \link{rollApply}
#' @param step_size Numeric, integer as passed on to \link{rollApply}.
#' @param epoch_size Numeric, epoch size as passed on to \link{rollApply}.
#'
#' @return Numeric vector with with length equal to x with classification  \cr
#' (1 = condition met, 0 = condition not met)
#' 
#' @export
longLowValue = function(x, x_threshold, window_size_hours, step_size, epoch_size) {
  # Detects time windows longer than window_size_hours
  # for which x is less than x_threshold, where x is assumed to be a rolling 
  # statistic of those window_size_hours, e.g. the 95th percentile
  classification = rep(0, length(x))
  condition_met = which(x < x_threshold)
  if (length(condition_met) > 0) {
    classification[condition_met] = 1
    # Label all 16 hours and not only the epoch in the center
    classification = rollApply(classification, window_size_hours = window_size_hours,
                               FUN = max, step_size = step_size, epoch_size)
  }
  return(classification)
}