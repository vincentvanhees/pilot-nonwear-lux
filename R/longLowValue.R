#' Classify long period with low value
#'
#' @description Classify long period with low valued rolling statistic. This function \cr
#' is a post-processing step of rollApply, where rollApply classifies \cr
#' only the mid-point of the rolling window, and this function then labels all the \cr
#' points in a way accordingly, if the value is below x_threshold.
#'
#' @param x Numeric vector with rolling statistic based on window_size_hours \cr
#' e.g. the 95th percentile
#' @param x_threshold Numeric, threshold to defined low
#' @param window_size_hours Numeric, rolling window size in hours as passed \cr
#' on to \link{rollApply}
#' @param step_size Numeric, integer as passed on to \link{rollApply}.
#' @param epoch_size Numeric, epoch size as passed on to \link{rollApply}.
#'
#' @return Numeric vector with with length equal to x with classification  \cr
#' (1 = condition met, 0 = condition not met)
#' 
#' @export
longLowValue = function(x, x_threshold, window_size_hours, step_size, epoch_size) {
  classification = rep(0, length(x))
  condition_met = which(x < x_threshold)
  if (length(condition_met) > 0) {
    # Label all timepoints for which condition is met
    classification[condition_met] = 1
    # Label all 16 hours and not only the epoch in the center
    classification = rollApply(classification, window_size_hours = window_size_hours,
                               FUN = max, step_size = step_size, epoch_size)
  }
  return(classification)
}