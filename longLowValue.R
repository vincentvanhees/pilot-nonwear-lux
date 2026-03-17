longLowValue = function(x, x_threshold, window_size_hours, N, step_size, epoch_size) {
  # Detects time windows longer than window_size_hours
  # for which x is less than x_threshold, where x is assumed to be a rolling 
  # statistic of those window_size_hours, e.g. the 95th percentile
  classification = rep(0, N)
  condition_met = which(x < x_threshold)
  if (length(condition_met) > 0) {
    data$nonwearA[condition_met] = 1
    # Label all 16 hours and not only the epoch in the center
    classification = rollApply(classification, window_size_hours = window_size_hours,
                               FUN = max, step_size = step_size, N, epoch_size)
  }
  return(classification)
}