constantNonZeroLux = function(x, minimum_relval_per_hour, N) {
  
  # x: data.frame with columns min_per_hour, relvar_hour, and sd_diff_per_hour
  # minimum_relval_per_hour: numeric with the minimum relative variance in lux per hour
  
  # Time windows with consistently a non-zero Lux but hardly any lux variation for at 
  # least an hour or variance in the derivative of the lux.
  classification = rep(0, N)
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