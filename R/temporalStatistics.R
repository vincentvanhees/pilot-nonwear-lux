#' Derive temporal statistics from a tibble with time series
#'
#' @description Adds temporal statistics to tibble to a tibble with time series \cr
#' and derive a separate data.frame with daily statistics
#'
#' @param data Tibble, that holds at least a Datetime and Lux column
#' @param epoch_size Numeric, epoch size  of input data in seconds
#' @param lowLuxThreshold Numeric, Lux value below which Lux is considered zero (darkness).
#' @param maxLowLuxSequenceHours Numeric, maximum number of hours with low lux
#' @param step_size Numeric, used as input for function \link{rollApply}
#' 
#' @export
#'
#' @return Tibble data provided as input enhanced with temporal statistics
temporalStatistics = function(data,
                              epoch_size = 5,
                              lowLuxThreshold = 50,
                              maxLowLuxSequenceHours = 16,
                              step_size = NULL)
{
  #=============================================================================
  # Declare local functions to aid derivation of temporal statistics:
  p05 = function(x) { #5th percentile of x
    return(as.numeric(quantile(x, probs = 0.05, na.rm = TRUE)))
  }
  p95 = function(x) { #5th percentile of x
    return(as.numeric(quantile(x, probs = 0.95, na.rm = TRUE)))
  }
  NA_percentage = function(x) { # percentage of NA values in x
    return((length(which(is.na(x))) / length(x)) * 100)
  }
  n_consecutive_nonzero = function(x, lowLuxThreshold) { # Length of longest sequences of zeros
    nonzero = x < lowLuxThreshold
    if (any(nonzero, na.rm = TRUE)) {
      y = rle(nonzero)
      z = max(y$lengths[y$values == TRUE], na.rm = TRUE)
    } else {
      z = 0
    }
    return(z)
  }
  # One function to derive multiple stats per calendar day
  aggDay = function(x, lowLuxThreshold = lowLuxThreshold) {
    NAper = NA_percentage(x)
    mean_val = n_peaks = n_values = NA
    p05 = p95 = ncnz = NA
    if (NAper < 25) {
      mean_val = mean(x, na.rm = TRUE)
      z = (x - mean_val) / sd(x)
      n_peaks = length(which(z > 3))
      n_values = length(x)
      p05 = p05(x)
      p95 = p95(x)
      ncnz = n_consecutive_nonzero(x, lowLuxThreshold)
    }
    return(c(mean_val = mean_val,
             n_peaks = n_peaks,
             n_values = n_values,
             p05 = p05,
             p95 = p95,
             ncnz = ncnz,
             NAper = NAper))
  }
  #=============================================================================
  # Derive temporal statistics as needed for classification
  # add hour and day column
  data$hour = floor((as.numeric(data$Datetime) - as.numeric(data$Datetime[1])) / 3600)
  data$day = as.Date(data$Datetime)
  # daily aggregate
  daily_stats = aggregate(data$Lux, by = list(data$day), FUN = aggDay, lowLuxThreshold = lowLuxThreshold)
  daily_stats = as.data.frame(daily_stats$x)
  daily_stats$day = unique(data$day)
  # rolling 16 hour window aggregates
  p95_per_16hours = rollApply(data$Lux, window_size_hours = maxLowLuxSequenceHours,
                              FUN = p95, step_size = step_size, epoch_size)
  NAper_per_16hours = rollApply(data$Lux, window_size_hours = maxLowLuxSequenceHours,
                                FUN = NA_percentage, step_size = step_size, epoch_size)
  # rolling 1 hour aggregates
  min_per_hour = rollApply(data$Lux, window_size_hours = 1,
                           FUN = min, step_size = step_size, epoch_size)
  sd_per_hour = rollApply(data$Lux, window_size_hours = 1,
                          FUN = sd, step_size = step_size, epoch_size)
  sd_diff_per_hour = rollApply(c(diff(data$Lux), 0), window_size_hours = 1,
                               FUN = sd, step_size = step_size, epoch_size)
  mean_per_hour = rollApply(data$Lux, window_size_hours = 1,
                            FUN = mean, step_size = step_size, epoch_size)
  p05_per_hour = rollApply(data$Lux, window_size_hours = 1,
                           FUN = p05, step_size = step_size, epoch_size)
  NAper_per_hour = rollApply(data$Lux, window_size_hours = 1,
                             FUN = NA_percentage, step_size = step_size, epoch_size)
  # combine into one data.frame
  data = cbind(dplyr::ungroup(data), p95_per_16hours, NAper_per_16hours,
               min_per_hour, sd_per_hour, mean_per_hour, p05_per_hour,
               NAper_per_hour, sd_diff_per_hour)
  # convert back to tibble as that is what LightLogR expects
  data = dplyr::group_by(data)
  # add measure of relative variance per hour
  data$relvar_hour = 0
  nonzero = which(data$mean_per_hour != 0)
  if (length(nonzero) > 0) {
    data$relvar_hour[nonzero] = data$sd_per_hour[nonzero] / data$mean_per_hour[nonzero]
  }
  invisible(list(data = data, daily_stats = daily_stats))
}