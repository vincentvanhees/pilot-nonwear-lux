
#' Add classifications of potential abnormalities to input data.
#'
#' @description Add classifications of potential abnormalities to input data.
#'
#' @param data Tibble, that holds at least a Datetime and Lux column. \cr
#' Date time is expected to be a regular time series. \cr
#' If columns weight_X is included then this will be used as weighting for criteria X \cr
#' , e.g. weight_A for criteria A.
#' @param resolution_seconds Numeric, resolution in seconds at which to perform analysis. \cr
#' When epoch_size is small, consider setting this to a multitude of the \code{epoch_size} to speed up processing.
#' @param N_days_required_daily_stats Numeric, integer to indicate the minimum number \cr
#'  of days required for deriving daily statistics
#' @param minimum_relval_per_hour Numeric, minimum relative variance per hour
#' @param epoch_size Numeric, epoch size  of input data in seconds
#' @param lowLuxThreshold Numeric, Lux value below which Lux is considered zero (darkness).
#' @param maxLowLuxSequenceHours Numeric, maximum number of hours with low lux
#' @param userStatsFunction function as provide by user for deriving temporal statistics
#' @param userCriteriaFunction function as provide by user for applying a new criteria
#'
#' @return Tibble data provided as input enhanced with classifications
#' 
#' @importFrom stats aggregate median quantile sd
#' 
#' @export
classifyAbnormal = function(data,
                           resolution_seconds = 60, 
                           N_days_required_daily_stats = 3,
                           minimum_relval_per_hour = 0.1,
                           epoch_size = 5,
                           lowLuxThreshold = 50,
                           maxLowLuxSequenceHours = 16,
                           userStatsFunction = NULL,
                           userCriteriaFunction = NULL) 
{
  if (length(table(diff(data$Datetime))) != 1) {
    stop("Irregular time series is not accepted for non-wear classification")
  }
  step_size = resolution_seconds / epoch_size
  N = nrow(data) #length(which(!is.na(data$Lux)))
  original_colnames = colnames(data)
  #============================================
  # Derive temporal statistics which are used across classification criteria further down
  tempStats = temporalStatistics(data = data,
                                 epoch_size = epoch_size,
                                 lowLuxThreshold = lowLuxThreshold,
                                 maxLowLuxSequenceHours = maxLowLuxSequenceHours,
                                 step_size = step_size,
                                 userStatsFunction = userStatsFunction)
  data = tempStats$data
  daily_stats = tempStats$daily_stats
  #============================================
  # Classify abnormality based on epoch-by-epoch time series (Criteria A-B)
  data$indicatorA = data$indicatorB = 0 # 0 = good, 1 = potentially abnormal
  # -------------------------------
  # Criteria A:
  # Any time window in the data longer than plausible sleep window (16 hours)
  # that is filled with mostly zeros.
  data$indicator_A = longLowValue(x = data$p95_per_16hours,
                               x_threshold = lowLuxThreshold,
                               window_size_hours = maxLowLuxSequenceHours,
                               step_size = step_size,
                               epoch_size = epoch_size)
  #--------------------------------------------
  # Criteria B:
  # Time windows with consistently a non-zero Lux but hardly any lux variation for at
  # least an hour or variance in the derivative of the lux.
  data$indicator_B = constantNonZeroLux(data, minimum_relval_per_hour)
  #============================================
  # Classify abnormality based on daily statistics (Criteria C-F)
  N_epochs_per_day = (60/epoch_size) * 24 * 60
  daily_stats$indicator_C = daily_stats$indicator_D = daily_stats$indicator_E = daily_stats$indicator_F = 0
  # Assess whether there are 3 days of data
  derive_ref_values = N / N_epochs_per_day > N_days_required_daily_stats
  if (derive_ref_values) { 
    # More than 3 days of data
    # Derive various reference values needed for criteria C, D, E and/or F
    mean_daily_mean_lux = mean(daily_stats$mean_val) # Criteria C + E
    mean_daily_n_peaks = mean(daily_stats$n_peaks) # Criteria C
    median_daily_p05 = median(daily_stats$p05) # Criteria D
    sd_daily_p05 = sd(daily_stats$p05) # Criteria D
    median_daily_ncnz = median(daily_stats$ncnz) # Criteria F 
  } else {
    # Less than 3  days of data
    # Use hardcoded reference values for criteria C, D, E and/or F
    cat("Criteria C-F rely on reference values as there are less than 3 days of data.\n")
    mean_daily_mean_lux = 277.6518 # Criteria C + E
    mean_daily_n_peaks = 78.57143 # Criteria C
    median_daily_p05 = 0.001733333 # Criteria D
    sd_daily_p05 = 3.147352 # Criteria D
    median_daily_ncnz = 5662 # Criteria F 
  }
  # Criteria C:
  # Any day with both mean and peak values substantially lower
  # than other days in the recording and more than half a day worth of data points.
  detect_C = which((daily_stats$mean_val < mean_daily_mean_lux * 0.5 &
                      daily_stats$n_peaks < mean_daily_n_peaks * 0.5) &
                     daily_stats$n_values > N_epochs_per_day * 0.5)
  if (length(detect_C) > 0) {
    daily_stats$indicator_C[detect_C] = 1
  }
  #--------------------------------------------
  # Criteria D:
  # Any day where the lower tail of the Lux distribution is 
  # substantially higher than other days in the recording. 
  # Implemented here as the difference between the 5th percentile and the median 
  # daily 5th percentile being more than 3 times the standard deviation of the
  # 5th percentile
  detect_D = which(daily_stats$p05 - median_daily_p05 > 3 * sd_daily_p05)
  if (length(detect_D) > 0) {
    daily_stats$indicator_D[detect_D] = 1
  }
  #--------------------------------------------
  # Criteria E:
  # Any day where the upper tail of the Lux distribution is 
  # substantially lower than other days in the recording.
  daily_stats$var_p95mean =  round(daily_stats$p95 / mean_daily_mean_lux, digits = 2)
  detect_E = which(daily_stats$var_p95mean < 2 & daily_stats$n_values > 1200)
  if (length(detect_E) > 0) {
    daily_stats$indicator_E[detect_E] = 1
  }
  #--------------------------------------------
  # Criteria F:
  # Long sequences of non-zero, relative to other days in the recording and at least 300
  # minutes in absolute duration.
  # Implemented as proportion of longest number of consecutive non-zeros on a certain day
  # relative to its median value acros days being larger than 5.
  daily_stats$var_ncnz =  round(daily_stats$ncnz / median_daily_ncnz, digits = 2)
  detect_F = which(daily_stats$var_ncnz > 5 &
                     daily_stats$ncnz > 300 & daily_stats$n_values > 1200)
  if (length(detect_F) > 0) {
    daily_stats$indicator_F[detect_F] = 1
  }
  #--------------------------------------------
  # merge daily_stats into data
  data$date = as.Date(data$Datetime)
  data$indicator_C = data$indicator_D = data$indicator_E = data$indicator_F = 0
  for (j in 1:nrow(daily_stats)) {
    data$indicator_C[which(data$date == daily_stats$day[j])] = daily_stats$indicator_C[j]
    data$indicator_D[which(data$date == daily_stats$day[j])] = daily_stats$indicator_D[j]
    data$indicator_E[which(data$date == daily_stats$day[j])] = daily_stats$indicator_E[j]
    data$indicator_F[which(data$date == daily_stats$day[j])] = daily_stats$indicator_F[j]
  }
  #--------------------------------------------
  # Criteria G:
  # Long time jumps are always nonwear
  # Here defined as NA percentage per hour being NA or 100.
  time_jumps = which(is.na(data$NAper_per_hour) | data$NAper_per_hour == 100)
  data$indicator_G = 0
  if (length(time_jumps) > 0) {
    data$indicator_G[time_jumps] = 1
  }
  
  #--------------------------------------------
  # Criteria H:
  if (!is.null(userCriteriaFunction)) {
    data$indicator_H = 0
    data$indicator_H = userCriteriaFunction(data)
  }
  
  #===========================================
  # Apply indicator specific weights if available.
  # For example, weights_A are applied to indicator_A
  indicator_col_names = grep(pattern = "indicator_", x = colnames(data), value = TRUE)
  for (j in 1:length(indicator_col_names)) {
    weight_column = gsub(pattern = "indicator", replacement = "weight", x = indicator_col_names[j])
    if (weight_column %in% colnames(data)) {
      data[, indicator_col_names[j]] = data[, indicator_col_names[j]] * data[, weight_column]
    }
  }
  #--------------------------------------------
  # Create composite indicator by collapsing all indicators into one
  hasNonZero = function(x) {
    if (any(x != 0, na.rm = TRUE)) {
      return(1)
    } else {
      return(0)
    }
  }
  
  data$comp_estimate = apply(data[, indicator_col_names], 1, hasNonZero)
  # Remove temporary variables
  # data = data[,c(original_colnames, grep(pattern = "indicator", x = colnames(data), value = TRUE))]
  return(data)
}