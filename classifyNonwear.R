classifyNonwear = function(data, # assumed to have columns time and Lux, and represent a continuous regular time series
                           resolution_seconds = 60, # for computational reasons only derive statistics at 30 sec resolution
                           N_days_required_daily_stats = 3,
                           minimum_relval_per_hour = 0.1, #minimum relative variance per hour
                           epoch_size = 5, # epoch size in seconds
                           lowLuxThreshold = 50, # Lux below this value is considered closed to zero
                           maxLowLuxSequenceHours = 16) # max number of hours with low Lux
{
  if (length(table(diff(data$Datetime))) != 1) {
    stop("Irregular time series is not accepted for non-wear classification")
  }
  
  # Code dependencies: slider
  step_size = resolution_seconds / epoch_size
  N = nrow(data) #length(which(!is.na(data$Lux)))
  original_colnames = colnames(data)
  
  tempStats = temporalStatistics(data = data,
                                 epoch_size = epoch_size,
                                 lowLuxThreshold = lowLuxThreshold,
                                 maxLowLuxSequenceHours = maxLowLuxSequenceHours,
                                 step_size = step_size,
                                 N = N)
  
  data = tempStats$data
  daily_stats = tempStats$daily_stats
  #=============================================================================
  # Lux-based non-wear classification
  #=============================================================================
  # Initialise to 0 (nonwear = 0 reflects wear time, nonwear = 1 refletcs nonwear time)
  data$nonwearA = data$nonwearB = 0   
  
  # -------------------------------
  # Criteria A:
  # Any time window in the data longer than plausible sleep window (16 hours)
  # that is filled with mostly zeros.
  data$nonwearA = longLowValue(x = data$p95_per_16hours,
                               x_threshold = lowLuxThreshold,
                               window_size_hours = maxLowLuxSequenceHours,
                               N = N,
                               step_size = step_size,
                               epoch_size = epoch_size)
  #--------------------------------------------
  # Criteria B:
  # Time windows with consistently a non-zero Lux but hardly any lux variation for at
  # least an hour or variance in the derivative of the lux.
  data$nonwearB = constantNonZeroLux(data, minimum_relval_per_hour, N)
  
  #--------------------------------------------
  # Criteria C, D, E and F
  N_epochs_per_day = (60/epoch_size) * 24 * 60
  daily_stats$nonwearC = daily_stats$nonwearD = daily_stats$nonwearE = daily_stats$nonwearF = 0
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
    daily_stats$nonwearC[detect_C] = 1
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
    daily_stats$nonwearD[detect_D] = 1
  }
  #--------------------------------------------
  # Criteria E:
  # Any day where the upper tail of the Lux distribution is 
  # substantially lower than other days in the recording.
  daily_stats$var_p95mean =  round(daily_stats$p95 / mean_daily_mean_lux, digits = 2)
  detect_E = which(daily_stats$var_p95mean < 2 & daily_stats$n_values > 1200)
  if (length(detect_E) > 0) {
    daily_stats$nonwearE[detect_E] = 1
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
    daily_stats$nonwearF[detect_F] = 1
  }
  #--------------------------------------------
  # merge daily_stats into data
  data$date = as.Date(data$Datetime)
  data$nonwearC = data$nonwearD = data$nonwearE = data$nonwearF = 0
  for (j in 1:nrow(daily_stats)) {
    data$nonwearC[which(data$date == daily_stats$day[j])] = daily_stats$nonwearC[j]
    data$nonwearD[which(data$date == daily_stats$day[j])] = daily_stats$nonwearD[j]
    data$nonwearE[which(data$date == daily_stats$day[j])] = daily_stats$nonwearE[j]
    data$nonwearF[which(data$date == daily_stats$day[j])] = daily_stats$nonwearF[j]
  }
  #--------------------------------------------
  # Criteria G:
  # Long time jumps are always nonwear
  # Here defined as NA percentage per hour being NA or 100.
  time_jumps = which(is.na(data$NAper_per_hour) | data$NAper_per_hour == 100)
  data$nonwearG = 0
  if (length(time_jumps) > 0) {
    data$nonwearG[time_jumps] = 1
  }
  #--------------------------------------------
  # Composite score by colapsing all scores into one
  hasNonZero = function(x) {
    if (any(x != 0, na.rm = TRUE)) {
      return(1)
    } else {
      return(0)
    }
  }
  nonwear_col_names = c("nonwearA", "nonwearB", "nonwearC", "nonwearD",
                        "nonwearE","nonwearF", "nonwearG")
  data$nonwear_estimate = apply(data[, nonwear_col_names], 1, hasNonZero)
  # Remove temporary variables
  # data = data[,c(original_colnames, grep(pattern = "nonwear", x = colnames(data), value = TRUE))]
  return(data)
}