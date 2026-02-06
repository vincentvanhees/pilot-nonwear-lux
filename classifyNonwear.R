classifyNonwear = function(data, # assumed to have columns time and Lux, and represent a continuous regular time series
                           resolution_seconds = 60, # for computational only deride statistics at 1 minute resolution
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
  
  #==============================================
  # Declare local functions:
  p05 = function(x) { #5th percentile
    return(as.numeric(quantile(x, probs = 0.05, na.rm = TRUE)))
  }
  p95 = function(x) { #5th percentile
    return(as.numeric(quantile(x, probs = 0.95, na.rm = TRUE)))
  }
  
  NA_percentage = function(x) {
    return((length(which(is.na(x))) / length(x)) * 100)
  }
  
  n_consecutive_nonzero = function(x) {
    nonzero = x != 0
    if (any(nonzero, na.rm = TRUE)) {
      y = rle(nonzero)
      z = max(y$lengths[y$values == TRUE], na.rm = TRUE)
    } else {
      z = 0
    }
    return(z)
  }
  
  # several daily stats
  aggDay = function(x) {
    NAper = NA_percentage(x)
    if (NAper < 25) {
      mean_val = mean(x, na.rm = TRUE)
      z = (x - mean_val) / sd(x)
      n_peaks = length(which(z > 3))
      n_values = length(x)
      p95 = p95(x)
      ncn = n_consecutive_nonzero(x)
    } else {
      mean_val = NA
      n_peaks = NA
      n_values = NA
      p95 = NA
      ncn = NA
    }
    return(c(mean_val = mean_val,
             n_peaks = n_peaks,
             n_values = n_values,
             p95 = p95,
             ncn = ncn,
             NAper = NAper))
  }
  # helper function to ease applying 1 and 16 hour rolling window functions:
  rollApply = function(data, window_size_hours, FUN, step_size, N) {
    half_window_size = ceiling(window_size_hours * 60 * (60/epoch_size) / 2)
    x = slider::slide_dbl(data,
                          ~FUN(.x),
                          .before = half_window_size,
                          .after = half_window_size,
                          .step = step_size,
                          .complete = FALSE)
    # interpolate NA values created by slide_dbl because of steps it takes
    valid = which(!is.na(x) == TRUE)
    half_step_size = round(step_size / 2)
    for (i in c(-half_step_size:-1, 1:half_step_size)) {
      validi = valid + i
      select = which(validi > 0 & validi <= N)
      x[validi[select]] = x[valid[select]]
    }
    return(x)
  }
  
  #=============================================================================
  # Derive statistics as needed for classification
  data$hour = floor((as.numeric(data$Datetime) - as.numeric(data$Datetime[1])) / 3600)
  data$day = as.Date(data$Datetime)
  
  # daily aggregate
  daily_stats = aggregate(data$Lux, by = list(data$day), FUN = aggDay)
  daily_stats = as.data.frame(daily_stats$x)
  daily_stats$day = unique(data$day)
  # per 16 hour window
  p95_per_16hours = rollApply(data$Lux, window_size_hours = maxLowLuxSequenceHours,
                              FUN = p95, step_size = step_size, N)
  NAper_per_16hours = rollApply(data$Lux, window_size_hours = maxLowLuxSequenceHours,
                                FUN = NA_percentage, step_size = step_size, N)
  # per hour
  min_per_hour = rollApply(data$Lux, window_size_hours = 1,
                           FUN = min, step_size = step_size, N)
  sd_per_hour = rollApply(data$Lux, window_size_hours = 1,
                          FUN = sd, step_size = step_size, N)
  mean_per_hour = rollApply(data$Lux, window_size_hours = 1,
                            FUN = mean, step_size = step_size, N)
  p05_per_hour = rollApply(data$Lux, window_size_hours = 1,
                           FUN = p05, step_size = step_size, N)
  NAper_per_hour = rollApply(data$Lux, window_size_hours = 1,
                             FUN = NA_percentage, step_size = step_size, N)
  
  # combine into one data.frame
  data = cbind(dplyr::ungroup(data), p95_per_16hours, NAper_per_16hours,
               min_per_hour, sd_per_hour, mean_per_hour, p05_per_hour,
               NAper_per_hour)
  # convert back to tibble as that is what LightLogR expects
  data = dplyr::group_by(data)
  
  # add measure of relative variance per hour
  data$relvar_hour = 0
  nonzero = which(data$mean_per_hour != 0)
  if (length(nonzero) > 0) {
    data$relvar_hour[nonzero] = data$sd_per_hour[nonzero] / data$mean_per_hour[nonzero]
  }
  #=============================================================================
  # Classification
  data$nonwearA = data$nonwearB = 0
  
  # A. Any time window in the data longer than plausible sleep window (16 hours)
  # that is filled with mostly zeros.
  detect_A = which(data$p95_per_16hours < lowLuxThreshold)
  if (length(detect_A) > 0) {
    data$nonwearA[detect_A] = 1
    # make sure all 16 hours are labelled and not just the epoch in the center
    data$nonwearA = rollApply(data$nonwearA, window_size_hours = maxLowLuxSequenceHours,
                              FUN = max, step_size = step_size, N)
  }
  # B. Time windows with consistently a non-zero Lux but hardly any variation for at 
  # least an hour
  detect_B = which((data$min_per_hour > pmin(min(data$min_per_hour, na.rm = TRUE), 1) &
                     data$relvar_hour < minimum_relval_per_hour))
  if (length(detect_B) > 0) {
    data$nonwearB[detect_B] = 1
  }
  N_epochs_per_day = (60/epoch_size) * 24 * 60
  
  daily_stats$nonwearC = daily_stats$nonwearD = daily_stats$nonwearE = daily_stats$nonwearF = 0
  if (N / N_epochs_per_day > N_days_required_daily_stats) {
    # C. Any day with both mean and peak values substantially lower
    # than other days in the recording and more than half a day worth of data points.
    detect_C = which(daily_stats$n_values > N_epochs_per_day * 0.5 &
                       (daily_stats$mean_val < mean(daily_stats$mean_val) * 0.5 &
                          daily_stats$n_peaks < mean(daily_stats$n_peaks) * 0.5))
    if (length(detect_C) > 0) {
      daily_stats$nonwearC[detect_C] = 1
    }
    # D. Any day where the lower tail of the Lux distribution is 
    # substantially higher than other days in the recording. 
    detect_D = which(abs((daily_stats$p05 - median(daily_stats$p05)) / sd(daily_stats$p05)) > 3)
    if (length(detect_D) > 0) {
      daily_stats$nonwearD[detect_D] = 1
    }
    # E. Any day where the upper tail of the Lux distribution is 
    # substantially lower than other days in the recording.
    daily_stats$var_p95mean =  round(daily_stats$p95 / mean(daily_stats$mean_val), digits = 2)
    detect_E = which(daily_stats$var_p95mean < 2 & daily_stats$n_values > 1200)
    if (length(detect_E) > 0) {
      daily_stats$nonwearE[detect_E] = 1
    }
    # F. Long sequences of non-zero, relative to other days in the recording and at least 300
    # minutes in absolute terms
    daily_stats$var_ncn =  round(daily_stats$ncn / median(daily_stats$ncn), digits = 2)
    detect_F = which(daily_stats$var_ncn > 5 &
                       daily_stats$ncn > 300 & daily_stats$n_values > 1200)
    if (length(detect_F) > 0) {
      daily_stats$nonwearF[detect_F] = 1
    }
  } else {
    cat("Criteria C-F skipped because not more than 3 days of data.\n")
  }
  # merge daily_stats into data
  data$date = as.Date(data$Datetime)
  data$nonwearC = data$nonwearD = data$nonwearE = data$nonwearF = 0
  for (j in 1:nrow(daily_stats)) {
    data$nonwearC[which(data$date == daily_stats$day[j])] = daily_stats$nonwearC[j]
    data$nonwearD[which(data$date == daily_stats$day[j])] = daily_stats$nonwearD[j]
    data$nonwearE[which(data$date == daily_stats$day[j])] = daily_stats$nonwearE[j]
    data$nonwearF[which(data$date == daily_stats$day[j])] = daily_stats$nonwearF[j]
  }
  
  # G Long time jumps are always nonwear
  time_jumps = which(is.na(data$NAper_per_hour))
  data$nonwearG = 0
  if (length(time_jumps) > 0) {
    data$nonwearG[time_jumps] = 1
  }
  # Composite score
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
  # remove temporary variables
  # data = data[,c(original_colnames, grep(pattern = "nonwear", x = colnames(data), value = TRUE))]
  return(data)
}