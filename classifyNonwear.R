classifyNonwear = function(data, # assumed to have columns time and lux
                           resolution_seconds = 60, # for computational only deride statistics at 1 minute resolution
                           N_days_required_daily_stats = 3,
                           minimum_relval_per_hour = 0.1, #minimum relative variance per hour
                           epoch_size = 5 # epoch size in seconds
                           
) {
  # Code dependencies: slider
  
  step_size = resolution_seconds / epoch_size
  N = nrow(data)
  original_colnames = colnames(data)
  
  #==============================================
  # Declare local functions:
  p05 = function(x) { #5th percentile
    return(as.numeric(quantile(x, probs = 0.05)))
  }
  p95 = function(x) { #5th percentile
    return(as.numeric(quantile(x, probs = 0.95)))
  }
  # several daily stats
  aggDay = function(x) {
    z = (x - mean(x)) / sd(x)
    n_peaks = length(which(z > 3))
    return(c(mean_val = mean(x),
             n_peaks = n_peaks,
             n_values = length(x),
             p05 = p05(x)))
  }
  # helper function to ease applying 1 and 16 hour rolling window functions:
  rollApply = function(data, window_size_hours, FUN, step_size, N) {
    half_window_size = ceiling(window_size_hours * 60 * (60/epoch_size) / 2)
    x = slide_dbl(data,
                  ~FUN(.x),
                  .before = half_window_size,
                  .after = half_window_size,
                  .step = step_size,
                  .complete = FALSE)
    # interpolate NA values
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
  data$hour = floor((as.numeric(data$time) - as.numeric(data$time[1])) / 3600)
  data$day = as.Date(data$time)
  
  # daily aggregate
  daily_stats = aggregate(data$lux, by = list(data$day), FUN = aggDay)
  # per 16 hour window
  p95_per_16hours = rollApply(data$lux, window_size_hours = 16,
                              FUN = p95, step_size = step_size, N)
  # per hour
  min_per_hour = rollApply(data$lux, window_size_hours = 1,
                           FUN = min, step_size = step_size, N)
  sd_per_hour = rollApply(data$lux, window_size_hours = 1,
                          FUN = sd, step_size = step_size, N)
  mean_per_hour = rollApply(data$lux, window_size_hours = 1,
                            FUN = mean, step_size = step_size, N)
  p05_per_hour = rollApply(data$lux, window_size_hours = 1,
                           FUN = p05, step_size = step_size, N)
  # combine into one data.frame
  data = cbind(data, p95_per_16hours,
               min_per_hour, sd_per_hour, mean_per_hour, p05_per_hour)
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
  detect_A = which(data$p95_per_16hours < 50)
  if (length(detect_A) > 0) {
    data$nonwearA[detect_A] = 1
    # make sure all 16 hours are labelled and not just the epoch in the center
    data$nonwearA = rollApply(data$nonwearA, window_size_hours = 16,
                              FUN = max, step_size = step_size, N)
  }
  # B. Time windows with consistently a non-zero lux but hardly any variation for at 
  # least an hour.
  detect_B = which(data$min_per_hour > 0 & data$relvar_hour < minimum_relval_per_hour)
  if (length(detect_B) > 0) {
    data$nonwearB[detect_B] = 1
  }
  N_epochs_per_day = (60/epoch_size) * 24 * 60
  if (N / N_epochs_per_day > N_days_required_daily_stats) {
    daily_stats = as.data.frame(daily_stats$x)
    daily_stats$nonwearC = daily_stats$nonwearD = 0
    # C. Any day with both mean and peak values substantially lower
    # than other days in the recording.
    detect_C = which(daily_stats$n_values > N_epochs_per_day * 0.5 &
                       (daily_stats$mean_val < mean(daily_stats$mean_val) * 0.5 &
                          daily_stats$n_peaks < mean(daily_stats$n_peaks) * 0.5))
    if (length(detect_C) > 0) {
      daily_stats$nonwearC[detect_C] = 1
    }
    # D. Any day where the lower tail of the lux distribution is 
    # substantially higher than other days in the recording. 
    detect_D = which(abs((daily_stats$p05 - median(daily_stats$p05)) / sd(daily_stats$p05)) > 3)
    if (length(detect_D) > 0) {
      daily_stats$nonwearD[detect_D] = 1
    }
  }
  # interpolate daily stats to merge with rest of data
  daily_stats = daily_stats[rep(1:nrow(daily_stats), daily_stats$n_values),]
  daily_stats = as.data.frame(daily_stats)
  data = cbind(data, daily_stats)
  # remove temporary variables
  # data = data[,c(original_colnames, grep(pattern = "nonwear", x = colnames(data), value = TRUE))]
  return(data)
}