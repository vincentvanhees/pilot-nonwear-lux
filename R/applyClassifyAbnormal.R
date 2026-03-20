
#' Split data in segments if necessary and apply classifyAbnormal
#'
#' @description Split data in segments if necessary and apply classifyAbnormal
#'
#' @param data Tibble, that holds at least a Datetime and Lux column. \cr
#' Date time is expected to be a regular time series. \cr
#' If columns weight_X is included then this will be used as weighting for criteria X \cr
#' , e.g. weight_A for criteria A.
#' @param resolution_seconds Numeric, resolution in seconds as passed on to \link{classifyAbnormal}
#' @param N_days_required_daily_stats Numeric, integer to indicate the minimum number \cr
#'  of days required for deriving daily statistics as passed on to \link{classifyAbnormal}
#' @param minimum_relval_per_hour Numeric, minimum relative variance per hour as passed on to \link{classifyAbnormal}
#' @param epoch_size Numeric, epoch size  of input data in seconds
#' @param lowLuxThreshold Numeric, Lux value below which Lux is considered zero (darkness) as passed on to \link{classifyAbnormal}.
#' @param maxLowLuxSequenceHours Numeric, maximum number of hours with low lux as passed on to \link{classifyAbnormal}
#' @param plot_path Character, to indicate the folder path where to store output plot in png format
#' @param plot_id Character or numeric to be appended to the end of the plot file name
#' @param userStatsFunction function as provide by user for deriving temporal statistics
#' @param userCriteriaFunction function as provide by user for applying a new criteria
#'
#' @return Tibble data provided as input enhanced with classifications
#' 
#' @importFrom grDevices dev.off png
#' 
#' @export

applyClassifyAbnormal = function(data,
                                resolution_seconds = 60,
                                N_days_required_daily_stats = 3,
                                minimum_relval_per_hour = 0.1,
                                epoch_size = 5,
                                lowLuxThreshold = 50,
                                maxLowLuxSequenceHours = 16,
                                plot_path = NULL,
                                plot_id = "",
                                userStatsFunction = NULL,
                                userCriteriaFunction = NULL) {
  # Remove duplicates
  data = data[!duplicated(data),]
  
  # If there are timegaps, they can be too large for LightLogR::gap_handler to handle
  # instead split data in segments that has gaps shorter than 2 hours
  delta = as.numeric(diff(data$Datetime))
  gaps = which(delta > 3600 * 2) # only split if gaps are larger than 2 hours
  data_out = NULL
  # Loop over segments
  for (i in 1:(length(gaps) + 1)) {
    cat(paste0("segment ", i, "\n"))
    # Define start and end index of segment
    start = ifelse(i == 1, yes = 1, no = gaps[i - 1] + 1)
    end = ifelse(i <= length(gaps), yes = gaps[i], no = nrow(data))
    segment_size_hours = ((end - start) + 1) / (3600 / epoch_size)
    # Only consider segments larger than 6 hours
    if (segment_size_hours > 6) { 
      data_subset = data[start:end, ]
      # Now impute remaining timegaps with NA values
      data_subset <- data_subset |>
        LightLogR::add_Date_col(group.by = TRUE) |>
        LightLogR::gap_handler(full.days = TRUE)
      # Remove duplicate timestamps inserted by gap_handler function.
      data_subset = data_subset[!duplicated(data_subset$Datetime),]
      # Apply classifier
      data_subset = classifyAbnormal(data = data_subset,
                                    resolution_seconds = resolution_seconds, # for computational only derive statistics at 1 minute resolution
                                    N_days_required_daily_stats = N_days_required_daily_stats,
                                    minimum_relval_per_hour = minimum_relval_per_hour, #minimum relative variance per hour
                                    epoch_size = epoch_size,
                                    lowLuxThreshold = lowLuxThreshold, # Lux below this value is considered closed to zero
                                    maxLowLuxSequenceHours = maxLowLuxSequenceHours,
                                    userStatsFunction = userStatsFunction,
                                    userCriteriaFunction = userCriteriaFunction)
      # Generate plot if plot_path is specified
      if (!is.null(plot_path)) {
        # Set wear to NA to ease plot
        indicator_col_names = grep(pattern = "indicator_", x = colnames(data_subset), value = TRUE)
        for (j in 1:length(indicator_col_names)) {
          indicator_name = indicator_col_names[j]
          is.na(data_subset[which(data_subset[, indicator_name] == 0), indicator_name]) = TRUE
        }
        is.na(data_subset$comp_estimate[which(data_subset$comp_estimate == 0)]) = TRUE
        is.na(data_subset$nonwear_ref[which(data_subset$nonwear_ref == 0)]) = TRUE
        if (length(which(is.na(data_subset$Lux)))) {
          is.na(data_subset$Lux[which(is.na(data_subset$Lux))]) = TRUE
        }
        #----------------------------------
        # Generate png file
        png(filename = paste0(plot_path, "/", plot_id, "_segment_", i, ".png"), width = 9, height = 6, units = "in", res = 300)
        plotAbnormality(data_subset)
        dev.off()
      }
      # Append the segments such that there is one output object
      data_out = rbind(data_out, data_subset)
      rm(data_subset)  
    } else {
      cat(paste0("skipped because length of ", segment_size_hours, " hour is too short.\n"))
    }
  }
  return(data_out)
}