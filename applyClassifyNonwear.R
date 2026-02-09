applyClassifyNonwear = function(data, # assumed to have columns time and Lux, and represent a continuous regular time series
                                resolution_seconds = 60, # for computational only deride statistics at 1 minute resolution
                                N_days_required_daily_stats = 3,
                                minimum_relval_per_hour = 0.1, #minimum relative variance per hour
                                epoch_size = 5, # epoch size in seconds
                                lowLuxThreshold = 50, # Lux below this value is considered closed to zero
                                maxLowLuxSequenceHours = 16, # max number of hours with low Lux
                                plot_path = NULL,
                                plot_id = "") {
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
      data_subset = classifyNonwear(data = data_subset,
                                    resolution_seconds = resolution_seconds, # for computational only derive statistics at 1 minute resolution
                                    N_days_required_daily_stats = N_days_required_daily_stats,
                                    minimum_relval_per_hour = minimum_relval_per_hour, #minimum relative variance per hour
                                    epoch_size = epoch_size,
                                    lowLuxThreshold = lowLuxThreshold, # Lux below this value is considered closed to zero
                                    maxLowLuxSequenceHours = maxLowLuxSequenceHours)
      # Generate plot if plot_path is specified
      if (!is.null(plot_path)) {
        # Set wear to NA to ease plot
        is.na(data_subset$nonwearA[which(data_subset$nonwearA == 0)]) = TRUE
        is.na(data_subset$nonwearB[which(data_subset$nonwearB == 0)]) = TRUE
        is.na(data_subset$nonwearC[which(data_subset$nonwearC == 0)]) = TRUE
        is.na(data_subset$nonwearD[which(data_subset$nonwearD == 0)]) = TRUE
        is.na(data_subset$nonwearE[which(data_subset$nonwearE == 0)]) = TRUE
        is.na(data_subset$nonwearF[which(data_subset$nonwearF == 0)]) = TRUE
        is.na(data_subset$nonwearG[which(data_subset$nonwearG == 0)]) = TRUE
        is.na(data_subset$nonwear_estimate[which(data_subset$nonwear_estimate == 0)]) = TRUE
        is.na(data_subset$nonwear_ref[which(data_subset$nonwear_ref == 0)]) = TRUE
        if (length(which(is.na(data_subset$Lux)))) {
          is.na(data_subset$Lux[which(is.na(data_subset$Lux))]) = TRUE
        }
        #----------------------------------
        # Generate png file
        png(filename = paste0(plot_path, "/", plot_id, "_segment_", i, ".png"), width = 9, height = 6, units = "in", res = 300)
        # plot classifications in lower half of the plot
        scale_value = max(data_subset$Lux, na.rm = TRUE) * 0.5
        offset_value = scale_value * 0.02
        colors = c("orange","brown", "cyan", "purple", "darkgreen", "grey", "pink", "red" , "lightblue")
        par(mar = c(5.1, 4.1, 4.1, 9))
        plot(data_subset$Datetime, data_subset$Lux, type = "l",
             xlab = "Time (with hourly grid lines)", ylab = "Lux", bty = "l")
        par(lwd = 6, lend = 2)
        # Vertical grid lines for each hour
        abline(v = data_subset$Datetime[which(format(data_subset$Datetime, "%M:%S") == "00:00")],
               lty = 3, lwd = 0.5, col = "black")
        # Classification based on each criteria (A-G)
        lines(data_subset$Datetime, data_subset$nonwearA * scale_value + offset_value, type = "l", xlab = "",
              main = "lux-based nonwear", col = colors[1])
        lines(data_subset$Datetime, data_subset$nonwearB * scale_value * 0.96 + offset_value, type = "l", col = colors[2])
        lines(data_subset$Datetime, data_subset$nonwearC * scale_value * 0.92  + offset_value, type = "l", col = colors[3])
        lines(data_subset$Datetime, data_subset$nonwearD * scale_value * 0.88 + offset_value, type = "l", col = colors[4])
        lines(data_subset$Datetime, data_subset$nonwearE * scale_value * 0.84  + offset_value, type = "l", col = colors[5])
        lines(data_subset$Datetime, data_subset$nonwearF * scale_value * 0.80  + offset_value, type = "l", col = colors[6])
        lines(data_subset$Datetime, data_subset$nonwearG * scale_value * 0.76 + offset_value, type = "l", col = colors[7])
        # Composite classification
        lines(data_subset$Datetime, data_subset$nonwear_estimate * scale_value * 0.72, type = "l", xlab = "", col = colors[8])
        # Reference classification
        lines(data_subset$Datetime, data_subset$nonwear_ref * scale_value * 0.68, type = "l", xlab = "", col = colors[9])
        # Plot original lux signal on top
        lines(data_subset$Datetime, data_subset$Lux, type = "l", lwd = 1)
        # Legend
        par(lwd = 1, xpd = TRUE)
        legend("topright", legend = c("criteria A", "criteria B", "criteria C", "criteria D",
                                      "criteria E", "criteria F", "criteria G", "criteria A-G",
                                      "reference"),
               lty = rep(1, 9), lwd = rep(6, 9), col = colors, inset = c(-0.25, 0))
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