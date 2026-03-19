
#' SPlot data with abnormality classifications
#'
#' @description Plot data with abnormality classifications
#'
#' @param x Tibble, that holds at least columns Datetime, Lux, nonwearA, \cr
#' nonwearB, nonwearC, nonwearD, nonwearE, nonwearF
#'
#' @return A plot
#' 
#' @importFrom graphics abline legend lines par
#' 
#' @export

plotAbnormality = function(x) {
  # plot classifications in lower half of the plot
  scale_value = max(x$Lux, na.rm = TRUE) * 0.5
  offset_value = scale_value * 0.02
  colors = c("orange","brown", "cyan", "purple", "darkgreen", "grey", "pink", "red" , "lightblue")
  par(mar = c(5.1, 4.1, 4.1, 9))
  plot(x$Datetime, x$Lux, type = "l",
       xlab = "Time (with hourly grid lines)", ylab = "Lux", bty = "l")
  par(lwd = 6, lend = 2)
  # Vertical grid lines for each hour
  abline(v = x$Datetime[which(format(x$Datetime, "%M:%S") == "00:00")],
         lty = 3, lwd = 0.5, col = "black")
  # Classification based on each criteria (A-G)
  lines(x$Datetime, x$nonwearA * scale_value + offset_value, type = "l", xlab = "",
        main = "lux-based nonwear", col = colors[1])
  lines(x$Datetime, x$nonwearB * scale_value * 0.96 + offset_value, type = "l", col = colors[2])
  lines(x$Datetime, x$nonwearC * scale_value * 0.92  + offset_value, type = "l", col = colors[3])
  lines(x$Datetime, x$nonwearD * scale_value * 0.88 + offset_value, type = "l", col = colors[4])
  lines(x$Datetime, x$nonwearE * scale_value * 0.84  + offset_value, type = "l", col = colors[5])
  lines(x$Datetime, x$nonwearF * scale_value * 0.80  + offset_value, type = "l", col = colors[6])
  lines(x$Datetime, x$nonwearG * scale_value * 0.76 + offset_value, type = "l", col = colors[7])
  # Composite classification
  lines(x$Datetime, x$nonwear_estimate * scale_value * 0.72, type = "l", xlab = "", col = colors[8])
  # Reference classification
  lines(x$Datetime, x$nonwear_ref * scale_value * 0.68, type = "l", xlab = "", col = colors[9])
  # Plot original lux signal on top
  lines(x$Datetime, x$Lux, type = "l", lwd = 1)
  # Legend
  par(lwd = 1, xpd = TRUE)
  legend("topright", legend = c("criteria A", "criteria B", "criteria C", "criteria D",
                                "criteria E", "criteria F", "criteria G", "criteria A-G",
                                "reference"),
         lty = rep(1, 9), lwd = rep(6, 9), col = colors, inset = c(-0.25, 0))
}