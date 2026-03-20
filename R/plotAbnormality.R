
#' SPlot data with abnormality classifications
#'
#' @description Plot data with abnormality classifications
#'
#' @param x Tibble, that holds at least columns Datetime, Lux, indicator_A, \cr
#' indicator_B, indicator_C, indicator_D, indicator_E, indicator_F
#'
#' @return A plot
#' 
#' @importFrom graphics abline legend lines par
#' @importFrom grDevices rainbow
#' 
#' @export

plotAbnormality = function(x) {
  # Extract indicator names
  indicator_col_names = grep(pattern = "indicator_", x = colnames(x), value = TRUE)
  N_indicators = length(indicator_col_names)
  # Generate colours for the indicators, the composite indicator and the reference value
  colors = rainbow(N_indicators + 2)
  # Define scale and offsets for the positioning of the horizontal lines
  scale_value = max(x$Lux, na.rm = TRUE) * 0.5
  offset_value = scale_value * 0.02
  spacing = scale_value * 0.04
  # Plotting
  par(mar = c(5.1, 4.1, 4.1, 9)) 
  # Line for lux values
  plot(x$Datetime, x$Lux, type = "l",
       xlab = "Time (with hourly grid lines)", ylab = "Lux", bty = "l")
  par(lwd = 6, lend = 2)
  # Vertical grid lines for each hour
  abline(v = x$Datetime[which(format(x$Datetime, "%M:%S") == "00:00")],
         lty = 3, lwd = 0.5, col = "black") 
  # Line for each indicator (criteria)
  for (i in 1:length(indicator_col_names)) {
    indicator_name = indicator_col_names[i]
    if (i == 1) {
      lines(x$Datetime, unlist(x[, indicator_name]) * scale_value + offset_value, type = "l", xlab = "",
            main = "lux-based indicator_", col = colors[i])
    } else {
      lines(x$Datetime, unlist(x[, indicator_name]) * scale_value + offset_value, type = "l", col = colors[i])
    }
    scale_value = scale_value - spacing
  }
  # Line for the composite classification
  lines(x$Datetime, unlist(x$comp_estimate) * scale_value, type = "l", col = colors[i + 1])
  scale_value = scale_value - spacing
  # Line for reference classification
  lines(x$Datetime, unlist(x$nonwear_ref) * scale_value, type = "l", col = colors[i + 2])
  scale_value = scale_value - spacing
  # Line for original lux signal on top
  lines(x$Datetime, x$Lux, type = "l", lwd = 1)
  # Legend to explain colours of all lines
  par(lwd = 1, xpd = TRUE)
  legend_names = c(indicator_col_names, "any criteria", "reference")
  Nleg = length(legend_names)
  legend("topright", legend = legend_names,
         lty = rep(1, Nleg), lwd = rep(6, Nleg), col = colors, inset = c(-0.25, 0))
}