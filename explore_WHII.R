rm(list = ls())
graphics.off()

library(slider)

# load time series WHII:
path1 = "D:/Projects/ERC_Paris/Whitehall/output_bins_test_batch/meta/basic"
path2 = "D:/Projects/ERC_Paris/Whitehall/output_bins_test_batch/meta/ms2.out"
fnames1 = dir(path = path1, full.names = TRUE)
fnames2 = dir(path = path2, full.names = TRUE)

k = 60
myVar = function(x) {
  return(mean(abs(diff(x))))
}

for (i in 1:length(fnames1)) {
  print("----------------------------------------")
  print(paste0("i ", i))
  M = NULL
  load(fnames1[i])
  j = grep(pattern = unlist(strsplit(basename(fnames1[i]), "meta_"))[2],
        x = basename(fnames2))
  load(fnames2[j])
  nonweari = which(IMP$rout$r5 == 1)
  weari = which(IMP$rout$r5 == 0)
  M$metalong$timestamp = as.POSIXct(M$metalong$timestamp,
                           format = "%Y-%m-%dT%H:%M:%S%z",
                           tz = "Europe/London")
  N = M$metalong[nonweari,]
  W = M$metalong[weari,]
  W = W[1:nrow(N),]
  
  data = data.frame(time = M$metalong$timestamp, lux = M$metalong$lightpeak * 100)
  source(paste0(getwd(), "/classifyNonwear.R"))
  data = classifyNonwear(data,
                         resolution_seconds = 60, # for computational only derive statistics at 1 minute resolution
                         N_days_required_daily_stats = 3,
                         minimum_relval_per_hour = 0.1, #minimum relative variance per hour
                         epoch_size = 60)
  
  # Plot
  YLIM = c(-200, 200)
  YLIM2 = c(0, 200)
  png(filename = paste0("D:/Projects/Spitschan/WHexplore_", i, ".png"),
      width = 9, height = 7, units = "in", res = 300)
  par(mfcol = c(3, 3), oma = c(0,0,0, 0), mar = c(4,4,3,1))
  # Column 1
  timeshifts = which(diff(as.numeric(N$timestamp)) > 900)
  plot(N$lightpeak, main = "Peak Lux per minute - nonwear",
       type = "l", ylim = YLIM2, bty = "l", xlab = "", ylab = "lux")
  abline(v = timeshifts, col = "red")
  plot(diff(N$lightpeak), main = "Derivative of Peak Lux per minute - nonwear",
       type = "l", ylim = YLIM, bty = "l", xlab = "", ylab = "lux")
  abline(v = timeshifts, col = "red")
  plot(N$EN, main = "Acceleration (VM) per minute - nonwear",
       type = "l", ylim = c(0.1, 1.5), bty = "l",
       xlab = "minutes", ylab = substitute(paste(italic('g'))))
  abline(v = timeshifts, col = "red")

  # Column 2
  timeshifts = which(diff(as.numeric(W$timestamp)) > 900)
  plot(W$lightpeak, main = "Peak Lux per minute - wear",
       type = "l", ylim = YLIM2, xlab = "", ylab = "lux")
  abline(v = timeshifts, col = "red")
  plot(diff(W$lightpeak), main = "Derivative of Peak Lux per minute - wear",
       type = "l", ylim = YLIM, xlab = "", ylab = "lux")
  abline(v = timeshifts, col = "red")
  plot(W$EN, main = "Acceleration (VM) per minute - wear",
       type = "l", ylim = c(0.1, 1.5),
       xlab = "minutes", ylab = substitute(paste(italic('g'))))
  abline(v = timeshifts, col = "red")
  
  # Column 3
  plot(M$metalong$timestamp, M$metalong$lightpeak, main = "Peak Lux per minute - all",
       type = "l", bty = "l", xlab = "time", ylab = "lux")
  M2 = M
  is.na(M2$metalong$lightpeak[which(IMP$rout$r5 == 0)]) = TRUE
  lines(M2$metalong$timestamp, M2$metalong$lightpeak, main = "Peak Lux per minute - all",
        type = "l", pch = 20, col = "green", cex = 0.05)
  scale_value = max(M$metalong$lightpeak)
  offset_value = scale_value * 0.02
  lines(data$time, data$nonwearA * scale_value, type = "l", xlab = "",
        main = "lux-based nonwear", ylim = c(0, 1), col = "orange")
  lines(data$time, data$nonwearB * scale_value * 0.95 + offset_value, type = "l", col = "brown")
  lines(data$time, data$nonwearC * scale_value * 0.9  + offset_value, type = "l", col = "cyan")
  lines(data$time, data$nonwearD * scale_value * 0.85 + offset_value, type = "l", col = "purple")
  lines(data$time, data$nonwearE * scale_value * 0.8  + offset_value, type = "l", col = "darkgreen")
  lines(data$time, data$nonwearF * scale_value * 0.75  + offset_value, type = "l", col = "grey")
  dev.off()
}