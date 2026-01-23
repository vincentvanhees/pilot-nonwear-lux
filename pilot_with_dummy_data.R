library(slider)

# create dummy data to test code
graphics.off()
epoch_size = 5
start = as.POSIXct("2025-12-23 00:00:00", tz = "")
time = seq(start, start - 1 + (3600 * 24 * 7), by = epoch_size)
N = length(time)
time_num = as.numeric(time)
time_num = time_num - time_num[1]
time_num = time_num / (3600)
lux = cos((2 * pi * time_num) / 24) * 30
lux = lux + rnorm(n = N, mean = 120, sd = 15)
lux[which(lux < 0)] = 0
lux[sample(x = 1:N, size = 100)] = rnorm(n = 100, mean = 300, sd = 50)
data = data.frame(time = time, lux = lux)


source(paste0(getwd(), "/classifyNonwear.R"))
data = classifyNonwear(data, resolution_seconds = 60, # for computational only deride statistics at 1 minute resolution
                       N_days_required_daily_stats = 3,
                       minimum_relval_per_hour = 0.2, #minimum relative variance per hour
                       epoch_size = 5)

# Crude plot to inspect classification
x11()
par(mfrow = c(5, 1), mar = c(3, 3, 3, 1), bty = "l")
plot(data$time, data$lux, type = "l")
plot(data$time, data$nonwearA, type = "l", main = "A", ylim = c(0, 1))
plot(data$time, data$nonwearB, type = "l", main = "B", ylim = c(0, 1))
plot(data$time, data$nonwearC, type = "l", main = "C", ylim = c(0, 1))
plot(data$time, data$nonwearD, type = "l", main = "D", ylim = c(0, 1))

