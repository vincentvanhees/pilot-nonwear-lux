graphics.off()

library(slider)
load("D:/Code/ZaunerEtAl_NonWearDetection/data/cleaned/wear_data_cleaned.RData")

# prepare
# VEET = VEET[which(VEET$file.name.activity == "Left_Temple_Sensor_Data.csv"),]
VEET = VEET[which(VEET$file.name.activity == "Right_Temple_Sensor_Data.csv"),]
VEET = VEET[order(VEET$Datetime),]
VEET$nonwear = 0
VEET$nonwear[which(VEET$wear == "Off")] = 1
# VEET = VEET[1:13000, ]

is.na(VEET$nonwear[is.na(VEET$Lux)]) = TRUE


# change column names to match expectation by function, to do: align with default in LightLogR
data = VEET[, c("Datetime", "Lux")]
data = data[!duplicated(data),] # remove duplicates


# colnames(data) = c("Lux", "time")

# data$lux[is.na(data$lux)] = 0
source(paste0(getwd(), "/classifyNonwear.R"))
data = classifyNonwear(data,
                       resolution_seconds = 60, # for computational only derive statistics at 1 minute resolution
                       N_days_required_daily_stats = 3,
                       minimum_relval_per_hour = 0.1, #minimum relative variance per hour
                       epoch_size = 10)

# Crude plot to inspect classification
graphics.off()
x11()
par(mfrow = c(2,1), mar = c(3, 3, 3, 1))
# plot(VEET$Datetime, VEET$Lux, type = "l", xlab = "", ylab = "Lux per 10 second")
plot(VEET$Datetime, VEET$Lux, type = "l", xlab = "", main = "Lux")
scale_value = max(VEET$Lux, na.rm = TRUE)
offset_value = scale_value * 0.02
lines(data$Datetime, data$nonwearA * scale_value + offset_value, type = "l", xlab = "",
      main = "lux-based nonwear", col = "orange")
lines(data$Datetime, data$nonwearB * scale_value * 0.95 + offset_value, type = "l", col = "brown")
lines(data$Datetime, data$nonwearC * scale_value * 0.9  + offset_value, type = "l", col = "cyan")
lines(data$Datetime, data$nonwearD * scale_value * 0.85 + offset_value, type = "l", col = "purple")
lines(data$Datetime, data$nonwearE * scale_value * 0.8  + offset_value, type = "l", col = "darkgreen")
lines(data$Datetime, data$nonwearF * scale_value * 0.75  + offset_value, type = "l", col = "grey")
lines(data$Datetime, data$nonwearG * scale_value * 0.7  + offset_value, type = "l", col = "pink")
plot(VEET$Datetime, VEET$nonwear, type = "l", xlab = "", main = "Non-wear reference")
lines(data$Datetime, data$nonwear_estimate * 0.7, type = "l", xlab = "", col = "red")

