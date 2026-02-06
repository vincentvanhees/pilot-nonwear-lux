# rm(list = ls())
graphics.off()

library(slider)
source(paste0(getwd(), "/classifyNonwear.R"))
source(paste0(getwd(), "/applyClassifyNonwear.R"))

load("D:/Code/ZaunerEtAl_NonWearDetection/data/cleaned/wear_data_cleaned.RData")


epoch_size = 10

# prepare
# filename =  "Left_Temple_Sensor_Data.csv"
filename = "Right_Temple_Sensor_Data.csv"
VEET = VEET[which(VEET$file.name.activity == "Left_Temple_Sensor_Data.csv"),]
VEET = VEET[order(VEET$Datetime),]

# For comparison later on, create numeric non-wear column to ease plotting
# where 0 is wear and 1 is nonwear
VEET$nonwear = 0
VEET$nonwear[which(VEET$wear == "Off")] = 1
# make sure nonwear column has NA when Lux has NA
is.na(VEET$nonwear[is.na(VEET$Lux)]) = TRUE

# change column names to match expectation by function, to do: align with default in LightLogR
VEET = VEET[, c("Datetime", "Lux", "nonwear")]
colnames(VEET)[3] = "nonwear_ref"

out = applyClassifyNonwear(VEET, # assumed to have columns time and Lux, and represent a continuous regular time series
                     resolution_seconds = 60, # for computational only deride statistics at 1 minute resolution
                     N_days_required_daily_stats = 3,
                     minimum_relval_per_hour = 0.5, #minimum relative variance per hour
                     epoch_size = 5, # epoch size in seconds
                     lowLuxThreshold = 50, # Lux below this value is considered closed to zero
                     maxLowLuxSequenceHours = 16,
                     plot_path = "D:/Projects/Spitschan",
                     plot_id =  "Left_Temple_Sensor_Data.csv") # max number of hours with low Lux
