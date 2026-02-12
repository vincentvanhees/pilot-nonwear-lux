# rm(list = ls())
graphics.off()

library(slider)
source(paste0(getwd(), "/classifyNonwear.R"))
source(paste0(getwd(), "/applyClassifyNonwear.R"))

load("D:/Code/ZaunerEtAl_NonWearDetection/data/cleaned/wear_data_cleaned.RData")


epoch_size = 10

# prepare
for (filename in c("Left_Temple_Sensor_Data.csv", "Right_Temple_Sensor_Data.csv")) {
  cat(paste0("**** filename: ", filename))
  data = VEET[which(VEET$file.name.activity == filename),]
  data = data[order(data$Datetime),]
  
  # For comparison later on, create numeric non-wear column to ease plotting
  # where 0 is wear and 1 is nonwear
  data$nonwear = 0
  data$nonwear[which(data$wear == "Off")] = 1
  # make sure nonwear column has NA when Lux has NA
  is.na(data$nonwear[is.na(data$Lux)]) = TRUE
  
  # keep only column names that will be used
  data = data[, c("Datetime", "Lux", "nonwear")]
  colnames(data)[3] = "nonwear_ref" # rename to clarify that this is reference nonwear
  
  out = applyClassifyNonwear(data, # assumed to have columns time and Lux, and represent a continuous regular time series
                             resolution_seconds = 30, # for computational reasons only derive statistics at 30 sec resolution
                             N_days_required_daily_stats = 3,
                             minimum_relval_per_hour = 0.7, #minimum relative variance per hour
                             epoch_size = 5, # epoch size in seconds
                             lowLuxThreshold = 50, # Lux below this value is considered closed to zero
                             maxLowLuxSequenceHours = 16,
                             plot_path = "D:/Projects/Spitschan",
                             plot_id =  filename) # max number of hours with low Lux
}