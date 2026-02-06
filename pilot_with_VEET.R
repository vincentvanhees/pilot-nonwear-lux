rm(list = ls())
graphics.off()

library(tidyverse)
library(LightLogR)
library(gt)
library(ggridges)
library(downlit)
library(magick)

source(paste0(getwd(), "/classifyNonwear.R"))
source(paste0(getwd(), "/applyClassifyNonwear.R"))


path <- "D:/Projects/Spitschan/01_VEET_L.csv.zip"
tz   <- "US/Central"

#>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Pre-process data based on code snippets from
# https://tscnlab.github.io/ZaunerEtAl_JVis_2026/supplement.html#identifying-non-wear-times

# Load acceletation data
dataIMU <-
  import$VEET(path, tz = tz, modality = "IMU", manual.id = "VEET",
              remove_duplicates = TRUE,
              silent = TRUE)

# Wear detection
Id = Datetime = wear = NULL
wear_data <- 
  dataIMU |> 
  aggregate_Datetime("5 mins",numeric.handler = sd) |>
  mutate(wear = ax > 0.05) |>
  select(Id, Datetime, wear) |>
  extract_states(wear)
rm(dataIMU)
wear_data <- wear_data |> select(Id, wear, start, end)

# Load light sensor data
VEET <- import$VEET(path, tz = tz, modality = "ALS", manual.id = "VEET", silent = TRUE)

# Select relevant columns
VEET <- VEET[, c("time_stamp", "Datetime", "Lux")]  #"integration_time"

# Aggregate
VEET <- VEET |>
  aggregate_Datetime(unit = "5 seconds") |>
  gap_handler(full.days = TRUE) |>
  add_Date_col(group.by = TRUE) |>
  remove_partial_data(Lux, threshold.missing = "1 hour")

# Add wear column
VEET$nonwear = TRUE
for (i in 1:nrow(wear_data)) {
  if (wear_data$wear[i] == TRUE) {
    wear = which(VEET$Datetime >= wear_data$start[i] & VEET$Datetime <= wear_data$end[i])
    print(length(wear))
    if (length(wear) > 0) {
      VEET$nonwear[wear] = FALSE
    }
  }
}
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Classify nonwear

filename = "01_VEET_L.csv.zip"

# Prepare data:
# change column names to match expectation by function, to do: align with default in LightLogR
data = VEET[, c("Lux", "Datetime", "nonwear")]
data = data[order(VEET$Datetime),]

# For comparison later on, create numeric non-wear column to ease plotting
# where 0 is wear and 1 is nonwear
VEET$nonwear = as.numeric(VEET$nonwear)
# make sure nonwear column has NA when Lux has NA
is.na(VEET$nonwear[is.na(VEET$Lux)]) = TRUE

# change column names to match expectation by function, to do: align with default in LightLogR
VEET = VEET[, c("Datetime", "Lux", "nonwear")]
colnames(VEET)[3] = "nonwear_ref"

# Run classifier
out = applyClassifyNonwear(VEET, # assumed to have columns time and Lux, and represent a continuous regular time series
                           resolution_seconds = 60, # for computational only deride statistics at 1 minute resolution
                           N_days_required_daily_stats = 3,
                           minimum_relval_per_hour = 0.1, #minimum relative variance per hour
                           epoch_size = 5, # epoch size in seconds
                           lowLuxThreshold = 50, # Lux below this value is considered closed to zero
                           maxLowLuxSequenceHours = 16,
                           plot_path = "D:/Projects/Spitschan",
                           plot_id = filename) # max number of hours with low Lux


