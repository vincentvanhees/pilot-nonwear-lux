graphics.off()

library(tidyverse)
library(LightLogR)
library(gt)
library(ggridges)
library(downlit)
library(magick)

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
dataVEET <- import$VEET(path, tz = tz, modality = "ALS", manual.id = "VEET", silent = TRUE)

# Select relevant columns
dataVEET <- dataVEET[, c("time_stamp", "Datetime", "Lux")]  #"integration_time"

# Aggregate
dataVEET <- dataVEET |>
  aggregate_Datetime(unit = "5 seconds") |>
  gap_handler(full.days = TRUE) |>
  add_Date_col(group.by = TRUE) |>
  remove_partial_data(Lux, threshold.missing = "1 hour")

# Add wear column
dataVEET$nonwear = TRUE
for (i in 1:nrow(wear_data)) {
  if (wear_data$wear[i] == TRUE) {
    wear = which(dataVEET$Datetime >= wear_data$start[i] & dataVEET$Datetime <= wear_data$end[i])
    print(length(wear))
    if (length(wear) > 0) {
      dataVEET$nonwear[wear] = FALSE
    }
  }
}
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
print(table(dataVEET$nonwear))

# change column names to match expectation by function, to do: align with default in LightLogR
data = dataVEET[, c("Lux", "Datetime")]
colnames(data) = c("lux", "time")
data$lux[is.na(data$lux)] = 0
source(paste0(getwd(), "/classifyNonwear.R"))
data = classifyNonwear(data,
                       resolution_seconds = 60, # for computational only derive statistics at 1 minute resolution
                       N_days_required_daily_stats = 3,
                       minimum_relval_per_hour = 0.1, #minimum relative variance per hour
                       epoch_size = 5)


# Crude plot to inspect classification
graphics.off()
x11()
par(mfrow = c(5,1), mar = c(3, 3, 3, 1))
plot(dataVEET$Datetime, dataVEET$Lux, type = "l", xlab = "", ylab = "Lux per 5 second")
plot(dataVEET$Datetime, dataVEET$nonwear, type = "l", xlab = "", ylab = "Non-wear")
plot(data$time, data$nonwearA, type = "l", xlab = "", main = "lux-based nonwear A", ylim = c(0, 1))
plot(data$time, data$nonwearB, type = "l", xlab = "", main = "lux-based nonwear B", col = "red", ylim = c(0, 1))
plot(data$time, data$nonwearC, type = "l", xlab = "", main = "lux nonwear C", col = "blue", ylim = c(0, 1))
lines(data$time, data$nonwearD, type = "l", main = "lux nonwear D", col = "green")
