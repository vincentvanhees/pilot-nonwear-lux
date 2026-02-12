rm(list = ls())
graphics.off()

library(slider)
source(paste0(getwd(), "/classifyNonwear.R"))
source(paste0(getwd(), "/applyClassifyNonwear.R"))

# load time series WHII:
path1 = "D:/Projects/ERC_Paris/Whitehall/output_bins_test_batch/meta/basic"
path2 = "D:/Projects/ERC_Paris/Whitehall/output_bins_test_batch/meta/ms2.out"
fnames1 = dir(path = path1, full.names = TRUE)
fnames2 = dir(path = path2, full.names = TRUE)

for (i in 1:5) { # 1:length(fnames1)
  print("----------------------------------------")
  print(paste0("i ", i))
  M = NULL
  load(fnames1[i])
  j = grep(pattern = unlist(strsplit(basename(fnames1[i]), "meta_"))[2],
        x = basename(fnames2))
  load(fnames2[j])
  M$metalong$timestamp = as.POSIXct(M$metalong$timestamp,
                           format = "%Y-%m-%dT%H:%M:%S%z",
                           tz = "Europe/London")
  M$metalong$nonwearscore = ifelse(M$metalong$nonwearscore > 2, yes = 1, no = 0)
  data = data.frame(Datetime = M$metalong$timestamp,
                    Lux = M$metalong$lightpeak * 100,
                    nonwear_ref = M$metalong$nonwearscore)
  source(paste0(getwd(), "/classifyNonwear.R"))
  
  
  colnames(data)[3] = "nonwear_ref"
  
  out = applyClassifyNonwear(data, # assumed to have columns time and Lux, and represent a continuous regular time series
                             resolution_seconds = 60, # for computational reasons only derive statistics at 30 sec resolution
                             N_days_required_daily_stats = 3,
                             minimum_relval_per_hour = 0.1, #minimum relative variance per hour
                             epoch_size = 60, # epoch size in seconds
                             lowLuxThreshold = 50, # Lux below this value is considered closed to zero
                             maxLowLuxSequenceHours = 16,
                             plot_path = "D:/Projects/Spitschan",
                             plot_id =  paste0("wrist_", i))
}