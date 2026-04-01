library(abnormality)
context("longLowValue")
test_that("longLowValue is able to identify a long series of low values", {
  # Generate synthetic data: 48 hours of 1 minute epoch
  set.seed(1234)
  x = runif(n = 2880, min = -500, max = 500)
  x = cumsum(x)
  x[300:1600] = 0 # zeros
  x = abs(x)
  
  # Parameters
  window_size_hours = 16
  step_size = 1
  epoch_size = 60
  
  # Derive rolling statistic
  p95 = function(x) { #95th percentile of x
    return(as.numeric(quantile(x, probs = 0.95, na.rm = TRUE)))
  }
  p95_per_16hours = rollApply(x, window_size_hours,
                              FUN = p95,
                              step_size = step_size,
                              epoch_size)
  # Derive classification based on the rolling statistic
  classification = longLowValue(p95_per_16hours,
                                x_threshold = 10,
                                window_size_hours,
                                step_size,
                                epoch_size)
  
  expect_equal(length(classification), 2880)
  expect_equal(length(which(classification == 0)), 1483)
  expect_equal(length(which(classification == 1)), 1397)
})
