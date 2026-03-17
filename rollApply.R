# Helper function to ease applying 1- and 16 hour rolling window functions:
rollApply = function(data, window_size_hours, FUN, step_size, N, epoch_size) {
  half_window_size = ceiling(window_size_hours * 60 * (60 / epoch_size) / 2)
  x = slider::slide_dbl(data,
                        ~FUN(.x),
                        .before = half_window_size,
                        .after = half_window_size,
                        .step = step_size,
                        .complete = FALSE)
  # interpolate NA values created by slide_dbl because of the 
  # steps it takes
  valid = which(!is.na(x) == TRUE)
  half_step_size = round(step_size / 2)
  for (i in c(-half_step_size:-1, 1:half_step_size)) {
    validi = valid + i
    select = which(validi > 0 & validi <= N)
    x[validi[select]] = x[valid[select]]
  }
  return(x)
}