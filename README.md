# R package abnormality

Code to explore identifying abnormalities in lux data based on criteria referred to with capital letters (A, B, etc).

## Install and load

```
remotes::install_github("vincentvanhees/pilot-nonwear-lux")`
library(abnormality)
```

## Input

### data

A tibble with at least columns Datetime and Lux.

Optional additional columns include:

#### nonwear

Only used as reference value for the visualisation, and expected to have numeric values 0 or 1.

#### weight_X

Where X refers to the weighting used for abnormality indicator X.
For example, weight_A for indicator_A. Weight can be useful for masking parts of the day based on photoperiod.

### various other parameters

Used to configure the identification criteria as can be found in the documentation of in function classifyAbnormal.


## Key functions

### classifyAbnormal

- Extracts statistics from the time series with the help of function `temporalStatistics`
- Uses these temporal statistics to classify the time series with help of functions `constantNonZeroLux`, `longLowValue`, and `rollApply`
- Applies the weight factors (see weight_X above)
- Returns the classifications of abnormality via new column names that all start with idicator_X, where X refers to the criteria.


### applyClassifyAbnormal

Wrapper function around function `classifyAbnormal`:
- Makes sure `classifyAbnormal` is only applied to segments of the data where there is data and skipping large time gaps.
- Plots the resulting classification in a plot and save this to a png file.

## How to experiment with new or modified criteria

- Modify `temporalStatistics` such that the temporal statistics you need are generated. Note that the functional already generates statistics at two time resolutions:
  1. Per day
  2. Per epoch, with code to apply rolling window characteristics
- Modify `classifyAbnormal` to use these new statistics. Here you can modify the existing criteria or or add new one. When adding new ones make sure to use the same name structure starting with indicator_.


## Example scripts

[pilot_with_VEET.R](https://github.com/vincentvanhees/pilot-nonwear-lux/blob/main/pilot_with_VEET.R) and [pilot_zauneretal_nonweardetection.R](https://github.com/vincentvanhees/pilot-nonwear-lux/blob/main/pilot_zauneretal_nonweardetection.R) are examples of how to use the packages.
