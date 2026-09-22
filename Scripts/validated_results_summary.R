
library(prophet)
library(readxl)
library(tidyverse)

options(scipen = 999) # avoid using scientific notation.

setwd("C:/R Projects/JPJ_Car_Viz")

validated_results <- readr::read_csv("Forecasts/actual_vs_forecast_summary.csv")