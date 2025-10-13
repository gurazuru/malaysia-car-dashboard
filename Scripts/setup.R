library(tidyverse)
library(lubridate)
library(DT)
library(ggplot2)
library(plotly)

setwd("F:/Users/fis/Documents/JPJ_Car_Viz")

# Define the years you want to pull
years <- 2010:2025

# Generate URLs and read CSVs
car_data_list <- map(years, ~ read_csv(
  paste0("https://storage.data.gov.my/transportation/cars_", .x, ".csv")
))

# Combine all years into one data frame
car_data_full <- bind_rows(car_data_list)

# Makes all dates to use 1st day of the month only.
car_data_full <- car_data_full |> mutate(date_reg = floor_date(date_reg, unit = "month")) |>
  mutate(fuel_grouped = case_when(
    fuel %in% c("petrol") ~ "Petrol",
    fuel %in% c("greendiesel", "diesel") ~ "Diesel",
    fuel %in% c("electric") ~ "BEV",
    fuel %in% c("hybrid_petrol", "hybrid_diesel") ~ "Hybrid",
    TRUE ~ "Others"
  )) 

head(car_data_full)

car_data_path <- "data/car_data_sum.csv"

car_data_sum <- car_data_full |>
  count(date_reg, type, maker, model, fuel_grouped, name = "count")

write_csv(car_data_sum, car_data_path)

# --- get unique make models ---
model_list <- car_data_sum |>
  distinct(maker, model) |>
  arrange(maker, model)