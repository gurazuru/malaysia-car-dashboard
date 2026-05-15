library(tidyverse)
library(lubridate)
library(DT)
library(ggplot2)
library(plotly)

setwd("C:/R Projects/JPJ_Car_Viz")

# Define the years to pull
years <- 2010:2026

# Generate URLs and read CSVs
car_data_list <- map(years, ~ read_csv(
  paste0("https://storage.data.gov.my/transportation/cars_", .x, ".csv")
))

# Combine all years into one data frame
car_data_full <- bind_rows(car_data_list)

# Makes all dates to use 1st day of the month only.
car_data_full <- car_data_full |>
  mutate(fuel_grouped = case_when(
    fuel %in% c("petrol") ~ "Petrol",
    fuel %in% c("greendiesel", "diesel") ~ "Diesel",
    fuel %in% c("electric") ~ "BEV",
    fuel %in% c("hybrid_petrol", "hybrid_diesel") ~ "Hybrid",
    TRUE ~ "Others"
  )) 

head(car_data_full)

# --- load segment master ref ---
segment_list <- readr::read_csv("Data/master_ref.csv") |>
  select(maker, model, segment)

# --- write the files to the folder ---
car_data_path <- "Data/car_data_sum.csv"
car_data_sample_path <- "Data/car_data_sum_sample.csv"

car_data_sum <- car_data_full |>
  count(date_reg, type, maker, model, fuel_grouped, state, name = "count")

car_data_sum <- car_data_sum |>
  left_join(segment_list, by = c("maker", "model"))

car_data_sum_sample <- head(car_data_sum, 1000)

write_csv(car_data_sum, car_data_path)
write_csv(car_data_sum_sample, car_data_sample_path)

# --- get unique make models ---
model_list <- car_data_sum |>
  distinct(maker, model) |>
  arrange(maker, model)

model_list_path <- "Data/model_list.csv"
write_csv(model_list, model_list_path)

model_list_top <- car_data_sum |>
  group_by(maker, model) |>
  summarise(count = sum(count), .groups = "drop")
