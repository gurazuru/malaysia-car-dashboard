library(tidyverse)
library(lubridate)
library(DT)
library(ggplot2)
library(plotly)

setwd("C:/R Projects/JPJ_Car_Viz")

# Define the years to pull
years <- 2016:2026

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
  )) |>
  select(-type)

# Fix some duplicate model names
car_data_full2 <- car_data_full |>
  mutate(
    model = ifelse(maker == "iCaur" & model == "3", "03", model),
    model = ifelse(model == "iCAUR 03 iWD", "03", model),
    model = ifelse(model == "iCaur 03", "03", model),
    model = ifelse(model == "iCar V23", "V23", model),
    model = ifelse(model == "iCaur V23", "V23", model),
    
    maker = ifelse(maker == "Chery" & model == "03", "iCaur", maker),
    maker = ifelse(maker == "Chery" & model == "V23", "iCaur", maker),
    
    model = ifelse(model == "Jaecoo J7", "J7", model),
    model = ifelse(model == "Jaecoo J5", "J5", model),
    model = ifelse(model == "Omoda 7", "Omoda C7", model),
    model = ifelse(model == "Omoda 9", "Omoda C9", model),
    model = ifelse(model == "Jaecoo J8", "J8", model),
    model = ifelse(model == "Jaecoo J6", "J6", model),
    
    maker = ifelse(maker == "Chery" & model == "J7", "Jaecoo", maker),
    maker = ifelse(maker == "Chery" & model == "J5", "Jaecoo", maker),
    maker = ifelse(maker == "Chery" & model == "Omoda C7", "Jaecoo", maker),
    maker = ifelse(maker == "Chery" & model == "Omoda C9", "Jaecoo", maker),
    maker = ifelse(maker == "Chery" & model == "J8", "Jaecoo", maker),
    maker = ifelse(maker == "Chery" & model == "J6", "Jaecoo", maker),
    
    maker = ifelse(maker == "Great Wall", "GWM", maker)
  )

head(car_data_full2)

# --- load segment master ref ---
segment_list <- readr::read_csv("Data/master_ref.csv") |>
  select(maker, model, segment)

# --- write the files to the folder ---
car_data_path <- "Data/car_data_sum.csv"
car_data_sample_path <- "Data/car_data_sum_sample.csv"

car_data_sum <- car_data_full2 |>
  count(date_reg, maker, model, fuel_grouped, state, name = "count")

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
