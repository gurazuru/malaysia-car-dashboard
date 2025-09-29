library(tidyverse)
library(lubridate)
library(DT)
library(ggplot2)
library(plotly)

car_data_path <- "JPJ_Car_Viz/data/car_data.csv"

url2025 <- "https://storage.data.gov.my/transportation/cars_2025.csv"
url2024 <- "https://storage.data.gov.my/transportation/cars_2024.csv"
url2023 <- "https://storage.data.gov.my/transportation/cars_2023.csv"
url2022 <- "https://storage.data.gov.my/transportation/cars_2022.csv"
url2021 <- "https://storage.data.gov.my/transportation/cars_2021.csv"

car_data_2025 <- read_csv(url2025)
car_data_2024 <- read_csv(url2024)
car_data_2023 <- read_csv(url2023)
car_data_2022 <- read_csv(url2022)
car_data_2021 <- read_csv(url2021)

car_data <- bind_rows(car_data_2021, car_data_2022, car_data_2023, car_data_2024, car_data_2025)

# Makes all dates to use 1st day of the month only.
car_data <- car_data |> mutate(date_reg = floor_date(date_reg, unit = "month")) |>
  mutate(fuel_grouped = case_when(
    fuel %in% c("petrol") ~ "Petrol",
    fuel %in% c("greendiesel", "diesel") ~ "Diesel",
    fuel %in% c("electric") ~ "BEV",
    fuel %in% c("hybrid_petrol", "hybrid_diesel") ~ "Hybrid",
    TRUE ~ "Others"
  )) 

head(car_data)

write_csv(car_data, car_data_path)

# --- get unique make models ---
model_list <- car_data |>
  distinct(maker, model) |>
  arrange(maker, model)