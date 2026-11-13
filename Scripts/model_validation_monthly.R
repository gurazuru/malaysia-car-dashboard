# model validation step is important to generate the confidence interval for each segment forecast.

library(prophet)
library(readxl)
library(tidyverse)

options(scipen = 999) # avoid using scientific notation.

setwd("C:/R Projects/JPJ_Car_Viz")

# ---- Update this each month!! ----
# M+1 to forecast:
Next_FC_Year = 2026
Next_FC_Month = 11

validation_car_data <- readr::read_csv("Data/car_data_sum.csv")
latest_cutoff <- max(validation_car_data$date_reg) %m-% months(1)

# Create a cutoff_df for validation steps later
cutoff_df <- tibble(
  y_plus1 = year(latest_cutoff),
  m_plus1 = month(latest_cutoff)
) |>
  tidyr::expand_grid(n = 0:35) |>
  mutate(
    ym = make_date(
      year = y_plus1,
      month = m_plus1,
      day = 1
    ) %m-% months(n),
    cutoff = ceiling_date(ym, "month") - days(1)
  ) |>
  mutate(
    y_plus1 = year(cutoff %m+% months(1)),
    m_plus1 = month(cutoff %m+% months(1))
  ) |>
  select(cutoff, y_plus1, m_plus1)

# No. of days to forecast in cross validation step
horizon = 31

# _________________________________________________________________________________________________

# ---- Load Public Holidays Data ----
PH_data <- read_excel("Data/Public_Holidays_MY_2010-2035.xlsx") |>
  rename(ds := 'Date', holiday := 'PH_Name') |>
  select('ds', 'holiday') |>
  mutate(upper_window = case_when(holiday == "Raya_AF" ~ 7, holiday == "Raya_AA" ~ 7, TRUE ~ 0)) |>
  mutate(lower_window = 0)

MCO_lockdowns <- tibble(
  holiday = c('MCO_1', 'MCO_2'),
  ds = as.Date(c('2020-03-18', '2021-06-01')),
  lower_window  = 0,
  ds_upper = as.Date(c('2020-05-03', '2021-06-28'))
)
MCO_lockdowns$upper_window <- as.numeric(MCO_lockdowns$ds_upper - MCO_lockdowns$ds)

holidays_MCO <- bind_rows(PH_data, MCO_lockdowns)

# Reset data_combined df before row bind the whole thing again
data_combined = data.frame()

# ---- Forecast Function ----
validate_step <- function(cutoff_date, y_plus1, m_plus1, segment_filter){
  cutoff = as.Date(c(cutoff_date))
  
  if (segment_filter != "All"){
    
    # ---- Load training data ----
    car_df_test <- validation_car_data |>
      filter(segment == segment_filter) |>
      filter(date_reg <= cutoff_date) |>
      group_by(date_reg) |>
      summarise(y = sum(count), .groups = "drop") |>
      rename(`ds` := date_reg) |> 
      filter(year(ds) >= 2022)
    
  } else {  
    
    # ---- Load training data ----
    car_df_test <- readr::read_csv("Data/car_data_sum.csv") |>
      filter(date_reg <= cutoff_date) |>
      group_by(date_reg) |>
      summarise(y = sum(count), .groups = "drop") |>
      rename(`ds` := date_reg) |> 
      filter(year(ds) >= 2022)
    
  }
  
  df7_test <- car_df_test |>
    mutate(
      is_final_week       = day(ds) > (days_in_month(ds) - 5),
      is_final_2ld        = day(ds) == (days_in_month(ds) - 1),
      is_final_day        = day(ds) == days_in_month(ds),
      is_first_week       = day(ds) <= 7 & day(ds) >= 3,
      is_first_2days      = day(ds) == 2,
      is_first_day        = day(ds) == 1
    )
  
  m7_test <- prophet(monthly.seasonality=20, holidays = holidays_MCO)
  m7_test <- add_seasonality(m7_test, name='monthly', period=30.5, fourier.order=10) # fourier order 10
  m7_test <- add_regressor(m7_test, 'is_final_week')
  m7_test <- add_regressor(m7_test, 'is_final_2ld')
  m7_test <- add_regressor(m7_test, 'is_final_day')
  m7_test <- add_regressor(m7_test, 'is_first_week')
  m7_test <- add_regressor(m7_test, 'is_first_2days')
  m7_test <- add_regressor(m7_test, 'is_first_day')
  m7_test <- fit.prophet(m7_test, df7_test)
  
  future7_test <- make_future_dataframe(m7_test, periods = 45)
  future7_test <- future7_test |>
    mutate(
      is_final_week       = day(ds) > (days_in_month(ds) - 5),
      is_final_2ld        = day(ds) == (days_in_month(ds) - 1),
      is_final_day        = day(ds) == days_in_month(ds),
      is_first_week       = day(ds) <= 7,
      is_first_2days      = day(ds) == 2,
      is_first_day        = day(ds) == 1
    )
  
  forecast7_test <- predict(m7_test, future7_test)
  tail(forecast7_test[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
  
  forecast7_test_summary <- forecast7_test |>
    mutate(year = year(ds), month = month(ds)) |>
    group_by(year, month) |>
    summarise(TIV_FC = sum(yhat), .groups = "drop") |>
    filter(year == as.numeric(y_plus1) & month == as.numeric(m_plus1)) 
  
  # Create a category based on vehicle segment
  if (segment_filter != "All"){
    forecast7_test_summary <- forecast7_test_summary |> mutate(segment = segment_filter)
  } else {
    forecast7_test_summary <- forecast7_test_summary |> mutate(segment = "All")      
  }
  
  return(forecast7_test_summary)
}

segments <- c(
  "All",
  "Hatchback",
  "MPV",
  "Mini",
  "Pick-Up",
  "Sedan-A",
  "Sedan-B",
  "Sedan-C/D",
  "SUV-B",
  "SUV-C/D"
)

validation_grid <- expand.grid(
  cutoff_date = cutoff_df$cutoff,
  segment_filter = segments
)

validation_grid <- validation_grid |>
  mutate(
    y_plus1 = year(cutoff_date %m+% months(1)),
    m_plus1 = month(cutoff_date %m+% months(1))
  ) |>
  filter(y_plus1 == Next_FC_Year & m_plus1 == Next_FC_Month - 1) # filter data for only the latest available month.

data_combined <- purrr::pmap_dfr(
  validation_grid,
  function(cutoff_date, segment_filter, y_plus1, m_plus1) {
    
    validate_step(
      cutoff_date = cutoff_date,
      y_plus1 = y_plus1,
      m_plus1 = m_plus1,
      segment_filter = segment_filter
    )
  }
)

data_combined_previous <- readr::read_csv("Forecasts/actual_vs_forecast_summary.csv") |>
  select('year', 'month', 'TIV_FC', 'segment')

data_combined_new <- data_combined |>
  bind_rows(data_combined_previous) |>
  arrange(segment, desc(year), desc(month))
  

# Optional step to view the m+1 for validation with the dashboard
# new_data_combined <- validate_step("2026-08-31", "2026", "9", "All")

# Load actual data
car_df_actual_all <- readr::read_csv("Data/car_data_sum.csv") |>
  filter(date_reg >= floor_date(max(date_reg, na.rm = TRUE), "month") %m-% months(35)) |>
  mutate(year = year(date_reg), month = month(date_reg), segment = "All") |>
  group_by(segment, year, month) |>
  summarise(TIV_ACTUAL = sum(count), .groups = "drop")

car_df_actual_segment <- readr::read_csv("Data/car_data_sum.csv") |>
  filter(date_reg >= floor_date(max(date_reg, na.rm = TRUE), "month") %m-% months(35)) |>
  mutate(year = year(date_reg), month = month(date_reg)) |>
  group_by(segment, year, month) |>
  summarise(TIV_ACTUAL = sum(count), .groups = "drop")

car_df_actual_comb <- car_df_actual_all |> 
  bind_rows(car_df_actual_segment)

car_df_fc_vs_actual <- data_combined_new |>
  left_join(car_df_actual_comb, by = c("segment", "year", "month")) |>
  mutate(error = TIV_FC - TIV_ACTUAL, abs_error = abs(error), pct_error = error / TIV_ACTUAL) |>
  filter(!is.na(TIV_ACTUAL))

car_df_fc_vs_actual_path <- "Forecasts/actual_vs_forecast_summary.csv"
write_csv(car_df_fc_vs_actual, car_df_fc_vs_actual_path)

# Calculate the standard deviation and mean from the last 36 months
segment_validation <- car_df_fc_vs_actual |>
  group_by(segment) |>
  summarise(
    mean_pct_error = mean(pct_error, na.rm = TRUE),
    sd_pct_error = sd(pct_error, na.rm = TRUE),
    n = sum(!is.na(pct_error)),
    .groups = "drop"
  )

# Apply the error distribution to the latest forecast.
forecast_monthly_all <- readRDS("Data/forecast_monthly.rds") |>
  mutate(segment = "All")

forecast_monthly_all <- forecast_monthly_all |>
  left_join(segment_validation, by = "segment") |>
  mutate(
    lower_error = (1 + mean_pct_error) - 1.645 * sd_pct_error,
    upper_error = (1 + mean_pct_error) + 1.645 * sd_pct_error,
    yhat_lower = yhat * lower_error,
    yhat_upper = yhat * upper_error
  ) |>
  select(segment, month, yhat, yhat_lower, yhat_upper)

forecast_monthly_hatchback  <- readRDS("Data/forecast_monthly_hatchback.rds") |> mutate(segment = "Hatchback")
forecast_monthly_mpv        <- readRDS("Data/forecast_monthly_mpv.rds") |> mutate(segment = "MPV")
forecast_monthly_mini       <- readRDS("Data/forecast_monthly_mini.rds") |> mutate(segment = "Mini")
forecast_monthly_pickup     <- readRDS("Data/forecast_monthly_pickup.rds") |> mutate(segment = "Pick-Up")
forecast_monthly_sedana     <- readRDS("Data/forecast_monthly_sedana.rds") |> mutate(segment = "Sedan-A")
forecast_monthly_sedanb     <- readRDS("Data/forecast_monthly_sedanb.rds") |> mutate(segment = "Sedan-B")
forecast_monthly_sedancd    <- readRDS("Data/forecast_monthly_sedancd.rds") |> mutate(segment = "Sedan-C/D")
forecast_monthly_suvb       <- readRDS("Data/forecast_monthly_suvb.rds") |> mutate(segment = "SUV-B")
forecast_monthly_suvcd       <- readRDS("Data/forecast_monthly_suvcd.rds") |> mutate(segment = "SUV-C/D")

forecast_monthly_segments <- forecast_monthly_hatchback |>
  bind_rows(
    forecast_monthly_mpv,
    forecast_monthly_mini,
    forecast_monthly_pickup,
    forecast_monthly_sedana,
    forecast_monthly_sedanb,
    forecast_monthly_sedancd,
    forecast_monthly_suvb,
    forecast_monthly_suvcd
  )

forecast_monthly_segments <- forecast_monthly_segments |>
  left_join(segment_validation, by = "segment") |>
  mutate(
    lower_error = (1 + mean_pct_error) - 1.645 * sd_pct_error,
    upper_error = (1 + mean_pct_error) + 1.645 * sd_pct_error,
    yhat_lower = yhat * lower_error,
    yhat_upper = yhat * upper_error
  ) |>
  select(segment, month, yhat, yhat_lower, yhat_upper)

saveRDS(forecast_monthly_all, "Data/forecast_monthly.rds")
saveRDS(forecast_monthly_segments, "Data/forecast_monthly_segments.rds")



test <- readRDS("Data/forecast_monthly_segments.rds")


