library(prophet)
library(readxl)
library(tidyverse)

options(scipen = 999) # avoid using scientific notation.

setwd("C:/R Projects/JPJ_Car_Viz")

# To update every month ----------------------------------------------------------------------------------
# Cutoffs for validation
cutoffs <- as.Date(c('2024-12-31', 
                     '2025-01-31', '2025-02-28', '2025-03-31', '2025-04-30', '2025-05-31', '2025-06-30', 
                     '2025-07-31', '2025-08-31', '2025-09-30', '2025-10-31', '2025-11-30', '2025-12-31', 
                     '2026-01-31', '2026-02-28', '2026-03-31', '2026-04-30', '2026-05-31', '2026-06-30',
                     '2026-07-31', '2026-08-31'
))

# M+1 to forecast:
Next_FC_Year = 2026
Next_FC_Month = 10
# --------------------------------------------------------------------------------------------------------

# ---- Review segment distribution ----
car_df_segment_exposure <- readr::read_csv("Data/car_data_sum.csv") |>
  filter(year(date_reg) >= 2025) |>
  mutate(year_reg = year(date_reg)) |>
  group_by(year_reg, segment) |>
  summarise(y = sum(count))

# ---- Load Public Holidays Data ----
PH_data <- read_excel("Data/Public_Holidays_MY_2010-2035.xlsx") |>
  rename(ds := 'Date', holiday := 'PH_Name') |>
  select('ds', 'holiday') |>
  mutate(upper_window = case_when(holiday == "Raya_AF" ~ 7, holiday == "Raya_AA" ~ 7, TRUE ~ 0)) |>
  mutate(lower_window = 0)

# ---- Forecast Function ----
forecast_tiv_segment <- function(segment_filter, segment_name) {
  # ---- Load JPJ Reg Data - Filter Segment ----
  car_df_segment <- readr::read_csv("Data/car_data_sum.csv") |>
    filter(segment == segment_filter) |>
    group_by(date_reg) |>
    summarise(y = sum(count), .groups = "drop") |>
    rename(`ds` := date_reg) |> 
    filter(year(ds) >= 2022) 
  
  # ---- Prepare factors ----
  df5_segment <- car_df_segment |>
    mutate(
      is_final_week       = day(ds) > (days_in_month(ds) - 5),
      is_final_2ld        = day(ds) == (days_in_month(ds) - 1),
      is_final_day        = day(ds) == days_in_month(ds),
      is_first_week       = day(ds) <= 7 & day(ds) >= 3,
      is_first_2days      = day(ds) == 2,
      is_first_day        = day(ds) == 1
    )
  
  # Model selection is based on the full TIV model validation
  # ---- Model 6: Log Transform for Non-Negativity ----
  df6_lt_segment <- df5_segment
  df6_lt_segment$y <- log1p(df6_lt_segment$y)
  
  m6_segment <- prophet()
  m6_segment <- prophet(monthly.seasonality=20, holidays = holidays_MCO)
  m6_segment <- add_seasonality(m6_segment, name='monthly', period=30.5, fourier.order=5)
  m6_segment <- add_regressor(m6_segment, 'is_final_week')
  m6_segment <- add_regressor(m6_segment, 'is_final_2ld')
  m6_segment <- add_regressor(m6_segment, 'is_final_day')
  m6_segment <- add_regressor(m6_segment, 'is_first_week')
  m6_segment <- add_regressor(m6_segment, 'is_first_2days')
  m6_segment <- add_regressor(m6_segment, 'is_first_day')
  m6_segment <- fit.prophet(m6_segment, df6_lt_segment)
  
  forecast6_segment <- predict(m6_segment, future5)
  
  plot(m6_segment, forecast6_segment)
  prophet_plot_components(m6_segment, forecast6_segment)
  
  forecast6_segment$yhat <- expm1(forecast6_segment$yhat)
  forecast6_segment$yhat_lower <- expm1(forecast6_segment$yhat_lower)
  forecast6_segment$yhat_upper <- expm1(forecast6_segment$yhat_upper)
  tail(forecast6_segment[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
  
  # ---- Model 7: Tweaked Version of Model 5 ----
  df7_segment <- car_df_segment |>
    mutate(
      is_final_week       = day(ds) > (days_in_month(ds) - 5),
      is_final_2ld        = day(ds) == (days_in_month(ds) - 1),
      is_final_day        = day(ds) == days_in_month(ds),
      is_first_week       = day(ds) <= 7 & day(ds) >= 3,
      is_first_2days      = day(ds) == 2,
      is_first_day        = day(ds) == 1
    )
  
  m7_segment <- prophet()
  m7_segment <- prophet(monthly.seasonality=20, holidays = holidays_MCO)
  m7_segment <- add_seasonality(m7_segment, name='monthly', period=30.5, fourier.order=10) # fourier order 10
  m7_segment <- add_regressor(m7_segment, 'is_final_week')
  m7_segment <- add_regressor(m7_segment, 'is_final_2ld')
  m7_segment <- add_regressor(m7_segment, 'is_final_day')
  m7_segment <- add_regressor(m7_segment, 'is_first_week')
  m7_segment <- add_regressor(m7_segment, 'is_first_2days')
  m7_segment <- add_regressor(m7_segment, 'is_first_day')
  m7_segment <- fit.prophet(m7_segment, df7_segment)
  
  future7_segment <- make_future_dataframe(m7_segment, periods = 365)
  future7_segment <- future7_segment |>
    mutate(
      is_final_week       = day(ds) > (days_in_month(ds) - 5),
      is_final_2ld        = day(ds) == (days_in_month(ds) - 1),
      is_final_day        = day(ds) == days_in_month(ds),
      is_first_week       = day(ds) <= 7,
      is_first_2days      = day(ds) == 2,
      is_first_day        = day(ds) == 1
    )
  
  forecast7_segment <- predict(m7_segment, future7_segment)
  tail(forecast7_segment[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
  
  plot(m7_segment, forecast7_segment)
  prophet_plot_components(m7_segment, forecast7_segment)
  
  # ---- Model Validation (horizon must be equal to number of days in the m+1 forecasted month)
  dfcv6_segment <- cross_validation(m6_segment, cutoffs = cutoffs, horizon = 30, units = 'days')
  dfcv6_segment$y <- expm1(dfcv6_segment$y)
  dfcv6_segment$yhat <- expm1(dfcv6_segment$yhat)
  dfcv6_segment$yhat_lower <- expm1(dfcv6_segment$yhat_lower)
  dfcv6_segment$yhat_upper <- expm1(dfcv6_segment$yhat_upper)
  dfp6_segment <- performance_metrics(dfcv6_segment)
  
  dfcv7_segment <- cross_validation(m7_segment, cutoffs = cutoffs, horizon = 30, units = 'days')
  dfp7_segment <- performance_metrics(dfcv7_segment)
  
  # --- write the files to the folder ---
  write_csv(forecast6_segment, paste0("Forecasts/m6_", segment_name,"_results.csv"))
  write_csv(forecast7_segment, paste0("Forecasts/m7_", segment_name,"_results.csv"))
  
  write_csv(dfcv6_segment, paste0("Forecasts/m6_", segment_name,"_crossvalid.csv"))
  write_csv(dfcv7_segment, paste0("Forecasts/m7_", segment_name,"_crossvalid.csv"))
  
  
  # Based on full TIV data validation steps, Model 7 has the lowest error against actual.
  # While Model 6 has the best monthly trend from 1st to 10th of the month
  # Prepare upcoming month's forecast including monthly trend
  forecast_sel1_segment <- forecast7_segment |>
    filter(year(ds) == Next_FC_Year & month(ds) == Next_FC_Month) |>
    select(ds, yhat, yhat_lower, yhat_upper) |>
    rename(Date := 'ds', yhat1 := 'yhat', yhat1_lower := 'yhat_lower', yhat1_upper := 'yhat_upper')
  
  forecast_sel2_segment <- forecast6_segment |> # for monthly trend, follows log model for the first 10 days of the month
    filter(year(ds) == Next_FC_Year & month(ds) == Next_FC_Month) |>
    select(ds, yhat, yhat_lower, yhat_upper) |>
    rename(Date := 'ds', yhat2 := 'yhat', yhat2_lower := 'yhat_lower', yhat2_upper := 'yhat_upper')
  
  yhat1_month = sum(forecast_sel1_segment$yhat1)
  yhat2_month = sum(forecast_sel2_segment$yhat2)
  
  forecast_nextmonth_2h_segment <- forecast_sel1_segment |>
    filter(day(Date) > 10) |>                                # note the 10th of the month, same as the next step
    summarise(yhat1_month2h = sum(yhat1), .groups = "drop")
  
  forecast_sel_comb_segment <- forecast_sel1_segment |>
    left_join(forecast_sel2_segment, by = c("Date")) |>
    mutate(yhat_adj = case_when(day(Date) <= 10 ~ yhat2, TRUE ~ yhat1 )) # note the 10th of the month being the separation here
  
  yhat_adj_month = sum(forecast_sel_comb_segment$yhat_adj)  # get the total monthly TIV
  
  forecast_sel_comb_segment <- forecast_sel_comb_segment |>
    mutate(yhat1_month = yhat1_month,
           yhat2_month = yhat2_month,
           yhat_adj_month = yhat_adj_month,
           yhat1_month2h = forecast_nextmonth_2h_segment$yhat1_month2h) |>
    mutate(yhat_adj_add = case_when(day(Date) <= 10 ~ 0, TRUE ~ ((yhat1_month - yhat_adj_month) * (yhat_adj/yhat1_month2h)))) |>
    mutate(yhat_adj_nodiff = yhat_adj + yhat_adj_add) |>
    mutate(yhat_neg = case_when(yhat_adj_nodiff < 0 ~ yhat_adj_nodiff, TRUE ~ 0))
  
  forecast_sel_month_negative <- sum(forecast_sel_comb_segment$yhat_neg) # get total negative forecast to reconcile back on the final day
  
  forecast_sel_comb_segment <- forecast_sel_comb_segment |>
    # now remove the negative values by setting them to 0
    mutate(yhat_adj_nodiff = case_when(yhat_adj_nodiff < 0 ~ 0, TRUE ~ yhat_adj_nodiff)) |>
    
    # Identify first 20 days
    mutate(is_first_20_days = day(Date) <= 20) |>
    
    # Calculate total forecast within first 20 days
    mutate(first_20_total = sum(yhat_adj_nodiff[is_first_20_days], na.rm = TRUE)) |>
    
    # calculate the ratio of each day within the first 20 days
    mutate(first_20_ratio = case_when(is_first_20_days & first_20_total > 0 ~ yhat_adj_nodiff / first_20_total,
                                      TRUE ~ 0)) |>
    
    # calculate the new yhat with negative values redistributed
    mutate(yhat_adj_nodiff = yhat_adj_nodiff + forecast_sel_month_negative * first_20_ratio) |>
    ungroup()
  
  yhat_adjnodiff_month = sum(forecast_sel_comb_segment$yhat_adj_nodiff)
  
  # Prepare final data frame for dashboard
  forecast_fd_segment <- forecast_sel_comb_segment |>
    select(Date, yhat_adj_nodiff) |>
    mutate(yhat_adj_nodiff = round(yhat_adj_nodiff)) |>
    rename(Forecast := yhat_adj_nodiff) |>
    mutate(Forecast_Cum = cumsum(Forecast))
  
  # Get standard deviation of the month's forecast for confidence interval calculation.
  # 1.645 -> 90%, 1.96 -> 95%
  forecast_monthly_segment <- forecast7_segment |>
    select(ds, yhat, yhat_upper) |>
    mutate(
      month = floor_date(ds, "month"),
      sd_day = (yhat_upper - yhat) / 1.645 
    ) |>
    # don't zeroize negative values to avoid overforecasting total monthly
    # mutate(yhat = case_when(yhat < 0 ~ 0, TRUE ~ yhat)) |>
    group_by(month) |>
    summarise(
      yhat = sum(yhat),
      sd_month = sqrt(sum(sd_day^2)),
      yhat_lower = yhat - 1.645  * sd_month,
      yhat_upper = yhat + 1.645  * sd_month
    ) |>
    mutate(yhat = round(yhat), yhat_lower = round(yhat_lower), yhat_upper = round(yhat_upper))
  
  # --- save the files to the folder for app.R ---
  saveRDS(forecast_monthly_segment, paste0("Data/forecast_monthly_", segment_name,".rds"))
  saveRDS(forecast_fd_segment, paste0("Data/forecast_fd_", segment_name,".rds"))
}

forecast_tiv_segment("Hatchback", "hatchback")
forecast_tiv_segment("MPV", "mpv")
forecast_tiv_segment("Mini", "mini")
forecast_tiv_segment("Pick-Up", "pickup")
forecast_tiv_segment("Sedan-A", "sedana")
forecast_tiv_segment("Sedan-B", "sedanb")
forecast_tiv_segment("Sedan-C/D", "sedancd")
forecast_tiv_segment("SUV-B", "suvb")
forecast_tiv_segment("SUV-C/D", "suvcd")