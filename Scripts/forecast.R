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
                     '2026-01-31', '2026-02-28', '2026-03-31', '2026-04-30', '2026-05-31', '2026-06-30'
))

# M+1 to forecast:
Next_FC_Year = 2026
Next_FC_Month = 8
# --------------------------------------------------------------------------------------------------------

# ---- Load Public Holidays Data ----
PH_data <- read_excel("Data/Public_Holidays_MY_2010-2035.xlsx") |>
  rename(ds := 'Date', holiday := 'PH_Name') |>
  select('ds', 'holiday') |>
  mutate(upper_window = case_when(holiday == "Raya_AF" ~ 7, holiday == "Raya_AA" ~ 7, TRUE ~ 0)) |>
  mutate(lower_window = 0)

# ---- Load JPJ Reg Data  ----
car_df <- readr::read_csv("Data/car_data_sum.csv") |>
  group_by(date_reg) |>
  summarise(y = sum(count), .groups = "drop") |>
  rename(`ds` := date_reg) |> 
  filter(year(ds) >= 2022)

# ---- Model 1: Default - Yearly, Weekly seasonality. ----
m1 <- prophet(car_df)

future1 <- make_future_dataframe(m1, periods = 365)

forecast1 <- predict(m1, future1)
tail(forecast1[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m1, forecast1)
prophet_plot_components(m1, forecast1)

# ---- Model 2: Add Monthly Seasonality - Yearly, Weekly, & Monthly seasonality. ----
m2 <- prophet(car_df)
m2 <- prophet(monthly.seasonality=TRUE)
m2 <- add_seasonality(m2, name='monthly', period=30.5, fourier.order=5)
m2 <- fit.prophet(m2, car_df)

future2 <- make_future_dataframe(m2, periods = 365)

forecast2 <- predict(m2, future2)
tail(forecast2[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m2, forecast2)
prophet_plot_components(m2, forecast2)

# ---- Model 3: Add Public Holidays - Yearly, Weekly, Monthly, & Public Holiday seasonality. ----
m3 <- prophet(car_df)
m3 <- prophet(monthly.seasonality=TRUE, holidays = PH_data)
m3 <- add_seasonality(m3, name='monthly', period=30.5, fourier.order=5)
m3 <- fit.prophet(m3, car_df)

future3 <- make_future_dataframe(m3, periods = 365)

forecast3 <- predict(m3, future3)
tail(forecast3[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m3, forecast3)
prophet_plot_components(m3, forecast3)

# ---- Model 4: Treat COVID Lockdown - Yearly, Weekly, Monthly, Public Holiday, & COVID Lockdown seasonality. ----
MCO_lockdowns <- tibble(
 holiday = c('MCO_1', 'MCO_2'),
 ds = as.Date(c('2020-03-18', '2021-06-01')),
 lower_window  = 0,
 ds_upper = as.Date(c('2020-05-03', '2021-06-28'))
)
MCO_lockdowns$upper_window <- as.numeric(MCO_lockdowns$ds_upper - MCO_lockdowns$ds)

holidays_MCO <- bind_rows(PH_data, MCO_lockdowns)

m4 <- prophet(car_df)
m4 <- prophet(monthly.seasonality=TRUE, holidays = holidays_MCO)
m4 <- add_seasonality(m4, name='monthly', period=30.5, fourier.order=5)
m4 <- fit.prophet(m4, car_df)

future4 <- make_future_dataframe(m4, periods = 365)

forecast4 <- predict(m4, future4)
tail(forecast4[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m4, forecast4)
prophet_plot_components(m4, forecast4)

# ---- Model 5: Month Closing Effect - Yearly, Weekly, Monthly, Public Holiday, COVID Lockdown seasonality, & Month Closing Effect.  ----
df5 <- car_df |>
  mutate(
    is_final_week       = day(ds) > (days_in_month(ds) - 5),
    is_final_2ld        = day(ds) == (days_in_month(ds) - 1),
    is_final_day        = day(ds) == days_in_month(ds),
    is_first_week       = day(ds) <= 7 & day(ds) >= 3,
    is_first_2days      = day(ds) == 2,
    is_first_day        = day(ds) == 1
  )

m5 <- prophet()
m5 <- prophet(monthly.seasonality=20, holidays = holidays_MCO)
m5 <- add_seasonality(m5, name='monthly', period=30.5, fourier.order=5)
m5 <- add_regressor(m5, 'is_final_week')
m5 <- add_regressor(m5, 'is_final_2ld')
m5 <- add_regressor(m5, 'is_final_day')
m5 <- add_regressor(m5, 'is_first_week')
m5 <- add_regressor(m5, 'is_first_2days')
m5 <- add_regressor(m5, 'is_first_day')
m5 <- fit.prophet(m5, df5)

future5 <- make_future_dataframe(m5, periods = 365)
future5 <- future5 |>
  mutate(
    is_final_week       = day(ds) > (days_in_month(ds) - 5),
    is_final_2ld        = day(ds) == (days_in_month(ds) - 1),
    is_final_day        = day(ds) == days_in_month(ds),
    is_first_week       = day(ds) <= 7 & day(ds) >= 3,
    is_first_2days      = day(ds) == 2,
    is_first_day        = day(ds) == 1
  )

forecast5 <- predict(m5, future5)
tail(forecast5[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m5, forecast5)
prophet_plot_components(m5, forecast5)

# ---- Model 6: Log Transform for Non-Negativity ----
df6_lt <- df5
df6_lt$y <- log1p(df6_lt$y)

m6 <- prophet()
m6 <- prophet(monthly.seasonality=20, holidays = holidays_MCO)
m6 <- add_seasonality(m6, name='monthly', period=30.5, fourier.order=5)
m6 <- add_regressor(m6, 'is_final_week')
m6 <- add_regressor(m6, 'is_final_2ld')
m6 <- add_regressor(m6, 'is_final_day')
m6 <- add_regressor(m6, 'is_first_week')
m6 <- add_regressor(m6, 'is_first_2days')
m6 <- add_regressor(m6, 'is_first_day')
m6 <- fit.prophet(m6, df6_lt)

forecast6 <- predict(m6, future5)

plot(m6, forecast6)
prophet_plot_components(m6, forecast6)

forecast6$yhat <- expm1(forecast6$yhat)
forecast6$yhat_lower <- expm1(forecast6$yhat_lower)
forecast6$yhat_upper <- expm1(forecast6$yhat_upper)
tail(forecast6[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

# ---- Model 7: Tweaked Version of Model 5 ----
df7 <- car_df |>
  mutate(
    is_final_week       = day(ds) > (days_in_month(ds) - 5),
    is_final_2ld        = day(ds) == (days_in_month(ds) - 1),
    is_final_day        = day(ds) == days_in_month(ds),
    is_first_week       = day(ds) <= 7 & day(ds) >= 3,
    is_first_2days      = day(ds) == 2,
    is_first_day        = day(ds) == 1
  )

m7 <- prophet()
m7 <- prophet(monthly.seasonality=20, holidays = holidays_MCO)
m7 <- add_seasonality(m7, name='monthly', period=30.5, fourier.order=10) # fourier order 10
m7 <- add_regressor(m7, 'is_final_week')
m7 <- add_regressor(m7, 'is_final_2ld')
m7 <- add_regressor(m7, 'is_final_day')
m7 <- add_regressor(m7, 'is_first_week')
m7 <- add_regressor(m7, 'is_first_2days')
m7 <- add_regressor(m7, 'is_first_day')
m7 <- fit.prophet(m7, df7)

future7 <- make_future_dataframe(m7, periods = 365)
future7 <- future7 |>
  mutate(
    is_final_week       = day(ds) > (days_in_month(ds) - 5),
    is_final_2ld        = day(ds) == (days_in_month(ds) - 1),
    is_final_day        = day(ds) == days_in_month(ds),
    is_first_week       = day(ds) <= 7,
    is_first_2days      = day(ds) == 2,
    is_first_day        = day(ds) == 1
  )

forecast7 <- predict(m7, future7)
tail(forecast5[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m7, forecast7)
prophet_plot_components(m7, forecast7)

# Save the actual trained model architecture
saveRDS(m7, "Data/prophet_model_m7.rds")

# ---- Model 8: Capped Growth ----
cap_m8 = 2500 # set daily carrying capacity

df8 <- car_df |>
  mutate(
    is_final_week       = day(ds) > (days_in_month(ds) - 5),
    is_final_2ld        = day(ds) == (days_in_month(ds) - 1),
    is_final_day        = day(ds) == days_in_month(ds),
    is_first_week       = day(ds) <= 7 & day(ds) >= 3,
    is_first_2days      = day(ds) == 2,
    is_first_day        = day(ds) == 1,
    
    cap = cap_m8 # recall daily carrying capacity
  )

m8 <- prophet()
m8 <- prophet(monthly.seasonality = 20, holidays = holidays_MCO, growth = 'logistic') #  logistic growth
m8 <- add_seasonality(m8, name = 'monthly', period = 30.5, fourier.order = 10) # fourier order 10
m8 <- add_regressor(m8, 'is_final_week')
m8 <- add_regressor(m8, 'is_final_2ld')
m8 <- add_regressor(m8, 'is_final_day')
m8 <- add_regressor(m8, 'is_first_week')
m8 <- add_regressor(m8, 'is_first_2days')
m8 <- add_regressor(m8, 'is_first_day')
m8 <- fit.prophet(m8, df8)

future8 <- make_future_dataframe(m8, periods = 365)
future8$cap <- cap_m8 # recall daily carrying capacity
future8 <- future8 |>
  mutate(
    is_final_week       = day(ds) > (days_in_month(ds) - 5),
    is_final_2ld        = day(ds) == (days_in_month(ds) - 1),
    is_final_day        = day(ds) == days_in_month(ds),
    is_first_week       = day(ds) <= 7,
    is_first_2days      = day(ds) == 2,
    is_first_day        = day(ds) == 1
  )

forecast8 <- predict(m8, future8)
tail(forecast5[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m8, forecast8)
prophet_plot_components(m8, forecast8)

# Plot trimmed (plus minus 365 days)
last_date  <- max(car_df$ds)
start_date <- last_date - 2450
end_date   <- last_date + 365

forecast_trimmed <- subset(forecast4, ds >= start_date & ds <= end_date)

car_df_trimmed <- subset(car_df, ds >= start_date & ds <= end_date)
m_trimmed <- prophet(car_df_trimmed)

plot(m_trimmed, forecast_trimmed)


# ---- Model Validation (horizon must be equal to number of days in the m+1 forecasted month)
dfcv1 <- cross_validation(m1, cutoffs = cutoffs, horizon = 30, units = 'days')
dfp1 <- performance_metrics(dfcv1)
    
dfcv2 <- cross_validation(m2, cutoffs = cutoffs, horizon = 30, units = 'days')
dfp2 <- performance_metrics(dfcv2)

dfcv3 <- cross_validation(m3, cutoffs = cutoffs, horizon = 30, units = 'days')
dfp3 <- performance_metrics(dfcv3)

dfcv4 <- cross_validation(m4, cutoffs = cutoffs, horizon = 30, units = 'days')
dfp4 <- performance_metrics(dfcv4)

dfcv5 <- cross_validation(m5, cutoffs = cutoffs, horizon = 30, units = 'days')
dfp5 <- performance_metrics(dfcv5)

dfcv6 <- cross_validation(m6, cutoffs = cutoffs, horizon = 30, units = 'days')
dfcv6$y <- expm1(dfcv6$y)
dfcv6$yhat <- expm1(dfcv6$yhat)
dfcv6$yhat_lower <- expm1(dfcv6$yhat_lower)
dfcv6$yhat_upper <- expm1(dfcv6$yhat_upper)
dfp6 <- performance_metrics(dfcv6)

dfcv7 <- cross_validation(m7, cutoffs = cutoffs, horizon = 30, units = 'days')
dfp7 <- performance_metrics(dfcv7)

dfcv8 <- cross_validation(m8, cutoffs = cutoffs, horizon = 30, units = 'days')
dfp8 <- performance_metrics(dfcv8)
write_csv(forecast8, "Forecasts/m8_results.csv")
write_csv(dfcv8, "Forecasts/m8_crossvalid.csv")

# --- write the files to the folder ---
write_csv(forecast1, "Forecasts/m1_results.csv")
write_csv(forecast2, "Forecasts/m2_results.csv")
write_csv(forecast3, "Forecasts/m3_results.csv")
write_csv(forecast4, "Forecasts/m4_results.csv")
write_csv(forecast5, "Forecasts/m5_results.csv")
write_csv(forecast6, "Forecasts/m6_results.csv")
write_csv(forecast7, "Forecasts/m7_results.csv")


write_csv(dfcv1, "Forecasts/m1_crossvalid.csv")
write_csv(dfcv2, "Forecasts/m2_crossvalid.csv")
write_csv(dfcv3, "Forecasts/m3_crossvalid.csv")
write_csv(dfcv4, "Forecasts/m4_crossvalid.csv")
write_csv(dfcv5, "Forecasts/m5_crossvalid.csv")
write_csv(dfcv6, "Forecasts/m6_crossvalid.csv")
write_csv(dfcv7, "Forecasts/m7_crossvalid.csv")


# Based on data validation steps, Model 7 has the lowest error against actual.
# While Model 6 has the best monthly trend from 1st to 9th of the month

# Prepare upcoming month's forecast including monthly trend
forecast_sel1 <- forecast7 |>
  filter(year(ds) == Next_FC_Year & month(ds) == Next_FC_Month) |>
  select(ds, yhat, yhat_lower, yhat_upper) |>
  rename(Date := 'ds', yhat1 := 'yhat', yhat1_lower := 'yhat_lower', yhat1_upper := 'yhat_upper') |>
  mutate(yhat1 = case_when(yhat1 < 0 ~ 0, TRUE ~ yhat1))

forecast_sel2 <- forecast6 |> # for monthly trend, follows log model for the first 10 days of the month
  filter(year(ds) == Next_FC_Year & month(ds) == Next_FC_Month) |>
  select(ds, yhat, yhat_lower, yhat_upper) |>
  rename(Date := 'ds', yhat2 := 'yhat', yhat2_lower := 'yhat_lower', yhat2_upper := 'yhat_upper') |>
  mutate(yhat2 = case_when(yhat2 < 0 ~ 0, TRUE ~ yhat2))

yhat1_month = sum(forecast_sel1$yhat1)
yhat2_month = sum(forecast_sel2$yhat2)

forecast_nextmonth_2h <- forecast_sel1 |>
  filter(day(Date) > 10) |>                                # note the 10th of the month, same as the next step
  summarise(yhat1_month2h = sum(yhat1), .groups = "drop")

forecast_sel_comb <- forecast_sel1 |>
  left_join(forecast_sel2, by = c("Date")) |>
  mutate(yhat_adj = case_when(day(Date) <= 10 ~ yhat2, TRUE ~ yhat1 )) # note the 10th of the month being the separation here

yhat_adj_month = sum(forecast_sel_comb$yhat_adj)  

forecast_sel_comb <- forecast_sel_comb |>
  mutate(yhat1_month = yhat1_month,
         yhat2_month = yhat2_month,
         yhat_adj_month = yhat_adj_month,
         yhat1_month2h = forecast_nextmonth_2h$yhat1_month2h) |>
  mutate(yhat_adj_add = case_when(day(Date) <= 10 ~ 0, TRUE ~ ((yhat1_month - yhat_adj_month) * (yhat_adj/yhat1_month2h)))) |>
  mutate(yhat_adj_nodiff = yhat_adj + yhat_adj_add)

yhat_adjnodiff_month = sum(forecast_sel_comb$yhat_adj_nodiff)

# Prepare final data frame for dashboard
forecast_fd <- forecast_sel_comb |>
  select(Date, yhat_adj_nodiff) |>
  mutate(yhat_adj_nodiff = round(yhat_adj_nodiff)) |>
  rename(Forecast := yhat_adj_nodiff) |>
  mutate(Forecast_Cum = cumsum(Forecast))

# Get standard deviation of the month's forecast for confidence interval calculation.
forecast_monthly <- forecast7 |>
  select(ds, yhat, yhat_upper) |>
  mutate(
    month = floor_date(ds, "month"),
    sd_day = (yhat_upper - yhat) / 1.96
  ) |>
  mutate(yhat = case_when(yhat < 0 ~ 0, TRUE ~ yhat))|>
  group_by(month) |>
  summarise(
    yhat = sum(yhat),
    sd_month = sqrt(sum(sd_day^2)),
    yhat_lower = yhat - 1.96 * sd_month,
    yhat_upper = yhat + 1.96 * sd_month
  ) |>
  mutate(yhat = round(yhat), yhat_lower = round(yhat_lower), yhat_upper = round(yhat_upper))


# --- save the files to the folder for app.R ---
saveRDS(forecast_monthly, "Data/forecast_monthly.rds")
saveRDS(forecast_fd, "Data/forecast_fd.rds")

