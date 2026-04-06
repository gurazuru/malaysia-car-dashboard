
# ---- Load packages ----
library(shiny)
library(tidyverse)
library(zoo)
library(DT)
library(plotly)
library(forecast)
library(bslib)

# setwd("C:/R Projects/JPJ_Car_Viz")
data_asof = "data as of 31st March 2026"

# ---- Load and combine data ----
car_data <- readr::read_csv("Data/car_data_sum.csv") |> 
   filter(year(date_reg) == 2025)

# car_data <- readr::read_csv("Data/car_data_sum_sample.csv")

# ---- Forecasting ----
# aggregate data to monthly data to get monthly TIV
tiv_monthly <- car_data |>
  group_by(date_reg = floor_date(date_reg, "month")) |>
  summarise(TIV = sum(count, na.rm = TRUE)) |>
  arrange(date_reg)

# convert to time series object
tiv_ts <- ts(tiv_monthly$TIV, start = c(2010, 1), frequency = 12)

# auto.arima to select the best seasonal ARIMA
fit <- auto.arima(tiv_ts, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
summary(fit)

# forecast the upcoming 12 months and bind back to the original monthly tiv data
forecast_12 <- forecast(fit, h = 12, level = c(70, 95))

# convert to dataframe
forecast_df <- tibble(
  date_reg = seq(max(tiv_monthly$date_reg) %m+% months(1),
                 by = "1 month", length.out = 12),
  TIV = as.numeric(forecast_12$mean),
  lower_70 = as.numeric(forecast_12$lower[, 1]),
  upper_70 = as.numeric(forecast_12$upper[, 1]),
  lower_95 = as.numeric(forecast_12$lower[, 2]),
  upper_95 = as.numeric(forecast_12$upper[, 2]),
  type = "Forecast"
) 

tiv_plot_df <- tiv_monthly |>
  mutate(type = "Actual") |>
  bind_rows(forecast_df)

# ---- UI ----
ui <- page_fluid(
  tags$head(
    tags$style(HTML("
      body {
        zoom: 85%;
        -moz-transform: scale(0.85);
        -moz-transform-origin: 0 0;
      }
    "))
  ),
  
  titlePanel("Malaysia Total Industry Volume (Vehicle Registrations)"),
  
  tags$h5("This report provides an overview of vehicle registration trends in Malaysia, 
           using JPJ data from data.gov.my.",
          style = "color: #555; margin-top: -2px;"),
  
  fluidRow(
    column(width = 4,
       selectInput(
          inputId = "global_fuel_type",
          label = "Fuel Type:",
          choices = c("All", sort(unique(car_data$fuel_grouped))),
          selected = "All"
       )
    ),
    column(width = 4,
      selectInput(
        inputId = "year_selected",
        label = "Year:",
        choices = c("2026", "2025", "2024", "2023", "2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011"),
        selected = "2026"
      )
    )
  ),
  
  tabsetPanel(
    id = "tabs",
    tabPanel("By Make",
             layout_columns(
               width = 12,
               card(
                 card_header("Vehicle Registration Summary by Make"),
                 DTOutput("summary_table")
               ),
               card(
                 card_header(
                   div(
                     style = "display: flex; align-items: center; justify-content: space-between;",
                     "View as: ",
                     selectInput(
                       inputId = "agg_level",
                       label = NULL,
                       choices = c("Monthly", "Quarterly", "Annually", "5-Month Average"),
                       selected = "Monthly"
                     )
                   ) 
                 ),
                 plotlyOutput("trend_plot_make")
               )
             )
    ),
    tabPanel("By Model",
             layout_columns(
               width = 12,
               card(
                 card_header("Vehicle Registration Summary by Model"),
                 DTOutput("summary_table_model")
               ),     
               card(
                 card_header(
                   div(
                     style = "display: flex; align-items: center; justify-content: space-between;",
                     "View as: ",
                     selectInput(
                       inputId = "agg_level_model",
                       label = NULL,
                       choices = c("Monthly", "Quarterly", "Annually", "5-Month Average"),
                       selected = "Monthly"
                     )
                   ) 
                 ),
                 plotlyOutput("trend_plot_model")
               )
             )
    ),
    tabPanel("By Fuel Type",
             layout_columns(
               width = 12,
               card(
                 card_header("Vehicle Registration Summary by Fuel Type"),
                 DTOutput("summary_table_fuel")
               ),
               card(
                 card_header(
                   div(
                     style = "display: flex; align-items: center; justify-content: space-between;",
                     "View as: ",
                     selectInput(
                       inputId = "agg_level_fuel",
                       label = NULL,
                       choices = c("Monthly", "Quarterly", "Annually", "5-Month Average"),
                       selected = "Monthly"
                     )
                   ) 
                 ),
                 plotlyOutput("trend_plot_fuel")
               ),
             )
    ),
    tabPanel("By Make (Yearly)",
             layout_columns(
               col_widths =  c(12),
               card(
                 card_header("Monthly Vehicle Registrations by Make"),
                 DTOutput("annual_table")
               )
               # ,     
               # card(
               #   card_header(
               #     div(
               #       style = "display: flex; align-items: center; justify-content: space-between;",
               #       "View as: ",
               #       selectInput(
               #         inputId = "agg_level_model",
               #         label = NULL,
               #         choices = c("Monthly", "Quarterly", "Annually", "5-Month Average"),
               #         selected = "Monthly"
               #       )
               #     ) 
               #   ),
               #   # plotlyOutput("trend_plot_model") <-- will add later once the tables are final
               # )
             )
    ),
    tabPanel("By Model (Yearly)",
             layout_columns(
               col_widths =  c(12),
               card(
                 card_header("Monthly Vehicle Registrations by Model"),
                 DTOutput("annual_table_model")
               )
               # ,     
               # card(
               #   card_header(
               #     div(
               #       style = "display: flex; align-items: center; justify-content: space-between;",
               #       "View as: ",
               #       selectInput(
               #         inputId = "agg_level_model",
               #         label = NULL,
               #         choices = c("Monthly", "Quarterly", "Annually", "5-Month Average"),
               #         selected = "Monthly"
               #       )
               #     ) 
               #   ),
               #   # plotlyOutput("trend_plot_model") <-- will add later once the tables are final
               # )
             )
    ),
  ),
  actionButton("reset_selection", "Reset Selection"),
  
  card(
    height = c(180),
    card_header(data_asof),
    DTOutput("summary_table_total")
  ),
  
  tabsetPanel(
    id = "toptabs",
    tabPanel("TIV",
             layout_columns( width = 12,
                             card(
                               height = c(280),
                               card_header(
                                 div(
                                   style = "display: flex; align-items: center; justify-content: space-between;",
                                   "View as: ",
                                   selectInput(
                                     inputId = "agg_level_ttl",
                                     label = NULL,
                                     choices = c("Monthly", "Quarterly", "Annually", "5-Month Average"),
                                     selected = "Monthly"
                                   )
                                 ),
                               ),
                               plotlyOutput("trend_plot_total"),
                             )
             )
    ),
    tabPanel("Forecast",
             layout_columns( width = 12,
                             card(
                               height = c(280),
                               card_header(
                                 div(
                                 ),
                               ),
                               plotlyOutput("forecast_plot_total"),
                             )
             )
    )
  ),
  
  tags$footer(
    style = "bottom:0; right:0; width:100%; padding:5px; font-size:10px; text-align:right;",
    HTML("Created by Nur Nafis Naim | <i>nafisnaim@gmail.com</i>")
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # ---- Filter by Fuel Type & Latest Year ----
  filtered_data <- reactive({
    df <- car_data
    if (input$global_fuel_type != "All") {
      df <- df |> filter(fuel_grouped == input$global_fuel_type)
    }

    df <- df |> filter(year(date_reg) %in% c(as.numeric(input$year_selected), 
                                             as.numeric(input$year_selected) - 1,
                                             as.numeric(input$year_selected) - 2,
                                             as.numeric(input$year_selected) - 3,
                                             as.numeric(input$year_selected) - 4))

    return(df)
  })
  
  
  # --- get unique make models ---
  model_list <- reactive({
    df <- filtered_data() |>
      group_by(maker, model) |>
      summarise(total_count = sum(count), .groups = "drop") |>
      arrange(desc(total_count))
    
    return(df)
  })
  model_list_annual <- reactive({     # this if for the "yearly" tab
    df <- filtered_data() |>
      group_by(maker, model) |>
      filter(year(date_reg) == input$year_selected) |>
      summarise(total_count = sum(count), .groups = "drop") |>
      arrange(desc(total_count))
    
    return(df)
  })
  
  maker_list <- reactive({
    df <- filtered_data() |>
      group_by(maker) |>
      summarise(total_count = sum(count), .groups = "drop") |>
      arrange(desc(total_count))
    
    return(df)
  })  
  maker_list_annual <- reactive({     # this if for the "yearly" tab
    df <- filtered_data() |>
      group_by(maker) |>
      filter(year(date_reg) == input$year_selected) |>
      summarise(total_count = sum(count), .groups = "drop") |>
      arrange(desc(total_count))
    
    return(df)
  })
  
  fuel_grouped_list <- reactive({
    filtered_data() |>
      group_by(fuel_grouped) |>
      summarise(total_count = sum(count), .groups = "drop") |>
      arrange(desc(total_count))
  })
  
  # ---- Summary Function ----
  make_summary <- function(data, group_col, group_col_name) {
    group_col <- sym(group_col)  # convert string to symbol for tidy evaluation
    
    data <- filtered_data()
    
    # ---- Key Date Variables ----
    month_current <- max(data$date_reg)
    month_previous <- month_current %m-% months(1)
    month_previous_year <- month_current %m-% months(12)
    
    year_current <- year(month_current)
    ytd_current_start <- ymd(paste0(year_current, "-01-01"))
    ytd_current_end <- month_current
    
    ytd_previous_start <- ytd_current_start %m-% years(1)
    ytd_previous_end <- ytd_current_end %m-% years(1)
    
    
    # Define all the relevant dates.
    month_current_name <- format(month_current, "%b %Y")
    month_previous_name <- format(month_previous, "%b %Y")
    month_previous_year_name <- format(month_previous_year, "%b %Y")
    year_current_name <- paste0("YTD ", format(ytd_current_end, "%Y"))
    year_previous_name <- paste0("YTD ", format(ytd_previous_end, "%Y"))
    
    if (group_col == "model"){
      month_current_count <- data |>
        filter(date_reg == month_current) |>
        group_by(maker, !!group_col) |>
        summarise(count_current = sum(count), .groups = "drop")
      
      month_previous_count <- data |>
        filter(date_reg == month_previous) |>
        group_by(maker, !!group_col) |>
        summarise(count_previous = sum(count), .groups = "drop")
      
      month_previous_year_count <- data |>
        filter(date_reg == month_previous_year) |>
        group_by(maker, !!group_col) |>
        summarise(count_previous_year = sum(count), .groups = "drop")
      
      ytd_current_count <- data |>
        filter(date_reg >= ytd_current_start & date_reg <= ytd_current_end) |>
        group_by(maker, !!group_col) |>
        summarise(count_ytd_current = sum(count), .groups = "drop")
      
      ytd_previous_count <- data |>
        filter(date_reg >= ytd_previous_start & date_reg <= ytd_previous_end) |>
        group_by(maker, !!group_col) |>
        summarise(count_ytd_previous = sum(count), .groups = "drop")
      
      model_list() |>
        left_join(month_current_count, by = c("maker", "model")) |>
        mutate(count_current = replace_na(count_current, 0)) |>
        full_join(month_previous_count, by = c("maker", rlang::as_string(group_col))) |>
        full_join(month_previous_year_count, by = c("maker", rlang::as_string(group_col))) |>
        full_join(ytd_current_count, by = c("maker", rlang::as_string(group_col))) |>
        full_join(ytd_previous_count, by = c("maker", rlang::as_string(group_col))) |>
        mutate(
          across(starts_with("count"), \(x) replace_na(x, 0)),
          growth_MoM = if_else(count_previous > 0, (count_current / count_previous - 1), NA_real_),
          growth_YoY = if_else(count_previous_year > 0, (count_current / count_previous_year - 1), NA_real_),
          growth_YTD = if_else(count_ytd_previous > 0, (count_ytd_current / count_ytd_previous - 1), NA_real_)
        ) |>
        arrange(desc(count_current)) |>
        mutate(rank = row_number()) |>
        select(rank, maker, !!group_col, count_current, count_previous, growth_MoM, count_previous_year, growth_YoY, count_ytd_current, count_ytd_previous, growth_YTD) |>
        rename(
          `Rank` := rank,
          `Make` := maker,
          !!group_col_name := !!group_col,
          !!month_current_name := count_current,
          !!month_previous_name := count_previous,
          `Growth (MoM)` := growth_MoM,
          !!month_previous_year_name := count_previous_year,
          `Growth (YoY)` := growth_YoY,
          !!year_current_name := count_ytd_current,
          !!year_previous_name := count_ytd_previous,
          `Growth (YTD)` := growth_YTD
        ) 
    } else if (group_col == "maker") {
      month_current_count <- data |>
        filter(date_reg == month_current) |>
        group_by(!!group_col) |>
        summarise(count_current = sum(count), .groups = "drop")
      
      month_previous_count <- data |>
        filter(date_reg == month_previous) |>
        group_by(!!group_col) |>
        summarise(count_previous = sum(count), .groups = "drop")
      
      month_previous_year_count <- data |>
        filter(date_reg == month_previous_year) |>
        group_by(!!group_col) |>
        summarise(count_previous_year = sum(count), .groups = "drop")
      
      ytd_current_count <- data |>
        filter(date_reg >= ytd_current_start & date_reg <= ytd_current_end) |>
        group_by(!!group_col) |>
        summarise(count_ytd_current = sum(count), .groups = "drop")
      
      ytd_previous_count <- data |>
        filter(date_reg >= ytd_previous_start & date_reg <= ytd_previous_end) |>
        group_by(!!group_col) |>
        summarise(count_ytd_previous = sum(count), .groups = "drop")
      
      maker_list() |>
        left_join(month_current_count, by = c(rlang::as_string(group_col))) |>
        mutate(count_current = replace_na(count_current, 0)) |>
        full_join(month_previous_count, by = rlang::as_string(group_col)) |>
        full_join(month_previous_year_count, by = rlang::as_string(group_col)) |>
        full_join(ytd_current_count, by = rlang::as_string(group_col)) |>
        full_join(ytd_previous_count, by = rlang::as_string(group_col)) |>
        mutate(
          across(starts_with("count"), \(x) replace_na(x, 0)),
          growth_MoM = if_else(count_previous > 0, (count_current / count_previous - 1), NA_real_),
          growth_YoY = if_else(count_previous_year > 0, (count_current / count_previous_year - 1), NA_real_),
          growth_YTD = if_else(count_ytd_previous > 0, (count_ytd_current / count_ytd_previous - 1), NA_real_)
        ) |>
        arrange(desc(count_current)) |>
        mutate(rank = row_number()) |>
        select(rank, !!group_col, count_current, count_previous, growth_MoM, count_previous_year, growth_YoY, count_ytd_current, count_ytd_previous, growth_YTD) |>
        rename(
          `Rank` := rank,
          !!group_col_name := !!group_col,
          !!month_current_name := count_current,
          !!month_previous_name := count_previous,
          `Growth (MoM)` := growth_MoM,
          !!month_previous_year_name := count_previous_year,
          `Growth (YoY)` := growth_YoY,
          !!year_current_name := count_ytd_current,
          !!year_previous_name := count_ytd_previous,
          `Growth (YTD)` := growth_YTD
        ) 
    } else {
      month_current_count <- data |>
        filter(date_reg == month_current) |>
        group_by(!!group_col) |>
        summarise(count_current = sum(count), .groups = "drop")
      
      month_previous_count <- data |>
        filter(date_reg == month_previous) |>
        group_by(!!group_col) |>
        summarise(count_previous = sum(count), .groups = "drop")
      
      month_previous_year_count <- data |>
        filter(date_reg == month_previous_year) |>
        group_by(!!group_col) |>
        summarise(count_previous_year = sum(count), .groups = "drop")
      
      ytd_current_count <- data |>
        filter(date_reg >= ytd_current_start & date_reg <= ytd_current_end) |>
        group_by(!!group_col) |>
        summarise(count_ytd_current = sum(count), .groups = "drop")
      
      ytd_previous_count <- data |>
        filter(date_reg >= ytd_previous_start & date_reg <= ytd_previous_end) |>
        group_by(!!group_col) |>
        summarise(count_ytd_previous = sum(count), .groups = "drop")
      
      fuel_grouped_list() |>
        left_join(month_current_count, by = c(rlang::as_string(group_col))) |>
        mutate(count_current = replace_na(count_current, 0)) |>
        full_join(month_previous_count, by = rlang::as_string(group_col)) |>
        full_join(month_previous_year_count, by = rlang::as_string(group_col)) |>
        full_join(ytd_current_count, by = rlang::as_string(group_col)) |>
        full_join(ytd_previous_count, by = rlang::as_string(group_col)) |>
        mutate(
          across(starts_with("count"), \(x) replace_na(x, 0)),
          growth_MoM = if_else(count_previous > 0, (count_current / count_previous - 1), NA_real_),
          growth_YoY = if_else(count_previous_year > 0, (count_current / count_previous_year - 1), NA_real_),
          growth_YTD = if_else(count_ytd_previous > 0, (count_ytd_current / count_ytd_previous - 1), NA_real_)
        ) |>
        arrange(desc(count_current)) |>
        mutate(rank = row_number()) |>
        select(rank, !!group_col, count_current, count_previous, growth_MoM, count_previous_year, growth_YoY, count_ytd_current, count_ytd_previous, growth_YTD) |>
        rename(
          `Rank` := rank,
          !!group_col_name := !!group_col,
          !!month_current_name := count_current,
          !!month_previous_name := count_previous,
          `Growth (MoM)` := growth_MoM,
          !!month_previous_year_name := count_previous_year,
          `Growth (YoY)` := growth_YoY,
          !!year_current_name := count_ytd_current,
          !!year_previous_name := count_ytd_previous,
          `Growth (YTD)` := growth_YTD
        ) 
    }
  }
  
  
  # ---- Annual YTD Function ----
  make_annual <- function(data, group_col, group_col_name) {
    group_col <- sym(group_col)  # convert string to symbol for tidy evaluation
    
    data <- filtered_data()
    
    if (group_col == "model"){
      
      monthly_data <- function(month_num, year_num){
        data <- data |>
          filter(month(date_reg) == month_num, year(date_reg) == year_num) |>
          group_by(maker, !!group_col) |>
          summarise(count_current = sum(count), .groups = "drop")
        
        colname <- format(as.Date(paste(year_num, month_num, 1, sep = "-")), "%b %Y")
        
        data <- data |> rename(!!colname := count_current)
        
        return(data)
      }
      
      Jan_Count <- monthly_data(1, input$year_selected)
      Feb_Count <- monthly_data(2, input$year_selected)
      Mar_Count <- monthly_data(3, input$year_selected)
      Apr_Count <- monthly_data(4, input$year_selected)
      May_Count <- monthly_data(5, input$year_selected)
      Jun_Count <- monthly_data(6, input$year_selected)
      Jul_Count <- monthly_data(7, input$year_selected)
      Aug_Count <- monthly_data(8, input$year_selected)
      Sep_Count <- monthly_data(9, input$year_selected)
      Oct_Count <- monthly_data(10, input$year_selected)
      Nov_Count <- monthly_data(11, input$year_selected)
      Dec_Count <- monthly_data(12, input$year_selected)
      
      Year_Total_Count <- data |>
        filter(year(date_reg) == input$year_selected) |>
        group_by(maker, !!group_col) |>
        summarise(count_current = sum(count), .groups = "drop")
      
      year_colname <- paste0("Total ", format(as.Date(paste(input$year_selected, 1, 1, sep = "-")), "%Y"))
      
      Year_Total_Count <- Year_Total_Count |> rename(!!year_colname := count_current)
      
      data <- model_list_annual() |>
        left_join(Year_Total_Count, by = c("maker", "model")) |>
        full_join(Jan_Count, by = c("maker", rlang::as_string(group_col))) |>
        full_join(Feb_Count, by = c("maker", rlang::as_string(group_col))) |>
        full_join(Mar_Count, by = c("maker", rlang::as_string(group_col))) |>
        full_join(Apr_Count, by = c("maker", rlang::as_string(group_col))) |>
        full_join(May_Count, by = c("maker", rlang::as_string(group_col))) |>
        full_join(Jun_Count, by = c("maker", rlang::as_string(group_col))) |>
        full_join(Jul_Count, by = c("maker", rlang::as_string(group_col))) |>
        full_join(Aug_Count, by = c("maker", rlang::as_string(group_col))) |>
        full_join(Sep_Count, by = c("maker", rlang::as_string(group_col))) |>
        full_join(Oct_Count, by = c("maker", rlang::as_string(group_col))) |>
        full_join(Nov_Count, by = c("maker", rlang::as_string(group_col))) |>
        full_join(Dec_Count, by = c("maker", rlang::as_string(group_col))) |>
        arrange(desc('Total')) |>
        mutate(rank = row_number()) |>
        select(rank, maker, !!group_col,
               all_of(paste(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Total"), input$year_selected))
        ) |>
        rename(
          `Rank` := rank,
          `Make` := maker,
          !!group_col_name := !!group_col,
        )
    } else if (group_col == "maker") {
      
      monthly_data <- function(month_num, year_num){
        data <- data |>
          filter(month(date_reg) == month_num, year(date_reg) == year_num) |>
          group_by(!!group_col) |>
          summarise(count_current = sum(count), .groups = "drop")
        
        colname <- format(as.Date(paste(year_num, month_num, 1, sep = "-")), "%b %Y")
        
        data <- data |> rename(!!colname := count_current)
        
        return(data)
      }
      
      Jan_Count <- monthly_data(1, input$year_selected)
      Feb_Count <- monthly_data(2, input$year_selected)
      Mar_Count <- monthly_data(3, input$year_selected)
      Apr_Count <- monthly_data(4, input$year_selected)
      May_Count <- monthly_data(5, input$year_selected)
      Jun_Count <- monthly_data(6, input$year_selected)
      Jul_Count <- monthly_data(7, input$year_selected)
      Aug_Count <- monthly_data(8, input$year_selected)
      Sep_Count <- monthly_data(9, input$year_selected)
      Oct_Count <- monthly_data(10, input$year_selected)
      Nov_Count <- monthly_data(11, input$year_selected)
      Dec_Count <- monthly_data(12, input$year_selected)
      
      Year_Total_Count <- data |>
        filter(year(date_reg) == input$year_selected) |>
        group_by(!!group_col) |>
        summarise(count_current = sum(count), .groups = "drop")
      
      year_colname <- paste0("Total ", format(as.Date(paste(input$year_selected, 1, 1, sep = "-")), "%Y"))
      
      Year_Total_Count <- Year_Total_Count |> rename(!!year_colname := count_current)
      
      data <- maker_list_annual() |>
        left_join(Year_Total_Count, by = c("maker")) |>
        full_join(Jan_Count, by = c(rlang::as_string(group_col))) |>
        full_join(Feb_Count, by = c(rlang::as_string(group_col))) |>
        full_join(Mar_Count, by = c(rlang::as_string(group_col))) |>
        full_join(Apr_Count, by = c(rlang::as_string(group_col))) |>
        full_join(May_Count, by = c(rlang::as_string(group_col))) |>
        full_join(Jun_Count, by = c(rlang::as_string(group_col))) |>
        full_join(Jul_Count, by = c(rlang::as_string(group_col))) |>
        full_join(Aug_Count, by = c(rlang::as_string(group_col))) |>
        full_join(Sep_Count, by = c(rlang::as_string(group_col))) |>
        full_join(Oct_Count, by = c(rlang::as_string(group_col))) |>
        full_join(Nov_Count, by = c(rlang::as_string(group_col))) |>
        full_join(Dec_Count, by = c(rlang::as_string(group_col))) |>
        arrange(desc('Total')) |>
        mutate(rank = row_number()) |>
        select(rank, !!group_col,
               all_of(paste(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Total"), input$year_selected))
        ) |>
        rename(
          `Rank` := rank,
          !!group_col_name := !!group_col,
        )
      
    } else {
    }
    return(data)
  }
  
  # ---- Output Data Table Functions ----
  data_table_TIV <- function(df) {
    df <- df() 
    
    datatable(df,
              rownames = FALSE,
              options = list(dom = 't', ordering = FALSE),
              class = 'cell-border stripe') |>
      formatPercentage("Growth (MoM)", 1) |>
      formatPercentage("Growth (YoY)", 1) |>
      formatPercentage("Growth (YTD)", 1) |>
      formatStyle("Growth (MoM)", color = styleInterval(c(0), c('red', 'green'))) |>
      formatStyle("Growth (YoY)", color = styleInterval(c(0), c('red', 'green'))) |>
      formatStyle("Growth (YTD)", color = styleInterval(c(0), c('red', 'green')))
  }
  
  data_table <- function(df) {
    df <- df() 
    
    datatable(df,
              rownames = FALSE,
              filter = 'top',
              selection = list(mode = "multiple"
                               #, selected = 1:5
              ),
              
              extensions = 'Buttons',
              
              options = list(
                dom = '<"d-flex justify-content-between align-items-center mb-2"Bf>tip', # B = Buttons, f = filter, t = table, i/p = info/pagination
                
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = TRUE,
                buttons = c('excel'),
                
                buttons = list(
                  list(
                    extend = "excel",
                    text = "Export to Excel",
                    title = NULL,
                    filename = "data",
                    exportOptions = list(
                      modifier = list(page = "all")
                    )
                  )# export all pages
                ),
                
                pageLength = 10
              ),
              class = 'cell-border stripe') |>
      formatPercentage("Growth (MoM)", 1) |>
      formatPercentage("Growth (YoY)", 1) |>
      formatPercentage("Growth (YTD)", 1) |>
      formatStyle("Growth (MoM)", color = styleInterval(c(0), c('red', 'green'))) |>
      formatStyle("Growth (YoY)", color = styleInterval(c(0), c('red', 'green'))) |>
      formatStyle("Growth (YTD)", color = styleInterval(c(0), c('red', 'green')))
  }
  
  # ---- Data table for annual (YTD) data. Diff is that there's no Growth data here ----
  data_table_annual <- function(df) {
    df <- df() 
    
    datatable(df,
              rownames = FALSE,
              filter = 'top',
              selection = list(mode = "multiple"
                               #, selected = 1:5
              ),
              
              extensions = 'Buttons',
              
              options = list(
                dom = '<"d-flex justify-content-between align-items-center mb-2"Bf>tip', # B = Buttons, f = filter, t = table, i/p = info/pagination
                
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = TRUE,
                buttons = c('excel'),
                
                buttons = list(
                  list(
                    extend = "excel",
                    text = "Export to Excel",
                    title = NULL,
                    filename = "data",
                    exportOptions = list(
                      modifier = list(page = "all")
                    )
                  )# export all pages
                ),
                
                pageLength = 10
              ),
              class = 'cell-border stripe')
  }
  
  # ---- Selections ----
  selected_make <- reactive({
    sel <- input$summary_table_rows_selected
    if (length(sel)) summary_data()$`Make`[sel] else summary_data()$`Make`[1:5]
  })
  selected_model <- reactive({
    sel <- input$summary_table_model_rows_selected
    if (length(sel)) summary_data_model()$`Model`[sel] else summary_data_model()$`Model`[1:5] 
  })
  selected_fuel <- reactive({
    sel <- input$summary_table_fuel_rows_selected
    if (length(sel)) summary_data_fuel()$`Fuel Type`[sel] else summary_data_fuel()$`Fuel Type`[1:5] 
  })
  
  # ---- Output Plotly Function ----
  plot_chart_total <- function(agg_choice) {
    monthly_trends_data_ttl <- car_data |>
      mutate(month = date_reg) |>
      group_by(month) |>
      summarise(registration = sum(count), .groups = "drop")
    
    if (!is.null(agg_choice)) {
      monthly_trends_data_ttl <- monthly_trends_data_ttl |>
        mutate(year = lubridate::year(month),
               quarter = lubridate::quarter(month),
               month_num = lubridate::month(month))
      
      if (agg_choice == "Quarterly") {
        monthly_trends_data_ttl <- monthly_trends_data_ttl |>
          group_by(year, quarter) |>
          summarise(registration = sum(registration), .groups = "drop") |>
          mutate(month = as.Date(paste0(year, "-", (quarter - 1)*3 +1, "-01")))
        
      } else if (agg_choice == "Annually") {
        monthly_trends_data_ttl <- monthly_trends_data_ttl |>
          group_by(year) |>
          summarise(registration = sum(registration), .groups = "drop") |>
          mutate(month = as.Date(paste0(year, "-01-01")))
        
      } else if (agg_choice == "5-Month Average") {
        monthly_trends_data_ttl <- monthly_trends_data_ttl |>
          arrange(month) |>
          mutate(registration = zoo::rollmean(registration, 5, fill = NA, align = "right")) |>
          ungroup()
      }
    }
    
    trend_plot <- ggplot(monthly_trends_data_ttl, aes(x = month, y = registration)) +
      geom_line(linewidth = 0.4) +
      geom_point(size = 0.8) +
      theme_minimal(base_size = 14) +
      labs(x = "Month", y = "Registration", color = NULL) +
      scale_y_continuous(labels = scales::comma)
    
    ggplotly(trend_plot) |>
      layout(
        xaxis = list(
          rangeslider = list(visible = FALSE, thickness = 0.04)
        )
      ) 
  }
  
  plot_chart <- function(df, group_col, group_col_name, agg_choice) {
    df <- filtered_data() # call the reactive to get the data frame.
    
    group_col_name_sym <- rlang::as_name(rlang::ensym(group_col))
    
    selected_value <- switch(
      group_col_name_sym,
      "maker" = selected_make(),
      "model" = selected_model(),
      "fuel_grouped" = selected_fuel(),
      NULL
    )
    
    if (!is.null(selected_value)) {
      monthly_trends_data <- df |>
        filter({{ group_col }} %in% selected_value) |>
        mutate(month = date_reg) |>
        group_by(month, {{ group_col }}) |>
        summarise(registration = sum(count), .groups = "drop")
    } else {
      if(group_col_name == "Fuel Type"){
        monthly_trends_data <- df |>
          mutate(month = date_reg) |>
          group_by(month, {{ group_col }}) |>
          summarise(registration = sum(count), .groups = "drop")
      } else{
        
        monthly_trends_data <- df |>
          filter({{ group_col }} %in% selected_value) |>
          mutate(month = date_reg) |>
          group_by(month, {{ group_col }}) |>
          summarise(registration = sum(count), .groups = "drop")
      }
    }
    
    if (!is.null(agg_choice)) {
      monthly_trends_data <- monthly_trends_data |>
        mutate(year = lubridate::year(month),
               quarter = lubridate::quarter(month),
               month_num = lubridate::month(month))
      
      if (agg_choice == "Quarterly") {
        monthly_trends_data <- monthly_trends_data |>
          group_by(year, quarter, {{ group_col }}) |>
          summarise(registration = sum(registration), .groups = "drop") |>
          mutate(month = as.Date(paste0(year, "-", (quarter - 1)*3 +1, "-01")))
        
      } else if (agg_choice == "Annually") {
        monthly_trends_data <- monthly_trends_data |>
          group_by(year, {{ group_col }}) |>
          summarise(registration = sum(registration), .groups = "drop") |>
          mutate(month = as.Date(paste0(year, "-01-01")))
        
      } else if (agg_choice == "5-Month Average") {
        monthly_trends_data <- monthly_trends_data |>
          arrange(month) |>
          group_by({{ group_col }}) |>
          mutate(registration = zoo::rollmean(registration, 5, fill = NA, align = "right")) |>
          ungroup()
      }
    }
    
    trend_plot <- ggplot(monthly_trends_data, aes(x = month, y = registration, color = {{ group_col }})) +
      geom_line(linewidth = 0.4) +
      geom_point(size = 0.8) +
      theme_minimal(base_size = 14) +
      labs(x = "Month", y = "Registration", color = NULL) +
      scale_y_continuous(labels = scales::comma)
    
    ggplotly(trend_plot) |>
      layout(
        xaxis = list(
          rangeslider = list(visible = FALSE, thickness = 0.04)
        )
      ) 
  }
  
  plot_chart_fc_total <- function() {
    plot_ly() |>
      add_lines(
        data = tiv_monthly,
        x = ~date_reg, y = ~TIV,
        name = "Actual",
        line = list(color = "black"),
        hovertemplate = "Actual: %{y:.3~s}<extra></extra>"
      ) |>
      add_lines(
        data = forecast_df,
        x = ~date_reg, y = ~TIV,
        name = "Forecast",
        line = list(color = "orange", dash = "dot"),
        hovertemplate = "Forecast: %{y:.3~s}<extra></extra>"
      ) |>
      add_ribbons( # This is the corrected section
        data = forecast_df,
        x = ~date_reg, ymin = ~lower_70, ymax = ~upper_70,
        name = "70% CI",
        fillcolor = 'rgba(255,165,0,0.2)', 
        line = list(color = 'transparent'),
        hovertemplate = "70% CI: %{y:.3~s}<extra></extra>"
      ) |>
      add_lines( # This is the corrected section
        data = forecast_df,
        x = ~date_reg, y = ~lower_70,
        name = "70% CI",
        line = list(color = "rgba(255,165,0,0.2)", width = 0), 
        hovertemplate = "70% CI: %{y:.3~s}<extra></extra>"
      ) |>
      layout(
        title = "TIV with 12-Month Forecast",
        xaxis = list(title = "Month"),
        yaxis = list(title = "Registration"),
        hovermode = "x"
      )
  }
  
  # ---- Reactive summary total ----
  summary_total <- reactive({
    month_current_count_ttl <- car_data |>
      filter(date_reg == month_current) |>
      summarise(count_current = sum(count), .groups = "drop")
    
    month_previous_count_ttl <- car_data |>
      filter(date_reg == month_previous) |>
      summarise(count_previous = sum(count), .groups = "drop")
    
    month_previous_year_count_ttl <- car_data |>
      filter(date_reg == month_previous_year) |>
      summarise(count_previous_year = sum(count), .groups = "drop")
    
    ytd_current_count_ttl <- car_data |>
      filter(date_reg >= ytd_current_start & date_reg <= ytd_current_end) |>
      summarise(count_ytd_current = sum(count), .groups = "drop")
    
    ytd_previous_count_ttl <- car_data |>
      filter(date_reg >= ytd_previous_start & date_reg <= ytd_previous_end) |>
      summarise(count_ytd_previous = sum(count), .groups = "drop")
    
    # Add label column first
    label_col <- tibble(category = "TIV")
    
    bind_cols(
      label_col,
      month_current_count_ttl,
      month_previous_count_ttl,
      month_previous_year_count_ttl,
      ytd_current_count_ttl,
      ytd_previous_count_ttl
    ) |>
      mutate(
        across(starts_with("count"), \(x) replace_na(x, 0)),
        growth_MoM = if_else(count_previous > 0, (count_current / count_previous - 1), NA_real_),
        growth_YoY = if_else(count_previous_year > 0, (count_current / count_previous_year - 1), NA_real_),
        growth_YTD = if_else(count_ytd_previous > 0, (count_ytd_current / count_ytd_previous - 1), NA_real_)
      ) |>
      arrange(desc(count_current)) |>
      select(category, count_current, count_previous, growth_MoM, count_previous_year, growth_YoY, count_ytd_current, 
             !!month_current_name := count_current, count_ytd_previous, growth_YTD) |>
      rename(
        `Total` := category,
        !!month_previous_name := count_previous,
        `Growth (MoM)` := growth_MoM,
        !!month_previous_year_name := count_previous_year,
        `Growth (YoY)` := growth_YoY,
        !!year_current_name := count_ytd_current,
        !!year_previous_name := count_ytd_previous,
        `Growth (YTD)` := growth_YTD
      ) 
  })
  
  # ---- Reactive summary ----
  summary_data <- reactive(make_summary(car_data, "maker", "Make"))
  summary_data_model <- reactive(make_summary(car_data, "model", "Model"))
  summary_data_fuel <- reactive(make_summary(car_data, "fuel_grouped", "Fuel Type"))
  
  annual_data <- reactive(make_annual(car_data, "maker", "Make"))
  annual_data_model <- reactive(make_annual(car_data, "model", "Model"))
  
  # ---- Output 1: DataTable ----
  output$summary_table_total  <- renderDT(data_table_TIV(summary_total))
  output$summary_table        <- renderDT(data_table(summary_data), server = FALSE)
  output$summary_table_model  <- renderDT(data_table(summary_data_model), server = FALSE)
  output$summary_table_fuel   <- renderDT(data_table(summary_data_fuel), server = FALSE)
  
  output$annual_table         <- renderDT(data_table_annual(annual_data), server = FALSE)
  output$annual_table_model   <- renderDT(data_table_annual(annual_data_model), server = FALSE)
  
  # ---- Output 2: Plotly Trend ----
  output$trend_plot_total     <- renderPlotly(plot_chart_total(input$agg_level_ttl))
  output$trend_plot_make      <- renderPlotly(plot_chart(summary_data, maker, "Make", input$agg_level))
  output$trend_plot_model     <- renderPlotly(plot_chart(summary_data_model, model, "Model", input$agg_level_model))
  output$trend_plot_fuel      <- renderPlotly(plot_chart(summary_data_fuel, fuel_grouped, "Fuel Type", input$agg_level_fuel))
  output$forecast_plot_total  <- renderPlotly(plot_chart_fc_total())
  
  observeEvent(input$reset_selection, {
    if (input$tabs == "By Make") {
      proxy <- DT::dataTableProxy("summary_table")
      DT::selectRows(proxy, NULL)
    } else if (input$tabs == "By Model") {
      proxy <- DT::dataTableProxy("summary_table_model")
      DT::selectRows(proxy, NULL)
    } else if (input$tabs == "By Fuel Type") {
      proxy <- DT::dataTableProxy("summary_table_fuel")
      DT::selectRows(proxy, NULL)
    }
  })
} # END OF SERVER

shinyApp(ui, server)